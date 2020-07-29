{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Converting the output of the asl translator into semmc formulas

module SemMC.Architecture.ARM.ASL
  ( encodingToFormula
  , symFnToFunFormula
  , ASLSemanticsOpts(..)
  , ASLSemantics(..)
  , loadSemantics
  ) where

import           GHC.TypeLits

import           Data.Kind
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Control.Applicative ( Const(..) )
import qualified Control.Monad.Except as ME
import           Control.Monad ( liftM, forM )
import           Data.Proxy
import qualified Data.HashTable.Class as H
import qualified Control.Monad.ST as ST
import           Data.Maybe ( fromMaybe )

import qualified Data.BitVector.Sized as BVS
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Type.List as TL
import qualified Data.Parameterized.TyMap as TM
import qualified Data.Parameterized.SymbolRepr as SR
import           Data.Parameterized.Classes
import           Data.Parameterized.List as SL
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some
import           Data.Parameterized.Pair
import           Data.Parameterized.HasRepr

-- from asl translator
import           Data.Parameterized.CtxFuns

import qualified Lang.Crucible.Backend.Simple as CBS
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import           Language.ASL.Globals ( UnitType )
import qualified SemMC.BoundVar as BV
import qualified SemMC.Formula.Env as SF
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Util as U

import qualified Dismantle.ARM.T32 as T32
import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.ASL as ASL

import           SemMC.Architecture.AArch32 ( AArch32 )
import qualified SemMC.Architecture.AArch32 as ARM
import qualified SemMC.Architecture.ARM.Location as ARM
import           SemMC.Architecture.ARM.Combined ( ARMOperandRepr )

import qualified Language.ASL.Globals as ASL
import qualified Language.ASL.Formulas as ASL

import qualified What4.Interface as WI
import qualified What4.Expr.Builder as WB
import           What4.Utils.Util ( SomeSome(..) )

data OperandTypeWrapper (arch :: Type) :: TL.TyFun Symbol WI.BaseType -> Type
type instance TL.Apply (OperandTypeWrapper arch) s = A.OperandType arch s
type OperandTypes arch sh = TL.Map (OperandTypeWrapper arch) sh
type OperandTypesCtx arch sh = TL.ToContextFwd (TL.Map (OperandTypeWrapper arch) sh)

data GlobalsTypeWrapper :: TL.TyFun Symbol WI.BaseType -> Type
type instance TL.Apply GlobalsTypeWrapper s = ASL.GlobalsType s

shapeOfOperands :: SL.List ARMOperandRepr sh -> SL.List WI.BaseTypeRepr (OperandTypes AArch32 sh)
shapeOfOperands SL.Nil = SL.Nil
shapeOfOperands (a SL.:< oprepr) = ARM.shapeReprType a SL.:< shapeOfOperands oprepr


mkOperandVars :: forall sym sh
               . WI.IsSymExprBuilder sym
              => sym
              -> ARM.ARMOpcode ARM.ARMOperand sh
              -> IO (SL.List (BV.BoundVar sym AArch32) sh)
mkOperandVars sym op = do
  SL.itraverse mkBV (typeRepr op)
  where
    mkBV :: forall s. SL.Index sh s -> ARMOperandRepr s -> IO (BV.BoundVar sym AArch32 s)
    mkBV idx opRep = do
      let ty = ARM.shapeReprType opRep
      let nm = WI.safeSymbol $ (A.operandTypeReprSymbol (Proxy @AArch32) opRep) ++ "_" ++ show (SL.indexValue idx)
      BV.BoundVar <$> WI.freshBoundVar sym nm ty

type FnArgSig sh = (OperandTypesCtx AArch32 sh Ctx.::> WI.BaseStructType ASL.StructGlobalsCtx)

testEqualityErr :: (TestEquality f, ShowF f, ME.MonadError String m)
                => f tps
                -> f tps'
                -> m (f tps :~: f tps')
testEqualityErr tps tps' = case testEquality tps tps' of
  Just Refl -> return Refl
  Nothing -> ME.throwError $ "Type mismatch. Expected: " ++ showF tps ++ "\nGot:" ++ showF tps'

testOpcodeSig :: forall sym args ret sh m
               . WI.IsSymExprBuilder sym
              => ME.MonadError String m
              => sym
              -> ARM.ARMOpcode ARM.ARMOperand sh
              -> WI.SymFn sym args ret
              -> m ((WI.BaseTypeRepr (WI.BaseStructType args), WI.BaseTypeRepr ret)
                    :~: (WI.BaseTypeRepr (WI.BaseStructType (FnArgSig sh)), (WI.BaseTypeRepr (WI.BaseStructType ASL.StructGlobalsCtx))))
testOpcodeSig _ op symfn = do
  globalBaseReprs <- return $ (knownRepr :: Ctx.Assignment WI.BaseTypeRepr ASL.StructGlobalsCtx)
  Refl <- testEqualityErr (operandsToAsn (typeRepr op) Ctx.:> WI.BaseStructRepr globalBaseReprs) (WI.fnArgTypes symfn)
  Refl <- testEqualityErr (WI.BaseStructRepr globalBaseReprs) (WI.fnReturnType symfn)
  return Refl

data GlobalBoundVar sym s where
  GlobalBoundVar :: WI.BoundVar sym (ASL.GlobalsType s)
                 -> ASL.GlobalRef s
                 -> GlobalBoundVar sym s

operandsToAsn :: SL.List ARMOperandRepr sh -> Ctx.Assignment WI.BaseTypeRepr (OperandTypesCtx AArch32 sh)
operandsToAsn repr = TL.toAssignmentFwd (shapeOfOperands repr)

bvsToAsn :: SL.List (BV.BoundVar sym AArch32) sh
         -> Ctx.Assignment (WI.BoundVar sym) (OperandTypesCtx AArch32 sh)
bvsToAsn bvs = TL.toAssignmentFwd $ TM.applyMapList (Proxy @(OperandTypeWrapper AArch32)) BV.unBoundVar bvs

mkGlobalVars :: forall sym
              . WI.IsSymExprBuilder sym
             => sym -> IO (Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx)
mkGlobalVars sym = FC.traverseFC mkBV ASL.allGlobalRefs
  where
    mkBV :: ASL.GlobalRef s -> IO (GlobalBoundVar sym s)
    mkBV gr = do
      bv <- WI.freshBoundVar sym (WI.safeSymbol $ T.unpack $ SR.symbolRepr $ ASL.globalRefSymbol gr) (ASL.globalRefRepr gr)
      return $ GlobalBoundVar bv gr

getLiteralVarMap :: Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx
                 -> MapF.MapF (L.Location AArch32) (WI.BoundVar sym)
getLiteralVarMap bvs = MapF.fromList $
  FC.toListFC (\(GlobalBoundVar bv gr) -> (Pair (ARM.Location gr) bv)) bvs



mkOpParams :: SL.List ARMOperandRepr sh -> Set.Set (Some (SF.Parameter AArch32 sh))
mkOpParams bvs =
  Set.fromList $ FC.toListFC (\(Const c) -> c) $
    SL.imap (\idx v -> Const (Some (SF.OperandParameter @AArch32 (ARM.shapeReprType v) idx))) bvs


class WI.IsSymExprBuilder sym => SymFnsHaveBVs sym where
  fnBoundVars :: sym -> WI.SymFn sym args ret -> Maybe (Ctx.Assignment (WI.BoundVar sym) args)

instance SymFnsHaveBVs (WB.ExprBuilder t st fs) where
  fnBoundVars _sym symFn = case WB.symFnInfo symFn of
      WB.DefinedFnInfo vars _ _ -> Just vars
      _ -> Nothing

data UFBundle sym =
   UFBundle { getGPR :: forall n
                      . n <= ASL.MaxGPR
                     => NR.NatRepr n
                     -> WI.SymExpr sym (ASL.GlobalsType "GPRS")
                     -> Ctx.Assignment (WB.SymExpr sym) ASL.GPRCtx
                     -> IO (WI.SymExpr sym (WI.BaseBVType 32))
            , getSIMD :: forall n
                       . n <= ASL.MaxSIMD
                      => NR.NatRepr n
                      -> WI.SymExpr sym (ASL.GlobalsType "SIMDS")
                      -> Ctx.Assignment (WB.SymExpr sym) ASL.SIMDCtx
                      -> IO (WI.SymExpr sym (WI.BaseBVType 128))

            , initGPRs :: WI.SymFn sym Ctx.EmptyCtx (ASL.GlobalsType "GPRS")
            , initSIMDs :: WI.SymFn sym Ctx.EmptyCtx (ASL.GlobalsType "SIMDS")
            , initMemory :: WI.SymFn sym Ctx.EmptyCtx (ASL.GlobalsType "__Memory")
            , updateGPRs :: WI.SymFn sym (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "GPRS") UnitType
            , updateSIMDs :: WI.SymFn sym (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "SIMDS") UnitType
            , updateMemory :: WI.SymFn sym (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "__Memory") (ASL.GlobalsType "__Memory")
            , updateAssert :: WI.SymFn sym (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "__AssertionFailure") (ASL.GlobalsType "__AssertionFailure")
            , updateUndefB :: WI.SymFn sym (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "__UndefinedBehavior") (ASL.GlobalsType "__UndefinedBehavior")
            , updateUnpredB :: WI.SymFn sym (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "__UnpredictableBehavior") (ASL.GlobalsType "__UnpredictableBehavior")
            , joinUnits :: WI.SymFn sym (Ctx.EmptyCtx Ctx.::> UnitType Ctx.::> UnitType) UnitType
            , noop :: WI.SymFn sym Ctx.EmptyCtx UnitType
            }

encodingToFormula :: forall sym t st fs sh
                   . (sym ~ WB.ExprBuilder t st fs)
                  => sym
                  -> UFBundle sym
                  -> Map.Map T.Text (SomeSome (WI.SymFn sym))
                  -> ARM.ARMOpcode ARM.ARMOperand sh
                  -> ASL.Encoding
                  -> IO (Maybe (SF.ParameterizedFormula sym AArch32 sh))
encodingToFormula sym ufBundle symFnEnv opcode enc = case Map.lookup (T.pack $ (ASL.encName enc)) symFnEnv of
  Just (SomeSome symFn) -> do
    case testOpcodeSig sym opcode symFn of
      Right Refl -> Just <$> symFnToParamFormula sym ufBundle opcode symFn
      Left err -> do error $ "testOpcodeSig: " ++ ASL.encName enc ++ "\n" ++ err
  Nothing -> do
    putStrLn $ "Missing function definition for: " ++ ASL.encName enc
    return Nothing

data GlobalParameter sym sh s where
  GlobalParameter :: SF.Parameter AArch32 sh (ASL.GlobalsType s)
                  -> WI.SymExpr sym (ASL.GlobalsType s)
                  -> GlobalParameter sym sh s

-- The second 'GlobalsStruct' here creates an artificial dependency between the bound variables representing
-- the input registers and the fresh bound variables representing the entire initial register state.
getGlobalStruct :: WI.IsSymExprBuilder sym
                => sym
                -> Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx
                -> IO ( ASL.GlobalsStruct (WI.BoundVar sym)
                      , Ctx.Assignment (WI.SymExpr sym) ASL.GPRCtx
                      , Ctx.Assignment (WI.SymExpr sym) ASL.SIMDCtx
                      )
getGlobalStruct sym gbvs = do
  (simple, gprs, simds, mem) <- ASL.destructGlobals gbvs (\(GlobalBoundVar bv _) -> return $ bv)
  gprsBv <- WI.freshBoundVar sym WI.emptySymbol (knownRepr :: WI.BaseTypeRepr (ASL.GlobalsType "GPRS"))
  simdsBv <- WI.freshBoundVar sym WI.emptySymbol (knownRepr :: WI.BaseTypeRepr (ASL.GlobalsType "SIMDS"))


  return $ (ASL.GlobalsStruct simple gprsBv simdsBv mem
           , FC.fmapFC (WI.varExpr sym) gprs
           , FC.fmapFC (WI.varExpr sym) simds)

-- | Add an artificial dependency between expressions.
wrapInSymFn :: WI.IsSymExprBuilder sym
            => sym
            -> WI.SymExpr sym tp
            -> Ctx.Assignment (WI.SymExpr sym) outerargs
            -> IO (WI.SymExpr sym tp)
wrapInSymFn sym expr args = do
  freshBvs <- FC.traverseFC (\arg -> WI.freshBoundVar sym WI.emptySymbol (WI.exprType arg)) args
  symFn <- WI.definedFn sym WI.emptySymbol freshBvs expr WI.NeverUnfold
  WI.applySymFn sym symFn args

-- | Expand the body of a function, ignoring its evaluation criteria
unfoldSymFn :: (sym ~ WB.ExprBuilder t st fs)
            => sym
            -> WI.SymFn sym args ret
            -> Ctx.Assignment (WI.SymExpr sym) args
            -> IO (WI.SymExpr sym ret)
unfoldSymFn sym symFn args = case WB.symFnInfo symFn of
  WB.DefinedFnInfo argBvs expr _ -> WB.evalBoundVars sym expr argBvs args
  _ -> fail "unfoldSymFn: unexpected function kind"

getUsedBvsOfExpr :: (sym ~ WB.ExprBuilder t st fs)
                 => WI.SymExpr sym tp
                 -> IO (Set (Some (WI.BoundVar sym)))
getUsedBvsOfExpr expr = liftM (Set.unions . map snd) $ ST.stToIO $ H.toList =<< WB.boundVars expr

-- | Extract a parameterized formula from a given function.
symFnToParamFormula :: forall sym t st fs sh
                     . (sym ~ WB.ExprBuilder t st fs)
                    => sym
                    -> UFBundle sym
                    -> ARM.ARMOpcode ARM.ARMOperand sh
                    -> WI.SymFn sym (FnArgSig sh) (WI.BaseStructType ASL.StructGlobalsCtx)
                    -> IO (SF.ParameterizedFormula sym AArch32 sh)
symFnToParamFormula sym (UFBundle {..}) opcode symFn = do
  opvars <- mkOperandVars sym opcode
  gbvars <- mkGlobalVars sym
  (gbPreStructBvs, allGPRVars, allSIMDVars) <- getGlobalStruct sym gbvars

  gbStructExpr <- gbStructToExpr gbPreStructBvs
  argbvs <- return $ FC.fmapFC (WI.varExpr sym) $ bvsToAsn opvars

  exprRaw <- unfoldSymFn sym symFn (argbvs Ctx.:> gbStructExpr)
  usedParams <- getUsedParams gbvars gbPreStructBvs allGPRVars allSIMDVars exprRaw

  iGPRs <- WI.applySymFn sym initGPRs Ctx.empty
  iSIMDs <- WI.applySymFn sym initSIMDs Ctx.empty
  iMemory <- WI.applySymFn sym initMemory Ctx.empty

  expr <- WB.evalBoundVars sym exprRaw
            (Ctx.empty Ctx.:> ASL.sGPRs gbPreStructBvs Ctx.:> ASL.sSIMDs gbPreStructBvs Ctx.:> ASL.sMem gbPreStructBvs)
            (Ctx.empty Ctx.:> iGPRs Ctx.:> iSIMDs Ctx.:> iMemory)

  let WI.BaseStructRepr structRepr = WI.exprType expr
  structExprs <- ASL.toGlobalsStruct <$> Ctx.traverseWithIndex (\idx _ -> WI.structField sym expr idx) structRepr

  glbParams <- ASL.flattenGlobalsStruct structExprs
    (\ref expr' -> do
       expr'' <- case ref of
         _ | Just Refl <- testEquality ref (ASL.knownSimpleGlobalRef @"__AssertionFailure") ->
               WI.applySymFn sym updateAssert (Ctx.singleton expr')
         _ | Just Refl <- testEquality ref (ASL.knownSimpleGlobalRef @"__UndefinedBehavior") ->
               WI.applySymFn sym updateUndefB (Ctx.singleton expr')
         _ | Just Refl <- testEquality ref (ASL.knownSimpleGlobalRef @"__UnpredictableBehavior") ->
               WI.applySymFn sym updateUnpredB (Ctx.singleton expr')
         _ -> return expr'
       return $ GlobalParameter (SF.LiteralParameter (ARM.Location (ASL.SimpleGlobalRef ref))) expr'')
    (\ref expr' -> ASL.withGPRRef ref $ \n -> do
        gprExpr <- getGPR n expr' allGPRVars
        return $ GlobalParameter (SF.LiteralParameter (ARM.Location (ASL.GPRRef ref))) gprExpr)
    (\ref expr' -> ASL.withSIMDRef ref $ \n -> do
        simdExpr <- getSIMD n expr' allSIMDVars
        return $ GlobalParameter (SF.LiteralParameter (ARM.Location (ASL.SIMDRef ref))) simdExpr)
    (\expr' -> do
        expr'' <- WI.applySymFn sym updateMemory (Ctx.singleton expr') 
        return $ GlobalParameter (SF.LiteralParameter (ARM.Location ASL.MemoryRef)) expr'')

  defs' <- MapF.fromList . catMaybes . FC.toListFC (\(Const c) -> c) <$> Ctx.zipWithM filterTrivial gbvars glbParams

  gprsExpr' <- do
    gprsExpr <- return $ ASL.sGPRs structExprs
    case gprsExpr of
      WB.NonceAppExpr nae
        | WB.FnApp symFn' _args <- WB.nonceExprApp nae
        , Just Refl <- testEqualitySymFn symFn' initGPRs
        -> return Nothing
      _ -> Just <$> WI.applySymFn sym updateGPRs (Ctx.singleton gprsExpr)

  simdsExpr' <- do
    simdsExpr <- return $ ASL.sSIMDs structExprs
    case simdsExpr of
      WB.NonceAppExpr nae
        | WB.FnApp symFn' _args <- WB.nonceExprApp nae
        , Just Refl <- testEqualitySymFn symFn' initSIMDs
        -> return Nothing
      _ -> Just <$> WI.applySymFn sym updateSIMDs (Ctx.singleton simdsExpr)

  let statefulOp = case (gprsExpr', simdsExpr') of
        (Nothing, Nothing) -> False
        _ -> True

  noopApplied <- WI.applySymFn sym noop Ctx.empty
  let gprsExpr = fromMaybe noopApplied gprsExpr'
  let simdsExpr = fromMaybe noopApplied simdsExpr'

  dummyExpr <- WI.applySymFn sym joinUnits (Ctx.empty Ctx.:> gprsExpr Ctx.:> simdsExpr)

  let defs = case statefulOp of
        True ->  MapF.insert (SF.LiteralParameter (ARM.Location (ASL.knownGlobalRef @"__DummyValue"))) dummyExpr defs'
        False -> defs'

  let litVarMap = getLiteralVarMap gbvars

  let params = mkOpParams (typeRepr opcode) `Set.union` usedParams
  return $ SF.ParameterizedFormula
             { SF.pfUses = params
             , SF.pfOperandVars = opvars
             , SF.pfLiteralVars = litVarMap
             , SF.pfDefs =defs
             }

  where
    gbStructToExpr :: ASL.GlobalsStruct (WI.BoundVar sym)
                   -> IO (WI.SymExpr sym (WI.BaseStructType ASL.StructGlobalsCtx))
    gbStructToExpr (ASL.GlobalsStruct simple allgprs allsimds mem) = do
      WI.mkStruct sym (FC.fmapFC (WI.varExpr sym) (simple Ctx.:> allgprs Ctx.:> allsimds Ctx.:> mem))

    -- | We can drop any output parameters that trivially pass through their inputs
    filterTrivial :: forall s
                   . GlobalBoundVar sym s
                  -> GlobalParameter sym sh s
                  -> IO (Const (Maybe (Pair (SF.Parameter AArch32 sh) (WI.SymExpr sym))) s)
    filterTrivial (GlobalBoundVar bv _) (GlobalParameter param expr) =
      case asBoundVar expr of
        Just bv' | bv == bv' -> return $ Const $ Nothing
        _ -> return $ Const $ Just $ (Pair param expr)

    -- | Here we rewrite the each initial register state bound variable
    -- into a dummy expression that forces a dependency on all the corresponding
    -- bound variables representing all of those globals
    getUsedParams :: Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx
                  -> ASL.GlobalsStruct (WI.BoundVar sym)
                  -> Ctx.Assignment (WI.SymExpr sym) ASL.GPRCtx
                  -> Ctx.Assignment (WI.SymExpr sym) ASL.SIMDCtx
                  -> WI.SymExpr sym (WI.BaseStructType ASL.StructGlobalsCtx)
                  -> IO (Set.Set (Some (SF.Parameter AArch32 sh)))
    getUsedParams gbvars preBvs allGPRVars allSIMDVars expr = do
      allGPRs <- wrapInSymFn sym (WI.varExpr sym (ASL.sGPRs preBvs)) allGPRVars
      allSIMDs <- wrapInSymFn sym (WI.varExpr sym (ASL.sSIMDs preBvs)) allSIMDVars
      expr' <- WB.evalBoundVars sym expr
                 (Ctx.empty Ctx.:> ASL.sGPRs preBvs Ctx.:> ASL.sSIMDs preBvs)
                 (Ctx.empty Ctx.:> allGPRs Ctx.:> allSIMDs)
      usedBvs <- getUsedBvsOfExpr expr'

      let
        filterUsed :: forall s. GlobalBoundVar sym s -> Maybe (Some (SF.Parameter AArch32 sh))
        filterUsed (GlobalBoundVar bv ref) =
            if Set.member (Some bv) usedBvs then
              Just $ Some $ SF.LiteralParameter (ARM.Location ref)
            else Nothing

      return $ Set.fromList $ catMaybes $ FC.toListFC filterUsed gbvars

asBoundVar :: WB.Expr t tp
           -> Maybe (WB.ExprBoundVar t tp)
asBoundVar e = case e of
  WB.AppExpr appExpr -> case WB.appExprApp appExpr of
    WB.StructField struct idx _ -> do
      flds <- asStructCtor struct
      asBoundVar $ flds Ctx.! idx
    _ -> Nothing
  WB.BoundVarExpr bv -> return bv
  _ -> Nothing

asStructCtor :: WB.Expr t (WI.BaseStructType ctx)
             -> Maybe (Ctx.Assignment (WB.Expr t) ctx)
asStructCtor e = case WB.asApp e of
  Just (WB.StructCtor _ flds) -> return flds
  Just (WB.StructField struct idx _) -> do
    flds <- asStructCtor struct
    asStructCtor $ flds Ctx.! idx
  _ -> Nothing

symFnToFunFormula :: forall sym args ret
                 . SymFnsHaveBVs sym
                => sym
                -> T.Text
                -> WI.SymFn sym args ret
                -> (String, SF.FunctionFormula sym '(TL.FromContextFwd args, ret))
symFnToFunFormula sym name symFn
  | (argtps, argvars) <- mkArgLists
  , Refl <- TL.toFromCtxFwd (WI.fnArgTypes symFn)
  = (T.unpack name, SF.FunctionFormula
        { SF.ffName = T.unpack name
        , SF.ffArgTypes = argtps
        , SF.ffArgVars = argvars
        , SF.ffRetType = WI.fnReturnType symFn
        , SF.ffDef = symFn
        })
  where
    mkArgLists :: ( SL.List WI.BaseTypeRepr (TL.FromContextFwd args)
                  , SL.List (WI.BoundVar sym) (TL.FromContextFwd args)
                  )
    mkArgLists = case fnBoundVars sym symFn of
      Just vars -> (TL.fromAssignmentFwd (WI.fnArgTypes symFn), TL.fromAssignmentFwd vars)
      _ -> error $ "mkArgLists: unexpected function: " ++ T.unpack name

sanitizedName :: String -> String
sanitizedName name = map (\c -> case c of ' ' -> '_'; '.' -> '_'; _ -> c) name


formulaEnv :: forall sym arch
            . (A.Architecture arch, WI.IsSymExprBuilder sym)
           => Proxy arch
           -> sym
           -> IO (SF.FormulaEnv sym arch)
formulaEnv proxy sym = do
  undefinedBit <- WI.freshConstant sym (U.makeSymbol "undefined_bit") knownRepr
  ufs <- Map.fromList <$> mapM toUF (A.uninterpretedFunctions proxy)
  return SF.FormulaEnv { SF.envFunctions = ufs
                       , SF.envUndefinedBit = undefinedBit
                       }
  where
    toUF :: A.UninterpFn arch
         -> IO (String, (SF.SomeSome (WI.SymFn sym), Some WI.BaseTypeRepr))
    toUF (A.MkUninterpFn name args ret _) = do
      let ufname = "uf_" ++ name
      uf <- SF.SomeSome <$> WI.freshTotalUninterpFn sym (U.makeSymbol ufname) args ret
      return (sanitizedName ufname, (uf, Some ret))

getParserEnv :: forall sym. SF.FormulaEnv sym AArch32 -> Map.Map T.Text (SomeSome (WI.SymFn sym))
getParserEnv (SF.FormulaEnv env _) = Map.fromList $ map reshape $ Map.assocs env
  where
    reshape :: (String, (SF.SomeSome (WI.SymFn sym), Some WI.BaseTypeRepr)) -> (T.Text, (SomeSome (WI.SymFn sym)))
    reshape (nm, (SF.SomeSome symfn, _)) = (T.pack $ nm, SomeSome symfn)

mkFormula :: forall sym t st fs
           . (sym ~ WB.ExprBuilder t st fs)
          => sym
          -> (T.Text, SomeSome (WI.SymFn sym))
          -> (String, Some (SF.FunctionFormula sym))
mkFormula sym (nm, SomeSome symFn) =
  let (nm', formula) = symFnToFunFormula sym nm symFn
  in (nm', Some formula)

data ASLSemantics sym = ASLSemantics
  { a32Semantics :: MapF.MapF (A32.Opcode A32.Operand) (SF.ParameterizedFormula sym AArch32)
  , t32Semantics :: MapF.MapF (T32.Opcode T32.Operand) (SF.ParameterizedFormula sym AArch32)
  , funSemantics :: SF.Library sym
  }

testEqualitySymFn :: WB.ExprSymFn t (WB.Expr t) args ret
                  -> WB.ExprSymFn t (WB.Expr t) args' ret'
                  -> Maybe (args Ctx.::> ret :~: args' Ctx.::> ret')
testEqualitySymFn symFn symFn' = testEquality (WB.symFnId symFn) (WB.symFnId symFn')

mkUFBundle :: forall sym t st fs arch
            . (sym ~ WB.ExprBuilder t st fs)
           => sym
           -> SF.FormulaEnv sym arch
           -> IO (UFBundle sym)
mkUFBundle sym (SF.FormulaEnv env _) = do
  initGPRs <- getUF "uf_init_gprs" knownRepr knownRepr
  initSIMDs <- getUF "uf_init_simds" knownRepr knownRepr
  initMemory <- getUF "uf_init_memory" knownRepr knownRepr
  updateGPRs <- getUF "uf_update_gprs" knownRepr knownRepr
  updateSIMDs <- getUF "uf_update_simds" knownRepr knownRepr
  updateMemory <- getUF "uf_update_memory" knownRepr knownRepr
  updateAssert <- getUF "uf_update_assert" knownRepr knownRepr
  updateUndefB <- getUF "uf_update_undefB" knownRepr knownRepr
  updateUnpredB <- getUF "uf_update_unpredB" knownRepr knownRepr
  joinUnits <- getUF "uf_join_units" knownRepr knownRepr
  noop <- getUF "uf_noop" knownRepr knownRepr


  getGPRBase <- getUF "uf_gpr_get"
    (knownRepr :: Ctx.Assignment WI.BaseTypeRepr (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "GPRS" Ctx.::> WI.BaseBVType 4))
    (knownRepr :: WI.BaseTypeRepr (WI.BaseBVType 32))
  getSIMDBase <- getUF "uf_simd_get"
    (knownRepr :: Ctx.Assignment WI.BaseTypeRepr (Ctx.EmptyCtx Ctx.::> ASL.GlobalsType "SIMDS" Ctx.::> WI.BaseBVType 8))
    (knownRepr :: WI.BaseTypeRepr (WI.BaseBVType 128))

  let
    getGPR :: n <= ASL.MaxGPR
           => NR.NatRepr n
           -> WB.Expr t (ASL.GlobalsType "GPRS")
           -> Ctx.Assignment (WB.SymExpr sym) ASL.GPRCtx
           -> IO (WB.Expr t (WI.BaseBVType 32))
    getGPR n expr allGPRs
      | WB.NonceAppExpr nae <- expr
      , WB.FnApp symFn _args <- WB.nonceExprApp nae
      , Just Refl <- testEqualitySymFn symFn initGPRs
      , idx <- natReplicatedIndex ASL.maxGPRRepr n (Ctx.size allGPRs)
      = return $ allGPRs Ctx.! idx
    getGPR n expr _ = do
      idxBv <- WI.bvLit sym (NR.knownNat @4) (BVS.mkBV NR.knownNat (NR.intValue n))
      WI.applySymFn sym getGPRBase (Ctx.empty Ctx.:> expr Ctx.:> idxBv)

    getSIMD :: n <= ASL.MaxSIMD
            => NR.NatRepr n
            -> WB.Expr t (ASL.GlobalsType "SIMDS")
            -> Ctx.Assignment (WB.SymExpr sym) ASL.SIMDCtx
            -> IO (WB.Expr t (WI.BaseBVType 128))
    getSIMD n expr allSIMDs
      | WB.NonceAppExpr nae <- expr
      , WB.FnApp symFn _args <- WB.nonceExprApp nae
      , Just Refl <- testEqualitySymFn symFn initSIMDs
      , idx <- natReplicatedIndex ASL.maxSIMDRepr n (Ctx.size allSIMDs)
      = return $ allSIMDs Ctx.! idx
    getSIMD n expr _ = do
      idxBv <- WI.bvLit sym (NR.knownNat @8) (BVS.mkBV NR.knownNat (NR.intValue n))
      WI.applySymFn sym getSIMDBase (Ctx.empty Ctx.:> expr Ctx.:> idxBv)

  return $ UFBundle getGPR getSIMD initGPRs initSIMDs initMemory updateGPRs updateSIMDs updateMemory updateAssert updateUndefB updateUnpredB joinUnits noop
  where
    getUF :: String -> Ctx.Assignment WI.BaseTypeRepr args -> WI.BaseTypeRepr ret -> IO (WI.SymFn sym args ret)
    getUF nm argsT retT = case Map.lookup nm env of
      Just (SF.SomeSome symFn, _)
       | Just Refl <- testEquality (WI.fnArgTypes symFn) argsT
       , Just Refl <- testEquality (WI.fnReturnType symFn) retT
       -> return symFn
      _ -> fail $ "mkUFBundle: missing uninterpreted function: " ++ nm

data ASLSemanticsOpts =
  ASLSemanticsOpts { aslOptTrimRegs :: Bool 
                   -- ^ only emit a single register 'Location' update
                   }

postProcess :: forall sym sh
             . ASLSemanticsOpts
            -> SF.ParameterizedFormula sym AArch32 sh
            -> SF.ParameterizedFormula sym AArch32 sh
postProcess opts pf =
  case aslOptTrimRegs opts of
    True ->
      let
        filt :: Pair (SF.Parameter AArch32 sh) (WI.SymExpr sym) -> Bool
        filt (Pair (SF.LiteralParameter (ARM.Location ref)) _expr) = case ref of
          ASL.GPRRef _ -> False
          ASL.SIMDRef _ -> False
          _ -> True
        filt _ = error "postProcess: filt: invalid arguments"

        defs' = MapF.fromList $ filter filt $ MapF.toList $ SF.pfDefs pf
      in pf { SF.pfDefs = defs' }
    False -> pf

loadSemantics :: forall t fs
               . CBS.SimpleBackend t fs
              -> ASLSemanticsOpts
              -> IO (ASLSemantics (CBS.SimpleBackend t fs))
loadSemantics sym opts = do
  initenv <- formulaEnv (Proxy @ARM.AArch32) sym
  ufBundle <- mkUFBundle sym initenv

  funcFormulas <- ASL.getFunctionFormulas sym (getParserEnv initenv)
  funcEnv <- return $ Map.fromList funcFormulas

  instrFormulas <- ASL.getInstructionFormulas sym funcEnv
  instrEnv <- return $ Map.fromList instrFormulas

  a32pfs <- liftM catMaybes $ forM (Map.assocs A32.aslEncodingMap) $ \(Some a32opcode, enc) -> do
    result <- encodingToFormula sym ufBundle instrEnv (ARM.A32Opcode a32opcode) enc
    return $ fmap (\pf -> (Pair a32opcode (postProcess opts pf))) result
  t32pfs <- liftM catMaybes $ forM (Map.assocs T32.aslEncodingMap) $ \(Some t32opcode, enc) -> do
    result <- encodingToFormula sym ufBundle instrEnv (ARM.T32Opcode t32opcode) enc
    return $ fmap (\pf -> (Pair t32opcode (postProcess opts pf))) result

  let fformulas = map (mkFormula sym) funcFormulas

  let addFunctionFormula :: (String, Some (SF.FunctionFormula (CBS.SimpleBackend t fs)))
                         -> MapF.MapF SF.FunctionRef (SF.FunctionFormula (CBS.SimpleBackend t fs))
                         -> MapF.MapF SF.FunctionRef (SF.FunctionFormula (CBS.SimpleBackend t fs))
      addFunctionFormula (_nm, Some f) l =
        MapF.insert (SF.functionRef f) f l
  let lib = foldr addFunctionFormula SF.emptyLibrary fformulas
  return $ ASLSemantics { a32Semantics = MapF.fromList a32pfs
                        , t32Semantics = MapF.fromList t32pfs
                        , funSemantics = lib
                        }
