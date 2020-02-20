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

-- | Converting the output of the asl translator into semmc formulas

module SemMC.Architecture.ARM.ASL
  ( encodingToFormula
  , symFnToFunFormula
  , ASLSemantics(..)
  , loadSemantics
  , attachSemantics
  ) where

import           Data.Kind
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Control.Applicative ( Const(..) )
import           Control.Monad.Identity
import qualified Control.Monad.Except as ME
import           Control.Monad ( liftM, forM )
import           Data.Proxy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as UBS
import qualified System.IO.Unsafe as IO

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Type.List as TL
import qualified Data.Parameterized.TyMap as TM
import           Data.Parameterized.SymbolRepr ( Symbol )
import qualified Data.Parameterized.SymbolRepr as SR
import           Data.Parameterized.Classes
import           Data.Parameterized.List as SL
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some
import           Data.Parameterized.Pair
import           Data.Parameterized.HasRepr
import           Data.Parameterized.Lift ( LiftF(..) )

import qualified Lang.Crucible.Backend.Simple as CB
import qualified SemMC.BoundVar as BV
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Formula.Env as SF
import qualified SemMC.Formula.Printer as SF
import qualified SemMC.Util as U

import qualified Dismantle.ARM.T32 as T32
import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.ASL as ASL

import           SemMC.Architecture.AArch32 ( AArch32 )
import qualified SemMC.Architecture.AArch32 as ARM
import qualified SemMC.Architecture.ARM.Location as ARM
import           SemMC.Architecture.ARM.Combined ( ARMOperandRepr )
import qualified SemMC.Architecture.ARM.Combined as ARM

import qualified Language.ASL.Globals as ASL
import qualified Language.ASL.Formulas as ASL

import qualified What4.Interface as WI
import qualified What4.Expr.Builder as WB
import           What4.Utils.Util ( SomeSome(..) )
import qualified What4.Utils.Log as Log
import qualified What4.Serialize.Parser as WP

data OperandTypeWrapper (arch :: Type) :: TL.TyFun Symbol WI.BaseType -> Type
type instance TL.Apply (OperandTypeWrapper arch) s = A.OperandType arch s
type OperandTypes arch sh = TL.Map (OperandTypeWrapper arch) sh
type OperandTypesCtx arch sh = TL.ToContextFwd (TL.Map (OperandTypeWrapper arch) sh)

data GlobalsTypeWrapper :: TL.TyFun Symbol WI.BaseType -> Type
type instance TL.Apply GlobalsTypeWrapper s = ASL.GlobalsType s


_testGlobalSyms :: Ctx.Assignment f (TM.MapContext GlobalsTypeWrapper ASL.GlobalSymsCtx) :~: Ctx.Assignment f ASL.GlobalsCtx
_testGlobalSyms = Refl


shapeOfOperands :: SL.List ARMOperandRepr sh -> SL.List WI.BaseTypeRepr (OperandTypes AArch32 sh)
shapeOfOperands SL.Nil = SL.Nil
shapeOfOperands (a SL.:< oprepr) = ARM.shapeReprType a SL.:< shapeOfOperands oprepr


mkOperandVars :: forall sym sh
               . WI.IsSymExprBuilder sym
              => sym
              -> ARM.ARMOpcode ARM.ARMOperand sh
              -> IO (SL.List (BV.BoundVar sym AArch32) sh)
mkOperandVars sym op = do
  FC.traverseFC mkBV (typeRepr op)
  where
    mkBV :: ARMOperandRepr s -> IO (BV.BoundVar sym AArch32 s)
    mkBV opRep = do
      let ty = ARM.shapeReprType opRep
      BV.BoundVar <$> WI.freshBoundVar sym (WI.safeSymbol $ A.operandTypeReprSymbol (Proxy @AArch32) opRep) ty

type FnArgSig sh = (OperandTypesCtx AArch32 sh Ctx.::> WI.BaseStructType ASL.GlobalsCtx)

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
                    :~: (WI.BaseTypeRepr (WI.BaseStructType (FnArgSig sh)), (WI.BaseTypeRepr (WI.BaseStructType ASL.GlobalsCtx))))
testOpcodeSig _ op symfn = do
  Refl <- testEqualityErr (operandsToAsn (typeRepr op) Ctx.:> WI.BaseStructRepr ASL.trackedGlobalBaseReprs) (WI.fnArgTypes symfn) 
  Refl <- testEqualityErr (WI.BaseStructRepr ASL.trackedGlobalBaseReprs) (WI.fnReturnType symfn)
  return Refl

data GlobalBoundVar sym s =
  GlobalBoundVar { unGBoundVar :: WI.BoundVar sym (ASL.GlobalsType s) }


operandsToAsn :: SL.List ARMOperandRepr sh -> Ctx.Assignment WI.BaseTypeRepr (OperandTypesCtx AArch32 sh)
operandsToAsn repr = TL.toAssignmentFwd (shapeOfOperands repr)

bvsToAsn :: SL.List (BV.BoundVar sym AArch32) sh
         -> Ctx.Assignment (WI.BoundVar sym) (OperandTypesCtx AArch32 sh)
bvsToAsn bvs = TL.toAssignmentFwd $ TM.applyMapList (Proxy @(OperandTypeWrapper AArch32)) BV.unBoundVar bvs

unGBoundVarAsn :: WI.IsSymExprBuilder sym
               => sym
               -> Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx
               -> Ctx.Assignment (WI.BoundVar sym) ASL.GlobalsCtx
unGBoundVarAsn sym gbvs = TM.applyMapContext (Proxy @GlobalsTypeWrapper) unGBoundVar gbvs


mkGlobalVars :: forall sym
              . WI.IsSymExprBuilder sym
             => sym -> IO (Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx)
mkGlobalVars sym = FC.traverseFC mkBV ASL.allGlobalRefs
  where
    mkBV :: ASL.GlobalRef s -> IO (GlobalBoundVar sym s)
    mkBV gr =
      GlobalBoundVar <$> WI.freshBoundVar sym (WI.safeSymbol $ T.unpack $ SR.symbolRepr $ ASL.globalRefSymbol gr) (ASL.globalRefRepr gr)

getLiteralVarMap :: Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx
                 -> MapF.MapF (L.Location AArch32) (WI.BoundVar sym)
getLiteralVarMap bvs = MapF.fromList $ FC.toListFC (\(Const c) -> c) $
  Ctx.zipWith (\(GlobalBoundVar bv) gr -> Const (Pair (ARM.Location gr) bv)) bvs ASL.allGlobalRefs

getGlobalStruct :: WI.IsSymExprBuilder sym
                => sym
                -> Ctx.Assignment (GlobalBoundVar sym) ASL.GlobalSymsCtx
                -> IO (WI.SymExpr sym (WI.BaseStructType ASL.GlobalsCtx))
getGlobalStruct sym gbvs = do
  let bvs = unGBoundVarAsn sym gbvs
  WI.mkStruct sym (FC.fmapFC (WI.varExpr sym) bvs)


mkOpParams :: SL.List ARMOperandRepr sh -> Set.Set (Some (SF.Parameter AArch32 sh))
mkOpParams bvs =
  Set.fromList $ FC.toListFC (\(Const c) -> c) $
    SL.imap (\idx v -> Const (Some (SF.OperandParameter @AArch32 (ARM.shapeReprType v) idx))) bvs


class WI.IsSymExprBuilder sym => SymFnsHaveBVs sym where
  fnBoundVars :: sym -> WI.SymFn sym args ret -> Maybe (Ctx.Assignment (WI.BoundVar sym) args)
  showSymFn :: sym -> WI.SymFn sym args ret -> String

instance SymFnsHaveBVs (WB.ExprBuilder t st fs) where
  fnBoundVars _sym symFn = case WB.symFnInfo symFn of
      WB.DefinedFnInfo vars _ _ -> Just vars
      _ -> Nothing
  showSymFn _sym symFn = show symFn


encodingToFormula :: forall sym sh
                   . WI.IsSymExprBuilder sym
                  => sym
                  -> Map.Map T.Text (SomeSome (WI.SymFn sym))
                  -> ARM.ARMOpcode ARM.ARMOperand sh
                  -> ASL.Encoding
                  -> IO (Maybe (SF.ParameterizedFormula sym AArch32 sh))
encodingToFormula sym symFnEnv opcode enc = case Map.lookup (T.pack $ (ASL.encName enc)) symFnEnv of
  Just (SomeSome symFn) -> do
    case testOpcodeSig sym opcode symFn of
      Right Refl -> Just <$> symFnToParamFormula sym opcode symFn
      Left err -> do error $ "testOpcodeSig: " ++ ASL.encName enc ++ "\n" ++ err
  Nothing -> do
    putStrLn $ "Missing function definition for: " ++ ASL.encName enc
    return Nothing
    -- let argRepr = operandsToAsn (typeRepr opcode) Ctx.:> WI.BaseStructRepr ASL.trackedGlobalBaseReprs
    -- let retRepr = WI.BaseStructRepr ASL.trackedGlobalBaseReprs
    -- symFn <- WI.freshTotalUninterpFn sym (WI.safeSymbol $ ASL.encName enc) argRepr retRepr
    -- mkFormula sym opcode symFn


symFnToParamFormula :: forall sym sh
                     . WI.IsSymExprBuilder sym
                    => sym
                    -> ARM.ARMOpcode ARM.ARMOperand sh
                    -> WI.SymFn sym (FnArgSig sh) (WI.BaseStructType ASL.GlobalsCtx)
                    -> IO (SF.ParameterizedFormula sym AArch32 sh)
symFnToParamFormula sym opcode symFn = do
  opvars <- mkOperandVars sym opcode
  gbvars <- mkGlobalVars sym
  gbstruct <- getGlobalStruct sym gbvars
  argbvs <- return $ FC.fmapFC (WI.varExpr sym) $ bvsToAsn opvars

  expr <- WI.applySymFn sym symFn (argbvs Ctx.:> gbstruct)
  defs <- (MapF.fromList . FC.toListFC (\(Const c) -> c)) <$> FC.traverseFC (mkPair expr) ASL.allGlobalRefs
  let litVarMap = getLiteralVarMap gbvars
  let params = mkOpParams (typeRepr opcode) `Set.union` (Set.fromList $ MapF.keys defs)
  return $
    SF.ParameterizedFormula
      { SF.pfUses = params
      , SF.pfOperandVars = opvars
      , SF.pfLiteralVars = litVarMap
      , SF.pfDefs = defs
      }
  where
    mkPair :: forall s. WI.SymExpr sym (WI.BaseStructType ASL.GlobalsCtx)
           -> ASL.GlobalRef s
           -> IO (Const (Pair (SF.Parameter AArch32 sh) (WI.SymExpr sym)) s)
    mkPair struct gr = do
      expr <- WI.structField sym struct (ASL.globalRefIndex gr)
      return $ Const $ Pair (SF.LiteralParameter (ARM.Location gr)) expr      

dropUFPrefix :: String -> String
dropUFPrefix nm = case List.stripPrefix "uf." nm of
  Just nm' -> nm'
  Nothing -> nm

symFnToFunFormula :: forall sym args ret
                 . SymFnsHaveBVs sym
                => sym
                -> T.Text
                -> WI.SymFn sym args ret
                -> Maybe (String, SF.FunctionFormula sym '(TL.FromContextFwd args, ret))
symFnToFunFormula sym name symFn = case mkArgLists symFn of
    Just (argtps, argvars) | Refl <- TL.toFromCtxFwd (WI.fnArgTypes symFn) ->
      Just $ (dropUFPrefix $ T.unpack name, SF.FunctionFormula
        { SF.ffName = dropUFPrefix $ T.unpack name
        , SF.ffArgTypes = argtps
        , SF.ffArgVars = argvars
        , SF.ffRetType = WI.fnReturnType symFn
        , SF.ffDef = symFn
        })
    Nothing -> Nothing
  where
    mkArgLists :: WI.SymFn sym args ret
               -> Maybe ( SL.List WI.BaseTypeRepr (TL.FromContextFwd args)
                        , SL.List (WI.BoundVar sym) (TL.FromContextFwd args)
                        )
    mkArgLists symFn = case fnBoundVars sym symFn of
      Just vars -> Just $ (TL.fromAssignmentFwd (WI.fnArgTypes symFn), TL.fromAssignmentFwd vars)
      _ -> Nothing

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
      uf <- SF.SomeSome <$> WI.freshTotalUninterpFn sym (U.makeSymbol name) args ret
      return (("uf." ++ sanitizedName name), (uf, Some ret))

getParserEnv :: forall sym. SF.FormulaEnv sym AArch32 -> Map.Map T.Text (SomeSome (WI.SymFn sym))
getParserEnv (SF.FormulaEnv env _) = Map.fromList $ map reshape $ Map.assocs env
  where
    reshape :: (String, (SF.SomeSome (WI.SymFn sym), Some WI.BaseTypeRepr)) -> (T.Text, (SomeSome (WI.SymFn sym)))
    reshape (nm, (SF.SomeSome symfn, _)) = (T.pack $ nm, SomeSome symfn)

toOpcodePair :: (LiftF a) => (Some a, BS.ByteString) -> TH.ExpQ
toOpcodePair (Some o, bs) = TH.tupE [ [| Some $(liftF o) |], embedByteString bs ]

toFunctionPair :: (String, BS.ByteString) -> TH.ExpQ
toFunctionPair (name, bs) = TH.tupE [ [| $(TH.litE (TH.StringL name)) |], embedByteString bs ]

embedByteString :: BS.ByteString -> TH.ExpQ
embedByteString bs =
  [| IO.unsafePerformIO (UBS.unsafePackAddressLen len $(TH.litE (TH.StringPrimL (BS.unpack bs)))) |]
  where
    len = BS.length bs

mkEncoding :: (sym ~ WB.ExprBuilder t st fs)
           => sym
           -> WP.SymFnEnv sym
           -> ARM.ARMOpcode ARM.ARMOperand sh
           -> ASL.Encoding
           -> IO (Maybe (String, Pair (ARM.ARMOpcode ARM.ARMOperand) (SF.ParameterizedFormula sym AArch32)))
mkEncoding sym symfns opcode enc = do
  encodingToFormula sym symfns opcode enc >>= \case
    Just formula -> return $ Just $ (ASL.encName enc, Pair opcode formula)
    Nothing -> return Nothing

mkFormula :: forall sym t st fs
           . (sym ~ WB.ExprBuilder t st fs)
          => sym
          -> (T.Text, SomeSome (WI.SymFn sym))
          -> Maybe (String, Some (SF.FunctionFormula sym))
mkFormula sym (nm, SomeSome symFn) = do
  (nm', formula) <- symFnToFunFormula sym nm symFn
  return (nm', Some formula)

data ASLSemantics = ASLSemantics
  { a32Semantics :: [(Some (A32.Opcode A32.Operand), BS.ByteString)]
  , t32Semantics :: [(Some (T32.Opcode T32.Operand), BS.ByteString)]
  , funSemantics :: [(String, BS.ByteString)]
  }

loadSemantics :: IO ASLSemantics
loadSemantics = do
  Some ng <- PN.newIONonceGenerator
  sym <- CB.newSimpleBackend CB.FloatIEEERepr ng
  initenv <- formulaEnv (Proxy @ARM.AArch32) sym
  funs <- ASL.getFormulas sym (getParserEnv initenv)
  symfnEnv <- return $ Map.fromList funs
  a32pfs <- liftM catMaybes $ forM (Map.assocs A32.aslEncodingMap) $ \(Some a32opcode, enc) ->
    fmap (\pf -> Pair a32opcode pf) <$> encodingToFormula sym symfnEnv (ARM.A32Opcode a32opcode) enc
  t32pfs <- liftM catMaybes $ forM (Map.assocs T32.aslEncodingMap) $ \(Some t32opcode, enc) ->
    fmap (\pf -> Pair t32opcode pf) <$> encodingToFormula sym symfnEnv (ARM.T32Opcode t32opcode) enc

  a32Bytes <- forM a32pfs $ \(Pair opcode pformula) -> do
    let bs = T.encodeUtf8 $ SF.printParameterizedFormula (typeRepr (ARM.A32Opcode opcode)) pformula
    return (Some opcode, bs)
  t32Bytes <- forM t32pfs $ \(Pair opcode pformula) -> do
    let bs = T.encodeUtf8 $ SF.printParameterizedFormula (typeRepr (ARM.T32Opcode opcode)) pformula
    return (Some opcode, bs)
  let fformulas = catMaybes $ map (mkFormula sym) funs
  defBytes <- forM fformulas $ \(nm, Some fformula) -> do
    let bs = T.encodeUtf8 $ SF.printFunctionFormula fformula
    return (nm, bs)

  return $ ASLSemantics a32Bytes t32Bytes defBytes


attachSemantics :: TH.ExpQ
attachSemantics = do
  ASLSemantics a32sem t32sem funsem <- TH.runIO $ loadSemantics
  [| ASLSemantics $(TH.listE (map toOpcodePair a32sem))
                  $(TH.listE (map toOpcodePair t32sem))
                  $(TH.listE (map toFunctionPair funsem)) |]
