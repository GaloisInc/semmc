{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}

module SemMC.ASL (
    simulateFunction
  , simulateInstruction
  , SimulatorConfig(..)
  , SimulationException(..)
  ) where

import           GHC.TypeLits ( Symbol )
import qualified Data.Type.List as TL

import qualified Control.Exception as X
import           Control.Lens ( (^.) )
import           Data.Monoid
import qualified Control.Monad.State.Class as MS
import qualified Control.Monad.State as MS
import           Control.Applicative ( (<|>) )
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.ST ( RealWorld )
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Time.Clock
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Data.Type.List as TL
import           Data.Maybe ( fromMaybe, catMaybes, maybeToList, mapMaybe )
-- import qualified Dismantle.XML.AArch32 as DA
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Expr as CCE
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.CallFrame as CSC
import qualified Lang.Crucible.Simulator.GlobalState as CSG
import qualified Lang.Crucible.Types as CT
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Simulator.PathSatisfiability as CSP
import qualified System.IO as IO
import qualified What4.Config as WC
import qualified What4.BaseTypes as WT
import qualified What4.Interface as WI
import qualified What4.Symbol as WS
import qualified What4.Protocol.Online as WPO

import qualified What4.Interface as S
import qualified What4.Expr as S
import qualified What4.Expr.Builder as WEB

import qualified SemMC.Formula as SF
import qualified SemMC.ASL.Crucible as AC
import qualified SemMC.ASL.Signature as AS
import qualified SemMC.ASL.Types as AT

import           SemMC.Architecture.ARM.Location ( A32, T32 )
import qualified SemMC.Architecture.ARM.Location as AL
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Architecture as A
import qualified SemMC.BoundVar as BV


data SimulatorConfig scope =
  SimulatorConfig { simOutputHandle :: IO.Handle
                  , simHandleAllocator :: CFH.HandleAllocator
                  , simSym :: CBO.YicesOnlineBackend scope (WEB.Flags WEB.FloatReal)
                  }

type OnlineSolver scope sym = sym ~ CBO.YicesOnlineBackend scope (WEB.Flags WEB.FloatReal)

reshape :: Ctx.Assignment CT.BaseTypeRepr btps -> PL.List WT.BaseTypeRepr (AT.ToBaseTypesListBase btps)
reshape Ctx.Empty = PL.Nil
reshape (reprs Ctx.:> brepr) = brepr PL.:< reshape reprs

freshRegEntries :: Ctx.Assignment (FreshArg sym) btps -> Ctx.Assignment (CS.RegEntry sym) (AT.ToCrucTypes btps)
freshRegEntries Ctx.Empty = Ctx.Empty
freshRegEntries (fargs Ctx.:> farg) = (freshRegEntries fargs Ctx.:> (freshArgEntry farg))


genSimulation :: forall arch sym init globalReads globalWrites tps scope a
               . (CB.IsSymInterface sym, OnlineSolver scope sym)
              => SimulatorConfig scope
              -> AC.Function arch globalReads globalWrites init tps
              -> (CS.RegEntry sym (AS.FuncReturn globalWrites tps)
                  -> Ctx.Assignment (FreshArg sym) init
                  -> Ctx.Assignment (FreshArg sym) globalReads
                  -> IO a)
              -> IO a
genSimulation symCfg crucFunc extractResult =
  case AC.funcCFG crucFunc of
    CCC.SomeCFG cfg -> do
      let sig = AC.funcSig crucFunc
      let argReprNames = FC.fmapFC (\(AT.LabeledValue (AS.FunctionArg nm _ _) v) -> AT.LabeledValue nm v) (AC.funcArgReprs sig)
      initArgs <- FC.traverseFC (allocateFreshArg (simSym symCfg)) argReprNames
      let retRepr = AS.funcSigRepr sig
      let econt = CS.runOverrideSim retRepr $ do
            re <- CS.callCFG cfg (CS.RegMap $ freshRegEntries initArgs)
            return (CS.regValue re)
      let globalReads = AC.funcGlobalReads crucFunc
      (freshGlobalReads, globalState) <- initGlobals symCfg globalReads
      s0 <- initialSimulatorState symCfg globalState econt retRepr
      ft <- executionFeatures (AS.funcName $ AC.funcSig crucFunc) (simSym symCfg)
      eres <- CS.executeCrucible ft s0
      case eres of
        CS.TimeoutResult {} -> X.throwIO (SimulationTimeout (Some (AC.SomeFunctionSignature sig)))
        CS.AbortedResult context ab -> X.throwIO $ SimulationAbort (Some (AC.SomeFunctionSignature sig)) (showAbortedResult ab)
        CS.FinishedResult _ pres ->
          case pres of
            CS.TotalRes gp -> extractResult (gp ^. CS.gpValue) initArgs freshGlobalReads
            CS.PartialRes _ _ gp _ -> extractResult (gp ^. CS.gpValue) initArgs freshGlobalReads

-- | Simulate a function
--
-- We have to maintain the mapping between exprs and the global
-- location (i.e., register or memory) that is being updated by the function.  This is also
-- suitable for the top-level instruction semantics, which operate in the same way (but take no
-- arguments)
simulateFunction :: forall arch sym init globalReads globalWrites tps scope
                  . (CB.IsSymInterface sym, OnlineSolver scope sym)
                 => SimulatorConfig scope
                 -> AC.Function arch globalReads globalWrites init tps
                 -> IO (SF.FunctionFormula sym '(AT.ToBaseTypesListBase init, WT.BaseStructType (AS.FuncReturnCtx globalWrites tps)))
simulateFunction symCfg crucFunc = genSimulation symCfg crucFunc extractResult
  where
    extractResult re initArgs _ =
      let sig = AC.funcSig crucFunc
          globalWriteTypes = WT.BaseStructRepr $ FC.fmapFC AT.projectValue (AS.funcGlobalWriteReprs sig)
          naturalRetType = WT.BaseStructRepr $ AS.funcRetRepr sig
          retType = WT.BaseStructRepr (Ctx.empty Ctx.:> globalWriteTypes Ctx.:> naturalRetType )
      in case CT.asBaseType (CS.regType re) of
        CT.NotBaseType -> X.throwIO (NonBaseTypeReturn (CS.regType re))
        CT.AsBaseType btr
          | Just Refl <- testEquality btr retType -> do
              -- print (WI.printSymExpr (CS.regValue re))
              let name = T.unpack (AS.funcName sig)
                  argTypes = reshape (FC.fmapFC AT.projectValue (AS.funcArgReprs sig))
                  argVars = freshArgBoundVars' initArgs
                  solverSymbolName = case WI.userSymbol name of
                    Left err -> error (show err)
                    Right symbol -> symbol
              fn <- WI.definedFn
                (simSym symCfg)
                solverSymbolName
                (freshArgBoundVars initArgs)
                (CS.regValue re)
                (const False)
              return $ SF.FunctionFormula name argTypes argVars retType fn
          | otherwise -> X.throwIO (UnexpectedReturnType btr)


simulateInstruction :: forall sym init globalReads globalWrites tps scope
                     . (CB.IsSymInterface sym, OnlineSolver scope sym)
                    => SimulatorConfig scope
                    -> AC.Function A32 globalReads globalWrites init tps
                    -> IO (Some (SF.ParameterizedFormula sym A32))
simulateInstruction symCfg crucFunc = genSimulation symCfg crucFunc extractResult
  where
    sig = AC.funcSig crucFunc
    extractResult re initArgs freshGlobals =
      let sig = AC.funcSig crucFunc
          labeledInitArgs = AT.addLabels (AS.funcArgReprs sig) initArgs
          globalWriteTypes = WT.BaseStructRepr $ FC.fmapFC AT.projectValue (AS.funcGlobalWriteReprs sig)
          naturalRetType = WT.BaseStructRepr $ AS.funcRetRepr sig
          retType = WT.BaseStructRepr (Ctx.empty Ctx.:> globalWriteTypes Ctx.:> naturalRetType )
      in case CT.asBaseType (CS.regType re) of
        CT.NotBaseType -> X.throwIO (NonBaseTypeReturn (CS.regType re))
        CT.AsBaseType btr
          | Just Refl <- testEquality btr retType -> do
              let sym = simSym symCfg
              -- print (WI.printSymExpr (CS.regValue re))
              let name = T.unpack (AS.funcName sig)
                  argTypes = reshape (FC.fmapFC AT.projectValue (AS.funcArgReprs sig))
                  argVars = freshArgBoundVars' initArgs
                  solverSymbolName = case WI.userSymbol name of
                    Left err -> error (show err)
                    Right symbol -> symbol
              naturalRetStruct <- WI.structField sym (CS.regValue re) Ctx.i2of2
              Some opcol <- FC.foldrMFC' (collectOperand sym naturalRetType naturalRetStruct) (Some emptyOperandCollector) labeledInitArgs

              let globalReads = collectGlobalReads (mkFreshArgMap freshGlobals)

              globalRetStruct <- WI.structField sym (CS.regValue re) Ctx.i1of2
              globalWrites <- Ctx.traverseAndCollect
                (collectGlobalWrites sym globalWriteTypes globalRetStruct globalReads)
                  (AS.funcGlobalWriteReprs sig)

              let uses = Set.fromList $ map (\(ParamExpr param _) -> Some param) (opParams opcol)
                    ++ map (\(LocExpr _ loc _ _) -> Some (SF.LiteralParameter loc)) (Map.elems globalReads)
              let vars = opBVs opcol
              let lits = MapF.fromList $ map (\(LocExpr _ loc bv _) -> Pair loc bv) (Map.elems globalReads)
              let opExprs = MapF.fromList $
                    mapMaybe (\(ParamExpr param (Just expr)) ->
                                return (Pair param expr)) (opParams opcol)
                    ++ mapMaybe (\(LocExpr _ loc _ (Just expr)) ->
                                   return (Pair (SF.LiteralParameter loc) expr)) (Map.elems globalWrites)

              return $ Some $ SF.ParameterizedFormula {
                  SF.pfUses = uses
                , SF.pfOperandVars = vars
                , SF.pfLiteralVars = lits
                , SF.pfDefs = opExprs
                }

          | otherwise -> X.throwIO (UnexpectedReturnType btr)

    collectGlobalWrites :: forall tp ctx
                         . sym
                        -> WT.BaseTypeRepr (WT.BaseStructType ctx)
                        -> S.SymExpr sym (WT.BaseStructType ctx) -- globals result struct
                        -> Map.Map T.Text (LocExpr sym A32)
                        -> Ctx.Index ctx tp
                        -> (AC.LabeledValue T.Text WT.BaseTypeRepr) tp
                        -> IO (Map.Map T.Text (LocExpr sym A32))
    collectGlobalWrites sym globRepr globExpr readMap ix (AC.LabeledValue gName repr) =
      case Map.lookup gName readMap of
        Nothing -> return Map.empty
        Just (LocExpr locRepr loc bv _) -> do
          globStructVal <- WI.structField sym globExpr ix
          let globStructFieldType = WI.exprType globStructVal
          case testEquality globStructFieldType locRepr of
            Nothing -> error $ "Mismatch in global read vs. write type" ++ show globStructFieldType
            Just Refl ->
              return $ Map.singleton gName $
                LocExpr locRepr loc bv (Just globStructVal)

    collectGlobalReads :: Map.Map T.Text (Some (FreshArg sym))
                       -> Map.Map T.Text (LocExpr sym A32)
    collectGlobalReads =
      Map.mapMaybeWithKey (\k (Some freshArg) -> let
        freshArgType = freshArgRepr freshArg
        in case globalToA32Location k of
          Nothing -> Nothing
          Just (Pair loc locRepr) -> case testEquality locRepr freshArgType of
            Nothing -> error $ "Unexpected type for global:" ++ show k ++ " " ++ show locRepr
            Just Refl -> Just $ LocExpr locRepr loc (freshArgBoundVar freshArg) Nothing)


    collectOperand :: forall ctx tp
                    . sym
                   -> WT.BaseTypeRepr (WT.BaseStructType ctx)
                   -> S.SymExpr sym (WT.BaseStructType ctx) -- instruction "return" struct
                   -> (AC.LabeledValue AS.FunctionArg (FreshArg sym)) tp
                   -> Some (OperandCollector sym A32)
                   -> IO (Some (OperandCollector sym A32))
    collectOperand sym retRepr retExpr (AC.LabeledValue funarg freshArg) (Some opcol) = let
      AS.FunctionArg nm ty rkind = funarg
      in case rkind of
        Just AT.RegisterR ->
          toOperandCollector <$> collectROperandWrite sym retRepr retExpr freshArg opcol
        _ -> do
          Some opcol' <- collectBVOperand freshArg opcol
          return $ toOperandCollector opcol'


    -- | We currently assume that non register-indexed operands are all immediates
    -- (this is a gross simplification), and therefore we only need to collect their bound
    -- variables.
    collectBVOperand :: forall sh tp
                      . FreshArg sym tp
                     -> OperandCollector sym A32 sh
                     -> IO (Some (OperandCollector' sym A32 sh))
    collectBVOperand freshArg opcol = let
      bt = freshArgRepr freshArg
      in case bt of
        CT.BaseBVRepr nr
          | BVTypeProof Refl srepr <- getRetRepr nr -> do
            let
              newParam = SF.OperandParameter bt (symbolIndex srepr)
              newBV = BV.BoundVar $ freshArgBoundVar freshArg
              pexpr = ParamExpr newParam Nothing
            return $ Some $ OperandCollector' $
              opcol{ opParams = addParamExpr pexpr (opParams opcol)
                   , opParamRepr = srepr PL.:< (opParamRepr opcol)
                   , opBVs = newBV PL.:< (opBVs opcol)}
        _ -> error $ "Expected base type: " ++ show bt

    -- | We assume that the list of written-to register-indexed operands exactly corresponds to the
    -- returned struct of the instruction. i.e.
    -- field imm
    -- field Rb
    -- field imm2
    -- field Rd
    -- ...
    -- (R[b], b_idx) = Rb
    -- b = UInt(b_idx)
    -- (R[d], d_idx) = Rd
    -- d = UInt(d_idx)
    -- ..
    -- return ((R[b], b_idx), (R[d], d_idx))
    collectROperandWrite :: forall ctx tp sh
                          . sym
                         -> WT.BaseTypeRepr (WT.BaseStructType ctx)
                         -> S.SymExpr sym (WT.BaseStructType ctx) -- instruction "return" struct
                         -> FreshArg sym tp
                         -> OperandCollector sym A32 sh
                         -> IO (OperandCollector' sym A32 sh "R")
    collectROperandWrite sym (WT.BaseStructRepr symRepr) structExpr freshArg opcol = do
      let retVal = (retValueCount opcol)
      case Ctx.intIndex retVal (Ctx.size symRepr) of
        Nothing -> error $ "Mismatch in returned registers vs. operand registers"
        Just (Some ix) -> do
          regVal <- WI.structField sym structExpr ix
          let regType = WI.exprType regVal
          let bt = freshArgRepr freshArg
          if | Just Refl <- testEquality regType registerRetRepr
             , Just Refl <- testEquality bt registerRetRepr -> do
               let
                 newParam = SF.OperandParameter regType PL.IndexHere
                 newBV = BV.BoundVar $ freshArgBoundVar freshArg
                 pexpr = ParamExpr newParam (Just regVal)
               return $ OperandCollector' $ opcol{ opParams = addParamExpr pexpr (opParams opcol)
                       , opBVs = newBV PL.:< (opBVs opcol)
                       , opParamRepr = CT.knownSymbol PL.:< (opParamRepr opcol)
                       , retValueCount = retVal + 1}
             | otherwise -> error $ "Expected register type:" ++ show regType ++ " " ++ show bt

-- | Mapping from ASL globals to their ISA location
globalToA32Location :: T.Text -> Maybe (Pair (L.Location A32) CT.BaseTypeRepr)
globalToA32Location globName = case globName of
  "_PC" -> Just $ Pair AL.LocPC (CT.BaseBVRepr knownNat)
  "PSTATE_N" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_N) (CT.BaseBVRepr knownNat)
  "PSTATE_Z" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_Z) (CT.BaseBVRepr knownNat)
  "PSTATE_C" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_C) (CT.BaseBVRepr knownNat)
  "PSTATE_V" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_V) (CT.BaseBVRepr knownNat)
  "PSTATE_D" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_D) (CT.BaseBVRepr knownNat)
  "PSTATE_A" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_A) (CT.BaseBVRepr knownNat)
  "PSTATE_I" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_I) (CT.BaseBVRepr knownNat)
  "PSTATE_F" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_F) (CT.BaseBVRepr knownNat)
  "PSTATE_PAN" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_PAN) (CT.BaseBVRepr knownNat)
  "PSTATE_UAO" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_UAO) (CT.BaseBVRepr knownNat)
  "PSTATE_DIT" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_DIT) (CT.BaseBVRepr knownNat)
  "PSTATE_TCO" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_TCO) (CT.BaseBVRepr knownNat)
  "PSTATE_BTYPE" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_BTYPE) (CT.BaseBVRepr knownNat)
  "PSTATE_SS" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_SS) (CT.BaseBVRepr knownNat)
  "PSTATE_IL" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_IL) (CT.BaseBVRepr knownNat)
  "PSTATE_EL" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_EL) (CT.BaseBVRepr knownNat)
  "PSTATE_nRW" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_nRW) (CT.BaseBVRepr knownNat)
  "PSTATE_SP" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_SP) (CT.BaseBVRepr knownNat)
  "PSTATE_Q" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_Q) (CT.BaseBVRepr knownNat)
  "PSTATE_GE" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_GE) (CT.BaseBVRepr knownNat)
  "PSTATE_SSBS" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_SSBS) (CT.BaseBVRepr knownNat)
  "PSTATE_IT" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_IT) (CT.BaseBVRepr knownNat)
  "PSTATE_J" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_J) (CT.BaseBVRepr knownNat)
  "PSTATE_T" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_T) (CT.BaseBVRepr knownNat)
  "PSTATE_E" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_E) (CT.BaseBVRepr knownNat)
  "PSTATE_M" -> Just $ Pair (AL.LocPSTATE AL.PSTATE_M) (CT.BaseBVRepr knownNat)
  _ -> Nothing


mkFreshArgMap :: Ctx.Assignment (FreshArg sym) tps -> Map.Map T.Text (Some (FreshArg sym))
mkFreshArgMap = FC.foldrFC' (\fa -> Map.insert (freshArgName fa) (Some fa)) Map.empty

data BVTypeProof n where
  BVTypeProof :: A.OperandType A32 s :~: CT.BaseBVType n -> CT.SymbolRepr s -> BVTypeProof n

-- | Clumsily reversing the symbol mapping

getRetRepr :: forall n. 1 WT.<= n => WT.NatRepr n -> BVTypeProof n
getRetRepr n =
   fromMaybe (error $ "bad bv" ++ show n) $
   testEq @"Bv1" knownRepr
   <|> testEq @"Bv2" knownRepr
   <|> testEq @"Bv3" knownRepr
   <|> testEq @"Bv4" knownRepr
   <|> testEq @"Bv5" knownRepr
   <|> testEq @"Bv6" knownRepr
   <|> testEq @"Bv7" knownRepr
   <|> testEq @"Bv8" knownRepr
   <|> testEq @"Bv9" knownRepr
   <|> testEq @"Bv10" knownRepr
   <|> testEq @"Bv11" knownRepr
   <|> testEq @"Bv12" knownRepr
   <|> testEq @"Bv13" knownRepr
   <|> testEq @"Bv14" knownRepr
   <|> testEq @"Bv15" knownRepr
   <|> testEq @"Bv16" knownRepr
   <|> testEq @"Bv17" knownRepr
   <|> testEq @"Bv18" knownRepr
   <|> testEq @"Bv19" knownRepr
   <|> testEq @"Bv20" knownRepr
   <|> testEq @"Bv21" knownRepr
   <|> testEq @"Bv22" knownRepr
   <|> testEq @"Bv23" knownRepr
   <|> testEq @"Bv24" knownRepr
  where
    testEq :: forall s. CT.KnownSymbol s => CT.BaseTypeRepr (A.OperandType A32 s) -> Maybe (BVTypeProof n)
    testEq btrepr =
      let
        srepr = CT.knownSymbol :: CT.SymbolRepr s
      in case testEquality btrepr (CT.BaseBVRepr n) of
      Just Refl -> Just $ BVTypeProof Refl srepr
      Nothing -> Nothing

symbolIndex :: CT.SymbolRepr s -> PL.Index (s ': sh) s
symbolIndex repr = PL.IndexHere

registerRetRepr :: CT.BaseTypeRepr (A.OperandType A32 "R")
registerRetRepr = knownRepr


addParamExpr :: ParamExpr arch (s ': sh) sym
             -> [ParamExpr arch sh sym]
             -> [ParamExpr arch (s ': sh) sym]
addParamExpr pexpr l = pexpr : map (\(ParamExpr pf se) -> (ParamExpr (mapParam pf) se)) l

-- | Lift a Parameter to one more argument
mapParam :: forall arch sh tp s. SF.Parameter arch sh tp -> SF.Parameter arch (s ': sh) tp
mapParam p = case p of
  SF.OperandParameter repr idx -> SF.OperandParameter repr (PL.IndexThere idx)
  SF.LiteralParameter loc -> SF.LiteralParameter loc
  SF.FunctionParameter _ _ _  -> error "Unsupported parameter type"

data ParamExpr arch sh sym where
  ParamExpr :: SF.Parameter arch sh tp -> Maybe (S.SymExpr sym tp) -> ParamExpr arch sh sym

data OperandCollector' sym arch sh s = OperandCollector' { opC :: OperandCollector sym arch (s ': sh) }

toOperandCollector :: OperandCollector' sym arch sh s -> Some (OperandCollector sym arch)
toOperandCollector (OperandCollector' opcol) = Some opcol

emptyOperandCollector :: OperandCollector sym arch '[]
emptyOperandCollector = OperandCollector [] PL.Nil PL.Nil 0

data OperandCollector sym arch sh = OperandCollector
  { opParams :: [ParamExpr arch sh sym]
  , opBVs :: PL.List (BV.BoundVar sym arch) sh
  , opParamRepr :: PL.List CT.SymbolRepr sh
  , retValueCount :: Int -- number of written-to register operands we've collected
  }

data LocExpr sym arch where
  LocExpr :: CT.BaseTypeRepr tp -> L.Location arch tp -> S.BoundVar sym tp -> Maybe (S.SymExpr sym tp) -> LocExpr sym arch

data FreshArg sym bt = FreshArg { freshArgEntry :: CS.RegEntry sym (CT.BaseToType bt)
                                , freshArgBoundVar :: WI.BoundVar sym bt
                                , freshArgRepr :: CT.BaseTypeRepr bt
                                , freshArgName :: T.Text
                                }

freshArgBoundVars :: Ctx.Assignment (FreshArg sym) init -> Ctx.Assignment (WI.BoundVar sym) (TL.ToContextFwd (AT.ToBaseTypesListBase init))
freshArgBoundVars args = TL.toAssignmentFwd (freshArgBoundVars' args)

freshArgBoundVars' :: Ctx.Assignment (FreshArg sym) init -> PL.List (WI.BoundVar sym) (AT.ToBaseTypesListBase init)
freshArgBoundVars' Ctx.Empty = PL.Nil
freshArgBoundVars' (args Ctx.:> arg) = freshArgBoundVar arg PL.:< freshArgBoundVars' args

allocateFreshArg :: (CB.IsSymInterface sym)
                 => sym
                 -> AC.LabeledValue T.Text CT.BaseTypeRepr btp
                 -> IO (FreshArg sym btp)
allocateFreshArg sym (AC.LabeledValue name rep) = do
  case rep of
    CT.BaseBVRepr w -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname (WT.BaseBVRepr w)
      rv <- WI.freshConstant sym sname (WT.BaseBVRepr w)
      return $ FreshArg
        ( CS.RegEntry { CS.regType = CT.baseToType rep
                      , CS.regValue = rv
                      } )
        bv
        rep
        name
    CT.BaseIntegerRepr -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname WT.BaseIntegerRepr
      rv <- WI.freshConstant sym sname WT.BaseIntegerRepr
      return $ FreshArg
        ( CS.RegEntry { CS.regType = CT.baseToType rep
                      , CS.regValue = rv
                      } )
        bv
        rep
        name
    CT.BaseBoolRepr -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname WT.BaseBoolRepr
      rv <- WI.freshConstant sym sname WT.BaseBoolRepr
      return $ FreshArg
        ( CS.RegEntry { CS.regType = CT.baseToType rep
                      , CS.regValue = rv
                      } )
        bv
        rep
        name
    CT.BaseArrayRepr idxTy vTy -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname (WT.BaseArrayRepr idxTy vTy)
      rv <- WI.freshConstant sym sname (WT.BaseArrayRepr idxTy vTy)
      return $ FreshArg
        ( CS.RegEntry { CS.regType = CT.baseToType rep
                      , CS.regValue = rv
                      } )
        bv
        rep
        name
    CT.BaseStructRepr idxTy -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname (WT.BaseStructRepr idxTy)
      rv <- WI.freshConstant sym sname (WT.BaseStructRepr idxTy)
      return $ FreshArg
        ( CS.RegEntry { CS.regType = CT.baseToType rep
                      , CS.regValue = rv
                      } )
        bv
        rep
        name
    _ -> X.throwIO (CannotAllocateFresh name rep)

toSolverSymbol :: String -> IO WS.SolverSymbol
toSolverSymbol s' =
  let s = case s' of '_' : rst -> "UU_" ++ rst
                     _ -> s'
  in case WS.userSymbol s of
    Right sy -> return sy
    Left _err -> X.throwIO (InvalidSymbolName s)

initialSimulatorState :: (CB.IsSymInterface sym, OnlineSolver scope sym)
                      => SimulatorConfig scope
                      -> CS.SymGlobalState sym
                      -> CS.ExecCont () sym (AC.ASLExt arch) (CS.RegEntry sym ret) (CSC.OverrideLang ret) ('Just CT.EmptyCtx)
                      -> CT.TypeRepr ret
                      -> IO (CS.ExecState () sym (AC.ASLExt arch) (CS.RegEntry sym ret))
initialSimulatorState symCfg symGlobalState econt retRepr = do
  let intrinsics = CS.emptyIntrinsicTypes
  let sym = simSym symCfg
  let hdlAlloc = simHandleAllocator symCfg
  let outputHandle = simOutputHandle symCfg
  let simContext = CS.initSimContext sym intrinsics hdlAlloc outputHandle CFH.emptyHandleMap AC.aslExtImpl ()
  let hdlr = CS.defaultAbortHandler
  return (CS.InitialState simContext symGlobalState hdlr retRepr econt)

-- | Allocate all of the globals that will be referred to by the statement
-- sequence (even indirectly) and use them to populate a 'CS.GlobalSymState'
initGlobals :: forall sym env scope
             . (CB.IsSymInterface sym, OnlineSolver scope sym)
            => SimulatorConfig scope
            -> Ctx.Assignment AC.BaseGlobalVar env
            -> IO (Ctx.Assignment (FreshArg sym) env, CS.SymGlobalState sym)
initGlobals symCfg globals = do
  MS.runStateT (FC.forFC globals addGlobal) (CS.emptyGlobals)
  where
    addGlobal :: forall bt
               . AC.BaseGlobalVar bt
              -> MS.StateT (CSG.SymGlobalState sym) IO (FreshArg sym bt)
    addGlobal (AC.BaseGlobalVar gv) = do
      gs <- MS.get
      let baseGlobalType = AT.toBaseType (CCG.globalType gv)
      arg <- liftIO $ allocateFreshArg (simSym symCfg) (AC.LabeledValue (CCG.globalName gv) baseGlobalType)
      MS.put $ CSG.insertGlobal gv (CS.regValue (freshArgEntry arg)) gs
      return arg

executionFeatures :: sym ~ CBO.OnlineBackend scope solver fs
                  => WPO.OnlineSolver scope solver
                  => CB.IsSymInterface sym
                  => CCE.IsSyntaxExtension ext
                  => T.Text -> sym -> IO [CS.ExecutionFeature p sym ext rtp]
executionFeatures nm sym = do
  gft <- CSP.pathSatisfiabilityFeature sym (CBO.considerSatisfiability sym)
  -- FIXME: What is the general requirement here?
  let psf = if nm `elem` ["aarch32_VLDM_A_aarch32_VLDM_T1A1_A","aarch32_VMOV_r_A_aarch32_VMOV_r_T2A2_A"
                         , "aarch32_VSTM_A_aarch32_VSTM_T1A1_A", "aarch32_VMOV_i_A_aarch32_VMOV_i_A2_A"
                         , "aarch32_VMOV_i_A_aarch32_VMOV_i_T2_A"
                         ]
        then [CS.genericToExecutionFeature gft] else []
  timeout <- CS.genericToExecutionFeature <$> CS.timeoutFeature (5.00 :: NominalDiffTime)
  let fts = psf ++ [timeout]
  let cfg = WI.getConfiguration sym
  pathSetter <- WC.getOptionSetting CBO.solverInteractionFile cfg
  res <- WC.setOpt pathSetter (T.pack "./yices.out")
  X.assert (null res) (return fts)
  --return []

data SimulationException = SimulationTimeout (Some AC.SomeFunctionSignature)
                         | SimulationAbort (Some AC.SomeFunctionSignature) T.Text
                         | forall tp . NonBaseTypeReturn (CT.TypeRepr tp)
                         | forall btp . UnexpectedReturnType (WT.BaseTypeRepr btp)
                         | forall tp . MissingGlobalDefinition (CS.GlobalVar tp)
                         | forall tp . CannotAllocateFresh T.Text (CT.BaseTypeRepr tp)
                         | InvalidSymbolName String


showAbortedResult :: CS.AbortedResult c d -> T.Text
showAbortedResult ar = case ar of
  CS.AbortedExec reason st -> T.pack $ show reason
  CS.AbortedExit code -> T.pack $ show code
  CS.AbortedBranch loc pre res' res'' -> "BRANCH: " <> showAbortedResult res' <> "\n" <> showAbortedResult res''

deriving instance Show SimulationException

instance X.Exception SimulationException
