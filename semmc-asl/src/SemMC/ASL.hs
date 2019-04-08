{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module SemMC.ASL (
    simulateFunction
  , simulateProcedure
  , SimulatorConfig(..)
  , SimulationException(..)
  ) where

import qualified Control.Exception as X
import           Control.Lens ( (^.) )
import           Control.Monad.ST ( RealWorld, stToIO )
import           Data.Functor.Product ( Product(Pair) )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( viewSome )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.CallFrame as CSC
import qualified Lang.Crucible.Simulator.GlobalState as CSG
import qualified Lang.Crucible.Types as CT
import qualified System.IO as IO
import qualified What4.BaseTypes as WT
import qualified What4.Interface as WI
import qualified What4.Symbol as WS

import qualified SemMC.ASL.Crucible as AC

data SimulatorConfig sym =
  SimulatorConfig { simOutputHandle :: IO.Handle
                  , simHandleAllocator :: CFH.HandleAllocator RealWorld
                  , simSym :: sym
                  }

-- | Symbolically simulate a function, which has a single return value (which may itself be a tuple
-- i.e., struct)
--
-- Procedures have a different return type, where we need to track not only the value returned, but
-- also the global location to which it should be assigned
simulateFunction :: (CB.IsSymInterface sym, CS.RegValue sym ret ~ WI.SymExpr sym tp)
                 => SimulatorConfig sym
                 -> AC.FunctionSignature init ret tp
                 -> CCC.SomeCFG () init ret
                 -> IO (WI.SymExpr sym tp)
simulateFunction symCfg sig (CCC.SomeCFG cfg) = do
  initArgs <- FC.traverseFC (allocateFreshArg (simSym symCfg)) (AC.funcArgReprs sig)
  let econt = CS.runOverrideSim (CT.baseToType (AC.funcSigRepr sig)) $ do
        re <- CS.callCFG cfg (CS.RegMap initArgs)
        return (CS.regValue re)
  globals <- viewSome (initGlobals symCfg) (AC.funcGlobalReprs sig)
  s0 <- initialSimulatorState symCfg globals econt
  eres <- CS.executeCrucible executionFeatures s0
  case eres of
    CS.TimeoutResult {} -> X.throwIO (SimulationTimeout (AC.SomeFunctionSignature sig))
    CS.AbortedResult {} -> X.throwIO (SimulationAbort (AC.SomeFunctionSignature sig))
    CS.FinishedResult _ pres ->
      case pres of
        CS.TotalRes gp -> extractResult gp
        CS.PartialRes _ gp _ -> extractResult gp
  where
    extractResult gp =
      let re = gp ^. CS.gpValue
      in case CT.asBaseType (CS.regType re) of
        CT.NotBaseType -> X.throwIO (NonBaseTypeReturn (CS.regType re))
        CT.AsBaseType btr
          | Just Refl <- testEquality btr (AC.funcSigRepr sig) ->
            return (CS.regValue re)
          | otherwise -> X.throwIO (UnexpectedReturnType btr)

-- | Simulate a procedure
--
-- This is more complicated, as we have to maintain the mapping between exprs and the global
-- location (i.e., register or memory) that is being updated by the procedure.  This is also
-- suitable for the top-level instruction semantics, which operate in the same way (but take no
-- arguments)
--
-- Note that the type tps works out, as the sequence collection of types is BaseStructType
simulateProcedure :: (CB.IsSymInterface sym)
                  => SimulatorConfig sym
                  -> AC.ProcedureSignature init ret tps
                  -> CCC.SomeCFG () init ret
                  -> IO (Ctx.Assignment (AC.LabeledValue T.Text (WI.SymExpr sym)) tps)
simulateProcedure symCfg sig (CCC.SomeCFG cfg) = do
  initArgs <- FC.traverseFC (allocateFreshArg (simSym symCfg)) (AC.procArgReprs sig)
  let econt = CS.runOverrideSim CT.UnitRepr $ do
        _ <- CS.callCFG cfg (CS.RegMap initArgs)
        return ()
  globals <- viewSome (initGlobals symCfg) (AC.procGlobalReprs sig)
  s0 <- initialSimulatorState symCfg globals econt
  eres <- CS.executeCrucible executionFeatures s0
  case eres of
    CS.TimeoutResult {} -> X.throwIO (SimulationTimeout (AC.SomeProcedureSignature sig))
    CS.AbortedResult {} -> X.throwIO (SimulationAbort (AC.SomeProcedureSignature sig))
    CS.FinishedResult _ pres ->
      case pres of
        CS.TotalRes gp -> extractResult gp
        CS.PartialRes _ gp _ -> extractResult gp
  where
    -- Look up all of the values of the globals we allocated (which capture all of the side effects)
    extractResult gp = FC.traverseFC (lookupBaseGlobalVal (gp ^. CS.gpGlobals)) (AC.procSigRepr sig)
    lookupBaseGlobalVal gs (AC.BaseGlobalVar gv) = do
      case CSG.lookupGlobal gv gs of
        Just rv -> return (AC.LabeledValue (CCG.globalName gv) rv)
        Nothing -> X.throwIO (MissingGlobalDefinition gv)

allocateFreshArg :: (CB.IsSymInterface sym)
                 => sym
                 -> AC.LabeledValue T.Text CT.TypeRepr tp
                 -> IO (CS.RegEntry sym tp)
allocateFreshArg sym (AC.LabeledValue name rep) =
  case rep of
    CT.BVRepr w -> do
      sname <- toSolverSymbol (T.unpack name)
      rv <- WI.freshConstant sym sname (WT.BaseBVRepr w)
      return CS.RegEntry { CS.regType = rep
                         , CS.regValue = rv
                         }
    _ -> X.throwIO (CannotAllocateFresh rep)

toSolverSymbol :: String -> IO WS.SolverSymbol
toSolverSymbol s =
  case WS.userSymbol s of
    Right sy -> return sy
    Left _err -> X.throwIO (InvalidSymbolName s)

initialSimulatorState :: (CB.IsSymInterface sym)
                      => SimulatorConfig sym
                      -> CS.SymGlobalState sym
                      -> CS.ExecCont () sym () (CS.RegEntry sym ret) (CSC.OverrideLang ret) ('Just CT.EmptyCtx)
                      -> IO (CS.ExecState () sym () (CS.RegEntry sym ret))
initialSimulatorState symCfg symGlobalState econt = do
  let intrinsics = CS.emptyIntrinsicTypes
  let sym = simSym symCfg
  let hdlAlloc = simHandleAllocator symCfg
  let outputHandle = simOutputHandle symCfg
  let simContext = CS.initSimContext sym intrinsics hdlAlloc outputHandle CFH.emptyHandleMap CS.emptyExtensionImpl ()
  let hdlr = CS.defaultAbortHandler
  return (CS.InitialState simContext symGlobalState hdlr econt)

-- | Allocate all of the globals that will be referred to by the statement sequence (even indirectly)
initGlobals :: forall sym env
             . (CB.IsSymInterface sym)
            => SimulatorConfig sym
            -> Ctx.Assignment (AC.LabeledValue T.Text CT.TypeRepr) env
            -> IO (CS.SymGlobalState sym)
initGlobals symCfg reps = do
  globals <- FC.traverseFC allocGlobal reps
  return (FC.foldrFC addGlobal CS.emptyGlobals globals)
  where
    allocGlobal :: forall tp . AC.LabeledValue T.Text CT.TypeRepr tp -> IO (Product CS.GlobalVar (CS.RegEntry sym) tp)
    allocGlobal lv@(AC.LabeledValue name rep) = do
      entry <- allocateFreshArg (simSym symCfg) lv
      gv <- stToIO $ CCG.freshGlobalVar (simHandleAllocator symCfg) name rep
      return (Pair gv entry)
    addGlobal (Pair gv entry) = CSG.insertGlobal gv (CS.regValue entry)

executionFeatures :: [CS.ExecutionFeature p sym ext rtp]
executionFeatures = []

data SimulationException = SimulationTimeout AC.SomeSignature
                         | SimulationAbort AC.SomeSignature
                         | forall tp . NonBaseTypeReturn (CT.TypeRepr tp)
                         | forall btp . UnexpectedReturnType (WT.BaseTypeRepr btp)
                         | forall tp . MissingGlobalDefinition (CS.GlobalVar tp)
                         | forall tp . CannotAllocateFresh (CT.TypeRepr tp)
                         | InvalidSymbolName String

deriving instance Show SimulationException

instance X.Exception SimulationException
