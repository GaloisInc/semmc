{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.ASL (
    simulateFunction
  , simulateProcedure
  , SimulatorConfig(..)
  , SimulationException(..)
  ) where

import qualified Control.Exception as X
import           Control.Lens ( (^.) )
import           Control.Monad.ST ( RealWorld )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Data.Type.List as TL
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

import qualified SemMC.Formula as SF
import qualified SemMC.ASL.Crucible as AC
import qualified SemMC.ASL.Signature as AS
import qualified SemMC.ASL.Types as AT

data SimulatorConfig sym =
  SimulatorConfig { simOutputHandle :: IO.Handle
                  , simHandleAllocator :: CFH.HandleAllocator RealWorld
                  , simSym :: sym
                  }

reshape :: Ctx.Assignment CT.TypeRepr ctps -> PL.List WT.BaseTypeRepr (AT.ToBaseTypesList ctps)
reshape Ctx.Empty = PL.Nil
reshape (reprs Ctx.:> repr) = case CT.asBaseType repr of
  CT.NotBaseType -> error "Illegal crucible type"
  CT.AsBaseType brepr -> brepr PL.:< reshape reprs

-- | Symbolically simulate a function, which has a single return value (which may itself be a tuple
-- i.e., struct)
--
-- Procedures have a different return type, where we need to track not only the value returned, but
-- also the global location to which it should be assigned
simulateFunction :: (CB.IsSymInterface sym)
                 => SimulatorConfig sym
                 -> AC.Function arch globals init tp
                 -> IO (SF.FunctionFormula sym '(AT.ToBaseTypesList init, tp))
simulateFunction symCfg func = do
  case AC.funcCFG func of
    CCC.SomeCFG cfg -> do
      let sig = AC.funcSig func
      initArgs <- FC.traverseFC (allocateFreshArg (simSym symCfg)) (AC.funcArgReprs sig)
      let econt = CS.runOverrideSim (CT.baseToType (AC.funcSigRepr sig)) $ do
            re <- CS.callCFG cfg (CS.RegMap (FC.fmapFC freshArgEntry initArgs))
            return (CS.regValue re)
      let globals = AC.funcGlobals func
      globalState <- initGlobals symCfg globals
      s0 <- initialSimulatorState symCfg globalState econt
      eres <- CS.executeCrucible executionFeatures s0
      case eres of
        CS.TimeoutResult {} -> X.throwIO (SimulationTimeout (Some (AC.SomeFunctionSignature sig)))
        CS.AbortedResult {} -> X.throwIO (SimulationAbort (Some (AC.SomeFunctionSignature sig)))
        CS.FinishedResult _ pres ->
          case pres of
            CS.TotalRes gp -> extractResult gp initArgs
            CS.PartialRes _ _ gp _ -> extractResult gp initArgs
  where
    extractResult gp initArgs =
      let re = gp ^. CS.gpValue
      in case CT.asBaseType (CS.regType re) of
        CT.NotBaseType -> X.throwIO (NonBaseTypeReturn (CS.regType re))
        CT.AsBaseType btr
          | Just Refl <- testEquality btr (AC.funcSigRepr (AC.funcSig func)) -> do
              print (WI.printSymExpr (CS.regValue re))
              let name = T.unpack (AS.funcName (AC.funcSig func))
                  argTypes = reshape (FC.fmapFC AT.projectValue (AS.funcArgReprs (AC.funcSig func)))
                  argVars = freshArgBoundVars' initArgs
                  retType = AS.funcSigRepr (AC.funcSig func)
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

-- | Simulate a procedure
--
-- This is more complicated, as we have to maintain the mapping between exprs and the global
-- location (i.e., register or memory) that is being updated by the procedure.  This is also
-- suitable for the top-level instruction semantics, which operate in the same way (but take no
-- arguments)
--
-- Note that the type tps works out, as the sequence collection of types is BaseStructType
simulateProcedure :: forall arch sym init globals
                   . (CB.IsSymInterface sym)
                  => SimulatorConfig sym
                  -> AC.Procedure arch globals init
                  -> IO (SF.FunctionFormula sym '(AT.ToBaseTypesList init, WT.BaseStructType globals))
simulateProcedure symCfg crucProc =
  case AC.procCFG crucProc of
    CCC.SomeCFG cfg -> do
      let sig = AC.procSig crucProc
      initArgs <- FC.traverseFC (allocateFreshArg (simSym symCfg)) (AC.procArgReprs sig)
      let globalTypes = FC.fmapFC AT.projectValue (AS.procGlobalReprs sig)
      let econt = CS.runOverrideSim (CT.baseToType (WT.BaseStructRepr globalTypes)) $ do
            re <- CS.callCFG cfg (CS.RegMap (FC.fmapFC freshArgEntry initArgs))
            return (CS.regValue re)
      let globals = AC.procGlobals crucProc
      globalState <- initGlobals symCfg globals
      s0 <- initialSimulatorState symCfg globalState econt
      eres <- CS.executeCrucible executionFeatures s0
      case eres of
        CS.TimeoutResult {} -> X.throwIO (SimulationTimeout (Some (AC.SomeProcedureSignature sig)))
        CS.AbortedResult context ab -> X.throwIO (SimulationAbort (Some (AC.SomeProcedureSignature sig)))
        CS.FinishedResult _ pres ->
          case pres of
            CS.TotalRes gp -> extractResult gp initArgs
            CS.PartialRes _ _ gp _ -> extractResult gp initArgs
  where
    extractResult gp initArgs =
      let re = gp ^. CS.gpValue
          sig = AC.procSig crucProc
          globalTypes = FC.fmapFC AT.projectValue (AS.procGlobalReprs sig)
          retType = WT.BaseStructRepr globalTypes
          -- argTypes = FC.fmapFC AS.projectValue (AS.procArgReprs sig)
      in case CT.asBaseType (CS.regType re) of
        CT.NotBaseType -> X.throwIO (NonBaseTypeReturn (CS.regType re))
        CT.AsBaseType btr
          | Just Refl <- testEquality btr retType -> do
              print (WI.printSymExpr (CS.regValue re))
              let name = T.unpack (AS.procName sig)
                  argTypes = reshape (FC.fmapFC AT.projectValue (AS.procArgReprs sig))
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

  -- case AC.procCFG crucProc of
  --   CCC.SomeCFG cfg -> do
  --     let sig = AC.procSig crucProc
  --     let globalVars = AC.procGlobals crucProc
  --     initArgs <- FC.traverseFC (allocateFreshArg (simSym symCfg)) (AC.procArgReprs sig)
  --     let econt = CS.runOverrideSim CT.UnitRepr $ do
  --           _ <- CS.callCFG cfg (CS.RegMap initArgs)
  --           return ()
  --     globalState <- initGlobals symCfg globalVars
  --     s0 <- initialSimulatorState symCfg globalState econt
  --     eres <- CS.executeCrucible executionFeatures s0
  --     case eres of
  --       CS.TimeoutResult {} -> X.throwIO (SimulationTimeout (AC.SomeProcedureSignature sig))
  --       CS.AbortedResult {} -> X.throwIO (SimulationAbort (AC.SomeProcedureSignature sig))
  --       CS.FinishedResult _ pres ->
  --         case pres of
  --           CS.TotalRes gp -> extractResult globalVars gp
  --           CS.PartialRes _ gp _ -> extractResult globalVars gp
  -- where
  --   -- Look up all of the values of the globals we allocated (which capture all of the side effects)
  --   extractResult globalVars gp = FC.traverseFC (lookupBaseGlobalVal (gp ^. CS.gpGlobals)) globalVars
  --   lookupBaseGlobalVal gs (AC.BaseGlobalVar gv) = do
  --     case CSG.lookupGlobal gv gs of
  --       Just rv -> return (AC.LabeledValue (CCG.globalName gv) rv)
  --       Nothing -> X.throwIO (MissingGlobalDefinition gv)

data FreshArg sym tp = FreshArg { freshArgEntry :: CS.RegEntry sym tp
                                , freshArgBoundVar :: WI.BoundVar sym (AT.ToBaseType tp)
                                }

freshArgBoundVars :: Ctx.Assignment (FreshArg sym) init -> Ctx.Assignment (WI.BoundVar sym) (TL.ToContextFwd (AT.ToBaseTypesList init))
freshArgBoundVars args = TL.toAssignment (TL.reverse (freshArgBoundVars' args))

freshArgBoundVars' :: Ctx.Assignment (FreshArg sym) init -> PL.List (WI.BoundVar sym) (AT.ToBaseTypesList init)
freshArgBoundVars' Ctx.Empty = PL.Nil
freshArgBoundVars' (args Ctx.:> arg) = freshArgBoundVar arg PL.:< freshArgBoundVars' args

allocateFreshArg :: (CB.IsSymInterface sym)
                 => sym
                 -> AC.LabeledValue T.Text CT.TypeRepr tp
                 -> IO (FreshArg sym tp)
allocateFreshArg sym (AC.LabeledValue name rep) = do
  case rep of
    CT.BVRepr w -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname (WT.BaseBVRepr w)
      rv <- WI.freshConstant sym sname (WT.BaseBVRepr w)
      return $ FreshArg
        ( CS.RegEntry { CS.regType = rep
                      , CS.regValue = rv
                      } )
        bv
    CT.IntegerRepr -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname WT.BaseIntegerRepr
      rv <- WI.freshConstant sym sname WT.BaseIntegerRepr
      return $ FreshArg
        ( CS.RegEntry { CS.regType = rep
                      , CS.regValue = rv
                      } )
        bv
    CT.BoolRepr -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname WT.BaseBoolRepr
      rv <- WI.freshConstant sym sname WT.BaseBoolRepr
      return $ FreshArg
        ( CS.RegEntry { CS.regType = rep
                      , CS.regValue = rv
                      } )
        bv
    CT.SymbolicArrayRepr idxTy vTy -> do
      sname <- toSolverSymbol (T.unpack name)
      bv <- WI.freshBoundVar sym sname (WT.BaseArrayRepr idxTy vTy)
      rv <- WI.freshConstant sym sname (WT.BaseArrayRepr idxTy vTy)
      return $ FreshArg
        ( CS.RegEntry { CS.regType = rep
                      , CS.regValue = rv
                      } )
        bv
    _ -> X.throwIO (CannotAllocateFresh name rep)

toSolverSymbol :: String -> IO WS.SolverSymbol
toSolverSymbol s' =
  let s = case s' of '_' : rst -> "UU_" ++ rst
                     _ -> s'
  in case WS.userSymbol s of
    Right sy -> return sy
    Left _err -> X.throwIO (InvalidSymbolName s)

initialSimulatorState :: (CB.IsSymInterface sym)
                      => SimulatorConfig sym
                      -> CS.SymGlobalState sym
                      -> CS.ExecCont () sym (AC.ASLExt arch) (CS.RegEntry sym ret) (CSC.OverrideLang ret) ('Just CT.EmptyCtx)
                      -> IO (CS.ExecState () sym (AC.ASLExt arch) (CS.RegEntry sym ret))
initialSimulatorState symCfg symGlobalState econt = do
  let intrinsics = CS.emptyIntrinsicTypes
  let sym = simSym symCfg
  let hdlAlloc = simHandleAllocator symCfg
  let outputHandle = simOutputHandle symCfg
  let simContext = CS.initSimContext sym intrinsics hdlAlloc outputHandle CFH.emptyHandleMap AC.aslExtImpl ()
  let hdlr = CS.defaultAbortHandler
  return (CS.InitialState simContext symGlobalState hdlr econt)

-- | Allocate all of the globals that will be referred to by the statement
-- sequence (even indirectly) and use them to populate a 'CS.GlobalSymState'
initGlobals :: forall sym env
             . (CB.IsSymInterface sym)
            => SimulatorConfig sym
            -> Ctx.Assignment AC.BaseGlobalVar env
            -> IO (CS.SymGlobalState sym)
initGlobals symCfg globals = do
  FC.foldrFC addGlobal (pure CS.emptyGlobals) globals
  where
    addGlobal :: forall tp
               . AC.BaseGlobalVar tp
              -> IO (CSG.SymGlobalState sym)
              -> IO (CSG.SymGlobalState sym)
    addGlobal (AC.BaseGlobalVar gv) mgs = do
      gs <- mgs
      arg <- allocateFreshArg (simSym symCfg) (AC.LabeledValue (CCG.globalName gv) (CCG.globalType gv))
      return (CSG.insertGlobal gv (CS.regValue (freshArgEntry arg)) gs)

executionFeatures :: [CS.ExecutionFeature p sym ext rtp]
executionFeatures = []

data SimulationException = SimulationTimeout (Some AC.SomeSignature)
                         | SimulationAbort (Some AC.SomeSignature)
                         | forall tp . NonBaseTypeReturn (CT.TypeRepr tp)
                         | forall btp . UnexpectedReturnType (WT.BaseTypeRepr btp)
                         | forall tp . MissingGlobalDefinition (CS.GlobalVar tp)
                         | forall tp . CannotAllocateFresh T.Text (CT.TypeRepr tp)
                         | InvalidSymbolName String

deriving instance Show SimulationException

instance X.Exception SimulationException
