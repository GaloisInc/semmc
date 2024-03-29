{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module defines a monad ('Syn') used in all of the stochastic synthesis code.
--
-- The 'Syn' monad is just a 'R.ReaderT' over 'IO', but this module encapsulates
-- all of the details.  The goal is that any function that needs *direct* access
-- to the environment of the reader should be in this module.  Code outside of
-- this module shouldn't have to know about any of the details.
module SemMC.Stochastic.Monad (
  -- * Monad definition
  Syn,
  SynC,
  LocalSynEnv(..),
  runSyn,
  runSynInNewLocalEnv,
  -- * Environment queries
  askGen,
  askGlobalEnv,
  askBaseSet,
  askConfig,
  askParallelSynth,
  askTestCases,
  askFormulas,
  askPseudoFormulas,
  askKnownCongruentOps,
  opcodeIORelation,
  -- * Symbolic backend manipulation
  withBackend,
  withSym,
  withTimeout,
  -- * Recording results
  withStats,
  timeSyn,
  addTestCase,
  recordLearnedFormula,
  -- * Worklist
  takeWork,
  addWork,
  -- * Remote test execution
  mkTestCase,
  runConcreteTests,
  runConcreteTest,
  -- * Exceptions
  RemoteRunnerTimeout(..)
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as C
import qualified Control.Monad.Catch as MC
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans ( MonadIO, liftIO )
import           Data.IORef ( IORef, readIORef, modifyIORef' )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text.IO as T
import qualified Data.Time.Clock as TM
import           Data.Typeable ( Typeable )
import           Data.Word ( Word64 )
import qualified System.Timeout as IO
import           Text.Printf (printf)
import qualified UnliftIO as U

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( mapSome, Some(..) )

import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Dismantle.Arbitrary as DA
import qualified Data.Set.NonEmpty as NES
import qualified Lang.Crucible.Backend as CB
import qualified What4.Protocol.Online as W4

import qualified Data.Parameterized.Seq as SeqF
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.View as V
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym, Backend )
import qualified SemMC.Util as U
import qualified SemMC.Worklist as WL

import qualified SemMC.Architecture.Pseudo as AP
import qualified SemMC.Concrete.Execution as CE
import           SemMC.Stochastic.Constraints ( SynC )
import           SemMC.Stochastic.IORelation ( IORelation )
import           SemMC.Stochastic.Initialize ( Config(..), SynEnv(..), mkFormulaFilename )
import qualified SemMC.Stochastic.Statistics as S

-- | Thread-local environment
--
-- This includes a remote connection to run test cases, as well as a nonce
-- source for test cases.
data LocalSynEnv solver t fs arch =
  LocalSynEnv { seGlobalEnv :: SynEnv solver t fs arch
              , seRandomGen :: DA.Gen
              , seNonceSource :: IORef Word64
              -- ^ Nonces for test cases sent to the remote runner.
              , seTestChan :: C.Chan (Maybe [CE.TestCase (V.ConcreteState arch) (A.Instruction arch)])
              , seResChan :: C.Chan (CE.ResultOrError (V.ConcreteState arch))
              }

-- | A monad for the stochastic synthesis code.  It maintains the necessary
-- environment to connect to the theorem prover and run remote tests.
--
-- Note: the 'U.MonadUnliftIO' instance makes sense for 'Reader' +
-- 'IO', but will no longer make sense if e.g. 'Syn' grows 'State'.
newtype Syn solver t fs arch a = Syn { unSyn :: R.ReaderT (LocalSynEnv solver t fs arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            U.MonadUnliftIO,
            R.MonadReader (LocalSynEnv solver t fs arch))

instance MC.MonadThrow (Syn solver t fs arch) where
  throwM = liftIO . MC.throwM

instance U.MonadHasLogCfg (Syn solver t fs arch) where
  getLogCfgM = logConfig <$> askConfig

-- | This is the exception that is thrown if the synthesis times out waiting for
-- a result from the remote test runner.
data RemoteRunnerTimeout arch = RemoteRunnerTimeout (Proxy arch) [CE.TestCase (V.ConcreteState arch) (A.Instruction arch)]

instance (SynC arch) => Show (RemoteRunnerTimeout arch) where
  show (RemoteRunnerTimeout _ tcs) =
    -- Printing full test case list results in lines 6 million
    -- characters long in the log output.
    printf "RemoteRunnerTimeout <%i test cases, omitted>" (length tcs)

instance (SynC arch, Typeable arch) => C.Exception (RemoteRunnerTimeout arch)

-- | Runner for 'Syn' monad.
--
-- The ty vars are explicitly quantified with @arch@ first so that we
-- can use @-XTypeApplications@ more conveniently.
runSyn :: forall arch t solver fs a . LocalSynEnv solver t fs arch -> Syn solver t fs arch a -> IO a
runSyn e a = R.runReaderT (unSyn a) e

-- | Run a computation under the general timeout for the maximum operation
-- length for any synthesis operation
withTimeout :: Syn solver t fs arch a -> Syn solver t fs arch (Maybe a, TM.NominalDiffTime)
withTimeout action = do
  us <- timeoutMicroseconds opcodeTimeoutSeconds
  timeSyn $ U.timeout us action

-- | Time an action, returning its value as well as the time taken to execute
-- the action
timeSyn :: Syn solver t fs arch a -> Syn solver t fs arch (a, TM.NominalDiffTime)
timeSyn action = do
  start <- liftIO TM.getCurrentTime
  res <- action
  end <- liftIO TM.getCurrentTime
  return (res, TM.diffUTCTime end start)

-- | Run a 'Syn' computation in a new local env with a new test
-- runner, and clean up the test runner at the end.
runSynInNewLocalEnv :: (U.MonadIO m, U.MonadUnliftIO m)
                    => SynEnv solver t fs arch -> Syn solver t fs arch a -> m a
runSynInNewLocalEnv env0 action = do
  nonceRef <- U.newIORef 0
  gen <- liftIO $ DA.createGen
  tChan <- U.newChan
  rChan <- U.newChan
  runner <- U.asyncLinked $ liftIO $ testRunner (seConfig env0) tChan rChan
  let newEnv = LocalSynEnv { seGlobalEnv = env0
                           , seRandomGen = gen
                           , seNonceSource = nonceRef
                           , seTestChan = tChan
                           , seResChan = rChan
                           }
  liftIO $ runSyn newEnv action `U.finally` U.cancel runner

-- | Record a learned formula for the opcode in the state
recordLearnedFormula :: (SynC arch)
                     => A.Opcode arch (A.Operand arch) sh
                     -> F.ParameterizedFormula (Sym t fs) arch sh
                     -> Syn solver t fs arch ()
recordLearnedFormula op f = do
  formulasRef <- R.asks (seFormulas . seGlobalEnv)
  congruentRef <- R.asks (seKnownCongruentOps . seGlobalEnv)
  learnedDir <- R.asks (learnedSetDir . seConfig . seGlobalEnv)

  let opShape = typeRepr op
      newOps = SeqF.singleton (AP.RealOpcode op)
  liftIO $ T.writeFile (mkFormulaFilename learnedDir op) (F.printParameterizedFormula opShape f)
  liftIO $ STM.atomically $ do
    STM.modifyTVar' formulasRef (MapF.insert op f)
    STM.modifyTVar' congruentRef (MapF.insertWith (SeqF.><) opShape newOps)

-- | Take an opcode off of the worklist
takeWork :: Syn solver t fs arch (Maybe (Some (A.Opcode arch (A.Operand arch))))
takeWork = do
  wlref <- R.asks (seWorklist . seGlobalEnv)
  liftIO $ STM.atomically $ do
    wl0 <- STM.readTVar wlref
    case WL.takeWork wl0 of
      Nothing -> return Nothing
      Just (work, rest) -> do
        STM.writeTVar wlref rest
        return (Just work)

-- | Add an opcode back to into the worklist
addWork :: Some (A.Opcode arch (A.Operand arch)) -> Syn solver t fs arch ()
addWork op = do
  wlref <- R.asks (seWorklist . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' wlref (WL.putWork op)

askConfig :: Syn solver t fs arch (Config arch)
askConfig = R.asks (seConfig . seGlobalEnv)

askGlobalEnv :: Syn solver t fs arch (SynEnv solver t fs arch)
askGlobalEnv = R.asks seGlobalEnv

askGen :: Syn solver t fs arch DA.Gen
askGen = R.asks seRandomGen

timeoutMicroseconds :: (Num b) => (Config arch -> b) -> Syn solver t fs arch b
timeoutMicroseconds accessor = do
  seconds <- R.asks (accessor . seConfig . seGlobalEnv)
  return (seconds * 1000 * 1000)

-- | Get access to the symbolic backend to compute something.
--
-- NOTE: Nested calls to this function are not allowed!  It takes an MVar, so
-- nested calls will deadlock.  Also, do not call `withSym`.
withBackend :: (Backend solver t fs -> Syn solver t fs arch a) -> Syn solver t fs arch a
withBackend k = do
  symVar <- R.asks (seSymBackend . seGlobalEnv)
  env <- R.ask
  liftIO $ C.bracket (STM.atomically $ STM.takeTMVar symVar)
                     (STM.atomically . STM.putTMVar symVar)
                     (runSyn env . k)

-- | Get access to the symbolic expression builder.
--
-- NOTE: Nested calls to this function are not allowed!  It takes an MVar, so
-- nested calls will deadlock.  Also, do not call `withBackend`.
withSym :: W4.OnlineSolver solver => (Sym t fs -> Syn solver t fs arch a) -> Syn solver t fs arch a
withSym k = withBackend (\bak -> k (CB.backendGetSym bak))

withStats :: (S.StatisticsThread arch -> IO ()) -> Syn solver t fs arch ()
withStats k = do
  st <- R.asks (statsThread . seConfig . seGlobalEnv)
  liftIO (k st)

askTestCases :: Syn solver t fs arch [V.ConcreteState arch]
askTestCases = R.asks (seTestCases . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

-- | Add a counterexample test case to the set of tests
addTestCase :: V.ConcreteState arch -> Syn solver t fs arch ()
addTestCase tc = do
  testref <- R.asks (seTestCases . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' testref (tc:)

askFormulas :: Syn solver t fs arch (MapF.MapF ((A.Opcode arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t fs) arch))
askFormulas = R.asks (seFormulas . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

askPseudoFormulas :: Syn t solver fs arch (MapF.MapF (AP.Pseudo arch (A.Operand arch)) (F.ParameterizedFormula (Sym solver fs) arch))
askPseudoFormulas = R.asks (sePseudoFormulas . seGlobalEnv)

-- | Get the number of requested parallel synthesis operations
askParallelSynth :: Syn solver t fs arch Int
askParallelSynth = R.asks (parallelSynth . seConfig . seGlobalEnv)

askKnownCongruentOps :: Syn solver t fs arch (MapF.MapF (A.ShapeRepr arch) (SeqF.SeqF (AP.SynthOpcode arch)))
askKnownCongruentOps = R.asks (seKnownCongruentOps . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

-- | Return the set of opcodes with known semantics.
--
-- WARNING: The use of "base set" here with 'askBaseSet' is not the
-- same as in 'Config.baseSetDir' below, where we distinguish between
-- the initial base set and the opcodes we learn semantics for
-- later. The occurrences of "base set" in 'Dismantle.Random' refer to
-- the whole set like here with 'askBaseSet', which is also consistent
-- with the STRATA paper. We probably want to change one of these
-- naming conventions to make them distinct.
askBaseSet :: (P.OrdF (A.Opcode arch (A.Operand arch)),
               P.OrdF (AP.Pseudo arch (A.Operand arch)))
           => Syn solver t fs arch (NES.Set (Some (AP.SynthOpcode arch)))
askBaseSet = do
  -- Since we don't update the base set during a round, it would make
  -- sense to cache this for constant lookup, instead of doing this
  -- O(n) lookup every time!
  realOps <- map (mapSome AP.RealOpcode) . MapF.keys <$> askFormulas
  pseudoOps <- map (mapSome AP.PseudoOpcode) . MapF.keys <$> askPseudoFormulas
  let allOps = realOps ++ pseudoOps
  case allOps of
    [] -> L.error "askBaseSet: empty base set!"
    (x:xs) -> return $ NES.fromList x xs

opcodeIORelation :: (A.Architecture arch)
                 => A.Opcode arch (A.Operand arch) sh
                 -> Syn solver t fs arch (Maybe (IORelation arch sh))
opcodeIORelation op = do
  iorels <- R.asks (seIORelations . seGlobalEnv)
  return $ MapF.lookup op iorels


-- | Wrap a test vector + test program into a form suitable for the test runners
-- (see 'runConcreteTest' and 'runConcreteTests').
mkTestCase :: V.ConcreteState arch
           -> [A.Instruction arch]
           -> Syn solver t fs arch (CE.TestCase (V.ConcreteState arch) (A.Instruction arch))
mkTestCase s0 prog = do
  nref <- R.asks seNonceSource
  nonce <- liftIO $ readIORef nref
  liftIO $ modifyIORef' nref (+1)
  return CE.TestCase { CE.testNonce = nonce
                     , CE.testContext = s0
                     , CE.testProgram = prog
                     }

-- | Run a set of concrete tests
--
-- The results are compiled into an index (basically a map from test nonce to
-- result) for easy lookups.
--
-- Throws a 'RemoteRunnerTimeout' exception if the remote runner doesn't respond
-- by the configured timeout ('remoteRunnerTimeoutSeconds').
runConcreteTests :: forall t solver fs arch
                  . (SynC arch)
                 => [CE.TestCase (V.ConcreteState arch) (A.Instruction arch)]
                 -> Syn solver t fs arch (CE.ResultIndex (V.ConcreteState arch))
runConcreteTests tests = do
  tChan <- R.asks seTestChan
  rChan <- R.asks seResChan
  us <- timeoutMicroseconds remoteRunnerTimeoutSeconds
  mresults <- liftIO $ IO.timeout us $ CE.withTestResults tChan rChan tests
  case mresults of
    Nothing -> liftIO $ C.throwIO $ RemoteRunnerTimeout (Proxy @arch) tests
    Just results -> return (CE.indexResults results)

-- | Run a single test case and return the result
--
-- Throws a 'RemoteRunnerTimeout' exception if there is a timeout.
--
-- Calls 'L.error' if more than one result is returned.
runConcreteTest :: forall t solver fs arch
                 . (SynC arch)
                => CE.TestCase (V.ConcreteState arch) (A.Instruction arch)
                -> Syn solver t fs arch (CE.ResultOrError (V.ConcreteState arch))
runConcreteTest tc = do
  tChan <- R.asks seTestChan
  rChan <- R.asks seResChan
  us <- timeoutMicroseconds remoteRunnerTimeoutSeconds
  mresults <- liftIO $ IO.timeout us $ CE.withTestResults tChan rChan [tc]
  case mresults of
    Just [result] -> return result
    Nothing -> liftIO $ C.throwIO $ RemoteRunnerTimeout (Proxy @arch) [tc]
    _ -> L.error "Unexpected number of results from a single concrete test run"

{-

FIXME:

* Persist test cases for determinism?

-}
