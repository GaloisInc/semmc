{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  tryEither,
  -- * Environment queries
  askGen,
  askBaseSet,
  askConfig,
  askTestCases,
  askFormulas,
  askPseudoFormulas,
  askKnownCongruentOps,
  opcodeIORelation,
  -- * Symbolic backend manipulation
  withSymBackend,
  withTimeout,
  -- * Recording results
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
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as C
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans ( MonadIO, liftIO )
import           Data.IORef ( IORef, readIORef, modifyIORef' )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text.IO as T
import           Data.Typeable ( Typeable )
import           Data.Word ( Word64 )
import qualified System.Timeout as IO

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( mapSome, Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )

import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.ShapedList as SL
import qualified Dismantle.Arbitrary as DA
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Seq as SeqF
import qualified SemMC.Architecture as A
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym )
import qualified SemMC.Util as U
import qualified SemMC.Worklist as WL

import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Concrete.State as CS
import           SemMC.Stochastic.Constraints ( SynC )
import           SemMC.Stochastic.IORelation ( IORelation )
import           SemMC.Stochastic.Initialize ( Config(..), SynEnv(..), mkFormulaFilename )
import           SemMC.Stochastic.Pseudo
                 ( Pseudo
                 , SynthOpcode(..)
                 )

-- | Thread-local environment
--
-- This includes a remote connection to run test cases, as well as a nonce
-- source for test cases.
data LocalSynEnv t arch =
  LocalSynEnv { seGlobalEnv :: SynEnv t arch
              , seRandomGen :: DA.Gen
              , seNonceSource :: IORef Word64
              -- ^ Nonces for test cases sent to the remote runner.
              , seTestChan :: C.Chan (Maybe (CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)))
              , seResChan :: C.Chan (CE.ResultOrError (CS.ConcreteState arch))
              }

-- | A monad for the stochastic synthesis code.  It maintains the necessary
-- environment to connect to the theorem prover and run remote tests.
newtype Syn t arch a = Syn { unSyn :: R.ReaderT (LocalSynEnv t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            R.MonadReader (LocalSynEnv t arch))

instance U.MonadHasLogCfg (Syn t arch) where
  getLogCfgM = logConfig <$> askConfig

-- | This is the exception that is thrown if the synthesis times out waiting for
-- a result from the remote test runner.
data RemoteRunnerTimeout arch = RemoteRunnerTimeout (Proxy arch) [CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)]

instance (SynC arch) => Show (RemoteRunnerTimeout arch) where
  show (RemoteRunnerTimeout _ tcs) = unwords [ "RemoteRunnerTimeout", show tcs ]

instance (SynC arch, Typeable arch) => C.Exception (RemoteRunnerTimeout arch)

-- | Runner for 'Syn' monad.
--
-- The ty vars are explicitly quantified with @arch@ first so that we
-- can use @-XTypeApplications@ more conveniently.
runSyn :: forall arch t a. LocalSynEnv t arch -> Syn t arch a -> IO a
runSyn e a = R.runReaderT (unSyn a) e

-- | Run a 'Syn' action, returning thrown exceptions as a 'Left'
tryEither :: Syn t arch a -> Syn t arch (Either C.SomeException a)
tryEither syn = do
  localEnv <- R.ask
  a <- liftIO $ A.async (runSyn localEnv syn)
  liftIO (A.waitCatch a)

-- | Run a computation under the general timeout for the maximum operation
-- length for any synthesis operation
withTimeout :: Syn t arch a -> Syn t arch (Maybe a)
withTimeout action = do
  us <- timeoutMicroseconds opcodeTimeoutSeconds
  env <- R.ask
  liftIO $ IO.timeout us $ runSyn env action


-- | Record a learned formula for the opcode in the state
recordLearnedFormula :: (SynC arch, F.ConvertShape sh) -- P.OrdF (A.Opcode arch (A.Operand arch)),
                         -- HasRepr (A.Opcode arch (A.Operand arch)) SL.ShapeRepr)
                     => A.Opcode arch (A.Operand arch) sh
                     -> F.ParameterizedFormula (Sym t) arch sh
                     -> Syn t arch ()
recordLearnedFormula op f = do
  formulasRef <- R.asks (seFormulas . seGlobalEnv)
  congruentRef <- R.asks (seKnownCongruentOps . seGlobalEnv)
  learnedDir <- R.asks (learnedSetDir . seConfig . seGlobalEnv)

  let opShape = typeRepr op
      newOps = SeqF.singleton (RealOpcode op)
  liftIO $ T.writeFile (mkFormulaFilename learnedDir op) (F.printFormula f)
  liftIO $ STM.atomically $ do
    STM.modifyTVar' formulasRef (MapF.insert op f)
    STM.modifyTVar' congruentRef (MapF.insertWith (SeqF.><) opShape newOps)

-- | Take an opcode off of the worklist
takeWork :: Syn t arch (Maybe (Some (Witness F.ConvertShape (A.Opcode arch (A.Operand arch)))))
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
addWork :: Some (Witness F.ConvertShape (A.Opcode arch (A.Operand arch))) -> Syn t arch ()
addWork op = do
  wlref <- R.asks (seWorklist . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' wlref (WL.putWork op)

askConfig :: Syn t arch (Config arch)
askConfig = R.asks (seConfig . seGlobalEnv)

askGen :: Syn t arch DA.Gen
askGen = R.asks seRandomGen

timeoutMicroseconds :: (Num b) => (Config arch -> b) -> Syn t arch b
timeoutMicroseconds accessor = do
  seconds <- R.asks (accessor . seConfig . seGlobalEnv)
  return (seconds * 1000 * 1000)

-- | Get access to the symbolic backend to compute something.
--
-- NOTE: Nested calls to this function are not allowed!  It takes an MVar, so
-- nested calls will deadlock.
withSymBackend :: (Sym t -> Syn t arch a) -> Syn t arch a
withSymBackend k = do
  symVar <- R.asks (seSymBackend . seGlobalEnv)
  env <- R.ask
  liftIO $ C.bracket (STM.atomically $ STM.takeTMVar symVar)
                     (STM.atomically . STM.putTMVar symVar)
                     (runSyn env . k)

askTestCases :: Syn t arch [CS.ConcreteState arch]
askTestCases = R.asks (seTestCases . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

-- | Add a counterexample test case to the set of tests
addTestCase :: CS.ConcreteState arch -> Syn t arch ()
addTestCase tc = do
  testref <- R.asks (seTestCases . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' testref (tc:)

askFormulas :: Syn t arch (MapF.MapF ((A.Opcode arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch))
askFormulas = R.asks (seFormulas . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

askPseudoFormulas :: Syn t arch (MapF.MapF (Pseudo arch (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch))
askPseudoFormulas = R.asks (sePseudoFormulas . seGlobalEnv)

askKnownCongruentOps :: Syn t arch (MapF.MapF SL.ShapeRepr (SeqF.SeqF (SynthOpcode arch)))
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
               P.OrdF (Pseudo arch (A.Operand arch)))
           => Syn t arch (NES.Set (Some (SynthOpcode arch)))
askBaseSet = do
  -- Since we don't update the base set during a round, it would make
  -- sense to cache this for constant lookup, instead of doing this
  -- O(n) lookup every time!
  realOps <- map (mapSome RealOpcode) . MapF.keys <$> askFormulas
  pseudoOps <- map (mapSome PseudoOpcode) . MapF.keys <$> askPseudoFormulas
  let allOps = realOps ++ pseudoOps
  case allOps of
    [] -> L.error "askBaseSet: empty base set!"
    (x:xs) -> return $ NES.fromList x xs

opcodeIORelation :: (A.Architecture arch)
                 => A.Opcode arch (A.Operand arch) sh
                 -> Syn t arch (Maybe (IORelation arch sh))
opcodeIORelation op = do
  iorels <- R.asks (seIORelations . seGlobalEnv)
  return $ MapF.lookup op iorels


-- | Wrap a test vector + test program into a form suitable for the test runners
-- (see 'runConcreteTest' and 'runConcreteTests').
mkTestCase :: CS.ConcreteState arch
           -> [A.Instruction arch]
           -> Syn t arch (CE.TestCase (CS.ConcreteState arch) (A.Instruction arch))
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
runConcreteTests :: forall t arch
                  . (SynC arch)
                 => [CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)]
                 -> Syn t arch (CE.ResultIndex (CS.ConcreteState arch))
runConcreteTests tests = do
  tChan <- R.asks seTestChan
  rChan <- R.asks seResChan
  us <- timeoutMicroseconds remoteRunnerTimeoutSeconds
  mresults <- liftIO $ IO.timeout us $ CE.withTestResults tChan rChan tests return
  case mresults of
    Nothing -> liftIO $ C.throwIO $ RemoteRunnerTimeout (Proxy @arch) tests
    Just results -> return (CE.indexResults results)

-- | Run a single test case and return the result
--
-- Throws a 'RemoteRunnerTimeout' exception if there is a timeout.
--
-- Calls 'L.error' if more than one result is returned.
runConcreteTest :: forall t arch
                 . (SynC arch)
                => CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)
                -> Syn t arch (CE.ResultOrError (CS.ConcreteState arch))
runConcreteTest tc = do
  tChan <- R.asks seTestChan
  rChan <- R.asks seResChan
  us <- timeoutMicroseconds remoteRunnerTimeoutSeconds
  mresults <- liftIO $ IO.timeout us $ CE.withTestResults tChan rChan [tc] return
  case mresults of
    Just [result] -> return result
    Nothing -> liftIO $ C.throwIO $ RemoteRunnerTimeout (Proxy @arch) [tc]
    _ -> L.error "Unexpected number of results from a single concrete test run"

{-

FIXME:

* Persist test cases for determinism?

-}
