{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Stochastic.Monad (
  Syn,
  SynC,
  SynEnv(..),
  LocalSynEnv(..),
  loadInitialState,
  runSyn,
  tryJust,
  Test,
  Config(..),
  -- * Candidate Programs
  CandidateProgram(..),
  programFormula,
  -- * Operations
  askGen,
  askBaseSet,
  askConfig,
  askTestCases,
  askFormulas,
  askPseudoFormulas,
  lookupFormula,
  lookupCongruent,
  withSymBackend,
  withTimeout,
  addTestCase,
  mkTestCase,
  runConcreteTests,
  runConcreteTest,
  recordLearnedFormula,
  instantiateFormula,
  takeWork,
  addWork,
  opcodeIORelation
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as C
import           Control.Monad ( replicateM )
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.Constraint as C
import qualified Data.Foldable as F
import qualified Data.Functor.Identity as I
import           Data.IORef ( IORef, readIORef, modifyIORef' )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import           Data.Typeable ( Typeable )
import           Data.Word ( Word64 )
import           System.FilePath ( (</>), (<.>) )
import qualified System.Timeout as IO

import qualified Data.EnumF as P
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( mapSome, Some(..) )

import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.ShapedList as SL
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.Instruction.Random as D
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Seq as SeqF
import qualified SemMC.Architecture as A
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym )
import qualified SemMC.Util as U
import qualified SemMC.Worklist as WL

import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Concrete.State as CS
import           SemMC.Stochastic.IORelation ( IORelation )
import qualified SemMC.Stochastic.IORelation.Types as I
import           SemMC.Stochastic.Pseudo
                 ( ArchitectureWithPseudo(..)
                 , Pseudo
                 , SynthOpcode(..)
                 , SynthInstruction(..)
                 )
import qualified SemMC.Stochastic.Statistics as S

-- | A test here is an initial machine state.
--
-- The actual test process is to run the target program and candidate
-- program on the initial state and compare the final states in some
-- way.
--
-- The 'SynEnv' type has a collection of 'Test's, but we may want to
-- cache the result of evaluating the target on them, in which case we
-- can change this to be a pair of states.
--
-- Note there is also 'I.TestCase', which includes a machine state
-- (the 'Test' here), a program, and a nonce. We don't want to store
-- those tests here, since the nonce is just an implementation detail
-- for matching test inputs with their results, and the same 'Test'
-- will be run on multiple programs.
type Test arch = CS.ConcreteState arch

-- | Synthesis environment.
data SynEnv t arch =
  SynEnv { seFormulas :: STM.TVar (MapF.MapF ((A.Opcode arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch))
         -- ^ All of the known formulas (base set + learned set) for real opcodes
         , sePseudoFormulas :: MapF.MapF ((Pseudo arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch)
         -- ^ Formulas for all pseudo opcodes
         , seKnownCongruentOps :: STM.TVar (MapF.MapF SL.ShapeRepr (SeqF.SeqF (SynthOpcode arch)))
         -- ^ All opcodes with known formulas with operands of a given shape
         , seWorklist :: STM.TVar (WL.Worklist (Some (A.Opcode arch (A.Operand arch))))
         -- ^ Work items
         , seTestCases :: STM.TVar [CS.ConcreteState arch]
         -- ^ All of the test cases we have accumulated.  This includes a set of
         -- initial heuristically interesting tests, as well as a set of ~1000
         -- random tests.  It also includes counterexamples learned during
         -- classification.
         , seIORelations :: MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch)
         -- ^ Descriptions of implicit and explicit operands for each opcode
         , seSymBackend :: STM.TMVar (Sym t)
         -- ^ The solver backend from Crucible.  This is behind a TMVar because
         -- it isn't thread safe - we have to serialize access.  We can't
         -- allocate one per thread, otherwise we won't be able to compare
         -- formulas (they will have different @t@ parameters).
         , seStatsThread :: S.StatisticsThread arch
         -- ^ A thread for maintaining statistics about the search
         , seConfig :: Config arch
         -- ^ The initial configuration
         }

-- | Thread-local environment
--
-- This includes a remote connection to run test cases
data LocalSynEnv t arch =
  LocalSynEnv { seGlobalEnv :: SynEnv t arch
              , seRandomGen :: DA.Gen
              , seNonceSource :: IORef Word64
              -- ^ Nonces for test cases sent to the remote runner.
              , seTestChan :: C.Chan (Maybe (CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)))
              , seResChan :: C.Chan (CE.ResultOrError (CS.ConcreteState arch))
              }

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

data CandidateProgram t arch =
  CandidateProgram { cpInstructions :: [SynthInstruction arch]
                   , cpFormula :: F.Formula (Sym t) arch
                   }

-- | Convert an instruction into a 'F.Formula'
instructionFormula :: (ArchitectureWithPseudo arch)
                   => Sym t
                   -> SynthInstruction arch
                   -> Syn t arch (F.Formula (Sym t) arch)
instructionFormula sym i = do
  case i of
    SynthInstruction op operands -> do
      Just pf <- lookupFormula op
      (_, f) <- liftIO $ F.instantiateFormula sym pf operands
      return f

-- | Convert a program into a formula
programFormula :: (ArchitectureWithPseudo arch)
               => Sym t
               -> [SynthInstruction arch]
               -> Syn t arch (F.Formula (Sym t) arch)
programFormula sym insns = do
  fs <- mapM (instructionFormula sym) insns
  liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula fs


-- | Synthesis constraints.
type SynC arch = ( P.OrdF (A.Opcode arch (A.Operand arch))
                 , P.OrdF (A.Operand arch)
                 , P.ShowF (A.Operand arch)
                 , P.ShowF (A.Opcode arch (A.Operand arch))
                 , D.ArbitraryOperand (A.Operand arch)
                 , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
                 , D.ArbitraryOperands (Pseudo arch) (A.Operand arch)
                 , P.EnumF (A.Opcode arch (A.Operand arch))
                 , HasRepr (A.Opcode arch (A.Operand arch)) SL.ShapeRepr
                 , HasRepr (Pseudo arch (A.Operand arch)) SL.ShapeRepr
                 , CS.ConcreteArchitecture arch
                 , ArchitectureWithPseudo arch )

-- Synthesis monad.
newtype Syn t arch a = Syn { unSyn :: R.ReaderT (LocalSynEnv t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            R.MonadReader (LocalSynEnv t arch))

instance U.MonadHasLogCfg (Syn t arch) where
  getLogCfgM = logConfig <$> askConfig

-- | Runner for 'Syn' monad.
--
-- The ty vars are explicitly quantified with @arch@ first so that we
-- can use @-XTypeApplications@ more conveniently.
runSyn :: forall arch t a. LocalSynEnv t arch -> Syn t arch a -> IO a
runSyn e a = R.runReaderT (unSyn a) e

-- | Run a computation under the general timeout for the maximum operation
-- length for any synthesis operation
withTimeout :: Syn t arch a -> Syn t arch (Maybe a)
withTimeout action = do
  us <- timeoutMicroseconds opcodeTimeoutSeconds
  env <- R.ask
  liftIO $ IO.timeout us $ runSyn env action

-- | A version of 'C.tryJust' wrapped for our 'Syn' monad.
--
-- The @unliftio@ package generalizes this idea and provides a
-- @tryJust@ like this.
tryJust :: C.Exception e
        => (e -> Maybe b) -> Syn t arch a -> Syn t arch (Either b a)
tryJust p action = do
  localEnv <- R.ask
  liftIO $ C.tryJust p (runSyn localEnv action)

-- | Record a learned formula for the opcode in the state
recordLearnedFormula :: (P.OrdF (A.Opcode arch (A.Operand arch)),
                         HasRepr (A.Opcode arch (A.Operand arch)) SL.ShapeRepr)
                     => A.Opcode arch (A.Operand arch) sh
                     -> F.ParameterizedFormula (Sym t) arch sh
                     -> Syn t arch ()
recordLearnedFormula op f = do
  formulasRef <- R.asks (seFormulas . seGlobalEnv)
  congruentRef <- R.asks (seKnownCongruentOps . seGlobalEnv)
  let opShape = typeRepr op
      newOps = SeqF.singleton (RealOpcode op)
  liftIO $ STM.atomically $ do
    STM.modifyTVar' formulasRef (MapF.insert op f)
    STM.modifyTVar' congruentRef (MapF.insertWith (SeqF.><) opShape newOps)

-- | Take an opcode off of the worklist
takeWork :: Syn t arch (Maybe (Some (A.Opcode arch (A.Operand arch))))
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
addWork :: A.Opcode arch (A.Operand arch) sh -> Syn t arch ()
addWork op = do
  wlref <- R.asks (seWorklist . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' wlref (WL.putWork (Some op))

askConfig :: Syn t arch (Config arch)
askConfig = R.asks (seConfig . seGlobalEnv)

askGen :: Syn t arch DA.Gen
askGen = R.asks seRandomGen

timeoutMicroseconds :: (Num b) => (Config arch -> b) -> Syn t arch b
timeoutMicroseconds accessor = do
  seconds <- R.asks (accessor . seConfig . seGlobalEnv)
  return (seconds * 1000 * 1000)

data RemoteRunnerTimeout arch = RemoteRunnerTimeout (Proxy arch) [CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)]

instance (SynC arch) => Show (RemoteRunnerTimeout arch) where
  show (RemoteRunnerTimeout _ tcs) = unwords [ "RemoteRunnerTimeout", show tcs ]

instance (SynC arch, Typeable arch) => C.Exception (RemoteRunnerTimeout arch)

-- | Run a set of concrete tests
--
-- Returns 'Nothing' if the remote runner doesn't respond by the configured
-- timeout ('remoteRunnerTimeoutSeconds')
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

lookupFormula :: (ArchitectureWithPseudo arch)
              => SynthOpcode arch sh
              -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
lookupFormula (RealOpcode op) = MapF.lookup op <$> askFormulas
lookupFormula (PseudoOpcode pseudo) = MapF.lookup pseudo <$> askPseudoFormulas

lookupCongruent :: (HasRepr (A.Opcode arch (A.Operand arch)) SL.ShapeRepr,
                    HasRepr (Pseudo arch (A.Operand arch)) SL.ShapeRepr)
                => SynthOpcode arch sh
                -> Syn t arch (Seq.Seq (SynthOpcode arch sh))
lookupCongruent op = maybe Seq.empty SeqF.unSeqF . MapF.lookup (typeRepr op) <$> askKnownCongruentOps

instantiateFormula :: (ArchitectureWithPseudo arch)
                   => SynthInstruction arch
                   -> Syn t arch (Maybe (F.Formula (Sym t) arch))
instantiateFormula (SynthInstruction opcode oplist) = do
  mpf <- lookupFormula opcode
  case mpf of
    Nothing -> return Nothing
    Just pf -> do
      withSymBackend $ \sym -> do
        (_, f) <- liftIO $ F.instantiateFormula sym pf oplist
        return (Just f)

opcodeIORelation :: (A.Architecture arch)
                 => A.Opcode arch (A.Operand arch) sh
                 -> Syn t arch (Maybe (IORelation arch sh))
opcodeIORelation op = do
  iorels <- R.asks (seIORelations . seGlobalEnv)
  return $ MapF.lookup op iorels

data Config arch =
  Config { baseSetDir :: FilePath
         , pseudoSetDir :: FilePath
         , learnedSetDir :: FilePath
         , statisticsFile :: FilePath
         -- ^ A file to store statistics in
         , programCountThreshold :: Int
         -- ^ Minimum number of equivalent programs to require
         -- before stopping the stochastic search
         , randomTestCount :: Int
         -- ^ The number of random tests to generate
         , threadCount :: Int
         -- ^ The number of threads to use for the search.  More is usually
         -- better, but there is locking around the symbolic backend (i.e.,
         -- 'SimpleBuilder'), so there could be contention if there is too much
         -- parallelism.
         , logChannel :: C.Chan CE.LogMessage
         -- ^ The channel to send log messages to from the remote runner
         , remoteRunnerTimeoutSeconds :: Int
         -- ^ The number of seconds to wait for a response from the remote
         -- runner after sending a batch of tests
         , opcodeTimeoutSeconds :: Int
         -- ^ The number of seconds to spend trying to find a candidate program
         -- that explains a target instruction before giving up.
         , testRunner :: I.TestRunner arch
         -- ^ See the related @lcTestRunner@ for usage examples.
         , logConfig :: U.LogCfg
         -- ^ A configuration for the general logging facility
         }

addCongruentOp :: (HasRepr a SL.ShapeRepr)
               => a sh -- Witness c a sh
               -> v
               -> MapF.MapF SL.ShapeRepr (SeqF.SeqF a)
               -> MapF.MapF SL.ShapeRepr (SeqF.SeqF a)
addCongruentOp op _ = MapF.insertWith (SeqF.><) (typeRepr op) (SeqF.singleton op)

dropKeyWitnesses :: forall c a v . (MapF.OrdF a) => MapF.MapF (Witness c a) v -> MapF.MapF a v
dropKeyWitnesses = I.runIdentity . U.mapFMapBothM f
  where
    f :: Witness c a sh -> v sh -> I.Identity (a sh, v sh)
    f (Witness a) v = return (a, v)

loadInitialState :: forall arch t
                  . (SynC arch, U.HasLogCfg)
                 => Config arch
                 -> Sym t
                 -> IO (CS.ConcreteState arch)
                 -- ^ A generator of random test cases
                 -> [CS.ConcreteState arch]
                 -- ^ Heuristically-interesting test cases
                 -> [Some (Witness (F.BuildOperandList arch) ((A.Opcode arch) (A.Operand arch)))]
                 -- ^ All possible opcodes. These are used to guess
                 -- the names of opcode semantics files to attempt to
                 -- read from disk.
                 -> [Some (Witness (F.BuildOperandList arch) ((Pseudo arch) (A.Operand arch)))]
                 -- ^ All pseudo opcodes
                 -> [Some (Witness (F.BuildOperandList arch) ((A.Opcode arch) (A.Operand arch)))]
                 -- ^ The opcodes we want to learn formulas for (could
                 -- be all, but could omit instructions e.g., jumps)
                 -> MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch)
                 -- ^ IORelations
                 -> IO (SynEnv t arch)
loadInitialState cfg sym genTest interestingTests allOpcodes pseudoOpcodes targetOpcodes iorels = do
  let toFP dir oc = dir </> P.showF oc <.> "sem"
      load dir = F.loadFormulas sym (toFP dir) (C.Sub C.Dict) allOpcodes
  baseSet <- dropKeyWitnesses <$> load (baseSetDir cfg)
  learnedSet <- dropKeyWitnesses <$> load (learnedSetDir cfg)
  let initialFormulas = MapF.union baseSet learnedSet
  pseudoSet <- dropKeyWitnesses <$> F.loadFormulas sym (toFP (pseudoSetDir cfg)) (C.Sub C.Dict) pseudoOpcodes
  let congruentOps' = MapF.foldrWithKey (addCongruentOp . RealOpcode) MapF.empty initialFormulas
      congruentOps = MapF.foldrWithKey (addCongruentOp . PseudoOpcode) congruentOps' pseudoSet
  fref <- STM.newTVarIO initialFormulas
  congruentRef <- STM.newTVarIO congruentOps
  wlref <- STM.newTVarIO (makeWorklist targetOpcodes initialFormulas)
  randomTests <- replicateM (randomTestCount cfg) genTest
  testref <- STM.newTVarIO (interestingTests ++ randomTests)
  symVar <- STM.newTMVarIO sym
  statsThread <- S.newStatisticsThread (statisticsFile cfg)
  return SynEnv { seFormulas = fref
                , sePseudoFormulas = pseudoSet
                , seKnownCongruentOps = congruentRef
                , seTestCases = testref
                , seWorklist = wlref
                , seIORelations = iorels
                , seSymBackend = symVar
                , seConfig = cfg
                , seStatsThread = statsThread
                }

-- | The worklist consists of all of the opcodes for which we do not already
-- have a formula (and that we actually want to learn)
makeWorklist :: (MapF.OrdF (A.Opcode arch (A.Operand arch)))
             => [Some (Witness (F.BuildOperandList arch) ((A.Opcode arch) (A.Operand arch)))]
             -> MapF.MapF ((A.Opcode arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch)
             -> WL.Worklist (Some (A.Opcode arch (A.Operand arch)))
makeWorklist allOps knownFormulas = WL.fromList (S.toList opSet')
  where
    opSet = S.fromList [ Some op | Some (Witness op) <- allOps ]
    opSet' = F.foldl' removeIfPresent opSet (MapF.keys knownFormulas)
    removeIfPresent s sop = S.delete sop s


{-

FIXME:

* Persist test cases for determinism?

-}
