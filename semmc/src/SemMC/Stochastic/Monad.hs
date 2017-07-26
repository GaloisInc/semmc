{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Stochastic.Monad (
  Syn,
  SynC,
  SynEnv(..),
  LocalSynEnv(..),
  loadInitialState,
  runSyn,
  Config(..),
  -- * Operations
  askGen,
  askBaseSet,
  askConfig,
  askTestCases,
  askFormulas,
  lookupFormula,
  withSymBackend,
  addTestCase,
  recordLearnedFormula,
  takeWork,
  addWork,
  Sym
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent.STM as STM
import           Control.Monad ( replicateM )
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as S
import           System.FilePath ( (</>), (<.>) )

import qualified Data.EnumF as P
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )

import           Lang.Crucible.BaseTypes ( knownRepr )
import qualified Lang.Crucible.Solver.SimpleBackend as CRU
import qualified Lang.Crucible.Solver.Interface as CRUI

import           Data.Parameterized.Witness ( Witness(..) )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction.Random as D
import qualified Data.Set.NonEmpty as NES

import           SemMC.Architecture ( ArchState, Architecture, Opcode, Operand, Instruction )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Formula.Load as F
import           SemMC.Util ( makeSymbol )
import qualified SemMC.Worklist as WL

import qualified SemMC.Stochastic.Remote as R
import qualified SemMC.Stochastic.Statistics as S

-- | Symbolic something?
type Sym t = CRU.SimpleBackend t

-- | Synthesis environment.
data SynEnv t arch =
  SynEnv { seFormulas :: STM.TVar (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
         -- ^ All of the known formulas (base set + learned set)
         , seWorklist :: STM.TVar (WL.Worklist (Some (Opcode arch (Operand arch))))
         -- ^ Work items
         , seTestCases :: STM.TVar [ArchState (Sym t) arch]
         -- ^ All of the test cases we have accumulated.  This includes a set of
         -- initial heuristically interesting tests, as well as a set of ~1000
         -- random tests.  It also includes counterexamples learned during
         -- classification.
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
              , seRandomGen :: A.Gen
              }

-- Synthesis constraints.
type SynC arch = ( P.OrdF (Opcode arch (Operand arch))
                 , D.ArbitraryOperand (Operand arch)
                 , D.ArbitraryOperands (Opcode arch) (Operand arch)
                 , Ord (Instruction arch)
                 , P.EnumF (Opcode arch (Operand arch)) )

-- Synthesis monad.
newtype Syn t arch a = Syn { unSyn :: R.ReaderT (LocalSynEnv t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            R.MonadReader (LocalSynEnv t arch))

runSyn :: LocalSynEnv t arch -> Syn t arch a -> IO a
runSyn e a = R.runReaderT (unSyn a) e

-- | Record a learned formula for the opcode in the state
recordLearnedFormula :: (P.OrdF (Opcode arch (Operand arch)))
                     => Opcode arch (Operand arch) sh
                     -> F.ParameterizedFormula (Sym t) arch sh
                     -> Syn t arch ()
recordLearnedFormula op f = do
  mref <- R.asks (seFormulas . seGlobalEnv)
  liftIO $ STM.atomically $ do
    STM.modifyTVar' mref (MapF.insert op f)

-- | Take an opcode off of the worklist
takeWork :: Syn t arch (Maybe (Some (Opcode arch (Operand arch))))
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
addWork :: Opcode arch (Operand arch) sh -> Syn t arch ()
addWork op = do
  wlref <- R.asks (seWorklist . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' wlref (WL.putWork (Some op))

askConfig :: Syn t arch (Config arch)
askConfig = R.asks (seConfig . seGlobalEnv)

askGen :: Syn t arch A.Gen
askGen = R.asks seRandomGen

withSymBackend :: (Sym t -> Syn t arch a) -> Syn t arch a
withSymBackend k = do
  -- FIXME: Use a bracket here
  symVar <- R.asks (seSymBackend . seGlobalEnv)
  sym <- liftIO $ STM.atomically $ STM.takeTMVar symVar
  res <- k sym
  liftIO $ STM.atomically $ STM.putTMVar symVar sym
  return res

askTestCases :: Syn t arch [ArchState (Sym t) arch]
askTestCases = R.asks (seTestCases . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

-- | Add a counterexample test case to the set of tests
addTestCase :: ArchState (Sym t) arch -> Syn t arch ()
addTestCase tc = do
  testref <- R.asks (seTestCases . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' testref (tc:)

askFormulas :: Syn t arch (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
askFormulas = R.asks (seFormulas . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

-- | Return the set of opcodes with known semantics.
--
-- WARNING: The use of "base set" here with 'askBaseSet' is not the
-- same as in 'Config.baseSetDir' below, where we distinguish between
-- the initial base set and the opcodes we learn semantics for
-- later. The occurrences of "base set" in 'Dismantle.Random' refer to
-- the whole set like here with 'askBaseSet', which is also consistent
-- with the STRATA paper. We probably want to change one of these
-- naming conventions to make them distinct.
askBaseSet :: Ord (Some (Opcode arch (Operand arch)))
           => Syn t arch (NES.Set (Some (Opcode arch (Operand arch))))
askBaseSet = do
  -- Since we don't update the base set during a round, it would make
  -- sense to cache this for constant lookup, instead of doing this
  -- O(n) lookup every time!
  xs <- MapF.keys <$> askFormulas
  case xs of
    [] -> L.error "askBaseSet: empty base set!"
    (x:xs') -> return $ NES.fromList x xs'

lookupFormula :: (Architecture arch)
              => Opcode arch (Operand arch) sh
              -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
lookupFormula op = do
  frms <- askFormulas
  return $ MapF.lookup op frms

data Config arch =
  Config { baseSetDir :: FilePath
         , learnedSetDir :: FilePath
         , statisticsFile :: FilePath
         -- ^ A file to store statistics in
         , programCountThreshold :: Int
         -- ^ Minimum number of equivalent programs to require
         -- before stopping the stochastic search
         , randomTestCount :: Int
         -- ^ The number of random tests to generate
         , threadCount :: Int
         , remoteHost :: String
         , machineState :: forall t . () -> R.MachineState (ArchState (Sym t) arch)
         }

loadInitialState :: (Architecture arch,
                     D.ArbitraryOperands (Opcode arch) (Operand arch))
                 => Config arch
                 -> Sym t
                 -> IO (ArchState (Sym t) arch)
                 -- ^ A generator of random test cases
                 -> [ArchState (Sym t) arch]
                 -- ^ Heuristically-interesting test cases
                 -> [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
                 -- ^ All possible opcodes
                 -> [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
                 -- ^ The opcodes we want to learn formulas for (could be all, but could omit instructions e.g., jumps)
                 -> IO (SynEnv t arch)
loadInitialState cfg sym genTest interestingTests allOpcodes targetOpcodes = do
  undefinedBit <- CRUI.freshConstant sym (makeSymbol "undefined_bit") knownRepr
  let toFP dir oc = dir </> P.showF oc <.> "sem"
      -- TODO: handle uninterpreted functions
      env = F.FormulaEnv { F.envFunctions = Map.empty
                         , F.envUndefinedBit = undefinedBit
                         }
      load dir = F.loadFormulas sym (toFP dir) env allOpcodes
  baseSet <- load (baseSetDir cfg)
  learnedSet <- load (learnedSetDir cfg)
  let initialFormulas = MapF.union baseSet learnedSet
  fref <- STM.newTVarIO initialFormulas
  wlref <- STM.newTVarIO (makeWorklist targetOpcodes initialFormulas)
  randomTests <- replicateM (randomTestCount cfg) genTest
  testref <- STM.newTVarIO (interestingTests ++ randomTests)
  symVar <- STM.newTMVarIO sym
  statsThread <- S.newStatisticsThread (statisticsFile cfg)
  return SynEnv { seFormulas = fref
                , seTestCases = testref
                , seWorklist = wlref
                , seSymBackend = symVar
                , seConfig = cfg
                , seStatsThread = statsThread
                }

-- | The worklist consists of all of the opcodes for which we do not already
-- have a formula (and that we actually want to learn)
makeWorklist :: (MapF.OrdF (Opcode arch (Operand arch)))
             => [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
             -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch)
             -> WL.Worklist (Some (Opcode arch (Operand arch)))
makeWorklist allOps knownFormulas = WL.fromList (S.toList opSet')
  where
    opSet = S.fromList [ Some op | Some (Witness op) <- allOps ]
    opSet' = F.foldl' removeIfPresent opSet (MapF.keys knownFormulas)
    removeIfPresent s sop = S.delete sop s

{-

FIXME:

* Persist test cases for determinism?

-}
