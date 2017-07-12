{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module SemMC.Stochastic.Monad (
  Syn,
  loadInitialState,
  runSyn,
  Config(..),
  -- * Operations
  askGen,
  askConfig,
  askTestCases,
  askFormulas,
  lookupFormula,
  askSymBackend,
  addTestCase,
  recordLearnedFormula,
  takeWork,
  addWork,
  Sym
  ) where

import qualified Control.Concurrent.STM as STM
import Control.Monad ( replicateM )
import qualified Control.Monad.Reader as R
import Control.Monad.Trans ( MonadIO, liftIO )
import System.FilePath ( (</>), (<.>) )

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )

import qualified Lang.Crucible.Solver.SimpleBackend as CRU

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( ArchState, Architecture, Opcode, Operand )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Formula.Load as F
import qualified SemMC.Worklist as WL
import SemMC.Util ( Witness(..) )

import qualified SemMC.Stochastic.Statistics as S

type Sym t = CRU.SimpleBackend t

data SynEnv t arch =
  SymEnv { seFormulas :: STM.TVar (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
         -- ^ All of the known formulas (base set + learned set)
         , seWorklist :: STM.TVar (WL.Worklist (Some (Opcode arch (Operand arch))))
         -- ^ Work items
         , seTestCases :: STM.TVar [ArchState (Sym t) arch]
         -- ^ All of the test cases we have accumulated.  This includes a set of
         -- initial heuristically interesting tests, as well as a set of ~1000
         -- random tests.  It also includes counterexamples learned during
         -- classification.
         , seAllOpcodes :: [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
         -- ^ The list of all available opcodes for the architecture
         , seRandomGen :: A.Gen
         -- ^ A random generator used for creating random instructions
         , seSymBackend :: Sym t
         -- ^ The solver backend from Crucible
         , seStatsThread :: S.StatisticsThread arch
         -- ^ A thread for maintaining statistics about the search
         , seConfig :: Config
         -- ^ The initial configuration
         }

-- | Thread-local environment
--
-- This includes a remote connection to run test cases
data LocalSymEnv = LocalSymEnv

newtype Syn t arch a = Syn { unSyn :: R.ReaderT (SynEnv t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            R.MonadReader (SynEnv t arch))

runSyn :: SynEnv t arch -> Syn t arch a -> IO a
runSyn e a = R.runReaderT (unSyn a) e

-- | Record a learned formula for the opcode in the state
recordLearnedFormula :: (P.OrdF (Opcode arch (Operand arch)))
                     => Opcode arch (Operand arch) sh
                     -> F.ParameterizedFormula (Sym t) arch sh
                     -> Syn t arch ()
recordLearnedFormula op f = do
  mref <- R.asks seFormulas
  liftIO $ STM.atomically $ do
    STM.modifyTVar' mref (MapF.insert op f)

-- | Take an opcode off of the worklist
takeWork :: Syn t arch (Maybe (Some (Opcode arch (Operand arch))))
takeWork = do
  wlref <- R.asks seWorklist
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
  wlref <- R.asks seWorklist
  liftIO $ STM.atomically $ STM.modifyTVar' wlref (WL.putWork (Some op))

askConfig :: Syn t arch Config
askConfig = R.asks seConfig

askGen :: Syn t arch A.Gen
askGen = R.asks seRandomGen

askSymBackend :: Syn t arch (Sym t)
askSymBackend = R.asks seSymBackend

askTestCases :: Syn t arch [ArchState (Sym t) arch]
askTestCases = R.asks seTestCases >>= (liftIO . STM.readTVarIO)

-- | Add a counterexample test case to the set of tests
addTestCase :: ArchState (Sym t) arch -> Syn t arch ()
addTestCase tc = do
  testref <- R.asks seTestCases
  liftIO $ STM.atomically $ STM.modifyTVar' testref (tc:)

askFormulas :: Syn t arch (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
askFormulas = R.asks seFormulas >>= (liftIO . STM.readTVarIO)

lookupFormula :: (Architecture arch)
              => Opcode arch (Operand arch) sh
              -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
lookupFormula op = do
  frms <- askFormulas
  return $ MapF.lookup op frms

data Config = Config { baseSetDir :: FilePath
                     , learnedSetDir :: FilePath
                     , statisticsFile :: FilePath
                     -- ^ A file to store statistics in
                     , programCountThreshold :: Int
                     -- ^ Minimum number of equivalent programs to require
                     -- before stopping the stochastic search
                     , randomTestCount :: Int
                     -- ^ The number of random tests to generate
                     }

loadInitialState :: (Architecture arch,
                     D.ArbitraryOperands (Opcode arch) (Operand arch))
                 => Config
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
  let toFP dir oc = dir </> P.showF oc <.> "sem"
      load dir = F.loadFormulas sym (toFP dir) allOpcodes
  baseSet <- load (baseSetDir cfg)
  learnedSet <- load (learnedSetDir cfg)
  let initialFormulas = MapF.union baseSet learnedSet
  fref <- STM.newTVarIO initialFormulas
  wlref <- STM.newTVarIO (makeWorklist targetOpcodes initialFormulas)
  randomTests <- replicateM (randomTestCount cfg) genTest
  testref <- STM.newTVarIO (interestingTests ++ randomTests)
  gen <- A.createGen
  statsThread <- S.newStatisticsThread (statisticsFile cfg)
  return SymEnv { seFormulas = fref
                , seTestCases = testref
                , seWorklist = wlref
                , seAllOpcodes = allOpcodes
                , seRandomGen = gen
                , seSymBackend = sym
                , seConfig = cfg
                , seStatsThread = statsThread
                }

-- | The worklist consists of all of the opcodes for which we do not already
-- have a formula (and that we actually want to learn)
makeWorklist :: [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
              -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch)
              -> WL.Worklist (Some (Opcode arch (Operand arch)))
makeWorklist allOps knownFormulas = WL.fromList []

{-

FIXME:

* Persist test cases for determinism?

-}
