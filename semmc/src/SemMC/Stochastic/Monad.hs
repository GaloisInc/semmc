{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  SynthOpcode(..),
  SynthInstruction(..),
  synthInsnToActual,
  SynEnv(..),
  LocalSynEnv(..),
  loadInitialState,
  runSyn,
  Config(..),
  Test,
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
  opcodeIORelation
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent.STM as STM
import           Control.Monad ( replicateM )
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import           System.FilePath ( (</>), (<.>) )
import           Text.Printf ( printf )
import           Unsafe.Coerce ( unsafeCoerce )

import qualified Data.EnumF as P
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )

import           Lang.Crucible.BaseTypes ( knownRepr )
import qualified Lang.Crucible.Solver.Interface as CRUI

import           Data.Parameterized.Witness ( Witness(..) )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D
import qualified Data.Set.NonEmpty as NES

import           SemMC.Architecture ( Architecture, Opcode, Operand, Instruction )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Formula.Load as F
import           SemMC.Symbolic ( Sym )
import           SemMC.Util ( makeSymbol )
import qualified SemMC.Worklist as WL

import           SemMC.ConcreteState ( ConcreteArchitecture, ConcreteState )
import           SemMC.Stochastic.IORelation ( IORelation )
import qualified SemMC.Stochastic.Remote as R
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
type Test arch = ConcreteState arch

data SynthOpcode arch sh = RealOpcode (Opcode arch (Operand arch) sh)
                         | PseudoOpcode String (D.OperandList (Operand arch) sh -> [Instruction arch])

instance (Show (Opcode arch (Operand arch) sh)) => Show (SynthOpcode arch sh) where
  show (RealOpcode op) = printf "RealOpcode %s" (show op)
  show (PseudoOpcode name _) = printf "PseudoOpcode %s" name

instance forall arch . (P.ShowF (Opcode arch (Operand arch))) => P.ShowF (SynthOpcode arch) where
  withShow _ (_ :: q sh) x = P.withShow (Proxy @(Opcode arch (Operand arch)))
                                        (Proxy @sh)
                                        x

instance (P.TestEquality (Opcode arch (Operand arch))) => P.TestEquality (SynthOpcode arch) where
  testEquality (RealOpcode op1) (RealOpcode op2) =
    fmap (\P.Refl -> P.Refl) (P.testEquality op1 op2)
  testEquality (PseudoOpcode name1 _) (PseudoOpcode name2 _)
    | name1 == name2 = Just (unsafeCoerce P.Refl)
    | otherwise = Nothing
  testEquality _ _ = Nothing

instance forall arch . (P.OrdF (Opcode arch (Operand arch))) => P.OrdF (SynthOpcode arch) where
  compareF (RealOpcode op1) (RealOpcode op2) =
    case P.compareF op1 op2 of
      P.LTF -> P.LTF
      P.EQF -> P.EQF
      P.GTF -> P.GTF
  compareF (RealOpcode _) (PseudoOpcode _ _) = P.LTF
  compareF (PseudoOpcode _ _) (RealOpcode _) = P.GTF
  compareF (PseudoOpcode name1 _ :: SynthOpcode arch sh1) (PseudoOpcode name2 _ :: SynthOpcode arch sh2) =
    case compare name1 name2 of
      LT -> P.LTF
      EQ -> case unsafeCoerce P.Refl of
              (P.Refl :: sh1 P.:~: sh2) -> P.EQF
      GT -> P.GTF

data SynthInstruction arch =
  forall sh . SynthInstruction (SynthOpcode arch sh) (D.OperandList (Operand arch) sh)

synthInsnToActual :: SynthInstruction arch -> [Instruction arch]
synthInsnToActual (SynthInstruction opcode operands) =
  case opcode of
    RealOpcode opcode' -> [D.Instruction opcode' operands]
    PseudoOpcode _ convert -> convert operands

-- | Synthesis environment.
data SynEnv t arch =
  SynEnv { seFormulas :: STM.TVar (MapF.MapF (SynthOpcode arch) (F.ParameterizedFormula (Sym t) arch))
         -- ^ All of the known formulas (base set + learned set)
         , seWorklist :: STM.TVar (WL.Worklist (Some (Opcode arch (Operand arch))))
         -- ^ Work items
         , seTestCases :: STM.TVar [Test arch]
         -- ^ All of the test cases we have accumulated.  This includes a set of
         -- initial heuristically interesting tests, as well as a set of ~1000
         -- random tests.  It also includes counterexamples learned during
         -- classification.
         , seIORelations :: MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
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
              , seRandomGen :: A.Gen
              }

-- Synthesis constraints.
type SynC arch = ( P.OrdF (Opcode arch (Operand arch))
                 , P.OrdF (Operand arch)
                 , D.ArbitraryOperand (Operand arch)
                 , D.ArbitraryOperands (Opcode arch) (Operand arch)
                 , Ord (Instruction arch)
                 , P.EnumF (Opcode arch (Operand arch))
                 , ConcreteArchitecture arch )

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
    STM.modifyTVar' mref (MapF.insert (RealOpcode op) f)

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

askTestCases :: Syn t arch [Test arch]
askTestCases = R.asks (seTestCases . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

-- | Add a counterexample test case to the set of tests
addTestCase :: Test arch -> Syn t arch ()
addTestCase tc = do
  testref <- R.asks (seTestCases . seGlobalEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' testref (tc:)

askFormulas :: Syn t arch (MapF.MapF (SynthOpcode arch) (F.ParameterizedFormula (Sym t) arch))
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
askBaseSet :: P.OrdF (Opcode arch (Operand arch))
           => Syn t arch (NES.Set (Some (SynthOpcode arch)))
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
  return $ MapF.lookup (RealOpcode op) frms

opcodeIORelation :: (Architecture arch)
                 => Opcode arch (Operand arch) sh
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
         , remoteHost :: String
         , machineState :: R.MachineState (ConcreteState arch)
         }

loadInitialState :: (Architecture arch,
                     D.ArbitraryOperands (Opcode arch) (Operand arch))
                 => Config arch
                 -> Sym t
                 -> IO (ConcreteState arch)
                 -- ^ A generator of random test cases
                 -> [Test arch]
                 -- ^ Heuristically-interesting test cases
                 -> [Some (Witness (F.BuildOperandList arch) (SynthOpcode arch))]
                 -- ^ All possible opcodes
                 -> [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
                 -- ^ The opcodes we want to learn formulas for (could be all, but could omit instructions e.g., jumps)
                 -> MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
                 -- ^ IORelations
                 -> IO (SynEnv t arch)
loadInitialState cfg sym genTest interestingTests allOpcodes targetOpcodes iorels = do
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
                , seIORelations = iorels
                , seSymBackend = symVar
                , seConfig = cfg
                , seStatsThread = statsThread
                }

-- | The worklist consists of all of the opcodes for which we do not already
-- have a formula (and that we actually want to learn)
makeWorklist :: (MapF.OrdF (Opcode arch (Operand arch)))
             => [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
             -> MapF.MapF (SynthOpcode arch) (F.ParameterizedFormula (Sym t) arch)
             -> WL.Worklist (Some (Opcode arch (Operand arch)))
makeWorklist allOps knownFormulas = WL.fromList (S.toList opSet')
  where
    opSet = S.fromList [ Some op | Some (Witness op) <- allOps ]
    opSet' = F.foldl' removeIfPresent opSet (MapF.keys knownFormulas)
    removeIfPresent s (Some (RealOpcode sop)) = S.delete (Some sop) s
    removeIfPresent s (Some (PseudoOpcode _ _)) = s


{-

FIXME:

* Persist test cases for determinism?

-}
