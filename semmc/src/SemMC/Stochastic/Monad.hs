{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  Config(..),
  Test,
  -- * Operations
  askGen,
  askBaseSet,
  askConfig,
  askRunTest,
  askTestCases,
  askFormulas,
  askPseudoFormulas,
  lookupFormula,
  lookupCongruent,
  withSymBackend,
  addTestCase,
  recordLearnedFormula,
  instantiateFormula,
  takeWork,
  addWork,
  opcodeIORelation
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as C
import           Control.Monad ( replicateM )
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import           System.FilePath ( (</>), (<.>) )

import qualified Data.EnumF as P
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( mapSome, Some(..) )

import           Lang.Crucible.BaseTypes ( knownRepr )
import qualified Lang.Crucible.Solver.Interface as CRUI

import           Data.Parameterized.HasRepr ( HasRepr(..) )
import           Data.Parameterized.ShapedList ( ShapeRepr )
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction.Random as D
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Seq as SeqF
import           SemMC.Architecture ( Architecture, Opcode, Operand, Instruction )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Instantiate as F
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Formula.Load as F
import           SemMC.Symbolic ( Sym )
import           SemMC.Util ( makeSymbol )
import qualified SemMC.Worklist as WL

import           SemMC.ConcreteState ( ConcreteArchitecture, ConcreteState )
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
type Test arch = ConcreteState arch

-- | Synthesis environment.
data SynEnv t arch =
  SynEnv { seFormulas :: STM.TVar (MapF.MapF ((Opcode arch) (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
         -- ^ All of the known formulas (base set + learned set) for real opcodes
         , sePseudoFormulas :: MapF.MapF ((Pseudo arch) (Operand arch)) (F.ParameterizedFormula (Sym t) arch)
         -- ^ Formulas for all pseudo opcodes
         , seKnownCongruentOps :: STM.TVar (MapF.MapF ShapeRepr (SeqF.SeqF (SynthOpcode arch)))
         -- ^ All opcodes with known formulas with operands of a given shape
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
              , seRunTest :: ConcreteState arch -> [Instruction arch] -> Syn t arch (ConcreteState arch)
                -- ^ Starting with a synchronous test runner. Will
                -- worry about batching tests and running them in
                -- parallel later.
              }

-- | Synthesis constraints.
type SynC arch = ( P.OrdF (Opcode arch (Operand arch))
                 , P.OrdF (Operand arch)
                 , D.ArbitraryOperand (Operand arch)
                 , D.ArbitraryOperands (Opcode arch) (Operand arch)
                 , D.ArbitraryOperands (Pseudo arch) (Operand arch)
                 , P.EnumF (Opcode arch (Operand arch))
                 , HasRepr (Opcode arch (Operand arch)) ShapeRepr
                 , HasRepr (Pseudo arch (Operand arch)) ShapeRepr
                 , ConcreteArchitecture arch
                 , ArchitectureWithPseudo arch )

-- Synthesis monad.
newtype Syn t arch a = Syn { unSyn :: R.ReaderT (LocalSynEnv t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            R.MonadReader (LocalSynEnv t arch))

-- | Runner for 'Syn' monad.
--
-- The ty vars are explicitly quantified with @arch@ first so that we
-- can use @-XTypeApplications@ more conveniently.
runSyn :: forall arch t a. LocalSynEnv t arch -> Syn t arch a -> IO a
runSyn e a = R.runReaderT (unSyn a) e

-- | A version of 'C.tryJust' wrapped for our 'Syn' monad.
--
-- The @unliftio@ package generalizes this idea and provides a
-- @tryJust@ like this.
tryJust :: C.Exception e
        => (e -> Maybe b) -> Syn t arch a -> Syn t arch (Either b a)
tryJust pred action = do
  localEnv <- R.ask
  liftIO $ C.tryJust pred (runSyn localEnv action)

-- | Record a learned formula for the opcode in the state
recordLearnedFormula :: (P.OrdF (Opcode arch (Operand arch)),
                         HasRepr (Opcode arch (Operand arch)) ShapeRepr)
                     => Opcode arch (Operand arch) sh
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

askRunTest :: Syn t arch (ConcreteState arch -> [Instruction arch] -> Syn t arch (ConcreteState arch))
askRunTest = R.asks seRunTest

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

askFormulas :: Syn t arch (MapF.MapF ((Opcode arch) (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
askFormulas = R.asks (seFormulas . seGlobalEnv) >>= (liftIO . STM.readTVarIO)

askPseudoFormulas :: Syn t arch (MapF.MapF (Pseudo arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
askPseudoFormulas = R.asks (sePseudoFormulas . seGlobalEnv)

askKnownCongruentOps :: Syn t arch (MapF.MapF ShapeRepr (SeqF.SeqF (SynthOpcode arch)))
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
askBaseSet :: (P.OrdF (Opcode arch (Operand arch)),
               P.OrdF (Pseudo arch (Operand arch)))
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

lookupCongruent :: (HasRepr (Opcode arch (Operand arch)) ShapeRepr,
                    HasRepr (Pseudo arch (Operand arch)) ShapeRepr)
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
         , testRunner :: I.TestRunner arch
         -- ^ See the related @lcTestRunner@ for usage examples.
         }

loadInitialState :: forall arch t
                  . (SynC arch)
                 => Config arch
                 -> Sym t
                 -> IO (ConcreteState arch)
                 -- ^ A generator of random test cases
                 -> [Test arch]
                 -- ^ Heuristically-interesting test cases
                 -> [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
                 -- ^ All possible opcodes. These are used to guess
                 -- the names of opcode semantics files to attempt to
                 -- read from disk.
                 -> [Some (Witness (F.BuildOperandList arch) ((Pseudo arch) (Operand arch)))]
                 -- ^ All pseudo opcodes
                 -> [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
                 -- ^ The opcodes we want to learn formulas for (could
                 -- be all, but could omit instructions e.g., jumps)
                 -> MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
                 -- ^ IORelations
                 -> IO (SynEnv t arch)
loadInitialState cfg sym genTest interestingTests allOpcodes pseudoOpcodes targetOpcodes iorels = do
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
  pseudoSet <- F.loadFormulas sym (toFP (pseudoSetDir cfg)) env pseudoOpcodes
  let addCongruentOp op _ = MapF.insertWith (SeqF.><) (typeRepr op) (SeqF.singleton op)
      congruentOps' = MapF.foldrWithKey (addCongruentOp . RealOpcode) MapF.empty initialFormulas
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
makeWorklist :: (MapF.OrdF (Opcode arch (Operand arch)))
             => [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
             -> MapF.MapF ((Opcode arch) (Operand arch)) (F.ParameterizedFormula (Sym t) arch)
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
