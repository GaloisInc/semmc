{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module SemMC.Stochastic.Monad.Load (
  Config(..),
  SynEnv(..),
  loadInitialState
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import           Control.Monad ( replicateM )
import qualified Data.Constraint as C
import qualified Data.Foldable as F
import qualified Data.Functor.Identity as I
import qualified Data.Set as S
import           System.FilePath ( (</>), (<.>) )

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Seq as SeqF
import qualified Data.Parameterized.ShapedList as SL
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )

import qualified SemMC.Architecture as A
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Formula as F
import qualified SemMC.Log as L
import           SemMC.Stochastic.IORelation ( IORelation )
import qualified SemMC.Stochastic.IORelation.Types as I
import           SemMC.Stochastic.Monad.Constraints ( SynC )
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Stochastic.Statistics as S
import           SemMC.Symbolic ( Sym )
import qualified SemMC.Util as U
import qualified SemMC.Worklist as WL

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
         , remoteRunnerOutputChannel :: C.Chan CE.LogMessage
         -- ^ The channel to send log messages to from the remote runner
         , remoteRunnerTimeoutSeconds :: Int
         -- ^ The number of seconds to wait for a response from the remote
         -- runner after sending a batch of tests
         , opcodeTimeoutSeconds :: Int
         -- ^ The number of seconds to spend trying to find a candidate program
         -- that explains a target instruction before giving up.
         , testRunner :: I.TestRunner arch
         -- ^ See the related @lcTestRunner@ for usage examples.
         , logConfig :: L.LogCfg
         -- ^ A configuration for the general logging facility
         }

-- | Synthesis environment.
data SynEnv t arch =
  SynEnv { seFormulas :: STM.TVar (MapF.MapF ((A.Opcode arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch))
         -- ^ All of the known formulas (base set + learned set) for real opcodes
         , sePseudoFormulas :: MapF.MapF ((P.Pseudo arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch)
         -- ^ Formulas for all pseudo opcodes
         , seKnownCongruentOps :: STM.TVar (MapF.MapF SL.ShapeRepr (SeqF.SeqF (P.SynthOpcode arch)))
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

loadInitialState :: forall arch t
                  . (SynC arch, L.HasLogCfg)
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
                 -> [Some (Witness (F.BuildOperandList arch) ((P.Pseudo arch) (A.Operand arch)))]
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
  let congruentOps' = MapF.foldrWithKey (addCongruentOp . P.RealOpcode) MapF.empty initialFormulas
      congruentOps = MapF.foldrWithKey (addCongruentOp . P.PseudoOpcode) congruentOps' pseudoSet
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

dropKeyWitnesses :: forall c a v . (MapF.OrdF a) => MapF.MapF (Witness c a) v -> MapF.MapF a v
dropKeyWitnesses = I.runIdentity . U.mapFMapBothM f
  where
    f :: Witness c a sh -> v sh -> I.Identity (a sh, v sh)
    f (Witness a) v = return (a, v)

addCongruentOp :: (HR.HasRepr a SL.ShapeRepr)
               => a sh
               -> v
               -> MapF.MapF SL.ShapeRepr (SeqF.SeqF a)
               -> MapF.MapF SL.ShapeRepr (SeqF.SeqF a)
addCongruentOp op _ = MapF.insertWith (SeqF.><) (HR.typeRepr op) (SeqF.singleton op)
