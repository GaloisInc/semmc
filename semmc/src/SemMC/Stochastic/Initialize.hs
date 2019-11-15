{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Stochastic.Initialize (
  Config(..),
  SynEnv(..),
  withInitialState,
  mkFormulaFilename
  ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad
import qualified Data.Foldable as F
import qualified Data.List as DL
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import           System.FilePath ( (</>), (<.>) )

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as N
import qualified Data.Parameterized.Seq as SeqF
import           Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Arbitrary as DA

import qualified Lang.Crucible.Backend.Online as CBO
import qualified What4.Protocol.Online as WPO

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.View as V
import qualified SemMC.Formula as F
import qualified SemMC.Log as L
import           SemMC.Stochastic.IORelation ( IORelation )
import qualified SemMC.Stochastic.IORelation.Types as I
import           SemMC.Stochastic.Constraints ( SynC )
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Stochastic.Statistics as S
import           SemMC.Symbolic ( Sym )
import qualified SemMC.Worklist as WL

data Config arch =
  Config { baseSetDir :: FilePath
         , pseudoSetDir :: FilePath
         , learnedSetDir :: FilePath
         , programCountThreshold :: Int
         -- ^ Minimum number of equivalent programs to require
         -- before stopping the stochastic search
         , randomTestCount :: Int
         -- ^ The number of random tests to generate
         , parallelOpcodes :: Int
         -- ^ The number opcodes to process in parallel.  More is usually
         -- better, but there is locking around the symbolic backend (i.e.,
         -- 'SimpleBuilder'), so there could be contention if there is too much
         -- parallelism.
         , parallelSynth :: Int
         -- ^ The number of threads to run in parallel to try to synthesize
         -- candidate programs for *each* opcode.
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
         , statsThread :: S.StatisticsThread arch
         -- ^ A thread that persists statistics collected during synthesis
         }

-- | Synthesis environment.
data SynEnv t solver fs arch =
  SynEnv { seFormulas :: STM.TVar (MapF.MapF ((A.Opcode arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t solver fs) arch))
         -- ^ All of the known formulas (base set + learned set) for real opcodes
         , sePseudoFormulas :: MapF.MapF ((P.Pseudo arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t solver fs) arch)
         -- ^ Formulas for all pseudo opcodes
         , seKnownCongruentOps :: STM.TVar (MapF.MapF (A.ShapeRepr arch) (SeqF.SeqF (P.SynthOpcode arch)))
         -- ^ All opcodes with known formulas with operands of a given shape
         , seWorklist :: STM.TVar (WL.Worklist (Some (A.Opcode arch (A.Operand arch))))
         -- ^ Work items
         , seTestCases :: STM.TVar [V.ConcreteState arch]
         -- ^ All of the test cases we have accumulated.  This includes a set of
         -- initial heuristically interesting tests, as well as a set of ~1000
         -- random tests.  It also includes counterexamples learned during
         -- classification.
         , seIORelations :: MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch)
         -- ^ Descriptions of implicit and explicit operands for each opcode
         , seSymBackend :: STM.TMVar (Sym t solver fs)
         -- ^ The solver backend from Crucible.  This is behind a TMVar because
         -- it isn't thread safe - we have to serialize access.  We can't
         -- allocate one per thread, otherwise we won't be able to compare
         -- formulas (they will have different @t@ parameters).
         , seConfig :: Config arch
         -- ^ The initial configuration
         }

-- | Construct an initial state ('SynEnv') and pass it to the given
-- continuation.
--
-- This is a "with" function, vs just returning the the constructed
-- 'SynEnv', because of the existential @t@ param in @Sym t solver fs@.
withInitialState :: forall arch a
                  . (SynC arch, L.HasLogCfg, L.HasCallStack,
                    HR.HasRepr (A.Opcode arch (A.Operand arch)) (A.ShapeRepr arch),
                    HR.HasRepr (P.Pseudo arch (A.Operand arch)) (A.ShapeRepr arch))
                 => Config arch
                 -> [Some ((A.Opcode arch) (A.Operand arch))]
                 -- ^ All possible opcodes. These are used to guess
                 -- the names of opcode semantics files to attempt to
                 -- read from disk.
                 -> [Some ((P.Pseudo arch) (A.Operand arch))]
                 -- ^ All pseudo opcodes
                 -> [Some ((A.Opcode arch) (A.Operand arch))]
                 -- ^ The opcodes we want to learn formulas for (could
                 -- be all, but could omit instructions e.g., jumps)
                 -> MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch)
                 -- ^ IORelations
                 -> (forall t solver fs . (WPO.OnlineSolver t solver) => SynEnv t solver fs arch -> IO a)
                 -- ^ Computation to pass run with access to the 'SynEnv'
                 -> IO a
withInitialState cfg allOpcodes pseudoOpcodes targetOpcodes iorels k = do
  Some ng <- N.newIONonceGenerator
  CBO.withYicesOnlineBackend CBO.FloatRealRepr ng CBO.NoUnsatFeatures $ \sym -> do

    rng <- DA.createGen
    let genTest = AC.randomState (Proxy @arch) rng

    let interestingTests = AC.heuristicallyInterestingStates (Proxy @arch)

    synEnv <- loadInitialStateExplicit cfg sym genTest interestingTests
      allOpcodes pseudoOpcodes targetOpcodes iorels
    k synEnv

-- | A version of 'withInitialState' that returns the constructed
-- state and makes several implicit parameters explicit.
--
-- Prefer 'withInitialState' when possible.
--
-- In 'withInitialState', the implicit parameters come from class
-- instances. Here we make them explicit so they can be
-- overridden. Also, in 'withInitialState' the symbolic backend
-- ('Sym') is constructed locally, which introduces an existentially
-- bound param which is explicit here.
loadInitialStateExplicit :: forall arch t solver fs
                  . (SynC arch, L.HasLogCfg, L.HasCallStack,
                     WPO.OnlineSolver t solver,
                    HR.HasRepr (A.Opcode arch (A.Operand arch)) (A.ShapeRepr arch),
                    HR.HasRepr (P.Pseudo arch (A.Operand arch)) (A.ShapeRepr arch))
                 => Config arch
                 -> Sym t solver fs
                 -> IO (V.ConcreteState arch)
                 -- ^ A generator of random test cases
                 -> [V.ConcreteState arch]
                 -- ^ Heuristically-interesting test cases
                 -> [Some ((A.Opcode arch) (A.Operand arch))]
                 -- ^ All possible opcodes. These are used to guess
                 -- the names of opcode semantics files to attempt to
                 -- read from disk.
                 -> [Some ((P.Pseudo arch) (A.Operand arch))]
                 -- ^ All pseudo opcodes
                 -> [Some ((A.Opcode arch) (A.Operand arch))]
                 -- ^ The opcodes we want to learn formulas for (could
                 -- be all, but could omit instructions e.g., jumps)
                 -> MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch)
                 -- ^ IORelations
                 -> IO (SynEnv t solver fs arch)
loadInitialStateExplicit cfg sym genTest interestingTests allOpcodes pseudoOpcodes targetOpcodes iorels = do
  env <- F.formulaEnv (Proxy @arch) sym
  let load dir = F.loadFormulasFromFiles sym env F.emptyLibrary (mkFormulaFilename dir) allOpcodes
  baseSet <- load (baseSetDir cfg)
  L.logIO L.Info "Finished loading the base set"
  learnedSet <- load (learnedSetDir cfg)
  L.logIO L.Info "Finished loading learned set"
  let initialFormulas = MapF.union baseSet learnedSet
  pseudoSet <- F.loadFormulasFromFiles sym env F.emptyLibrary (mkFormulaFilename (pseudoSetDir cfg)) pseudoOpcodes
  L.logIO L.Info "Finished loading pseudo ops"
  let congruentOps' = MapF.foldrWithKey (addCongruentOp (Proxy @arch). P.RealOpcode) MapF.empty initialFormulas
      congruentOps = MapF.foldrWithKey (addCongruentOp (Proxy @arch) . P.PseudoOpcode) congruentOps' pseudoSet
  fref <- STM.newTVarIO initialFormulas
  congruentRef <- STM.newTVarIO congruentOps
  let worklist = makeWorklist targetOpcodes initialFormulas
  when (WL.null worklist) $ L.logIO L.Warn "Empty worklist!"
  wlref <- STM.newTVarIO worklist
  randomTests <- replicateM (randomTestCount cfg) genTest
  testref <- STM.newTVarIO (interestingTests ++ randomTests)
  symVar <- STM.newTMVarIO sym
  return SynEnv { seFormulas = fref
                , sePseudoFormulas = pseudoSet
                , seKnownCongruentOps = congruentRef
                , seTestCases = testref
                , seWorklist = wlref
                , seIORelations = iorels
                , seSymBackend = symVar
                , seConfig = cfg
                }

-- | Build the formula filename for a given opcode (given its base directory)
mkFormulaFilename :: (P.ShowF a)
                  => FilePath
                  -- ^ Directory to store the file
                  -> a sh
                  -- ^ The opcode to store a formula for
                  -> FilePath
mkFormulaFilename dir oc =
  -- NOTE: There is a name clash between ARM's TSTri and Thumb's TSTRi opcodes on
  -- case-insensitive file systems (Mac in particular). Therefore, we are using a
  -- slightly different filename for the Thumb store instruction to circumvent this
  -- problem. It is an ugly hack.
  let showOC = P.showF oc
      ocName = if "TSTR" `DL.isPrefixOf` showOC
               then "TSTOR" ++ drop 4 showOC
               else showOC
  in dir </> ocName <.> "sem"

-- | The worklist consists of all of the opcodes for which we do not already
-- have a formula (and that we actually want to learn)
makeWorklist :: (MapF.OrdF (A.Opcode arch (A.Operand arch)))
             => [Some ((A.Opcode arch) (A.Operand arch))]
             -> MapF.MapF ((A.Opcode arch) (A.Operand arch)) (F.ParameterizedFormula (Sym t solver fs) arch)
             -> WL.Worklist (Some (A.Opcode arch (A.Operand arch)))
makeWorklist allOps knownFormulas = WL.fromList (S.toList opSet')
  where
    opSet = S.fromList allOps
    opSet' = F.foldl' removeIfPresent opSet opSet
    removeIfPresent s sop@(Some op) =
      case MapF.lookup op knownFormulas of
        Nothing -> s
        Just _ -> S.delete sop s

addCongruentOp :: (HR.HasRepr a rep,
                   MapF.OrdF rep)
               => proxy arch
               -> a sh
               -> v
               -> MapF.MapF rep (SeqF.SeqF a)
               -> MapF.MapF rep (SeqF.SeqF a)
addCongruentOp _ op _ = MapF.insertWith (SeqF.><) (HR.typeRepr op) (SeqF.singleton op)
