{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | This is the main entry point to the stochastic synthesis (SS)
--
-- Client code should only need to import this module in order to run the
-- stochastic synthesis.
module SemMC.Stochastic.Strata (
  SynEnv,
  Config(..),
  withInitialState,
  stratifiedSynthesis,
  -- * Statistics
  S.StatisticsThread,
  S.newStatisticsThread,
  S.terminateStatisticsThread
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import           Text.Printf ( printf )

import           UnliftIO as U

import           Data.Parameterized.Classes ( showF )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Instruction as D

import qualified What4.Protocol.Online as WPO

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Formula as F
import qualified SemMC.Log as L
import           SemMC.Symbolic ( Sym )

import qualified SemMC.Stochastic.CandidateProgram as CP
import qualified SemMC.Stochastic.Classify as C
import           SemMC.Stochastic.Extract ( extractFormula )
import           SemMC.Stochastic.Generalize ( generalize )
import           SemMC.Stochastic.Instantiate ( instantiateInstruction )
import           SemMC.Stochastic.Initialize ( withInitialState, Config(..), SynEnv(..) )
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.Statistics as S
import           SemMC.Stochastic.Synthesize ( synthesize )

{-

Goal: Have a basic setup function to establish the environment (e.g., parse the
base set and build a worklist).  The caller should be able to pass that
environment to multiple threads running the strata function.  Key point: the
caller controls the number and placement of threads.

-}

-- | Run many threads of stratified synthesis (SS) concurrently.
--
-- The different synthesis threads are independent, except that they
-- all draw work (opcodes to synthesize semantics for) from the same
-- shared work list.
stratifiedSynthesis :: forall arch t solver fs
                     . (SynC arch, L.HasCallStack, L.HasLogCfg, WPO.OnlineSolver t solver)
                    => SynEnv t solver fs arch
                    -> IO (MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (Sym t solver fs) arch))
stratifiedSynthesis env0 = do
  A.replicateConcurrently_ (parallelOpcodes (seConfig env0)) $
    (L.namedIO "strata" $ runSynInNewLocalEnv env0 go)
  STM.readTVarIO (seFormulas env0)
  where
    go = processWorklist >> generalize

-- | Synthesize semantics for single opcodes until there are no more
-- left.
--
-- In practice, most runs of SS should timeout with a non-empty work
-- list, because some instructions can't be learned effectively
-- yet. This "timing out" is handled by the orchestration code (not in
-- this module) that calls 'stratifiedSynthesis' itself.
processWorklist :: (SynC arch, L.HasCallStack, WPO.OnlineSolver t solver)
                => Syn t solver fs arch ()
processWorklist = do
  mwork <- takeWork
  case mwork of
    Nothing -> L.logM L.Info "No more work items, exiting!"
    Just (Some op) -> do
      L.logM L.Info $ printf "Processing work item '%s'" (showF op)
      processOpcode op
      processWorklist

-- | Process a single opcode, requeueing it on the work list if
-- synthesis times out.
processOpcode :: (SynC arch, L.HasCallStack, WPO.OnlineSolver t solver)
              => A.Opcode arch (A.Operand arch) sh -> Syn t solver fs arch ()
processOpcode op = do
  rinstr <- instantiateInstruction op
  let instr = AC.riInstruction rinstr
  L.logM L.Info $ printf "Beginning MC search with instantiation '%s'" (show instr)
  -- Catch all exceptions in the stratification process.
  (res, strataTime) <- timeSyn (U.tryAny (strataOne op rinstr))
  case res of
    Left err -> do
      -- If we got an actual error, don't retry the opcode.  We'll log it for later analysis
      L.logM L.Error $ printf "Error while processing instruction '%s': %s" (show instr) (show err)
      -- And then die, since errors are bugs at this point.
      throwIO err
    -- Timeout, so we can't learn it yet.  Come back later
    Right Nothing -> do
      L.logM L.Info $ printf "Timeout while processing opcode '%s'" (show instr)
      withStats $ S.recordStrataTimeout (Some op)
      -- Would it make more sense to put this in a different queue
      -- that corresponds to opcodes that timed out in the current
      -- round, instead of requeuing?
      addWork (Some op)
    -- Success, record the formula
    Right (Just formula) -> do
      L.logM L.Info $ unlines
        [ printf "Learned a formula for '%s' in %s seconds" (show instr) (show strataTime)
        , printf "Parameterized formula = %s" (show formula) ]
      withStats $ S.recordStrataSuccess (Some op) strataTime
      recordLearnedFormula op formula


-- | Attempt to learn a formula for the given opcode
--
-- Return 'Nothing' if we timed out before learning any candidates.
strataOne :: (SynC arch, L.HasCallStack, WPO.OnlineSolver t solver)
          => A.Opcode arch (A.Operand arch) sh
          -> AC.RegisterizedInstruction arch
          -> Syn t solver fs arch (Maybe (F.ParameterizedFormula (Sym t solver fs) arch sh ))
strataOne op rinstr = strataOneLoop op rinstr (C.emptyEquivalenceClasses)

-- | Main loop of stratified synthesis for a single instruction.
strataOneLoop :: (SynC arch, L.HasCallStack, WPO.OnlineSolver t solver)
              => A.Opcode arch (A.Operand arch) sh
              -> AC.RegisterizedInstruction arch
              -> C.EquivalenceClasses (CP.CandidateProgram t solver fs arch)
              -> Syn t solver fs arch (Maybe (F.ParameterizedFormula (Sym t solver fs) arch sh))
strataOneLoop op rinstr eqclasses = do
  let instr = AC.riInstruction rinstr
  cfg <- askConfig
  (mprog, synDuration) <- withTimeout (L.namedM "synthesize" $ synthesize rinstr)
  case mprog of
    Nothing -> do
      L.logM L.Info $ printf "Synthesis of a single candidate for '%s' timed out." (show instr)
      -- If we already learned at least one candidate then we assume
      -- the timeout is bad luck and keep going. On the other hand, if
      -- the first attempt times out, then we give up, assuming that
      -- we'll always fail.
      --
      -- An alternative approach would be to keep trying in any
      -- case. That would allow us to learn more formulas in the
      -- current round if we get unlucky the first time, but otherwise
      -- will waste resources as we continue in vain.
      if C.countPrograms eqclasses == 0
        then return Nothing
        else strataOneLoop op rinstr eqclasses
    Just prog -> do
      L.logM L.Info $ unlines
        [ printf "Synthesis found another candidate for '%s' in %s seconds."
          (show instr) (show synDuration)
        , printf "We've found %i candidates so far. The current candidate:"
          (C.countPrograms eqclasses)
        , show prog ]
      withStats $ S.recordSynthesizeSuccess (Some op) synDuration
      meqclasses' <- C.classify rinstr prog eqclasses
      case meqclasses' of
        Nothing -> do
          L.logM L.Info "No equivalence classes left after counterexample"
          return Nothing
        Just eqclasses'
          | C.countPrograms eqclasses' > programCountThreshold cfg -> do
              L.logM L.Info "Found enough candidate programs, finishing with formula extraction"
              finishStrataOne op rinstr eqclasses'
          | otherwise -> do
              L.logM L.Info $ printf "Currently have %d candidate programs, need %d"
                (C.countPrograms eqclasses') (programCountThreshold cfg)
              strataOneLoop op rinstr eqclasses'

-- | Choose the best candidate and return its formula.
--
-- Returns 'Nothing' if there are no candidates in the eqclasses.
finishStrataOne :: (SynC arch, L.HasCallStack)
                => A.Opcode arch (A.Operand arch) sh
                -> AC.RegisterizedInstruction arch
                -> C.EquivalenceClasses (CP.CandidateProgram t solver fs arch)
                -> Syn t solver fs arch (Maybe (F.ParameterizedFormula (Sym t solver fs) arch sh))
finishStrataOne op instr eqclasses = do
  L.logM L.Info ("Equivalence classes: " ++ show eqclasses)
  mBestClass <- C.chooseClass eqclasses
  case mBestClass of
    Nothing -> return Nothing
    Just bestClass -> do
      prog <- C.chooseProgram bestClass
      Just <$> buildFormula op instr (CP.cpFormula prog)

-- | Construct a formula for the given instruction based on the selected representative program.
--
-- We pass in the opcode because we need the shape of the opcode in the type signature.
buildFormula :: (SynC arch)
             => A.Opcode arch (A.Operand arch) sh
             -> AC.RegisterizedInstruction arch
             -> F.Formula (Sym t solver fs) arch
             -> Syn t solver fs arch (F.ParameterizedFormula (Sym t solver fs) arch sh)
buildFormula o i progFormula =
  opcodeIORelation o >>= \case
    Just iorel ->
        case AC.riInstruction i of
          D.Instruction opcode operands
              | Just MapF.Refl <- MapF.testEquality opcode o ->
                -- Now, for all of the outputs (implicit and explicit) in the target
                -- instruction, look up the corresponding formula in `progFormula`
                                extractFormula i opcode operands progFormula iorel
              | otherwise -> L.error ("Unexpected opcode mismatch: " ++ MapF.showF o)
    Nothing -> L.error ("Unable to get opcode IORelation in SemMC Strata buildFormula")
