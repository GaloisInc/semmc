{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | This is the main entry point to the stochastic synthesis
--
-- Client code should only need to import this module in order to run the
-- stochastic synthesis.
module SemMC.Stochastic.Strata (
  SynEnv,
  Config(..),
  loadInitialState,
  stratifiedSynthesis,
  -- * Statistics
  S.StatisticsThread,
  S.newStatisticsThread,
  S.terminateStatisticsThread,
  -- * Classes
  BuildAndConvert
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent as C
import qualified Control.Exception as C
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import           Data.IORef ( newIORef )
import           Text.Printf ( printf )

import           Data.Parameterized.Classes ( showF )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.Instruction as D

import qualified SemMC.Architecture as A
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Formula as F
import qualified SemMC.Log as L
import           SemMC.Symbolic ( Sym )

import qualified SemMC.Stochastic.CandidateProgram as CP
import qualified SemMC.Stochastic.Classify as C
import           SemMC.Stochastic.Extract ( extractFormula )
import           SemMC.Stochastic.Generalize ( generalize )
import           SemMC.Stochastic.Instantiate ( instantiateInstruction )
import           SemMC.Stochastic.Initialize ( loadInitialState, Config(..), SynEnv(..), BuildAndConvert )
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.Statistics as S
import           SemMC.Stochastic.Synthesize ( synthesize )

{-

Goal: Have a basic setup function to establish the environment (e.g., parse the
base set and build a worklist).  The caller should be able to pass that
environment to multiple threads running the strata function.  Key point: the
caller controls the number and placement of threads.

-}

stratifiedSynthesis :: forall arch t
                     . (SynC arch)
                    => SynEnv t arch
                    -> IO (MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch))
stratifiedSynthesis env0 = do
  A.replicateConcurrently_ (threadCount (seConfig env0)) $ do
    nonceRef <- newIORef 0
    gen <- DA.createGen
    tChan <- C.newChan
    rChan <- C.newChan
    testRunner' <- A.async $ testRunner (seConfig env0) tChan rChan (remoteRunnerOutputChannel (seConfig env0))
    A.link testRunner'
    let localEnv = LocalSynEnv { seGlobalEnv = env0
                               , seRandomGen = gen
                               , seNonceSource = nonceRef
                               , seTestChan = tChan
                               , seResChan = rChan
                               }
    runSyn localEnv strata `C.finally` A.cancel testRunner'
  STM.readTVarIO (seFormulas env0)

strata :: (SynC arch)
       => Syn t arch (MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (Sym t) arch))
strata = processWorklist >> generalize

processWorklist :: (SynC arch)
                => Syn t arch ()
processWorklist = do
  mwork <- takeWork
  case mwork of
    Nothing -> return ()
    Just (Some (Witness so)) -> do
      -- Catch all exceptions in the stratification process.
      (res, strataTime) <- timeSyn (tryEither "strataOne" (strataOne so))
      case res of
        Left err -> do
          -- If we got an actual error, don't retry the opcode.  We'll log it for later analysis
          L.logM L.Error $ printf "Error while processing opcode %s: %s" (showF so) (show err)
        -- Timeout, so we can't learn it yet.  Come back later
        Right Nothing -> do
          L.logM L.Info $ printf "Timeout while processing opcode %s" (showF so)
          withStats $ S.recordStrataTimeout (Some so)
          addWork (Some (Witness so))
        -- Success, record the formula
        Right (Just formula) -> do
          L.logM L.Info $ printf "Learned a formula for %s in %s seconds" (showF so) (show strataTime)
          withStats $ S.recordStrataSuccess (Some so) strataTime
          recordLearnedFormula so formula
      processWorklist

-- | Attempt to learn a formula for the given opcode
--
-- Return 'Nothing' if we time out trying to find a formula
strataOne :: (SynC arch)
          => A.Opcode arch (A.Operand arch) sh
          -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
strataOne op = do
  L.logM L.Info $ printf "Beginning stratification for opcode %s" (showF op)
  instr <- instantiateInstruction op
  (mprog, synDuration) <- withTimeout (synthesize instr)
  case mprog of
    Nothing -> return Nothing
    Just prog -> do
      L.logM L.Info $ printf "Synthesis success for %s in %s seconds" (showF op) (show synDuration)
      withStats $ S.recordSynthesizeSuccess (Some op) synDuration
      strataOneLoop op instr (C.equivalenceClasses prog)

strataOneLoop :: (SynC arch)
              => A.Opcode arch (A.Operand arch) sh
              -> CS.RegisterizedInstruction arch
              -> C.EquivalenceClasses (CP.CandidateProgram t arch)
              -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
strataOneLoop op instr eqclasses = do
  cfg <- askConfig
  (mprog, synDuration) <- withTimeout (synthesize instr)
  case mprog of
    Nothing -> do
      -- We hit a timeout, so just try to build a formula based on what we have
      Just <$> finishStrataOne op instr eqclasses
    Just prog -> do
      L.logM L.Info $ printf "Synthesis success for %s in %s seconds" (showF op) (show synDuration)
      withStats $ S.recordSynthesizeSuccess (Some op) synDuration
      meqclasses' <- C.classify instr prog eqclasses
      case meqclasses' of
        Nothing -> return Nothing
        Just eqclasses'
          | C.countPrograms eqclasses' > programCountThreshold cfg ->
            Just <$> finishStrataOne op instr eqclasses'
          | otherwise -> strataOneLoop op instr eqclasses'

finishStrataOne :: (SynC arch)
                => A.Opcode arch (A.Operand arch) sh
                -> CS.RegisterizedInstruction arch
                -> C.EquivalenceClasses (CP.CandidateProgram t arch)
                -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
finishStrataOne op instr eqclasses = do
  bestClass <- C.chooseClass eqclasses
  prog <- C.chooseProgram bestClass
  buildFormula op instr (CP.cpFormula prog)

-- | Construct a formula for the given instruction based on the selected representative program.
--
-- We pass in the opcode because we need the shape of the opcode in the type signature.
buildFormula :: (SynC arch)
             => A.Opcode arch (A.Operand arch) sh
             -> CS.RegisterizedInstruction arch
             -> F.Formula (Sym t) arch
             -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
buildFormula o i progFormula = do
  Just iorel <- opcodeIORelation o
  case CS.riInstruction i of
    D.Instruction opcode operands
      | Just MapF.Refl <- MapF.testEquality opcode o -> do
          -- Now, for all of the outputs (implicit and explicit) in the target
          -- instruction, look up the corresponding formula in `progFormula`
          extractFormula i opcode operands progFormula iorel
      | otherwise -> L.error ("Unexpected opcode mismatch: " ++ MapF.showF o)
