{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.Strata (
  Config(..),
  strata
  ) where

import Control.Monad.Trans ( liftIO )
import qualified Data.Set as S

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )

import qualified Lang.Crucible.Solver.Interface as CRU

import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( Architecture, Instruction, Opcode, Operand )
import qualified SemMC.Formula as F

import qualified SemMC.Stochastic.Classify as C
import SemMC.Stochastic.Generalize ( generalize )
import SemMC.Stochastic.Monad
import SemMC.Stochastic.Synthesize ( synthesize )

{-

Goal: Have a basic setup function to establish the environment (e.g., parse the
base set and build a worklist).  The caller should be able to pass that
environment to multiple threads running the strata function.  Key point: the
caller controls the number and placement of threads.

-}

strata :: (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), Ord (Instruction arch))
       => Syn sym arch (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch))
strata = processWorklist >> generalize

processWorklist :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), Ord (Instruction arch))
                => Syn sym arch ()
processWorklist = do
  mwork <- takeWork
  case mwork of
    Nothing -> return ()
    Just (Some so) -> do
      res <- strataOne so
      case res of
        -- Timeout, so we can't learn it yet.  Come back later
        Nothing -> addWork so
        -- Success, record the formula
        Just formula -> recordLearnedFormula so formula
      processWorklist

-- | Attempt to learn a formula for the given opcode
--
-- Return 'Nothing' if we time out trying to find a formula
strataOne :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), Ord (Instruction arch))
          => Opcode arch (Operand arch) sh
          -> Syn sym arch (Maybe (F.ParameterizedFormula sym arch sh))
strataOne op = do
  instr <- instantiateInstruction op
  mprog <- synthesize instr
  case mprog of
    Nothing -> return Nothing
    Just prog -> strataOneLoop op instr (C.equivalenceClasses prog)

strataOneLoop :: (Ord (Instruction arch))
              => Opcode arch (Operand arch) sh
              -> Instruction arch
              -> C.EquivalenceClasses arch
              -> Syn sym arch (Maybe (F.ParameterizedFormula sym arch sh))
strataOneLoop op instr eqclasses = do
  cfg <- askConfig
  mprog <- synthesize instr
  case mprog of
    Nothing -> do
      -- We hit a timeout, so just try to build a formula based on what we have
      Just <$> finishStrataOne op instr eqclasses
    Just prog -> do
      meqclasses' <- C.classify prog eqclasses
      case meqclasses' of
        Nothing -> return Nothing
        Just eqclasses'
          | C.countPrograms eqclasses' > programCountThreshold cfg ->
            Just <$> finishStrataOne op instr eqclasses'
          | otherwise -> strataOneLoop op instr eqclasses'

finishStrataOne :: Opcode arch (Operand arch) sh
                -> Instruction arch
                -> C.EquivalenceClasses arch
                -> Syn sym arch (F.ParameterizedFormula sym arch sh)
finishStrataOne op instr eqclasses = do
  bestClass <- C.chooseClass eqclasses
  prog <- C.chooseProgram bestClass
  buildFormula op instr prog

-- | Construct a formula for the given instruction based on the selected representative program.
--
-- We pass in the opcode because we need the shape of the opcode in the type signature.
buildFormula :: Opcode arch (Operand arch) sh
             -> Instruction arch
             -> [Instruction arch]
             -> Syn sym arch (F.ParameterizedFormula sym arch sh)
buildFormula = undefined

instantiateInstruction :: (D.ArbitraryOperands (Opcode arch) (Operand arch))
                       => Opcode arch (Operand arch) sh
                       -> Syn sym arch (Instruction arch)
instantiateInstruction op = do
  gen <- askGen
  -- Note: randomInstruction can only return Nothing if the set it is given is
  -- empty.  We should probably change it to accept a non-empty list.
  Just target <- liftIO $ D.randomInstruction gen (S.singleton (Some op))
  return target
