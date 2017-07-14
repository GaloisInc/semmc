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
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( Architecture, Instruction, Opcode, Operand, Location )
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

strata :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), Ord (Instruction arch))
       => Syn t arch (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
strata = processWorklist >> generalize

processWorklist :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), Ord (Instruction arch))
                => Syn t arch ()
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
          -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
strataOne op = do
  instr <- instantiateInstruction op undefined -- Add in the input relation
  mprog <- synthesize instr
  case mprog of
    Nothing -> return Nothing
    Just prog -> strataOneLoop op instr (C.equivalenceClasses prog)

strataOneLoop :: (Architecture arch, Ord (Instruction arch), SynC arch)
              => Opcode arch (Operand arch) sh
              -> Instruction arch
              -> C.EquivalenceClasses arch
              -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
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
                -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
finishStrataOne op instr eqclasses = do
  bestClass <- C.chooseClass eqclasses
  prog <- C.chooseProgram bestClass
  buildFormula op instr undefined prog

-- | Construct a formula for the given instruction based on the selected representative program.
--
-- We pass in the opcode because we need the shape of the opcode in the type signature.
--
-- Here we need the set of output locations (which could include implicit locations)
buildFormula :: Opcode arch (Operand arch) sh
             -> Instruction arch
             -> [Some (Location arch)]
             -- ^ Output locations for the opcode
             -> [Instruction arch]
             -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
buildFormula = undefined

-- | Generate an arbitrary instruction for the given opcode.
--
-- Note that this function should strive to avoid operands that are distinct
-- from any implicit operands.  It may also want to ensure that registers are
-- not reused.  To do that, we'll need to pass in the set of input locations
--
-- We'll also want to return a mapping from locations to input values to be used
-- to inform the initial state generation.
instantiateInstruction :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                       => Opcode arch (Operand arch) sh
                       -> [Some (Location arch)]
                       -- ^ Input locations for the opcode
                       -> Syn t arch (Instruction arch)
instantiateInstruction op _inputs = do
  gen <- askGen
  target <- liftIO $ D.randomInstruction gen (NES.singleton (Some op))
  return target

