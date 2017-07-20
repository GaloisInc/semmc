{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.IORelation.Implicit (
  findImplicitOperands
  ) where

import Control.Monad ( replicateM )
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F

import qualified Data.Set.NonEmpty as NES
import Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture

import SemMC.Stochastic.Monad ( Sym )
import SemMC.Stochastic.IORelation.Shared
import SemMC.Stochastic.IORelation.Types
import qualified SemMC.Stochastic.Remote as R

-- | Sweep through the parameter space to find locations not mentioned in
-- parameter lists that are modified by the instruction.
--
-- To do this, we generate a bunch of randomized operand lists to cycle through
-- possible registers.
--
-- For this, we will want to focus on generating values that trigger edge cases
-- to make sure we can deal with flags registers.
findImplicitOperands :: forall t arch sh
                      . (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), R.MachineState (ArchState (Sym t) arch))
                     => Opcode arch (Operand arch) sh
                     -> M t arch (IORelation arch sh)
findImplicitOperands op = do
  mkTest <- St.gets testGen
  g <- St.gets gen
  tests <- concat <$> replicateM 20 (genTestSet mkTest g)
  withTestResults op tests $ computeImplicitOperands op tests
  where
    genTestSet mkTest g = do
      t0 <- liftIO mkTest
      insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
      tb0 <- generateImplicitTests insn t0
      mapM (wrapTestBundle insn) tb0

-- | Interpret the results of the implicit operand search
--
-- The fact records all of the explicit locations.  We just need to look at all
-- of the *other* (unmentioned) locations to find any changes.  Any changed
-- locations are implicit outputs.
--
-- FIXME: To find implicit inputs, we have to keep the explicit operands
-- constant and vary all of the other locations.
computeImplicitOperands :: (Architecture arch)
                        => Opcode arch (Operand arch) sh
                        -> [TestBundle (R.TestCase (ArchState (Sym t) arch)) (ImplicitFact arch)]
                        -> [R.ResultOrError (ArchState (Sym t) arch)]
                        -> M t arch (IORelation arch sh)
computeImplicitOperands op tests results = undefined idx
  where
    idx = F.foldl' indexResults emptyResultIndex results

-- | For an instruction instance, sweep across the parameter space of all of the
-- interesting values for the operands.  Examine all of the locations that do
-- not appear in the register list for changes.
--
-- Repeat for a number of random instructions
generateImplicitTests :: forall arch t
                       . (Architecture arch)
                      => Instruction arch
                      -> ArchState (Sym t) arch
                      -> M t arch [TestBundle (ArchState (Sym t) arch) (ImplicitFact arch)]
generateImplicitTests = undefined
