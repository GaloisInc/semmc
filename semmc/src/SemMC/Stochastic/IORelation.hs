{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | A module for learning the input and output relations for instructions
module SemMC.Stochastic.IORelation (
  LearnConfig(..),
  IORelation(..),
  OperandRef(..),
  learn,
  readIORelation,
  printIORelation
  ) where

import qualified GHC.Err.Located as L

import Control.Monad ( replicateM )
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import Data.Monoid
import Data.Proxy ( Proxy(..) )

import qualified Data.Set.NonEmpty as NES
import qualified Data.Parameterized.Classes as P
import Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Map as MapF

import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture
import qualified SemMC.Formula.Parser as F
import SemMC.Util ( Witness(..) )

import SemMC.Stochastic.Monad ( Sym )
import SemMC.Stochastic.IORelation.Explicit ( generateExplicitInstruction,
                                              classifyExplicitOperands
                                            )
import SemMC.Stochastic.IORelation.Parser
import SemMC.Stochastic.IORelation.Shared
import SemMC.Stochastic.IORelation.Types
import qualified SemMC.Stochastic.Remote as R



-- | Find the locations read from and written to by each instruction passed in
--
-- This is determined by observing the behavior of instructions on tests and
-- perturbing inputs randomly.
learn :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
      => LearnConfig t arch
      -> [Some (Witness (F.BuildOperandList arch) (Opcode arch (Operand arch)))]
      -> IO (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
learn config ops = St.evalStateT (runM act) config
  where
    act = F.foldlM (\m (Some (Witness op)) -> testOpcode m op) MapF.empty ops

testOpcode :: forall arch sh t . (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
           => MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
           -> Opcode arch (Operand arch) sh
           -> M t arch (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
testOpcode m op = do
  implicitOperands <- findImplicitOperands op
  insn <- generateExplicitInstruction (Proxy :: Proxy arch) op (implicitLocations implicitOperands)
  case insn of
    D.Instruction op' operandList
      | Just P.Refl <- P.testEquality op op' -> do
        explicitOperands <- classifyExplicitOperands op operandList
        let ioRelation = implicitOperands <> explicitOperands
        return $ MapF.insert op ioRelation m
      | otherwise -> L.error ("randomInstruction returned an instruction with the wrong opcode: " ++ P.showF op')

-- | Collect all of the locations that are read from or written to implicitly
implicitLocations :: IORelation arch sh -> [Some (Location arch)]
implicitLocations ior = foldr collectImplicits (foldr collectImplicits [] (inputs ior)) (outputs ior)
  where
    collectImplicits opRef acc =
      case opRef of
        ImplicitOperand sloc -> sloc : acc
        OperandRef {} -> acc

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



{-

We want to generate tests to determine, for each register operand, if it is
input, output, or both.

We'll start off with a single initial register state passed to
generateTestVariants.  All of the variants will be derived from that state.

We need to walk down the operand list and, for each register operand (r0),
generate a set of new states with that register (r0) value tweaked.  For those
nonces, if other registers change in the post state, r0 was an input register.
Registers that change values in the post state are outputs.  If registers that
are not mentioned in the operand list change, they are implicit outputs.  If
changing a register not in the operand list causes a change in outputs, it is an
implicit input.


-}


