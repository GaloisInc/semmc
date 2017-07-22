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
  IORelation(..),
  OperandRef(..),
  learn,
  readIORelation,
  printIORelation
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.State.Strict as St
import qualified Data.Foldable as F
import Data.Monoid
import Data.Proxy ( Proxy(..) )

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
import SemMC.Stochastic.IORelation.Implicit ( findImplicitOperands )
import SemMC.Stochastic.IORelation.Parser
import SemMC.Stochastic.IORelation.Types
import qualified SemMC.Stochastic.Remote as R

-- | Find the locations read from and written to by each instruction passed in
--
-- This is determined by observing the behavior of instructions on tests and
-- perturbing inputs randomly.
learn :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), R.MachineState (ArchState (Sym t) arch))
      => Learning t arch (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
learn = go MapF.empty
  where
    go m = do
      mop <- nextOpcode
      case mop of
        Nothing -> return m
        Just (Some (Witness op)) -> testOpcode m op >>= go

testOpcode :: forall arch sh t . (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
           => MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
           -> Opcode arch (Operand arch) sh
           -> Learning t arch (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
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


