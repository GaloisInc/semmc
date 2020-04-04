{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Stochastic.Pseudo
  ( synthArbitraryOperands
  , synthInsnToActual
  , actualInsnToSynth
  ) where

import           Data.Kind
import           Data.Monoid ( (<>) )
import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.SymbolRepr as SR
import qualified Data.Parameterized.List as SL
import           Data.Proxy ( Proxy(..) )
import           GHC.TypeLits ( Symbol )

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Pseudo as AP


----------------------------------------------------------------

-- | Generate random operands for the given 'SynthOpcode'.
synthArbitraryOperands :: (D.ArbitraryOperands (A.Opcode arch) (A.Operand arch),
                           D.ArbitraryOperands (AP.Pseudo arch) (A.Operand arch))
                       => DA.Gen
                       -> AP.SynthOpcode arch sh
                       -> IO (SL.List (A.Operand arch) sh)
synthArbitraryOperands gen (AP.RealOpcode opcode) = D.arbitraryOperands gen opcode
synthArbitraryOperands gen (AP.PseudoOpcode opcode) = D.arbitraryOperands gen opcode

-- | Convert a 'SynthInstruction' into a list of 'Instruction's, either by
-- pulling out the real opcode, or by assembling the pseudo-opcode into real
-- instructions.
synthInsnToActual :: forall arch . (AP.ArchitectureWithPseudo arch) => AP.SynthInstruction arch -> [A.Instruction arch]
synthInsnToActual (AP.SynthInstruction opcode operands) =
  case opcode of
    AP.RealOpcode opcode' -> [D.Instruction opcode' operands]
    AP.PseudoOpcode opcode' -> AP.assemblePseudo (Proxy @arch) opcode' operands

-- | Convert a machine-level 'Instruction' into a 'SynthInstruction'.
actualInsnToSynth :: A.Instruction arch -> AP.SynthInstruction arch
actualInsnToSynth (D.Instruction opcode operands) = AP.SynthInstruction (AP.RealOpcode opcode) operands
