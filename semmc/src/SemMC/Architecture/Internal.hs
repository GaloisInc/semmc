{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module is an internal implementation detail and is just here to break
-- an import cycle around a few definitions that were formerly in Architecture.
module SemMC.Architecture.Internal (
  Instruction,
  Operand,
  IsOperand,
  Opcode,
  IsOpcode,
  OperandType,
  IsOperandTypeRepr(..)
  ) where

import           Data.Kind
import           GHC.TypeLits ( Symbol )

import qualified Dismantle.Instruction as I
import           What4.BaseTypes

type Instruction arch = I.GenericInstruction (Opcode arch) (Operand arch)

-- | Type of operands for a given architecture.
type family Operand (arch :: Type) :: Symbol -> Type

-- | Class containing methods we want on operands. (Nothing for now.)
class IsOperand (o :: Symbol -> Type) where

-- | Type of opcodes for a given architecture.
type family Opcode (arch :: Type) = (r :: (Symbol -> Type) -> [Symbol] -> Type)

-- | Class containing methods we want on opcodes. (Nothing for now.)
class IsOpcode (op :: (Symbol -> Type) -> [Symbol] -> Type)

-- | Mapping from a particular instance of operand (characterized by a symbol)
-- to a Crucible type that is the type of expression an occurrence of said
-- operand should generate.
type family OperandType (arch :: Type) (op :: Symbol) :: BaseType

-- | The shape representative for the given architecture
class IsOperandTypeRepr arch where
  type OperandTypeRepr (arch :: Type) :: Symbol -> Type
  operandTypeReprSymbol :: proxy arch -> OperandTypeRepr arch s -> String

