{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture where

import           Data.EnumF
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           GHC.TypeLits ( KnownSymbol, Symbol )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval

import qualified Dismantle.Instruction as I

type Instruction arch = I.GenericInstruction (Opcode arch) (Operand arch)

-- | Type of operands for a given architecture.
type family Operand (arch :: *) :: Symbol -> *

type family IsReg (arch :: *) (s :: Symbol) :: Bool

-- | Class containing methods we want on operands. (Nothing for now.)
class IsOperand (o :: Symbol -> *) where

class IsSpecificOperand o (s :: Symbol) where
  allOperandValues :: [o s]

-- | Type of opcodes for a given architecture.
type family Opcode (arch :: *) = (r :: (Symbol -> *) -> [Symbol] -> *)

-- | Class containing methods we want on opcodes. (Nothing for now.)
class IsOpcode (op :: (Symbol -> *) -> [Symbol] -> *)

-- | Mapping from a particular instance of operand (characterized by a symbol)
-- to a Crucible type that is the type of expression an occurrence of said
-- operand should generate.
type family OperandType (arch :: *) (op :: Symbol) :: BaseType

-- XXX: Does this really belong in Architecture?
newtype BoundVar (sym :: *) (arch :: *) (op :: Symbol) =
  BoundVar { unBoundVar :: S.BoundVar sym (OperandType arch op) }
deriving instance (Eq (S.BoundVar sym (OperandType arch op))) => Eq (BoundVar sym arch op)
deriving instance (Ord (S.BoundVar sym (OperandType arch op))) => Ord (BoundVar sym arch op)

instance (ShowF (S.BoundVar sym)) => Show (BoundVar sym arch op) where
  show (BoundVar var) = showF var

instance (ShowF (S.BoundVar sym)) => ShowF (BoundVar sym arch)

-- | Represents the different registers, flags, and (eventually) memory a given
-- architecture has.
type family Location (arch :: *) :: BaseType -> *

-- | Methods we want on state variables.
class (OrdF a, TestEquality a, ShowF a) => IsLocation a where
  -- | Try parsing a human representation of a state variable like "r8" into its
  -- representation (and its type).
  readLocation :: String -> Maybe (Some a)
  -- | Given a state variable, return a representation of its type matching its
  -- parameter.
  locationType :: a tp -> BaseTypeRepr tp
  -- | Default value for this location. Typically something like 0.
  defaultLocationExpr :: (S.IsExprBuilder sym) => sym -> a tp -> IO (S.SymExpr sym tp)

type ArchState sym arch = MapF.MapF (Location arch) (S.SymExpr sym)

-- | An architecture is the top-level interface for specifying a semantics
-- implementation. It has specific operands, opcodes, and state variables.
class (IsOperand (Operand arch),
       IsOpcode (Opcode arch),
       IsLocation (Location arch),
       OrdF (Opcode arch (Operand arch)),
       ShowF (Opcode arch (Operand arch)),
       EnumF (Opcode arch (Operand arch)))
      => Architecture arch where
  -- | Map an operand to a Crucible expression, given a mapping from each state
  -- variable to a Crucible variable.
  operandValue :: forall proxy sym s.
                  (S.IsSymInterface sym,
                   S.IsExprBuilder sym)
               => proxy arch
               -> sym
               -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
               -> Operand arch s
               -> IO (S.SymExpr sym (OperandType arch s))

  -- | Map an operand to a specific state variable, if possible.
  operandToLocation :: forall proxy s.
                       proxy arch
                    -> Operand arch s
                    -> Maybe (Location arch (OperandType arch s))

  -- | Recover an operand value from a "ground value" of the same type. Unclear
  -- how we should handle registers here; for now, just do undefined...
  valueToOperand :: forall proxy s.
                    (KnownSymbol s)
                 => proxy arch
                 -> GroundValue (OperandType arch s)
                 -> Operand arch s
