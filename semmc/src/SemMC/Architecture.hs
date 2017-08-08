{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
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
import           Data.Typeable ( Typeable )
import           GHC.TypeLits ( Symbol )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.Instruction as I

type Instruction arch = I.GenericInstruction (Opcode arch) (Operand arch)

-- | Type of operands for a given architecture.
type family Operand (arch :: *) :: Symbol -> *

-- | Class containing methods we want on operands. (Nothing for now.)
class IsOperand (o :: Symbol -> *) where

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

type ArchState arch ex = MapF.MapF (Location arch) ex

-- | An architecture is the top-level interface for specifying a semantics
-- implementation. It has specific operands, opcodes, and state variables.
class (IsOperand (Operand arch),
       IsOpcode (Opcode arch),
       IsLocation (Location arch),
       Show (Instruction arch),
       ShowF (Operand arch),
       Typeable arch,
       OrdF (Opcode arch (Operand arch)),
       ShowF (Opcode arch (Operand arch)),
       EnumF (Opcode arch (Operand arch)))
      => Architecture arch where
  -- | Tagged expression type for this architecture.
  --
  -- This is a bit of a hack to add extra metadata needed for the templating stuff.
  data TaggedExpr arch sym :: Symbol -> *

  -- | Untag a tagged expression.
  unTagged :: TaggedExpr arch sym s -> S.SymExpr sym (OperandType arch s)

  -- | Map an operand to a Crucible expression, given a mapping from each state
  -- variable to a Crucible variable.
  operandValue :: forall proxy sym s.
                  (S.IsSymInterface sym,
                   S.IsExprBuilder sym)
               => proxy arch
               -> sym
               -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
               -> Operand arch s
               -> IO (TaggedExpr arch sym s)

  -- | Map an operand to a specific state variable, if possible.
  operandToLocation :: forall proxy s.
                       proxy arch
                    -> Operand arch s
                    -> Maybe (Location arch (OperandType arch s))
