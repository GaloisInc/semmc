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
-- {-# LANGUAGE TypeFamilyDependencies #-}

module SemMC.Architecture where

import Data.Type.Equality
import GHC.TypeLits ( Symbol )

import qualified Data.ShowF as SF
import Data.Parameterized.Classes
import Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.Solver.Interface as S
import Lang.Crucible.BaseTypes

import qualified Dismantle.Instruction as I

type Instruction arch = I.GenericInstruction (Opcode arch) (Operand arch)

type family Operand (arch :: *) :: Symbol -> *

class IsOperand a where

instance IsOperand a where

type family Opcode (arch :: *) :: (Symbol -> *) -> [Symbol] -> *

class IsOpcode a where

instance IsOpcode a where

type family OperandType (arch :: *) (op :: Symbol) :: BaseType

newtype BoundVar (sym :: *) (arch :: *) (op :: Symbol) =
  BoundVar { unBoundVar :: S.BoundVar sym (OperandType arch op) }
deriving instance (Show (S.BoundVar sym (OperandType arch op))) => Show (BoundVar sym arch op)
deriving instance (Eq (S.BoundVar sym (OperandType arch op))) => Eq (BoundVar sym arch op)
deriving instance (Ord (S.BoundVar sym (OperandType arch op))) => Ord (BoundVar sym arch op)

instance (ShowF (S.BoundVar sym)) => ShowF (BoundVar sym arch) where
  showF (BoundVar bv) = "BoundVar { unBoundVar = " ++ showF bv ++ "}"

instance (ShowF (S.BoundVar sym)) => SF.ShowF (BoundVar sym arch) where
  showF = showF

-- type family StateVar (arch :: *) = (var :: BaseType -> *) | var -> arch
type family StateVar (arch :: *) :: BaseType -> *

data StateVarDesc (var :: BaseType -> *) where
  StateVarDesc :: forall var tp. BaseTypeRepr tp -> var tp -> StateVarDesc var

class (OrdF a, TestEquality a, ShowF a) => IsStateVar a where
  readStateVar :: String -> Maybe (StateVarDesc a)
  stateVarType :: a tp -> BaseTypeRepr tp
  -- | This should probably be done with EnumF, but I'm lazy for now.
  allStateVars :: [Some a]

class (IsOperand (Operand arch),
       IsOpcode (Opcode arch),
       IsStateVar (StateVar arch),
       OrdF (Opcode arch (Operand arch)),
       I.OpcodeConstraints (Opcode arch) (Operand arch))
      => Architecture arch where
  operandValue :: forall sym s.
                  (S.IsSymInterface sym)
               => arch
               -> sym
               -> MapF.MapF (StateVar arch) (S.BoundVar sym)
               -> Operand arch s
               -> IO (S.SymExpr sym (OperandType arch s))

  operandToStateVar :: forall s. arch -> Operand arch s -> Maybe (StateVar arch (OperandType arch s))
