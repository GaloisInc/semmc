{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-| Minimal definition of a test architecture which allows various
  testing to be performed.  Not a real architecture at all... not even
  a little bit.
|-}

module TestArch
where

import           Data.EnumF -- in Dismantle Tablegen!
import           Data.Kind ( Type )
import           Data.Parameterized.Classes
import           Data.Parameterized.List ( List( (:<) ) )
import qualified Data.Parameterized.List as PL
import           GHC.TypeLits ( Symbol )
import           Numeric.Natural
import qualified SemMC.Architecture as SA
import qualified SemMC.Architecture.Location as L
import           What4.BaseTypes


data TestGenArch  -- ^ the architecture type for testing

instance SA.Architecture TestGenArch where
  shapeReprToTypeRepr _ FooArg = BaseNatRepr


----------------------------------------------------------------------
-- Location

data TestLocation :: BaseType -> Type where
  TestNatLoc :: Natural -> TestLocation BaseNatType
  TestIntLoc :: Integer -> TestLocation BaseIntegerType
  -- TBD: more basetype locations
  -- TBD: some memory locations
  -- MemLoc :: Mem -> Location (BaseBVType 32)

deriving instance Show (TestLocation tp)
deriving instance Eq (TestLocation tp)
deriving instance Ord (TestLocation tp)

instance ShowF TestLocation where
  showF = show

instance TestEquality TestLocation where
  TestNatLoc l1 `testEquality` TestNatLoc l2 | l1 == l2 = Just Refl
                                             | otherwise = Nothing
  TestIntLoc l1 `testEquality` TestIntLoc l2 | l1 == l2 = Just Refl
                                             | otherwise = Nothing
  _ `testEquality` _ = Nothing

instance OrdF TestLocation where
  TestNatLoc l1 `compareF` TestNatLoc l2 = fromOrdering $ l1 `compare` l2
  TestIntLoc l1 `compareF` TestIntLoc l2 = fromOrdering $ l1 `compare` l2
  -- for mismatched location types, arbitrarily: any Int < any Nat
  TestIntLoc _ `compareF` TestNatLoc _ = LTF
  TestNatLoc _ `compareF` TestIntLoc _ = GTF
  -- _ `compareF` _ = LTF

instance L.IsLocation TestLocation where
  locationType (TestNatLoc _) = BaseNatRepr
  locationType (TestIntLoc _) = BaseIntegerRepr

  isMemoryLocation _ = False


type instance L.Location TestGenArch = TestLocation


----------------------------------------------------------------------
-- Operands

-- type ShapeRepr arch = SL.List (OperandTypeRepr arch)
-- type family OperandType (arch::Type) (op[erand]::Symbol) :: BaseType

type instance SA.OperandType TestGenArch "Foo" = BaseNatType

data TestGenOperand (nm::Symbol) where
  FooArg :: TestGenOperand "Foo"

deriving instance Show (TestGenOperand nm)
instance ShowF TestGenOperand
  where
    showF _ = "<<OPERAND>>"
instance TestEquality TestGenOperand where
  FooArg `testEquality` FooArg = Just Refl

instance OrdF TestGenOperand where
  compareF FooArg FooArg = EQF

-- data TestGenOperandType (operand :: Symbol) where
--   "Wave" :: TestGenOpcodeType "Wave"

data TestGenOpcode (operand_type :: Symbol -> Type) (opcodes :: [Symbol]) where
  OpWave :: TestGenOpcode TestGenOperand '[]
  -- TestGenOperand ["Wave"] = OpWave

deriving instance Show (TestGenOpcode operand_type opcodes)

instance TestEquality (TestGenOpcode operand_type) where
  OpWave `testEquality` OpWave = Just Refl

opWaveShape :: List TestGenOperand '["Foo"]
opWaveShape = FooArg :< PL.Nil

type instance SA.Opcode TestGenArch = TestGenOpcode
type instance SA.Operand TestGenArch = TestGenOperand

instance SA.IsOperand TestGenOperand
instance SA.IsOpcode TestGenOpcode

instance SA.IsOperandTypeRepr TestGenArch where
  type OperandTypeRepr TestGenArch = TestGenOperand
  operandTypeReprSymbol arch operandType =
    case testEquality operandType FooArg of
      Just Refl -> "Foo"
      Nothing -> error "unrecognized operandtype for reprsymbol"

instance ShowF (TestGenOpcode TestGenOperand)
  where
    showF _ = "<<OPCODE>>"
instance EnumF (TestGenOpcode TestGenOperand) where
instance OrdF (TestGenOpcode TestGenOperand)

-- BV :: SemMC.BoundVar
-- newtype BoundVar (sym :: Type) (arch :: Type) (op :: Symbol) =
--   BoundVar { unBoundVar :: S.BoundVar sym (OperandType arch op) }

-- S (What4.Interface):
-- type family BoundVar (sym :: Type) :: BaseType -> Type
