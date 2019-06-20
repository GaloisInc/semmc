{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Data.Parameterized.Some
import qualified Data.Parameterized.SymbolRepr as SR
import qualified Data.Parameterized.TH.GADT as TH
import qualified Data.Text as T
import           GHC.TypeLits ( Symbol )
import           Numeric.Natural
import qualified SemMC.Architecture as SA
import qualified SemMC.Architecture.Location as L
import           What4.BaseTypes


data TestGenArch  -- ^ the architecture type for testing

instance SA.Architecture TestGenArch where
  data TaggedExpr TestGenArch sym s =
    TaggedExpr { unTaggedExpr :: SA.AllocatedOperand TestGenArch sym s }

  unTagged te = case unTaggedExpr te of
    SA.LocationOperand _ se -> Just se

  taggedOperand = unTaggedExpr

  operandToLocation _ BarArg = Just (TestRegLoc 0)

  uninterpretedFunctions _ = []  -- TODO: add some

  shapeReprToTypeRepr _ sr =
    case SR.symbolRepr sr of
      "Foo"
        | Just Refl <- testEquality sr (SR.knownSymbol @"Foo")
          -> BaseNatRepr
      "Bar"
        | Just Refl <- testEquality sr (SR.knownSymbol @"Bar")
          -> BaseBVRepr (knownNat :: NatRepr 32)
      _ -> error ("Invalid shape repr: " ++ show sr)

  allocateSymExprsForOperand _arch _sym newVars FooArg = undefined
  allocateSymExprsForOperand _arch _sym newVars BarArg =
    let loc = TestRegLoc 0 in
    TaggedExpr <$> SA.LocationOperand loc <$> newVars loc

  locationFuncInterpretation _ = []


----------------------------------------------------------------------
-- Location

data TestLocation :: BaseType -> Type where
  TestNatLoc :: Natural -> TestLocation BaseNatType
  TestIntLoc :: Integer -> TestLocation BaseIntegerType
  TestRegLoc :: Natural -> TestLocation (BaseBVType 32)
  -- TBD: more basetype locations
  -- TBD: some memory locations
  -- MemLoc :: Mem -> Location (BaseBVType 32)

instance Show (TestLocation tp) where
  show (TestRegLoc n) = "Reg_" <> show n  -- KWQ: must be parseable; Reg#0 fails with the #... needs quoting or input validation in the Printer
  show (TestNatLoc n) = "NAT_" <> show n  -- KWQ: want NAT@... see above
  show (TestIntLoc i) = if i >= 0
                        then "INT_" <> show i  -- KWQ: want INT@... see above
                        else "NEGINT_" <> show (-i)

instance ShowF TestLocation

-- must be the inverse of the show instance above
readTestLocation "Reg_0" = Just $ Some $ TestRegLoc 0
readTestLocation "Reg_1" = Just $ Some $ TestRegLoc 1
readTestLocation "Reg_2" = Just $ Some $ TestRegLoc 2
readTestLocation "Reg_3" = Just $ Some $ TestRegLoc 3
readTestLocation _ = Nothing

deriving instance Eq (TestLocation tp)
deriving instance Ord (TestLocation tp)

instance L.IsLocation TestLocation where
  locationType (TestNatLoc _) = BaseNatRepr
  locationType (TestIntLoc _) = BaseIntegerRepr
  locationType (TestRegLoc _) = BaseBVRepr knownNat

  readLocation = readTestLocation

  isMemoryLocation _ = False

  allLocations = []
                 <> (Some . TestRegLoc <$> [0..3])
                 <> (Some . TestNatLoc <$> [0..6])
                 <> (Some . TestIntLoc <$> [-10..10])


type instance L.Location TestGenArch = TestLocation


----------------------------------------------------------------------
-- Operands

-- type ShapeRepr arch = SL.List (OperandTypeRepr arch)
-- type family OperandType (arch::Type) (op[erand]::Symbol) :: BaseType

type instance SA.OperandType TestGenArch "Foo" = BaseNatType  -- unsupported for Printer
type instance SA.OperandType TestGenArch "Bar" = BaseBVType 32  -- unsupported for Printer

data TestGenOperand (nm::Symbol) where
  FooArg :: TestGenOperand "Foo"
  BarArg :: TestGenOperand "Bar"

deriving instance Show (TestGenOperand nm)
instance ShowF TestGenOperand
  where
    showF _ = "<<OPERAND>>"
-- data TestGenOperandType (operand :: Symbol) where
--   "Wave" :: TestGenOpcodeType "Wave"

data TestGenOpcode (operand_constr :: Symbol -> Type) (operands :: [Symbol]) where
  OpWave :: TestGenOpcode TestGenOperand '["Bar"]
  OpSurf :: TestGenOpcode TestGenOperand '["Foo"]

deriving instance Show (TestGenOpcode operand_constr operands)

opWaveShape :: List (SA.OperandTypeRepr TestGenArch) '["Bar"]
opWaveShape = SR.knownSymbol :< PL.Nil

type instance SA.Opcode TestGenArch = TestGenOpcode
type instance SA.Operand TestGenArch = TestGenOperand

instance SA.IsOperand TestGenOperand
instance SA.IsOpcode TestGenOpcode

instance SA.IsOperandTypeRepr TestGenArch where
  type OperandTypeRepr TestGenArch = SR.SymbolRepr
  operandTypeReprSymbol arch = T.unpack . SR.symbolRepr

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

----------------------------------------------------------------------
-- TestEquality and OrdF instances

$(return [])

instance TestEquality TestGenOperand where
  testEquality = $(TH.structuralTypeEquality [t| TestGenOperand |] [])

instance OrdF TestGenOperand where
  compareF = $(TH.structuralTypeOrd [t| TestGenOperand |] [])

instance TestEquality (TestGenOpcode operand_constr) where
  testEquality = $(TH.structuralTypeEquality [t| TestGenOpcode |] [])

instance TestEquality TestLocation where
  testEquality = $(TH.structuralTypeEquality [t| TestLocation |] [])

instance OrdF TestLocation where
  compareF = $(TH.structuralTypeOrd [t| TestLocation |] [])
