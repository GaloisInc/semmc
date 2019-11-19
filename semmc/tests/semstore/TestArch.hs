{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some
import qualified Data.Parameterized.SymbolRepr as SR
import qualified Data.Parameterized.TH.GADT as TH
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           GHC.TypeLits ( Symbol )
import           Numeric.Natural
import qualified SemMC.Architecture as SA
import qualified SemMC.Architecture.Location as L
import           What4.BaseTypes


data TestGenArch  -- ^ the architecture type for testing

type instance SA.RegWidth TestGenArch = 32

instance SA.Architecture TestGenArch where
  data TaggedExpr TestGenArch sym s =
    TaggedExpr { unTaggedExpr :: SA.AllocatedOperand TestGenArch sym s }

  unTagged te = case unTaggedExpr te of
    SA.LocationOperand _ se -> Just se

  taggedOperand = unTaggedExpr

  operandToLocation _ FooArg     = Nothing
  operandToLocation _ BarArg     = Just TestBarLoc
  operandToLocation _ (BoxArg n) = Just (TestBoxLoc n)

  uninterpretedFunctions _ = []  -- TODO: add some

  shapeReprToTypeRepr _ sr =
    case SR.symbolRepr sr of
      "Foo"
        | Just Refl <- testEquality sr (SR.knownSymbol @"Foo")
          -> BaseNatRepr
      "Bar"
        | Just Refl <- testEquality sr (SR.knownSymbol @"Bar")
          -> BaseBVRepr (knownNat :: NatRepr 32)
      "Box"
        | Just Refl <- testEquality sr (SR.knownSymbol @"Box")
          -> BaseBVRepr (knownNat :: NatRepr 32)
      _ -> error ("Invalid shape repr: " ++ show sr)

  allocateSymExprsForOperand _arch _sym _newVars FooArg =
    error "allocateSymExprsForOperand FooArg TBD"
  allocateSymExprsForOperand _arch _sym newVars BarArg =
    let loc = TestBarLoc in
    TaggedExpr <$> SA.LocationOperand loc <$> newVars loc
  allocateSymExprsForOperand _arch _sym newVars (BoxArg n) =
    let loc = TestBoxLoc n in
    TaggedExpr <$> SA.LocationOperand loc <$> newVars loc

  locationFuncInterpretation _ = []


----------------------------------------------------------------------
-- Location

data TestLocation :: BaseType -> Type where
  TestNatLoc :: Natural -> TestLocation BaseNatType
  TestIntLoc :: Integer -> TestLocation BaseIntegerType
  TestBoxLoc :: Natural -> TestLocation (BaseBVType 32)
  TestBarLoc :: TestLocation (BaseBVType 32)
  -- TBD: more basetype locations
  -- TBD: some memory locations
  -- MemLoc :: Mem -> Location (BaseBVType 32)

-- Specifies the serialized S-expression form of these locations.
-- These must be valid S-expression symbols: alphanumeric and
-- underscores only.
instance Show (TestLocation tp) where
  show TestBarLoc     = "Bar"
  show (TestBoxLoc n) = "Box_" <> show n
  show (TestNatLoc n) = "NAT_" <> show n
  show (TestIntLoc i) = if i >= 0
                        then "INT_" <> show i
                        else "NEGINT_" <> show (-i)

instance ShowF TestLocation

deriving instance Eq (TestLocation tp)

instance L.IsLocation TestLocation where
  locationType (TestNatLoc _) = BaseNatRepr
  locationType (TestIntLoc _) = BaseIntegerRepr
  locationType (TestBoxLoc _) = BaseBVRepr knownNat
  locationType TestBarLoc     = BaseBVRepr knownNat

  -- must be the inverse of the show instance above
  readLocation "Box_0" = Just $ Some $ TestBoxLoc 0
  readLocation "Box_1" = Just $ Some $ TestBoxLoc 1
  readLocation "Box_2" = Just $ Some $ TestBoxLoc 2
  readLocation "Box_3" = Just $ Some $ TestBoxLoc 3
  readLocation "Bar"   = Just $ Some $ TestBarLoc
  readLocation _ = Nothing

  isMemoryLocation _ = False
  memLocation = []

  nonMemLocations = [Some TestBarLoc]
                 <> (Some . TestBoxLoc <$> [0..3])
                 <> (Some . TestNatLoc <$> [0..6])
                 <> (Some . TestIntLoc <$> [-10..10])

  defaultLocationExpr = error "TestLocation defaultLocationExpr TBD"

  registerizationLocations = error "TestLocation registerizationLocations TBD"


type instance L.Location TestGenArch = TestLocation


----------------------------------------------------------------------
-- Operands

type instance SA.OperandType TestGenArch "Foo" = BaseNatType  -- unsupported for Printer
type instance SA.OperandType TestGenArch "Bar" = BaseBVType 32
type instance SA.OperandType TestGenArch "Box" = BaseBVType 32

data TestGenOperand (nm::Symbol) where
  FooArg :: TestGenOperand "Foo"
  BarArg :: TestGenOperand "Bar"
  BoxArg :: Natural -> TestGenOperand "Box"

deriving instance Show (TestGenOperand nm)
instance ShowF TestGenOperand
  where
    -- once it's clear where this is needed, a more descriptive form
    -- can be provided.
    showF _ = "<<OPERAND>>"

data TestGenOpcode (operand_constr :: Symbol -> Type) (operands :: [Symbol]) where
  OpWave :: TestGenOpcode TestGenOperand '["Bar"]
  OpSurf :: TestGenOpcode TestGenOperand '["Foo"]
  OpPack :: TestGenOpcode TestGenOperand '["Box", "Bar", "Box", "Box"]
  OpSolo :: TestGenOpcode TestGenOperand '[]

deriving instance Show (TestGenOpcode operand_constr operands)
deriving instance Eq   (TestGenOpcode operand_constr operands)
deriving instance Ord  (TestGenOpcode operand_constr operands)

instance HR.HasRepr (TestGenOpcode TestGenOperand) (PL.List SR.SymbolRepr) where
  typeRepr OpWave = knownRepr
  typeRepr OpSurf = knownRepr
  typeRepr OpPack = knownRepr
  typeRepr OpSolo = knownRepr


type instance SA.Opcode TestGenArch = TestGenOpcode
type instance SA.Operand TestGenArch = TestGenOperand

instance SA.IsOperand TestGenOperand
instance SA.IsOpcode TestGenOpcode

instance SA.IsOperandTypeRepr TestGenArch where
  type OperandTypeRepr TestGenArch = SR.SymbolRepr
  operandTypeReprSymbol _arch = T.unpack . SR.symbolRepr

instance ShowF (TestGenOpcode TestGenOperand)
  where
    -- once it's clear where this is needed, a more descriptive form
    -- can be provided.
    showF _ = "<<OPCODE>>"
instance EnumF (TestGenOpcode TestGenOperand) where
  enumF OpWave = 0
  enumF OpSurf = 1
  enumF OpPack = 2
  enumF OpSolo = 3
  congruentF op = NES.singleton op

instance OrdF (TestGenOpcode TestGenOperand) where
  compareF = enumCompareF

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
