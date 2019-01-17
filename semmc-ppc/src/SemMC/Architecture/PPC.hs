{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SemMC.Architecture.PPC ( AnyPPC, Variant, V32, V64
                              , VariantRepr(..), KnownVariant(..)
                              , AddrWidth, addrWidth ) where

import           Data.Type.Equality ( (:~:)(Refl), TestEquality(testEquality) )
import           Data.Typeable ( Typeable )

import qualified Dismantle.PPC as PPC

import           Data.Parameterized.Classes ( ShowF )
import           Data.Parameterized.NatRepr ( type (<=), NatRepr, knownNat )

import qualified SemMC.Architecture as A
import           SemMC.Architecture.PPC.Location

----------------------------------------------------------------------

data AnyPPC (v :: Variant)

data Variant = V32 | V64

type V32 = 'V32
type V64 = 'V64

----------------------------------------------------------------------

type instance A.Operand (AnyPPC v) = PPC.Operand

instance A.IsOperand PPC.Operand

type instance A.Opcode (AnyPPC v) = PPC.Opcode

instance A.IsOpcode PPC.Opcode

instance A.IsOperandTypeRepr (AnyPPC v) where
  type OperandTypeRepr (AnyPPC v) = PPC.OperandRepr
  operandTypeReprSymbol _ = PPC.operandReprString

type family AddrWidth v = w | w -> v where
  AddrWidth V32 = 32
  AddrWidth V64 = 64

addrWidth :: VariantRepr v -> NatRepr (AddrWidth v)
addrWidth V32Repr = knownNat
addrWidth V64Repr = knownNat

type instance A.RegWidth (AnyPPC v) = AddrWidth v
type instance A.Location (AnyPPC v) = Location (AnyPPC v)

----------------------------------------------------------------------

data VariantRepr v where
  V32Repr :: VariantRepr V32
  V64Repr :: VariantRepr V64

instance Show (VariantRepr v) where
  show V32Repr = "V32"
  show V64Repr = "V64"

instance ShowF VariantRepr

instance TestEquality VariantRepr where
  testEquality V32Repr V32Repr = Just Refl
  testEquality V64Repr V64Repr = Just Refl
  testEquality _ _ = Nothing

class ( 1 <= AddrWidth v
      , Typeable v -- Needed for using Variants in exception types
      ) => KnownVariant v where
  knownVariant :: VariantRepr v

instance KnownVariant V32 where
  knownVariant = V32Repr

instance KnownVariant V64 where
  knownVariant = V64Repr
