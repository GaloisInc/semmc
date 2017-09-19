{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module SemMC.Architecture.PPC.Pseudo (
  PseudoOpcode(..)
  ) where

import           GHC.TypeLits ( Symbol )

import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.ShapedList as SL
import qualified Data.Parameterized.TH.GADT as TH

import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()

data PseudoOpcode :: (Symbol -> *) -> [Symbol] -> * where
  -- | @ReplaceByteGPR rA, n, rB@ replaces the @n@th byte of @rA@ with the low
  -- byte of @rB@.
  ReplaceByteGPR :: PseudoOpcode PPC.Operand '["Gprc", "U2imm", "Gprc"]
  -- | @ExtractByteGPR rA, rB, n@ extracts the @n@th byte of @rB@ into the low
  -- byte of @rA@, zero-extending it.
  ExtractByteGPR :: PseudoOpcode PPC.Operand '["Gprc", "Gprc", "U2imm"]
  -- | @ReplaceWordVR vrA, n, rB@ replaces the @n@th word of @vrA@ with the
  -- value of @rB@.
  ReplaceWordVR :: PseudoOpcode PPC.Operand '["Vrrc", "U2imm", "Gprc"]
  -- | @ExtractWordVR rA, vrB, n@ extracts the @n@th word of @vrB@ into @rA@.
  ExtractWordVR :: PseudoOpcode PPC.Operand '["Gprc", "Vrrc", "U2imm"]

deriving instance Show (PseudoOpcode op sh)

instance ShowF (PseudoOpcode op)

$(return [])

instance TestEquality (PseudoOpcode op) where
  testEquality = $(TH.structuralTypeEquality [t| PseudoOpcode |] [])

instance OrdF (PseudoOpcode op) where
  compareF = $(TH.structuralTypeOrd [t| PseudoOpcode |] [])

instance HasRepr (PseudoOpcode op) SL.ShapeRepr where
  typeRepr ReplaceByteGPR = knownRepr
  typeRepr ExtractByteGPR = knownRepr
  typeRepr ReplaceWordVR = knownRepr
  typeRepr ExtractWordVR = knownRepr

instance D.ArbitraryOperands PseudoOpcode PPC.Operand where
  arbitraryOperands gen op = case op of
    ReplaceByteGPR -> D.arbitraryShapedList gen
    ExtractByteGPR -> D.arbitraryShapedList gen
    ReplaceWordVR  -> D.arbitraryShapedList gen
    ExtractWordVR  -> D.arbitraryShapedList gen
