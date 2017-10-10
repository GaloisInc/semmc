{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluators for location functions in formula definitions (e.g., memri_reg)
module SemMC.Architecture.PPC.Eval (
  interpMemriReg,
  interpMemrixReg,
  interpMemrrBase
  ) where

import           Data.Parameterized.Classes
import qualified Data.Parameterized.ShapedList as SL
import           Lang.Crucible.BaseTypes

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F

import           SemMC.Architecture.PPC.Location

interpMemriReg :: forall sh s ppc tp
                . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
               => SL.ShapedList PPC.Operand sh
               -> F.WrappedOperand ppc sh s
               -> BaseTypeRepr tp
               -> L.Location ppc tp
interpMemriReg operands (F.WrappedOperand _orep ix) rep =
  case SL.indexShapedList operands ix of
    PPC.Memri (PPC.MemRI (Just base) _) ->
      let loc :: Location ppc (BaseBVType (ArchRegWidth ppc))
          loc = LocGPR base
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'memri_reg' at index " ++ show ix)
    PPC.Memri (PPC.MemRI Nothing _) -> error ("Invalid instruction form with operand " ++ show ix ++ " = r0")
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrixReg :: forall sh s ppc tp
                 . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
                => SL.ShapedList PPC.Operand sh
                -> F.WrappedOperand ppc sh s
                -> BaseTypeRepr tp
                -> L.Location ppc tp
interpMemrixReg operands (F.WrappedOperand _orep ix) rep =
  case SL.indexShapedList operands ix of
    PPC.Memrix (PPC.MemRIX (Just base) _) ->
      let loc :: Location ppc (BaseBVType (ArchRegWidth ppc))
          loc = LocGPR base
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'memrix_reg' at index " ++ show ix)
    PPC.Memrix (PPC.MemRIX Nothing _) -> error ("Invalid instruction form with operand " ++ show ix ++ " = r0")
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrrBase :: forall sh s ppc tp
                . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
               => SL.ShapedList PPC.Operand sh
               -> F.WrappedOperand ppc sh s
               -> BaseTypeRepr tp
               -> L.Location ppc tp
interpMemrrBase operands (F.WrappedOperand _orep ix) rep =
  case SL.indexShapedList operands ix of
    PPC.Memrr (PPC.MemRR (Just base) _) ->
      let loc :: Location ppc (BaseBVType (ArchRegWidth ppc))
          loc = LocGPR base
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'memrr_base' at index " ++ show ix)
    PPC.Memrr (PPC.MemRR Nothing _) -> error ("Invalid instruction form with operand " ++ show ix ++ " = r0")
    _ -> error ("Invalid operand type at index " ++ show ix)
