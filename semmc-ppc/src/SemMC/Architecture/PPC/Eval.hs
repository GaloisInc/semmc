{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluators for location functions in formula definitions (e.g., memri_reg)
module SemMC.Architecture.PPC.Eval (
  interpMemriReg,
  interpMemriRegExtractor,
  interpMemriOffsetExtractor,
  interpMemriOffset,
  interpMemrixReg,
  interpMemrixRegExtractor,
  interpMemrixOffsetExtractor,
  interpMemrixOffset,
  interpMemrrBase,
  interpMemrrOffset,
  interpMemrrBaseExtractor,
  interpMemrrOffsetExtractor,
  interpIsR0,
  createSymbolicEntries
  ) where

import           Data.Int ( Int16 )
import qualified Data.Int.Indexed as I
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as SL
import           What4.BaseTypes

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F

import           SemMC.Architecture.PPC.Location

-- | Uninterpreted function names are mangled in SimpleBuilder, so we need to
-- create extra entries to match their mangled names.
--
-- In particular, periods in names are converted to underscores.
--
-- This function creates copies of entries with periods in their names with the
-- escaped version as it appears in a SimpleBuilder symbolic function.  For
-- example, if there is an entry with the name @ppc.foo@, this function retains
-- that entry in the input list and adds an additional entry under @ppc_foo@.
createSymbolicEntries :: [(String, a)] -> [(String, a)]
createSymbolicEntries = foldr duplicateIfDotted []
  where
    duplicateIfDotted elt@(s, e) acc =
      case '.' `elem` s of
        False -> acc
        True ->
          let newElt = (map (\c -> if c == '.' then '_' else c) s, e)
          in newElt : elt : acc

interpMemriReg :: forall sh s ppc tp
                . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
               => SL.List PPC.Operand sh
               -> F.WrappedOperand ppc sh s
               -> BaseTypeRepr tp
               -> Maybe (L.Location ppc tp)
interpMemriReg operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    PPC.Memri (PPC.MemRI (Just base) _) ->
      let loc :: Location ppc (BaseBVType (ArchRegWidth ppc))
          loc = LocGPR base
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
          | otherwise -> error ("Invalid return type for location function 'memri_reg' at index " ++ show ix)
    PPC.Memri (PPC.MemRI Nothing _) -> error ("Invalid instruction form with operand " ++ show ix ++ " = r0")
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemriRegExtractor :: PPC.MemRI -> Maybe PPC.GPR
interpMemriRegExtractor (PPC.MemRI mgpr _) = mgpr

interpMemriOffsetExtractor :: PPC.MemRI -> Int16
interpMemriOffsetExtractor (PPC.MemRI _ off) = off

interpMemrixReg :: forall sh s ppc tp
                 . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
                => SL.List PPC.Operand sh
                -> F.WrappedOperand ppc sh s
                -> BaseTypeRepr tp
                -> Maybe (L.Location ppc tp)
interpMemrixReg operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    PPC.Memrix (PPC.MemRIX (Just base) _) ->
      let loc :: Location ppc (BaseBVType (ArchRegWidth ppc))
          loc = LocGPR base
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
          | otherwise -> error ("Invalid return type for location function 'memrix_reg' at index " ++ show ix)
    PPC.Memrix (PPC.MemRIX Nothing _) -> error ("Invalid instruction form with operand " ++ show ix ++ " = r0")
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrixOffset :: forall sh s ppc tp
                 . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
                => SL.List PPC.Operand sh
                -> F.WrappedOperand ppc sh s
                -> BaseTypeRepr tp
                -> Maybe (L.Location ppc tp)
interpMemrixOffset _ _ _ = Nothing

interpMemriOffset :: forall sh s ppc tp
                 . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
                => SL.List PPC.Operand sh
                -> F.WrappedOperand ppc sh s
                -> BaseTypeRepr tp
                -> Maybe (L.Location ppc tp)
interpMemriOffset _ _ _ = Nothing

interpMemrixRegExtractor :: PPC.MemRIX -> Maybe PPC.GPR
interpMemrixRegExtractor (PPC.MemRIX mgpr _) = mgpr

interpMemrixOffsetExtractor :: PPC.MemRIX -> I.I 14
interpMemrixOffsetExtractor (PPC.MemRIX _ off) = off

interpMemrrBase :: forall sh s ppc tp
                . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
               => SL.List PPC.Operand sh
               -> F.WrappedOperand ppc sh s
               -> BaseTypeRepr tp
               -> Maybe (L.Location ppc tp)
interpMemrrBase operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    PPC.Memrr (PPC.MemRR (Just base) _) ->
      let loc :: Location ppc (BaseBVType (ArchRegWidth ppc))
          loc = LocGPR base
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
          | otherwise -> error ("Invalid return type for location function 'memrr_base' at index " ++ show ix)
    PPC.Memrr (PPC.MemRR Nothing _) -> error ("Invalid instruction form with operand " ++ show ix ++ " = r0")
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrrOffset
  :: forall sh s ppc tp . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
  => SL.List PPC.Operand sh
  -> F.WrappedOperand ppc sh s
  -> BaseTypeRepr tp
  -> Maybe (L.Location ppc tp)
interpMemrrOffset operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    PPC.Memrr (PPC.MemRR _ offset) ->
      let loc :: Location ppc (BaseBVType (ArchRegWidth ppc))
          loc = LocGPR offset
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
          | otherwise -> error ("Invalid return type for location function 'memrr_offset' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrrBaseExtractor :: PPC.MemRR -> Maybe PPC.GPR
interpMemrrBaseExtractor (PPC.MemRR mgpr _) = mgpr

interpMemrrOffsetExtractor :: PPC.MemRR -> PPC.GPR
interpMemrrOffsetExtractor (PPC.MemRR _ gpr) = gpr

class InterpIsR0 a where
  interpIsR0 :: a -> Bool

instance InterpIsR0 PPC.GPR where
  interpIsR0 (PPC.GPR rnum) = rnum == 0

instance InterpIsR0 (Maybe PPC.GPR) where
  interpIsR0 mr =
    case mr of
      Nothing -> True
      Just r -> interpIsR0 r
