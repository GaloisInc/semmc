-- | Evaluators for location functions in formula definitions (e.g., memri_reg)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Eval
    ( createSymbolicEntries
    , interpIsR15
    , interpAm2offsetimmImmExtractor
    , interpAm2offsetimmAddExtractor
    , interpImm12Reg
    , interpImm12RegExtractor
    , interpImm12OffsetExtractor
    , interpImm12AddFlgExtractor
    , interpModimmImmExtractor
    , interpModimmRotExtractor
    , interpSoregimmTypeExtractor
    , interpSoregimmImmExtractor
    , interpSoregimmRegExtractor
    , interpSoregimmReg
    , interpSoregregTypeExtractor
    , interpSoregregReg1Extractor
    , interpSoregregReg2Extractor
    , interpSoregregReg1
    , interpSoregregReg2
    )
    where

import           Data.Int ( Int16, Int8 )
import qualified Data.Parameterized.List as PL
import           Data.Word
import qualified Data.Word.Indexed as W
import qualified Dismantle.ARM as ARM
import qualified Dismantle.ARM.Operands as ARMOperands
import           Lang.Crucible.BaseTypes
import           SemMC.Architecture.ARM.Location
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F


-- | Uninterpreted function names are mangled in SimpleBuilder, so we need to
-- create extra entries to match their mangled names.
--
-- In particular, periods in names are converted to underscores.
--
-- This function creates copies of entries with periods in their names with the
-- escaped version as it appears in a SimpleBuilder symbolic function.  For
-- example, if there is an entry with the name @arm.foo@, this function retains
-- that entry in the input list and adds an additional entry under @arm_foo@.
createSymbolicEntries :: [(String, a)] -> [(String, a)]
createSymbolicEntries = foldr duplicateIfDotted []
  where
    duplicateIfDotted elt@(s, e) acc =
      case '.' `elem` s of
        False -> acc
        True ->
          let newElt = (map (\c -> if c == '.' then '_' else c) s, e)
          in newElt : elt : acc


------------------------------------------------------------------------
-- | Extract values from the Am2offset_imm operand

interpAm2offsetimmImmExtractor :: ARMOperands.Am2OffsetImm -> Int16
interpAm2offsetimmImmExtractor = fromInteger . toInteger . ARMOperands.am2OffsetImmImmediate

interpAm2offsetimmAddExtractor :: ARMOperands.Am2OffsetImm -> Bool
interpAm2offsetimmAddExtractor = (== 1) . ARMOperands.am2OffsetImmAdd


------------------------------------------------------------------------

-- | Extract the register value from an addrmode_imm12[_pre] via
-- the a32.imm12_reg user function.
interpImm12Reg :: forall sh s arm tp
                   . (L.IsLocation (Location arm), L.Location arm ~ Location arm)
                   => PL.List ARM.Operand sh
                 -> F.WrappedOperand arm sh s
                 -> BaseTypeRepr tp
                 -> L.Location arm tp
interpImm12Reg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    ARM.Addrmode_imm12_pre oprnd ->
      let loc :: Location arm (BaseBVType (ArchRegWidth arm))
          loc = LocGPR $ ARMOperands.addrModeImm12Register oprnd
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'imm12_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpImm12RegExtractor :: ARMOperands.AddrModeImm12 -> Maybe ARMOperands.GPR
interpImm12RegExtractor = Just . ARMOperands.addrModeImm12Register

interpImm12OffsetExtractor :: ARMOperands.AddrModeImm12 -> Int16
interpImm12OffsetExtractor = fromInteger . toInteger . ARMOperands.addrModeImm12Immediate

interpImm12AddFlgExtractor :: ARMOperands.AddrModeImm12 -> Bool
interpImm12AddFlgExtractor = (== 1) . ARMOperands.addrModeImm12Add


------------------------------------------------------------------------
-- | Extract values from the Mod_imm operand

interpModimmImmExtractor :: ARMOperands.ModImm -> Int8
interpModimmImmExtractor = fromInteger . toInteger . ARMOperands.modImmOrigImmediate

interpModimmRotExtractor :: ARMOperands.ModImm -> W.W 4
interpModimmRotExtractor = fromInteger . toInteger . ARMOperands.modImmOrigRotate



------------------------------------------------------------------------
-- | Extract values from the SoRegImm operand

interpSoregimmTypeExtractor :: ARMOperands.SoRegImm -> W.W 2
interpSoregimmTypeExtractor = fromInteger . toInteger . ARMOperands.soRegImmShiftType

interpSoregimmImmExtractor :: ARMOperands.SoRegImm -> W.W 5
interpSoregimmImmExtractor = fromInteger . toInteger . ARMOperands.soRegImmImmediate

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpSoregimmRegExtractor :: ARMOperands.SoRegImm -> Maybe ARMOperands.GPR
interpSoregimmRegExtractor = Just . ARMOperands.soRegImmReg


-- | Extract the register value from a SoRegReg via the
-- a32.soregimm_reg user function.
interpSoregimmReg :: forall sh s arm tp
                     . (L.IsLocation (Location arm), L.Location arm ~ Location arm) =>
                     PL.List ARM.Operand sh
                  -> F.WrappedOperand arm sh s
                  -> BaseTypeRepr tp
                  -> L.Location arm tp
interpSoregimmReg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    ARM.So_reg_imm oprnd ->
      let loc :: Location arm (BaseBVType (ArchRegWidth arm))
          loc = LocGPR $ ARMOperands.soRegImmReg oprnd
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'soregimm_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)


------------------------------------------------------------------------
-- | Extract values from the SoRegReg operand

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpSoregregReg1Extractor :: ARMOperands.SoRegReg -> Maybe ARMOperands.GPR
interpSoregregReg1Extractor = Just . ARMOperands.soRegRegReg1

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpSoregregReg2Extractor :: ARMOperands.SoRegReg -> Maybe ARMOperands.GPR
interpSoregregReg2Extractor = Just . ARMOperands.soRegRegReg2

interpSoregregTypeExtractor :: ARMOperands.SoRegReg -> W.W 2
interpSoregregTypeExtractor = fromInteger . toInteger . ARMOperands.soRegRegShiftType

-- | Extract the register value from a SoRegReg via the
-- a32.soregreg_reg user function.
interpSoregregReg1 :: forall sh s arm tp
                      . (L.IsLocation (Location arm), L.Location arm ~ Location arm) =>
                      PL.List ARM.Operand sh
                   -> F.WrappedOperand arm sh s
                   -> BaseTypeRepr tp
                   -> L.Location arm tp
interpSoregregReg1 operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    ARM.So_reg_reg oprnd ->
      let loc :: Location arm (BaseBVType (ArchRegWidth arm))
          loc = LocGPR $ ARMOperands.soRegRegReg1 oprnd
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'soregreg_reg' 1 at index " ++ show ix)
    _ -> error ("Invalid operand type 1 at index " ++ show ix)


-- | Extract the register value from a SoRegReg via the
-- a32.soregreg_reg user function.
interpSoregregReg2 :: forall sh s arm tp
                      . (L.IsLocation (Location arm), L.Location arm ~ Location arm) =>
                      PL.List ARM.Operand sh
                   -> F.WrappedOperand arm sh s
                   -> BaseTypeRepr tp
                   -> L.Location arm tp
interpSoregregReg2 operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    ARM.So_reg_reg oprnd ->
      let loc :: Location arm (BaseBVType (ArchRegWidth arm))
          loc = LocGPR $ ARMOperands.soRegRegReg2 oprnd
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'soregreg_reg' 2 at index " ++ show ix)
    _ -> error ("Invalid operand type 2 at index " ++ show ix)


------------------------------------------------------------------------

-- | Determination of whether this register reference is for R15
-- (which is often, but not always, the PC).

class InterpIsR15 a where
  interpIsR15 :: a -> Bool

instance InterpIsR15 ARMOperands.GPR where
    interpIsR15 gprReg = ARMOperands.unGPR gprReg == 15

instance InterpIsR15 (Maybe ARMOperands.GPR) where
  interpIsR15 mr =
    case mr of
      Nothing -> True
      Just r -> interpIsR15 r
