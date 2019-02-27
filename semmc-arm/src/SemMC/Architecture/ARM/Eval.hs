-- | Evaluators for location functions in formula definitions (e.g., memri_reg)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Eval
    ( interpIsR15
    , interpAm2offsetimmImmExtractor
    , interpAm2offsetimmAddExtractor
    , interpBlxTarget_S
    , interpBlxTarget_imm10H
    , interpBlxTarget_imm10L
    , interpBlxTarget_J1
    , interpBlxTarget_J2
    , interpImm12Reg
    , interpImm12RegExtractor
    , interpImm12OffsetExtractor
    , interpImm12AddFlgExtractor
    , interpImm01020s4ImmExtractor
    , interpImm0508s4ImmExtractor
    , interpLdstsoregAddExtractor
    , interpLdstsoregImmExtractor
    , interpLdstsoregTypeExtractor
    , interpLdstsoregBaseRegExtractor
    , interpLdstsoregBaseReg
    , interpLdstsoregOffRegExtractor
    , interpLdstsoregOffReg
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
    , interpT2soimmImmExtractor
    , interpT2soregImmExtractor
    , interpT2soregRegExtractor
    , interpT2soregTypeExtractor
    , interpT2soregReg
    , interpTaddrmodeis2ImmExtractor
    , interpTaddrmodeis2RegExtractor
    , interpTaddrmodeis2Reg
    , interpTaddrmodeis4ImmExtractor
    , interpTaddrmodeis4RegExtractor
    , interpTaddrmodeis4Reg
    , interpTaddrmodepcExtractor
    , interpTReglistExtractor
    )
    where

import           Data.Int ( Int16, Int8 )
import qualified Data.Parameterized.List as PL
import qualified Data.Word.Indexed as W
import qualified Dismantle.ARM.Operands as ARMOperands
import qualified Dismantle.Thumb.Operands as ThumbOperands
import qualified SemMC.Architecture as A
import           SemMC.Architecture.ARM.Location
import qualified SemMC.Architecture.ARM.OperandComponents as AOC
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F
import           What4.BaseTypes


------------------------------------------------------------------------
-- | Extract values from the ARM Am2offset_imm operand

interpAm2offsetimmImmExtractor :: ARMOperands.Am2OffsetImm -> W.W 12 -- Int16
interpAm2offsetimmImmExtractor = {- fromIntegral . W.unW . -} ARMOperands.am2OffsetImmImmediate

interpAm2offsetimmAddExtractor :: ARMOperands.Am2OffsetImm -> Bool
interpAm2offsetimmAddExtractor = (== 1) . ARMOperands.am2OffsetImmAdd


------------------------------------------------------------------------
-- | Extract values from the ARM Addrmode_imm12 operand

-- | Extract the register value from an addrmode_imm12[_pre] via
-- the a32.imm12_reg user function.
interpImm12Reg :: forall sh s arm tp sym
                . ( L.IsLocation (L.Location arm)
                  , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                  )
               => PL.List (A.AllocatedOperand arm sym) sh
               -> F.WrappedOperand arm sh s
               -> BaseTypeRepr tp
               -> Maybe (L.Location arm tp)
interpImm12Reg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCAddrmodeImm12 loc _ _ _)
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 'imm12_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpImm12RegExtractor :: ARMOperands.AddrModeImm12 -> Maybe ARMOperands.GPR
interpImm12RegExtractor = Just . ARMOperands.addrModeImm12Register

interpImm12OffsetExtractor :: ARMOperands.AddrModeImm12 -> Int16
interpImm12OffsetExtractor = fromIntegral . W.unW . ARMOperands.addrModeImm12Immediate

interpImm12AddFlgExtractor :: ARMOperands.AddrModeImm12 -> Bool
interpImm12AddFlgExtractor = (== 1) . ARMOperands.addrModeImm12Add


------------------------------------------------------------------------
-- | Extract values from the Thumb AddrModePc operand

interpTaddrmodepcExtractor :: ThumbOperands.AddrModePc -> Int8
interpTaddrmodepcExtractor = fromInteger . toInteger . ThumbOperands.addrModePcToBits


------------------------------------------------------------------------
-- | Extract values from the Thumb Imm0_1020S4 operand

interpImm01020s4ImmExtractor :: ThumbOperands.TImm01020S4 -> Int8
interpImm01020s4ImmExtractor = fromInteger . toInteger . ThumbOperands.tImm01020S4ToBits

------------------------------------------------------------------------
-- | Extract values from the Thumb Imm0_508S4 operand

interpImm0508s4ImmExtractor :: ThumbOperands.TImm0508S4 -> Int8
interpImm0508s4ImmExtractor = fromInteger . toInteger . ThumbOperands.tImm0508S4ToBits


------------------------------------------------------------------------
-- | Extract values from the ARM LdstSoReg operand

interpLdstsoregAddExtractor :: ARMOperands.LdstSoReg -> Bool
interpLdstsoregAddExtractor = (== 1) . ARMOperands.ldstSoRegAdd

interpLdstsoregImmExtractor :: ARMOperands.LdstSoReg -> W.W 5
interpLdstsoregImmExtractor = ARMOperands.ldstSoRegImmediate

interpLdstsoregTypeExtractor :: ARMOperands.LdstSoReg -> W.W 2
interpLdstsoregTypeExtractor = ARMOperands.ldstSoRegShiftType

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpLdstsoregBaseRegExtractor :: ARMOperands.LdstSoReg -> Maybe ARMOperands.GPR
interpLdstsoregBaseRegExtractor = Just . ARMOperands.ldstSoRegBaseRegister

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpLdstsoregOffRegExtractor :: ARMOperands.LdstSoReg -> Maybe ARMOperands.GPR
interpLdstsoregOffRegExtractor = Just . ARMOperands.ldstSoRegOffsetRegister


interpLdstsoregBaseReg :: forall sh s arm tp sym
                        . ( L.IsLocation (L.Location arm)
                          , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                          )
                       => PL.List (A.AllocatedOperand arm sym) sh
                       -> F.WrappedOperand arm sh s
                       -> BaseTypeRepr tp
                       -> Maybe (L.Location arm tp)
interpLdstsoregBaseReg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCLdstSoReg { AOC.ldstSoRegBaseLoc = loc })
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 'ldst_so_reg' base reg at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

interpLdstsoregOffReg :: forall sh s arm tp sym
                       . ( L.IsLocation (L.Location arm)
                         , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                         )
                      => PL.List (A.AllocatedOperand arm sym) sh
                      -> F.WrappedOperand arm sh s
                      -> BaseTypeRepr tp
                      -> Maybe (L.Location arm tp)
interpLdstsoregOffReg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCLdstSoReg { AOC.ldstSoRegOffsetLoc = loc })
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 'ldst_so_reg' offset reg at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

------------------------------------------------------------------------
-- | Extract values from the ARM Mod_imm operand

interpModimmImmExtractor :: ARMOperands.ModImm -> Int8
interpModimmImmExtractor = fromIntegral . W.unW . ARMOperands.modImmOrigImmediate

interpModimmRotExtractor :: ARMOperands.ModImm -> W.W 4
interpModimmRotExtractor = ARMOperands.modImmOrigRotate


------------------------------------------------------------------------
-- | Extract values from the Thumb ThumbBlxTarget operand

interpBlxTarget_S :: ThumbOperands.ThumbBlxTarget -> W.W 1
interpBlxTarget_S = fromInteger . toInteger . ThumbOperands.thumbBlxTargetS

interpBlxTarget_J1 :: ThumbOperands.ThumbBlxTarget -> W.W 1
interpBlxTarget_J1 = fromInteger . toInteger . ThumbOperands.thumbBlxTargetJ1

interpBlxTarget_J2 :: ThumbOperands.ThumbBlxTarget -> W.W 1
interpBlxTarget_J2 = fromInteger . toInteger . ThumbOperands.thumbBlxTargetJ2

interpBlxTarget_imm10H :: ThumbOperands.ThumbBlxTarget -> W.W 10
interpBlxTarget_imm10H = fromInteger . toInteger . ThumbOperands.thumbBlxTargetImm10H

interpBlxTarget_imm10L :: ThumbOperands.ThumbBlxTarget -> W.W 10
interpBlxTarget_imm10L = fromInteger . toInteger . ThumbOperands.thumbBlxTargetImm10L


------------------------------------------------------------------------
-- | Extract values from the ARM SoRegImm operand

interpSoregimmTypeExtractor :: ARMOperands.SoRegImm -> W.W 2
interpSoregimmTypeExtractor = ARMOperands.soRegImmShiftType

interpSoregimmImmExtractor :: ARMOperands.SoRegImm -> W.W 5
interpSoregimmImmExtractor = ARMOperands.soRegImmImmediate

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpSoregimmRegExtractor :: ARMOperands.SoRegImm -> Maybe ARMOperands.GPR
interpSoregimmRegExtractor = Just . ARMOperands.soRegImmReg


-- | Extract the register value from a SoRegReg via the
-- a32.soregimm_reg user function.
interpSoregimmReg :: forall sh s arm tp sym
                   . ( L.IsLocation (L.Location arm)
                     , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                     )
                  => PL.List (A.AllocatedOperand arm sym) sh
                  -> F.WrappedOperand arm sh s
                  -> BaseTypeRepr tp
                  -> Maybe (L.Location arm tp)
interpSoregimmReg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCSoRegImm loc _ _ _)
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise ->  error ("Invalid return type for location function 'soregimm_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)


------------------------------------------------------------------------
-- | Extract values from the ARM SoRegReg operand

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpSoregregReg1Extractor :: ARMOperands.SoRegReg -> Maybe ARMOperands.GPR
interpSoregregReg1Extractor = Just . ARMOperands.soRegRegReg1

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpSoregregReg2Extractor :: ARMOperands.SoRegReg -> Maybe ARMOperands.GPR
interpSoregregReg2Extractor = Just . ARMOperands.soRegRegReg2

interpSoregregTypeExtractor :: ARMOperands.SoRegReg -> W.W 2
interpSoregregTypeExtractor = ARMOperands.soRegRegShiftType

-- | Extract the register value from a SoRegReg via the
-- a32.soregreg_reg user function.
interpSoregregReg1 :: forall sh s arm tp sym
                    . ( L.IsLocation (L.Location arm)
                      , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                      )
                   => PL.List (A.AllocatedOperand arm sym) sh
                   -> F.WrappedOperand arm sh s
                   -> BaseTypeRepr tp
                   -> Maybe (L.Location arm tp)
interpSoregregReg1 operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCSoRegReg loc _ _ _ _)
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 'soregreg_reg' 1 at index " ++ show ix)
    _ -> error ("Invalid operand type 1 at index " ++ show ix)


-- | Extract the register value from a SoRegReg via the
-- a32.soregreg_reg user function.
interpSoregregReg2 :: forall sh s arm tp sym
                    . ( L.IsLocation (L.Location arm)
                      , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                      )
                   => PL.List (A.AllocatedOperand arm sym) sh
                   -> F.WrappedOperand arm sh s
                   -> BaseTypeRepr tp
                   -> Maybe (L.Location arm tp)
interpSoregregReg2 operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCSoRegReg _ _ loc _ _)
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 'soregreg_reg' 2 at index " ++ show ix)
    _ -> error ("Invalid operand type 2 at index " ++ show ix)


------------------------------------------------------------------------
-- | Extract values from the Thumb AddrModeIs4 operand

interpTaddrmodeis2ImmExtractor :: ThumbOperands.AddrModeIs2 -> W.W 5
interpTaddrmodeis2ImmExtractor = fromInteger . toInteger . ThumbOperands.addrModeIs2Imm

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpTaddrmodeis2RegExtractor :: ThumbOperands.AddrModeIs2 -> Maybe ThumbOperands.LowGPR
interpTaddrmodeis2RegExtractor = Just . ThumbOperands.addrModeIs2Reg

interpTaddrmodeis2Reg :: forall sh s arm tp sym
                       . ( L.IsLocation (Location arm)
                         , L.Location arm ~ Location arm
                         , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                         )
                      => PL.List (A.AllocatedOperand arm sym) sh
                      -> F.WrappedOperand arm sh s
                      -> BaseTypeRepr tp
                      -> Maybe (L.Location arm tp)
interpTaddrmodeis2Reg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCTAddrModeIs2 loc _ _)
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 'addrmode_is2_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)


------------------------------------------------------------------------
-- | Extract values from the Thumb AddrModeIs4 operand

interpTaddrmodeis4ImmExtractor :: ThumbOperands.AddrModeIs4 -> W.W 5
interpTaddrmodeis4ImmExtractor = fromInteger . toInteger . ThumbOperands.addrModeIs4Imm

-- n.b. there is no Nothing, but the call in macaw.SemMC.TH expects a Maybe result.
interpTaddrmodeis4RegExtractor :: ThumbOperands.AddrModeIs4 -> Maybe ThumbOperands.LowGPR
interpTaddrmodeis4RegExtractor = Just . ThumbOperands.addrModeIs4Reg

interpTaddrmodeis4Reg :: forall sh s arm tp sym
                       . ( L.IsLocation (Location arm)
                         , L.Location arm ~ Location arm
                         , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                         )
                      => PL.List (A.AllocatedOperand arm sym) sh
                      -> F.WrappedOperand arm sh s
                      -> BaseTypeRepr tp
                      -> Maybe (L.Location arm tp)
interpTaddrmodeis4Reg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCTAddrModeIs4 loc _ _)
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 'addrmode_is4_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)


------------------------------------------------------------------------
-- | Extract values from the Thumb T2SoImm operand

interpT2soimmImmExtractor :: ThumbOperands.T2SoImm -> W.W 12
interpT2soimmImmExtractor = fromInteger . toInteger . ThumbOperands.t2SoImmToBits

------------------------------------------------------------------------
-- | Extract values from Thumb T2SoReg operand

interpT2soregImmExtractor :: ThumbOperands.T2SoReg -> W.W 5
interpT2soregImmExtractor = fromInteger . toInteger . ThumbOperands.t2SoRegImm5

interpT2soregRegExtractor :: ThumbOperands.T2SoReg -> Maybe ThumbOperands.GPR
interpT2soregRegExtractor = Just . ThumbOperands.t2SoRegRm

interpT2soregTypeExtractor :: ThumbOperands.T2SoReg -> W.W 2
interpT2soregTypeExtractor = fromInteger . toInteger . ThumbOperands.t2SoRegShiftType

interpT2soregReg :: forall sh s arm tp sym
                  . ( L.IsLocation (Location arm)
                    , L.Location arm ~ Location arm
                    , A.OperandComponents arm sym ~ AOC.OperandComponents arm sym
                    )
                 => PL.List (A.AllocatedOperand arm sym) sh
                 -> F.WrappedOperand arm sh s
                 -> BaseTypeRepr tp
                 -> Maybe (L.Location arm tp)
interpT2soregReg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    A.CompoundOperand (AOC.OCT2SoReg loc _ _ _)
      | Just Refl <- testEquality (L.locationType loc) rep -> Just loc
      | otherwise -> error ("Invalid return type for location function 't2_so_reg_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)


------------------------------------------------------------------------
-- | Extract values from the Thumb Reglist operand

interpTReglistExtractor :: ThumbOperands.Reglist -> Int16
interpTReglistExtractor = fromInteger . toInteger . ThumbOperands.regListToBits


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

instance InterpIsR15 ThumbOperands.GPR where
    interpIsR15 gprReg = ThumbOperands.unGPR gprReg == 15

instance InterpIsR15 (Maybe ThumbOperands.GPR) where
  interpIsR15 mr =
    case mr of
      Nothing -> True
      Just r -> interpIsR15 r


instance InterpIsR15 ThumbOperands.LowGPR where
    interpIsR15 _ = False  -- only 3 bits, can never be 15

instance InterpIsR15 (Maybe ThumbOperands.LowGPR) where
  interpIsR15 mr =
    case mr of
      Nothing -> True
      Just r -> interpIsR15 r
