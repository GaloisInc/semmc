-- | Defines the ARM local operand references (used in the SemMC DSL
-- specification of operands) and the corresponding operand names in
-- Dismantle.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module SemMC.Architecture.ARM.BaseSemantics.OperandClasses
    where

import           Data.Kind ( Type )
import qualified Data.Type.List as TL
import           GHC.TypeLits ( Symbol )
import           SemMC.DSL


gpr :: String
gpr = "GPR"

-- FIXME: Do we actually need to use the below, or can we just use gpr?
gprnopc :: String
gprnopc = "GPRnopc"

tgpr :: String
tgpr = "TGPR"

rgpr :: String
rgpr = "RGPR"

memri :: String
memri = "Memri"

-- | pred is the 4-bit condition value that indicates whether an instruction will be executed.
pred :: String
pred = "Pred"

addrmode_imm12 :: String
addrmode_imm12 = "Addrmode_imm12"

addrmode_imm12_pre :: String
addrmode_imm12_pre = "Addrmode_imm12_pre"

am2offset_imm :: String
am2offset_imm = "Am2offset_imm"

addr_offset_none :: String
addr_offset_none = "Addr_offset_none"

arm_bl_target :: String
arm_bl_target = "Arm_bl_target"

arm_blx_target :: String
arm_blx_target = "Arm_blx_target"

arm_br_target :: String
arm_br_target = "Arm_br_target"

cc_out :: String
cc_out = "Cc_out"

imm0_7 :: String
imm0_7 = "Imm0_7"

imm0_15 :: String
imm0_15 = "Imm0_15"

imm0_31 :: String
imm0_31 = "Imm0_31"

imm0_255 :: String
imm0_255 = "Imm0_255"

imm0_4095 :: String
imm0_4095 = "Imm0_4095"

ldst_so_reg :: String
ldst_so_reg = "Ldst_so_reg"

mod_imm :: String
mod_imm = "Mod_imm"

reglist :: String
reglist = "Reglist"

shift_so_reg_imm :: String
shift_so_reg_imm = "Shift_so_reg_imm"

so_reg_reg :: String
so_reg_reg = "So_reg_reg"

so_reg_imm :: String
so_reg_imm = "So_reg_imm"

t2_so_imm :: String
t2_so_imm = "T2_so_imm"

t2_so_reg :: String
t2_so_reg = "T2_so_reg"

t_addrmode_is2 :: String
t_addrmode_is2 = "T_addrmode_is2"

t_addrmode_is4 :: String
t_addrmode_is4 = "T_addrmode_is4"

t_addrmode_pc :: String
t_addrmode_pc = "T_addrmode_pc"

t_imm0_1020s4 :: String
t_imm0_1020s4 = "T_imm0_1020s4"

t_imm0_508s4 :: String
t_imm0_508s4 = "T_imm0_508s4"

thumb_blx_target :: String
thumb_blx_target = "Thumb_blx_target"

thumb_bcc_target :: String
thumb_bcc_target = "Thumb_bcc_target"

-- | The 'unpredictableInstrBits' operand is used to define bits in
-- the instruction encoding that the architecture specification
-- declares as "unpredictable" and which may have different values
-- (e.g. for future expansions).
--
-- These are declared by Dismantle so that round-trip decode/encode
-- can pass through and reproduce the same values.
--
-- The generated semantics will always ignore these.
unpredictableInstrBits :: String
unpredictableInstrBits = "Unpredictable"


-- ----------------------------------------------------------------------

-- | We need a data wrapper around the 'SymToExprTag' to work around what is
-- known as the "saturation requirement" on type families:
--
-- https://stackoverflow.com/questions/40758738/why-doesnt-this-code-infringe-the-saturation-requirement-of-type-families
data SymToExprTagWrapper :: TL.TyFun k1 k2 -> Type
type instance TL.Apply SymToExprTagWrapper x = SymToExprTag x
type family SymToExprTag (sym :: Symbol) :: ExprTag where
  SymToExprTag "GPR" = 'TBV
  SymToExprTag "GPRnopc" = 'TBV
  SymToExprTag "Mod_imm" = 'TPackedOperand
  SymToExprTag "Imm0_7" = 'TBV
  SymToExprTag "Imm0_15" = 'TBV
  SymToExprTag "Imm0_31" = 'TBV
  SymToExprTag "Imm0_255" = 'TBV
  SymToExprTag "Imm0_4095" = 'TBV
  SymToExprTag "Pred" = 'TBV
  SymToExprTag "Cc_out" = 'TBV
  SymToExprTag "Addrmode_imm12" = 'TPackedOperand
  SymToExprTag "Addrmode_imm12_pre" = 'TPackedOperand
  SymToExprTag "Am2offset_imm" = 'TPackedOperand
  SymToExprTag "Addr_offset_none" = 'TBV
  SymToExprTag "Ldst_so_reg" = 'TPackedOperand
  SymToExprTag "Arm_bl_target" = 'TBV
  SymToExprTag "Arm_blx_target" = 'TBV
  SymToExprTag "Arm_br_target" = 'TBV
  SymToExprTag "Shift_so_reg_imm" = 'TBV
  SymToExprTag "So_reg_reg" = 'TPackedOperand
  SymToExprTag "So_reg_imm" = 'TPackedOperand
  SymToExprTag "Reglist" = 'TPackedOperand
  SymToExprTag "RGPR" = 'TBV
  SymToExprTag "T_addrmode_is2" = 'TPackedOperand
  SymToExprTag "T_addrmode_is4" = 'TPackedOperand
  SymToExprTag "T_addrmode_pc" = 'TPackedOperand
  SymToExprTag "T_imm0_1020s4" = 'TPackedOperand
  SymToExprTag "T_imm0_508s4" = 'TPackedOperand
  SymToExprTag "T2_so_imm" = 'TPackedOperand
  SymToExprTag "T2_so_reg" = 'TPackedOperand
  SymToExprTag "TGPR" = 'TBV
  SymToExprTag "Thumb_bcc_target" = 'TBV
  SymToExprTag "Thumb_blx_target" = 'TPackedOperand
  SymToExprTag "Unpredictable" = 'TBV
