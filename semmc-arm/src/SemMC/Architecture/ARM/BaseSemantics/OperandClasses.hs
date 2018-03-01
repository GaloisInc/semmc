-- | Defines the ARM local operand references (used in the SemMC DSL
-- specification of operands) and the corresponding operand names in
-- Dismantle.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module SemMC.Architecture.ARM.BaseSemantics.OperandClasses
    where

import qualified Data.Type.List as TL
import           GHC.TypeLits ( Symbol )
import           SemMC.DSL


gpr :: String
gpr = "GPR"

tgpr :: String
tgpr = "TGPR"

-- ----------------------------------------------------------------------
-- A32 operand names

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

ldst_so_reg :: String
ldst_so_reg = "Ldst_so_reg"

cc_out :: String
cc_out = "Cc_out"

mod_imm :: String
mod_imm = "Mod_imm"

shift_so_reg_imm :: String
shift_so_reg_imm = "Shift_so_reg_imm"

so_reg_reg :: String
so_reg_reg = "So_reg_reg"

so_reg_imm :: String
so_reg_imm = "So_reg_imm"

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
-- T32 operand names

thumb_blx_target :: String
thumb_blx_target = "ThumbBlxTarget"


-- ----------------------------------------------------------------------

-- | We need a data wrapper around the 'SymToExprTag' to work around what is
-- known as the "saturation requirement" on type families:
--
-- https://stackoverflow.com/questions/40758738/why-doesnt-this-code-infringe-the-saturation-requirement-of-type-families
data SymToExprTagWrapper :: TL.TyFun k1 k2 -> *
type instance TL.Apply SymToExprTagWrapper x = SymToExprTag x
type family SymToExprTag (sym :: Symbol) :: ExprTag where
  SymToExprTag "GPR" = 'TBV
  SymToExprTag "Mod_imm" = 'TBV
  SymToExprTag "Pred" = 'TBV
  SymToExprTag "Cc_out" = 'TBV
  SymToExprTag "Addrmode_imm12" = 'TMemRef
  SymToExprTag "Addrmode_imm12_pre" = 'TMemRef
  SymToExprTag "Am2offset_imm" = 'TMemRef
  SymToExprTag "Addr_offset_none" = 'TBV
  SymToExprTag "Ldst_so_reg" = 'TMemRef
  SymToExprTag "Arm_bl_target" = 'TBV
  SymToExprTag "Arm_blx_target" = 'TBV
  SymToExprTag "Arm_br_target" = 'TBV
  SymToExprTag "Shift_so_reg_imm" = 'TBV
  SymToExprTag "So_reg_reg" = 'TMemRef
  SymToExprTag "So_reg_imm" = 'TMemRef
  SymToExprTag "RGPR" = 'TBV
  SymToExprTag "TGPR" = 'TBV
  SymToExprTag "Thumb_blx_target" = 'TBV
  SymToExprTag "Unpredictable" = 'TBV
