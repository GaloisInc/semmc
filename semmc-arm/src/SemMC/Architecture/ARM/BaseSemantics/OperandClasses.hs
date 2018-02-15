-- | Defines the ARM local operand references (used in the SemMC DSL
-- specification of operands) and the corresponding operand names in
-- Dismantle.

module SemMC.Architecture.ARM.BaseSemantics.OperandClasses
    where

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

addrmode_imm12_pre :: String
addrmode_imm12_pre = "Addrmode_imm12_pre"

am2offset_imm :: String
am2offset_imm = "Am2offset_imm"

addr_offset_none :: String
addr_offset_none = "Addr_offset_none"

arm_blx_target :: String
arm_blx_target = "Arm_blx_target"

cc_out :: String
cc_out = "Cc_out"

mod_imm :: String
mod_imm = "Mod_imm"

so_reg_reg :: String
so_reg_reg = "So_reg_reg"

so_reg_imm :: String
so_reg_imm = "So_reg_imm"

-- ----------------------------------------------------------------------
-- T32 operand names

thumb_blx_target :: String
thumb_blx_target = "ThumbBlxTarget"
