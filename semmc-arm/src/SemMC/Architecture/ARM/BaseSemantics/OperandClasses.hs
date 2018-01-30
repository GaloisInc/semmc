-- | Defines the ARM local operand references (used in the SemMC DSL
-- specification of operands) and the corresponding operand names in
-- Dismantle.

module SemMC.Architecture.ARM.BaseSemantics.OperandClasses
    where

gpr :: String
gpr = "GPR"

-- ----------------------------------------------------------------------
-- A32 operand names

memri :: String
memri = "Memri"

-- | pred is the 4-bit condition value that indicates whether an instruction will be executed.
pred :: String
pred = "Pred"

addrmode_imm12_pre :: String
addrmode_imm12_pre = "Addrmode_imm12_pre"

arm_blx_target :: String
arm_blx_target = "Arm_blx_target"

cc_out :: String
cc_out = "cc_out"

mod_imm :: String
mod_imm = "mod_imm"

-- ----------------------------------------------------------------------
-- T32 operand names

thumb_blx_target :: String
thumb_blx_target = "ThumbBlxTarget"
