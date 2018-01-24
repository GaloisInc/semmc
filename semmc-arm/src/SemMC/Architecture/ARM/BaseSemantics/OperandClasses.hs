-- | Defines the local operand references (used in the SemMC DSL
-- specification of operands) and the corresponding operand names in
-- Dismantle.

module SemMC.Architecture.ARM.BaseSemantics.OperandClasses
    where

gpr :: String
gpr = "GPR"

memri :: String
memri = "Memri"

-- | pred is the 4-bit condition value that indicates whether an instruction will be executed.
pred :: String
pred = "Pred"

addrmode_imm12_pre :: String
addrmode_imm12_pre = "Addrmode_imm12_pre"
