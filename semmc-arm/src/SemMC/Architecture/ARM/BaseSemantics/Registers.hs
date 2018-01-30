{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Registers
    where

import SemMC.DSL
import SemMC.Architecture.ARM.BaseSemantics.Natural


-- | The PC (Program Counter).  This is roughly analagous to R15, but
-- there is a distinction between R15 and the PC and while R15 is
-- always read as PC (+offset), some instructions can write a new
-- address there and cause a branch and other instructions can write
-- R15 as if it is a regular register (no branch incurred).  For SemMC
-- purposes, R15 and the PC are treated distinctly, but the
-- instruction semantics definitions will reconcile the two.
pc :: Location 'TBV
pc = LiteralLoc Literal { lName = "PC"
                        , lExprType = naturalBV
                        }

lr :: Location 'TBV
lr = LiteralLoc Literal { lName = "R14"  -- the "Link Register" (e.g. function return PC)
                        , lExprType = naturalBV
                        }

sp :: Location 'TBV
sp = LiteralLoc Literal { lName = "R13"  -- the Stack Pointer
                        , lExprType = naturalBV
                        }

-- | CPSR (Current Program State Register) maintains the condition
-- codes and other execution status registers (see Notes and (G1.9.3,
-- G1-3422).  This is a system register that is only accessible via
-- specific operations and not one of the general purpose registers.
cpsr :: Location 'TBV
cpsr = LiteralLoc Literal { lName = "CPSR"
                          , lExprType = naturalBV
                          }
