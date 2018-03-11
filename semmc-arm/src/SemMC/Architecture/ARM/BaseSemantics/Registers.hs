{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Registers
    where

import Data.Word ( Word8 )
import Prelude hiding ( concat )
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.DSL


numGPR :: Word8
numGPR = 16


-- | The width of a register (in bits), which typically also
-- corresponds to the width of memory words for that architecture.
regWidth :: Integer
regWidth = 32  -- # bits


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

itBlockState :: Expr 'TBV
itBlockState =
  concat itstate_7_4 (concat itstate_3_2 itstate_1_0)
  where
    sr = Loc cpsr
    itstate_7_4 = extract 15 12 sr
    itstate_3_2 = extract 11 10 sr
    itstate_1_0 = extract 26 25 sr

-- | Is the current T32 instruction executing in the context of an IT
-- (IfThen) block
inITBlock :: Expr 'TBool
inITBlock = bvne (LitBV 8 0x0) itBlockState


-- | Is this the last entry in a T32 IT (IfThen) block.  At present,
-- semmc itself does not manage the IT block so this is a noop; it may
-- be changed to an uninterpreted function for evaluation.
lastInITBlock :: Expr 'TBool
lastInITBlock = uf EBool "t32.lastInITBlock" []
