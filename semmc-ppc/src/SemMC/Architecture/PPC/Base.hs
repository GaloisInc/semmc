{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
-- | The definitions of the base and manual sets of formulas
--
-- This is the set of definitions shared between PPC32 and PPC64
--
-- base and manual are specified separately so that we can store them in
-- different places (manual definitions are not included in the base set, since
-- we don't want to or cannot learn from them).
module SemMC.Architecture.PPC.Base (
  BitSize(..),
  base,
  pseudo,
  manual
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core


-- Defs

base :: BitSize -> [(String, Definition)]
base bitSize = runSem $ do
  let ?bitSize = bitSize
  defineOpcode "ADD4" $ do
    (rT, rA, rB) <- xoform3
    defLoc rT (bvadd (Loc rA) (Loc rB))
  defineOpcode "SUBF" $ do
    (rT, rA, rB) <- xoform3
    defLoc rT (bvsub (Loc rB) (Loc rA))
  defineOpcode "NEG" $ do
    rT <- param "rT" gprc naturalBV
    rA <- param "rA" gprc naturalBV
    input rA
    defLoc rT (bvadd (bvnot (Loc rA)) (naturalLitBV 0x1))
  defineOpcode "MULLW" $ do
    (rT, rA, rB) <- xoform3
    let lhs = sext (lowBits 32 (Loc rA))
    let rhs = sext (lowBits 32 (Loc rB))
    defLoc rT (bvmul lhs rhs)
  defineOpcode "MULHW" $ do
    (rT, rA, rB) <- xoform3
    let lhs = sext (highBits 32 (Loc rA))
    let rhs = sext (highBits 32 (Loc rB))
    defLoc rT (bvmul lhs rhs)
  when (bitSize == Size64) $ do
    -- Not valid in 32 bit mode
    defineOpcode "MULLD" $ do
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      defLoc rT (lowBits128 64 prod)
    defineOpcode "MULHD" $ do
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      defLoc rT (highBits128 64 prod)
  defineOpcode "XOR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvxor (Loc rS) (Loc rB))
  defineOpcode "OR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvor (Loc rS) (Loc rB))
  defineOpcode "AND" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvand (Loc rS) (Loc rB))
  defineOpcode "NAND" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvand (Loc rS) (Loc rB)))
  defineOpcode "NOR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvor (Loc rS) (Loc rB)))
  defineOpcode "EQV" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvxor (Loc rS) (Loc rB)))
  defineOpcode "ANDC" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvand (Loc rS) (bvnot (Loc rB)))
  defineOpcode "ORC" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvor (Loc rS) (bvnot (Loc rB)))
  defineOpcode "SLW" $ do
    comment "Shift Left Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    defLoc rA (zext (bvshl w n))
  defineOpcode "SRW" $ do
    comment "Shift Right Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    defLoc rA (zext (bvlshr w n))
  when (bitSize == Size64) $ do
    defineOpcode "SLD" $ do
      comment "Shift Left Doubleword (X-form)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits 6 (Loc rB))
      defLoc rA  (bvshl (Loc rS) n)
    defineOpcode "SRD" $ do
      comment "Shift Right Doubleword (X-form)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits64 6 (Loc rB))
      defLoc rA (bvlshr (Loc rS) n)
  defineOpcode "ADDI" $ do
    comment "Add Immediate (D-form)"
    comment "We hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    (rT, rA, si) <- dform
    let lhs = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    defLoc rT (bvadd lhs (sext (Loc si)))
  defineOpcode "ADDIS" $ do
    comment "Add Immediate Shifted (D-form)"
    comment "Like 'ADDI', we hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    (rT, rA, si) <- dform
    let lhs = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let imm = concat (Loc si) (LitBV 16 0x0)
    defLoc rT (bvadd lhs (sext imm))
  defineOpcode "CMPDI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the double word variant (where L=1)"
    fld <- param "fld" crrc (EBV 3)
    imm <- param "imm" s16imm (EBV 16)
    rA <- param "rA" gprc naturalBV
    input imm
    input rA
    input cr
    let ximm = sext (Loc imm)
    let newCR = cmpImm bvslt bvsgt (Loc fld) ximm (Loc rA)
    defLoc cr newCR
  defineOpcode "CMPWI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    imm <- param "imm" s16imm (EBV 16)
    rA <- param "rA" gprc naturalBV
    input imm
    input rA
    input cr
    let ximm = sext (Loc imm)
    let lowreg = lowBits 32 (Loc rA)
    let newCR = cmpImm bvslt bvsgt (Loc fld) ximm (sext lowreg)
    defLoc cr newCR
  defineOpcode "CMPD" $ do
    comment "Compare (X-form)"
    comment "Compare double word where L=1"
    fld <- param "fld" crrc (EBV 3)
    rA <- param "rA" gprc naturalBV
    rB <- param "rB" gprc naturalBV
    input rA
    input rB
    input cr
    let newCR = cmpImm bvslt bvsgt (Loc fld) (Loc rA) (Loc rB)
    defLoc cr newCR
  defineOpcode "CMPLW" $ do
    comment "Compare (X-form)"
    comment "Compare word (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    rA <- param "rA" gprc naturalBV
    rB <- param "rB" gprc naturalBV
    input rA
    input rB
    input cr
    let lowa = lowBits 32 (Loc rA)
    let lowb = lowBits 32 (Loc rB)
    let newCR = cmpImm bvslt bvsgt (Loc fld) (zext lowa) (zext lowb)
    defLoc cr newCR
  defineOpcode "CMPLDI" $ do
    comment "Compare Logical Immediate (D-form)"
    comment "This variant is the double word variant (where L=1)"
    fld <- param "fld" crrc (EBV 3)
    imm <- param "imm" s16imm (EBV 16)
    rA <- param "rA" gprc naturalBV
    input imm
    input rA
    input cr
    let ximm = zext (Loc imm)
    let newCR = cmpImm bvult bvugt (Loc fld) ximm (Loc rA)
    defLoc cr newCR
  defineOpcode "CMPLWI" $ do
    comment "Compare Logical Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    imm <- param "imm" s16imm (EBV 16)
    rA <- param "rA" gprc naturalBV
    input imm
    input rA
    input cr
    let ximm = zext (Loc imm)
    let lowreg = lowBits 32 (Loc rA)
    let newCR = cmpImm bvult bvugt (Loc fld) ximm (zext lowreg)
    defLoc cr newCR
  defineOpcode "CMPLD" $ do
    comment "Compare Logical (X-form)"
    comment "Compare double word (where L=1)"
    fld <- param "fld" crrc (EBV 3)
    rA <- param "rA" gprc naturalBV
    rB <- param "rB" gprc naturalBV
    input rA
    input rB
    input cr
    let newCR = cmpImm bvult bvugt (Loc fld) (Loc rA) (Loc rB)
    defLoc cr newCR
  defineOpcode "CMPLW" $ do
    comment "Compare Logical (X-form)"
    comment "Compare word (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    rA <- param "rA" gprc naturalBV
    rB <- param "rB" gprc naturalBV
    input rA
    input rB
    input cr
    let lowa = lowBits 32 (Loc rA)
    let lowb = lowBits 32 (Loc rB)
    let newCR = cmpImm bvult bvugt (Loc fld) (zext lowa) (zext lowb)
    defLoc cr newCR
  return ()

pseudo :: BitSize -> [(String, Definition)]
pseudo bitSize = runSem $ do
  let ?bitSize = bitSize
  defineOpcode "Move" $ do
    target <- param "target" gprc naturalBV
    source <- param "source" gprc_nor0 naturalBV
    input source
    defLoc target (Loc source)
  defineOpcode "ExtractByteGPR" $ do
    target <- param "target" gprc naturalBV
    source <- param "source" gprc naturalBV
    n <- if bitSize == Size32 then param "n" u2imm (EBV 2) else param "n" u4imm (EBV 4)
    input source
    input n
    let shiftAmount = bvshl (zext (Loc n)) (naturalLitBV 0x3)
    let shiftedInput = bvlshr (Loc source) shiftAmount
    let bits = lowBits 8 shiftedInput
    let padding = LitBV (bitSizeValue bitSize - 8) 0x0
    defLoc target (concat padding bits)
  defineOpcode "SetSignedCR0" $ do
    comment "SetCR0"
    comment "This pseudo-opcode sets the value of CR0 based on a comparison"
    comment "of the value in the input register against zero, as in CMPDI or CMPWI"
    rA <- param "rA" g8rc naturalBV
    input cr
    input xer
    input rA
    let ximm = naturalLitBV 0x0
    let newCR = cmpImm bvslt bvsgt (LitBV 3 0x0) ximm (Loc rA)
    defLoc cr newCR
  return ()

manual :: BitSize -> [(String, Definition)]
manual bitSize = runSem $ do
  let ?bitSize = bitSize
  defineOpcode "MTLR" $ do
    rA <- param "rA" gprc naturalBV
    input rA
    defLoc lnk (Loc rA)
  defineOpcode "MFLR" $ do
    rA <- param "rA" gprc naturalBV
    defLoc rA (Loc lnk)
  defineOpcode "MTCTR" $ do
    rA <- param "rA" gprc naturalBV
    input rA
    defLoc ctr (Loc rA)
  defineOpcode "MFCTR" $ do
    rA <- param "rA" gprc naturalBV
    defLoc rA (Loc ctr)
  -- defineOpcode "LD" do
  --   rT <- param "rT" gprc
  --   memRef <- param "memRef" memrix
  --   let rA = memrixReg (Param memRef)
  --   let ds = memrixOffset (Param memRef)
  --   let b = ite (isR0 rA) (LitBV bitSize 0) rA
  --   let ea = bvadd b (sext bitSize 16 (concat ds (LitBV 2 0)))
  --   defLoc (ParamLoc rT) (Loc memory)
  return ()


{- Note [PPC Condition Register (CR)]

The CR is 32 bits, which are usually addressed as eight 4-bit fields (CR0-CR7).

 - CR0 is set as the implicit result of many fixed point operations
 - CR1 is set as the implicit result of many floating point operations
 - CR fields can be set as the result of various compare instructions

Individual field bits are assigned based on comparison of the result of an
operation (R) to a constant (C) (either via the compare instructions *or*
against 0 when CR0 is implicitly set) as follows:

| Bit | Name | Description               |
|-----+------+---------------------------|
|   0 | LT   | True if R < C             |
|   1 | GT   | True if R > C             |
|   2 | EQ   | True if R = C             |
|   3 | SO   | Summary Overflow from XER |

If we write out the semantics for the basic compare instructions, we should be
in pretty good shape.  We can then write a specialized compare instruction as an
intrinsic with its constant fixed to zero.  That would let us learn most of the
dotted variants.

-}
