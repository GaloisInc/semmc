{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Exception ( assert )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core


-- Defs

base :: BitSize -> [(String, Definition)]
base bitSize = runSem $ do
  defineOpcode "ADD4" $ do
    (rT, rA, rB) <- xoform3
    defLoc (ParamLoc rT) (bvadd (Param rA) (Param rB))
  defineOpcode "SUBF" $ do
    (rT, rA, rB) <- xoform3
    defLoc (ParamLoc rT) (bvsub (Param rB) (Param rA))
  defineOpcode "NEG" $ do
    rT <- param "rT" gprc
    rA <- param "rA" gprc
    input rA
    defLoc (ParamLoc rT) (bvadd (bvnot (Param rA)) (LitBV (bitSizeValue bitSize) 0x1))
  defineOpcode "MULLW" $ do
    (rT, rA, rB) <- xoform3
    -- FIXME: What does this do on PPC32?  Does it just save the low 32 bits of
    -- the answer, and the caller is responsible for calling MULHW?
    defLoc (ParamLoc rT) (bvmul (lowBits bitSize 32 (Param rA)) (lowBits bitSize 32 (Param rB)))
  defineOpcode "MULHW" $ do
    (rT, rA, rB) <- xoform3
    -- FIXME: Extend to 64 bit bitvectors
    defLoc (ParamLoc rT) (bvmul (highBits bitSize 32 (Param rA)) (highBits bitSize 32 (Param rB)))
  when (bitSize == Size64) $ do
    -- Not valid in 32 bit mode
    defineOpcode "MULLD" $ do
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 64 (Param rA)) (sext' 128 64 (Param rB))
      defLoc (ParamLoc rT) (lowBits128 64 prod)
    defineOpcode "MULHD" $ do
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 64 (Param rA)) (sext' 128 64 (Param rB))
      defLoc (ParamLoc rT) (highBits128 64 prod)
  defineOpcode "XOR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvxor (Param rS) (Param rB))
  defineOpcode "OR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvor (Param rS) (Param rB))
  defineOpcode "AND" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvand (Param rS) (Param rB))
  defineOpcode "NAND" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvand (Param rS) (Param rB)))
  defineOpcode "NOR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvor (Param rS) (Param rB)))
  defineOpcode "EQV" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvxor (Param rS) (Param rB)))
  defineOpcode "ANDC" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvand (Param rS) (bvnot (Param rB)))
  defineOpcode "ORC" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvor (Param rS) (bvnot (Param rB)))
  defineOpcode "SLD" $ do
    comment "Shift Left Doubleword (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 6 (Param rB)
    defLoc (ParamLoc rA)  (bvshl (Param rS) n)
  defineOpcode "SLW" $ do
    comment "Shift Left Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 5 (Param rB)
    defLoc (ParamLoc rA) (maskHigh32 (bvshl (Param rS) n))
  defineOpcode "SRD" $ do
    comment "Shift Right Doubleword (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 6 (Param rB)
    defLoc (ParamLoc rA) (bvlshr (Param rS) n)
  defineOpcode "SRW" $ do
    comment "Shift Right Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 5 (Param rB)
    defLoc (ParamLoc rA) (maskHigh32 (bvlshr (Param rS) n))
  defineOpcode "ADDI" $ do
    comment "Add Immediate (D-form)"
    comment "We hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    (rT, rA, si) <- dform
    let lhs = ite (isR0 (Param rA)) (LitBV (bitSizeValue bitSize) 0x0) (Param rA)
    defLoc (ParamLoc rT) (bvadd lhs (sext bitSize 16 (Param si)))
  defineOpcode "ADDIS" $ do
    comment "Add Immediate Shifted (D-form)"
    comment "Like 'ADDI', we hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    (rT, rA, si) <- dform
    let lhs = ite (isR0 (Param rA)) (LitBV (bitSizeValue bitSize) 0x0) (Param rA)
    let imm = concat (Param si) (LitBV 16 0x0)
    defLoc (ParamLoc rT) (bvadd lhs (sext bitSize 32 imm))
  defineOpcode "CMPDI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the double word variant (where L=1)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    inputLiteral cr
    let ximm = sext bitSize 16 (Param imm)
    let newCR = cmpImm bvslt bvsgt (Param fld) ximm (Param rA)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPWI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    inputLiteral cr
    let ximm = sext bitSize 16 (Param imm)
    let lowreg = lowBits bitSize 32 (Param rA)
    let newCR = cmpImm bvslt bvsgt (Param fld) ximm (sext bitSize 32 lowreg)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPD" $ do
    comment "Compare (X-form)"
    comment "Compare double word where L=1"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    inputLiteral cr
    let newCR = cmpImm bvslt bvsgt (Param fld) (Param rA) (Param rB)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLW" $ do
    comment "Compare (X-form)"
    comment "Compare word (where L=0)"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    inputLiteral cr
    let lowa = lowBits bitSize 32 (Param rA)
    let lowb = lowBits bitSize 32 (Param rB)
    let newCR = cmpImm bvslt bvsgt (Param fld) (zext bitSize 32 lowa) (zext bitSize 32 lowb)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLDI" $ do
    comment "Compare Logical Immediate (D-form)"
    comment "This variant is the double word variant (where L=1)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    inputLiteral cr
    let ximm = zext bitSize 16 (Param imm)
    let newCR = cmpImm bvult bvugt (Param fld) ximm (Param rA)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLWI" $ do
    comment "Compare Logical Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    inputLiteral cr
    let ximm = zext bitSize 16 (Param imm)
    let lowreg = lowBits bitSize 32 (Param rA)
    let newCR = cmpImm bvult bvugt (Param fld) ximm (zext bitSize 32 lowreg)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLD" $ do
    comment "Compare Logical (X-form)"
    comment "Compare double word (where L=1)"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    inputLiteral cr
    let newCR = cmpImm bvult bvugt (Param fld) (Param rA) (Param rB)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLW" $ do
    comment "Compare Logical (X-form)"
    comment "Compare word (where L=0)"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    inputLiteral cr
    let lowa = lowBits bitSize 32 (Param rA)
    let lowb = lowBits bitSize 32 (Param rB)
    let newCR = cmpImm bvult bvugt (Param fld) (zext bitSize 32 lowa) (zext bitSize 32 lowb)
    defLoc (LiteralLoc cr) newCR
  return ()

pseudo :: BitSize -> [(String, Definition)]
pseudo bitSize = runSem $ do
  defineOpcode "Move" $ do
    target <- param "target" gprc
    source <- param "source" gprc_nor0
    input source
    defLoc (ParamLoc target) (Param source)
  defineOpcode "ExtractByteGPR" $ do
    target <- param "target" gprc
    source <- param "source" gprc
    n <- if bitSize == Size32 then param "n" u2imm else param "n" u4imm
    input source
    input n
    let shiftAmount = bvshl (Param n) (LitBV (bitSizeValue bitSize) 0x3)
    let shiftedInput = bvlshr (Param source) shiftAmount
    let bits = lowBits bitSize 8 shiftedInput
    let padding = LitBV (bitSizeValue bitSize - 8) 0x0
    defLoc (ParamLoc target) (concat padding bits)
  defineOpcode "SetSignedCR0" $ do
    comment "SetCR0"
    comment "This pseudo-opcode sets the value of CR0 based on a comparison"
    comment "of the value in the input register against zero, as in CMPDI or CMPWI"
    rA <- param "rA" g8rc
    inputLiteral cr
    inputLiteral xer
    input rA
    let ximm = LitBV (bitSizeValue bitSize) 0x0
    let newCR = cmpImm bvslt bvsgt (LitBV 3 0) ximm (Param rA)
    defLoc (LiteralLoc cr) newCR
  return ()

manual :: BitSize -> [(String, Definition)]
manual bitSize = runSem $ do
  defineOpcode "MTLR" $ do
    rA <- param "rA" gprc
    input rA
    defLoc (LiteralLoc lnk) (Param rA)
  defineOpcode "MFLR" $ do
    rA <- param "rA" gprc
    defLoc (ParamLoc rA) (Loc lnk)
  defineOpcode "MTCTR" $ do
    rA <- param "rA" gprc
    input rA
    defLoc (LiteralLoc ctr) (Param rA)
  defineOpcode "MFCTR" $ do
    rA <- param "rA" gprc
    defLoc (ParamLoc rA) (Loc ctr)
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
