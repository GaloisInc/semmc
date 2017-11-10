{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Arithmetic (
  baseArithmetic
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

baseArithmetic :: (?bitSize :: BitSize) => SemM 'Top ()
baseArithmetic = do
  defineOpcodeWithIP "ADD4" $ do
    comment "ADD (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    let val = bvadd (Loc rA) (Loc rB)
    defLoc rT val
    defineRCVariant "ADD4o" val $ do
      comment "ADD. (XO-form, RC=1)"
  defineOpcodeWithIP "SUBF" $ do
    comment "SUBF (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    let val = bvsub (Loc rB) (Loc rA)
    defLoc rT val
    defineRCVariant "SUBFo" val $ do
      comment "SUBF. (XO-form, RC=1)"
  defineOpcodeWithIP "NEG" $ do
    comment "Negate (XO-form, RC=0)"
    rT <- param "rT" gprc naturalBV
    rA <- param "rA" gprc naturalBV
    input rA
    let res = bvadd (bvnot (Loc rA)) (naturalLitBV 0x1)
    defLoc rT res
    defineRCVariant "NEGo" res $ do
      comment "Negate (XO-form, RC=1)"
  defineOpcodeWithIP "MULLI" $ do
    comment "Multiply Low Immediate (D-form)"
    (rT, rA, si) <- dformr0
    let prod = bvmul (Loc rA) (sext (Loc si))
    defLoc rT prod
  defineOpcodeWithIP "MULLW" $ do
    comment "Multiply Low Word (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    let lhs = sext' 64 (lowBits 32 (Loc rA))
    let rhs = sext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    let res = zext (lowBits64 32 prod)
    defLoc rT res
    defineRCVariant "MULLWo" res $ do
      comment "Multiply Low Word (XO-form, RC=1)"
  defineOpcodeWithIP "MULHW" $ do
    comment "Multiply High Word (XO-form, RC=0)"
    comment "Multiply the low 32 bits of two registers, producing a 64 bit result."
    comment "Save the high 32 bits of the result into the output register"
    (rT, rA, rB) <- xoform3
    -- This is a high-word multiply, so we always need to perform it at 64 bits.
    -- Then we just take the high 32 bits of the result as our answer.
    let lhs = sext' 64 (lowBits 32 (Loc rA))
    let rhs = sext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    -- Now we have to extract the high word (and store it in the low 32 bits of
    -- the output register)
    --
    -- NOTE: the high bits are technically undefined.  How do we want to
    -- represent that?
    let res = zext (highBits64 32 prod)
    defLoc rT res
    defineRCVariant "MULHWo" res $ do
      comment "Multiply High Word (XO-form, RC=1)"
  defineOpcodeWithIP "MULHWU" $ do
    comment "Multiply High Word Unsigned (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    let lhs = zext' 64 (lowBits 32 (Loc rA))
    let rhs = zext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    let res = zext (highBits64 32 prod)
    defLoc rT res
    defineRCVariant "MULHWUo" res $ do
      comment "Multiply High Word Unsigned (XO-form, RC=1)"
  defineOpcodeWithIP "ADDI" $ do
    comment "Add Immediate (D-form)"
    comment "We hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    rT <- param "rT" gprc naturalBV
    si <- param "si" s16imm (EBV 16)
    rA <- param "rA" gprc_nor0 naturalBV
    input rA
    input si
    let lhs = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    defLoc rT (bvadd lhs (sext (Loc si)))
  defineOpcodeWithIP "ADDIS" $ do
    comment "Add Immediate Shifted (D-form)"
    comment "Like 'ADDI', we hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    rT <- param "rT" gprc naturalBV
    si <- param "si" s17imm (EBV 16)
    rA <- param "rA" gprc_nor0 naturalBV
    input rA
    input si
    let lhs = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let imm = concat (Loc si) (LitBV 16 0x0)
    defLoc rT (bvadd lhs (sext imm))

  defineOpcodeWithIP "ADDC" $ do
    comment "Add Carrying (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    input xer
    let len = bitSizeValue ?bitSize
    let eres = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (Loc rB))
    let res = lowBits' len eres
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres))
    defineRCVariant "ADDCo" res $ do
      comment "Add Carrying (XO-form, RC=1)"
  defineOpcodeWithIP "SUBFC" $ do
    comment "Subtract From Carrying (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (bvnot (zext' (len + 1) (Loc rA))) (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (LitBV (len + 1) 0x1)
    let res = lowBits' len eres1
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres1))
    defineRCVariant "SUBFCo" res $ do
      comment "Subtract From Carrying (XO-form, RC=1)"
  defineOpcodeWithIP "ADDE" $ do
    comment "Add Extended (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (zext' (len + 1) (xerBit CA (Loc xer)))
    let res = lowBits' len eres1
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres1))
    defineRCVariant "ADDEo" res $ do
      comment "Add Extended (XO-form, RC=1)"
  defineOpcodeWithIP "SUBFE" $ do
    comment "Subtract From Extended (XO-form, RC=0)"
    (rT, rA, rB) <- xoform3
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (bvnot (zext' (len + 1) (Loc rA))) (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (zext' (len + 1) (xerBit CA (Loc xer)))
    let res = lowBits' len eres1
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres1))
    defineRCVariant "SUBFEo" res $ do
      comment "Subtract From Extended (XO-form, RC=1)"

  when (?bitSize == Size64) $ do
    -- Not valid in 32 bit mode
    defineOpcodeWithIP "MULLD" $ do
      comment "Multiply Low Doubleword (XO-form, RC=0)"
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      let res = lowBits128 64 prod
      defLoc rT res
      defineRCVariant "MULLDo" res $ do
        comment "Multiply Low Doubleword (XO-form, RC=1)"
    defineOpcodeWithIP "MULHD" $ do
      comment "Multiply High Doubleword (XO-form, RC=0)"
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      let res = highBits128 64 prod
      defLoc rT res
      defineRCVariant "MULHDo" res $ do
        comment "Multiply High Doubleword (XO-form, RC=1)"
    defineOpcodeWithIP "MULHDU" $ do
      comment "Multiply High Doubleword Unsigned (XO-form, RC=0)"
      (rT, rA, rB) <- xoform3
      let prod = bvmul (zext' 128 (Loc rA)) (zext' 128 (Loc rB))
      let res = highBits128 64 prod
      defLoc rT res
      defineRCVariant "MULHDUo" res $ do
        comment "Multiply High Doubleword Unsigned (XO-form, RC=1)"
