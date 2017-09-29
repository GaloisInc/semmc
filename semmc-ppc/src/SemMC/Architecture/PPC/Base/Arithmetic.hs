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
    let lhs = sext' 64 (lowBits 32 (Loc rA))
    let rhs = sext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    defLoc rT (zext (lowBits64 32 prod))
  defineOpcode "MULHW" $ do
    comment "Multiply High Word (XO-form)"
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
    defLoc rT (zext (highBits64 32 prod))
  defineOpcode "MULHWU" $ do
    comment "Multiply High Word Unsigned (XO-form)"
    (rT, rA, rB) <- xoform3
    let lhs = zext' 64 (lowBits 32 (Loc rA))
    let rhs = zext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    defLoc rT (zext (highBits64 32 prod))
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
  when (?bitSize == Size64) $ do
    -- Not valid in 32 bit mode
    defineOpcode "MULLD" $ do
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      defLoc rT (lowBits128 64 prod)
    defineOpcode "MULHD" $ do
      (rT, rA, rB) <- xoform3
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      defLoc rT (highBits128 64 prod)
    defineOpcode "MULHDU" $ do
      (rT, rA, rB) <- xoform3
      let prod = bvmul (zext' 128 (Loc rA)) (zext' 128 (Loc rB))
      defLoc rT (highBits128 64 prod)
