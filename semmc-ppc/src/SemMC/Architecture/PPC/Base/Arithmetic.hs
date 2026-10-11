{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Arithmetic (
  baseArithmetic
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )

import qualified Dismantle.PPC.Opcodes as P

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

-- | Update @XER[SO]@ and @XER[OV]@ bits for adding @in1@ and @in2@.
--
-- The @XER[OV]@ bit gets set if @add in1 in2@ signed overflows.
--
-- The @XER[SO]@ bit get set if it was already set, or if @XER[OV]@
-- gets set.
updateXEROVSOForAdd :: (?bitSize :: BitSize)
                    => Expr 'TBV
                    -> Expr 'TBV
                    -> Expr 'TBV
                    -> Expr 'TBV
updateXEROVSOForAdd xer0 in1 in2 =
  let out = bvadd in1 in2
      zero = naturalLitBV 0
      -- Check for signed overflow. Note that grouping numbers into
      -- negative and non-negative here, instead of positive and
      -- non-positive, is important: @MIN_INT + MIN_INT == 0@, but
      -- @MAX_INT + MAX_INT < 0@. Our definition of signed overflow
      -- is: both inputs are in the same group, but the output is in
      -- not in that group.
      --
      -- The PPC ISA manual defines signed overflow as "the carry out
      -- bit of the most significant and second most sigificant bits
      -- differ", but that seems much harder to calculate, and I
      -- believe it's equivalent to our definition here.
      ov = bvPredToBit $ cases
        [ ( bvslt in1 zero `andp` bvslt in2 zero
          , notp (bvslt out zero) )
        , ( bvsge in1 zero `andp` bvsge in2 zero
          , notp (bvsge out zero) ) ]
        (LitBool False)
      so = xerBit SO xer0 `bvor` ov
  in updateXER SO (updateXER OV xer0 ov) so

-- | Update @XER[SO]@ and @XER[OV]@ for an addition with carry-in.
updateXEROVSOForAddWithCarry :: (?bitSize :: BitSize)
                            => Expr 'TBV
                            -> Expr 'TBV
                            -> Expr 'TBV
                            -> Expr 'TBV
                            -> Expr 'TBV
updateXEROVSOForAddWithCarry xer0 in1 in2 carry =
  let len = bitSizeValue ?bitSize
      wide0 = bvadd (sext' (len + 2) in1) (sext' (len + 2) in2)
      wide = bvadd wide0 (zext' (len + 2) carry)
      result = lowBits' len wide
      overflow = notp (bveq wide (sext' (len + 2) result))
  in updateXEROVSO xer0 overflow

-- | Update @XER[SO]@ and @XER[OV]@ for subtraction with carry-in.
--
-- PPC extended subtraction computes @minuend - subtrahend + carry - 1@.
updateXEROVSOForSubtractWithCarry :: (?bitSize :: BitSize)
                                 => Expr 'TBV
                                 -> Expr 'TBV
                                 -> Expr 'TBV
                                 -> Expr 'TBV
                                 -> Expr 'TBV
updateXEROVSOForSubtractWithCarry xer0 minuend subtrahend carry =
  let len = bitSizeValue ?bitSize
      wide0 = bvsub
        (sext' (len + 2) minuend)
        (sext' (len + 2) subtrahend)
      wide1 = bvadd wide0 (zext' (len + 2) carry)
      wide = bvsub wide1 (LitBV (len + 2) 1)
      result = lowBits' len wide
      overflow = notp (bveq wide (sext' (len + 2) result))
  in updateXEROVSO xer0 overflow

-- | Set @XER[OV]@ for a division overflow and make @XER[SO]@ sticky.
updateXEROVSO :: (?bitSize :: BitSize)
              => Expr 'TBV
              -> Expr 'TBool
              -> Expr 'TBV
updateXEROVSO xer0 overflow =
  let ov = bvPredToBit overflow
      so = xerBit SO xer0 `bvor` ov
  in updateXER SO (updateXER OV xer0 ov) so

baseArithmetic :: (?bitSize :: BitSize) => SemM 'Top ()
baseArithmetic = do
  definePPCOpcode P.ADD4 xoform3c $ \rT rB rA -> do
    comment "ADD (XO-form, OE=0, RC=0)"
    let val = bvadd (Loc rA) (Loc rB)
    defLoc rT val
    definePPCOpcodeRC P.ADD4o val $ do
      comment "ADD. (XO-form, OE=0, RC=1)"
    let modifiedXer = updateXEROVSOForAdd (Loc xer) (Loc rA) (Loc rB)
    defineOERCVariant P.ADD4Oo val modifiedXer $ do
      comment "ADDO. (XO-form, OE=1, RC=1)"
  definePPCOpcode P.ADD4OE xoform3c $ \rT rB rA -> do
    comment "ADDO (XO-form, OE=1, RC=0)"
    let val = bvadd (Loc rA) (Loc rB)
    let modifiedXer = updateXEROVSOForAdd (Loc xer) (Loc rA) (Loc rB)
    input xer
    defLoc rT val
    defLoc xer modifiedXer
  definePPCOpcode P.SUBF xoform3c $ \rT rB rA -> do
    comment "SUBF (XO-form, RC=0)"
    let val = bvsub (Loc rB) (Loc rA)
    defLoc rT val
    definePPCOpcodeRC P.SUBFo val $ do
      comment "SUBF. (XO-form, RC=1)"
  definePPCOpcode P.SUBFOE xoform3c $ \rT rB rA -> do
    comment "SUBFO (XO-form, OE=1, RC=0)"
    let val = bvsub (Loc rB) (Loc rA)
    let zero = naturalLitBV 0
    let overflow =
          (bvslt (Loc rA) zero `andp` bvsge (Loc rB) zero `andp`
           bvslt val zero) `orp`
          (bvsge (Loc rA) zero `andp` bvslt (Loc rB) zero `andp`
           bvsge val zero)
    let modifiedXer = updateXEROVSO (Loc xer) overflow
    defLoc rT val
    defineOERCVariant P.SUBFOEo val modifiedXer $ do
      comment "SUBFO. (XO-form, OE=1, RC=1)"
    input xer
    defLoc xer modifiedXer
  defineOpcodeWithIP "NEG" $ do
    comment "Negate (XO-form, RC=0)"
    rT <- param "rT" gprc naturalBV
    rA <- param "rA" gprc naturalBV
    input rA
    let res = bvadd (bvnot (Loc rA)) (naturalLitBV 0x1)
    defLoc rT res
    defineRCVariant "NEGo" res $ do
      comment "Negate (XO-form, RC=1)"
  definePPCOpcode P.NEGOE xoform2c $ \rT rA -> do
    comment "Negate with Overflow Enabled (XO-form, OE=1, RC=0)"
    let res = bvadd (bvnot (Loc rA)) (naturalLitBV 0x1)
    let zero = naturalLitBV 0
    let overflow = bvslt (Loc rA) zero `andp` bveq res (Loc rA)
    let modifiedXer = updateXEROVSO (Loc xer) overflow
    defLoc rT res
    defineOERCVariant P.NEGOEo res modifiedXer $ do
      comment "Negate with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    input xer
    defLoc xer modifiedXer
  definePPCOpcode P.MULLI dformr0c $ \rT si rA -> do
    comment "Multiply Low Immediate (D-form)"
    let prod = bvmul (Loc rA) (sext (Loc si))
    defLoc rT prod
  definePPCOpcode P.MULLW xoform3c $ \rT rB rA -> do
    comment "Multiply Low Word (XO-form, RC=0)"
    let lhs = sext' 64 (lowBits 32 (Loc rA))
    let rhs = sext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    let res = zext (lowBits64 32 prod)
    defLoc rT res
    definePPCOpcodeRC P.MULLWo res $ do
      comment "Multiply Low Word (XO-form, RC=1)"
  definePPCOpcode P.MULLWOE xoform3c $ \rT rB rA -> do
    comment "Multiply Low Word with Overflow Enabled (XO-form, OE=1, RC=0)"
    let lhs = sext' 64 (lowBits 32 (Loc rA))
    let rhs = sext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    let lowProd = lowBits64 32 prod
    let res = sext lowProd
    let overflow = notp (bveq prod (sext' 64 lowProd))
    let modifiedXer = updateXEROVSO (Loc xer) overflow
    defLoc rT res
    defineOERCVariant P.MULLWOEo res modifiedXer $ do
      comment "Multiply Low Word with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    input xer
    defLoc xer modifiedXer
  definePPCOpcode P.MULHW xoform3c $ \rT rB rA -> do
    comment "Multiply High Word (XO-form, RC=0)"
    comment "Multiply the low 32 bits of two registers, producing a 64 bit result."
    comment "Save the high 32 bits of the result into the output register"
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
    definePPCOpcodeRC P.MULHWo res $ do
      comment "Multiply High Word (XO-form, RC=1)"
  definePPCOpcode P.MULHWU xoform3c $ \rT rB rA -> do
    comment "Multiply High Word Unsigned (XO-form, RC=0)"
    let lhs = zext' 64 (lowBits 32 (Loc rA))
    let rhs = zext' 64 (lowBits 32 (Loc rB))
    let prod = bvmul lhs rhs
    let res = zext (highBits64 32 prod)
    defLoc rT res
    definePPCOpcodeRC P.MULHWUo res $ do
      comment "Multiply High Word Unsigned (XO-form, RC=1)"
  definePPCOpcode P.DIVW xoform3c $ \rT rB rA -> do
    comment "Divide Word (XO-form, RC=0)"
    let dividend = lowBits 32 (Loc rA)
    let divisor = lowBits 32 (Loc rB)
    let res = bvsdiv (sext dividend) (sext divisor)
    defLoc rT res
    definePPCOpcodeRC P.DIVWo res $ do
      comment "Divide Word (XO-form, RC=1)"
  definePPCOpcode P.DIVWOE xoform3c $ \rT rB rA -> do
    comment "Divide Word with Overflow Enabled (XO-form, OE=1, RC=0)"
    let dividend = lowBits 32 (Loc rA)
    let divisor = lowBits 32 (Loc rB)
    let res = bvsdiv (sext dividend) (sext divisor)
    let overflow =
          bveq divisor (LitBV 32 0) `orp`
          (bveq dividend (LitBV 32 0x80000000) `andp`
           bveq divisor (LitBV 32 0xffffffff))
    let modifiedXer = updateXEROVSO (Loc xer) overflow
    defLoc rT res
    defineOERCVariant P.DIVWOEo res modifiedXer $ do
      comment "Divide Word with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    input xer
    defLoc xer modifiedXer
  definePPCOpcode P.DIVWU xoform3c $ \rT rB rA -> do
    comment "Divide Word Unsigned (XO-form, RC=0)"
    let dividend = lowBits 32 (Loc rA)
    let divisor = lowBits 32 (Loc rB)
    let res = bvudiv (sext dividend) (sext divisor)
    defLoc rT res
    definePPCOpcodeRC P.DIVWUo res $ do
      comment "Divide Word Unsigned (XO-form, RC=1)"
  definePPCOpcode P.DIVWUOE xoform3c $ \rT rB rA -> do
    comment "Divide Word Unsigned with Overflow Enabled (XO-form, OE=1, RC=0)"
    let dividend = lowBits 32 (Loc rA)
    let divisor = lowBits 32 (Loc rB)
    let res = bvudiv (sext dividend) (sext divisor)
    let modifiedXer = updateXEROVSO (Loc xer) (bveq divisor (LitBV 32 0))
    defLoc rT res
    defineOERCVariant P.DIVWUOEo res modifiedXer $ do
      comment "Divide Word Unsigned with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    input xer
    defLoc xer modifiedXer
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

  definePPCOpcode P.ADDC xoform3c $ \rT rB rA -> do
    comment "Add Carrying (XO-form, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (Loc rB))
    let res = lowBits' len eres
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres))
    definePPCOpcodeRC P.ADDCo res $ do
      comment "Add Carrying (XO-form, RC=1)"
  definePPCOpcode P.ADDCOE xoform3c $ \rT rB rA -> do
    comment "Add Carrying with Overflow Enabled (XO-form, OE=1, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (Loc rB))
    let res = lowBits' len eres
    let xerWithCarry = updateXER CA (Loc xer) (highBits' 1 eres)
    let modifiedXer = updateXEROVSOForAdd xerWithCarry (Loc rA) (Loc rB)
    defLoc rT res
    defineOERCVariant P.ADDCOEo res modifiedXer $ do
      comment "Add Carrying with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    defLoc xer modifiedXer
  definePPCOpcode P.ADDIC dformr0c $ \rT si rA -> do
    comment "Add Immediate Carrying (D-form)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres = bvadd (zext' (len + 1) (Loc rA)) (concat (LitBV 1 0x0) (sext (Loc si)))
    let res = lowBits' len eres
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres))
    definePPCOpcodeRC P.ADDICo res $ do
      comment "Add Immediate Carrying and Record (D-form)"
  definePPCOpcode P.SUBFIC dformr0c $ \rT si rA -> do
    comment "Subtract From Immediate Carrying (D-form)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (zext' (len + 1) (bvnot (Loc rA))) (concat (LitBV 1 0x0) (sext (Loc si)))
    let eres = bvadd eres0 (LitBV (len + 1) 0x1)
    let res = lowBits' len eres
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres))
  definePPCOpcode P.SUBFC xoform3c $ \rT rB rA -> do
    comment "Subtract From Carrying (XO-form, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (zext' (len + 1) (bvnot (Loc rA))) (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (LitBV (len + 1) 0x1)
    let res = lowBits' len eres1
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres1))
    definePPCOpcodeRC P.SUBFCo res $ do
      comment "Subtract From Carrying (XO-form, RC=1)"
  definePPCOpcode P.SUBFCOE xoform3c $ \rT rB rA -> do
    comment "Subtract From Carrying with Overflow Enabled (XO-form, OE=1, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 =
          bvadd (zext' (len + 1) (bvnot (Loc rA)))
                (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (LitBV (len + 1) 0x1)
    let res = lowBits' len eres1
    let xerWithCarry = updateXER CA (Loc xer) (highBits' 1 eres1)
    let modifiedXer =
          updateXEROVSOForSubtractWithCarry
            xerWithCarry (Loc rB) (Loc rA) (LitBV 1 1)
    defLoc rT res
    defineOERCVariant P.SUBFCOEo res modifiedXer $ do
      comment "Subtract From Carrying with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    defLoc xer modifiedXer
  definePPCOpcode P.ADDE xoform3c $ \rT rB rA -> do
    comment "Add Extended (XO-form, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (zext' (len + 1) (xerBit CA (Loc xer)))
    let res = lowBits' len eres1
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres1))
    definePPCOpcodeRC P.ADDEo res $ do
      comment "Add Extended (XO-form, RC=1)"
  definePPCOpcode P.ADDEOE xoform3c $ \rT rB rA -> do
    comment "Add Extended with Overflow Enabled (XO-form, OE=1, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let carryIn = xerBit CA (Loc xer)
    let eres0 = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (zext' (len + 1) carryIn)
    let res = lowBits' len eres1
    let xerWithCarry = updateXER CA (Loc xer) (highBits' 1 eres1)
    let modifiedXer =
          updateXEROVSOForAddWithCarry xerWithCarry (Loc rA) (Loc rB) carryIn
    defLoc rT res
    defineOERCVariant P.ADDEOEo res modifiedXer $ do
      comment "Add Extended with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    defLoc xer modifiedXer
  definePPCOpcode P.ADDME xoform2c $ \rT rA -> do
    comment "Add to Minus One Extended (XO-form, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (xerBit CA (Loc xer)))
    let eres1 = bvadd eres0 (zext' (len + 1) (bvnot (naturalLitBV 0x0)))
    let res = lowBits' len eres1
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres1))
    definePPCOpcodeRC P.ADDMEo res $ do
      comment "Add to Minus One Extended (XO-form, RC=1)"
  definePPCOpcode P.ADDMEOE xoform2c $ \rT rA -> do
    comment "Add to Minus One Extended with Overflow Enabled (XO-form, OE=1, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let carryIn = xerBit CA (Loc xer)
    let minusOne = bvnot (naturalLitBV 0x0)
    let eres0 = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) minusOne)
    let eres1 = bvadd eres0 (zext' (len + 1) carryIn)
    let res = lowBits' len eres1
    let xerWithCarry = updateXER CA (Loc xer) (highBits' 1 eres1)
    let modifiedXer =
          updateXEROVSOForAddWithCarry xerWithCarry (Loc rA) minusOne carryIn
    defLoc rT res
    defineOERCVariant P.ADDMEOEo res modifiedXer $ do
      comment "Add to Minus One Extended with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    defLoc xer modifiedXer
  definePPCOpcode P.SUBFE xoform3c $ \rT rB rA -> do
    comment "Subtract From Extended (XO-form, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres0 = bvadd (zext' (len + 1) (bvnot (Loc rA))) (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (zext' (len + 1) (xerBit CA (Loc xer)))
    let res = lowBits' len eres1
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres1))
    definePPCOpcodeRC P.SUBFEo res $ do
      comment "Subtract From Extended (XO-form, RC=1)"
  definePPCOpcode P.SUBFEOE xoform3c $ \rT rB rA -> do
    comment "Subtract From Extended with Overflow Enabled (XO-form, OE=1, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let carryIn = xerBit CA (Loc xer)
    let eres0 =
          bvadd (zext' (len + 1) (bvnot (Loc rA)))
                (zext' (len + 1) (Loc rB))
    let eres1 = bvadd eres0 (zext' (len + 1) carryIn)
    let res = lowBits' len eres1
    let xerWithCarry = updateXER CA (Loc xer) (highBits' 1 eres1)
    let modifiedXer =
          updateXEROVSOForSubtractWithCarry
            xerWithCarry (Loc rB) (Loc rA) carryIn
    defLoc rT res
    defineOERCVariant P.SUBFEOEo res modifiedXer $ do
      comment "Subtract From Extended with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    defLoc xer modifiedXer
  definePPCOpcode P.ADDZE xoform2c $ \rT rA -> do
    comment "Add to Zero Extended (XO-form, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) (xerBit CA (Loc xer)))
    let res = lowBits' len eres
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres))
    definePPCOpcodeRC P.ADDZEo res $ do
      comment "Add to Zero Extended (XO-form, RC=1)"
  definePPCOpcode P.ADDZEOE xoform2c $ \rT rA -> do
    comment "Add to Zero Extended with Overflow Enabled (XO-form, OE=1, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let carryIn = xerBit CA (Loc xer)
    let zero = naturalLitBV 0x0
    let eres = bvadd (zext' (len + 1) (Loc rA)) (zext' (len + 1) carryIn)
    let res = lowBits' len eres
    let xerWithCarry = updateXER CA (Loc xer) (highBits' 1 eres)
    let modifiedXer =
          updateXEROVSOForAddWithCarry xerWithCarry (Loc rA) zero carryIn
    defLoc rT res
    defineOERCVariant P.ADDZEOEo res modifiedXer $ do
      comment "Add to Zero Extended with Overflow Enabled and Record (XO-form, OE=1, RC=1)"
    defLoc xer modifiedXer
  definePPCOpcode P.SUBFZE xoform2c $ \rT rA -> do
    comment "Subtract From Zero Extended (XO-form, RC=0)"
    input xer
    let len = bitSizeValue ?bitSize
    let eres = bvadd (zext' (len + 1) (bvnot (Loc rA))) (zext' (len + 1) (xerBit CA (Loc xer)))
    let res = lowBits' len eres
    defLoc rT res
    defLoc xer (updateXER CA (Loc xer) (highBits' 1 eres))
    defineRCVariant "SUBFZEo" res $ do
      comment "Subtract From Zero Extended (XO-form, RC=1)"

  when (?bitSize == Size64) $ do
    -- Not valid in 32 bit mode
    definePPCOpcode P.MULLD xoform3c $ \rT rB rA -> do
      comment "Multiply Low Doubleword (XO-form, RC=0)"
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      let res = lowBits128 64 prod
      defLoc rT res
      definePPCOpcodeRC P.MULLDo res $ do
        comment "Multiply Low Doubleword (XO-form, RC=1)"
    definePPCOpcode P.MULHD xoform3c $ \rT rB rA -> do
      comment "Multiply High Doubleword (XO-form, RC=0)"
      let prod = bvmul (sext' 128 (Loc rA)) (sext' 128 (Loc rB))
      let res = highBits128 64 prod
      defLoc rT res
      definePPCOpcodeRC P.MULHDo res $ do
        comment "Multiply High Doubleword (XO-form, RC=1)"
    definePPCOpcode P.MULHDU xoform3c $ \rT rB rA -> do
      comment "Multiply High Doubleword Unsigned (XO-form, RC=0)"
      let prod = bvmul (zext' 128 (Loc rA)) (zext' 128 (Loc rB))
      let res = highBits128 64 prod
      defLoc rT res
      definePPCOpcodeRC P.MULHDUo res $ do
        comment "Multiply High Doubleword Unsigned (XO-form, RC=1)"
    definePPCOpcode P.DIVD xoform3c $ \rT rB rA -> do
      comment "Divide Doubleword Signed (XO-form, RC=0)"
      let res = bvsdiv (Loc rA) (Loc rB)
      defLoc rT res
      definePPCOpcodeRC P.DIVDo res $ do
        comment "Divide Doubleword Signed (XO-form, RC=1)"
    definePPCOpcode P.DIVDU xoform3c $ \rT rB rA -> do
      comment "Divide Doubleword Unsigned (XO-form, RC=0)"
      let res = bvudiv (Loc rA) (Loc rB)
      defLoc rT res
      definePPCOpcodeRC P.DIVDUo res $ do
        comment "Divide Doubleword Unsigned (XO-form, RC=1)"
