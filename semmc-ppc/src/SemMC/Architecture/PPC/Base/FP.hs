{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Architecture.PPC.Base.FP (
  floatingPoint,
  floatingPointLoads,
  floatingPointStores,
  floatingPointCompare,
  -- * Primitives
  froundsingle,
  fsingletodouble
  ) where

import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat )
import Data.Parameterized.Some ( Some(..) )

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

fp1op :: String -> SemM 'Def ()
fp1op name = do
  (frT, frB) <- xform2f
  input fpscr
  let res = ppcvec1 name (Loc frB) (Loc fpscr)
  defLoc frT (highBits' 128 res)
  defLoc fpscr (lowBits' 32 res)

fp2op :: String -> SemM 'Def ()
fp2op name = do
  (frT, frA, frB) <- aform
  input fpscr
  let res = ppcvec2 name (Loc frA) (Loc frB) (Loc fpscr)
  defLoc frT (highBits' 128 res)
  defLoc fpscr (lowBits' 32 res)

fp3op :: String -> SemM 'Def ()
fp3op name = do
  (frT, frA, frB, frC) <- aform4
  input fpscr
  let res = ppcvec3 name (Loc frA) (Loc frB) (Loc frC) (Loc fpscr)
  defLoc frT (highBits' 128 res)
  defLoc fpscr (lowBits' 32 res)

froundsingle :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
froundsingle = uf (EBV 32) "fp.round_single" . ((:[]) . Some)

fsingletodouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
fsingletodouble = uf (EBV 64) "fp.single_to_double" . ((:[]) . Some)

flt :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBool
flt e1 e2 = uf EBool "fp.lt" [ Some e1, Some e2 ]

fisqnan64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBool
fisqnan64 = uf EBool "fp.is_qnan64" . ((:[]) . Some)

fissnan64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBool
fissnan64 = uf EBool "fp.is_snan64" . ((:[]) . Some)

fisnan64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBool
fisnan64 e = orp (fisqnan64 e) (fissnan64 e)

-- | Extract the double-precision part of a vector register
extractDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extractDouble = highBits128 64

-- | Extend a double-precision value out to 128 bits
extendDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extendDouble = concat (LitBV 64 0x0)

-- | Floating point comparison definitions
--
fcbits :: (HasCallStack, ?bitSize :: BitSize)
       => Expr 'TBV
       -- ^ The first operand
       -> Expr 'TBV
       -- ^ the second operand
       -> Expr 'TBV
fcbits opa opb = LitBV 4 0x0000 -- c
                 -- FIXME
  where
    c = ite (orp (fisnan64 opa) (fisnan64 opb)) (LitBV 4 0x0001)
        (ite (flt opa opb) (LitBV 4 0x1000)
         (ite (flt opb opa) (LitBV 4 0x0100) (LitBV 4 0x0010)))

fcmp :: (HasCallStack, ?bitSize :: BitSize)
     => Expr 'TBV
     -- ^ The crrc field
     -> Expr 'TBV
     -- ^ The first operand
     -> Expr 'TBV
     -- ^ The second operand
     -> Expr 'TBV
fcmp fld opa opb =
  bvor crFld0 shiftedNibble
  where
    c = fcbits opa opb
    shiftedNibble = bvshl (zext' 32 c) (bvmul (zext' 32 fld) (LitBV 32 0x4))
    crFld0 = bvand (Loc cr) (bvnot (bvshl (LitBV 32 0xf) (bvmul (zext' 32 fld) (LitBV 32 0x4))))

floatingPointCompare :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPointCompare = do
  -- For some reason, Dismantle disassembles the FCMPU instruction in two
  -- variants. There really is no difference between the two.

  -- FIXME:
  -- Here, we are either setting or unsetting the FPCC and VXSNAN fields (either 0 or
  -- 1), but we are not unsetting the FX field if VXSNAN gets set to 0. I'm not sure
  -- if this is the correct behavior; something to look into.

  defineOpcodeWithIP "FCMPUS" $ do
    comment "Floating Compare Unordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frA <- param "frA" fprc (EBV 128)
    frB <- param "frB" fprc (EBV 128)
    input frA
    input frB
    input cr
    input fpscr

    let lowA = extractDouble (Loc frA)
    let lowB = extractDouble (Loc frB)

    let c     = fcbits lowA lowB
    let newCR = fcmp (Loc bf) lowA lowB

    let snan = orp (fissnan64 lowA) (fissnan64 lowB)

    -- zero out the FPCC and VXSNAN bits
    let fpscrFld0 = bvand (Loc fpscr) (LitBV 32 0xfff0ff7f)

    let snanMask = ite snan (LitBV 32 0x00000080) (LitBV 32 0x00000000)
    let fpccMask = bvshl (zext' 32 c) (LitBV 32 0x00000010)
    let fxMask   = ite snan (LitBV 32 0x00000001) (LitBV 32 0x00000000)

    defLoc cr newCR
    defLoc fpscr (bvor snanMask
                  (bvor fpccMask
                   (bvor fxMask fpscrFld0)))
  defineOpcodeWithIP "FCMPUD" $ do
    comment "Floating Compare Unordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frA <- param "frA" fprc (EBV 128)
    frB <- param "frB" fprc (EBV 128)
    input frA
    input frB
    input cr
    input fpscr

    let lowA = extractDouble (Loc frA)
    let lowB = extractDouble (Loc frB)

    let c     = fcbits lowA lowB
    let newCR = fcmp (Loc bf) lowA lowB

    let snan = orp (fissnan64 lowA) (fissnan64 lowB)

    -- zero out the FPCC and VXSNAN bits
    let fpscrFld0 = bvand (Loc fpscr) (LitBV 32 0xfff0ff7f)

    let snanMask = ite snan (LitBV 32 0x00000080) (LitBV 32 0x00000000)
    let fpccMask = bvshl (zext' 32 c) (LitBV 32 0x00000010)
    let fxMask   = ite snan (LitBV 32 0x00000001) (LitBV 32 0x00000000)

    defLoc cr newCR
    defLoc fpscr (bvor snanMask
                  (bvor fpccMask
                   (bvor fxMask fpscrFld0)))

  -- FIXME: CR is left undefined here
  defineOpcodeWithIP "MFFS" $ do
    comment "Move From FPSCR (X-form, RC=0)"
    frT <- param "FRT" fprc vectorBV
    input fpscr
    defLoc frT (concat (Loc fpscr) (undefinedBV 96))
    forkDefinition "MFFSo" $ do
      comment "Move From FPSCR (X-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MCRFS" $ do
    comment "Move to Condition Register from FPSCR (X-form)"
    _bf <- param "BF" crrc (EBV 3)
    _bfa <- param "BFA" crrc (EBV 3)
    defLoc cr (undefinedBV 32)
    defLoc fpscr (undefinedBV 32)

  defineOpcodeWithIP "MTFSFI" $ do
    comment "Move to FPSCR Field Immediate (X-form, RC=0)"
    _bf <- param "BF" crrc (EBV 3)
    _u <- param "U" "I32imm" (EBV 4)
    _w <- param "W" "I32imm" (EBV 1)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSFIo" $ do
      comment "Move to FPSCR Field Immediate (X-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MTFSF" $ do
    comment "Move to FPSCR Fields (XFL-form, RC=0)"
    _flm <- param "FLM" "I32imm" (EBV 8)
    _l <- param "L" "I32imm" (EBV 1)
    _frB <- param "frB" fprc vectorBV
    _w <- param "W" "I32imm" (EBV 1)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSFo" $ do
      comment "Move to FPSCR Fields (XFL-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MTFSB0" $ do
    comment "Move to FPSCR Bit 0 (X-form, RC=0)"
    _bt <- param "BT" u5imm (EBV 5)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSB0o" $ do
      comment "Move to FPSCR Bit 0 (X-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MTFSB1" $ do
    comment "Move to FPSCR Bit 1 (X-form, RC=0)"
    _bt <- param "BT" u5imm (EBV 5)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSB1o" $ do
      comment "Move to FPSCR Bit 1 (X-form, RC=1)"
      defLoc cr (undefinedBV 32)

-- | Floating point operation definitions
--
-- FIXME: None of these are defining the status or control registers yet
floatingPoint :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPoint = do
  defineOpcodeWithIP "FADD" $ do
    comment "Floating Add (A-form)"
    fp2op "FADD"

  defineOpcodeWithIP "FADDS" $ do
    comment "Floating Add Single (A-form)"
    fp2op "FADDS"

  defineOpcodeWithIP "FSUB" $ do
    comment "Floating Subtract (A-form)"
    fp2op "FSUB"

  defineOpcodeWithIP "FSUBS" $ do
    comment "Floating Subtract Single (A-form)"
    fp2op "FSUBS"

  defineOpcodeWithIP "FMUL" $ do
    comment "Floating Multiply (A-form)"
    fp2op "FMUL"

  defineOpcodeWithIP "FMULS" $ do
    comment "Floating Multiply Single (A-form)"
    fp2op "FMULS"

  defineOpcodeWithIP "FDIV" $ do
    comment "Floating Divide (A-form)"
    fp2op "FDIV"

  defineOpcodeWithIP "FDIVS" $ do
    comment "Floating Divide Single (A-form)"
    fp2op "FDIVS"

  defineOpcodeWithIP "FMADD" $ do
    comment "Floating Multiply-Add (A-form)"
    fp3op "FMADD"

  defineOpcodeWithIP "FMADDS" $ do
    comment "Floating Multiply-Add Single (A-form)"
    fp3op "FMADDS"

  -- NOTE: This functions were previously defined in terms of lower-level operations
  -- like negation and multiply-add, but our new encoding just pushes the opcode
  -- through for consistency.
  defineOpcodeWithIP "FMSUB" $ do
    comment "Floating Multiply-Subtract (A-form)"
    fp3op "FMSUB"

  defineOpcodeWithIP "FMSUBS" $ do
    comment "Floating Multiply-Subtract Single (A-form)"
    fp3op "FMSUBS"

  defineOpcodeWithIP "FNMADD" $ do
    comment "Floating Negative Multiply-Add (A-form)"
    fp3op "FNMADD"

  defineOpcodeWithIP "FNMADDS" $ do
    comment "Floating Negative Multiply-Add Single (A-form)"
    fp3op "FNMADDS"

  defineOpcodeWithIP "FNMSUB" $ do
    comment "Floating Negative Multiply-Subtract (A-form)"
    fp3op "FNMSUB"

  defineOpcodeWithIP "FNMSUBS" $ do
    comment "Floating Negative Multiply-Subtract Single (A-form)"
    fp3op "FNMSUBS"

  defineOpcodeWithIP "FRSP" $ do
    comment "Floating Round to Single-Precision (X-form)"
    fp1op "FRSP"

  defineOpcodeWithIP "FCTID" $ do
    comment "Floating Point Convert to Integer Doubleword (X-form)"
    fp1op "FCTID"

  defineOpcodeWithIP "FCTIDZ" $ do
    comment "Floating Point Convert to Integer Doubleword with Round Towards Zero (X-form)"
    fp1op "FCTIDZ"

  defineOpcodeWithIP "FCTIDU" $ do
    comment "Floating Point Convert to Integer Doubleword Unsigned (X-form)"
    fp1op "FCTIDU"

  defineOpcodeWithIP "FCTIDUZ" $ do
    comment "Floating Point Convert to Integer Doubleword Unsigned with Round Towards Zero (X-form)"
    fp1op "FCTIDUZ"

  defineOpcodeWithIP "FCTIW" $ do
    comment "Floating Point Convert to Integer Word (X-form)"
    fp1op "FCTIW"

  defineOpcodeWithIP "FCTIWZ" $ do
    comment "Floating Point Convert to Integer Word with Round Towards Zero (X-form)"
    fp1op "FCTIWZ"

  defineOpcodeWithIP "FCTIWU" $ do
    comment "Floating Point Convert to Integer Word Unsigned (X-form)"
    fp1op "FCTIWU"

  defineOpcodeWithIP "FCTIWUZ" $ do
    comment "Floating Point Convert to Integer Word Unsigned with Round Towards Zero (X-form)"
    fp1op "FCTIWUZ"

  defineOpcodeWithIP "FCFID" $ do
    comment "Floating Point Convert from Integer Doubleword (X-form)"
    fp1op "FCFID"

  defineOpcodeWithIP "FCFIDU" $ do
    comment "Floating Point Convert from Integer Doubleword Unsigned (X-form)"
    fp1op "FCFIDU"

  defineOpcodeWithIP "FCFIDS" $ do
    comment "Floating Point Convert from Integer Doubleword Single (X-form)"
    fp1op "FCFIDS"

  defineOpcodeWithIP "FCFIDUS" $ do
    comment "Floating Point Convert from Integer Doubleword Unsigned Single (X-form)"
    fp1op "FCFIDUS"

  defineOpcodeWithIP "FNEGD" $ do
    comment "Floating Negate (X-form)"
    comment "There is no single-precision form of this because"
    comment "the sign bit is always in the same place (MSB)"
    fp1op "FNEGD"

  defineOpcodeWithIP "FNEGS" $ do
    comment "Floating Negate (X-form)"
    comment "There is no single-precision form of this because"
    comment "the sign bit is always in the same place (MSB)"
    fp1op "FNEGS"

  defineOpcodeWithIP "FMR" $ do
    comment "Floating Move Register (X-form)"
    fp1op "FMR"

  -- See Note [FABS]
  defineOpcodeWithIP "FABSD" $ do
    comment "Floating Absolute Value (X-form)"
    fp1op "FABSD"

  defineOpcodeWithIP "FNABSD" $ do
    comment "Floating Negative Absolute Value (X-form)"
    fp1op "FNABSD"

  defineOpcodeWithIP "FABSS" $ do
    comment "Floating Absolute Value (X-form)"
    fp1op "FABSS"

  defineOpcodeWithIP "FNABSS" $ do
    comment "Floating Negative Absolute Value (X-form)"
    fp1op "FNABSS"

  defineOpcodeWithIP "FSQRT" $ do
    comment "Floating Square Root (A-form)"
    fp1op "FSQRT"

  defineOpcodeWithIP "FSQRTS" $ do
    comment "Floating Square Root Single (A-form)"
    fp1op "FSQRT"

  defineOpcodeWithIP "FRE" $ do
    comment "Floating Reciprocal Estimate (A-form)"
    fp1op "FRE"

  defineOpcodeWithIP "FRES" $ do
    comment "Floating Reciprocal Estimate Single (A-form)"
    fp1op "FRES"

  defineOpcodeWithIP "FRSQRTE" $ do
    comment "Floating Reciprocal Square Root Estimate (A-form)"
    fp1op "FRSQRTE"

  defineOpcodeWithIP "FRSQRTES" $ do
    comment "Floating Reciprocal Square Root Estimate Single (A-form)"
    fp1op "FRSQRTES"

  defineOpcodeWithIP "FSELD" $ do
    comment "Floating-Point Select (A-form)"
    fp3op "FSELD"

  defineOpcodeWithIP "FSELS" $ do
    comment "Floating-Point Select Single (A-form)"
    fp3op "FSELS"

-- | Define a load and double conversion of a single floating-point (D-form)
loadFloat :: (?bitSize :: BitSize)
          => Int
          -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
          -> SemM 'Def ()
loadFloat nBytes convert = do
  frT <- param "frT" fprc (EBV 128)
  memref <- param "memref" memri EMemRef
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext disp)
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))

loadFloatWithUpdate :: (?bitSize :: BitSize)
                   => Int
                   -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                   -> SemM 'Def ()
loadFloatWithUpdate nBytes convert = do
  frT <- param "frT" fprc (EBV 128)
  memref <- param "memref" memri EMemRef
  input memory
  input memref
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let ea = bvadd (Loc rA) (sext disp)
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))
  defLoc rA ea

loadFloatIndexed :: (?bitSize :: BitSize)
                 => Int
                 -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                 -> SemM 'Def ()
loadFloatIndexed nBytes convert = do
  frT <- param "rT" fprc (EBV 128)
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b rB
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))

loadFloatWithUpdateIndexed :: (?bitSize :: BitSize)
                          => Int
                          -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                          -> SemM 'Def ()
loadFloatWithUpdateIndexed nBytes convert = do
  frT <- param "frT" fprc (EBV 128)
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let ea = bvadd (Loc rA) rB
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))
  defLoc rA ea

floatingPointLoads :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPointLoads = do
  defineOpcodeWithIP "LFS" $ do
    comment "Load Floating-Point Single (D-form)"
    loadFloat 4 fsingletodouble
  defineOpcodeWithIP "LFSX" $ do
    comment "Load Floating-Point Single Indexed (X-form)"
    loadFloatIndexed 4 fsingletodouble
  defineOpcodeWithIP "LFSU" $ do
    comment "Load Floating-Point Single with Update (D-form)"
    loadFloatWithUpdate 4 fsingletodouble
  defineOpcodeWithIP "LFSUX" $ do
    comment "Load Floating-Point Single with Update Indexed (X-form)"
    loadFloatWithUpdateIndexed 4 fsingletodouble
  defineOpcodeWithIP "LFD" $ do
    comment "Load Floating-Point Double (D-form)"
    loadFloat 8 id
  defineOpcodeWithIP "LFDX" $ do
    comment "Load Floating-Point Double Indexed (X-form)"
    loadFloatIndexed 8 id
  defineOpcodeWithIP "LFDU" $ do
    comment "Load Floating-Point Double with Update (D-form)"
    loadFloatWithUpdate 8 id
  defineOpcodeWithIP "LFDUX" $ do
    comment "Load Floating-Point Single with Update Indexed (X-form)"
    loadFloatWithUpdateIndexed 8 id

  defineOpcodeWithIP "LFIWAX" $ do
    comment "Load Floating-Point as Integer Word Algebraic Indexed (X-form)"
    loadFloatIndexed 4 (sext' 64)

  defineOpcodeWithIP "LFIWZX" $ do
    comment "Load Floating-Point as Integer Word Zero Indexed (X-form)"
    loadFloatIndexed 4 (zext' 64)
  return ()



storeFloat :: (?bitSize :: BitSize)
           => Int
           -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
           -> SemM 'Def ()
storeFloat nBytes convert = do
  memref <- param "memref" memri EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext disp)
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))

storeFloatWithUpdate :: (?bitSize :: BitSize)
                     => Int
                     -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                     -> SemM 'Def ()
storeFloatWithUpdate nBytes convert = do
  memref <- param "memref" memri EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let ea = bvadd (Loc rA) (sext disp)
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))
  defLoc rA ea

storeFloatIndexed :: (?bitSize :: BitSize)
                  => Int
                  -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                  -> SemM 'Def ()
storeFloatIndexed nBytes convert = do
  memref <- param "memref" memrr EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b rB
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))

storeFloatWithUpdateIndexed :: (?bitSize :: BitSize)
                            => Int
                            -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                            -> SemM 'Def ()
storeFloatWithUpdateIndexed nBytes convert = do
  memref <- param "memref" memrr EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let ea = bvadd (Loc rA) rB
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))
  defLoc rA ea

floatingPointStores :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPointStores = do
  defineOpcodeWithIP "STFS" $ do
    comment "Store Floating-Point Single (D-form)"
    storeFloat 4 froundsingle
  defineOpcodeWithIP "STFSU" $ do
    comment "Store Floating-Point Single with Update (D-form)"
    storeFloatWithUpdate 4 froundsingle
  defineOpcodeWithIP "STFSX" $ do
    comment "Store Floating-Point Single Indexed (X-form)"
    storeFloatIndexed 4 froundsingle
  defineOpcodeWithIP "STFSUX" $ do
    comment "Store Floating-Point Single with Update Indexed (X-form)"
    storeFloatWithUpdateIndexed 4 froundsingle
  defineOpcodeWithIP "STFD" $ do
    comment "Store Floating-Point Double (D-form)"
    storeFloat 8 id
  defineOpcodeWithIP "STFDU" $ do
    comment "Store Floating-Point Double with Update (D-form)"
    storeFloatWithUpdate 8 id
  defineOpcodeWithIP "STFDX" $ do
    comment "Store Floating-Point Double Indexed (X-form)"
    storeFloatIndexed 8 id
  defineOpcodeWithIP "STFDUX" $ do
    comment "Store Floating-Point Double with Update Indexed (X-form)"
    storeFloatWithUpdateIndexed 8 id

  defineOpcodeWithIP "STFIWX" $ do
    comment "Store Floating-Point as Integer Word Indexed (X-form)"
    storeFloatIndexed 4 (lowBits' 32)
  return ()


{- Note [FABS and FNEG]

There is actually only one FABS instruction on PPC: the 64 bit FABS.  The
operation happens to have the same effect on single and double precision values,
so only one instruction is necessary.

The LLVM tablegen data includes a single and double precision version,
presumably to simplify code generation.  We specify semantics here for both to
mirror LLVM.

The same is true of FNEG

-}
