{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Architecture.PPC.Base.FP (
  floatingPoint,
  floatingPointLoads,
  floatingPointStores,
  floatingPointCompare
  ) where

import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat )
import Data.Parameterized.Some ( Some(..) )

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

fbinop :: (HasCallStack) => Int -> String -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fbinop sz name e1 e2 =
  uf (EBV sz) name [ Some e1, Some e2 ]

ftrop :: (HasCallStack) => Int
      -> String
      -> Expr 'TBV
      -> Expr 'TBV
      -> Expr 'TBV
      -> Expr 'TBV
ftrop sz name e1 e2 e3 =
  uf (EBV sz) name [ Some e1, Some e2, Some e3 ]

fadd64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fadd64 = fbinop 64 "fp.add64"

fadd32 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fadd32 = fbinop 32 "fp.add32"

fsub64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fsub64 = fbinop 64 "fp.sub64"

fsub32 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fsub32 = fbinop 32 "fp.sub32"

fmul64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fmul64 = fbinop 64 "fp.mul64"

fmul32 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fmul32 = fbinop 32 "fp.mul32"

fdiv64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fdiv64 = fbinop 64 "fp.div64"

fdiv32 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fdiv32 = fbinop 32 "fp.div32"

fmuladd64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fmuladd64 = ftrop 64 "fp.muladd64"

fmuladd32 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fmuladd32 = ftrop 32 "fp.muladd32"

fnegate64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
fnegate64 = uf (EBV 64) "fp.negate64" . ((:[]) . Some)

fnegate32 :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
fnegate32 = uf (EBV 32) "fp.negate32" . ((:[]) . Some)

froundsingle :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
froundsingle = uf (EBV 32) "fp.round_single" . ((:[]) . Some)

fsingletodouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
fsingletodouble = uf (EBV 64) "fp.single_to_double" . ((:[]) . Some)

fabs :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
fabs = uf (EBV 64) "fp.abs" . ((:[]) . Some)

fissnan32 :: (HasCallStack) => Expr 'TBV -> Expr 'TBool
fissnan32 = uf EBool "fp.is_snan32" . ((:[]) . Some)

fissnan64 :: (HasCallStack) => Expr 'TBV -> Expr 'TBool
fissnan64 = uf EBool "fp.is_snan64" . ((:[]) . Some)

-- | Extract the single-precision part of a vector register
extractSingle :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extractSingle = highBits128 32

-- | Extend a single-precision value out to 128 bits
extendSingle :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extendSingle = concat (LitBV 96 0x0)

-- | Extract the double-precision part of a vector register
extractDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extractDouble = highBits128 64

-- | Extend a double-precision value out to 128 bits
extendDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extendDouble = concat (LitBV 64 0x0)

-- | Lift a two-operand operation to single-precision values
--
-- Or maybe better thought of as lifting a single precision operation onto 128
-- bit registers.
liftSingle2 :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV -> Expr 'TBV)
            -- ^ An operation over 32 bit (single-precision) floats
            -> Expr 'TBV
            -- ^ 128 bit operand 1
            -> Expr 'TBV
            -- ^ 128-bit operand 2
            -> Expr 'TBV
liftSingle2 operation op1 op2 = do
  extendSingle (operation (extractSingle op1) (extractSingle op2))

liftDouble2 :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV -> Expr 'TBV)
            -> Expr 'TBV
            -> Expr 'TBV
            -> Expr 'TBV
liftDouble2 operation op1 op2 = do
  extendDouble (operation (extractDouble op1) (extractDouble op2))

liftSingle3 :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV)
            -> Expr 'TBV
            -> Expr 'TBV
            -> Expr 'TBV
            -> Expr 'TBV
liftSingle3 operation op1 op2 op3 =
  extendSingle (operation (extractSingle op1) (extractSingle op2) (extractSingle op3))

liftDouble3 :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV)
            -> Expr 'TBV
            -> Expr 'TBV
            -> Expr 'TBV
            -> Expr 'TBV
liftDouble3 operation op1 op2 op3 =
  extendDouble (operation (extractDouble op1) (extractDouble op2) (extractDouble op3))

liftDouble1 :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV) -> Expr 'TBV -> Expr 'TBV
liftDouble1 operation op =
  extendDouble (operation (extractDouble op))

liftSingle1 :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV) -> Expr 'TBV -> Expr 'TBV
liftSingle1 operation op =
  extendSingle (operation (extractSingle op))

-- | Floating point comparison definitions
--
floatingPointCompare :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPointCompare = do
  defineOpcodeWithIP "FCMPUS" $ do
    comment "Floating Compare Unordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frA <- param "frA" fprc (EBV 128)
    frB <- param "frB" fprc (EBV 128)
    input frA
    input frB
    input fpscr
    let sNaN = orp (fissnan32 (extractSingle (Loc frA))) (fissnan32 (extractSingle (Loc frB)))
    defLoc fpscr (ite sNaN (bvor (Loc fpscr) (LitBV 32 0x00000080)) (Loc fpscr))
  defineOpcodeWithIP "FCMPUD" $ do
    comment "Floating Compare Unordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frA <- param "frA" fprc (EBV 128)
    frB <- param "frB" fprc (EBV 128)
    input frA
    input frB
    input fpscr
    let sNaN = orp (fissnan64 (extractDouble (Loc frA))) (fissnan64 (extractDouble (Loc frB)))
    defLoc fpscr (ite sNaN (bvor (Loc fpscr) (LitBV 32 0x00000080)) (Loc fpscr))

-- | Floating point operation definitions
--
-- FIXME: None of these are defining the status or control registers yet
floatingPoint :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPoint = do
  defineOpcodeWithIP "FADD" $ do
    comment "Floating Add (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftDouble2 fadd64 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FADDS" $ do
    comment "Floating Add Single (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftSingle2 fadd32 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FSUB" $ do
    comment "Floating Subtract (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftDouble2 fsub64 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FSUBS" $ do
    comment "Floating Subtract Single (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftSingle2 fsub32 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FMUL" $ do
    comment "Floating Multiply (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftDouble2 fmul64 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FMULS" $ do
    comment "Floating Multiply Single (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftSingle2 fmul32 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FDIV" $ do
    comment "Floating Divide (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftDouble2 fdiv64 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FDIVS" $ do
    comment "Floating Divide Single (A-form)"
    (frT, frA, frB) <- aform
    defLoc frT (liftSingle2 fdiv32 (Loc frA) (Loc frB))

  defineOpcodeWithIP "FMADD" $ do
    comment "Floating Multiply-Add (A-form)"
    (frT, frA, frB, frC) <- aform4
    defLoc frT (liftDouble3 fmuladd64 (Loc frA) (Loc frB) (Loc frC))

  defineOpcodeWithIP "FMADDS" $ do
    comment "Floating Multiply-Add Single (A-form)"
    (frT, frA, frB, frC) <- aform4
    defLoc frT (liftSingle3 fmuladd32 (Loc frA) (Loc frB) (Loc frC))

  defineOpcodeWithIP "FMSUB" $ do
    comment "Floating Multiply-Subtract (A-form)"
    (frT, frA, frB, frC) <- aform4
    let frB' = liftDouble1 fnegate64 (Loc frB)
    defLoc frT (liftDouble3 fmuladd64 (Loc frA) frB' (Loc frC))

  defineOpcodeWithIP "FMSUBS" $ do
    comment "Floating Multiply-Subtract Single (A-form)"
    (frT, frA, frB, frC) <- aform4
    let frB' = liftSingle1 fnegate32 (Loc frB)
    defLoc frT (liftSingle3 fmuladd32 (Loc frA) frB' (Loc frC))

  defineOpcodeWithIP "FNMADD" $ do
    comment "Floating Negative Multiply-Add (A-form)"
    (frT, frA, frB, frC) <- aform4
    let nres = liftDouble3 fmuladd64 (Loc frA) (Loc frB) (Loc frC)
    defLoc frT (liftDouble1 fnegate64 nres)

  defineOpcodeWithIP "FNMADDS" $ do
    comment "Floating Negative Multiply-Add Single (A-form)"
    (frT, frA, frB, frC) <- aform4
    let nres = liftSingle3 fmuladd32 (Loc frA) (Loc frB) (Loc frC)
    defLoc frT (liftSingle1 fnegate32 nres)

  defineOpcodeWithIP "FNMSUB" $ do
    comment "Floating Negative Multiply-Subtract (A-form)"
    (frT, frA, frB, frC) <- aform4
    let frB' = liftDouble1 fnegate64 (Loc frB)
    let nres = liftDouble3 fmuladd64 (Loc frA) frB' (Loc frC)
    defLoc frT (liftDouble1 fnegate64 nres)

  defineOpcodeWithIP "FNMSUBS" $ do
    comment "Floating Negative Multiply-Subtract Single (A-form)"
    (frT, frA, frB, frC) <- aform4
    let frB' = liftSingle1 fnegate32 (Loc frB)
    let nres = liftSingle3 fmuladd32 (Loc frA) frB' (Loc frC)
    defLoc frT (liftSingle1 fnegate32 nres)

  defineOpcodeWithIP "FRSP" $ do
    comment "Floating Round to Single-Precision (X-form)"
    (frT, frB) <- xform2f
    defLoc frT (extendSingle (froundsingle (extractDouble (Loc frB))))

  defineOpcodeWithIP "FNEGD" $ do
    comment "Floating Negate (X-form)"
    comment "There is no single-precision form of this because"
    comment "the sign bit is always in the same place (MSB)"
    (frT, frB) <- xform2f
    defLoc frT (extendDouble (fnegate64 (extractDouble (Loc frB))))

  defineOpcodeWithIP "FNEGS" $ do
    comment "Floating Negate (X-form)"
    comment "There is no single-precision form of this because"
    comment "the sign bit is always in the same place (MSB)"
    (frT, frB) <- xform2f
    defLoc frT (extendDouble (fnegate64 (extractDouble (Loc frB))))

  defineOpcodeWithIP "FMR" $ do
    comment "Floating Move Register (X-form)"
    (frT, frB) <- xform2f
    defLoc frT (Loc frB)

  -- See Note [FABS]
  defineOpcodeWithIP "FABSD" $ do
    comment "Floating Absolute Value (X-form)"
    (frT, frB) <- xform2f
    defLoc frT (extendDouble (fabs (extractDouble (Loc frB))))

  defineOpcodeWithIP "FNABSD" $ do
    comment "Floating Negative Absolute Value (X-form)"
    (frT, frB) <- xform2f
    let av = fabs (extractDouble (Loc frB))
    defLoc frT (extendDouble (fnegate64 av))

  defineOpcodeWithIP "FABSS" $ do
    comment "Floating Absolute Value (X-form)"
    (frT, frB) <- xform2f
    defLoc frT (extendDouble (fabs (extractDouble (Loc frB))))

  defineOpcodeWithIP "FNABSS" $ do
    comment "Floating Negative Absolute Value (X-form)"
    (frT, frB) <- xform2f
    let av = fabs (extractDouble (Loc frB))
    defLoc frT (extendDouble (fnegate64 av))

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
