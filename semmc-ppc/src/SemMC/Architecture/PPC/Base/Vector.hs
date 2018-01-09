{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
-- | Semantics for the AltiVec (aka VMX) instructions
module SemMC.Architecture.PPC.Base.Vector (
  baseVector
  ) where

import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

import Data.Parameterized.Some

-- | This is a cheap way to encode strings in our DSL in order to pass along a little
-- extra information with an uninterpreted function -- in this case, our UFs are the
-- ppcvec functions, but we also would like to pass the *name* of the particular
-- operation we are considering so that the backend has that information as well. The
-- type of the expression is a boolean because we didn't have a unit.
vecFn :: String -> Expr 'TString
vecFn s = LitString s

-- | These encode various architecture-specific vector functions by storing the
-- string along with the opcodes. The result is a 160-bit vector; the high 128 bits
-- are used for the target register and the remaining 32 bits are stored in the
-- VSCR.
ppcvec1 :: (HasCallStack)
        => String
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
ppcvec1 name vr1 vvscr =
  uf (EBV 160) "ppc.vec1" [ Some (vecFn name), Some vr1, Some vvscr ]

ppcvec2 :: (HasCallStack)
        => String
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
ppcvec2 name vr1 vr2 vvscr =
  uf (EBV 160) "ppc.vec2" [ Some (vecFn name), Some vr1, Some vr2, Some vvscr ]

ppcvec3 :: (HasCallStack)
        => String
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
ppcvec3 name vr1 vr2 vr3 vvscr =
  uf (EBV 160) "ppc.vec3" [ Some (vecFn name), Some vr1, Some vr2, Some vr3, Some vvscr ]

vec1op :: String -> SemM 'Def ()
vec1op name = do
  (vrT, vrA) <- vxform2
  input vscr
  let res = ppcvec1 name (Loc vrA) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

vec2op :: String -> SemM 'Def ()
vec2op name = do
  (vrT, vrA, vrB) <- vxform3
  input vscr
  let res = ppcvec2 name (Loc vrA) (Loc vrB) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

-- Uses VA-form
vec3op :: String -> SemM 'Def ()
vec3op name = do
  (vrT, vrA, vrB, vrC) <- vaform
  input vscr
  let res = ppcvec3 name (Loc vrA) (Loc vrB) (Loc vrC) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

-- Uses VA-form (4u)
vec3op4u :: String -> SemM 'Def ()
vec3op4u name = do
  (vrT, vrA, vrB, vrC) <- vaform4u
  input vscr
  let res = ppcvec3 name (Loc vrA) (Loc vrB) (Loc vrC) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

-- | Definitions of vector instructions
--
-- FIXME: For now, these are all stubs that leave their destination register as
-- undefined.
baseVector :: (?bitSize :: BitSize) => SemM 'Top ()
baseVector = do
  vecMerge
  vecSplat
  vecPack
  vecLoad
  vecStore
  vecArith
  vecBitwise
  vecCompare

  defineOpcodeWithIP "VPERM" $ do
    comment "Vector Permute (VA-form)"
    vec3op "VPERM"

  defineOpcodeWithIP "VSEL" $ do
    comment "Vector Select (VA-form)"
    vec3op "VSEL"
  return ()

vecCompare :: (?bitSize :: BitSize) => SemM 'Top ()
vecCompare = do
  defineOpcodeWithIP "VCMPEQUB" $ do
    comment "Vector Compare Equal To Unsigned Byte (VC-form, RC=0)"
    vec2op "VCMPEQUB"
    defineVRCVariant "VCMPEQUBo" $ do
      comment "Vector Compare Equal To Unsigned Byte (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPEQUH" $ do
    comment "Vector Compare Equal To Unsigned Halfword (VC-form, RC=0)"
    vec2op "VCMPEQUH"
    defineVRCVariant "VCMPEQUHo" $ do
      comment "Vector Compare Equal To Unsigned Halfword (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPEQUW" $ do
    comment "Vector Compare Equal To Unsigned Word (VC-form, RC=0)"
    vec2op "VCMPEQUW"
    defineVRCVariant "VCMPEQUWo" $ do
      comment "Vector Compare Equal To Unsigned Word (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPEQUD" $ do
    comment "Vector Compare Equal To Unsigned Doubleword (VC-form, RC=0)"
    vec2op "VCMPEQUD"
    defineVRCVariant "VCMPEQUDo" $ do
      comment "Vector Compare Equal To Unsigned Doubleword (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTSB" $ do
    comment "Vector Compare Greater Than Signed Byte (VC-form, RC=0)"
    vec2op "VCMPGTSB"
    defineVRCVariant "VCMPGTSBo" $ do
      comment "Vector Compare Greater Than Signed Byte (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTSH" $ do
    comment "Vector Compare Greater Than Signed Halfword (VC-form, RC=0)"
    vec2op "VCMPGTSH"
    defineVRCVariant "VCMPGTSHo" $ do
      comment "Vector Compare Greater Than Signed Halfword (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTSW" $ do
    comment "Vector Compare Greater Than Signed Word (VC-form, RC=0)"
    vec2op "VCMPGTSW"
    defineVRCVariant "VCMPGTSWo" $ do
      comment "Vector Compare Greater Than Signed Word (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTSD" $ do
    comment "Vector Compare Greater Than Signed Doubleword (VC-form, RC=0)"
    vec2op "VCMPGTSD"
    defineVRCVariant "VCMPGTSDo" $ do
      comment "Vector Compare Greater Than Signed Doubleword (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTUB" $ do
    comment "Vector Compare Greater Than Unsigned Byte (VC-form, RC=0)"
    vec2op "VCMPGTUB"
    defineVRCVariant "VCMPGTUBo" $ do
      comment "Vector Compare Greater Than Unsigned Byte (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTUH" $ do
    comment "Vector Compare Greater Than Unsigned Halfword (VC-form, RC=0)"
    vec2op "VCMPGTUH"
    defineVRCVariant "VCMPGTUHo" $ do
      comment "Vector Compare Greater Than Unsigned Halfword (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTUW" $ do
    comment "Vector Compare Greater Than Unsigned Word (VC-form, RC=0)"
    vec2op "VCMPGTUW"
    defineVRCVariant "VCMPGTUWo" $ do
      comment "Vector Compare Greater Than Unsigned Word (VC-form, RC=1)"

  defineOpcodeWithIP "VCMPGTUD" $ do
    comment "Vector Compare Greater Than Unsigned Doubleword (VC-form, RC=0)"
    vec2op "VCMPGTUD"
    defineVRCVariant "VCMPGTUDo" $ do
      comment "Vector Compare Greater Than Unsigned Doubleword (VC-form, RC=1)"

vecArith :: (?bitSize :: BitSize) => SemM 'Top ()
vecArith = do
  defineOpcodeWithIP "VADDCUW" $ do
    comment "Vector Add and Write Carry-Out Unsigned Word (VX-form)"
    vec2op "VADDCUW"

  defineOpcodeWithIP "VADDSBS" $ do
    comment "Vector Add Signed Byte Saturate (VX-form)"
    vec2op "VADDSBS"

  defineOpcodeWithIP "VADDSHS" $ do
    comment "Vector Add Signed Halfword Saturate (VX-form)"
    vec2op "VADDSHS"

  defineOpcodeWithIP "VADDSWS" $ do
    comment "Vector Add Signed Word Saturate (VX-form)"
    vec2op "VADDSWS"

  defineOpcodeWithIP "VADDUBM" $ do
    comment "Vector Add Unsigned Byte Modulo (VX-form)"
    vec2op "VADDUBM"

  defineOpcodeWithIP "VADDUDM" $ do
    comment "Vector Add Unsigned Doubleword Modulo (VX-form)"
    vec2op "VADDUDM"

  defineOpcodeWithIP "VADDUHM" $ do
    comment "Vector Add Unsigned Halfword Modulo (VX-form)"
    vec2op "VADDUHM"

  defineOpcodeWithIP "VADDUWM" $ do
    comment "Vector Add Unsigned Word Modulo (VX-form)"
    vec2op "VADDUWM"

  defineOpcodeWithIP "VADDUBS" $ do
    comment "Vector Add Unsigned Byte Saturate (VX-form)"
    vec2op "VADDUBS"

  defineOpcodeWithIP "VADDUHS" $ do
    comment "Vector Add Unsigned Halfword Saturate (VX-form)"
    vec2op "VADDUHS"

  defineOpcodeWithIP "VADDUWS" $ do
    comment "Vector Add Unsigned Word Saturate (VX-form)"
    vec2op "VADDUWS"

  defineOpcodeWithIP "VADDUQM" $ do
    comment "Vector Add Unsigned Quadword Modulo (VX-form)"
    vec2op "VADDUQM"

  defineOpcodeWithIP "VADDEUQM" $ do
    comment "Vector Add Extended Unsigned Quadword Modulo (VA-form)"
    vec3op "VADDEUQM"

  defineOpcodeWithIP "VADDCUQ" $ do
    comment "Vector Add & Write Carry Unsigned Quadword (VX-form)"
    vec2op "VADDCUQ"

  defineOpcodeWithIP "VADDECUQ" $ do
    comment "Vector Add Extended & Write Carry Unsigned Quadword (VA-form)"
    vec3op "VADDECUQ"

vecBitwise :: (?bitSize :: BitSize) => SemM 'Top ()
vecBitwise = do
  defineOpcodeWithIP "VAND" $ do
    comment "Vector Logical AND (VX-form)"
    vec2op "VAND"

  defineOpcodeWithIP "VANDC" $ do
    comment "Vector Logical AND with Complement (VX-form)"
    vec2op "VANDC"

  defineOpcodeWithIP "VEQV" $ do
    comment "Vector Logical Equivalent (VX-form)"
    vec2op "VEQV"

  defineOpcodeWithIP "VNAND" $ do
    comment "Vector Logical NAND (VX-form)"
    vec2op "VNAND"

  defineOpcodeWithIP "VORC" $ do
    comment "Vector Logical OR with Complement (VX-form)"
    vec2op "VORC"

  defineOpcodeWithIP "VNOR" $ do
    comment "Vector Logical NOR (VX-form)"
    vec2op "VNOR"

  defineOpcodeWithIP "VOR" $ do
    comment "Vector Logical OR (VX-form)"
    vec2op "VOR"

  defineOpcodeWithIP "VXOR" $ do
    comment "Vector Logical XOR (VX-form)"
    vec2op "VXOR"

  defineOpcodeWithIP "VSL" $ do
    comment "Vector Shift Left (VX-form)"
    vec2op "VSL"

  defineOpcodeWithIP "VSLDOI" $ do
    comment "Vector Shift Left Double by Octet Immediate (VA-form)"
    vec3op4u "VSLDOI"

  defineOpcodeWithIP "VSLO" $ do
    comment "Vector Shift Left by Octet (VX-form)"
    vec2op "VSLO"

  defineOpcodeWithIP "VSR" $ do
    comment "Vector Shift Right (VX-form)"
    vec2op "VSR"

  defineOpcodeWithIP "VSRO" $ do
    comment "Vector Shift Right by Octet (VX-form)"
    vec2op "VSRO"

  defineOpcodeWithIP "VSLB" $ do
    comment "Vector Shift Left Byte (VX-form)"
    vec2op "VSLB"

  defineOpcodeWithIP "VSLH" $ do
    comment "Vector Shift Left Halfword (VX-form)"
    vec2op "VSLH"

  defineOpcodeWithIP "VSLW" $ do
    comment "Vector Shift Left Word (VX-form)"
    vec2op "VSLW"

  defineOpcodeWithIP "VSLD" $ do
    comment "Vector Shift Left Doubleword (VX-form)"
    vec2op "VSLD"

  defineOpcodeWithIP "VSRB" $ do
    comment "Vector Shift Right Byte (VX-form)"
    vec2op "VSRB"

  defineOpcodeWithIP "VSRH" $ do
    comment "Vector Shift Right Halfword (VX-form)"
    vec2op "VSRH"

  defineOpcodeWithIP "VSRW" $ do
    comment "Vector Shift Right Word (VX-form)"
    vec2op "VSRW"

  defineOpcodeWithIP "VSRD" $ do
    comment "Vector Shift Right Doubleword (VX-form)"
    vec2op "VSRD"

  defineOpcodeWithIP "VSRAB" $ do
    comment "Vector Shift Right Algebraic Byte (VX-form)"
    vec2op "VSRAB"

  defineOpcodeWithIP "VSRAH" $ do
    comment "Vector Shift Right Algebraic Halfword (VX-form)"
    vec2op "VSRAH"

  defineOpcodeWithIP "VSRAW" $ do
    comment "Vector Shift Right Algebraic Word (VX-form)"
    vec2op "VSRAW"

  defineOpcodeWithIP "VSRAD" $ do
    comment "Vector Shift Right Algebraic Doubleword (VX-form)"
    vec2op "VSRAD"

  defineOpcodeWithIP "VGBBD" $ do
    comment "Vector Gather Bits by Bytes by Doubleword (VX-form)"
    vec1op "VGBBD"

  defineOpcodeWithIP "VCLZB" $ do
    comment "Vector Count Leading Zeros Byte (VX-form)"
    vec1op "VCLZB"

  defineOpcodeWithIP "VCLZH" $ do
    comment "Vector Count Leading Zeros Halfword (VX-form)"
    vec1op "VCLZH"

  defineOpcodeWithIP "VCLZW" $ do
    comment "Vector Count Leading Zeros Word (VX-form)"
    vec1op "VCLZW"

  defineOpcodeWithIP "VCLZD" $ do
    comment "Vector Count Leading Zeros Doubleword (VX-form)"
    vec1op "VCLZD"

  defineOpcodeWithIP "VPOPCNTB" $ do
    comment "Vector Population Count Byte (VX-form)"
    vec1op "VPOPCNTB"

  defineOpcodeWithIP "VPOPCNTH" $ do
    comment "Vector Population Count Halfword (VX-form)"
    vec1op "VPOPCNTH"

  defineOpcodeWithIP "VPOPCNTW" $ do
    comment "Vector Population Count Word (VX-form)"
    vec1op "VPOPCNTW"

  defineOpcodeWithIP "VPOPCNTD" $ do
    comment "Vector Population Count Doubleword (VX-form)"
    vec1op "VPOPCNTD"

  defineOpcodeWithIP "VBPERMQ" $ do
    comment "Vector Bit Permute Quadword (VX-form)"
    vec2op "VBPERMQ"

vecMerge :: (?bitSize :: BitSize) => SemM 'Top ()
vecMerge = do
  defineOpcodeWithIP "VMRGHB" $ do
    comment "Vector Merge High Byte (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VMRGLB" $ do
    comment "Vector Merge Low Byte (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VMRGHH" $ do
    comment "Vector Merge High Halfword (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VMRGLH" $ do
    comment "Vector Merge Low Halfword (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VMRGHW" $ do
    comment "Vector Merge High Word (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VMRGLW" $ do
    comment "Vector Merge Low Word (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VMRGEW" $ do
    comment "Vector Merge Even Word (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VMRGOW" $ do
    comment "Vector Merge Odd Word (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

vecSplat :: (?bitSize :: BitSize) => SemM 'Top ()
vecSplat = do
  defineOpcodeWithIP "VSPLTB" $ do
    comment "Vector Splat Byte (VX-form)"
    (vrT, _, _) <- vxform3u
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSPLTH" $ do
    comment "Vector Splat Halfword (VX-form)"
    (vrT, _, _) <- vxform3u
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSPLTW" $ do
    comment "Vector Splat Word (VX-form)"
    (vrT, _, _) <- vxform3u
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSPLTISB" $ do
    comment "Vector Splat Immediate Signed Byte (VX-form)"
    (vrT, _) <- vxform2s
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSPLTISH" $ do
    comment "Vector Splat Immediate Signed Halfword (VX-form)"
    (vrT, _) <- vxform2s
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSPLTISW" $ do
    comment "Vector Splat Immediate Signed Word (VX-form)"
    (vrT, _) <- vxform2s
    defLoc vrT (undefinedBV 128)

vecPack :: (?bitSize :: BitSize) => SemM 'Top ()
vecPack = do
  defineOpcodeWithIP "VPKPX" $ do
    comment "Vector Pack Pixel (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKSDSS" $ do
    comment "Vector Pack Signed Doubleword Signed Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKSDUS" $ do
    comment "Vector Pack Signed Doubleword Unsigned Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKSHSS" $ do
    comment "Vector Pack Signed Halfword Signed Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKSHUS" $ do
    comment "Vector Pack Signed Halfword Unsigned Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKSWSS" $ do
    comment "Vector Pack Signed Word Signed Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKSWUS" $ do
    comment "Vector Pack Signed Word Unsigned Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKUDUS" $ do
    comment "Vector Pack Unsigned Doubleword Unsigned Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKUDUM" $ do
    comment "Vector Pack Unsigned Doubleword Unsigned Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKUHUM" $ do
    comment "Vector Pack Unsigned Halfword Unsigned Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKUHUS" $ do
    comment "Vector Pack Unsigned Halfword Unsigned Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKUWUS" $ do
    comment "Vector Pack Unsigned Word Unsigned Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VPKUWUM" $ do
    comment "Vector Pack Unsigned Word Unsigned Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKHPX" $ do
    comment "Vector Unpack High Pixel (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKLPX" $ do
    comment "Vector Unpack Low Pixel (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKHSB" $ do
    comment "Vector Unpack High Signed Byte (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKLSB" $ do
    comment "Vector Unpack Low Signed Byte (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKHSH" $ do
    comment "Vector Unpack High Signed Halfword (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKLSH" $ do
    comment "Vector Unpack Low Signed Halfword (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKHSW" $ do
    comment "Vector Unpack High Signed Word (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VUPKLSW" $ do
    comment "Vector Unpack Low Signed Word (VX-form)"
    (vrT, _) <- vxform2
    defLoc vrT (undefinedBV 128)

-- | The mask to clear the low bit of the effective address to ensure aligned
-- loads of larger-than-byte values into vector registers.
memMask :: (?bitSize :: BitSize) => Expr 'TBV
memMask = sext (LitBV 8 0xfe)

vecLoad :: (?bitSize :: BitSize) => SemM 'Top ()
vecLoad = do
  -- NOTE: This is only right for big endian
  defineOpcodeWithIP "LVEBX" $ do
    comment "Load Vector Element Byte Indexed (X-form)"
    vrT <- param "vrT" vrrc vectorBV
    memref <- param "memrr" memrr EMemRef
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvadd b rB
    defLoc vrT (concat (undefinedBV 120) (readMem (Loc memory) ea 1))

  defineOpcodeWithIP "LVEHX" $ do
    comment "Load Vector Element Halfword Indexed (X-form)"
    vrT <- param "vrT" vrrc vectorBV
    memref <- param "memrr" memrr EMemRef
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc vrT (concat (undefinedBV 112) (readMem (Loc memory) ea 2))

  defineOpcodeWithIP "LVEWX" $ do
    comment "Load Vector Element Word Indexed (X-form)"
    vrT <- param "vrT" vrrc vectorBV
    memref <- param "memrr" memrr EMemRef
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc vrT (concat (undefinedBV 96) (readMem (Loc memory) ea 4))

  defineOpcodeWithIP "LVX" $ do
    comment "Load Vector Indexed (X-form)"
    vrT <- param "vrT" vrrc vectorBV
    memref <- param "memrr" memrr EMemRef
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc vrT (readMem (Loc memory) ea 16)

  defineOpcodeWithIP "LVXL" $ do
    comment "Load Vector Indexed LRU (X-form)"
    comment "This form provides a cache hint"
    vrT <- param "vrT" vrrc vectorBV
    memref <- param "memrr" memrr EMemRef
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc vrT (readMem (Loc memory) ea 16)

  defineOpcodeWithIP "LVSL" $ do
    comment "Load Vector for Shift Left Indexed (X-form)"
    vrT <- param "vrT" vrrc vectorBV
    memref <- param "memrr" memrr EMemRef
    input memref
    input memory
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "LVSR" $ do
    comment "Load Vector for Shift Right Indexed (X-form)"
    vrT <- param "vrT" vrrc vectorBV
    memref <- param "memrr" memrr EMemRef
    input memref
    input memory
    defLoc vrT (undefinedBV 128)

vecStore :: (?bitSize :: BitSize) => SemM 'Top ()
vecStore = do
  defineOpcodeWithIP "STVEBX" $ do
    comment "Store Vector Element Byte Indexed (X-form)"
    memref <- param "memref" memrr EMemRef
    vrS <- param "vrS" vrrc vectorBV
    input vrS
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvadd rB b
    defLoc memory (storeMem (Loc memory) ea 1 (lowBits128 8 (Loc vrS)))

  defineOpcodeWithIP "STVEHX" $ do
    comment "Store Vector Element Halfword Indexed (X-form)"
    memref <- param "memref" memrr EMemRef
    vrS <- param "vrS" vrrc vectorBV
    input vrS
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc memory (storeMem (Loc memory) ea 2 (lowBits128 16 (Loc vrS)))

  defineOpcodeWithIP "STVEWX" $ do
    comment "Store Vector Element Word Indexed (X-form)"
    memref <- param "memref" memrr EMemRef
    vrS <- param "vrS" vrrc vectorBV
    input vrS
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc memory (storeMem (Loc memory) ea 4 (lowBits128 32 (Loc vrS)))

  defineOpcodeWithIP "STVX" $ do
    comment "Store Vector Indexed (X-form)"
    memref <- param "memref" memrr EMemRef
    vrS <- param "vrS" vrrc vectorBV
    input vrS
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc memory (storeMem (Loc memory) ea 16 (Loc vrS))

  defineOpcodeWithIP "STVXL" $ do
    comment "Store Vector Indexed LRU (X-form)"
    comment "This form provides a cache hint"
    memref <- param "memref" memrr EMemRef
    vrS <- param "vrS" vrrc vectorBV
    input vrS
    input memory
    input memref
    let rA = memrrBaseReg memref
    let rB = memrrOffsetReg (Loc memref)
    let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
    let ea = bvand memMask (bvadd rB b)
    defLoc memory (storeMem (Loc memory) ea 16 (Loc vrS))
