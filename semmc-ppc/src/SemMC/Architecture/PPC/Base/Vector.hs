{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
-- | Semantics for the AltiVec (aka VMX) instructions
module SemMC.Architecture.PPC.Base.Vector (
  baseVector
  ) where

import Prelude hiding ( concat )

import qualified Dismantle.PPC as P

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

vec1op :: String -> SemM 'Def ()
vec1op name = do
  (vrT, vrA) <- vxform2
  input vscr
  let res = ppcvec1 name (Loc vrA) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

vec1ops :: String -> SemM 'Def ()
vec1ops name = do
  (vrT, sim) <- vxform2s
  input vscr
  let sim_ext = sext' 128 (Loc sim)
  let res = ppcvec1 name sim_ext (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

vec2op :: String -> SemM 'Def ()
vec2op name = do
  (vrT, vrA, vrB) <- vxform3
  input vscr
  let res = ppcvec2 name (Loc vrA) (Loc vrB) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

vec2opu :: String -> SemM 'Def ()
vec2opu name = do
  (vrT, vrB, uim) <- vxform3u
  let uim_ext = zext' 128 (Loc uim)
  input vscr
  let res = ppcvec2 name uim_ext (Loc vrB) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

vec3op :: String -> SemM 'Def ()
vec3op name = do
  (vrT, vrA, vrB, vrC) <- vaform
  input vscr
  let res = ppcvec3 name (Loc vrA) (Loc vrB) (Loc vrC) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

vec3opu :: String -> SemM 'Def ()
vec3opu name = do
  (vrT, shb, vrA, vrB) <- vaform4u
  -- shb is a 5-bit unsigned immediate, so we extend to a 128-bit vector operand.
  let shb_ext = zext' 128 (Loc shb)
  input vscr
  let res = ppcvec3 name shb_ext (Loc vrA) (Loc vrB) (Loc vscr)
  defLoc vrT (highBits' 128 res)
  defLoc vscr (lowBits' 32 res)

-- | Definitions of vector instructions
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
  vecMinMax
  vecSum
  vecMultAdd
  vecAverage

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

  defineOpcodeWithIP "VSUBCUW" $ do
    comment "Vector Subtract and Write Carry-Out Unsigned Word (VX-form)"
    vec2op "VSUBCUW"

  defineOpcodeWithIP "VSUBSBS" $ do
    comment "Vector Subtract Signed Byte Saturate (VX-form)"
    vec2op "VSUBSBS"

  defineOpcodeWithIP "VSUBSHS" $ do
    comment "Vector Subtract Signed Halfword Saturate (VX-form)"
    vec2op "VSUBSHS"

  defineOpcodeWithIP "VSUBSWS" $ do
    comment "Vector Subtract Signed Word Saturate (VX-form)"
    vec2op "VSUBSWS"

  defineOpcodeWithIP "VSUBUBM" $ do
    comment "Vector Subtract Unsigned Byte Modulo (VX-form)"
    vec2op "VSUBUBM"

  defineOpcodeWithIP "VSUBUDM" $ do
    comment "Vector Subtract Unsigned Doubleword Modulo (VX-form)"
    vec2op "VSUBUDM"

  defineOpcodeWithIP "VSUBUHM" $ do
    comment "Vector Subtract Unsigned Halfword Modulo (VX-form)"
    vec2op "VSUBUHM"

  defineOpcodeWithIP "VSUBUWM" $ do
    comment "Vector Subtract Unsigned Word Modulo (VX-form)"
    vec2op "VSUBUWM"

  defineOpcodeWithIP "VSUBUBS" $ do
    comment "Vector Subtract Unsigned Byte Saturate (VX-form)"
    vec2op "VSUBUBS"

  defineOpcodeWithIP "VSUBUHS" $ do
    comment "Vector Subtract Unsigned Halfword Saturate (VX-form)"
    vec2op "VSUBUHS"

  defineOpcodeWithIP "VSUBUWS" $ do
    comment "Vector Subtract Unsigned Word Saturate (VX-form)"
    vec2op "VSUBUWS"

  defineOpcodeWithIP "VSUBUQM" $ do
    comment "Vector Subtract Unsigned Quadword Modulo (VX-form)"
    vec2op "VSUBUQM"

  defineOpcodeWithIP "VSUBEUQM" $ do
    comment "Vector Subtract Extended Unsigned Quadword Modulo (VA-form)"
    vec3op "VSUBEUQM"

  defineOpcodeWithIP "VSUBCUQ" $ do
    comment "Vector Subtract & Write Carry Unsigned Quadword (VX-form)"
    vec2op "VSUBCUQ"

  defineOpcodeWithIP "VSUBECUQ" $ do
    comment "Vector Subtract Extended & Write Carry Unsigned Quadword (VA-form)"
    vec3op "VSUBECUQ"

vecBitwise :: (?bitSize :: BitSize) => SemM 'Top ()
vecBitwise = do
  definePPCOpcode P.VAND vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical AND (VX-form)"
    defLoc vrT (bvand (Loc vrA) (Loc vrB))

  definePPCOpcode P.VANDC vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical AND with Complement (VX-form)"
    defLoc vrT (bvand (Loc vrA) (bvnot (Loc vrB)))

  definePPCOpcode P.VEQV vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical Equivalent (VX-form)"
    defLoc vrT (bvnot (bvxor (Loc vrA) (Loc vrB)))

  definePPCOpcode P.VNAND vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical NAND (VX-form)"
    defLoc vrT (bvnot (bvand (Loc vrA) (Loc vrB)))

  definePPCOpcode P.VORC vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical OR with Complement (VX-form)"
    defLoc vrT (bvor (Loc vrA) (bvnot (Loc vrB)))

  definePPCOpcode P.VNOR vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical NOR (VX-form)"
    defLoc vrT (bvnot (bvor (Loc vrA) (Loc vrB)))

  definePPCOpcode P.VOR vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical OR (VX-form)"
    defLoc vrT (bvor (Loc vrA) (Loc vrB))

  definePPCOpcode P.VXOR vxform3c $ \vrT vrA vrB -> do
    comment "Vector Logical XOR (VX-form)"
    defLoc vrT (bvxor (Loc vrA) (Loc vrB))

  defineOpcodeWithIP "VSL" $ do
    comment "Vector Shift Left (VX-form)"
    vec2op "VSL"

  defineOpcodeWithIP "VSLDOI" $ do
    comment "Vector Shift Left Double by Octet Immediate (VA-form)"
    vec3opu "VSLDOI"

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

vecMultAdd :: (?bitSize :: BitSize) => SemM 'Top ()
vecMultAdd = do
  defineOpcodeWithIP "VMHADDSHS" $ do
    comment "Vector Multiply-High-Add Signed Halfword Saturate (VA-form)"
    vec3op "VMHADDSHS"

  defineOpcodeWithIP "VMHRADDSHS" $ do
    comment "Vector Multiply-High-Round-Add Signed Halfword Saturate (VA-form)"
    vec3op "VMHRADDSHS"

  defineOpcodeWithIP "VMLADDUHM" $ do
    comment "Vector Multiply-Low-Add Unsigned Halfword Modulo (VA-form)"
    vec3op "VMLADDUHM"

  defineOpcodeWithIP "VMSUMUBM" $ do
    comment "Vector Multiply-Sum Unsigned Byte Modulo (VA-form)"
    vec3op "VMSUMUBM"

  defineOpcodeWithIP "VMSUMMBM" $ do
    comment "Vector Multiply-Sum Mixed Byte Modulo (VA-form)"
    vec3op "VMSUMMBM"

  defineOpcodeWithIP "VMSUMSHM" $ do
    comment "Vector Multiply-Sum Signed Halfword Modulo (VA-form)"
    vec3op "VMSUMSHM"

  defineOpcodeWithIP "VMSUMSHS" $ do
    comment "Vector Multiply-Sum Signed Halfword Saturate (VA-form)"
    vec3op "VMSUMSHS"

  defineOpcodeWithIP "VMSUMUHM" $ do
    comment "Vector Multiply-Sum Unsigned Halfword Modulo (VA-form)"
    vec3op "VMSUMUHM"

  defineOpcodeWithIP "VMSUMUHS" $ do
    comment "Vector Multiply-Sum Unsigned Halfword Saturate (VA-form)"
    vec3op "VMSUMUHS"

vecMerge :: (?bitSize :: BitSize) => SemM 'Top ()
vecMerge = do
  defineOpcodeWithIP "VMRGHB" $ do
    comment "Vector Merge High Byte (VX-form)"
    vec2op "VMRGHB"

  defineOpcodeWithIP "VMRGLB" $ do
    comment "Vector Merge Low Byte (VX-form)"
    vec2op "VMRGLB"

  defineOpcodeWithIP "VMRGHH" $ do
    comment "Vector Merge High Halfword (VX-form)"
    vec2op "VMRGHH"

  defineOpcodeWithIP "VMRGLH" $ do
    comment "Vector Merge Low Halfword (VX-form)"
    vec2op "VMRGLH"

  defineOpcodeWithIP "VMRGHW" $ do
    comment "Vector Merge High Word (VX-form)"
    vec2op "VMRGHW"

  defineOpcodeWithIP "VMRGLW" $ do
    comment "Vector Merge Low Word (VX-form)"
    vec2op "VMRGLW"

  defineOpcodeWithIP "VMRGEW" $ do
    comment "Vector Merge Even Word (VX-form)"
    vec2op "VMRGEW"

  defineOpcodeWithIP "VMRGOW" $ do
    comment "Vector Merge Odd Word (VX-form)"
    vec2op "VMRGOW"

vecSplat :: (?bitSize :: BitSize) => SemM 'Top ()
vecSplat = do
  defineOpcodeWithIP "VSPLTB" $ do
    comment "Vector Splat Byte (VX-form)"
    vec2opu "VSPLTB"

  defineOpcodeWithIP "VSPLTH" $ do
    comment "Vector Splat Halfword (VX-form)"
    vec2opu "VSPLTH"

  defineOpcodeWithIP "VSPLTW" $ do
    comment "Vector Splat Word (VX-form)"
    vec2opu "VSPLTW"

  defineOpcodeWithIP "VSPLTISB" $ do
    comment "Vector Splat Immediate Signed Byte (VX-form)"
    vec1ops "VSPLTISB"

  defineOpcodeWithIP "VSPLTISH" $ do
    comment "Vector Splat Immediate Signed Halfword (VX-form)"
    vec1ops "VSPLTISH"

  defineOpcodeWithIP "VSPLTISW" $ do
    comment "Vector Splat Immediate Signed Word (VX-form)"
    vec1ops "VSPLTISW"

vecSum :: (?bitSize :: BitSize) => SemM 'Top ()
vecSum = do
  defineOpcodeWithIP "VSUMSWS" $ do
    comment "Vector Sum Across Signed Word Saturate (VX-form)"
    vec2op "VSUMSWS"
  defineOpcodeWithIP "VSUM2SWS" $ do
    comment "Vector Sum Across Half Signed Word Saturate (VX-form)"
    vec2op "VSUM2SWS"
  defineOpcodeWithIP "VSUM4SBS" $ do
    comment "Vector Sum Across Quarter Signed Byte Saturate (VX-form)"
    vec2op "VSUM4SBS"
  defineOpcodeWithIP "VSUM4SHS" $ do
    comment "Vector Sum Across Quarter Signed Halfword Saturate (VX-form)"
    vec2op "VSUM4SHS"
  defineOpcodeWithIP "VSUM4UBS" $ do
    comment "Vector Sum Across Quarter Unsigned Byte Saturate (VX-form)"
    vec2op "VSUM4UBS"

vecAverage :: (?bitSize :: BitSize) => SemM 'Top ()
vecAverage = do
  defineOpcodeWithIP "VAVGSB" $ do
    comment "Vector Average Signed Byte (VX-form)"
    vec2op "VAVGSB"
  defineOpcodeWithIP "VAVGSW" $ do
    comment "Vector Average Signed Word (VX-form)"
    vec2op "VAVGSW"
  defineOpcodeWithIP "VAVGSH" $ do
    comment "Vector Average Signed Halfword (VX-form)"
    vec2op "VAVGSH"
  defineOpcodeWithIP "VAVGUB" $ do
    comment "Vector Average Unsigned Byte (VX-form)"
    vec2op "VAVGUB"
  defineOpcodeWithIP "VAVGUW" $ do
    comment "Vector Average Unsigned Word (VX-form)"
    vec2op "VAVGUW"
  defineOpcodeWithIP "VAVGUH" $ do
    comment "Vector Average Unsigned Halfword (VX-form)"
    vec2op "VAVGUH"

vecMinMax :: (?bitSize :: BitSize) => SemM 'Top ()
vecMinMax = do
  defineOpcodeWithIP "VMAXSB" $ do
    comment "Vector Maximum Signed Byte (VX-form)"
    vec2op "VMAXSB"
  defineOpcodeWithIP "VMAXUB" $ do
    comment "Vector Maximum Unsigned Byte (VX-form)"
    vec2op "VMAXUB"
  defineOpcodeWithIP "VMAXSD" $ do
    comment "Vector Maximum Signed Doubleword (VX-form)"
    vec2op "VMAXSD"
  defineOpcodeWithIP "VMAXUD" $ do
    comment "Vector Maximum Unsigned Doubleword (VX-form)"
    vec2op "VMAXUD"
  defineOpcodeWithIP "VMAXSH" $ do
    comment "Vector Maximum Signed Halfword (VX-form)"
    vec2op "VMAXSH"
  defineOpcodeWithIP "VMAXUH" $ do
    comment "Vector Maximum Unsigned Halfword (VX-form)"
    vec2op "VMAXUH"
  defineOpcodeWithIP "VMAXSW" $ do
    comment "Vector Maximum Signed Word (VX-form)"
    vec2op "VMAXSW"
  defineOpcodeWithIP "VMAXUW" $ do
    comment "Vector Maximum Unsigned Word (VX-form)"
    vec2op "VMAXUW"
  defineOpcodeWithIP "VMINSB" $ do
    comment "Vector Minimum Signed Byte (VX-form)"
    vec2op "VMINSB"
  defineOpcodeWithIP "VMINUB" $ do
    comment "Vector Minimum Unsigned Byte (VX-form)"
    vec2op "VMINUB"
  defineOpcodeWithIP "VMINSD" $ do
    comment "Vector Minimum Signed Doubleword (VX-form)"
    vec2op "VMINSD"
  defineOpcodeWithIP "VMINUD" $ do
    comment "Vector Minimum Unsigned Doubleword (VX-form)"
    vec2op "VMINUD"
  defineOpcodeWithIP "VMINSH" $ do
    comment "Vector Minimum Signed Halfword (VX-form)"
    vec2op "VMINSH"
  defineOpcodeWithIP "VMINUH" $ do
    comment "Vector Minimum Unsigned Halfword (VX-form)"
    vec2op "VMINUH"
  defineOpcodeWithIP "VMINSW" $ do
    comment "Vector Minimum Signed Word (VX-form)"
    vec2op "VMINSW"
  defineOpcodeWithIP "VMINUW" $ do
    comment "Vector Minimum Unsigned Word (VX-form)"
    vec2op "VMINUW"


vecPack :: (?bitSize :: BitSize) => SemM 'Top ()
vecPack = do
  defineOpcodeWithIP "VPKPX" $ do
    comment "Vector Pack Pixel (VX-form)"
    vec2op "VPKPX"

  defineOpcodeWithIP "VPKSDSS" $ do
    comment "Vector Pack Signed Doubleword Signed Saturate (VX-form)"
    vec2op "VPKSDSS"

  defineOpcodeWithIP "VPKSDUS" $ do
    comment "Vector Pack Signed Doubleword Unsigned Saturate (VX-form)"
    vec2op "VPKSDUS"

  defineOpcodeWithIP "VPKSHSS" $ do
    comment "Vector Pack Signed Halfword Signed Saturate (VX-form)"
    vec2op "VPKSHSS"

  defineOpcodeWithIP "VPKSHUS" $ do
    comment "Vector Pack Signed Halfword Unsigned Saturate (VX-form)"
    vec2op "VPKSHUS"

  defineOpcodeWithIP "VPKSWSS" $ do
    comment "Vector Pack Signed Word Signed Saturate (VX-form)"
    vec2op "VPKSWSS"

  defineOpcodeWithIP "VPKSWUS" $ do
    comment "Vector Pack Signed Word Unsigned Saturate (VX-form)"
    vec2op "VPKSWUS"

  defineOpcodeWithIP "VPKUDUS" $ do
    comment "Vector Pack Unsigned Doubleword Unsigned Saturate (VX-form)"
    vec2op "VPKUDUS"

  defineOpcodeWithIP "VPKUDUM" $ do
    comment "Vector Pack Unsigned Doubleword Unsigned Modulo (VX-form)"
    vec2op "VPKUDUM"

  defineOpcodeWithIP "VPKUHUM" $ do
    comment "Vector Pack Unsigned Halfword Unsigned Modulo (VX-form)"
    vec2op "VPKUHUM"

  defineOpcodeWithIP "VPKUHUS" $ do
    comment "Vector Pack Unsigned Halfword Unsigned Saturate (VX-form)"
    vec2op "VPKUHUS"

  defineOpcodeWithIP "VPKUWUS" $ do
    comment "Vector Pack Unsigned Word Unsigned Saturate (VX-form)"
    vec2op "VPKUWUS"

  defineOpcodeWithIP "VPKUWUM" $ do
    comment "Vector Pack Unsigned Word Unsigned Modulo (VX-form)"
    vec2op "VPKUWUM"

  defineOpcodeWithIP "VUPKHPX" $ do
    comment "Vector Unpack High Pixel (VX-form)"
    vec1op "VUPKHPX"

  defineOpcodeWithIP "VUPKLPX" $ do
    comment "Vector Unpack Low Pixel (VX-form)"
    vec1op "VUPKLPX"

  defineOpcodeWithIP "VUPKHSB" $ do
    comment "Vector Unpack High Signed Byte (VX-form)"
    vec1op "VUPKHSB"

  defineOpcodeWithIP "VUPKLSB" $ do
    comment "Vector Unpack Low Signed Byte (VX-form)"
    vec1op "VUPKLSB"

  defineOpcodeWithIP "VUPKHSH" $ do
    comment "Vector Unpack High Signed Halfword (VX-form)"
    vec1op "VUPKHSH"

  defineOpcodeWithIP "VUPKLSH" $ do
    comment "Vector Unpack Low Signed Halfword (VX-form)"
    vec1op "VUPKLSH"

  defineOpcodeWithIP "VUPKHSW" $ do
    comment "Vector Unpack High Signed Word (VX-form)"
    vec1op "VUPKHSW"

  defineOpcodeWithIP "VUPKLSW" $ do
    comment "Vector Unpack Low Signed Word (VX-form)"
    vec1op "VUPKLSW"

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
