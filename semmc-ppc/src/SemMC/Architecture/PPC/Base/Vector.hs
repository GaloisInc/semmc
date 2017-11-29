{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
-- | Semantics for the AltiVec (aka VMX) instructions
module SemMC.Architecture.PPC.Base.Vector (
  baseVector
  ) where

import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

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

  defineOpcodeWithIP "VPERM" $ do
    comment "Vector Permute (VA-form)"
    (vrT, _, _, _) <- vaform
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSEL" $ do
    comment "Vector Select (VA-form)"
    (vrT, _, _, _) <- vaform
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSL" $ do
    comment "Vector Shift Left (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSLO" $ do
    comment "Vector Shift Left by Octet (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSLDOI" $ do
    comment "Vector Shift Left Double by Octet Immediate (VA-form)"
    (vrT, _, _, _) <- vaform4u
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSR" $ do
    comment "Vector Shift Right (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VSRO" $ do
    comment "Vector Shift Right by Octet (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  return ()

vecArith :: (?bitSize :: BitSize) => SemM 'Top ()
vecArith = do
  defineOpcodeWithIP "VADDCUW" $ do
    comment "Vector Add and Write Carry-Out Unsigned Word (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDSBS" $ do
    comment "Vector Add Signed Byte Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDSHS" $ do
    comment "Vector Add Signed Halfword Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDSWS" $ do
    comment "Vector Add Signed Word Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUBM" $ do
    comment "Vector Add Unsigned Byte Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUDM" $ do
    comment "Vector Add Unsigned Doubleword Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUHM" $ do
    comment "Vector Add Unsigned Halfword Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUWM" $ do
    comment "Vector Add Unsigned Word Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUBS" $ do
    comment "Vector Add Unsigned Byte Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUHS" $ do
    comment "Vector Add Unsigned Halfword Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUWS" $ do
    comment "Vector Add Unsigned Word Saturate (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDUQM" $ do
    comment "Vector Add Unsigned Quadword Modulo (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDEUQM" $ do
    comment "Vector Add Extended Unsigned Quadword Modulo (VA-form)"
    (vrT, _, _, _) <- vaform
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDCUQ" $ do
    comment "Vector Add & Write Carry Unsigned Quadword (VX-form)"
    (vrT, _, _) <- vxform3
    defLoc vrT (undefinedBV 128)

  defineOpcodeWithIP "VADDECUQ" $ do
    comment "Vector Add Extended & Write Carry Unsigned Quadword (VA-form)"
    (vrT, _, _, _) <- vaform
    defLoc vrT (undefinedBV 128)

vecBitwise :: (?bitSize :: BitSize) => SemM 'Top ()
vecBitwise = do
  defineOpcodeWithIP "VAND" $ do
    comment "Vector Logical AND (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvand (Loc vrA) (Loc vrB))

  defineOpcodeWithIP "VANDC" $ do
    comment "Vector Logical AND with Complement (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvand (Loc vrA) (bvnot (Loc vrB)))

  defineOpcodeWithIP "VEQV" $ do
    comment "Vector Logical Equivalent (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvnot (bvxor (Loc vrA) (Loc vrB)))

  defineOpcodeWithIP "VNAND" $ do
    comment "Vector Logical NAND (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvnot (bvand (Loc vrA) (Loc vrB)))

  defineOpcodeWithIP "VORC" $ do
    comment "Vector Logical OR with Complement (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvor (Loc vrA) (bvnot (Loc vrB)))

  defineOpcodeWithIP "VNOR" $ do
    comment "Vector Logical NOR (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvnot (bvor (Loc vrA) (Loc vrB)))

  defineOpcodeWithIP "VOR" $ do
    comment "Vector Logical OR (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvor (Loc vrA) (Loc vrB))

  defineOpcodeWithIP "VXOR" $ do
    comment "Vector Logical XOR (VX-form)"
    (vrT, vrA, vrB) <- vxform3
    defLoc vrT (bvxor (Loc vrA) (Loc vrB))

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
