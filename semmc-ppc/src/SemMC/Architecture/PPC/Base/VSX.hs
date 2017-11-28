{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
-- | Semantics for the Vector Scalar Extensions (VSX)
--
-- This extends the number of vector registers to 64, of which 32 alias the
-- AltiVec registers.  They are still 128 bits.
module SemMC.Architecture.PPC.Base.VSX (
  baseVSX
  ) where

import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core
import SemMC.Architecture.PPC.Base.FP

-- | Definitions of vector instructions
--
-- FIXME: For now, these are all stubs that leave their destination register as
-- undefined.
baseVSX :: (?bitSize :: BitSize) => SemM 'Top ()
baseVSX = do
  vsxLoad
  vsxStore

effectiveAddress :: (?bitSize :: BitSize) => Location 'TMemRef -> Expr 'TBV
effectiveAddress memref =
  let rA = memrrBaseReg memref
      rB = memrrOffsetReg (Loc memref)
      b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  in (bvadd rB b)

storeForm :: SemM 'Def (Location 'TBV, Location 'TMemRef)
storeForm = do
  memref <- param "memref" memrr EMemRef
  xS <- param "xS" vsrc vectorBV
  input xS
  input memory
  input memref
  return (xS, memref)

vsxStore :: (?bitSize :: BitSize) => SemM 'Top ()
vsxStore = do
  defineOpcodeWithIP "STXSDX" $ do
    comment "Store VSX Scalar Doubleword Indexed (XX1-form)"
    (xS, memref) <- storeForm
    let ea = effectiveAddress memref
    defLoc memory (storeMem (Loc memory) ea 8 (highBits128 64 (Loc xS)))

loadForm :: SemM 'Def (Location 'TBV, Location 'TMemRef)
loadForm = do
  xT <- param "xT" vsrc vectorBV
  memref <- param "memref" memrr EMemRef
  input memory
  input memref
  return (xT, memref)

vsxLoad :: (?bitSize :: BitSize) => SemM 'Top ()
vsxLoad = do
  defineOpcodeWithIP "LXSDX" $ do
    comment "Load VSX Scalar Doubleword Indexed (XX1-form)"
    (xT, memref) <- loadForm
    let ea = effectiveAddress memref
    defLoc xT (concat (readMem (Loc memory) ea 8) (undefinedBV 64))

  defineOpcodeWithIP "LXSIWAX" $ do
    comment "Load VSX Scalar as Integer Word Algebraic Indexed (XX1-form)"
    (xT, memref) <- loadForm
    let ea = effectiveAddress memref
    defLoc xT (concat (sext' 64 (readMem (Loc memory) ea 4)) (undefinedBV 64))

  defineOpcodeWithIP "LXSIWZX" $ do
    comment "Load VSX Scalar as Integer Word and Zero Indexed (XX1-form)"
    (xT, memref) <- loadForm
    let ea = effectiveAddress memref
    defLoc xT (concat (zext' 64 (readMem (Loc memory) ea 4)) (undefinedBV 64))

  defineOpcodeWithIP "LXSSPX" $ do
    comment "Load VSX Scalar Single-Precision Indexed (XX1-form)"
    (xT, memref) <- loadForm
    let ea = effectiveAddress memref
    defLoc xT (concat (fsingletodouble (readMem (Loc memory) ea 4)) (undefinedBV 64))

  defineOpcodeWithIP "LXVD2X" $ do
    comment "Load VSX Vector Doubleword*2 Indexed (XX1-form)"
    (xT, memref) <- loadForm
    let ea = effectiveAddress memref
    defLoc xT (concat (readMem (Loc memory) ea 8) (readMem (Loc memory) (bvadd ea (naturalLitBV 0x8)) 8))

  defineOpcodeWithIP "LXVDSX" $ do
    comment "Load VSX Vector Doubleword & Splat Indexed (XX1-form)"
    (xT, memref) <- loadForm
    let ea = effectiveAddress memref
    let val = readMem (Loc memory) ea 8
    defLoc xT (concat val val)

  defineOpcodeWithIP "LXVW4X" $ do
    comment "Load VSX Vector Word*4 Indexed (XX1-form)"
    (xT, memref) <- loadForm
    let ea = effectiveAddress memref
    let w1 = readMem (Loc memory) ea 4
    let w2 = readMem (Loc memory) (bvadd ea (naturalLitBV 0x4)) 4
    let w3 = readMem (Loc memory) (bvadd ea (naturalLitBV 0x8)) 4
    let w4 = readMem (Loc memory) (bvadd ea (naturalLitBV 0x12)) 4
    defLoc xT (concat w1 (concat w2 (concat w3 w4)))
