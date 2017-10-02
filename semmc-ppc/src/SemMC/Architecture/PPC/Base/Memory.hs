{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Architecture.PPC.Base.Memory (
  manualMemory
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

manualMemory :: (?bitSize :: BitSize) => SemM 'Top ()
manualMemory = do
  defineOpcode "LBZ" $ do
    comment "Load Byte and Zero (D-form)"
    loadAndExtend 1 zext
  defineOpcode "LBZU" $ do
    comment "Load Byte and Zero with Update (D-form)"
    loadAndUpdate 1 zext
  defineOpcode "LBZX" $ do
    comment "Load Byte and Zero Indexed (X-form)"
    loadIndexed 1 zext
  defineOpcode "LBZUX" $ do
    comment "Load Byte and Zero with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 1 zext

  defineOpcode "LHZ" $ do
    comment "Load Halfword and Zero (D-form)"
    loadAndExtend 2 zext
  defineOpcode "LHZU" $ do
    comment "Load Halfword and Zero with Update (D-form)"
    loadAndUpdate 2 zext
  defineOpcode "LHZX" $ do
    comment "Load Halfword and Zero Indexed (X-form)"
    loadIndexed 2 zext
  defineOpcode "LHZUX" $ do
    comment "Load Halfword and Zero with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 2 zext

  defineOpcode "LHAU" $ do
    comment "Load Halfword Algebraic with Update (D-form)"
    loadAndUpdate 2 sext
  defineOpcode "LHA" $ do
    comment "Load Halfword Algebraic (D-form)"
    loadAndExtend 2 sext
  defineOpcode "LHAX" $ do
    comment "Load Halfword Algebraic Indexed (X-form)"
    loadIndexed 2 sext
  defineOpcode "LHAUX" $ do
    comment "Load Halfword Algebraic with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 2 sext

  defineOpcode "LWZ" $ do
    comment "Load Word and Zero (D-form)"
    loadAndExtend 4 zext
  defineOpcode "LWZU" $ do
    comment "Load Word and Zero with Update (D-form)"
    loadAndUpdate 4 zext
  defineOpcode "LWZX" $ do
    comment "Load Word and Zero Indexed (X-form)"
    loadIndexed 4 zext
  defineOpcode "LWZUX" $ do
    comment "Load Word and Zero with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 4 zext

  defineOpcode "LWA" $ do
    comment "Load Word Algebraic (DS-form)"
    loadAndExtendDS 4 sext
  defineOpcode "LWAX" $ do
    comment "Load Word Algebraic Indexed (X-form)"
    loadIndexed 4 sext
  defineOpcode "LWAUX" $ do
    comment "Load Word Algebraic with Update Indexed (X-form)"
    loadAndExtendWithUpdateIndexed 4 sext

  -- The 64 bit variants never need extension, so we use id for the extension function.
  when (?bitSize == Size64) $ do
    defineOpcode "LD" $ do
      comment "Load Doubleword (DS-form)"
      loadAndExtendDS 8 id
    defineOpcode "LDX" $ do
      comment "Load Doubleword Indexed (X-form)"
      loadIndexed 8 id
    defineOpcode "LDUX" $ do
      comment "Load Doubleword and Update Indexed (X-form)"
      loadAndExtendWithUpdateIndexed 8 id


-- | Define a load and zero of the given number of bytes.
loadAndExtend :: (?bitSize :: BitSize)
              => Int
              -- ^ Number of bytes
              -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
              -- ^ Extension function for the result (zext for the Zero variants,
              -- sext for the Algebraic variants)
              -> SemM 'Def ()
loadAndExtend nBytes extend = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memri EMemRef
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext disp)
  defLoc rT (extend (readMem (Loc memory) ea nBytes))

-- | This is a variant of 'loadAndExtend' where the displacement is concatenated
-- on the right by two zeros.  These are the DS-forms that are used for LWA and
-- LD only, for some reason.
loadAndExtendDS :: (?bitSize :: BitSize)
                => Int
                -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                -> SemM 'Def ()
loadAndExtendDS nBytes extend = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memri EMemRef
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 14 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext (concat disp (LitBV 2 0x0)))
  defLoc rT (extend (readMem (Loc memory) ea nBytes))


-- | This variant computes the effective address based on registers (instead of
-- an immediate displacement).
--
-- This is listed as X-form, but uses different variable names.  Also, dismantle
-- groups the second two operands into a single @Memrr@
loadIndexed :: (?bitSize :: BitSize)
            => Int
            -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
            -> SemM 'Def ()
loadIndexed nBytes ext = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b rB
  defLoc rT (ext (readMem (Loc memory) ea nBytes))

-- | This is separate from 'loadIndexed' because it doesn't have special
-- treatment of the case where rA is r0.
--
-- NOTE: There are special conditions: rA == r0 is invalid, and rA == rT is
-- invalid.  Do we want to somehow make that explicit?
loadAndExtendWithUpdateIndexed :: (?bitSize :: BitSize)
                               => Int
                               -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                               -> SemM 'Def ()
loadAndExtendWithUpdateIndexed nBytes ext = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let ea = bvadd (Loc rA) rB
  defLoc rT (ext (readMem (Loc memory) ea nBytes))
  defLoc rA ea

loadAndUpdate :: (?bitSize :: BitSize)
              => Int
              -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
              -> SemM 'Def ()
loadAndUpdate nBytes extend = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memri EMemRef
  input memory
  input memref
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let ea = bvadd (Loc rA) (sext disp)
  defLoc rT (extend (readMem (Loc memory) ea nBytes))
  defLoc rA ea
