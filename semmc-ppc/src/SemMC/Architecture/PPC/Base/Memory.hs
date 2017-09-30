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
  defineOpcode "LHZ" $ do
    comment "Load Halfword and Zero (D-form)"
    loadAndExtend 2 zext
  defineOpcode "LWZ" $ do
    comment "Load Word and Zero (D-form)"
    loadAndExtend 4 zext

  defineOpcode "LBZU" $ do
    comment "Load Byte and Zero with Update (D-form)"
    loadAndUpdate 1 zext
  defineOpcode "LHZU" $ do
    comment "Load Halfword and Zero with Update (D-form)"
    loadAndUpdate 2 zext
  defineOpcode "LWZU" $ do
    comment "Load Word and Zero with Update (D-form)"
    loadAndUpdate 4 zext

  defineOpcode "LHAU" $ do
    comment "Load Halfword Algebraic with Update (D-form)"
    loadAndUpdate 2 sext

  defineOpcode "LHA" $ do
    comment "Load Halfword Algebraic (D-form)"
    loadAndExtend 2 sext

  defineOpcode "LBZX" $ do
    comment "Load Byte and Zero Indexed (X-form)"
    loadAndZeroIndexed 1
  defineOpcode "LHZX" $ do
    comment "Load Halfword and Zero Indexed (X-form)"
    loadAndZeroIndexed 2
  defineOpcode "LWZX" $ do
    comment "Load Word and Zero Indexed (X-form)"
    loadAndZeroIndexed 4

  when (?bitSize == Size64) $ do
    defineOpcode "LDX" $ do
      comment "Load Doubleword Indexed (X-form)"
      loadAndZeroIndexed 8

  -- when (?bitSize == Size64) $ do
  --   defineOpcode "LD" $ do
  --     -- Note that this is not the same as the L*Z forms above; the displacement
  --     -- from the base is shorter (it can be due to alignment).
  --     comment "Load Doubleword (DS-form)"

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
  let disp = memriOffset (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext disp)
  defLoc rT (extend (readMem (Loc memory) ea nBytes))

-- | This variant computes the effective address based on registers (instead of
-- an immediate displacement).
--
-- This is listed as X-form, but uses different variable names.  Also, dismantle
-- groups the second two operands into a single @Memrr@
loadAndZeroIndexed :: (?bitSize :: BitSize) => Int -> SemM 'Def ()
loadAndZeroIndexed nBytes = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg (Loc memref)
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 rA) (naturalLitBV 0x0) rA
  let ea = bvadd b rB
  defLoc rT (zext (readMem (Loc memory) ea nBytes))

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
  let disp = memriOffset (Loc memref)
  let ea = bvadd (Loc rA) (sext disp)
  defLoc rT (extend (readMem (Loc memory) ea nBytes))
  defLoc rA ea
