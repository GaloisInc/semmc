{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
-- | This module provides helpers to define PowerPC memory operations
--
-- These helpers are mostly used in the 'SemMC.Architecture.PPC.Base.Memory'
-- module; however, they are also needed to define some of the synchronzied
-- instructions, which are defined separately.
module SemMC.Architecture.PPC.Base.Core.Memory (
  loadAndExtend,
  loadAndExtendDS,
  loadIndexed,
  loadAndExtendWithUpdateIndexed,
  loadAndUpdate,
  loadAndUpdateDS,
  store,
  storeDS,
  storeWithUpdate,
  storeWithUpdateDS,
  storeIndexed,
  storeWithUpdateIndexed,
  reverseBytes
  ) where

import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

-- | Define a load and zero of the given number of bytes (D-form)
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
  memref <- param "memref" memrix EMemRef
  input memref
  input memory
  let rA = memrixReg memref
  let disp = memrixOffset 14 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext (concat disp (LitBV 2 0x0)))
  defLoc rT (extend (readMem (Loc memory) ea nBytes))


-- | This variant computes the effective address based on registers (instead of
-- an immediate displacement).
--
-- This is listed as X-form, but uses different variable names.  Also, dismantle
-- groups the second two operands into a single @Memrr@
--
-- Also specify a transformation (the @xform@ parameter) that is applied to the
-- value pre-extension before it is loaded.
loadIndexed :: (?bitSize :: BitSize)
            => Int
            -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
            -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
            -> SemM 'Def ()
loadIndexed nBytes xform ext = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b rB
  defLoc rT (ext (xform (readMem (Loc memory) ea nBytes)))

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

loadAndUpdateDS :: (?bitSize :: BitSize)
                => Int
                -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                -> SemM 'Def ()
loadAndUpdateDS nBytes extend = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memrix EMemRef
  input memory
  input memref
  let rA = memrixReg memref
  let disp = memrixOffset 14 (Loc memref)
  let ea = bvadd (Loc rA) (sext (concat disp (LitBV 2 0x0)))
  defLoc rT (extend (readMem (Loc memory) ea nBytes))
  defLoc rA ea


-- | Define a store of @n@ bytes (D-form)
store :: (?bitSize :: BitSize)
      => Int
      -> SemM 'Def ()
store nBytes = do
  memref <- param "memref" memri EMemRef
  rS <- param "rS" gprc naturalBV
  input rS
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext disp)
  defLoc memory (storeMem (Loc memory) ea nBytes (lowBits (8 * nBytes) (Loc rS)))

-- | Define a store of @n@ bytes (DS-form)
--
-- This is just used for STD
storeDS :: (?bitSize :: BitSize)
        => Int
        -> SemM 'Def ()
storeDS nBytes = do
  memref <- param "memref" memrix EMemRef
  rS <- param "rS" gprc naturalBV
  input rS
  input memref
  input memory
  let rA = memrixReg memref
  let disp = memrixOffset 14 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext (concat disp (LitBV 2 0x0)))
  defLoc memory (storeMem (Loc memory) ea nBytes (lowBits (8 * nBytes) (Loc rS)))

storeWithUpdate :: (?bitSize :: BitSize)
                => Int
                -> SemM 'Def ()
storeWithUpdate nBytes = do
  memref <- param "memref" memri EMemRef
  rS <- param "rS" gprc naturalBV
  input rS
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let ea = bvadd (Loc rA) (sext disp)
  defLoc memory (storeMem (Loc memory) ea nBytes (lowBits (8 * nBytes) (Loc rS)))
  defLoc rA ea

storeWithUpdateDS :: (?bitSize :: BitSize)
                  => Int
                  -> SemM 'Def ()
storeWithUpdateDS nBytes = do
  memref <- param "memref" memrix EMemRef
  rS <- param "rS" gprc naturalBV
  input rS
  input memref
  input memory
  let rA = memrixReg memref
  let disp = memrixOffset 14 (Loc memref)
  let ea = bvadd (Loc rA) (sext (concat disp (LitBV 2 0x0)))
  defLoc memory (storeMem (Loc memory) ea nBytes (lowBits (8 * nBytes) (Loc rS)))
  defLoc rA ea

-- | Store indexed (the inverse of 'loadIndexed').  As with 'loadIndexed', a
-- transformation is applied to the bitvector before it is stored.  This
-- transformation is used to accommodate the "reversed" variants of store.
storeIndexed :: (?bitSize :: BitSize)
             => Int
             -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
             -> SemM 'Def ()
storeIndexed nBytes xform = do
  memref <- param "memref" memrr EMemRef
  rS <- param "rS" gprc naturalBV
  input rS
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b rB
  defLoc memory (storeMem (Loc memory) ea nBytes (xform (lowBits (8 * nBytes) (Loc rS))))

storeWithUpdateIndexed :: (?bitSize :: BitSize)
                       => Int
                       -> SemM 'Def ()
storeWithUpdateIndexed nBytes = do
  memref <- param "memref" memrr EMemRef
  rS <- param "rS" gprc naturalBV
  input rS
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let ea = bvadd (Loc rA) rB
  defLoc memory (storeMem (Loc memory) ea nBytes (lowBits (8 * nBytes) (Loc rS)))
  defLoc rA ea

-- | Given an @N@ byte word, reverse the bytes contained in the word
--
-- @N@ does not need to be specified.  It is inferred from the length of the
-- input bit vector.  The function raises an error if the bit size is not
-- divisible by 8.
reverseBytes :: (HasCallStack, ?bitSize :: BitSize)
             => Expr 'TBV
             -> Expr 'TBV
reverseBytes e
  | remBits /= 0 = error ("Bit size " ++ show (exprBVSize e) ++ " is not divisible by 8")
  | otherwise = foldr1 concat (go [] 0)
  where
    (nBytes, remBits) = exprBVSize e `divMod` 8
    go acc lowIdx
      | lowIdx >= nBytes * 8 = acc
      | otherwise = go (extract (lowIdx + 7) lowIdx e : acc) (lowIdx + 8)
