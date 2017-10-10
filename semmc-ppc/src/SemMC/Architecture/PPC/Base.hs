{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
-- | The definitions of the base and manual sets of formulas
--
-- This is the set of definitions shared between PPC32 and PPC64
--
-- base and manual are specified separately so that we can store them in
-- different places (manual definitions are not included in the base set, since
-- we don't want to or cannot learn from them).
module SemMC.Architecture.PPC.Base (
  BitSize(..),
  base,
  pseudo,
  manual
  ) where

import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core
import SemMC.Architecture.PPC.Base.Arithmetic
import SemMC.Architecture.PPC.Base.Bitwise
import SemMC.Architecture.PPC.Base.Branch
import SemMC.Architecture.PPC.Base.Compare
import SemMC.Architecture.PPC.Base.Memory
import SemMC.Architecture.PPC.Base.Special

-- Defs

base :: BitSize -> [(String, Definition)]
base bitSize = runSem $ do
  let ?bitSize = bitSize
  baseArithmetic
  baseBitwise
  baseCompare
  baseSpecial

pseudo :: BitSize -> [(String, Definition)]
pseudo bitSize = runSem $ do
  let ?bitSize = bitSize
  defineOpcode "Move" $ do
    target <- param "target" gprc naturalBV
    source <- param "source" gprc_nor0 naturalBV
    input source
    defLoc target (Loc source)
  defineOpcode "ExtractByteGPR" $ do
    target <- param "target" gprc naturalBV
    source <- param "source" gprc naturalBV
    n <- if bitSize == Size32 then param "n" u2imm (EBV 2) else param "n" u4imm (EBV 4)
    input source
    input n
    let shiftAmount = bvshl (zext (Loc n)) (naturalLitBV 0x3)
    let shiftedInput = bvlshr (Loc source) shiftAmount
    let bits = lowBits 8 shiftedInput
    let padding = LitBV (bitSizeValue bitSize - 8) 0x0
    defLoc target (concat padding bits)
  defineOpcode "SetSignedCR0" $ do
    comment "SetCR0"
    comment "This pseudo-opcode sets the value of CR0 based on a comparison"
    comment "of the value in the input register against zero, as in CMPDI or CMPWI"
    rA <- param "rA" g8rc naturalBV
    input cr
    input xer
    input rA
    let ximm = naturalLitBV 0x0
    let newCR = cmpImm bvslt bvsgt (LitBV 3 0x0) ximm (Loc rA)
    defLoc cr newCR
  return ()

manual :: BitSize -> [(String, Definition)]
manual bitSize = runSem $ do
  let ?bitSize = bitSize
  manualBranch
  manualMemory
  defineOpcode "MTLR" $ do
    rA <- param "rA" gprc naturalBV
    input rA
    defLoc lnk (Loc rA)
  defineOpcode "MFLR" $ do
    rA <- param "rA" gprc naturalBV
    input lnk
    defLoc rA (Loc lnk)
  defineOpcode "MTCTR" $ do
    rA <- param "rA" gprc naturalBV
    input rA
    defLoc ctr (Loc rA)
  defineOpcode "MFCTR" $ do
    rA <- param "rA" gprc naturalBV
    input ctr
    defLoc rA (Loc ctr)
  return ()


{- Note [PPC Condition Register (CR)]

The CR is 32 bits, which are usually addressed as eight 4-bit fields (CR0-CR7).

 - CR0 is set as the implicit result of many fixed point operations
 - CR1 is set as the implicit result of many floating point operations
 - CR fields can be set as the result of various compare instructions

Individual field bits are assigned based on comparison of the result of an
operation (R) to a constant (C) (either via the compare instructions *or*
against 0 when CR0 is implicitly set) as follows:

| Bit | Name | Description               |
|-----+------+---------------------------|
|   0 | LT   | True if R < C             |
|   1 | GT   | True if R > C             |
|   2 | EQ   | True if R = C             |
|   3 | SO   | Summary Overflow from XER |

If we write out the semantics for the basic compare instructions, we should be
in pretty good shape.  We can then write a specialized compare instruction as an
intrinsic with its constant fixed to zero.  That would let us learn most of the
dotted variants.

-}
