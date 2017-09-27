{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
-- | The definitions of the base and manual sets of formulas
--
-- This is the set of definitions shared between PPC32 and PPC64
--
-- base and manual are specified separately so that we can store them in
-- different places (manual definitions are not included in the base set, since
-- we don't want to or cannot learn from them).
module SemMC.Architecture.PPC.Base (
  base,
  pseudo,
  manual
  ) where

import Prelude hiding ( concat )
import SemMC.DSL

-- Types

gprc :: String
gprc = "Gprc"

gprc_nor0 :: String
gprc_nor0 = "Gprc_nor0"

crrc :: String
crrc = "Crrc"

s16imm :: String
s16imm = "S16imm"

u2imm :: String
u2imm = "U2imm"

u4imm :: String
u4imm = "U4imm"

memrix :: String
memrix = "Memrix"

-- Registers

lnk :: String
lnk = "LNK"

ctr :: String
ctr = "CTR"

cr :: String
cr = "CR"

xer :: String
xer = "XER"

memory :: String
memory = "Mem"

-- Form helpers

xoform3 :: SemM 'Def (Parameter, Parameter, Parameter)
xoform3 = do
  rT <- param "rT" gprc
  rA <- param "rA" gprc
  rB <- param "rB" gprc
  input rA
  input rB
  return (rT, rA, rB)

xform3 :: SemM 'Def (Parameter, Parameter, Parameter)
xform3 = do
  rA <- param "rA" gprc
  rS <- param "rS" gprc
  rB <- param "rB" gprc
  input rS
  input rB
  return (rA, rS, rB)

dform :: SemM 'Def (Parameter, Parameter, Parameter)
dform = do
  rT <- param "rT" gprc
  rA <- param "rA" gprc_nor0
  si <- param "si" s16imm
  input rA
  input si
  return (rT, rA, si)

-- Defs

base :: Int -> [(String, Definition)]
base bitSize = runSem $ do
  defineOpcode "ADD4" $ do
    (rT, rA, rB) <- xoform3
    defLoc (ParamLoc rT) (bvadd (Param rA) (Param rB))
  defineOpcode "SUBF" $ do
    (rT, rA, rB) <- xoform3
    defLoc (ParamLoc rT) (bvsub (Param rB) (Param rA))
  defineOpcode "XOR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvxor (Param rS) (Param rB))
  defineOpcode "OR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvor (Param rS) (Param rB))
  defineOpcode "AND" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvand (Param rS) (Param rB))
  defineOpcode "NAND" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvand (Param rS) (Param rB)))
  defineOpcode "NOR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvor (Param rS) (Param rB)))
  defineOpcode "EQV" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvxor (Param rS) (Param rB)))
  defineOpcode "ANDC" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvand (Param rS) (bvnot (Param rB)))
  defineOpcode "ORC" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvor (Param rS) (bvnot (Param rB)))
  defineOpcode "SLD" $ do
    comment "Shift Left Doubleword (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 6 (Param rB)
    defLoc (ParamLoc rA)  (bvshl (Param rS) n)
  defineOpcode "SLW" $ do
    comment "Shift Left Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 5 (Param rB)
    defLoc (ParamLoc rA) (maskHigh32 (bvshl (Param rS) n))
  defineOpcode "SRD" $ do
    comment "Shift Right Doubleword (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 6 (Param rB)
    defLoc (ParamLoc rA) (bvlshr (Param rS) n)
  defineOpcode "SRW" $ do
    comment "Shift Right Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 5 (Param rB)
    defLoc (ParamLoc rA) (maskHigh32 (bvlshr (Param rS) n))
  defineOpcode "ADDI" $ do
    comment "Add Immediate (D-form)"
    comment "We hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    (rT, rA, si) <- dform
    let lhs = ite (isR0 (Param rA)) (LitBV bitSize 0x0) (Param rA)
    defLoc (ParamLoc rT) (bvadd lhs (sext bitSize 16 (Param si)))
  defineOpcode "ADDIS" $ do
    comment "Add Immediate Shifted (D-form)"
    comment "Like 'ADDI', we hand wrote this formula because it is one of the few that"
    comment "have special treatment of r0"
    (rT, rA, si) <- dform
    let lhs = ite (isR0 (Param rA)) (LitBV bitSize 0x0) (Param rA)
    let imm = concat (Param si) (LitBV 16 0x0)
    defLoc (ParamLoc rT) (bvadd lhs (sext bitSize 32 imm))
  defineOpcode "CMPDI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the double word variant (where L=1)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    let ximm = sext bitSize 16 (Param imm)
    let newCR = cmpImm bvslt bvsgt (Param fld) ximm (Param rA)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPWI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    let ximm = sext bitSize 16 (Param imm)
    let lowreg = if bitSize == 32 then Param rA else lowBits64 32 (Param rA)
    let newCR = cmpImm bvslt bvsgt (Param fld) ximm (sext bitSize 32 lowreg)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPD" $ do
    comment "Compare (X-form)"
    comment "Compare double word where L=1"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    let newCR = cmpImm bvslt bvsgt (Param fld) (Param rA) (Param rB)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLW" $ do
    comment "Compare (X-form)"
    comment "Compare word (where L=0)"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    let lowa = if bitSize == 32 then Param rA else lowBits64 32 (Param rA)
    let lowb = if bitSize == 32 then Param rB else lowBits64 32 (Param rB)
    let newCR = cmpImm bvslt bvsgt (Param fld) (zext bitSize 32 lowa) (zext bitSize 32 lowb)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLDI" $ do
    comment "Compare Logical Immediate (D-form)"
    comment "This variant is the double word variant (where L=1)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    let ximm = zext bitSize 16 (Param imm)
    let newCR = cmpImm bvult bvugt (Param fld) ximm (Param rA)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLWI" $ do
    comment "Compare Logical Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
    fld <- param "fld" crrc
    imm <- param "imm" s16imm
    rA <- param "rA" gprc
    input imm
    input rA
    let ximm = zext bitSize 16 (Param imm)
    let lowreg = if bitSize == 32 then Param rA else lowBits64 32 (Param rA)
    let newCR = cmpImm bvult bvugt (Param fld) ximm (zext bitSize 32 lowreg)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLD" $ do
    comment "Compare Logical (X-form)"
    comment "Compare double word (where L=1)"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    let newCR = cmpImm bvult bvugt (Param fld) (Param rA) (Param rB)
    defLoc (LiteralLoc cr) newCR
  defineOpcode "CMPLW" $ do
    comment "Compare Logical (X-form)"
    comment "Compare word (where L=0)"
    fld <- param "fld" crrc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    let lowa = if bitSize == 32 then Param rA else lowBits64 32 (Param rA)
    let lowb = if bitSize == 32 then Param rB else lowBits64 32 (Param rB)
    let newCR = cmpImm bvult bvugt (Param fld) (zext bitSize 32 lowa) (zext bitSize 32 lowb)
    defLoc (LiteralLoc cr) newCR
  return ()

cmpImm :: (Expr -> Expr -> Expr)
       -- ^ LT
       -> (Expr -> Expr -> Expr)
       -- ^ GT
       -> Expr
       -- ^ The crrc field
       -> Expr
       -- ^ The extended immediate (extended to full dword size)
       -> Expr
       -- ^ The register expression
       -> Expr
cmpImm lt gt fld ximm reg =
  bvor (Loc cr) shiftedNibble
  where
    c = ite (lt reg ximm)
            (LitBV 3 0b100)
            (ite (gt reg ximm)
                 (LitBV 3 0b010)
                 (LitBV 3 0b001))
    crnibble = concat c (xerBit SO (Loc xer))
    shiftedNibble = bvshl (zext 32 4 crnibble) (bvmul (zext 32 3 (crToIndex fld)) (LitBV 32 0x4))

pseudo :: Int -> [(String, Definition)]
pseudo bitSize = runSem $ do
  defineOpcode "Move" $ do
    target <- param "target" gprc
    source <- param "source" gprc_nor0
    input source
    defLoc (ParamLoc target) (Param source)
  defineOpcode "ExtractByteGPR" $ do
    target <- param "target" gprc
    source <- param "source" gprc
    n <- if bitSize == 32 then param "n" u2imm else param "n" u4imm
    input source
    input n
    let shiftAmount = bvshl (Param n) (LitBV bitSize 0x3)
    let shiftedInput = bvlshr (Param source) shiftAmount
    let bits = if bitSize == 32 then lowBits32 8 shiftedInput else lowBits64 8 shiftedInput
    let padding = if bitSize == 32 then LitBV 24 0x0 else LitBV 56 0x0
    defLoc (ParamLoc target) (concat padding bits)

manual :: Int -> [(String, Definition)]
manual bitSize = runSem $ do
  defineOpcode "MTLR" $ do
    rA <- param "rA" gprc
    input rA
    defLoc (LiteralLoc lnk) (Param rA)
  defineOpcode "MFLR" $ do
    rA <- param "rA" gprc
    defLoc (ParamLoc rA) (Loc lnk)
  defineOpcode "MTCTR" $ do
    rA <- param "rA" gprc
    input rA
    defLoc (LiteralLoc ctr) (Param rA)
  defineOpcode "MFCTR" $ do
    rA <- param "rA" gprc
    defLoc (ParamLoc rA) (Loc ctr)
  -- defineOpcode "LD" do
  --   rT <- param "rT" gprc
  --   memRef <- param "memRef" memrix
  --   let rA = memrixReg (Param memRef)
  --   let ds = memrixOffset (Param memRef)
  --   let b = ite (isR0 rA) (LitBV bitSize 0) rA
  --   let ea = bvadd b (sext bitSize 16 (concat ds (LitBV 2 0)))
  --   defLoc (ParamLoc rT) (Loc memory)
  return ()

-- Common operations

-- | Smart sign extend (extend to the full word width, which is a parameter)
sext :: Int -> Int -> Expr -> Expr
sext fullWidth valWidth e
  | extendBy == 0 = e
  | otherwise = signExtend extendBy e
  where
    extendBy = fullWidth - valWidth

zext :: Int -> Int -> Expr -> Expr
zext fullWidth valWidth e
  | extendBy == 0 = e
  | otherwise = zeroExtend extendBy e
  where
    extendBy = fullWidth - valWidth

-- Helpers for endianness isolation

data XERBit = OV
            | CA
            | SO
            deriving (Eq, Ord, Show)

xerBitNum :: XERBit -> Int
xerBitNum b =
  case b of
    OV -> 33
    SO -> 32
    CA -> 34

-- | Extract a named bit from the @XER@
xerBit :: XERBit -> Expr -> Expr
xerBit xb = extract (xerBitNum xb) (xerBitNum xb)

-- | Extract the @n@ low bits of a 64 bit register.
--
-- This is parameterized so that we can easily adjust the index numbering if we
-- have to in order to interface with crucible/macaw.  The bit numbering in PPC
-- is somewhat odd compared to other architectures.
lowBits64 :: Int -> Expr -> Expr
lowBits64 n = extract 63 (63 - n + 1)

lowBits32 :: Int -> Expr -> Expr
lowBits32 n = extract 31 (31 - n + 1)

-- | Mask out the high 32 bits of a 64 bit bitvector.
--
-- Again, this is factored out so that we can easily adjust the bit indexing if
-- necessary.
maskHigh32 :: Expr -> Expr
maskHigh32 = bvand (LitBV 64 0xFFFF0000)

-- Uninterpreted function helpers

-- | Extract the base register from a memrix field
memrixReg :: Expr -> Expr
memrixReg = uf "memrix_reg" . (:[])

-- | Extract the offset (DS field) of a memrix memory access
memrixOffset :: Expr -> Expr
memrixOffset = uf "memrix_offset" . (:[])

-- | An uninterpreted function that converts a CR register field reference
-- (e.g. CR0) into a number.
--
-- Note that the result should be a 3 bit bitvector (representing field values
-- 0-7)
crToIndex :: Expr -> Expr
crToIndex = uf "cr_to_index" . (:[])


-- | An uninterpreted function that tests if the argument is zero
isR0 :: Expr -> Expr
isR0 = uf "is_r0" . (:[])

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
