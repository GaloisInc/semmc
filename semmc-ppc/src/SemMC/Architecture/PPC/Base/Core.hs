{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
module SemMC.Architecture.PPC.Base.Core (
  BitSize(..),
  bitSizeValue,
  -- * PPC Types
  gprc,
  gprc_nor0,
  g8rc,
  crrc,
  s16imm,
  u2imm,
  u4imm,
  memrix,
  -- * Registers
  lnk,
  ctr,
  cr,
  xer,
  memory,
  -- * Forms
  xoform3,
  xform3,
  dform,
  -- * Shared
  cmpImm,
  highBits,
  highBits32,
  highBits64,
  highBits128,
  lowBits,
  lowBits32,
  lowBits64,
  lowBits128,
  XERBit(..),
  xerBit,
  maskHigh32,
  sext,
  sext',
  zext,
  zext',
  -- * Uninterpreted Functions
  isR0,
  memrixOffset,
  memrixReg
  ) where

import Prelude hiding ( concat )
import SemMC.DSL

data BitSize = Size32
             | Size64
             deriving (Eq, Show, Read)

bitSizeValue :: BitSize -> Int
bitSizeValue Size32 = 32
bitSizeValue Size64 = 64

-- PPC Types

gprc :: String
gprc = "Gprc"

gprc_nor0 :: String
gprc_nor0 = "Gprc_nor0"

g8rc :: String
g8rc = "G8rc"

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
    shiftedNibble = bvshl (zext' 32 4 crnibble) (bvmul (zext' 32 3 fld) (LitBV 32 0x4))
-- Common operations

-- | Smart sign extend (extend to the full word width, which is a parameter) up
-- to the native width of registers
sext :: BitSize -> Int -> Expr -> Expr
sext bs = sext' (bitSizeValue bs)

-- | Generalized sign extension to arbitrary bit width
sext' :: Int -> Int -> Expr -> Expr
sext' fullWidth valWidth e
  | extendBy == 0 = e
  | otherwise = signExtend extendBy e
  where
    extendBy = fullWidth - valWidth

-- | Zero extension to the full native bit width of registers
zext :: BitSize -> Int -> Expr -> Expr
zext bs = zext' (bitSizeValue bs)

-- | Generalized zero extension to arbitrary width
zext' :: Int -> Int -> Expr -> Expr
zext' fullWidth valWidth e
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

lowBits128 :: Int -> Expr -> Expr
lowBits128 n = extract 128 (128 - n + 1)

-- | A wrapper around the two low bit extractors parameterized by bit size (it
-- selects the appropriate extractor based on architecture size)
lowBits :: BitSize -> Int -> Expr -> Expr
lowBits bitSize n e
  | bitSize == Size32 && n == 32 = e
  | bitSize == Size32 = lowBits32 n e
  | otherwise = lowBits64 n e

highBits64 :: Int -> Expr -> Expr
highBits64 n = extract 0 (n - 1)

highBits32 :: Int -> Expr -> Expr
highBits32 n = extract 0 (n - 1)

highBits128 :: Int -> Expr -> Expr
highBits128 n = extract 0 (n - 1)

highBits :: BitSize -> Int -> Expr -> Expr
highBits bitSize n e
  | bitSize == Size32 && n == 32 = e
  | bitSize == Size32 = highBits32 n e
  | otherwise = highBits64 n e

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
-- crToIndex :: Expr -> Expr
-- crToIndex = uf "cr_to_index" . (:[])


-- | An uninterpreted function that tests if the argument is zero
isR0 :: Expr -> Expr
isR0 = uf "is_r0" . (:[])
