{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
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
  memri,
  memrr,
  directbrtarget,
  absdirectbrtarget,
  -- * Registers
  ip,
  lnk,
  ctr,
  cr,
  xer,
  memory,
  -- * Forms
  naturalBV,
  xoform3,
  xform3,
  xform2,
  dform,
  iform,
  -- * Shared
  naturalLitBV,
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
  sext,
  sext',
  zext,
  zext',
  -- * Uninterpreted Functions
  isR0,
  memriReg,
  memriOffset,
  memrrBaseReg,
  memrrOffsetReg,
  readMem
  -- memrixOffset,
  -- memrixReg
  ) where

import GHC.Stack ( HasCallStack )

import Prelude hiding ( concat )
import Data.Parameterized.Some ( Some(..) )
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

absdirectbrtarget :: String
absdirectbrtarget = "Absdirectbrtarget"

directbrtarget :: String
directbrtarget = "Directbrtarget"

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

memri :: String
memri = "Memri"

memrr :: String
memrr = "Memrr"

-- Registers

ip :: (?bitSize :: BitSize) => Location 'TBV
ip = LiteralLoc Literal { lName = "IP"
                        , lExprType = naturalBV
                        }

lnk :: (?bitSize :: BitSize) => Location 'TBV
lnk = LiteralLoc Literal { lName = "LNK"
                         , lExprType = naturalBV
                         }

ctr :: (?bitSize :: BitSize) => Location 'TBV
ctr = LiteralLoc Literal { lName = "CTR"
                         , lExprType = naturalBV
                         }

-- | The CR is always 32 bits
cr :: Location 'TBV
cr = LiteralLoc Literal { lName = "CR"
                        , lExprType = EBV 32
                        }

xer :: (?bitSize :: BitSize) => Location 'TBV
xer = LiteralLoc Literal { lName = "XER"
                         , lExprType = naturalBV
                         }

memory :: Location 'TMemory
memory = LiteralLoc Literal { lName = "Mem"
                            , lExprType = EMemory
                            }

-- Form helpers

naturalBV :: (?bitSize :: BitSize) => ExprType 'TBV
naturalBV = EBV (bitSizeValue ?bitSize)

xoform3 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
xoform3 = do
  rT <- param "rT" gprc naturalBV
  rA <- param "rA" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input rA
  input rB
  return (rT, rA, rB)

xform3 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
xform3 = do
  rA <- param "rA" gprc naturalBV
  rS <- param "rS" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input rS
  input rB
  return (rA, rS, rB)

xform2 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV)
xform2 = do
  rA <- param "rA" gprc naturalBV
  rS <- param "rS" gprc naturalBV
  input rS
  return (rA, rS)

dform :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
dform = do
  rT <- param "rT" gprc naturalBV
  rA <- param "rA" gprc_nor0 naturalBV
  si <- param "si" s16imm (EBV 16)
  input rA
  input si
  return (rT, rA, si)

iform :: (?bitSize :: BitSize) => String -> SemM 'Def (Location 'TBV)
iform tag = do
  target <- param "target" tag (EBV 24)
  input target
  return target

-- Helpers

cmpImm :: (HasCallStack, ?bitSize :: BitSize)
       => (Expr 'TBV -> Expr 'TBV -> Expr 'TBool)
       -- ^ LT
       -> (Expr 'TBV -> Expr 'TBV -> Expr 'TBool)
       -- ^ GT
       -> Expr 'TBV
       -- ^ The crrc field
       -> Expr 'TBV
       -- ^ The extended immediate (extended to full dword size)
       -> Expr 'TBV
       -- ^ The register expression
       -> Expr 'TBV
cmpImm lt gt fld ximm reg =
  bvor (Loc cr) shiftedNibble
  where
    c = ite (lt reg ximm)
            (LitBV 3 0b100)
            (ite (gt reg ximm)
                 (LitBV 3 0b010)
                 (LitBV 3 0b001))
    crnibble = concat c (xerBit SO (Loc xer))
    shiftedNibble = bvshl (zext' 32 crnibble) (bvmul (zext' 32 fld) (LitBV 32 0x4))

-- Common operations

naturalLitBV :: (?bitSize :: BitSize) => Integer -> Expr 'TBV
naturalLitBV n = LitBV (bitSizeValue ?bitSize) n

-- | Smart sign extend (extend to the full word width, i.e., up to the native
-- width of registers)
sext :: (HasCallStack, ?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV
sext = sext' (bitSizeValue ?bitSize)

-- | Generalized sign extension to arbitrary bit width
sext' :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
sext' fullWidth e
  | extendBy == 0 = e
  | otherwise = signExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e

-- | Zero extension to the full native bit width of registers
zext :: (HasCallStack, ?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV
zext = zext' (bitSizeValue ?bitSize)

-- | Generalized zero extension to arbitrary width
zext' :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
zext' fullWidth e
  | extendBy == 0 = e
  | otherwise = zeroExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e

-- Helpers for endianness isolation

data XERBit = OV
            | CA
            | SO
            deriving (Eq, Ord, Show)

xerBitNum :: (?bitSize :: BitSize) => XERBit -> Int
xerBitNum b =
  case b of
    SO -> bitSizeValue ?bitSize - 32 + 0
    OV -> bitSizeValue ?bitSize - 32 + 1
    CA -> bitSizeValue ?bitSize - 32 + 2

-- | Extract a named bit from the @XER@
xerBit :: (HasCallStack, ?bitSize :: BitSize) => XERBit -> Expr 'TBV -> Expr 'TBV
xerBit xb = extract (xerBitNum xb) (xerBitNum xb)

-- | Extract the @n@ low bits of a 64 bit register.
--
-- This is parameterized so that we can easily adjust the index numbering if we
-- have to in order to interface with crucible/macaw.  The bit numbering in PPC
-- is somewhat odd compared to other architectures.
lowBits64 :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
lowBits64 n = extract 63 (63 - n + 1)

lowBits32 :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
lowBits32 n = extract 31 (31 - n + 1)

lowBits128 :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
lowBits128 n = extract 127 (127 - n + 1)

-- | A wrapper around the two low bit extractors parameterized by bit size (it
-- selects the appropriate extractor based on architecture size)
lowBits :: (HasCallStack, ?bitSize :: BitSize) => Int -> Expr 'TBV -> Expr 'TBV
lowBits n e
  | ?bitSize == Size32 && n == 32 = e
  | ?bitSize == Size32 = lowBits32 n e
  | otherwise = lowBits64 n e

highBits64 :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
highBits64 n = extract (n - 1) 0

highBits32 :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
highBits32 n = extract (n - 1) 0

highBits128 :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
highBits128 n = extract (n - 1) 0

highBits :: (HasCallStack, ?bitSize :: BitSize) => Int -> Expr 'TBV -> Expr 'TBV
highBits n e
  | ?bitSize == Size32 && n == 32 = e
  | ?bitSize == Size32 = highBits32 n e
  | otherwise = highBits64 n e

-- Uninterpreted function helpers

-- | Extract the base register from a memrix field
-- memrixReg :: Expr tp -> Expr 'TBV
-- memrixReg = uf "memrix_reg" . (:[])

-- -- | Extract the offset (DS field) of a memrix memory access
-- memrixOffset :: Expr tp -> Expr 'TBV
-- memrixOffset e = uf  "memrix_offset" . ((:[]) . Some)

memriReg :: (?bitSize :: BitSize) => Location 'TMemRef -> Location 'TBV
memriReg = locUF naturalBV "memri_reg"

memriOffset :: Expr 'TMemRef
            -- ^ The memory ref expression
            -> Expr 'TBV
memriOffset = uf (EBV 16) "memri_offset" . ((:[]) . Some)

memrrBaseReg :: (?bitSize :: BitSize)
             => Expr 'TMemRef
             -> Expr 'TBV
memrrBaseReg = uf naturalBV "memrr_base" . ((:[]) . Some)

memrrOffsetReg :: (?bitSize :: BitSize)
               => Expr 'TMemRef
               -> Expr 'TBV
memrrOffsetReg = uf naturalBV "memrr_offset" . ((:[]) . Some)

-- | An uninterpreted function that converts a CR register field reference
-- (e.g. CR0) into a number.
--
-- Note that the result should be a 3 bit bitvector (representing field values
-- 0-7)
-- crToIndex :: Expr -> Expr
-- crToIndex = uf "cr_to_index" . (:[])

-- | Read from the pseudo-location "Memory"
readMem :: (?bitSize :: BitSize)
        => Expr 'TMemory
        -- ^ The memory
        -> Expr 'TBV
        -- ^ The effective address to load
        -> Int
        -- ^ The number of bytes
        -> Expr 'TBV
readMem mem ea nBytes =
  uf (EBV (8 * nBytes)) "read_mem" [Some mem, Some ea, Some (LitBV 32 (fromIntegral nBytes))]

-- | An uninterpreted function that tests if the argument is register zero
isR0 :: (HasCallStack) => Expr 'TBV -> Expr 'TBool
isR0 = uf EBool "is_r0" . ((:[]) . Some)
