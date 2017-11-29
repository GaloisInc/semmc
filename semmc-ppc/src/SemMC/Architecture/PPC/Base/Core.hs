{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Core (
  BitSize(..),
  bitSizeValue,
  -- * PPC Types
  gprc,
  gprc_nor0,
  fprc,
  vrrc,
  vsrc,
  crrc,
  crbitrc,
  crbitm,
  s16imm,
  s16imm64,
  s17imm,
  u2imm,
  u4imm,
  u5imm,
  u6imm,
  u16imm,
  u16imm64,
  memrix,
  memri,
  memrr,
  directbrtarget,
  absdirectbrtarget,
  condbrtarget,
  calltarget,
  abscalltarget,
  -- * Registers
  ip,
  lnk,
  ctr,
  cr,
  fpscr,
  xer,
  memory,
  -- * IP Wrapper
  defineOpcodeWithIP,
  defineRCVariant,
  defineVRCVariant,
  -- * Forms
  naturalBV,
  vectorBV,
  xlformcr,
  mdform4,
  mdsform4,
  mform5i,
  mform5r,
  xoform3,
  xoform2,
  xform3,
  xform2,
  xform2f,
  dform,
  dformr0,
  dformu,
  iform,
  aform,
  aform4,
  vxform3,
  vxform3u,
  vxform2s,
  vxform2,
  vaform,
  vaform4u,
  -- * Shared
  naturalLitBV,
  cmpImm,
  highBits,
  highBits32,
  highBits64,
  highBits128,
  highBits',
  lowBits,
  lowBits32,
  lowBits64,
  lowBits128,
  lowBits',
  XERBit(..),
  xerBit,
  updateXER,
  sext,
  sext',
  zext,
  zext',
  rotl,
  mask,
  crField,
  updateCRField,
  -- * Uninterpreted Functions
  isR0,
  memriReg,
  memriOffset,
  memrixReg,
  memrixOffset,
  memrrBaseReg,
  memrrOffsetReg,
  storeMem,
  readMem
  -- memrixOffset,
  -- memrixReg
  ) where

import GHC.Stack ( HasCallStack )

import Prelude hiding ( concat )
import Text.Printf ( printf )
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

fprc :: String
fprc = "Fprc"

vrrc :: String
vrrc = "Vrrc"

vsrc :: String
vsrc = "Vsrc"

absdirectbrtarget :: String
absdirectbrtarget = "Absdirectbrtarget"

directbrtarget :: String
directbrtarget = "Directbrtarget"

abscalltarget :: String
abscalltarget = "Abscalltarget"

calltarget :: String
calltarget = "Calltarget"

condbrtarget :: String
condbrtarget = "Condbrtarget"

crrc :: String
crrc = "Crrc"

crbitrc :: String
crbitrc = "Crbitrc"

crbitm :: String
crbitm = "Crbitm"

s5imm :: String
s5imm = "S5imm"

s16imm :: String
s16imm = "S16imm"

s17imm :: String
s17imm = "S17imm"

s16imm64 :: String
s16imm64 = "S16imm64"

u2imm :: String
u2imm = "U2imm"

u4imm :: String
u4imm = "U4imm"

u5imm :: String
u5imm = "U5imm"

u6imm ::  String
u6imm = "U6imm"

u16imm :: String
u16imm = "U16imm"

u16imm64 :: String
u16imm64 = "U16imm64"

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

-- | The FPSCR is always 32 bits
fpscr :: Location 'TBV
fpscr = LiteralLoc Literal { lName = "FPSCR"
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

-- IP Helper Wrapper

-- | A wrapper around 'defineOpcode' that updates the IP after the instruction
-- executes (simply by adding 4).
defineOpcodeWithIP :: (?bitSize :: BitSize) => String -> SemM 'Def () -> SemM 'Top ()
defineOpcodeWithIP name def =
  defineOpcode name $ do
    input ip
    defLoc ip (bvadd (Loc ip) (naturalLitBV 0x4))
    def

-- | Fork the definition and define a variant that implements the behavior of
-- RC=1 in instructions
--
-- The CR0 register is set as if the given value is compared against zero as in
-- compare with immediate.  The low three bits of CR0 are set by comparison
-- against zero and the fourth bit is copied from XER.
defineRCVariant :: (?bitSize :: BitSize) => String -> Expr 'TBV -> SemM 'Def () ->  SemM 'Def ()
defineRCVariant newName modifiedReg def = do
  forkDefinition newName $ do
    input cr
    input xer
    defLoc cr (cmpImm bvslt bvsgt (LitBV 3 0x0) (naturalLitBV 0x0) modifiedReg)
    def

-- | Like 'defineRCVariant', but for vector instructions, which modify CR6
-- instead of CR0.
--
-- FIXME: Right now, we clobber the entire CR with undefined bits.
defineVRCVariant :: (?bitSize :: BitSize) => String -> Expr 'TBV -> SemM 'Def () ->  SemM 'Def ()
defineVRCVariant newName _modifiedReg def = do
  forkDefinition newName $ do
    input cr
    input xer
    defLoc cr (undefinedBV 32)
    def

-- Form helpers

naturalBV :: (?bitSize :: BitSize) => ExprType 'TBV
naturalBV = EBV (bitSizeValue ?bitSize)

vectorBV :: ExprType 'TBV
vectorBV = EBV 128

-- | The XL-form for the CR logical operations
xlformcr :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
xlformcr = do
  bt <- param "BT" crbitrc (EBV 5)
  ba <- param "BA" crbitrc (EBV 5)
  bb <- param "BB" crbitrc (EBV 5)
  input ba
  input bb
  input cr
  return (bt, ba, bb)

-- | The M-form for RLWINM with three 5 bit immediates
mform5i :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mform5i = do
  rA <- param "rA" gprc naturalBV
  sh <- param "sh" u5imm (EBV 5)
  mb <- param "mb" u5imm (EBV 5)
  me <- param "me" u5imm (EBV 5)
  rS <- param "rS" gprc naturalBV
  input sh
  input mb
  input me
  input rS
  return (rA, sh, mb, me, rS)

mform5r :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mform5r = do
  rA <- param "rA" gprc naturalBV
  mb <- param "mb" u5imm (EBV 5)
  me <- param "me" u5imm (EBV 5)
  rS <- param "rS" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input rB
  input mb
  input me
  input rS
  return (rA, mb, me, rS, rB)


mdform4 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mdform4 = do
  rA <- param "rA" gprc naturalBV
  sh <- param "sh" u6imm (EBV 6)
  mb <- param "mb" u6imm (EBV 6)
  rS <- param "rS" gprc naturalBV
  input rS
  input sh
  input mb
  return (rA, sh, mb, rS)

mdsform4 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mdsform4 = do
  rA <- param "rA" gprc naturalBV
  mb <- param "mb" u6imm (EBV 6)
  rS <- param "rS" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input mb
  input rS
  input rB
  return (rA, mb, rS, rB)

xoform3 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
xoform3 = do
  rT <- param "rT" gprc naturalBV
  rA <- param "rA" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input rA
  input rB
  return (rT, rA, rB)

vxform2s :: SemM 'Def (Location 'TBV, Location 'TBV)
vxform2s = do
  vrT <- param "vrT" vrrc vectorBV
  sim <- param "sim" s5imm (EBV 5)
  return (vrT, sim)

vxform3u :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
vxform3u = do
  vrT <- param "vrT" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  uim <- param "uim" u5imm (EBV 5)
  input vrB
  return (vrT, vrB, uim)

vxform3 :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
vxform3 = do
  vrT <- param "vrT" vrrc vectorBV
  vrA <- param "vrA" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  input vrA
  input vrB
  return (vrT, vrA, vrB)

vxform2 :: SemM 'Def (Location 'TBV, Location 'TBV)
vxform2 = do
  vrT <- param "vrT" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  input vrB
  return (vrT, vrB)

vaform :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
vaform = do
  vrT <- param "vrT" vrrc vectorBV
  vrA <- param "vrA" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  vrC <- param "vrC" vrrc vectorBV
  input vrA
  input vrB
  input vrC
  return (vrT, vrA, vrB, vrC)

vaform4u :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
vaform4u = do
  vrT <- param "vrT" vrrc vectorBV
  shb <- param "shb" u5imm (EBV 5)
  vrA <- param "vrA" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  input vrA
  input vrB
  return (vrT, shb, vrA, vrB)

xoform2 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV)
xoform2 = do
  rT <- param "rT" gprc naturalBV
  rA <- param "rA" gprc naturalBV
  input rA
  return (rT, rA)

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

xform2f :: SemM 'Def (Location 'TBV, Location 'TBV)
xform2f = do
  frT <- param "frT" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  input frB
  return (frT, frB)

dform :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
dform = do
  rT <- param "rT" gprc naturalBV
  si <- param "si" s16imm (EBV 16)
  rA <- param "rA" gprc_nor0 naturalBV
  input rA
  input si
  return (rT, rA, si)

-- | D-form instructions where the operand is allowed to contain r0
dformr0 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
dformr0 = do
  rT <- param "rT" gprc naturalBV
  si <- param "si" s16imm (EBV 16)
  rA <- param "rA" gprc naturalBV
  input rA
  input si
  return (rT, rA, si)

-- | The unsigned immediate version of the dform
dformu :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
dformu = do
  rT <- param "rT" gprc naturalBV
  ui <- param "ui" u16imm (EBV 16)
  rA <- param "rA" gprc naturalBV
  input rA
  input ui
  return (rT, rA, ui)

iform :: (?bitSize :: BitSize) => String -> SemM 'Def (Location 'TBV)
iform tag = do
  target <- param "target" tag (EBV 24)
  input target
  return target

-- Our floating point registers are considered as 128 bit registers, as they are
-- actually backed by the 128 bit vector registers.
--
-- It is the job of each semantics definition to extract the necessary parts of
-- the registers.
aform :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
aform = do
  frT <- param "frT" fprc (EBV 128)
  frA <- param "frA" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  input frA
  input frB
  return (frT, frA, frB)

aform4 :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
aform4 = do
  frT <- param "frT" fprc (EBV 128)
  frA <- param "frA" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  frC <- param "frC" fprc (EBV 128)
  input frA
  input frB
  input frC
  return (frT, frA, frB, frC)

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
  bvor crFld0 shiftedNibble
  where
    c = ite (lt reg ximm)
            (LitBV 3 0b100)
            (ite (gt reg ximm)
                 (LitBV 3 0b010)
                 (LitBV 3 0b001))
    crnibble = concat c (xerBit SO (Loc xer))
    shiftedNibble = bvshl (zext' 32 crnibble) (bvmul (zext' 32 fld) (LitBV 32 0x4))
    crFld0 = bvand (Loc cr) (bvnot (bvshl (LitBV 32 0xf) (bvmul (zext' 32 fld) (LitBV 32 0x4))))

-- | Produce an expression that extracts the given field from the CR as a four
-- bit bitvector
crField :: Expr 'TBV
        -- ^ The field number to extract from the CR (should be a 3 bit crrc value)
        -> Expr 'TBV
crField fldNum = lowBits' 4 shiftedCR
  where
    shiftedCR = bvlshr (Loc cr) (bvmul (zext' 32 fldNum) (LitBV 32 0x4))

-- | Update the named CR field with the given four bit value; returns a new CR value
--
-- The field is named by a 3 bit crrc value.
--
-- Generates a mask of four ones shifted to the field slot, then negates the
-- mask to clear that field.  Shifts the new field into the correct slot and
-- does an or.
updateCRField :: Expr 'TBV
              -- ^ A three bit crrc value naming the field to update
              -> Expr 'TBV
              -- ^ A four bit replacement field value
              -> Expr 'TBV
updateCRField fldNum newFldVal = bvor clearedCR shiftedVal
  where
    fieldMask = bvnot (bvshl (LitBV 32 0xf) (bvmul (zext' 32 fldNum) (LitBV 32 0x4)))
    clearedCR = bvand (Loc cr) fieldMask
    shiftedVal = bvshl (zext' 32 newFldVal) (bvmul (zext' 32 fldNum) (LitBV 32 0x4))

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

-- | Rotate a K bit value left
--
-- > rotl[k](v, n)
--
-- rotates the K bit bitvector @v@ left by @n@ bits.
--
-- All of the inputs are expected to be the same size
rotl :: (HasCallStack, ?bitSize :: BitSize)
     => Int
     -> Expr 'TBV
     -> Expr 'TBV
     -> Expr 'TBV
rotl k v n =
  let w1 = bvshl v n
      w2 = bvlshr v (bvsub (LitBV k (toInteger k)) n)
  in bvor w1 w2

-- | Generate a mask of all ones from b0 to b1
--
-- > mask b0 b1
mask :: (HasCallStack, ?bitSize :: BitSize)
     => Int
     -> Expr 'TBV
     -> Expr 'TBV
     -> Expr 'TBV
mask k b0 b1 =
  let allOnes = sext' k (LitBV 1 0x1)
      clearLeft = bvlshr (bvshl allOnes b0) b0
      shmax = LitBV k (toInteger (k - 1))
      shr = bvsub shmax b1
  in bvshl (bvlshr clearLeft shr) shr

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

updateXER :: (HasCallStack, ?bitSize :: BitSize)
          => XERBit
          -- ^ The bit to update
          -> Expr 'TBV
          -- ^ The XER
          -> Expr 'TBV
          -- ^ The one-bit value to substitute in
          -> Expr 'TBV
updateXER xb xerExp newBit
  | exprBVSize newBit == 1 = concat prefix (concat newBit suffix)
  | otherwise = error ("Invalid XER bit size: " ++ show (exprBVSize newBit))
  where
    prefix = highBits (xerBitNum xb) xerExp
    suffixLen = bitSizeValue ?bitSize - xerBitNum xb - 1
    suffix = lowBits suffixLen xerExp

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
  | ?bitSize == Size64 && n == 64 = e
  | otherwise = lowBits64 n e

lowBits' :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
lowBits' n e
  | nBits >= n = extract (nBits - 1) (nBits - n) e
  | otherwise = error ("Unexpected small slice: " ++ show n ++ " from " ++ show e)
  where
    nBits = exprBVSize e

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
  | ?bitSize == Size64 && n == 64 = e
  | otherwise = highBits64 n e

-- | Take the @n@ high bits of the given value
highBits' :: (HasCallStack) => Int -> Expr 'TBV -> Expr 'TBV
highBits' n e
  | exprBVSize e >= n = extract (n - 1) 0 e
  | otherwise = error ("Unexpected small slice: " ++ show n ++ " from " ++ show e)

-- Uninterpreted function helpers

-- | This is a function over locations instead of expressions because we need to
-- be able to call 'defLoc' on the result.
--
-- Note that we really need to accommodate this in the formula parser.
memriReg :: (?bitSize :: BitSize) => Location 'TMemRef -> Location 'TBV
memriReg = locUF naturalBV "ppc.memri_reg"

memriOffset :: Int
            -- ^ The number of bits of the offset
            -> Expr 'TMemRef
            -- ^ The memory ref expression
            -> Expr 'TBV
memriOffset osize = uf (EBV osize) "ppc.memri_offset" . ((:[]) . Some)

memrixReg :: (?bitSize :: BitSize) => Location 'TMemRef -> Location 'TBV
memrixReg = locUF naturalBV "ppc.memrix_reg"

memrixOffset :: Int
             -- ^ The number of bits of the offset
             -> Expr 'TMemRef
             -- ^ The memory ref expression
             -> Expr 'TBV
memrixOffset osize = uf (EBV osize) "ppc.memrix_offset" . ((:[]) . Some)

memrrBaseReg :: (?bitSize :: BitSize)
             => Location 'TMemRef
             -> Location 'TBV
memrrBaseReg = locUF naturalBV "ppc.memrr_base"

memrrOffsetReg :: (?bitSize :: BitSize)
               => Expr 'TMemRef
               -> Expr 'TBV
memrrOffsetReg = uf naturalBV "ppc.memrr_offset" . ((:[]) . Some)

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
  uf (EBV (8 * nBytes)) funcName [Some mem, Some ea]
  where
    funcName :: String
    funcName = printf "read_mem.%d" (nBytes * 8)

-- | Define a write to memory; it takes a memory and returns a whole new memory.
storeMem :: (?bitSize :: BitSize, HasCallStack)
         => Expr 'TMemory
         -- ^ The memory
         -> Expr 'TBV
         -- ^ The effective address to store at
         -> Int
         -- ^ The number of bytes to store
         -> Expr 'TBV
         -- ^ The bitvector value to store (size is checked)
         -> Expr 'TMemory
storeMem mem ea nBytes val
  | EBV w <- exprType val
  , w == nBytes * 8 =
    uf EMemory funcName [Some mem, Some ea, Some val]
  | otherwise = error ("Invalid byte count to store value " ++ show val)
  where
    funcName = printf "write_mem.%d" (nBytes * 8)

-- | An uninterpreted function that tests if the argument is register zero
isR0 :: (HasCallStack) => Expr 'TBV -> Expr 'TBool
isR0 = uf EBool "ppc.is_r0" . ((:[]) . Some)
