{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Core (
  BitSize(..),
  bitSizeValue,
  naturalBV,
  -- * Operand Classes
  module SemMC.Architecture.PPC.Base.Core.OperandClasses,
  -- * Registers
  module SemMC.Architecture.PPC.Base.Core.Registers,
  -- * IP Wrapper
  OpcodeParamDef(..),
  definePPCOpcode,
  definePPCOpcodeRC,
  defineOpcodeWithIP,
  defineRCVariant,
  defineVRCVariant,
  -- * Forms
  module SemMC.Architecture.PPC.Base.Core.Forms,
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
  zext,
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
  readMem,
  ppcvec1,
  ppcvec2,
  ppcvec3
  -- memrixOffset,
  -- memrixReg
  ) where

import GHC.Stack ( HasCallStack )

import Prelude hiding ( concat )
import Text.Printf ( printf )
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Type.List as TL

import qualified Dismantle.PPC as P

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core.BitSize
import SemMC.Architecture.PPC.Base.Core.Forms
import SemMC.Architecture.PPC.Base.Core.OperandClasses
import SemMC.Architecture.PPC.Base.Core.Registers


-- IP Helper Wrapper

-- | Type-safe definition of the semantics for a PPC opcode, with an implicit
-- update of the instruction pointer
definePPCOpcode :: ( Ctx.CurryAssignmentClass args
                   , TL.SameShape args (TL.Map SymToExprTagWrapper sh)
                   , ?bitSize :: BitSize
                   )
                => P.Opcode P.Operand sh
                -> Ctx.Assignment OpcodeParamDef args
                -> Ctx.CurryAssignment args Location (SemM 'Def ())
                -> SemM 'Top ()
definePPCOpcode opc defArgs defBody =
  defineOpcode name $ do
    input ip
    defLoc ip (bvadd (Loc ip) (naturalLitBV 0x4))
    defArgs' <- FC.traverseFC param' defArgs
    Ctx.uncurryAssignment defBody defArgs'
  where
    name = show opc
    param' :: OpcodeParamDef t -> SemM 'Def (Location t)
    param' (ParamDef pname ty ety) = param pname ty ety
    param' (InputParamDef pname ty ety) = do
      p <- param pname ty ety
      input p
      return p

-- | A variant of 'definePPCOpcode' that also sets the CR register (suitable for
-- defining dotted variants of non-vector instructions)
definePPCOpcodeRC :: ( Ctx.CurryAssignmentClass args
                     , TL.SameShape args (TL.Map SymToExprTagWrapper sh)
                     , ?bitSize :: BitSize
                     )
                  => P.Opcode P.Operand sh
                  -> Expr 'TBV
                  -> SemM 'Def ()
                  -> SemM 'Def ()
definePPCOpcodeRC opc modifiedReg def =
  forkDefinition newName $ do
  input cr
  input xer
  defLoc cr (cmpImm bvslt bvsgt (LitBV 3 0x0) (naturalLitBV 0x0) modifiedReg)
  def
  where
    newName = show opc

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
defineRCVariant :: (?bitSize :: BitSize, HasCallStack) => String -> Expr 'TBV -> SemM 'Def () ->  SemM 'Def ()
defineRCVariant newName modifiedReg def = do
  forkDefinition newName $ do
    input cr
    input xer
    defLoc cr (cmpImm bvslt bvsgt (LitBV 3 0x0) (naturalLitBV 0x0) modifiedReg)
    def

-- | Like 'defineRCVariant', but for vector instructions, which modify CR6
-- instead of CR0.
defineVRCVariant :: (?bitSize :: BitSize, HasCallStack) => String -> SemM 'Def () ->  SemM 'Def ()
defineVRCVariant newName def = do
  forkDefinition newName $ do
    input cr
    input xer
    defLoc cr (updateCRField (LitBV 3 0b110) (undefinedBV 4)) -- update CR6
    def

-- Form helpers

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
  lf cmpImmLF (lt reg ximm :< gt reg ximm :< fld :< Loc xer :< Loc cr :< Nil)

cmpImmLF :: (?bitSize :: BitSize)
         => LibraryFunctionDef '(['TBool, 'TBool, 'TBV, 'TBV, 'TBV], 'TBV)
cmpImmLF =
  defineLibraryFunction "cmpImm"
    (Arg "lt" EBool :<
     Arg "gt" EBool :<
     Arg "fld" (EBV 3) :<
     Arg "xer" naturalBV :<
     Arg "cr" (EBV 32) :< Nil)
    cmpImmImpl

cmpImmImpl :: (?bitSize::BitSize)
           => Expr 'TBool -> Expr 'TBool -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
           -> Expr 'TBV
cmpImmImpl lt gt fld xerValue crValue =
  updateCRFieldImpl crValue fld crnibble
  -- Use impl because we can't (yet) call one library function from another
  where
    c = ite lt
            (LitBV 3 0b100)
            (ite gt
                 (LitBV 3 0b010)
                 (LitBV 3 0b001))
    crnibble = concat c (xerBit SO xerValue)

-- | Produce an expression that extracts the given field from the CR as a four
-- bit bitvector
crField :: Expr 'TBV
        -- ^ The field number to extract from the CR (should be a 3 bit crrc value)
        -> Expr 'TBV
crField fldNum = lowBits' 4 shiftedCR
  where
    shiftedCR = bvlshr (Loc cr) (crFieldIndex fldNum)

-- | Update the named CR field with the given four bit value; returns a new CR value
--
-- The field is named by a 3 bit crrc value.
--
-- Generates a mask of four ones shifted to the field slot, then negates the
-- mask to clear that field.  Shifts the new field into the correct slot and
-- does an or.
updateCRField :: (?bitSize :: BitSize)
              => Expr 'TBV
              -- ^ A three bit crrc value naming the field to update
              -> Expr 'TBV
              -- ^ A four bit replacement field value
              -> Expr 'TBV
updateCRField =
  (wrapAsLibraryFunction "updateCRField"
     (Arg "cr" (EBV 32) :<
      Arg "fldNum" (EBV 3) :<
      Arg "newFldVal" (EBV 4) :< Nil)
   updateCRFieldImpl) (Loc cr)

updateCRFieldImpl :: (?bitSize :: BitSize)
                  => Expr 'TBV
                  -- ^ The current value of CR
                  -> Expr 'TBV
                  -> Expr 'TBV
                  -> Expr 'TBV
updateCRFieldImpl crValue fldNum newFldVal = bvor clearedCR shiftedVal
  where
    fieldMask = bvnot (bvshl (LitBV 32 0xf) (crFieldIndex fldNum))
    clearedCR = bvand crValue fieldMask
    shiftedVal = bvshl (zext' 32 newFldVal) (crFieldIndex fldNum)

-- | Given a condition register field number, produce the macaw index of the
-- least significant bit of that field -- that is, the index of the summary
-- overflow bit of that field. The remaining bits of the field are in larger
-- indices.
crFieldIndex :: Expr 'TBV -> Expr 'TBV
crFieldIndex fld = bvmul (bvsub (LitBV 32 7) (zext' 32 fld)) (LitBV 32 4)

-- Common operations

naturalLitBV :: (?bitSize :: BitSize) => Integer -> Expr 'TBV
naturalLitBV n = LitBV (bitSizeValue ?bitSize) n

-- | Smart sign extend (extend to the full word width, i.e., up to the native
-- width of registers)
sext :: (HasCallStack, ?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV
sext = sext' (bitSizeValue ?bitSize)

-- | Zero extension to the full native bit width of registers
zext :: (HasCallStack, ?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV
zext = zext' (bitSizeValue ?bitSize)

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

-- | Generate a mask of all ones from b0 to b1, wrapping around if b0 > b1
--
-- > mask b0 b1
mask :: (HasCallStack, ?bitSize :: BitSize)
     => Int
     -> Expr 'TBV
     -> Expr 'TBV
     -> Expr 'TBV
mask k b0 b1 = lf (maskLF k) (b0 :< b1 :< Nil)

maskLF :: (?bitSize :: BitSize) => Int
       -> LibraryFunctionDef '(['TBV, 'TBV], 'TBV)
maskLF k =
  defineLibraryFunction ("mask_" ++ show k)
    (Arg "b0" (EBV k) :<
     Arg "b1" (EBV k) :< Nil) $
  \b0 b1 -> ite
    (bvule b0 b1)
    (maskLE b0 b1)
    (bvnot (maskLE (bvadd b1 (LitBV k 1)) (bvsub b0 (LitBV k 1))))
  where
  -- produces the all-zero mask unless b0 <= b1
  maskLE b0 b1 =
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

-- Vector UFs

-- | This is a cheap way to encode strings in our DSL in order to pass along a little
-- extra information with an uninterpreted function -- in this case, our UFs are the
-- ppcvec functions, but we also would like to pass the *name* of the particular
-- operation we are considering so that the backend has that information as well. The
-- type of the expression is a boolean because we didn't have a unit.
vecFn :: String -> Expr 'TString
vecFn s = LitString s

-- These encode various architecture-specific vector functions by storing the
-- string along with the opcodes. The result is a 160-bit vector; the high 128 bits
-- are used for the target register and the remaining 32 bits are stored in the
-- VSCR.

-- | One-source vector/floating point instruction
ppcvec1 :: (HasCallStack)
        => String
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
ppcvec1 name vr1 vvscr =
  uf (EBV 160) "ppc.vec1" [ Some (vecFn name), Some vr1, Some vvscr ]

-- | Two-source vector/floating point instruction
ppcvec2 :: (HasCallStack)
        => String
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
ppcvec2 name vr1 vr2 vvscr =
  uf (EBV 160) "ppc.vec2" [ Some (vecFn name), Some vr1, Some vr2, Some vvscr ]

-- | Three-source vector/floating point instruction
ppcvec3 :: (HasCallStack)
        => String
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
        -> Expr 'TBV
ppcvec3 name vr1 vr2 vr3 vvscr =
  uf (EBV 160) "ppc.vec3" [ Some (vecFn name), Some vr1, Some vr2, Some vr3, Some vvscr ]
