-- | Pseudocode definitions of shifts and rotates from F2.4.3 (page
-- F2-2420) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
    ( SRType(..)
    , srtLSL, srtLSR, srtASR, srtROR, srtRRX
    , ImmShift(..)
    , splitImmShift, decodeRegShift, decodeImmShift
    , shift
    , shiftC
    , lslC
    , lsrC
    , asrC
    , rrxC
    , ror, rorC
    )
    where

-- import Control.Monad ( when )
-- import Data.Bits hiding (shift)
-- import Data.Maybe
-- import Data.Parameterized.Classes
-- import Data.Parameterized.Context
-- import Data.Parameterized.Some ( Some(..) )
-- import Data.Parameterized.TraversableFC
-- import Data.Semigroup
-- import qualified Data.Type.List as TL
import GHC.Stack ( HasCallStack )
-- import GHC.TypeLits ( Symbol )
import Prelude hiding ( concat, pred )
-- import qualified Dismantle.ARM as A
-- import qualified Dismantle.Thumb as T
-- import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
-- import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
-- import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


-- | A wrapper around expressions representing the shift type
--
-- In the ARM manual, this is represented with an ADT, but our formula language
-- does not have ADTs. Instead, we use the following encoding:
--
-- * 0b000 : SRType_LSL
-- * 0b001 : SRType_LSR
-- * 0b010 : SRType_ASR
-- * 0b011 : SRType_ROR
-- * 0b100 : SRType_RRX
--
-- Note that there is an unused bit pattern, which is unfortunate but unavoidable
newtype SRType = SRType { unSRType :: Expr 'TBV }

srtLSL, srtLSR, srtASR, srtROR, srtRRX :: SRType
srtLSL = SRType (LitBV 3 0b000)
srtLSR = SRType (LitBV 3 0b001)
srtASR = SRType (LitBV 3 0b010)
srtROR = SRType (LitBV 3 0b011)
srtRRX = SRType (LitBV 3 0b100)

-- | Represents the result of 'decodeImmShift', and is actually a pair of the
-- SRType (3 bits) followed by the shift amount (32 bits).
--
-- We need this because our formulas can't return multiple values
newtype ImmShift = ImmShift (Expr 'TBV)

-- | Split an 'ImmShift' into its component parts
--
-- We use this newtype to make it clear that the result of 'decodeImmShift' is
-- not directly usable as a bitvector.
--
-- The first element of the pair is the SRType, while the second is a 32 bit
-- bitvector encoding the shift amount.
splitImmShift :: ImmShift -> (SRType, Expr 'TBV)
splitImmShift (ImmShift is) = (SRType (extract 34 32 is), extract 31 0 is)

-- | Convert a two bit shift type into our SRType
--
-- > // DecodeRegShift()
-- > // ================
-- > SRType DecodeRegShift(bits(2) type)
-- >   case type of
-- >     when ‘00’ shift_t = SRType_LSL;
-- >     when ‘01’ shift_t = SRType_LSR;
-- >     when ‘10’ shift_t = SRType_ASR;
-- >     when ‘11’ shift_t = SRType_ROR;
-- >   return shift_t;
decodeRegShift :: Expr 'TBV -> SRType
decodeRegShift = SRType . concat (LitBV 1 0b0)


-- | This is the DecodeImmShift function in the ARM semantics.
--
-- Note that we only return the shift amount expression (shift_n); we can get
-- the shift type with a different accessor
--
-- > // DecodeImmShift()
-- > // ================
-- > (SRType, integer) DecodeImmShift(bits(2) type, bits(5) imm5)
-- > case type of
-- >   when ‘00’
-- >     shift_t = SRType_LSL; shift_n = UInt(imm5);
-- >   when ‘01’
-- >     shift_t = SRType_LSR; shift_n = if imm5 == ‘00000’ then 32 else UInt(imm5);
-- >   when ‘10’
-- >     shift_t = SRType_ASR; shift_n = if imm5 == ‘00000’ then 32 else UInt(imm5);
-- >   when ‘11’
-- >     if imm5 == ‘00000’ then
-- >       shift_t = SRType_RRX; shift_n = 1;
-- >     else
-- >       shift_t = SRType_ROR; shift_n = UInt(imm5);
-- > return (shift_t, shift_n);
decodeImmShift :: (HasCallStack) =>
                  Expr 'TBV  -- ^ 2-bit shift type specification
               -> Expr 'TBV  -- ^ 5-bit immediate value to be decoded
               -> ImmShift
decodeImmShift ty imm5 = ImmShift $ "immShift" =:
  cases [ (bveq ty (LitBV 2 0b00), concat (LitBV 3 0b000) (zext imm5))
        , (bveq ty (LitBV 2 0b01), concat (LitBV 3 0b001) (ite (bveq (LitBV 5 0b00000) imm5) (naturalLitBV 32) (zext imm5)))
        , (bveq ty (LitBV 2 0b10), concat (LitBV 3 0b010) (ite (bveq (LitBV 5 0b00000) imm5) (naturalLitBV 32) (zext imm5)))
        , (bveq imm5 (LitBV 5 0b00000), concat (LitBV 3 0b100) (naturalLitBV 1))
        ] (concat (LitBV 3 0b011) (zext imm5))

-- | The Shift function from the ARM manual (v8).
shift :: Expr 'TBV -> SRType -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
shift value srtype shift_n carry_in =
    extract 31 0 $ shiftC value srtype shift_n carry_in


-- | Shift with carry out.  (F2.4.3, F-2420)
--
-- The version in the manual returns a pair.  That is inconvenient in our
-- representation (we don't have pairs). Instead, when the input bitvector is N
-- bits, we return an N+1 bit bitvector where the top bit is the carry out
-- bit. The caller can dissect it (e.g. see 'shift' which discards the carry out).
shiftC :: (HasCallStack) => Expr 'TBV -> SRType -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
shiftC value (unSRType -> shift_t) shift_n c = "shiftC" =:
  cases [ (bveq shift_n (naturalLitBV 0x0), concat c value)
        , (bveq shift_t (LitBV 3 0b000), lslC value shift_n)
        , (bveq shift_t (LitBV 3 0b001), lsrC value shift_n)
        , (bveq shift_t (LitBV 3 0b010), asrC value shift_n)
        , (bveq shift_t (LitBV 3 0b011), rorC value shift_n)
        ] (rrxC value c)

-- | Logical Shift Left (with carry out)  AppxG-5008
--
-- Unlike the version in the manual, we return an N+1 bit bitvector, where the
-- highest bit is the carry out bit.  We do this because we don't have tuples.
--
-- > // LSL_C()
-- > // =======
-- > (bits(N), bit) LSL_C(bits(N) x, integer shift)
-- >   assert shift > 0;
-- >   extended_x = x : Zeros(shift);
-- >   result = extended_x<N-1:0>;
-- >   carry_out = extended_x<N>;
-- >   return (result, carry_out);
lslC :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
lslC x shft = "lslC" =:
  bvshl (zext' xsize x) (zext' xsize shft)
  where
    xsize = exprBVSize x + 1

-- | Logical shift right (with carry out) AppxG-5008
--
-- Our implementation isn't the same as the manual because we can't zero extend
-- out to a dynamic width.  Instead, we have to shift and then, if the shift
-- size is less than the bit width, pull out the last bit that would be shifted
-- off with @test_bit_dynamic@.
--
-- > // LSR_C()
-- > // =======
-- >   (bits(N), bit) LSR_C(bits(N) x, integer shift)
-- >   assert shift > 0;
-- >   extended_x = ZeroExtend(x, shift+N);
-- >   result = extended_x<shift+N-1:shift>;
-- >   carry_out = extended_x<shift-1>;
-- >   return (result, carry_out)
lsrC :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
lsrC x shft = "logicalShiftRightCarry" =: srC bvlshr x shft

asrC :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
asrC x shft = "arithmeticShiftRightCarry" =: srC bvashr x shft

-- | Generalized shift right with carry out
--
-- This is parameterized by the shift operation to perform (arithmetic vs logical)
srC :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV -> Expr 'TBV) -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
srC op x shft = concat carry_out rs
  where
    nBits = LitBV (exprBVSize x) (fromIntegral (exprBVSize x))
    m = bvurem shft nBits
    rs = op x m
    co = ite (testBitDynamic (bvsub shft (naturalLitBV 1)) x) (LitBV 1 1) (LitBV 1 0)
    carry_out = ite (bvult shft (naturalLitBV 32)) co (LitBV 1 0)

-- | Rotate right with carry in? (and with carry out) AppxG-4947
--
-- > // RRX_C()
-- > // =======
-- > (bits(N), bit) RRX_C(bits(N) x, bit carry_in)
-- >   result = carry_in : x<N-1:1>;
-- >   carry_out = x<0>;
-- >   return (result, carry_out);
rrxC :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
rrxC x carry_in = "rrxC" =: concat carry_out (concat carry_in slice)
  where
    carry_out = extract 0 0 x
    slice = extract (exprBVSize x - 1) 1 x

-- | Rotate right (with carry out) - AppxG-5009
--
-- > // ROR_C()
-- > // =======
-- > (bits(N), bit) ROR_C(bits(N) x, integer shift)
-- >   assert shift != 0;
-- >   m = shift MOD N;
-- >   result = LSR(x,m) OR LSL(x,N-m);
-- >   carry_out = result<N-1>;
-- >   return (result, carry_out);
rorC :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
rorC x shft = "rorC" =: concat carry_out (bvor ls rs)
  where
    nBits = LitBV (exprBVSize x) (fromIntegral (exprBVSize x))
    m = bvurem shft nBits
    rs = bvlshr x m
    ls = bvshl x (bvsub nBits m)
    co = ite (testBitDynamic (bvsub shft (naturalLitBV 1)) x) (LitBV 1 1) (LitBV 1 0)
    carry_out = ite (bvult shft (naturalLitBV 32)) co (LitBV 1 0)

ror :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
ror x shft = "ror" =: extract (nBits - 1) 0 wc
  where
    nBits = exprBVSize x
    wc = rorC x shft
