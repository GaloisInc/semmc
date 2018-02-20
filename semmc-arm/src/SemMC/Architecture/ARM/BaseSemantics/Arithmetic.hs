{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
module SemMC.Architecture.ARM.BaseSemantics.Arithmetic
    ( manualArithmetic
    , manualBitwise
    )
    where

import GHC.Stack ( HasCallStack )

import Data.Parameterized.Context
import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL
import qualified Dismantle.ARM as A
import qualified Dismantle.Thumb as T

manualArithmetic :: SemARM 'Top ()
manualArithmetic = do
  defineA32Opcode A.ADDri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          )
                      $ \rD setcc _ imm12 rN -> do
    comment "ADD immediate, A32, Encoding A1  (F7.1.5, F7-2542)"
    input rN
    input setcc
    input imm12
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        imm32 = armExpandImm imm12
        (result, nzcv) = addWithCarry (Loc rN) imm32 (LitBV 32 0)
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

  defineA32Opcode A.ADDrr (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "rM" gpr naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          ) $ \rD setcc _ rM rN -> do
    comment "ADD register, A32, Encoding A1  (F7.1.7, F7-2546)"
    input rM
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        (result, nzcv) = addWithCarry (Loc rN) (Loc rM) (LitBV 32 0)
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

  defineA32Opcode A.SUBri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          )
                      $ \rD setcc _ imm12 rN -> do
    comment "SUB immediate, A32, Encoding A1  (F7.1.235, F7-2916)"
    input rN
    input setcc
    input imm12
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        imm32 = armExpandImm imm12
        (result, nzcv) = addWithCarry (Loc rN) (bvnot imm32) (LitBV 32 1)
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

  defineA32Opcode A.SUBrr (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "rM" gpr naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          ) $ \rD setcc _ rM rN -> do
    comment "ADD register, A32, Encoding A1  (F7.1.7, F7-2546)"
    input rM
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        (result, nzcv) = addWithCarry (Loc rN) (bvnot (Loc rM)) (LitBV 32 1)
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

manualBitwise :: (HasCallStack) => SemARM 'Top ()
manualBitwise = do

  defineA32Opcode A.ANDri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          )
                $ \rD setcc _ imm12 rN -> do
    comment "AND immediate, Encoding A1  (F7.1.13, F7-2556)"
    input rN
    input setcc
    input imm12
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        (_, _, c, v) = getNZCV
        (imm32, c') = armExpandImmC imm12 c
        result = bvand (Loc rN) imm32
        n' = extract 31 31 result
        z' = ite (bveq result (LitBV 32 0b0)) (LitBV 1 0b1) (LitBV 1 0b0)
        v' = v
        nzcv = concat n' $ concat z' $ concat c' v'
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv
  defineA32Opcode A.ANDrr ( Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "rM" gpr naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          )
                    $ \ rD setcc _ rM rN -> do
    comment "AND register, Encoding A1  (F7.1.14, F7-2558)"
    comment "Note that this encoding fixes the shift to 0"
    input rM
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    let (shift_t, shift_n) = splitImmShift (decodeImmShift (LitBV 2 0b00) (LitBV 5 0b00000))
    andrr rD (Loc rM) (Loc rN) setflags shift_t shift_n
  defineA32Opcode A.ANDrsi (  Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sori" so_reg_imm naturalBV
                           :> ParamDef "rN" gpr naturalBV
                           )
                 $ \rD setcc _ sori rN -> do
    comment "AND register, Encoding A1 (F7.1.14, F7-2558)"
    input sori
    input setcc
    input rN
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    let ty = soRegImm_type sori
    let rM = soRegImm_reg sori
    let imm = soRegImm_imm sori
    let (shift_t, shift_n) = splitImmShift (decodeImmShift ty imm)
    andrr rD rM (Loc rN) setflags shift_t shift_n
  defineA32Opcode A.ANDrsr (  Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sorr" so_reg_reg naturalBV
                           :> ParamDef "rN" gpr naturalBV
                           )
                 $ \rD setcc _ sorr rN -> do
    comment "AND (register-shifted register), Encoding A1 (F7.1.15, F7-2560)"
    input rD
    input sorr
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    let shift_t = decodeRegShift (soRegReg_type sorr)
    let shift_n = zext (extract 7 0 (soRegReg_shift sorr))
    let rM = soRegReg_reg sorr
    andrsr rD (Loc rN) rM setflags shift_t shift_n
  defineT32Opcode T.TAND ( Empty
                         :> ParamDef "rD" tgpr naturalBV
                         :> ParamDef "rM" tgpr naturalBV
                         )
                  $ \rD rM -> do
    comment "AND register, Encoding T1 (F7.1.14, F7-2558)"
    comment "This encoding has no shift"
    input rD
    input rM
    let setflags = notp inITBlock
    let (shift_t, shift_n) = splitImmShift (decodeImmShift (LitBV 2 0b00) (LitBV 5 0b00000))
    andrr rD (Loc rM) (Loc rD) setflags shift_t shift_n
  defineT32Opcode T.T2ANDrr (  Empty
                            :> ParamDef "rD" tgpr naturalBV
                            :> ParamDef "setcc" cc_out (EBV 1)
                            :> ParamDef "rN" tgpr naturalBV
                            :> ParamDef "rM" tgpr naturalBV
                            )
                 $ \rD setcc rN rM -> do
    comment "AND register, Encoding T2 (F7.1.14, F7-2558)"
    comment "This encoding has no shift"
    input rD
    input rN
    input rM
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    let (shift_t, shift_n) = splitImmShift (decodeImmShift (LitBV 2 0b00) (LitBV 5 0b00000))
    andrr rD (Loc rM) (Loc rN) setflags shift_t shift_n

  defineA32Opcode A.ORRri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          )
                $ \rD setcc _ imm12 rN -> do
    comment "ORR immediate, Encoding A1  (F7.1.127, F7-2738)"
    input rN
    input setcc
    input imm12
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        (_, _, c, v) = getNZCV
        (imm32, c') = armExpandImmC imm12 c
        result = bvor (Loc rN) imm32
        n' = extract 31 31 result
        z' = ite (bveq result (LitBV 32 0b0)) (LitBV 1 0b1) (LitBV 1 0b0)
        v' = v
        nzcv = concat n' $ concat z' $ concat c' v'
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

-- ----------------------------------------------------------------------

andrr :: (HasCallStack) => Location 'TBV -> Expr 'TBV -> Expr 'TBV -> Expr 'TBool -> SRType -> Expr 'TBV -> SemARM 'Def ()
andrr rD rM rN setflags shift_t shift_n = do
  let (_, _, c, v) = getNZCV
  let shiftedWithCarry = shiftC rM shift_t shift_n c
  let shifted = extract 31 0 shiftedWithCarry
  let carry = extract 32 32 shiftedWithCarry
  let result = bvand rN shifted
  let n' = extract 31 31 result
  let z' = ite (bveq result (naturalLitBV 0x0)) (LitBV 1 0b1) (LitBV 1 0b0)
  let c' = carry
  let v' = v
  let nzcv = "nzcv" =: concat n' (concat z' (concat c' v'))
  defReg rD (ite (isR15 rD) (Loc rD) result)
  aluWritePC (isR15 rD) result
  cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

andrsr :: (HasCallStack) => Location 'TBV -> Expr 'TBV -> Expr 'TBV -> Expr 'TBool -> SRType -> Expr 'TBV -> SemARM 'Def ()
andrsr rD rM rN setflags shift_t shift_n = do
  let (_, _, c, v) = getNZCV
  let shiftedWithCarry = shiftC rM shift_t shift_n c
  let shifted = extract 31 0 shiftedWithCarry
  let carry = extract 32 32 shiftedWithCarry
  let result = bvand rN shifted
  let n' = extract 31 31 result
  let z' = ite (bveq result (naturalLitBV 0x0)) (LitBV 1 0b1) (LitBV 1 0b0)
  let c' = carry
  let v' = v
  let nzcv = "nzcv" =: concat n' (concat z' (concat c' v'))
  let writesOrReadsR15 = anyp  [ isR15 rD ] -- FIXME: We need the rest of these, too
  defReg rD (ite writesOrReadsR15 (unpredictable (Loc rD)) result)
  cpsrNZCV (andp setflags (notp writesOrReadsR15)) nzcv

anyp :: [Expr 'TBool] -> Expr 'TBool
anyp [] = LitBool False
anyp (r : rs) = orp r (anyp rs)

-- | Shift with carry out
--
-- The version in the manual returns a pair.  That is inconvenient in our
-- representation (we don't have pairs). Instead, when the input bitvector is N
-- bits, we return an N+1 bit bitvector where the top bit is the carry out
-- bit. The caller can dissect it.
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
lslC x shift = "lslC" =:
  bvshl (zext' xsize x) (zext' xsize shift)
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
lsrC x shift = "logicalShiftRightCarry" =: srC bvlshr x shift

asrC :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
asrC x shift = "arithmeticShiftRightCarry" =: srC bvashr x shift

-- | Generalized shift right with carry out
--
-- This is parameterized by the shift operation to perform (arithmetic vs logical)
srC :: (HasCallStack) => (Expr 'TBV -> Expr 'TBV -> Expr 'TBV) -> Expr 'TBV -> Expr 'TBV -> Expr 'TBV
srC op x shift = concat carry_out rs
  where
    nBits = LitBV (exprBVSize x) (fromIntegral (exprBVSize x))
    m = bvurem shift nBits
    rs = op x m
    co = ite (testBitDynamic (bvsub shift (naturalLitBV 1)) x) (LitBV 1 1) (LitBV 1 0)
    carry_out = ite (bvult shift (naturalLitBV 32)) co (LitBV 1 0)

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
rorC x shift = "rorC" =: concat carry_out (bvor ls rs)
  where
    nBits = LitBV (exprBVSize x) (fromIntegral (exprBVSize x))
    m = bvurem shift nBits
    rs = bvlshr x m
    ls = bvshl x (bvsub nBits m)
    co = ite (testBitDynamic (bvsub shift (naturalLitBV 1)) x) (LitBV 1 1) (LitBV 1 0)
    carry_out = ite (bvult shift (naturalLitBV 32)) co (LitBV 1 0)

ror :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> Expr 'TBV
ror x shift = "ror" =: extract (nBits - 1) 0 wc
  where
    nBits = exprBVSize x
    wc = rorC x shift

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473)
armExpandImm :: Location 'TBV -> Expr 'TBV
armExpandImm imm12 =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
        -- Determine value per Table F4-6 (F4.2.4, F4-2472)
        val32 = zext val
        rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
        rval32 = ite (bveq rotv (naturalLitBV 0)) val32 (ror rotv val32)
    in "armExpandImm" =: rval32

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473) with carry
armExpandImmC :: Location 'TBV -> Expr 'TBV -> (Expr 'TBV, Expr 'TBV)
armExpandImmC imm12 carry_in =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
        -- Determine value per Table F4-6 (F4.2.4, F4-2472)
        val32 = zext val
        rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
        rval32 = ite (bveq rotv (naturalLitBV 0)) val32 (ror rotv val32)
        msb = extract 31 31 rval32 -- return the MSB as the carry_out
        carry_out =
          ite (bveq rotv (naturalLitBV 0)) carry_in msb
    in (rval32, carry_out)


-- | Pseudocode AddWithCarry (E1-2292 or F2-2423)
addWithCarry :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
             -> (Expr 'TBV, Expr 'TBV)
                -- ^ 32-bit result, NZCV result bits  (E1-2292 or F2-2423)
addWithCarry x y carry_in =
    let eres = bvadd (bvadd (extval x) (extval y)) (extval carry_in)
        extval = zext' (naturalBitSize+1)
        signBit = extract (naturalBitSize-1) (naturalBitSize-1)
        res = extract (naturalBitSize-1) 0 eres
        n = signBit res
        z = ite (bveq res (naturalLitBV 0)) (LitBV 1 1) (LitBV 1 0)
        c = extract naturalBitSize naturalBitSize eres
        v = bvand n (extract naturalBitSize naturalBitSize eres)
    in ("addResult" =: res, "addCarry" =: (concat n $ concat z $ concat c v))

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
decodeImmShift :: (HasCallStack) => Expr 'TBV -> Expr 'TBV -> ImmShift
decodeImmShift ty imm5 = ImmShift $ "immShift" =:
  cases [ (bveq ty (LitBV 2 0b00), concat (LitBV 3 0b000) (zext imm5))
        , (bveq ty (LitBV 2 0b01), concat (LitBV 3 0b001) (ite (bveq (LitBV 5 0b00000) imm5) (naturalLitBV 32) (zext imm5)))
        , (bveq ty (LitBV 2 0b10), concat (LitBV 3 0b010) (ite (bveq (LitBV 5 0b00000) imm5) (naturalLitBV 32) (zext imm5)))
        , (bveq imm5 (LitBV 5 0b00000), concat (LitBV 3 0b100) (naturalLitBV 1))
        ] (concat (LitBV 3 0b011) (zext imm5))


