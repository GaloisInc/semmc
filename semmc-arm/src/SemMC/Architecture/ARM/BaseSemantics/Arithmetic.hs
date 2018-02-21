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
import SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
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
    input rM
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    comment "Note that this encoding fixes the shift to 0"
    let (shift_t, shift_n) = splitImmShift (decodeImmShift (LitBV 2 0b00) (LitBV 5 0b00000))
    andrr rD rM rN setflags shift_t shift_n
  defineA32Opcode A.ANDrsi (  Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sori" so_reg_imm EMemRef
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
    andrr rD rM rN setflags shift_t shift_n
  defineA32Opcode A.ANDrsr (  Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sorr" so_reg_reg EMemRef
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
    let rS = soRegReg_reg1 sorr
    let rM = soRegReg_reg2 sorr
    andrsr rD rM rN setflags shift_t rS
  defineT32Opcode T.TAND ( Empty
                         :> ParamDef "rDN" tgpr naturalBV
                         :> ParamDef "rM" tgpr naturalBV
                         )
                  $ \rDN rM -> do
    comment "AND register, Encoding T1 (F7.1.14, F7-2558)"
    input rDN
    input rM
    let setflags = notp inITBlock
    comment "This encoding has no shift; fixed to 0"
    let (shift_t, shift_n) = splitImmShift (decodeImmShift (LitBV 2 0b00) (LitBV 5 0b00000))
    andrr rDN rM rDN setflags shift_t shift_n
  defineT32Opcode T.T2ANDrr (  Empty
                            :> ParamDef "rD" tgpr naturalBV
                            :> ParamDef "setcc" cc_out (EBV 1)
                            :> ParamDef "rN" tgpr naturalBV
                            :> ParamDef "rM" tgpr naturalBV
                            )
                 $ \rD setcc rN rM -> do
    comment "AND register, Encoding T2 (F7.1.14, F7-2558)"
    input rD
    input rN
    input rM
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    comment "This encoding has no shift; fixed to 0"
    let (shift_t, shift_n) = splitImmShift (decodeImmShift (LitBV 2 0b00) (LitBV 5 0b00000))
    andrr rD rM rN setflags shift_t shift_n

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

andrr :: (HasCallStack) =>
         Location 'TBV
      -> Location 'TBV
      -> Location 'TBV -> Expr 'TBool -> SRType -> Expr 'TBV -> SemARM 'Def ()
andrr rD rM rN setflags shift_t shift_n = do
  let (_, _, c, v) = getNZCV
  let shiftedWithCarry = shiftC (Loc rM) shift_t shift_n c
  let shifted = extract 31 0 shiftedWithCarry
  let carry = extract 32 32 shiftedWithCarry
  let result = bvand (Loc rN) shifted
  let n' = extract 31 31 result
  let z' = ite (bveq result (naturalLitBV 0x0)) (LitBV 1 0b1) (LitBV 1 0b0)
  let c' = carry
  let v' = v
  let nzcv = "nzcv" =: concat n' (concat z' (concat c' v'))
  defReg rD (ite (isR15 rD) (Loc rD) result)
  aluWritePC (isR15 rD) result
  cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

andrsr :: (HasCallStack) =>
          Location 'TBV
       -> Location 'TBV
       -> Location 'TBV -> Expr 'TBool -> SRType -> Location 'TBV -> SemARM 'Def ()
andrsr rD rM rN setflags shift_t rS = do
  let (_, _, c, v) = getNZCV
      shift_n = zext $ extract 7 0 (Loc rS)
  let shiftedWithCarry = shiftC (Loc rM) shift_t shift_n c
  let shifted = extract 31 0 shiftedWithCarry
  let carry = extract 32 32 shiftedWithCarry
  let result = bvand (Loc rN) shifted
  let n' = extract 31 31 result
  let z' = ite (bveq result (naturalLitBV 0x0)) (LitBV 1 0b1) (LitBV 1 0b0)
  let nzcv = "nzcv" =: concat n' (concat z' (concat carry v))
  let writesOrReadsR15 = anyp $ fmap isR15 [ rD, rM, rN, rS ]
  defReg rD (ite writesOrReadsR15 (unpredictable (Loc rD)) result)
  cpsrNZCV (andp setflags (notp writesOrReadsR15)) nzcv

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
