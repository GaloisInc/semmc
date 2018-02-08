{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module SemMC.Architecture.ARM.BaseSemantics.Arithmetic
    ( manualArithmetic
    , manualBitwise
    )
    where

import Data.Parameterized.Context
import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.DSL
import qualified Dismantle.ARM as A

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

manualBitwise :: SemARM 'Top ()
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


-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473)
armExpandImm :: Location 'TBV -> Expr 'TBV
armExpandImm imm12 =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
        -- Determine value per Table F4-6 (F4.2.4, F4-2472)
        val32 = zext val
        rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
        rval32 = ite (bveq rotv (naturalLitBV 0)) val32 (bvror rotv val32)
    in rval32

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473) with carry
armExpandImmC :: Location 'TBV -> Expr 'TBV -> (Expr 'TBV, Expr 'TBV)
armExpandImmC imm12 carry_in =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
        -- Determine value per Table F4-6 (F4.2.4, F4-2472)
        val32 = zext val
        rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
        rval32 = ite (bveq rotv (naturalLitBV 0)) val32 (bvror rotv val32)
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
    in (res, concat n $ concat z $ concat c v)
