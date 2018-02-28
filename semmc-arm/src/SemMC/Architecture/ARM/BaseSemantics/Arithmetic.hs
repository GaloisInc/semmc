{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
module SemMC.Architecture.ARM.BaseSemantics.Arithmetic
    ( manualArithmetic
    , manualBitwise
    )
    where

import           Data.Parameterized.Context
import qualified Dismantle.ARM as A
import qualified Dismantle.Thumb as T
import           GHC.Stack ( HasCallStack )
import           Prelude hiding ( concat, pred )
import           SemMC.Architecture.ARM.BaseSemantics.Base
import           SemMC.Architecture.ARM.BaseSemantics.Helpers
import           SemMC.Architecture.ARM.BaseSemantics.Natural
import           SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.AddSub
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Bitstring
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ExpandImm
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Registers
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
import           SemMC.Architecture.ARM.BaseSemantics.Registers
import           SemMC.DSL


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

  defineT32Opcode T.TADDi3 (Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "imm" imm0_7 (EBV 3)
                           :> ParamDef "rN" gpr naturalBV
                           )
                       $ \rD imm3 rN -> do
    comment "ADD immediate, T32, encoding T1 (F7.1.4, F7-2540)"
    input rN
    input imm3
    let setflags = notp inITBlock
        imm32    = zext (Loc imm3)
    tadd rD rN imm32 setflags

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

  defineA32Opcode A.MOVr (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "rM" gpr naturalBV
                         )
                      $ \rD setcc _ rM -> do
    comment "MOV register, A32, Encoding A1  (F7.1.109, F7-2712)"
    input rD
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        result = Loc rM
        (_,_,c,v) = getNZCV
        n = extract 31 31 result
        z = isZeroBit result
        nzcv = concat n $ concat z $ concat c v
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
        z' = isZeroBit result
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
        z' = isZeroBit result
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
  let z' = isZeroBit result
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
  let z' = isZeroBit result
  let nzcv = "nzcv" =: concat n' (concat z' (concat carry v))
  let writesOrReadsR15 = anyp $ fmap isR15 [ rD, rM, rN, rS ]
  defReg rD (ite writesOrReadsR15 (unpredictable (Loc rD)) result)
  cpsrNZCV (andp setflags (notp writesOrReadsR15)) nzcv

tadd :: (HasCallStack)
     => Location 'TBV
     -> Location 'TBV
     -> Expr 'TBV
     -> Expr 'TBool
     -> SemARM 'Def ()
tadd rD rN imm32 setflags = do
  let (result, nzcv) = addWithCarry (Loc rN) imm32 (LitBV 1 0b0)
  defReg rD result
  cpsrNZCV setflags nzcv
