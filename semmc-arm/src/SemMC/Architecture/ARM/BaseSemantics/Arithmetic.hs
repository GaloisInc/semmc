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
  -- TODO: abstract this to merge with the Thumb encodings? Same with ADDri?
  defineA32Opcode A.ADCri (Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "mimm" mod_imm (EPackedOperand "ModImm")
                           :> ParamDef "rN" gpr naturalBV
                          ) $ \rD setcc _ imm12 rN -> do
    comment "ADC immediate, A32, Encoding A1 (F7.1.1, F7-2534)"
    input rN
    input setcc
    input imm12
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        imm32 = armExpandImm imm12
        (_,_,c,_) = getNZCV
        (result, nzcv) = addWithCarry (Loc rN) imm32 (zext c)
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

  defineA32Opcode A.ADDri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm (EPackedOperand "ModImm")
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
                           :> ParamDef "rD" tgpr naturalBV
                           :> ParamDef "imm" imm0_7 (EBV 3)
                           :> ParamDef "rN" tgpr naturalBV
                           )
                       $ \rD imm3 rN -> do
    comment "ADD immediate, T32, encoding T1 (F7.1.4, F7-2540)"
    input rN
    input imm3
    taddri rD rN (zext (Loc imm3)) (LitBool True) (notp inITBlock)


  defineT32Opcode T.TADDi8 (Empty
                           :> ParamDef "rDn" tgpr naturalBV
                           :> ParamDef "imm" imm0_255 (EBV 8)
                           )
                       $ \rDn imm8 -> do
    comment "Add immediate, T32, encoding T2 (F7.1.4, F7-2540)"
    input imm8
    taddri rDn rDn (zext (Loc imm8)) (LitBool True) (notp inITBlock)

  defineT32Opcode T.T2ADDri (Empty
                            :> ParamDef "rD" gprnopc naturalBV
                            :> ParamDef "setcc" cc_out (EBV 1)
                            -- TODO: Ask Kevin why we have to use two slightly
                            -- different strings here...
                            :> ParamDef "imm" t2_so_imm (EPackedOperand "T2_So_Imm")
                            :> ParamDef "rN" gprnopc naturalBV
                            )
                        $ \rD setcc imm16 rN -> do
    comment "Add immediate, T32, encoding T3 (F7.1.4, F7-2540)"
    input rN
    input imm16
    -- FIXME: Assume T32 version gets decoded the same was as A32 and we don't have
    -- to worry about CMN or ADD (SP plus immediate)
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        imm32    = thumbExpandImm imm16
        undef    = orp (andp (isR15 rD) (notp setflags)) (isR15 rN)
    taddri rD rN imm32 undef setflags

  defineT32Opcode T.T2ADDri12 (Empty
                               :> ParamDef "rD" gprnopc naturalBV
                               :> ParamDef "imm" imm0_4095 (EBV 16)
                               :> ParamDef "rN" gpr naturalBV
                              )
                        $ \rD imm12 rN -> do
    comment "Add immediate, T32, encoding T4 (F7.1.4, F7-2540)"
    input rN
    input imm12  -- n.b. encodes 12 bits, but Dismantle provides 16 bits (assumed zext)
    taddri rD rN (zext (Loc imm12)) (LitBool False) (LitBool False)

  defineT32Opcode T.TADDrSPi (Empty
                             :> ParamDef "rD" tgpr naturalBV
                             :> ParamDef "imm" t_imm0_1020s4 (EPackedOperand "imm0_1020s4")
                             )
                      $ \rD imm0_1020s4 -> do
    comment "ADD SP + immediate, T32, encoding T1 (F7.1.9, F7-2548)"
    input rD
    input imm0_1020s4
    let setflags = LitBool False
        imm8 = t32_imm_0_1020s4_val imm0_1020s4
        imm32 = zext $ concat imm8 (LitBV 2 0b00)
    taddSP rD imm32 setflags

  defineT32Opcode T.TADDspi (Empty
                             :> ParamDef "imm" t_imm0_508s4 (EPackedOperand "imm0_508s4")
                            )
                      $ \imm0_508s4 -> do
    comment "ADD SP + immediate, T32, encoding T2 (F7.1.9, F7-2548)"
    input imm0_508s4
    let setflags = LitBool False
        imm7 = t32_imm_0_508s4_val imm0_508s4
        imm32 = zext $ concat imm7 (LitBV 2 0b00)
    taddSP sp imm32 setflags

  -- TODO: abstract this with ADDrr?? TADC?
  defineA32Opcode A.ADCrr (Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "rM" gpr naturalBV
                           :> ParamDef "rN" gpr naturalBV
                          ) $ \rD setcc _ rM rN -> do
    comment "ADC register, A32, Encoding A1 (F7.1.2, F7-2536)"
    input rM
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        (_,_,c,_) = getNZCV
        (result, nzcv) = addWithCarry (Loc rN) (Loc rM) (zext c)
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
    comment "ADD register, A32, Encoding A1 (F7.1.7, F7-2546)"
    input rM
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        (result, nzcv) = addWithCarry (Loc rN) (Loc rM) (LitBV 32 0)
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

  defineA32Opcode A.ADCrsi (  Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sori" so_reg_imm (EPackedOperand "SoRegImm")
                           :> ParamDef "rN" gpr naturalBV
                           )
                 $ \rD setcc _ sori rN -> do
    comment "ADC register, A32, Encoding A1 (F7.1.2, F7-2536)"
    input sori
    input setcc
    input rN
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        ty = soRegImm_type sori
        rM = soRegImm_reg sori
        imm = soRegImm_imm sori
        (shift_t, shift_n) = splitImmShift (decodeImmShift ty imm)
    adcrr rD rM (Loc rN) setflags shift_t shift_n

  defineA32Opcode A.ADDrsi (  Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sori" so_reg_imm (EPackedOperand "SoRegImm")
                           :> ParamDef "rN" gpr naturalBV
                           )
                 $ \rD setcc _ sori rN -> do
    comment "ADD register, A32, Encoding A1 (F7.1.7, F7-2546)"
    input sori
    input setcc
    input rN
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        ty = soRegImm_type sori
        rM = soRegImm_reg sori
        imm = soRegImm_imm sori
        (shift_t, shift_n) = splitImmShift (decodeImmShift ty imm)
    addrr rD rM (Loc rN) setflags shift_t shift_n

  defineA32Opcode A.ADCrsr (  Empty
                           :> ParamDef "rD" gprnopc naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sorr" so_reg_reg (EPackedOperand "SoRegReg")
                           :> ParamDef "rN" gprnopc naturalBV
                           )
                 $ \rD setcc _ sorr rN -> do
    comment "ADC (register-shifted register), Encoding A1 (F7.1.3, F7-2538)"
    input rD
    input sorr
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    let shift_t = decodeRegShift (soRegReg_type sorr)
    let rS = soRegReg_reg1 sorr
    let rM = soRegReg_reg2 sorr
    adcrsr rD rM rN setflags shift_t rS

  defineA32Opcode A.ADDrsr (  Empty
                           :> ParamDef "rD" gpr naturalBV
                           :> ParamDef "setcc" cc_out (EBV 1)
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "sorr" so_reg_reg (EPackedOperand "SoRegReg")
                           :> ParamDef "rN" gpr naturalBV
                           )
                 $ \rD setcc _ sorr rN -> do
    comment "ADD (register-shifted register), Encoding A1 (F7.1.8, F7-2547)"
    input rD
    input sorr
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
    let shift_t = decodeRegShift (soRegReg_type sorr)
    let rS = soRegReg_reg1 sorr
    let rM = soRegReg_reg2 sorr
    addrsr rD rM rN setflags shift_t rS

  defineT32Opcode T.TADDrr (Empty
                           :> ParamDef "rD" tgpr naturalBV
                           :> ParamDef "rM" tgpr naturalBV
                           :> ParamDef "rN" tgpr naturalBV
                           ) $ \rD rM rN -> do
    comment "ADD register, T32, Encoding T1 (F7.1.6, F7-2544)"
    input rM
    input rN

    addrr rD rM (Loc rN) (notp inITBlock) srtLSL (LitBV 32 0)

  defineT32Opcode T.TADDhirr (Empty
                             :> ParamDef "rDN" gpr naturalBV
                             :> ParamDef "rM"  gpr naturalBV
                             ) $ \rDN rM -> do
    comment "ADD register, T32, Encoding T2 (F7.1.6, F7-2544)"
    input rM
    input rDN

    addrr rDN rM (Loc rDN) (LitBool False) srtLSL (LitBV 32 0)

  defineT32Opcode T.T2ADDrr (Empty
                             :> ParamDef "rD" gprnopc naturalBV
                             :> ParamDef "setcc" cc_out (EBV 1)
                             :> ParamDef "rN" rgpr naturalBV
                             :> ParamDef "rM" gprnopc naturalBV
                            ) $ \rD setcc rN rM -> do
    comment "ADD register, T32, encoding T3 [no shift] (F7.1.6, F7-2544)"
    input rM
    input rN
    input setcc

    addrr rD rM (Loc rN) (bveq (Loc setcc) (LitBV 1 0b1)) srtLSL (LitBV 32 0)

  defineT32Opcode T.T2ADDrs (Empty
                             :> ParamDef "rD" gprnopc naturalBV
                             :> ParamDef "setcc" cc_out (EBV 1)
                             -- TODO: Ask Kevin why we use slightly different strings here.
                             :> ParamDef "rN" t2_so_reg (EPackedOperand "T2_So_Reg")
                             :> ParamDef "rM" gprnopc naturalBV
                            ) $ \rD setcc rN_so_reg rM -> do
    comment "ADD register, T32, encoding T3 [no shift] (F7.1.6, F7-2544)"
    input rM
    input rN_so_reg
    input setcc

    let rNexpr  = t2SoReg_reg  rN_so_reg
        shift_n = zext (t2SoReg_imm  rN_so_reg)
        shift_t = SRType (t2SoReg_type rN_so_reg)

    addrr rD rM rNexpr (bveq (Loc setcc) (LitBV 1 0b1)) shift_t shift_n

  defineA32Opcode A.MOVr (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "rM" gpr naturalBV
                         )
                      $ \rD setcc _ rM -> do
    comment "MOV register, A32, Encoding A1  (F7.1.109, F7-2712)"
    input rM
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        unPred = LitBool False
    movr rD rM setflags unPred

  defineA32Opcode A.MOVsi (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "regimm" shift_so_reg_imm (EBV 16)
                         )
                      $ \rD setcc _ imm -> do
    comment "MOV immediate, A32, Encoding A1  (F7.1.107, F7-2708)"
    -- note: that encoding is instr 0xe3auDiii, u=undefined, D=rD, i=imm, s=lo-bit of a
    -- but actual is:               0xe1a0Diii
    input setcc
    input imm
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        imm12 = extract 11 0 (Loc imm)
        (_,_,c,v) = getNZCV
        (imm32, c') = armExpandImmC' imm12 c
        result = imm32
        n = extract 31 31 result
        z = isZeroBit result
        nzcv = concat n $ concat z $ concat c' v
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

  -- TODO Ben: MOVsi encoding A2, thumb encodings

  defineT32Opcode T.TMOVr (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "rM" gpr naturalBV
                          )
                      $ \rD rM -> do
    comment "MOV register, T32, Encoding T1  (F7.1.108, F7-2710)"
    input rM
    let setflags = LitBool False
        unPred = (andp (isR15 rD)
                       (andp inITBlock (notp lastInITBlock)))
    return ()
    movr rD rM setflags unPred

  -- TODO Ben: finish all sub encodings

  defineA32Opcode A.SUBri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm (EPackedOperand "ModImm")
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

  defineT32Opcode T.TSUBi3 (Empty
                           :> ParamDef "rD" tgpr naturalBV
                           :> ParamDef "imm" imm0_7 (EBV 3)
                           :> ParamDef "rN" tgpr naturalBV
                           )
                       $ \rD imm3 rN -> do
    comment "SUB immediate, T32, encoding T1 (F7.1.234, F7-2914)"
    input rN
    input imm3
    tsubri rD rN (zext (Loc imm3)) (LitBool True) (notp inITBlock)


  defineT32Opcode T.TSUBi8 (Empty
                           :> ParamDef "rDn" tgpr naturalBV
                           :> ParamDef "imm" imm0_255 (EBV 8)
                           )
                       $ \rDn imm8 -> do
    comment "SUB immediate, T32, encoding T2 (F7.1.234, F7-2914)"
    input imm8
    tsubri rDn rDn (zext (Loc imm8)) (LitBool True) (notp inITBlock)

  defineT32Opcode T.T2SUBri (Empty
                            :> ParamDef "rD" gprnopc naturalBV
                            :> ParamDef "setcc" cc_out (EBV 1)
                            -- TODO: Ask Kevin why we have to use two slightly
                            -- different strings here...
                            :> ParamDef "imm" t2_so_imm (EPackedOperand "T2_So_Imm")
                            :> ParamDef "rN" gprnopc naturalBV
                            )
                        $ \rD setcc imm16 rN -> do
    comment "SUB immediate, T32, encoding T3 (F7.1.234, F7-2914)"
    input rN
    input imm16
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        imm32    = thumbExpandImm imm16
        undef    = orp (andp (isR15 rD) (notp setflags)) (isR15 rN)
    tsubri rD rN imm32 undef setflags

  defineT32Opcode T.T2SUBri12 (Empty
                               :> ParamDef "rD" gprnopc naturalBV
                               :> ParamDef "imm" imm0_4095 (EBV 16)
                               :> ParamDef "rN" gpr naturalBV
                              )
                        $ \rD imm12 rN -> do
    comment "SUB immediate, T32, encoding T4 (F7.1.4, F7-2540)"
    input rN
    input imm12  -- n.b. encodes 12 bits, but Dismantle provides 16 bits (assumed zext)
    tsubri rD rN (zext (Loc imm12)) (LitBool False) (LitBool False)

  defineA32Opcode A.SUBrr (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "rM" gpr naturalBV
                          :> ParamDef "rN" gpr naturalBV
                          ) $ \rD setcc _ rM rN -> do
    comment "SUB register, A32, Encoding A1  (F7.1.236, F7-2918?)"
    input rM
    input rN
    input setcc
    let setflags = bveq (Loc setcc) (LitBV 1 0b1)
        (result, nzcv) = addWithCarry (Loc rN) (bvnot (Loc rM)) (LitBV 32 1)
    defReg rD (ite (isR15 rD) (Loc rD) result)
    aluWritePC (isR15 rD) result
    cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv


------------------------------------------------------------------------

-- TODO Ben: finish out all thumb encodings in here
manualBitwise :: (HasCallStack) => SemARM 'Top ()
manualBitwise = do

  defineA32Opcode A.ANDri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm (EPackedOperand "ModImm")
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
                           :> ParamDef "sori" so_reg_imm (EPackedOperand "SoRegImm")
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
                           :> ParamDef "sorr" so_reg_reg (EPackedOperand "SoRegReg")
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
                            :> ParamDef "rD" rgpr naturalBV
                            :> ParamDef "setcc" cc_out (EBV 1)
                            :> ParamDef "rN" rgpr naturalBV
                            :> ParamDef "rM" rgpr naturalBV
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

  defineA32Opcode A.CMPri (Empty
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "modimm" mod_imm (EPackedOperand "ModImm")
                          :> ParamDef "rN" gpr naturalBV
                          :> ParamDef "unpredictable" unpredictableInstrBits naturalBV
                          )
                      $ \_ mimm rN _unpred -> do
    comment "Compare immediate, Encoding A1"
    comment "doc: F7.1.36, page F7-2589"
    comment "unpredictable argument is ignored"
    input mimm
    input rN
    let imm32 = armExpandImm mimm
        (_, nzcv) = addWithCarry (Loc rN) (bvnot imm32) (LitBV 32 1)
    cpsrNZCV (LitBool True) nzcv

  defineT32Opcode T.TCMPi8 (Empty
                           :> ParamDef "imm" imm0_255 (EBV 8)
                           :> ParamDef "rN" tgpr naturalBV
                           )
                      $ \imm rN -> do
    comment "Compare immediate, Encoding T1 (F7.1.36, F7-2589)"
    input imm
    input rN
    let imm32 = zext $ Loc imm
        (_, nzcv) = addWithCarry (Loc rN) (bvnot imm32) (LitBV 32 1)
    cpsrNZCV (LitBool True) nzcv

  defineT32Opcode T.TLSLri (Empty
                           :> ParamDef "rD" tgpr naturalBV
                           :> ParamDef "imm" imm0_31 (EBV 5)
                           :> ParamDef "rM" tgpr naturalBV
                           )
                      $ \rD imm5 rM -> do
    comment "Logical Shift Left, Encoding T1"
    comment "doc: F7.1.99, page F7-2692"
    input imm5
    input rM
    let (_, shift_n) = splitImmShift $ decodeImmShift (LitBV 2 00) (Loc imm5)
        setflags = notp inITBlock
    lsl rD shift_n rM setflags

  defineA32Opcode A.ORRri (Empty
                          :> ParamDef "rD" gpr naturalBV
                          :> ParamDef "setcc" cc_out (EBV 1)
                          :> ParamDef "predBits" pred (EBV 4)
                          :> ParamDef "mimm" mod_imm (EPackedOperand "ModImm")
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
adcrr :: (HasCallStack)
      => Location 'TBV
      -> Location 'TBV
      -> Expr 'TBV -> Expr 'TBool -> SRType -> Expr 'TBV -> SemARM 'Def ()
adcrr rD rM rNexpr setflags shift_t shift_n = do
  let (_, _, c, _) = getNZCV
      shifted = shift (Loc rM) shift_t shift_n c
      (result, nzcv') = addWithCarry rNexpr shifted (zext c)
      nzcv = "nzcv" =: nzcv'
  defReg rD (ite (isR15 rD) (Loc rD) result)
  aluWritePC (isR15 rD) result
  cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

-- Note: the rN argument is an Expr 'TBV rather than Location 'TBV. The reason for
-- this is that the actual register id is sometimes inside a packed operand, and we
-- can only extract in in Expr form. Therefore, if we call this function on a
-- Location 'TBV for rN, we need to wrap it up with (Loc rN).
addrr :: (HasCallStack)
      => Location 'TBV
      -> Location 'TBV
      -> Expr 'TBV -> Expr 'TBool -> SRType -> Expr 'TBV -> SemARM 'Def ()
addrr rD rM rNexpr setflags shift_t shift_n = do
  let (_, _, c, _) = getNZCV
      shifted = shift (Loc rM) shift_t shift_n c
      (result, nzcv') = addWithCarry rNexpr shifted (LitBV 32 0)
      nzcv = "nzcv" =: nzcv'
  defReg rD (ite (isR15 rD) (Loc rD) result)
  aluWritePC (isR15 rD) result
  cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

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

adcrsr :: (HasCallStack) =>
          Location 'TBV
       -> Location 'TBV
       -> Location 'TBV -> Expr 'TBool -> SRType -> Location 'TBV -> SemARM 'Def ()
adcrsr rD rM rN setflags shift_t rS = do
  let (_, _, c, _) = getNZCV
      shift_n = zext $ extract 7 0 (Loc rS)
  let shifted = shift (Loc rM) shift_t shift_n c
  let (result, nzcv') = addWithCarry (Loc rN) shifted (zext c)
  let nzcv = "nzcv" =: nzcv'
  let writesOrReadsR15 = anyp $ fmap isR15 [ rD, rM, rN, rS ]
  defReg rD (ite writesOrReadsR15 (unpredictable (Loc rD)) result)
  cpsrNZCV (andp setflags (notp writesOrReadsR15)) nzcv

addrsr :: (HasCallStack) =>
          Location 'TBV
       -> Location 'TBV
       -> Location 'TBV -> Expr 'TBool -> SRType -> Location 'TBV -> SemARM 'Def ()
addrsr rD rM rN setflags shift_t rS = do
  let (_, _, c, _) = getNZCV
      shift_n = zext $ extract 7 0 (Loc rS)
  let shifted = shift (Loc rM) shift_t shift_n c
  let (result, nzcv') = addWithCarry (Loc rN) shifted (LitBV 32 0)
  let nzcv = "nzcv" =: nzcv'
  let writesOrReadsR15 = anyp $ fmap isR15 [ rD, rM, rN, rS ]
  defReg rD (ite writesOrReadsR15 (unpredictable (Loc rD)) result)
  cpsrNZCV (andp setflags (notp writesOrReadsR15)) nzcv

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

taddri :: (HasCallStack)
     => Location 'TBV
     -> Location 'TBV
     -> Expr 'TBV
     -> Expr 'TBool
     -> Expr 'TBool
     -> SemARM 'Def ()
taddri rD rN imm32 setflags undef = do
  let (result, nzcv) = addWithCarry (Loc rN) imm32 (LitBV 32 0)
  defReg rD (ite undef (unpredictable (Loc rD)) result)
  cpsrNZCV (andp setflags (andp (notp (isR15 rD)) (notp undef))) nzcv

tsubri :: (HasCallStack)
     => Location 'TBV
     -> Location 'TBV
     -> Expr 'TBV
     -> Expr 'TBool
     -> Expr 'TBool
     -> SemARM 'Def ()
tsubri rD rN imm32 setflags undef = do
  let (result, nzcv) = addWithCarry (Loc rN) (bvnot imm32) (LitBV 32 1)
  defReg rD (ite undef (unpredictable (Loc rD)) result)
  cpsrNZCV (andp setflags (andp (notp (isR15 rD)) (notp undef))) nzcv

taddSP :: (HasCallStack) =>
         Location 'TBV
      -> Expr 'TBV
      -> Expr 'TBool
      -> SemARM 'Def ()
taddSP rD imm32 setflags = do
  input sp
  let (result, nzcv) = addWithCarry (Loc sp) imm32 (LitBV 32 0b0)
  defReg rD $ ite (isR15 rD) (Loc rD) result
  aluWritePC (isR15 rD) result
  cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

lsl :: Location 'TBV -> Expr 'TBV -> Location 'TBV -> Expr 'TBool -> SemARM 'Def ()
lsl rD shift_n rM setflags = do
  let (n,z,c,v) = getNZCV
  let shiftres = shiftC (Loc rM) srtLSL shift_n c
      result = extract 31 0 shiftres
      c' = extract 32 32 shiftres
  defReg rD (ite (isR15 rD) (Loc rD) result)
  aluWritePC (isR15 rD) result
  let nzcv = "nzcv" =: concat n (concat z (concat c' v))
  cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv

movr :: Location 'TBV -> Location 'TBV -> Expr 'TBool -> Expr 'TBool -> SemARM 'Def ()
movr rD rM setflags unPred = do
  let result = Loc rM
      (_,_,c,v) = getNZCV
      n = extract 31 31 result
      z = isZeroBit result
      nzcv = concat n $ concat z $ concat c v
  defReg rD (ite (isR15 rD) (Loc rD) (ite unPred (unpredictable result) result))
  aluWritePC (isR15 rD) (ite unPred (unpredictable result) result)
  cpsrNZCV (andp setflags (notp (isR15 rD))) nzcv
