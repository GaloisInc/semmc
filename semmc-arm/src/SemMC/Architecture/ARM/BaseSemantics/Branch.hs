{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module SemMC.Architecture.ARM.BaseSemantics.Branch
    ( manualBranches
    )
    where

import Data.Maybe
import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


manualBranches :: SemARM 'Top ()
manualBranches = do
  a32_branches
  t32_branches

a32_branches :: SemARM 'Top ()
a32_branches = do
  blx_a32

t32_branches :: SemARM 'Top ()
t32_branches = do
  blx_t32

-- ----------------------------------------------------------------------

blx_a32 :: SemARM 'Top ()
blx_a32 =
    defineA32Branch "BLXi" $ do
      comment "Encoding A2"
      tgt <- param "target" arm_blx_target naturalBV
      input tgt
      let tgtarch = InstrSet_T32
          imm24 = extract 23 0 (Loc tgt)
          immH = extract 24 24 (Loc tgt)
          imm32 = sext $ concat imm24 $ concat immH $ LitBV 1 0
          newlr = (bvsub (Loc pc) (naturalLitBV 0x4))
      label <- target_label_align4 imm32
      blx_ newlr label tgtarch

blx_t32 :: SemARM 'Top ()
blx_t32 =
    defineT32Branch "TBLXi" $ do
      comment "Encoding T2"
      tgt <- param "target" thumb_blx_target naturalBV
      let tgtarch = InstrSet_A32
          tgt_S = blxtgt_S tgt
          tgt_imm10H = blxtgt_imm10H tgt
          tgt_imm10L = blxtgt_imm10L tgt
          tgt_J1 = blxtgt_J1 tgt
          tgt_J2 = blxtgt_J2 tgt
          i1 = bvnot $ bvxor tgt_J1 tgt_S
          i2 = bvnot $ bvxor tgt_J2 tgt_S
          imm32 = sext $ concat tgt_S $ concat i1 $ concat i2 $
                         concat tgt_imm10H $ concat tgt_imm10L $ LitBV 2 0x00
          newlr = concat (extract 31 1 (Loc pc)) (LitBV 1 0b1)
      label <- target_label_align4 imm32
      blx_ newlr label tgtarch

blx_ :: Expr 'TBV  -- ^ new LR value
     -> Expr 'TBV  -- ^ target label (address)
     -> ArchSubtype  -- ^ target architecture subtype
     -> SemARM 'Def ()
blx_ newlr tgtaddr tgtarch = do
    comment "Used to call a subroutine (branch) and switch between A32 and T32 states."
    comment "Branch with Link and Exchange (F7.1.25, F7-2576)"
    comment "Writes to PC, not R15."
    -- Assembler specifies the label of the instruction being branched
    -- to.  The encoding is a sign-extended immediate offset added to
    -- the PC value of the instruction to form the target
    -- address. (F1.1.2, F1-2381) The PC value of an instruction is
    -- its address plus 4 (T32) or 8 (A32) (F1.1.2, F1-2380).
    input lr
    defReg lr newlr
    selectInstrSet tgtarch
    branchWritePC tgtaddr


-- ----------------------------------------------------------------------

branchWritePC :: Expr 'TBV -> SemARM 'Def ()
branchWritePC addr = do  -- (E1.2.3, E1-2296)
    curarch <- (subArch . fromJust) <$> getArchData
    let address = if curarch == InstrSet_A32
                  then bvclr [0,1] addr
                  else bvclr [1] addr
    defReg pc address


selectInstrSet :: ArchSubtype -> SemARM 'Def ()
selectInstrSet tgtarch = do  -- (E1.2.3, E1-2300)
    -- assumes already: input cpsr
    curarch <- (subArch . fromJust) <$> getArchData
    let (j,t) = case tgtarch of
                      InstrSet_A32 -> if curarch == InstrSet_T32EE
                                     then error "Invalid INSTRSET change T32EE->A32"
                                     else (0, 0)
                      InstrSet_T32 -> (0, 1)
                      InstrSet_T32EE -> if curarch == InstrSet_A32
                                       then error "Invalid INSTRSET change A32-T32EE"
                                       else (1 :: Int, 1 :: Int)
                      _ -> error "INSTRSET unreachable"
        cpsr'j = if j == 1
                 then bvset [24] (Loc cpsr)
                 else bvclr [24] (Loc cpsr)
        cpsr'jt = if t == 1
                  then bvset [5] cpsr'j
                  else bvclr [5] cpsr'j
    defReg cpsr cpsr'jt


target_label_align4 :: Expr 'TBV -> SemARM 'Def (Expr 'TBV)
target_label_align4 off = do  -- Align(PC, 4) to force word-alignment (only affects T32, not A32).
    input pc
    let alignPC_4 = bvclr [0,1] (Loc pc)
    return $ bvadd alignPC_4 off
