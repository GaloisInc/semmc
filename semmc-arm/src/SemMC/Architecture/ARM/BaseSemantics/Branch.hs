{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module SemMC.Architecture.ARM.BaseSemantics.Branch
    ( manualBranches
    )
    where

import           Data.Maybe ( fromJust )
import           Data.Parameterized.Context
import           Data.Semigroup
import qualified Dismantle.ARM as A
import qualified Dismantle.Thumb as T
import           Prelude hiding ( concat, pred )
import           SemMC.Architecture.ARM.BaseSemantics.Base
import           SemMC.Architecture.ARM.BaseSemantics.Helpers
import           SemMC.Architecture.ARM.BaseSemantics.Natural
import           SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Arithmetic
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ExecState
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Registers
import           SemMC.Architecture.ARM.BaseSemantics.Registers
import           SemMC.DSL
import           SemMC.Util ( fromJust' )


manualBranches :: SemARM 'Top ()
manualBranches = do
  a32_branches
  t32_branches

-- ----------------------------------------------------------------------

a32_branches :: SemARM 'Top ()
a32_branches = do
  blx_a32

  defineA32Opcode A.BX_RET (Empty
                           :> ParamDef "predBits" pred (EBV 4)
                           )
                      $ \_ -> do
    comment "BX, returning (target addr in LR), Encoding A1"
    comment "F7.1.27, F7-2579"
    input lr
    bxWritePC (LitBool True) (Loc lr)

  defineA32Opcode A.Bcc (Empty
                        :> ParamDef "predBits" pred (EBV 4)
                        :> ParamDef "brTarget" arm_br_target naturalBV
                        )
                      $ \_ tgt -> do
    comment "B, branch, Encoding A1"
    comment "F7.1.18, F7-2566"
    input tgt
    let imm24 = extract 23 0 (Loc tgt)
        imm32 = "imm32" =: (sext $ concat imm24 $ LitBV 2 0b00)
        unPredictable = LitBool False
    branchWritePCRel imm32 unPredictable

  defineA32Opcode A.BL (Empty
                       :> ParamDef "blTarget" arm_bl_target naturalBV
                       )
                      $ \tgt -> do
    comment "BL, branch with link, Encoding A1"
    comment "F7.1.25, F7-2576"
    input tgt
    input pc
    curarch <- (subArch . fromJust' "curarch") <$> getArchData
    let imm24 = extract 23 0 (Loc tgt)
        imm32 = "imm32" =: (sext $ concat imm24 $ LitBV 2 0b00)
        tgtarch = InstrSet_A32
        newlr = if curarch == InstrSet_A32
                then bvsub (Loc pc) (LitBV 32 0x4)  -- n.b. PC is cur-instr + 8
                else concat (extract 31 1 (Loc pc)) (LitBV 1 0b1)
    blx_ newlr imm32 tgtarch


blx_a32 :: SemARM 'Top ()
blx_a32 =
    defineA32Opcode A.BLXi (Empty
                           :> ParamDef "target" arm_blx_target naturalBV
                           )
                        $ \tgt -> do
      comment "Branch with Link and Exchange (F7.1.25, F7-2576)"
      comment "Encoding A2"
      input tgt
      let tgtarch = InstrSet_T32
          imm24 = extract 23 0 (Loc tgt)
          immH = extract 24 24 (Loc tgt)
          imm32 = "imm32" =: (sext $ concat imm24 $ concat immH $ LitBV 1 0)
          newlr = "newlr" =: (bvsub (Loc pc) (naturalLitBV 0x4))
      label <- target_label_align4 imm32
      blx_ newlr label tgtarch


-- ----------------------------------------------------------------------

t32_branches :: SemARM 'Top ()
t32_branches = do
  blx_t32

  defineT32Opcode T.TBcc (Empty
                         :> ParamDef "predBits" pred (EBV 4)
                         :> ParamDef "bccTarget" thumb_bcc_target (EBV 8)
                        )
                      $ \_ tgt -> do
    comment "B, branch, Encoding T1"
    comment "F7.1.18, F7-2566"
    input tgt
    let imm8 = tgt
        imm32 = "imm32" =: (sext $ concat (Loc imm8) $ LitBV 1 0b0)
    comment "Not permitted in IT block"
    let unPredictable = inITBlock
    branchWritePCRel imm32 unPredictable

  defineT32Opcode T.TBX (Empty
                        :> ParamDef "rM" gpr naturalBV
                        :> ParamDef "unpredictable" unpredictableInstrBits naturalBV
                        )
                      $ \rM _ -> do
      comment "Branch and Exchange, Encoding T1 (F7.1.27, F7-2579)"
      comment "unpredictable argument is ignored"
      input rM
      let unpred = andp inITBlock (notp lastInITBlock)
      bxWritePC (LitBool True) (ite unpred (unpredictable (Loc rM)) (Loc rM))


blx_t32 :: SemARM 'Top ()
blx_t32 =
    defineT32Opcode T.TBLXi (Empty
                            :> ParamDef "target" thumb_blx_target (EPackedOperand "ThumbBlxTarget")
                            )
                        $ \tgt -> do
      comment "Branch with Link and Exchange (F7.1.25, F7-2576)"
      comment "Encoding T2"
      input tgt
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
          unpred = andp inITBlock (notp lastInITBlock)
      label <- target_label_align4 (ite unpred (unpredictable imm32) imm32)
      blx_ newlr label tgtarch


-- ----------------------------------------------------------------------

blx_ :: Expr 'TBV  -- ^ new LR value
     -> Expr 'TBV  -- ^ target label (address)
     -> ArchSubtype  -- ^ target architecture subtype
     -> SemARM 'Def ()
blx_ newlr tgtaddr tgtarch = do
    curarch <- (subArch . fromJust' "blx_") <$> getArchData
    let switching = curarch /= tgtarch
        switchmsg = if switching
                    then " and switch from " <> show curarch <> " to " <> show tgtarch
                    else ""
    comment $ "Used to call a subroutine (branch)" <> switchmsg
    comment "Writes to PC, not R15."
    -- Assembler specifies the label of the instruction being branched
    -- to.  The encoding is a sign-extended immediate offset added to
    -- the PC value of the instruction to form the target
    -- address. (F1.1.2, F1-2381) The PC value of an instruction is
    -- its address plus 4 (T32) or 8 (A32) (F1.1.2, F1-2380).
    input lr
    defReg lr newlr
    selectInstrSet' tgtarch
    branchWritePC (LitBool True) tgtaddr


-- ----------------------------------------------------------------------

target_label_align4 :: Expr 'TBV -> SemARM 'Def (Expr 'TBV)
target_label_align4 off = return $ bvadd off $ align (Loc pc) 4
