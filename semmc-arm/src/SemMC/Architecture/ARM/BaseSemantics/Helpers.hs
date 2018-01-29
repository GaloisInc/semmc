{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Helpers
    ( defineA32Opcode
    , defineA32OpcodeNoPred
    , defineA32Branch
    , defineT32Opcode
    , defineT32Branch
    , instrSetState
    , defReg
    , zext, zext'
    , sext, sext'
    , imm12Reg, imm12Imm, imm12Add
    , blxtgt_S, blxtgt_imm10H, blxtgt_imm10L, blxtgt_J1, blxtgt_J2
    , bvset, bvclr
    , unpredictable
    , sameLocation
    , isR15
    )
    where

import Control.Monad ( unless )
import Data.Bits
import Data.Maybe
import Data.Parameterized.Classes
import Data.Parameterized.Some ( Some(..) )
import Data.Semigroup
import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


-- | A wrapper around 'defineOpcode' that sets A32 mode and updates
-- the PC after the instruction executes (by 4 if A32 or 2 if T32).
-- Do not use for branches or any operation that might update R15/PC.
defineA32Opcode :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineA32Opcode = defineOpcode'A32 False False

-- | An alternative version of 'defineA32Opcode' for opcodes which do
-- not have a Pred operand that controls the expression of the opcode.
defineA32OpcodeNoPred :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineA32OpcodeNoPred = defineOpcode'A32 True False

-- | An alternative version of 'defineA32Opcode' for branch opcodes
-- which explicitly update the PC and therefore should not have the PC
-- automatically updated to the next instruction.
defineA32Branch :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineA32Branch = defineOpcode'A32 False True


defineOpcode'A32 :: HasCallStack => Bool -> Bool -> String -> SemARM 'Def () -> SemARM 'Top ()
defineOpcode'A32 noPred isBranch name def =
    if a32OpcodeNameLooksValid name
    then defineOpcode name $ do
      subarch InstrSet_A32
      unless (noPred) $ do
        predV <- param "predBits" pred (EBV 4)  -- CurrentCond() (F2.3.1, F2-2417)
        input cpsr
        testForConditionPassed (Loc predV)
      def
      unless isBranch updatePC
    else error $ "Opcode " <> name <> " does not look like a valid A32 ARM opcode"


-- | A wrapper around 'defineOpcode' that sets A32 mode and updates
-- the PC after the instruction executes (by 4 if A32 or 2 if T32).
-- Do not use for branches or any operation that might update R15/PC.
defineT32Opcode :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineT32Opcode = defineOpcode'T32 False

-- | An alternative form of defineT32Opcode that is used for Branch
-- opcodes; all PC updates are performed explicitly by the opcode
-- definition and not by this entry point.
defineT32Branch :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineT32Branch = defineOpcode'T32 True

defineOpcode'T32 :: HasCallStack => Bool -> String -> SemARM 'Def () -> SemARM 'Top ()
defineOpcode'T32 isBranch name def =  --- inIT, notInIT, lastInIT
    if t32OpcodeNameLooksValid name
    then defineOpcode name $ do
      subarch InstrSet_T32

      input cpsr
      let itstate_7_4 = extract 15 12 (Loc cpsr)
          itstate_3_2 = extract 11 10 (Loc cpsr)
          itstate_1_0 = extract 26 25 (Loc cpsr)
          itstate_3_0 = concat itstate_3_2 itstate_1_0
          -- CurrentCond() for T32 except T1 and T3 encodings of the Branch instruction (F2.3.1, F2-2417)
          predV = ite (bveq itstate_3_0 (LitBV 4 0b0000))
                  (ite (bveq itstate_7_4 (LitBV 4 0b0000))
                   (LitBV 4 0b1110)
                   (unpredictable (LitBV 4 0b1110))) -- ARM doc doesn't cover this case...
                  itstate_7_4
      testForConditionPassed predV

      def
      unless isBranch updatePC
    else error $ "Opcode " <> name <> " does not look like a valid T32 ARM opcode"



a32OpcodeNameLooksValid :: String -> Bool
a32OpcodeNameLooksValid name =
    take 1 name /= "T" ||
    take 3 name == "TEQ" ||
    take 4 name == "TRAP" ||
    take 3 name == "TST"

t32OpcodeNameLooksValid :: String -> Bool
t32OpcodeNameLooksValid name = take 1 name == "T"


-- ----------------------------------------------------------------------

-- | As described for the 'SemM_ARMData' datatype, the DSL state is
-- used to store the current instruction set type (normally A32 or
-- T32).
subarch :: ArchSubtype -> SemARM t ()
subarch sa =
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ newARMData { subArch = sa }
                              Just ad -> Just $ ad { subArch = sa })


-- | The processor mode can be determined by examining the ISETSTATE
-- execution state register, which is embedded in the CPSR (as 'J' at
-- bit 24 and 'T' at bit 5).  The following can be used to read the
-- current processor mode.  This is not normally used however because
-- the current instruction set (arch subtype) is set in the DSL state
-- (see 'SemM_ARMData').  [If this function becomes useful, add the
-- ArchSubtype to the ExprTag in the DSL definition to return the
-- value directly instead of returning the string form.]
instrSetState :: Location 'TBV -> Expr 'TString
instrSetState cpsReg =
    let cpsr_j = testBitDynamic (LitBV 32 24) (Loc cpsReg)
        cpsr_t = testBitDynamic (LitBV 32 5) (Loc cpsReg)
        isT32 = andp cpsr_t (notp cpsr_j)
        isA32 = andp (notp cpsr_t) (notp cpsr_j)
        -- isJazelle = andp (notp cpsr_t) cpsr_j
        isT32EE = andp cpsr_t cpsr_j
        toRet = LitString . show
    in ite isA32 (toRet InstrSet_A32)
           (ite isT32 (toRet InstrSet_T32)
            (ite isT32EE (toRet InstrSet_T32EE) (toRet InstrSet_Jazelle)))



-- | Update the PC to the next instruction.  The PC always points to
-- the current instruction (plus an offset), so the update should be
-- done as the final step of the semantic modifications for each
-- instruction.  The mode is always known here, as initially set by
-- 'defineA32Opcode' or 'defineT32Opcode' and possibly updated during
-- instruction execution.
updatePC :: SemARM 'Def ()
updatePC = do
  input pc
  instrSet <- (subArch . fromJust) <$> getArchData
  let updPCVal = case instrSet of
                   InstrSet_A32 -> bvadd (Loc pc) (naturalLitBV 4)
                   InstrSet_T32 -> bvadd (Loc pc) (naturalLitBV 2)
                   _ -> error "Execution PC update not currently supported for this arch subtype"
  defLoc pc updPCVal


-- ----------------------------------------------------------------------

-- | Stores the expression that calculates the ConditionPassed result
-- (see the ARM documentation pseudocode) for the current instruction
-- so that subsequent defLoc operations can use this to determine if
-- they are enabled to effect the changes from the Opcode.
testForConditionPassed :: Expr 'TBV -> SemARM 'Def ()
testForConditionPassed instrPred = do
    -- assumes already: input cpsr
    let pstate_n = extract 31 31 (Loc cpsr)  -- (E1.2.4, E1-2297)
        pstate_z = extract 30 30 (Loc cpsr)
        pstate_c = extract 29 29 (Loc cpsr)
        pstate_v = extract 28 28 (Loc cpsr)
        cond_3_1 = extract 3 1 instrPred
        cond_0   = extract 0 0 instrPred
        isBitSet = bveq (LitBV 1 0b1)
        -- ConditionHolds (F2.3.1, F2-2417):
        result = ite (bveq cond_3_1 (LitBV 3 0b000)) (isBitSet pstate_z) -- EQ or NE
                 (ite (bveq cond_3_1 (LitBV 3 0b001)) (isBitSet pstate_c) -- CS or CC
                  (ite (bveq cond_3_1 (LitBV 3 0b010)) (isBitSet pstate_n) -- MI or PL
                   (ite (bveq cond_3_1 (LitBV 3 0b011)) (isBitSet pstate_v) -- VS or VC
                    (ite (bveq cond_3_1 (LitBV 3 0b100)) (andp (isBitSet pstate_c)
                                                               (notp $ isBitSet pstate_z)) -- HI or LS
                     (ite (bveq cond_3_1 (LitBV 3 0b101)) (bveq pstate_n pstate_v)  -- GE or LT
                      (ite (bveq cond_3_1 (LitBV 3 0b110)) (andp (bveq pstate_n pstate_v)
                                                                 (notp $ isBitSet pstate_z)) -- GT or LE
                       {- (bveq cond_3_1 (LitBV 3 0b111)) -} (LitBool True))))))) -- AL
        result' = ite (andp (isBitSet cond_0)
                            (bvne instrPred (LitBV 4 0b1111))) (notp result) result
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ newARMData { condPassed = result' }
                              Just ad -> Just $ ad { condPassed = result' })


-- | Performs an assignment for a conditional Opcode when the target
-- is a register location.
defReg :: Location a -> Expr a -> SemARM 'Def ()
defReg loc expr = do
  isOK <- (condPassed . fromJust) <$> getArchData
  defLoc loc $ ite isOK expr (Loc loc)


-- ----------------------------------------------------------------------

-- | Zero extension to the full native bit width of registers
zext :: Expr 'TBV -> Expr 'TBV
zext = zext' naturalBitSize


-- | Generalized zero extension to arbitrary width
zext' :: Int -> Expr 'TBV -> Expr 'TBV
zext' fullWidth e
  | extendBy == 0 = e
  | otherwise = zeroExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e

-- | Zero extension to the full native bit width of registers
sext :: Expr 'TBV -> Expr 'TBV
sext = sext' naturalBitSize


-- | Generalized zero extension to arbitrary width
sext' :: Int -> Expr 'TBV -> Expr 'TBV
sext' fullWidth e
  | extendBy == 0 = e
  | otherwise = signExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e


-- ----------------------------------------------------------------------
-- Opcode unpacking

-- | User defined function to place in the SemMC DST to get the value
-- contained in the register referenced by the addrmode_imm12_[pre].
-- During the Eval stage this will perform the actual extraction of the value.
imm12Reg :: Location 'TMemRef -> Location 'TBV
imm12Reg = locUF naturalBV "a32.imm12_reg"

-- | Returns the immediate value in the addrmode_imm12_[pre]
imm12Imm :: [Some Expr] -> Expr 'TBV
imm12Imm = uf naturalBV "a32.imm12_imm"

-- | Returns the addition flag in the addrmode_imm12_[pre]
imm12Add :: [Some Expr] -> Expr 'TBool
imm12Add = uf EBool "a32.imm12_add"

-- | Decoding for the ThumbBlxTarget type: S bit
blxtgt_S :: Location 'TBV -> Expr 'TBV
blxtgt_S = uf (EBV 1) "t32.blxtarget_S" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: Imm10H
blxtgt_imm10H :: Location 'TBV -> Expr 'TBV
blxtgt_imm10H = uf (EBV 10) "t32.blxtarget_imm10H" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: Imm10L
blxtgt_imm10L :: Location 'TBV -> Expr 'TBV
blxtgt_imm10L = uf (EBV 10) "t32.blxtarget_imm10L" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: J1
blxtgt_J1 :: Location 'TBV -> Expr 'TBV
blxtgt_J1 = uf (EBV 1) "t32.blxtarget_J1" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: J2
blxtgt_J2 :: Location 'TBV -> Expr 'TBV
blxtgt_J2 = uf (EBV 1) "t32.blxtarget_J2" . ((:[]) . Some) . Loc


-- ----------------------------------------------------------------------

memriOffset :: Int
            -- ^ The number of bits of the offset
            -> Expr 'TMemRef
            -- ^ The memory ref expression
            -> Expr 'TBV
memriOffset osize = uf (EBV osize) "arm.memri_offset" . ((:[]) . Some)


-- | A wrapper around the two low bit extractors parameterized by bit size (it
-- selects the appropriate extractor based on architecture size)
lowBits :: Int -> Expr 'TBV -> Expr 'TBV
lowBits n e
  | n == 32 = e
  | otherwise = lowBits32 n e


lowBits64 :: Int -> Expr 'TBV -> Expr 'TBV
lowBits64 n = extract 63 (63 - n + 1)

lowBits32 :: Int -> Expr 'TBV -> Expr 'TBV
lowBits32 n = extract 31 (31 - n + 1)

lowBits128 :: Int -> Expr 'TBV -> Expr 'TBV
lowBits128 n = extract 127 (127 - n + 1)


-- | Set bits, specifying the list of bit numbers to set (0-based)
bvset :: [Int] -> Expr 'TBV -> Expr 'TBV
bvset bitnums = bvor (naturalLitBV $ toInteger $ foldl setBit naturalZero bitnums)

-- | Clear bits, specifying the list of bit numbers to clear (0-based)
bvclr :: [Int] -> Expr 'TBV -> Expr 'TBV
bvclr bitnums = bvand (naturalLitBV $ toInteger $ complement $ foldl setBit naturalZero bitnums)

-- | Can be used to flag unpredictable expressions
unpredictable :: Expr a -> Expr a
unpredictable = id


sameLocation :: Location tp -> Location tp -> Expr 'TBool
sameLocation l = LitBool . maybe False (const True) . testEquality l


-- | Is the current register R15 (aka PC)?
isR15 :: Location 'TBV -> Expr 'TBool
isR15 = uf EBool "arm.is_r15" . ((:[]) . Some) . Loc
