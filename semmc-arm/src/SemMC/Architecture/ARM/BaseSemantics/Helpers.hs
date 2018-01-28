{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Helpers
    where

import Data.Bits
import Data.Parameterized.Classes
import Data.Parameterized.Some ( Some(..) )
import Data.Maybe
import Prelude hiding ( pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


-- | A wrapper around 'defineOpcode' that sets A32 mode and updates
-- the PC after the instruction executes (by 4 if A32 or 2 if T32).
-- Do not use for branches or any operation that might update R15/PC.
defineA32Opcode :: String -> SemARM 'Def () -> SemARM 'Top ()
defineA32Opcode name def =
  defineOpcode name $ do
    subarch InstrSet_A32
    def
    updatePC


-- | A wrapper around 'defineOpcode' that sets A32 mode and updates
-- the PC after the instruction executes (by 4 if A32 or 2 if T32).
-- Do not use for branches or any operation that might update R15/PC.
defineT32Opcode :: String -> SemARM 'Def () -> SemARM 'Top ()
defineT32Opcode name def =
  defineOpcode name $ do
    subarch InstrSet_T32
    def
    updatePC


-- ----------------------------------------------------------------------

-- | As described for the 'SemM_ARMData' datatype, the DSL state is
-- used to store the current instruction set type (normally A32 or
-- T32).
subarch :: ArchSubtype -> SemARM t ()
subarch sa =
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ SemM_ARMData { subArch = sa }
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

-- | A32 instructions encode a predicate value as their high bits, and
-- this value is compared to the corresponding condition codes in the
-- CPSR register to determine if the execution can be performed.  This
-- check is called 'ConditionPassed' in the ARM documentation, and is
-- reproduced here by this DSL operation.
condPassed :: SemMD 'Def d (Expr 'TBool)
condPassed = do
    predV <- param "cond" pred (EBV 4)
    -- Assumed: input cpsr
    let canExec = conditionPassed [ Some (Loc predV)
                                  , Some (extract 31 28 (Loc cpsr))]
    return canExec


{-
-- conditionHolds :: BV 4 -> Bool
ConditionHolds cond =
    let r = case (extract cond "3:1") of
              0 -> PSTATE.Z == 1
              1 -> PSTATE.C == 1
              2 -> PSTATE.N == 1
              3 -> PSTATE.V == 1
              4 -> PSTATE.C == 1 && PSTATE.Z == 0
              5 -> PSTATE.N == PSTATE.V
              6 -> PSTATE.N == PSTATE.V && PSTATE.Z == 0
              7 -> True
        i = extract cond 0
    in if i == 1 and cond /= 0b1111 then not r else r
-}


-- | Performs an assignment for a conditional Opcode to the target
-- 'loc'.  Used in conjunction with the condPassed result to either
-- store 'expr' if the condPassed result was true (via 'isOK'), or not
-- perform the operation if the condPassed did not pass.
--
-- Because symbolic execution only has an ife (i.e. no ifthen), the
-- else case must have a value, which is the co-operation to the
-- store, and is passed as 'nodefExpr'.
defLocWhen :: Expr 'TBool -> Location a -> Expr a -> Expr a -> SemMD 'Def d ()
defLocWhen isOK loc expr nodefExpr = defLoc loc (ite isOK expr nodefExpr)

-- | Performs an assignment for a conditional Opcode when the target
-- is a register location.
defRegWhen :: Expr 'TBool -> Location a -> Expr a -> SemMD 'Def d ()
defRegWhen isOK loc expr = defLocWhen isOK loc expr (Loc loc)


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

-- ----------------------------------------------------------------------

-- | Each instruction contains a predicate field that is compared to
-- the CPSR to see if that instruction can be executed  (F2.3.1, F2-2417).
conditionPassed :: [Some Expr] -- ^ [CPSR, Pred operand]
                -> Expr 'TBool  -- ^ True if this instruction can be executed
conditionPassed = uf EBool "arm.conditionPassed"


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
