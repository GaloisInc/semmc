{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Helpers
    where

import Prelude hiding ( pred )
import SemMC.DSL
import Data.Parameterized.Some ( Some(..) )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses


-- | A wrapper around 'defineOpcode' that updates the PC after the
-- instruction executes (by 4 if A32 or 2 if T32).  Do not use for
-- branches or any operation that might update R15/PC.
defineLinearOpcode :: String -> SemM 'Def () -> SemM 'Top ()
defineLinearOpcode name def =
  defineOpcode name $ do
    input pc
    input cpsr
    let pcAdd = ite isA32 (naturalLitBV 0x4)
                (ite isT32 (naturalLitBV 0x2)
                     (naturalLitBV 0)) -- unsupported!
        cpsr_j = testBitDynamic (LitBV 32 24) (Loc cpsr)
        cpsr_t = testBitDynamic (LitBV 32 5) (Loc cpsr)
        isT32 = andp cpsr_t (notp cpsr_j)
        isA32 = andp (notp cpsr_t) (notp cpsr_j)
        -- isJazelle = andp (notp cpsr_t) cpsr_j
        -- isT32EE = andp cpsr_t cpsr_j
    defLoc pc (bvadd (Loc pc) pcAdd)
    def


-- ----------------------------------------------------------------------

condPassed :: SemM 'Def (Expr 'TBool)
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
defLocWhen :: Expr 'TBool -> Location a -> Expr a -> Expr a -> SemM 'Def ()
defLocWhen isOK loc expr nodefExpr = defLoc loc (ite isOK expr nodefExpr)

-- | Performs an assignment for a conditional Opcode when the target
-- is a register location.
defRegWhen :: Expr 'TBool -> Location a -> Expr a -> SemM 'Def ()
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
