{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Helpers
    where

import SemMC.DSL
import Data.Parameterized.Some ( Some(..) )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Registers


-- | A wrapper around 'defineOpcode' that updates the PC after the instruction
-- executes to the next instruction (simply by adding 4).
defineLinearOpcode :: String -> SemM 'Def () -> SemM 'Top ()
defineLinearOpcode name def =
  defineOpcode name $ do
    input pc
    defLoc pc (bvadd (Loc pc) (naturalLitBV 0x4))
    def


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
imm12Reg = locUF naturalBV "arm.imm12_reg"

-- | Returns the immediate value in the addrmode_imm12_[pre]
imm12Imm :: [Some Expr] -> Expr 'TBV
imm12Imm = uf naturalBV "arm.imm12_imm"

-- | Returns the addition flag in the addrmode_imm12_[pre]
imm12Add :: [Some Expr] -> Expr 'TBool
imm12Add = uf EBool "arm.imm12_add"


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
