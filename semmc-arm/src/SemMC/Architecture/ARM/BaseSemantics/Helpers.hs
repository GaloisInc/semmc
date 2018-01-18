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


-- | This is a function over locations instead of expressions because we need to
-- be able to call 'defLoc' on the result.
--
-- Note that we really need to accommodate this in the formula parser.
memriReg :: Location 'TMemRef -> Location 'TBV
memriReg = locUF naturalBV "arm.memri_reg"

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
