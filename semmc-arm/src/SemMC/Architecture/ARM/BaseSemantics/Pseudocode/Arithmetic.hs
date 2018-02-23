-- | Pseudocode definitions of arithmetic operations from H5.4
-- (page AppxH-5072) of the ARMv8 Architecture Reference Manual.
--
-- Much of what is here are trivial abstractions over the pseudocode,
-- but can be affected by the underlying representational language
-- within Macaw... for example, pMOD is defined in terms of bvurem
-- under the assumption that Macaw has support for division.  Many of
-- these trivial abstractions are automatically eliminated during
-- evaluation, but their use to match the documented semantics can
-- help if any core represntations or assumptions change later.

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Arithmetic
    ( roundDown, roundUp, roundTowardsZero
    , align
    , pDIV, pMOD
    , (.<<.), (.>>.)
    , pMax, pMin
    )
    where

import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.DSL


-- | Rounding down to the largest integer n less than x
roundDown :: Expr 'TBV -> Expr 'TBV
roundDown x = x

-- | Rounding up to the smallest integer n greater than x
roundUp :: Expr 'TBV -> Expr 'TBV
roundUp x = x

-- | Rounding towards zero
roundTowardsZero :: Expr 'TBV -> Expr 'TBV
roundTowardsZero x = ite (bvuge x (naturalLitBV 0)) (roundDown x) (roundUp x)

-- | Alignment
--
-- > Align(x, y) = y * (x DIV y)
--
-- Align is only used with y as a constant power of 2
align :: Expr 'TBV -> Int -> Expr 'TBV
align x y = let bits = [n | n <- [0..naturalBitSize], 2^n < y ]
            in bvclr bits x

-- | DIV divides the first argument by the second
pDIV :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
pDIV _x _y = undefined

-- | Pseudocode MOD (H5.4, AppxH-5073) for integers, the remainder of
-- dividing the first arg by the second arg.
--
-- > x MOD y = x - y * (x DIV y)
-- > x DIV y = RoundDown(x/y)
--
pMOD :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
pMOD x y = bvurem x y

-- | Scaling up
(.<<.) :: Expr 'TBV -> Integer -> Expr 'TBV
x .<<. n = bvshl x (LitBV 8 n)
infix 6 .<<.

-- | Scaling down
--
-- > x >> n = RoundDown(x * 2^(-n))
(.>>.) :: Expr 'TBV -> Integer -> Expr 'TBV
x .>>. n = bvashr x (LitBV 8 n)
infix 6 .>>.


-- | Maximum
pMax :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
pMax x y = ite (bvsge x y) x y


-- | Minimum
pMin :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
pMin x y = ite (bvsle x y) x y
