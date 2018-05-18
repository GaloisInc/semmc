{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module SemMC.Architecture.Value (
  Value(..),
  liftValueBV1,
  liftValueBV2,
  -- * Differences between 'Value's
  Diff,
  diffInt,
  diffFloat,
  -- * Arbitrary
  arbitraryBV
  ) where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Maybe ( isJust )
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Word.Indexed as W
import qualified Dismantle.Arbitrary as DA
import           GHC.TypeLits
import           What4.BaseTypes ( BaseBVType, BaseArrayType )


-- | Type of concrete values.
data Value tp where
  ValueBV :: (1 <= n) => W.W n -> Value (BaseBVType n)
  ValueMem :: B.ByteString -> Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8))

-- | Lift a bitvector computation to a value computation.
liftValueBV1 :: (W.W n -> W.W n)
             -> Value (BaseBVType n)
             -> Value (BaseBVType n)
liftValueBV1 op (ValueBV x) = ValueBV (op x)

-- | Lift a bitvector computation to a value computation.
--
-- E.g.
--
-- > liftValueBV2 (+) (ValueBV x) (ValueBV y) == ValueBV (x + y)
liftValueBV2 :: (W.W n -> W.W n -> W.W n)
             -> Value (BaseBVType n)
             -> Value (BaseBVType n)
             -> Value (BaseBVType n)
liftValueBV2 op (ValueBV x1) (ValueBV x2) = ValueBV (op x1 x2)


-- | Compute bit-error difference in two values.
--
-- Different depending on whether values are treated as integers
-- (regular equality) or floating point (equivalence relation that
-- equates various NaN representations).
type Diff n = Value (BaseBVType n) -> Value (BaseBVType n) -> Int

-- | Compute distance in bits between two bvs interpreted as integers.
diffInt :: Diff n
diffInt (ValueBV x) (ValueBV y) = popCount (x `xor` y)

-- | Compute distance in bits between two bvs interpreted as floats.
--
-- This will need to be a class method if different arches have
-- different float encodings.
diffFloat :: Diff n
diffFloat = error "diffFloat is not yet implemented"

deriving instance Show (Value tp)
deriving instance Eq (Value tp)
deriving instance Ord (Value tp)

instance P.ShowF Value

arbitraryBV :: (1 <= n) => DA.Gen -> NR.NatRepr n -> IO (Value (BaseBVType n))
arbitraryBV gen nr = ValueBV <$> DA.arbitraryWord gen nr

instance P.TestEquality Value where
  testEquality bv1 bv2 =
    case bv1 of
      ValueBV (w1 :: W.W n1) ->
        let repr1 = W.rep w1
        in case bv2 of
          ValueBV (w2 :: W.W n2) ->
            let repr2 = W.rep w2
            in case P.testEquality repr1 repr2 of
              Just P.Refl
                | w1 == w2 -> Just P.Refl
                | otherwise -> Nothing
              Nothing -> Nothing

instance P.EqF Value where
  eqF bv1 bv2 = isJust (P.testEquality bv1 bv2)
