{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module SemMC.ConcreteState
  ( Value(..)
  , liftValueBV1
  , liftValueBV2
  , View(..)
  , Slice(..)
  , peekSlice
  , pokeSlice
  , ConcreteState
  , peekMS
  , pokeMS
  , operandToView
  , Diff
  , OutMask(..)
  , OutMasks
  , congruentViews
  , ConcreteArchitecture
  , module Data.Parameterized.NatRepr
  , module GHC.TypeLits
  ) where

import           Data.Bits ( Bits, complement, (.&.), (.|.), shiftL, shiftR )
import           Data.Maybe ( fromJust )
import           Data.Proxy ( Proxy(..) )
import           Data.Parameterized.Classes ( OrdF )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr ( NatRepr, widthVal, knownNat )
import           Data.Parameterized.Some ( Some )
import qualified Data.Word.Indexed as W
import           GHC.TypeLits ( KnownNat, Nat, type (+), type (<=) )

import qualified Dismantle.Arbitrary as A

import           Lang.Crucible.BaseTypes ( BaseBVType )

import           SemMC.Architecture ( Architecture, ArchState, Location, Operand )

----------------------------------------------------------------
-- Locations, values, and views

-- | Type of concrete values.
data Value tp where
  ValueBV :: W.W n -> Value (BaseBVType n)

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

deriving instance (KnownNat n) => Show (Value (BaseBVType n))
deriving instance Eq (Value tp)
deriving instance Ord (Value tp)

instance (KnownNat n) => A.Arbitrary (Value (BaseBVType n)) where
  arbitrary gen = ValueBV <$> A.arbitrary gen

-- | A view into a location. Could be the whole location.
--
-- E.g.
--
-- > pattern F12 = View (Slice 0 64 :: Slice 64) VSX12 :: View PPC 64
--
-- Note that (non-immediate) operands are like 'View's, not
-- 'Location's.
data View arch (m :: Nat) where
  View :: Slice m n -> Location arch (BaseBVType n) -> View arch m

-- | A slice of a bit vector.
--
-- A
--
-- > Slice a b :: Slice m n
--
-- is a view into the @m@ contiguous bits in the range @[a,b)@ in an
-- @n@-bit bit vector. This is a little endian view, in that the bits
-- in an @n@-bit bv are numbered from @0@ to @n-1@, with the @0@th bit
-- the least significant. For example
--
-- > Slice 1 3 :: Slice 2 5
--
-- refers to bits 1 and 2 in a 5-bit bv with little-endian bits 0, 1,
-- 2, 3, and 4. So, for example
--
-- > :set -XBinaryLiterals
-- > peekSlice (Slice (knownNat :: NatRepr 1) (knownNat :: NatRepr 3) :: Slice 2 5) (ValueBV (0b00010 :: W.W 4))
-- > == ValueBV (0b01 :: W.W 2)
--
-- whereas a big-endian slice would have value @0b00@ here.
data Slice (m :: Nat) (n :: Nat) where
  Slice :: (a <= b, (a+m) ~ b, b <= n) => NatRepr a -> NatRepr b -> Slice m n

onesMask :: (Integral a, Bits b, Num b) => a -> b
onesMask sz = shiftL 1 (fromIntegral sz) - 1

-- | Read sliced bits.
peekSlice :: Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m)
peekSlice (Slice (widthVal -> a) (widthVal -> b)) (ValueBV (W.W (toInteger -> val))) =
  (ValueBV . W.W . fromInteger) ((val .&. onesMask b) `shiftR` a)

-- | Write sliced bits.
pokeSlice :: Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m) -> Value (BaseBVType n)
pokeSlice (Slice (widthVal -> a) (widthVal -> b)) (ValueBV (W.W (toInteger -> x))) (ValueBV (W.W (toInteger -> y))) =
  let shiftedY = y `shiftL` a
      clearLower nLower val = (val `shiftR` nLower) `shiftL` nLower
      xMask = complement (clearLower a (onesMask b))
  in (ValueBV . W.W . fromInteger) (shiftedY .|. (x .&. xMask))

----------------------------------------------------------------
-- Machine states

-- | Concrete machine state.
--
-- Currently we have symbolic machine state in 'ConcreteState'
-- (conathan: what does this mean?).
type ConcreteState arch = ArchState arch Value

-- | Read machine states.
peekMS :: (OrdF (Location arch)) => ConcreteState arch -> View arch n -> Value (BaseBVType n)
peekMS = flip peekMS'
  where peekMS' (View sl loc) = peekSlice sl . fromJust . MapF.lookup loc

-- | Write machine states.
--
-- This function is "dumb", in that it's not concerned with
-- e.g. zeroing out the upper bits of VSX12 when writing F12.
pokeMS :: (OrdF (Location arch)) => ConcreteState arch -> View arch n -> Value (BaseBVType n) -> ConcreteState arch
pokeMS m (View sl loc) newPart = MapF.insert loc new m
  where orig = fromJust (MapF.lookup loc m)
        new = pokeSlice sl orig newPart

----------------------------------------------------------------
-- Comparing machine states in stratified synthesis

-- | Compute bit-error difference in two values.
--
-- Different depending on whether values are treated as integers
-- (regular equality) or floating point (equivalence relation that
-- equates various NaN representations).
type Diff n = Value (BaseBVType n) -> Value (BaseBVType n) -> Int

-- | Some state that is live out of an instruction.
data OutMask arch n = OutMask (View arch n) (Diff n)

-- | All state that is live out of an instruction.
--
-- Need to learn one of these for each instruction.
type OutMasks arch = [Some (OutMask arch)]

-- | An architecture with certain operations needed for concrete work.
class (Architecture arch) => ConcreteArchitecture arch where
  -- | Convert an operand to the corresponding view, if any.
  --
  -- Useful for perturbing a machine state when computing the IO
  -- relation for an instruction?
  operandToView :: Proxy arch -> Operand arch sh -> Maybe (Some (View arch))

  -- | Return the other places where we should look for our target
  -- values in the candidate's out state.
  --
  -- The STOKE machine state comparison looks for the target values
  -- not just in the target locations, but in other locations, to
  -- allow discovering a program that computes the right values in the
  -- wrong places on the way to discovering a program that computes
  -- the right values in the right places.
  congruentViews :: Proxy arch -> View arch n -> [View arch n]
