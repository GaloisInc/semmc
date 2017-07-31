{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module SemMC.ConcreteState
  ( Value(..)
  , liftValueBV1
  , liftValueBV2
  , View(..)
  , viewTypeRepr
  , trivialView
  , someTrivialView
  , Slice(..)
  , peekSlice
  , pokeSlice
  , ConcreteState
  , peekMS
  , pokeMS
  , Diff
  , diffInt
  , diffFloat
  , OutMask(..)
  , outMaskView
  , OutMasks
  , ConcreteArchitecture(..)
  , module Data.Parameterized.NatRepr
  , module GHC.TypeLits
  ) where

import           Data.Bits ( Bits, complement, (.&.), (.|.), shiftL, shiftR, xor, popCount )
import qualified Data.ByteString as B
import           Data.Maybe ( fromJust, fromMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr ( NatRepr, widthVal, knownNat, withKnownNat )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Word.Indexed as W
import           GHC.TypeLits ( KnownNat, Nat, type (+), type (<=) )

import qualified Dismantle.Arbitrary as A

import           Lang.Crucible.BaseTypes ( BaseBVType, BaseTypeRepr(..) )

import           SemMC.Architecture ( Architecture, ArchState, Location, Operand, locationType )

----------------------------------------------------------------
-- Locations, values, and views

-- | Type of concrete values.
data Value tp where
  ValueBV :: (KnownNat n) => W.W n -> Value (BaseBVType n)

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

instance TestEquality Value where
  testEquality bv1 bv2 =
    case bv1 of
      ValueBV (w1 :: W.W n1) ->
        let repr1 = knownNat :: NatRepr n1
        in case bv2 of
          ValueBV (w2 :: W.W n2) ->
            let repr2 = knownNat :: NatRepr n2
            in case testEquality repr1 repr2 of
              Just Refl
                | w1 == w2 -> Just Refl
                | otherwise -> Nothing
              Nothing -> Nothing

-- | A slice of a bit vector.
--
-- A
--
-- > Slice a b :: Slice m n
--
-- is a view into the @m@ contiguous bits in the range @[a,b)@ in an
-- @n@-bit bit vector. This is a little endian view, in that the bits
-- in an @n@-bit bv are numbered from @0@ to @n-1@, with the @0@th bit
-- the least significant.
--
-- For example
--
-- > Slice 1 3 :: Slice 2 5
--
-- refers to bits 1 and 2 in a 5-bit bv with little-endian bits 0, 1,
-- 2, 3, and 4. So,
--
-- > :set -XBinaryLiterals
-- > peekSlice (Slice (knownNat :: NatRepr 1) (knownNat :: NatRepr 3) :: Slice 2 5) (ValueBV (0b00010 :: W.W 4))
-- > == ValueBV (0b01 :: W.W 2)
--
-- whereas a big-endian slice would have value @0b00@ here.
data Slice (m :: Nat) (n :: Nat) where
  Slice :: ( KnownNat m
           , KnownNat n
           , (a+m) ~ b  -- The last bit (b) and length of slice (m) are consistent
           , b <= n)    -- The last bit (b) doesn't run off the end of the location (n)
        => NatRepr m -> NatRepr n -> NatRepr a -> NatRepr b -> Slice m n

instance Show (Slice m n) where
  show (Slice m n a b) = unwords [ "Slice"
                                 , show m
                                 , show n
                                 , show a
                                 , show b
                                 ]
instance ShowF (Slice m)

instance Eq (Slice m n) where
  Slice _ _ a1 b1 == Slice _ _ a2 b2 = fromMaybe False $ do
    Refl <- testEquality a1 a2
    Refl <- testEquality b1 b2
    return True

instance Ord (Slice m n) where
  compare (Slice _ _ a1 b1) (Slice _ _ a2 b2) =
    case compareF a1 a2 of
      LTF -> LT
      GTF -> GT
      EQF ->
        case compareF b1 b2 of
          LTF -> LT
          GTF -> GT
          EQF -> EQ

instance TestEquality (Slice m) where
  testEquality (Slice m1 n1 a1 b1) (Slice m2 n2 a2 b2) = do
    Refl <- testEquality m1 m2
    Refl <- testEquality n1 n2
    Refl <- testEquality a1 a2
    Refl <- testEquality b1 b2
    return Refl

-- data Slice (l :: Nat) (h :: Nat) where
--   Slice :: (KnownNat l, KnownNat h, l < h) => Slice l h

-- | A view into a location. Could be the whole location.
--
-- E.g.
--
-- > pattern F12 = View (Slice 0 64 :: Slice 64) VSX12 :: View PPC 64
--
-- Note that (non-immediate) operands are like 'View's, not
-- 'Location's.
--
-- The @m@ parameter is the size (number of bits) of the view.
--
-- The @n@ parameter is the size of the underlying location.
--
-- The @s@ parameter is the start bit of the slice.
data View arch (m :: Nat) where
  View :: Slice m n -> Location arch (BaseBVType n) -> View arch m

viewTypeRepr :: View arch n -> NatRepr n
viewTypeRepr (View (Slice repr _ _ _) _) = repr

instance (Architecture arch) => TestEquality (View arch) where
  testEquality (View (Slice m1 n1 a1 b1) loc1) (View (Slice m2 n2 a2 b2) loc2) = do
    Refl <- testEquality loc1 loc2
    Refl <- testEquality m1 m2
    Refl <- testEquality n1 n2
    Refl <- testEquality a1 a2
    Refl <- testEquality b1 b2
    return Refl

instance (Architecture arch) => Show (View arch m) where
  show (View s loc) = "View " ++ showF s ++ " " ++ showF loc
instance (Architecture arch) => ShowF (View arch)

compareSliceF :: Slice m1 n1 -> Slice m2 n2 -> OrderingF m1 m2
compareSliceF (Slice m1 n1 a1 b1) (Slice m2 n2 a2 b2) =
  case compareF m1 m2 of
    LTF -> LTF
    GTF -> GTF
    EQF -> case compareF n1 n2 of
      LTF -> LTF
      GTF -> GTF
      EQF -> case compareF a1 a2 of
        LTF -> LTF
        GTF -> GTF
        EQF -> case compareF b1 b2 of
          LTF -> LTF
          GTF -> GTF
          EQF -> EQF

instance (Architecture arch) => OrdF (View arch) where
  compareF (View s1 l1) (View s2 l2) =
    case compareF l1 l2 of
      LTF -> LTF
      GTF -> GTF
      EQF -> compareSliceF s1 s2

-- | Produce a view of an entire location
trivialView :: forall proxy arch n . (KnownNat n, 1 <= n) => proxy arch -> Location arch (BaseBVType n) -> View arch n
trivialView _ loc = View s loc
  where
    s :: Slice n n
    s = Slice (knownNat :: NatRepr n) (knownNat :: NatRepr n) (knownNat :: NatRepr 0) (knownNat :: NatRepr n)

someTrivialView :: (ConcreteArchitecture arch)
                => proxy arch
                -> Some (Location arch)
                -> Some (View arch)
someTrivialView proxy (Some loc) =
  case locationType loc of
    BaseBVRepr nr -> withKnownNat nr (Some (trivialView proxy loc))
--    lt -> L.error ("Unsupported location type: " ++ show lt)

onesMask :: (Integral a, Bits b, Num b) => a -> b
onesMask sz = shiftL 1 (fromIntegral sz) - 1

-- | Read sliced bits.
peekSlice :: (KnownNat m) => Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m)
peekSlice (Slice _ _ (widthVal -> a) (widthVal -> b)) (ValueBV (W.W (toInteger -> val))) =
  (ValueBV . W.W . fromInteger) ((val .&. onesMask b) `shiftR` a)

-- | Write sliced bits.
pokeSlice :: Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m) -> Value (BaseBVType n)
pokeSlice (Slice _ _ (widthVal -> a) (widthVal -> b)) (ValueBV (W.W (toInteger -> x))) (ValueBV (W.W (toInteger -> y))) =
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
peekMS :: (OrdF (Location arch), KnownNat n) => ConcreteState arch -> View arch n -> Value (BaseBVType n)
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

-- | Compute distance in bits between two bvs interpreted as integers.
diffInt :: Diff n
diffInt (ValueBV x) (ValueBV y) = popCount (x `xor` y)

-- | Compute distance in bits between two bvs interpreted as floats.
--
-- This will need to be a class method if different arches have
-- different float encodings.
diffFloat :: Diff n
diffFloat = undefined "diffFloat"

-- | Some state that is live out of an instruction.
data OutMask arch n = OutMask (View arch n) (Diff n)

-- | All state that is live out of an instruction.
--
-- Need to learn one of these for each instruction.
type OutMasks arch = [Some (OutMask arch)]

-- | Project out the view of an 'OutMask'
outMaskView :: OutMask arch n -> View arch n
outMaskView (OutMask v _) = v

-- | An architecture with certain operations needed for concrete work.
class (Architecture arch) => ConcreteArchitecture arch where
  -- | Convert an operand to the corresponding view, if any.
  --
  -- Useful for perturbing a machine state when computing the IO
  -- relation for an instruction?
  operandToView :: proxy arch -> Operand arch sh -> Maybe (Some (View arch))

  -- | Return the other places where we should look for our target
  -- values in the candidate's out state.
  --
  -- The STOKE machine state comparison looks for the target values
  -- not just in the target locations, but in other locations, to
  -- allow discovering a program that computes the right values in the
  -- wrong places on the way to discovering a program that computes
  -- the right values in the right places.
  congruentViews :: proxy arch -> View arch n -> [View arch n]

  -- | Construct a complete state with all locations set to zero
  --
  -- This is a useful starting point for constructing a desired state to ensure
  -- that all locations are filled in.
  zeroState :: proxy arch -> ConcreteState arch

  -- | Generate a completely random state
  --
  -- The random state has all locations filled in
  randomState :: proxy arch -> A.Gen -> IO (ConcreteState arch)

  -- | Convert a 'ConcreteState' into a 'B.ByteString'
  serialize :: proxy arch -> ConcreteState arch -> B.ByteString

  -- | Try to convert a 'B.ByteString' into a 'ConcreteState'
  deserialize :: proxy arch -> B.ByteString -> Maybe (ConcreteState arch)

  readView :: String -> Maybe (Some (View arch))

  showView :: View arch n -> String
