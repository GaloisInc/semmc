{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
  , ConcreteState
  , peekMS
  , pokeMS
  , Diff
  , diffInt
  , diffFloat
  , SemanticView(..)
  , ConcreteArchitecture(..)
  , parseView
  , printView
  , module Data.Parameterized.NatRepr
  , module GHC.TypeLits
  ) where

import qualified GHC.Err.Located as L

import           Control.Applicative
import           Control.Monad ( guard )
import           Data.Bits ( Bits, complement, (.&.), (.|.), shiftL, shiftR, xor, popCount )
import qualified Data.ByteString as B
import           Data.Maybe ( fromJust, isJust, fromMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr ( NatRepr, widthVal, knownNat, withKnownNat, LeqProof(..), testLeq )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Word.Indexed as W
import           Numeric.Natural ( Natural )
import           GHC.TypeLits ( KnownNat, Nat, type (+), type (<=), SomeNat(..), someNatVal )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import           Text.Printf ( printf )
import qualified Unsafe.Coerce as U

import qualified Dismantle.Arbitrary as A

import           Lang.Crucible.BaseTypes ( BaseBVType, BaseTypeRepr(..), BaseArrayType )

import           SemMC.Architecture ( Architecture, ArchState, Location, Operand, locationType )

----------------------------------------------------------------
-- Locations, values, and views

-- | Type of concrete values.
data Value tp where
  ValueBV :: (KnownNat n) => W.W n -> Value (BaseBVType n)
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
           , b <= n     -- The last bit (b) doesn't run off the end of the location (n)
           )
        => NatRepr m -> NatRepr n -> NatRepr a -> NatRepr b -> Slice m n

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

-- | Produce a view of an entire location
trivialView :: forall proxy arch n
             . (KnownNat n,
                1 <= n)
            => proxy arch
            -> Location arch (BaseBVType n)
            -> View arch n
trivialView _ loc = View s loc
  where
    s :: Slice n n
    s = Slice (knownNat @n) (knownNat @n) (knownNat @0) (knownNat @n)

someTrivialView :: (ConcreteArchitecture arch)
                => proxy arch
                -> Some (Location arch)
                -> Some (View arch)
someTrivialView proxy (Some loc) =
  case locationType loc of
    BaseBVRepr nr -> withKnownNat nr (Some (trivialView proxy loc))
    lt -> L.error ("Unsupported location type: " ++ show lt)

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

-- | A 'View' along with more information about how it should be interepreted.
data SemanticView arch =
  forall n . SemanticView { semvView :: View arch n
                          -- ^ The underlying view.
                          , semvCongruentViews :: [View arch n]
                          -- ^ The other places where we should look for our
                          -- target values in the candidate's out state.
                          --
                          -- The STOKE machine state comparison looks for the
                          -- target values not just in the target locations, but
                          -- in other locations, to allow discovering a program
                          -- that computes the right values in the wrong places
                          -- on the way to discovering a program that computes
                          -- the right values in the right places.
                          , semvDiff :: Diff n
                          -- ^ The function that should be used to calculate the
                          -- distance between two values of this semantic view.
                          }

-- | An architecture with certain operations needed for concrete work.
class (Architecture arch) => ConcreteArchitecture arch where
  -- | Convert an operand to the corresponding view, if any.
  --
  -- Useful for perturbing a machine state when computing the IO
  -- relation for an instruction?
  operandToSemanticView :: proxy arch -> Operand arch sh -> Maybe (SemanticView arch)

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

type Parser = P.Parsec String String

parseView :: forall arch . (ConcreteArchitecture arch) => Parser (Some (Location arch)) -> Parser (Some (View arch))
parseView parseLoc = do
  loc <- parseLoc
  P.try (parseSlicedView loc) <|> pure (someTrivialView (Proxy :: Proxy arch) loc)

parseSlicedView :: (ConcreteArchitecture arch) => Some (Location arch) -> Parser (Some (View arch))
parseSlicedView (Some loc) = do
  _ <- P.char '['
  ix0 <- P.decimal
  _ <- P.char ':'
  ixN <- P.decimal
  _ <- P.char ']'
  guard (ix0 >= 0)
  guard (ixN > ix0)
  -- the range of bits is [a, b) (i.e., excludes b), so when we want to write
  -- loc[0:31] to get a 32 bit slice, we actually need to represent the b as 32
  -- internally.  We add one here to make it happen
  let a = ix0
      b = ixN + 1
      m = b - a
  withUnknownNat a $ \(arepr :: NatRepr a) ->
    withUnknownNat b $ \(brepr :: NatRepr b) ->
      withUnknownNat m $ \(mrepr :: NatRepr m) ->
          case locationType loc of
            BaseBVRepr nrepr ->
              case brepr `testLeq` nrepr of
                Just LeqProof ->
                  case U.unsafeCoerce (Refl :: 0 :~: 0) :: (b :~: (a + m)) of
                    Refl -> return (withKnownNat nrepr (Some (View (Slice mrepr nrepr arepr brepr) loc)))
                Nothing -> fail "Invalid slice"
            lt -> fail ("Unsupported location type: " ++ show lt)

printView :: (ConcreteArchitecture arch) => (forall tp . Location arch tp -> String) -> View arch m -> String
printView printLocation (View (Slice _m _n a b) loc) =
  printf "%s[%d:%d]" (printLocation loc) (widthVal a) (widthVal b)

withUnknownNat :: Natural -> (forall n . (KnownNat n) => NatRepr n -> a) -> a
withUnknownNat n k =
  case someNatVal (fromIntegral n) of
    Nothing -> error "impossible"
    Just (SomeNat (Proxy :: Proxy n')) -> k (knownNat :: NatRepr n')

-- Boring instances

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

deriving instance Show (Value tp)
deriving instance Eq (Value tp)
deriving instance Ord (Value tp)

instance ShowF Value

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

instance EqF Value where
  eqF bv1 bv2 = isJust (testEquality bv1 bv2)
