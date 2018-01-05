{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- | Views onto 'Location's
module SemMC.Architecture.View (
  View(..),
  viewTypeRepr,
  trivialView,
  someTrivialView,
  SemanticView(..),
  Slice(..),
  ConcreteState,
  peekMS,
  pokeMS,
  -- * Printing and Parsing
  parseView,
  printView
  ) where

import           GHC.TypeLits
import           Control.Applicative ( (<|>) )
import           Control.Monad ( guard )
import           Data.Bits
import           Data.Maybe ( fromJust, fromMaybe )
import           Data.Monoid ((<>))
import           Data.Proxy ( Proxy(..) )
import           Numeric.Natural ( Natural )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import           Text.Printf ( printf )
import qualified Unsafe.Coerce as U
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Word.Indexed as W
import           Lang.Crucible.BaseTypes

import qualified SemMC.Architecture.Location as L
import SemMC.Architecture.Value

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
        => NR.NatRepr m -> NR.NatRepr n -> NR.NatRepr a -> NR.NatRepr b -> Slice m n

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
  View :: (1 <= n, 1 <= m, KnownNat n, KnownNat m) => Slice m n -> L.Location arch (BaseBVType n) -> View arch m

viewTypeRepr :: View arch n -> NR.NatRepr n
viewTypeRepr (View (Slice repr _ _ _) _) = repr

-- | Produce a view of an entire location
trivialView :: forall proxy arch n
             . (KnownNat n,
                1 <= n)
            => proxy arch
            -> L.Location arch (BaseBVType n)
            -> View arch n
trivialView _ loc = View s loc
  where
    s :: Slice n n
    s = Slice (NR.knownNat @n) (NR.knownNat @n) (NR.knownNat @0) (NR.knownNat @n)

someTrivialView :: (L.IsLocation (L.Location arch))
                => proxy arch
                -> Some (L.Location arch)
                -> Some (View arch)
someTrivialView proxy (Some loc) =
  case L.locationType loc of
    BaseBVRepr nr -> NR.withKnownNat nr (Some (trivialView proxy loc))
    lt -> error ("Unsupported location type: " ++ show lt)


onesMask :: (Integral a, Bits b, Num b) => a -> b
onesMask sz = shiftL 1 (fromIntegral sz) - 1

-- | Read sliced bits.
peekSlice :: (1 <= m, KnownNat m) => Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m)
peekSlice (Slice _ _ (NR.widthVal -> a) (NR.widthVal -> b)) (ValueBV (W.unW -> val)) =
  (ValueBV . W.w) ((val .&. onesMask b) `shiftR` a)

-- | Write sliced bits.
pokeSlice :: Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m) -> Value (BaseBVType n)
pokeSlice (Slice _ _ (NR.widthVal -> a) (NR.widthVal -> b)) (ValueBV (W.unW -> x)) (ValueBV (W.unW -> y)) =
  let shiftedY = y `shiftL` a
      clearLower nLower val = (val `shiftR` nLower) `shiftL` nLower
      xMask = complement (clearLower a (onesMask b))
  in (ValueBV . W.w) (shiftedY .|. (x .&. xMask))

-- | Concrete machine state.
--
-- Currently we have symbolic machine state in 'ConcreteState'
-- (conathan: what does this mean?).
type ConcreteState arch = L.ArchState arch Value

-- | Read machine states.
peekMS :: (P.OrdF (L.Location arch), 1 <= n, KnownNat n) => ConcreteState arch -> View arch n -> Value (BaseBVType n)
peekMS cs (View sl loc) = peekSlice sl $ fromJust $ MapF.lookup loc cs

-- | Write machine states.
--
-- This function is "dumb", in that it's not concerned with
-- e.g. zeroing out the upper bits of VSX12 when writing F12.
pokeMS :: (P.OrdF (L.Location arch)) => ConcreteState arch -> View arch n -> Value (BaseBVType n) -> ConcreteState arch
pokeMS m (View sl loc) newPart = MapF.insert loc new m
  where orig = fromJust (MapF.lookup loc m)
        new = pokeSlice sl orig newPart

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


type Parser = P.Parsec String String

parseView :: forall arch . (L.IsLocation (L.Location arch)) => Parser (Some (L.Location arch)) -> Parser (Some (View arch))
parseView parseLoc = do
  loc <- parseLoc
  P.try (parseSlicedView loc) <|> pure (someTrivialView (Proxy :: Proxy arch) loc)

parseSlicedView :: (L.IsLocation (L.Location arch)) => Some (L.Location arch) -> Parser (Some (View arch))
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
  withUnknownNat a $ \(arepr :: NR.NatRepr a) ->
    withUnknownNat b $ \(brepr :: NR.NatRepr b) ->
      withUnknownNat m $ \(mrepr :: NR.NatRepr m) ->
          case L.locationType loc of
            BaseBVRepr nrepr ->
              case NR.knownNat @1 `NR.testLeq` nrepr of
                Nothing -> fail $ "Invalid slice n-value " <> show nrepr
                Just NR.LeqProof ->
                  case NR.knownNat @1 `NR.testLeq` mrepr of
                    Nothing -> fail $ "Invalid slice m-value " <> show mrepr
                    Just NR.LeqProof ->
                      case brepr `NR.testLeq` nrepr of
                        Just NR.LeqProof ->
                          case U.unsafeCoerce (P.Refl :: 0 P.:~: 0) :: (b P.:~: (a + m)) of
                            P.Refl -> return (NR.withKnownNat nrepr (Some (View (Slice mrepr nrepr arepr brepr) loc)))
                        Nothing -> fail "Invalid slice"
            lt -> fail ("Unsupported location type: " ++ show lt)

printView :: (forall tp . L.Location arch tp -> String) -> View arch m -> String
printView printLocation (View (Slice _m _n a b) loc) =
  printf "%s[%d:%d]" (printLocation loc) (NR.widthVal a) (NR.widthVal b - 1)

withUnknownNat :: Natural -> (forall n . (KnownNat n) => NR.NatRepr n -> a) -> a
withUnknownNat n k =
  case someNatVal (fromIntegral n) of
    Nothing -> error "impossible"
    Just (SomeNat (Proxy :: Proxy n')) -> k (NR.knownNat :: NR.NatRepr n')


instance (P.TestEquality (L.Location arch)) => P.TestEquality (View arch) where
  testEquality (View (Slice m1 n1 a1 b1) loc1) (View (Slice m2 n2 a2 b2) loc2) = do
    P.Refl <- P.testEquality loc1 loc2
    P.Refl <- P.testEquality m1 m2
    P.Refl <- P.testEquality n1 n2
    P.Refl <- P.testEquality a1 a2
    P.Refl <- P.testEquality b1 b2
    return P.Refl

instance (P.ShowF (L.Location arch)) => Show (View arch m) where
  show (View s loc) = "View " ++ P.showF s ++ " " ++ P.showF loc
instance (P.ShowF (L.Location arch)) => P.ShowF (View arch)

compareSliceF :: Slice m1 n1 -> Slice m2 n2 -> P.OrderingF m1 m2
compareSliceF (Slice m1 n1 a1 b1) (Slice m2 n2 a2 b2) =
  case P.compareF m1 m2 of
    P.LTF -> P.LTF
    P.GTF -> P.GTF
    P.EQF -> case P.compareF n1 n2 of
      P.LTF -> P.LTF
      P.GTF -> P.GTF
      P.EQF -> case P.compareF a1 a2 of
        P.LTF -> P.LTF
        P.GTF -> P.GTF
        P.EQF -> case P.compareF b1 b2 of
          P.LTF -> P.LTF
          P.GTF -> P.GTF
          P.EQF -> P.EQF

instance (P.OrdF (L.Location arch)) => P.OrdF (View arch) where
  compareF (View s1 l1) (View s2 l2) =
    case P.compareF l1 l2 of
      P.LTF -> P.LTF
      P.GTF -> P.GTF
      P.EQF -> compareSliceF s1 s2

instance Show (Slice m n) where
  show (Slice m n a b) = unwords [ "Slice"
                                 , show m
                                 , show n
                                 , show a
                                 , show b
                                 ]
instance P.ShowF (Slice m)

instance Eq (Slice m n) where
  Slice _ _ a1 b1 == Slice _ _ a2 b2 = fromMaybe False $ do
    P.Refl <- P.testEquality a1 a2
    P.Refl <- P.testEquality b1 b2
    return True

instance Ord (Slice m n) where
  compare (Slice _ _ a1 b1) (Slice _ _ a2 b2) =
    case P.compareF a1 a2 of
      P.LTF -> LT
      P.GTF -> GT
      P.EQF ->
        case P.compareF b1 b2 of
          P.LTF -> LT
          P.GTF -> GT
          P.EQF -> EQ

instance P.TestEquality (Slice m) where
  testEquality (Slice m1 n1 a1 b1) (Slice m2 n2 a2 b2) = do
    P.Refl <- P.testEquality m1 m2
    P.Refl <- P.testEquality n1 n2
    P.Refl <- P.testEquality a1 a2
    P.Refl <- P.testEquality b1 b2
    return P.Refl
