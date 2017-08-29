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
  , LiteralRef(..)
  , RegisterizedInstruction(..)
  , SemanticView(..)
  , ConcreteArchitecture(..)
  , parseView
  , printView
  , module GHC.TypeLits
  ) where

import qualified GHC.Err.Located as L

import           Control.Applicative
import           Control.Monad ( guard )
import           Data.Bits ( Bits, complement, (.&.), (.|.), shiftL, shiftR, xor, popCount )
import qualified Data.ByteString as B
import           Data.Maybe ( fromJust, isJust, fromMaybe )
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.ShapedList as SL
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

import qualified Dismantle.Arbitrary as DA

import           Lang.Crucible.BaseTypes ( BaseBVType, BaseTypeRepr(..), BaseArrayType )

import qualified SemMC.Architecture as A

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
  View :: Slice m n -> A.Location arch (BaseBVType n) -> View arch m

viewTypeRepr :: View arch n -> NR.NatRepr n
viewTypeRepr (View (Slice repr _ _ _) _) = repr

-- | Produce a view of an entire location
trivialView :: forall proxy arch n
             . (KnownNat n,
                1 <= n)
            => proxy arch
            -> A.Location arch (BaseBVType n)
            -> View arch n
trivialView _ loc = View s loc
  where
    s :: Slice n n
    s = Slice (NR.knownNat @n) (NR.knownNat @n) (NR.knownNat @0) (NR.knownNat @n)

someTrivialView :: (ConcreteArchitecture arch)
                => proxy arch
                -> Some (A.Location arch)
                -> Some (View arch)
someTrivialView proxy (Some loc) =
  case A.locationType loc of
    BaseBVRepr nr -> NR.withKnownNat nr (Some (trivialView proxy loc))
    lt -> L.error ("Unsupported location type: " ++ show lt)

onesMask :: (Integral a, Bits b, Num b) => a -> b
onesMask sz = shiftL 1 (fromIntegral sz) - 1

-- | Read sliced bits.
peekSlice :: (KnownNat m) => Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m)
peekSlice (Slice _ _ (NR.widthVal -> a) (NR.widthVal -> b)) (ValueBV (W.unW -> val)) =
  (ValueBV . W.w) ((val .&. onesMask b) `shiftR` a)

-- | Write sliced bits.
pokeSlice :: Slice m n -> Value (BaseBVType n) -> Value (BaseBVType m) -> Value (BaseBVType n)
pokeSlice (Slice _ _ (NR.widthVal -> a) (NR.widthVal -> b)) (ValueBV (W.unW -> x)) (ValueBV (W.unW -> y)) =
  let shiftedY = y `shiftL` a
      clearLower nLower val = (val `shiftR` nLower) `shiftL` nLower
      xMask = complement (clearLower a (onesMask b))
  in (ValueBV . W.w) (shiftedY .|. (x .&. xMask))

----------------------------------------------------------------
-- Machine states

-- | Concrete machine state.
--
-- Currently we have symbolic machine state in 'ConcreteState'
-- (conathan: what does this mean?).
type ConcreteState arch = A.ArchState arch Value

-- | Read machine states.
peekMS :: (P.OrdF (A.Location arch), KnownNat n) => ConcreteState arch -> View arch n -> Value (BaseBVType n)
peekMS = flip peekMS'
  where peekMS' (View sl loc) = peekSlice sl . fromJust . MapF.lookup loc

-- | Write machine states.
--
-- This function is "dumb", in that it's not concerned with
-- e.g. zeroing out the upper bits of VSX12 when writing F12.
pokeMS :: (P.OrdF (A.Location arch)) => ConcreteState arch -> View arch n -> Value (BaseBVType n) -> ConcreteState arch
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

data LiteralRef arch sh tp where
  LiteralRef :: SL.Index sh tp -> LiteralRef arch sh (A.OperandType arch tp)

instance P.TestEquality (LiteralRef arch sh) where
  testEquality (LiteralRef ix1) (LiteralRef ix2) = do
    P.Refl <- P.testEquality ix1 ix2
    return P.Refl

instance P.OrdF (LiteralRef arch sh) where
  compareF (LiteralRef ix1) (LiteralRef ix2) =
    case P.compareF ix1 ix2 of
      P.LTF -> P.LTF
      P.GTF -> P.GTF
      P.EQF -> P.EQF


-- | A wrapper around an instruction that notes part of the machine state that
-- will be used to represent immediate operands.
--
-- The extra map indicates which literals in the instruction are mapped to
-- locations in the state.  The key operation that is required to support this
-- is to be able to rewrite test programs (i.e., single instructions we are
-- trying to learn semantics for) such that their immediates have the same value
-- as the value in the indicated location.
--
-- For example, assume we have a concrete state C that is to be used for a test
-- case and a side note that the literal for our instruction I is stored in r15:
--
-- > let t = Test { testMachineState = C, testLiterals = MapF.fromList [Pair imm0 r15] }
--
-- Before sending the test, we would need to rewrite the test instruction to use
-- the immediate in r15 as its value.  This effectively lets us pretend that an
-- instruction like @ADDI r1, r2, imm@ is actually @ADDI r1, r2, r3@ where @r3@
-- happens to hold our immediate value.  The one difficulty here is that we need
-- to ensure that the value is in range for the literal in question.
--
-- The major use of this infrastructure is during formula extraction:
-- specifically, to figure out which part of the formula represents the
-- immediate of the instruction.  If we don't have an anchor to record what part
-- of the formula stands in for the immediate, we can't extract a formula since
-- we can't tell which literals in a formula might or might not correspond to
-- immediates.  If we instead pretend that immediates came from the machine
-- state, we will have a distinguished variable to pull out of the formula and
-- turn into a parameter.  That means that the 'testLiterals' map will need to
-- be an input to 'extractFormula'.
--
-- Note that, to construct the state to send to the remote host, we just need to
-- extract the 'testMachineState'.
data RegisterizedInstruction arch =
  forall sh .
  RI { riInstruction :: A.Instruction arch
     , riOpcode :: A.Opcode arch (A.Operand arch) sh
     , riOperands :: SL.ShapedList (A.Operand arch) sh
     , riLiteralLocs :: MapF.MapF (LiteralRef arch sh) (A.Location arch)
     }

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
class (A.Architecture arch) => ConcreteArchitecture arch where
  -- | Convert an operand to the corresponding view, if any.
  --
  -- Useful for perturbing a machine state when computing the IO
  -- relation for an instruction?
  operandToSemanticView :: proxy arch -> A.Operand arch sh -> Maybe (SemanticView arch)

  -- | Obtain the type of an operand (even an operand with no associated location)
  operandType :: proxy arch -> A.Operand arch s -> BaseTypeRepr (A.OperandType arch s)

  -- | Construct a complete state with all locations set to zero
  --
  -- This is a useful starting point for constructing a desired state to ensure
  -- that all locations are filled in.
  zeroState :: proxy arch -> ConcreteState arch

  -- | Update the immediate operands of the wrapped instruction (if any) based on
  -- the test environment.
  --
  -- Note that this may require constraining the range of the value assigned to
  -- the immediate, as the immediate may (probably will) have a restricted range
  -- compared to the full width of a register.  Because of this, we also return
  -- the modified 'Test' to reflect the range restriction of the chosen value.
  --
  -- This is safe, as long as we use the same modified test for both the candidate
  -- and target programs.
  registerizeInstruction :: RegisterizedInstruction arch -> ConcreteState arch -> (A.Instruction arch, ConcreteState arch)

  -- | Generate a completely random state
  --
  -- The random state has all locations filled in
  randomState :: proxy arch -> DA.Gen -> IO (ConcreteState arch)

  -- | Convert a 'ConcreteState' into a 'B.ByteString'
  serialize :: proxy arch -> ConcreteState arch -> B.ByteString

  -- | Try to convert a 'B.ByteString' into a 'ConcreteState'
  deserialize :: proxy arch -> B.ByteString -> Maybe (ConcreteState arch)

  readView :: String -> Maybe (Some (View arch))

  showView :: View arch n -> String

type Parser = P.Parsec String String

parseView :: forall arch . (ConcreteArchitecture arch) => Parser (Some (A.Location arch)) -> Parser (Some (View arch))
parseView parseLoc = do
  loc <- parseLoc
  P.try (parseSlicedView loc) <|> pure (someTrivialView (Proxy :: Proxy arch) loc)

parseSlicedView :: (ConcreteArchitecture arch) => Some (A.Location arch) -> Parser (Some (View arch))
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
          case A.locationType loc of
            BaseBVRepr nrepr ->
              case brepr `NR.testLeq` nrepr of
                Just NR.LeqProof ->
                  case U.unsafeCoerce (P.Refl :: 0 P.:~: 0) :: (b P.:~: (a + m)) of
                    P.Refl -> return (NR.withKnownNat nrepr (Some (View (Slice mrepr nrepr arepr brepr) loc)))
                Nothing -> fail "Invalid slice"
            lt -> fail ("Unsupported location type: " ++ show lt)

printView :: (ConcreteArchitecture arch) => (forall tp . A.Location arch tp -> String) -> View arch m -> String
printView printLocation (View (Slice _m _n a b) loc) =
  printf "%s[%d:%d]" (printLocation loc) (NR.widthVal a) (NR.widthVal b - 1)

withUnknownNat :: Natural -> (forall n . (KnownNat n) => NR.NatRepr n -> a) -> a
withUnknownNat n k =
  case someNatVal (fromIntegral n) of
    Nothing -> error "impossible"
    Just (SomeNat (Proxy :: Proxy n')) -> k (NR.knownNat :: NR.NatRepr n')

-- Boring instances

instance (A.Architecture arch) => P.TestEquality (View arch) where
  testEquality (View (Slice m1 n1 a1 b1) loc1) (View (Slice m2 n2 a2 b2) loc2) = do
    P.Refl <- P.testEquality loc1 loc2
    P.Refl <- P.testEquality m1 m2
    P.Refl <- P.testEquality n1 n2
    P.Refl <- P.testEquality a1 a2
    P.Refl <- P.testEquality b1 b2
    return P.Refl

instance (A.Architecture arch) => Show (View arch m) where
  show (View s loc) = "View " ++ P.showF s ++ " " ++ P.showF loc
instance (A.Architecture arch) => P.ShowF (View arch)

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

instance (A.Architecture arch) => P.OrdF (View arch) where
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

deriving instance Show (Value tp)
deriving instance Eq (Value tp)
deriving instance Ord (Value tp)

instance P.ShowF Value

instance (KnownNat n) => DA.Arbitrary (Value (BaseBVType n)) where
  arbitrary gen = ValueBV <$> DA.arbitrary gen

instance P.TestEquality Value where
  testEquality bv1 bv2 =
    case bv1 of
      ValueBV (w1 :: W.W n1) ->
        let repr1 = NR.knownNat :: NR.NatRepr n1
        in case bv2 of
          ValueBV (w2 :: W.W n2) ->
            let repr2 = NR.knownNat :: NR.NatRepr n2
            in case P.testEquality repr1 repr2 of
              Just P.Refl
                | w1 == w2 -> Just P.Refl
                | otherwise -> Nothing
              Nothing -> Nothing

instance P.EqF Value where
  eqF bv1 bv2 = isJust (P.testEquality bv1 bv2)
