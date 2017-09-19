{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
-- | Definitions common to PPC32 and PPC64
module SemMC.Architecture.PPC.Shared (
  -- * Type reprs
  repr32,
  repr64,
  repr128,
  extendBV,
  withTruncI16Val,
  withTruncIVal,
  withTruncWVal,
  -- * Serialization
  serializeVec,
  serializeSymVal,
  HighBits(..),
  getWord128be,
  getValue,
  -- * Parsing
  Parser,
  tryOne,
  parsePrefixedRegister
  ) where

import           GHC.TypeLits ( KnownNat )

import           Data.Bits ( shiftR, shiftL, (.|.), (.&.) )
import qualified Data.ByteString.Builder as B
import           Data.Int ( Int16 )
import           Data.Monoid ( (<>) )
import qualified Data.Serialize.Get as G
import           Data.Word ( Word16 )
import           Numeric.Natural ( Natural )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import qualified Data.Int.Indexed as I
import qualified Data.Word.Indexed as W
import           Lang.Crucible.BaseTypes ( BaseBVType, NatRepr, knownNat )

import qualified Dismantle.PPC as PPC
import qualified SemMC.Concrete.State as CS

-- Type Reprs

repr32 :: NatRepr 32
repr32 = knownNat

repr64 :: NatRepr 64
repr64 = knownNat

repr128 :: NatRepr 128
repr128 = knownNat

extendBV :: CS.Value (BaseBVType 64) -> CS.Value (BaseBVType 128)
extendBV (CS.ValueBV (W.unW -> n)) = CS.ValueBV (W.w n)

withTruncI16Val :: (KnownNat n)
                => CS.Value (BaseBVType n)
                -> Word16
                -> (Int16 -> PPC.Operand s)
                -> (CS.Value (BaseBVType n), PPC.Operand s)
withTruncI16Val (CS.ValueBV w) mask con =
  let w' = W.unW w .&. fromIntegral mask
  in (CS.ValueBV (W.w w'), con (fromIntegral w'))

withTruncIVal :: (KnownNat n)
             => CS.Value (BaseBVType n)
             -> W.W n
             -> (I.I n' -> PPC.Operand s)
             -> (CS.Value (BaseBVType n), PPC.Operand s)
withTruncIVal (CS.ValueBV w) mask con =
  let w' = w .&. mask
  in (CS.ValueBV w', con (I.I (fromIntegral (W.unW w'))))

withTruncWVal :: (KnownNat n, KnownNat n')
              => CS.Value (BaseBVType n)
              -> W.W n
              -> (W.W n' -> PPC.Operand s)
              -> (CS.Value (BaseBVType n), PPC.Operand s)
withTruncWVal (CS.ValueBV w) mask con =
  let w' = w .&. mask
  in (CS.ValueBV w', con (W.w (fromIntegral (W.unW w'))))

-- Serialization

-- | Serialize a 128 bit value into a bytestring
serializeVec :: Integer -> B.Builder
serializeVec i = B.word64BE w1 <> B.word64BE w2
  where
    w1 = fromInteger i
    w2 = fromInteger (i `shiftR` 64)

serializeSymVal :: (KnownNat n) => (Integer -> B.Builder) -> CS.Value (BaseBVType n) -> B.Builder
serializeSymVal toBuilder sv =
  case sv of
    CS.ValueBV (W.unW -> w) -> toBuilder (toInteger w)

data HighBits = IgnoreHighBits
              | KeepHighBits

getWord128be :: HighBits -> G.Get Natural
getWord128be hb = do
  w1 <- G.getWord64be
  w2 <- G.getWord64be
  case hb of
    IgnoreHighBits -> return (fromIntegral w1)
    KeepHighBits -> return ((fromIntegral w2 `shiftL` 64) .|. fromIntegral w1)

getValue :: (Integral w, KnownNat n)
         => G.Get w
         -> NatRepr n
         -> G.Get (CS.Value (BaseBVType n))
getValue g _ = (CS.ValueBV . W.w . fromIntegral) <$> g

-- Parsing

type Parser = P.Parsec String String

tryOne :: [Parser a] -> Parser a
tryOne = P.choice . map P.try

parsePrefixedRegister :: (Integral a, Show a) => (a -> b) -> Char -> Parser b
parsePrefixedRegister f c = do
  _ <- P.char c
  n <- P.decimal
  case n >= 0 && n <= 31 of
    True -> return (f n)
    False -> fail ("Register number out of range: " ++ show n)
