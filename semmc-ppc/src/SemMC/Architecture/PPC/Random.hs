{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.Random (
  randomArchState,
  machineState
  ) where

import GHC.TypeLits

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Serialize.Get as G
import Numeric.Natural

import qualified Data.Parameterized.Map as MapF
import qualified Data.Word.Indexed as W
import qualified Lang.Crucible.BaseTypes as C

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC

import SemMC.Architecture
import SemMC.ConcreteState ( ConcreteState, Value(ValueBV) )
import qualified SemMC.Architecture.PPC as PPC
import qualified SemMC.Stochastic.Remote as R


randomArchState :: A.Gen -> IO (ConcreteState PPC.PPC)
randomArchState gen = do
  m0 <- F.foldlM (addRandomBV gen) MapF.empty gprs
  m1 <- F.foldlM (addRandomBV gen) m0 [ PPC.LocMSR
                                      , PPC.LocCTR
                                      , PPC.LocLNK
                                      , PPC.LocXER
                                      , PPC.LocCR
                                      ]
  m2 <- F.foldlM (addRandomBV gen) m1 [ PPC.LocFPSCR ]
  m3 <- F.foldlM (addRandomBV gen) m2 vrs
  return m3

-- | Convert PPC machine states to/from the wire protocol
machineState :: R.MachineState (ConcreteState PPC.PPC)
machineState = R.MachineState { R.flattenMachineState = toBS
                              , R.parseMachineState = fromBS
                              }

-- | Convert a machine state to the wire protocol.
--
-- Note that we perform a byte swap to put data in big endian so that the
-- machine on the receiving end doesn't need to do anything special besides map
-- the data.
toBS :: ConcreteState PPC.PPC -> B.ByteString
toBS s = LB.toStrict (B.toLazyByteString b)
  where
    b = mconcat [ mconcat (map (serializeSymVal (B.word32BE . fromInteger)) (extractLocs s gprs))
                , mconcat (map (serializeSymVal (B.word32BE . fromInteger)) (extractLocs s specialRegs32))
                , mconcat (map (serializeSymVal (B.word64BE . fromInteger)) (extractLocs s specialRegs64))
                , mconcat (map (serializeSymVal serializeVec) (extractLocs s vrs))
                ]

-- | Serialize a 128 bit value into a bytestring
serializeVec :: Integer -> B.Builder
serializeVec i = B.word64BE w1 <> B.word64BE w2
  where
    w1 = fromInteger i
    w2 = fromInteger (i `shiftR` 64)

serializeSymVal :: (KnownNat n) => (Integer -> B.Builder) -> Value (C.BaseBVType n) -> B.Builder
serializeSymVal toBuilder sv =
  case sv of
    ValueBV (W.W w) -> toBuilder (toInteger w)

extractLocs :: ConcreteState PPC.PPC
            -> [PPC.Location tp]
            -> [Value tp]
extractLocs s locs = map extractLoc locs
  where
    extractLoc l =
      let Just v = MapF.lookup l s
      in v

fromBS :: B.ByteString -> Maybe (ConcreteState PPC.PPC)
fromBS bs =
  case G.runGet getArchState bs of
    Left _ -> Nothing
    Right s -> Just s

getArchState :: G.Get (ConcreteState PPC.PPC)
getArchState = do
  gprs' <- mapM (getWith (getValue G.getWord32be repr32)) gprs
  spregs32' <- mapM (getWith (getValue G.getWord32be repr32)) specialRegs32
  spregs64' <- mapM (getWith (getValue G.getWord64be repr64)) specialRegs64
  vrs' <- mapM (getWith (getValue getWord128be repr128)) vrs
  let m1 = F.foldl' addLoc MapF.empty gprs'
      m2 = F.foldl' addLoc m1 spregs32'
      m3 = F.foldl' addLoc m2 spregs64'
      m4 = F.foldl' addLoc m3 vrs'
  return m4
  where
    addLoc :: forall tp . ConcreteState PPC.PPC -> (PPC.Location tp, Value tp) -> ConcreteState PPC.PPC
    addLoc m (loc, v) = MapF.insert loc v m

getWord128be :: G.Get Natural
getWord128be = do
  w1 <- G.getWord64be
  w2 <- G.getWord64be
  return ((fromIntegral w2 `shiftL` 64) .|. fromIntegral w1)

getWith :: G.Get (Value tp)
        -> PPC.Location tp
        -> G.Get (PPC.Location tp, Value tp)
getWith g loc = do
  w <- g
  return (loc, w)

getValue :: (Integral w, KnownNat n)
         => G.Get w
         -> C.NatRepr n
         -> G.Get (Value (C.BaseBVType n))
getValue g _ = (ValueBV . W.W . fromIntegral) <$> g

addRandomBV :: (KnownNat n, 1 <= n)
            => A.Gen
            -> ConcreteState PPC.PPC
            -> Location PPC.PPC (C.BaseBVType n)
            -> IO (ConcreteState PPC.PPC)
addRandomBV gen m loc = do
  bv <- ValueBV <$> A.arbitrary gen
  return (MapF.insert loc bv m)

gprs :: [Location PPC.PPC (C.BaseBVType 32)]
gprs = fmap (PPC.LocGPR . PPC.GPR) [0..31]

fprs :: [Location PPC.PPC (C.BaseBVType 64)]
fprs = fmap (PPC.LocFR . PPC.FR) [0..31]

vrs :: [Location PPC.PPC (C.BaseBVType 128)]
vrs = fmap (PPC.LocVR . PPC.VR) [0..63]

specialRegs32 :: [Location PPC.PPC (C.BaseBVType 32)]
specialRegs32 = [ PPC.LocCTR
                , PPC.LocLNK
                , PPC.LocXER
                , PPC.LocCR
                  -- Lets not randomly generate an MSR.  That would
                  -- be problematic (e.g., it would switch endianness)
                  --
                  -- , PPC.LocMSR
                ]

specialRegs64 :: [Location PPC.PPC (C.BaseBVType 64)]
specialRegs64 = [ PPC.LocFPSCR ]

repr32 :: C.NatRepr 32
repr32 = C.knownNat

repr64 :: C.NatRepr 64
repr64 = C.knownNat

repr128 :: C.NatRepr 128
repr128 = C.knownNat

{- Note [Aliasing]

There are 64 VSRs (vector-scalar registers).  The lower 64 bits of the first 32
VSRs alias the floating point registers, so we don't add explicit
representations of the FPRs.  Manipulations of the FPR state will go through
views.

-}
