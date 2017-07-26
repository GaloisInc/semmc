{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.Random (
  randomArchState,
  machineState
  ) where

import qualified GHC.Err.Located as L
import GHC.TypeLits

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Serialize.Get as G
import Data.Word ( Word64 )


import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.BaseTypes as C

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC

import SemMC.Architecture
import SemMC.ConcreteState ( ConcreteState, Value(ValueBV) )
import qualified SemMC.Architecture.PPC as PPC
import SemMC.Symbolic ( Sym )
import qualified SemMC.Stochastic.Remote as R


randomArchState :: A.Gen -> IO (ConcreteState PPC.PPC)
randomArchState gen = do
  m0 <- F.foldlM (addRandomBV gen) MapF.empty gprs
  m1 <- F.foldlM (addRandomBV gen) m0 fprs
  m2 <- F.foldlM (addRandomBV gen) m1 [ PPC.LocMSR
                                      , PPC.LocCTR
                                      , PPC.LocLNK
                                      , PPC.LocXER
                                      , PPC.LocCR
                                      ]
  m3 <- F.foldlM (addRandomBV gen) m2 [ PPC.LocFPSCR ]
  m4 <- F.foldlM (addRandomBV gen) m3 vrs
  return m4

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
                , mconcat (map (serializeSymVal (B.word64BE . fromInteger)) (extractLocs s fprs))
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
serializeSymVal toBuilder sv = undefined

  -- case SB.asUnsignedBV sv of
  --   Nothing -> L.error ("Expected a concrete value in machine state: " ++ show sv)
  --   Just cv -> toBuilder cv

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
 -- gprs' <- mapM (getWith G.getWord32be) gprs
  undefined

-- getWith :: (1 <= n, KnownNat n)
--         => Sym t
--         -> G.Get w
--         -> NR.NatRepr n
--         -> Location PPC.PPC (C.BaseBVType n)
--         -> G.Get (Location PPC.PPC (C.BaseBVType n), SB.SymExpr (Sym t) (C.BaseBVType))


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
vrs = fmap (PPC.LocVR . PPC.VR) [0..31]

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
