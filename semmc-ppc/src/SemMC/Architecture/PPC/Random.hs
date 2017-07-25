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
import qualified Data.Parameterized.NatRepr as NR
import qualified Lang.Crucible.BaseTypes as C
import qualified Lang.Crucible.Solver.SimpleBuilder as SB
import qualified Lang.Crucible.Solver.Interface as SB

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC

import SemMC.Architecture
import qualified SemMC.Architecture.PPC as PPC
import SemMC.Stochastic.Monad ( Sym )
import qualified SemMC.Stochastic.Remote as R

-- | Generate a random PPC machine state
randomArchState :: A.Gen -> Sym t -> IO (ArchState (Sym t) PPC.PPC)
randomArchState gen backend = do
  m0 <- F.foldlM (addRandomBV gen backend repr32) MapF.empty gprs
  m1 <- F.foldlM (addRandomBV gen backend repr64) m0 fprs
  m2 <- F.foldlM (addRandomBV gen backend repr32) m1 specialRegs32
  m3 <- F.foldlM (addRandomBV gen backend repr64) m2 specialRegs64
  m4 <- F.foldlM (addRandomBV gen backend repr128) m3 vrs
  return m4

-- | Convert PPC machine states to/from the wire protocol
machineState :: R.MachineState (ArchState (Sym t) PPC.PPC)
machineState = R.MachineState { R.flattenMachineState = toBS
                              , R.parseMachineState = fromBS
                              }

-- | Convert a machine state to the wire protocol.
--
-- Note that we perform a byte swap to put data in big endian so that the
-- machine on the receiving end doesn't need to do anything special besides map
-- the data.
toBS :: ArchState (Sym t) PPC.PPC -> B.ByteString
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

serializeSymVal :: (KnownNat n) => (Integer -> B.Builder) -> SB.SymExpr (Sym t) (C.BaseBVType n) -> B.Builder
serializeSymVal toBuilder sv =
  case SB.asUnsignedBV sv of
    Nothing -> L.error ("Expected a concrete value in machine state: " ++ show sv)
    Just cv -> toBuilder cv

extractLocs :: (KnownNat n)
            => ArchState (Sym t) PPC.PPC
            -> [Location PPC.PPC (C.BaseBVType n)]
            -> [SB.SymExpr (Sym t) (C.BaseBVType n)]
extractLocs s locs = map extractLoc locs
  where
    extractLoc l =
      let Just v = MapF.lookup l s
      in v

fromBS :: B.ByteString -> Maybe (ArchState (Sym t) PPC.PPC)
fromBS bs =
  case G.runGet getArchState bs of
    Left _ -> Nothing
    Right s -> Just s

getArchState :: G.Get (ArchState (Sym t) PPC.PPC)
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
            -> Sym t
            -> NR.NatRepr n
            -> ArchState (Sym t) PPC.PPC
            -> Location PPC.PPC (C.BaseBVType n)
            -> IO (ArchState (Sym t) PPC.PPC)
addRandomBV gen backend repr m loc = do
  bv <- randomBV gen backend repr
  return (MapF.insert loc bv m)

randomBV :: (1 <= n, KnownNat n) => A.Gen -> Sym t -> NR.NatRepr n -> IO (SB.Elt t (C.BaseBVType n))
randomBV gen backend repr = do
  randomInt :: Word64
            <- A.uniform gen
  SB.bvLit backend repr (fromIntegral randomInt)

repr32 :: NR.NatRepr 32
repr32 = NR.knownNat :: NR.NatRepr 32

repr64 :: NR.NatRepr 64
repr64 = NR.knownNat :: NR.NatRepr 64

repr128 :: NR.NatRepr 128
repr128 = NR.knownNat :: NR.NatRepr 128

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
