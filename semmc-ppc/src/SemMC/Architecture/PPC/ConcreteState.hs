{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Architecture.PPC.ConcreteState (
  zeroState,
  randomState,
  serialize,
  deserialize
  ) where

import           GHC.TypeLits ( KnownNat )

import           Control.Monad.Trans ( liftIO )
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.Bits
import           Data.Monoid ( (<>) )
import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import qualified Data.Serialize.Get as G
import qualified Data.Word.Indexed as W
import           Numeric.Natural

import           Lang.Crucible.BaseTypes

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC

import qualified SemMC.ConcreteState as CS

import           SemMC.Architecture.PPC.Location

type ConcreteState = MapF.MapF Location CS.Value

-- | FIXME: Does not include memory
randomState :: A.Gen -> IO ConcreteState
randomState gen = St.execStateT randomize MapF.empty
  where
    randomize = do
      mapM_ addRandomBV gprs
      mapM_ addZeroBV vsrs -- RandomBV vsrs
      mapM_ addZeroBV specialRegs32
      mapM_ addZeroBV specialRegs64
--      St.modify' $ MapF.insert LocMem (CS.ValueMem (B.replicate 64 0))

    addRandomBV :: (KnownNat n) => Location (BaseBVType n) -> St.StateT ConcreteState IO ()
    addRandomBV loc = do
      bv <- CS.ValueBV <$> liftIO (A.arbitrary gen)
      St.modify' $ MapF.insert loc bv

    addZeroBV :: (KnownNat n) => Location (BaseBVType n) -> St.StateT ConcreteState IO ()
    addZeroBV loc = do
      let bv = CS.ValueBV (W.W 0)
      St.modify' $ MapF.insert loc bv

-- | FIXME: Does not include memory
zeroState :: ConcreteState
zeroState = St.execState addZeros MapF.empty
  where
    addZero :: KnownNat n => Location (BaseBVType n) -> St.State ConcreteState ()
    addZero loc = St.modify' $ MapF.insert loc (CS.ValueBV (W.W 0))
    addZeros = do
      mapM_ addZero gprs
      mapM_ addZero vsrs
      mapM_ addZero specialRegs32
      mapM_ addZero specialRegs64
--      St.modify' $ MapF.insert LocMem (CS.ValueMem (B.replicate 64 0))

-- | Convert a machine state to the wire protocol.
--
-- Note that we perform a byte swap to put data in big endian so that the
-- machine on the receiving end doesn't need to do anything special besides map
-- the data.
serialize :: ConcreteState -> B.ByteString
serialize s = LB.toStrict (B.toLazyByteString b)
  where
    b = mconcat [ mconcat (map (serializeSymVal (B.word32BE . fromInteger)) (extractLocs s gprs))
                , mconcat (map (serializeSymVal (B.word32BE . fromInteger)) (extractLocs s specialRegs32))
                , mconcat (map (serializeSymVal (B.word64BE . fromInteger)) (extractLocs s specialRegs64))
                , mconcat (map (serializeSymVal serializeVec) (extractLocs s vsrs))
--                , mconcat (map serializeMem (extractLocs s [LocMem]))
                ]

serializeMem :: CS.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)) -> B.Builder
serializeMem val =
  case val of
    CS.ValueMem bs -> B.byteString bs

-- | Serialize a 128 bit value into a bytestring
serializeVec :: Integer -> B.Builder
serializeVec i = B.word64BE w1 <> B.word64BE w2
  where
    w1 = fromInteger i
    w2 = fromInteger (i `shiftR` 64)

serializeSymVal :: (KnownNat n) => (Integer -> B.Builder) -> CS.Value (BaseBVType n) -> B.Builder
serializeSymVal toBuilder sv =
  case sv of
    CS.ValueBV (W.W w) -> toBuilder (toInteger w)

extractLocs :: ConcreteState
            -> [Location tp]
            -> [CS.Value tp]
extractLocs s locs = map extractLoc locs
  where
    extractLoc l =
      let Just v = MapF.lookup l s
      in v

deserialize :: B.ByteString -> Maybe ConcreteState
deserialize bs =
  case G.runGet getArchState bs of
    Left _ -> Nothing
    Right s -> Just s

getArchState :: G.Get ConcreteState
getArchState = do
  gprs' <- mapM (getWith (getValue G.getWord32be repr32)) gprs
  spregs32' <- mapM (getWith (getValue G.getWord32be repr32)) specialRegs32
  spregs64' <- mapM (getWith (getValue G.getWord64be repr64)) specialRegs64
  vsrs' <- mapM (getWith (getValue getWord128be repr128)) vsrs
--  mem' <- getBS
  return (St.execState (addLocs gprs' spregs32' spregs64' vsrs' {- >> addLoc (LocMem, mem') -}) MapF.empty)
  where
    addLoc :: forall tp . (Location tp, CS.Value tp) -> St.State ConcreteState ()
    addLoc (loc, v) = St.modify' $ MapF.insert loc v

    addLocs gprs' spregs32' spregs64' vsrs' = do
      mapM_ addLoc gprs'
      mapM_ addLoc spregs32'
      mapM_ addLoc spregs64'
      mapM_ addLoc [ (r, CS.ValueBV (W.W 0)) | r <- vsrs ]
      -- vsrs'

getWord128be :: G.Get Natural
getWord128be = do
  w1 <- G.getWord64be
  w2 <- G.getWord64be
  return ((fromIntegral w2 `shiftL` 64) .|. fromIntegral w1)

getWith :: G.Get (CS.Value tp)
        -> Location tp
        -> G.Get (Location tp, CS.Value tp)
getWith g loc = do
  w <- g
  return (loc, w)

getValue :: (Integral w, KnownNat n)
         => G.Get w
         -> NatRepr n
         -> G.Get (CS.Value (BaseBVType n))
getValue g _ = (CS.ValueBV . W.W . fromIntegral) <$> g

getBS :: G.Get (CS.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)))
getBS = CS.ValueMem <$> G.getBytes 64

gprs :: [Location (BaseBVType 32)]
gprs = fmap (LocGPR . PPC.GPR) [0..31]

vsrs :: [Location (BaseBVType 128)]
vsrs = fmap (LocVSR . PPC.VSReg) [0..63]

specialRegs32 :: [Location (BaseBVType 32)]
specialRegs32 = [ LocCTR
                , LocLNK
                , LocXER
                , LocCR
                  -- Lets not randomly generate an MSR.  That would
                  -- be problematic (e.g., it would switch endianness)
                  --
                  -- , LocMSR
                ]

specialRegs64 :: [Location (BaseBVType 64)]
specialRegs64 = [ LocFPSCR ]

repr32 :: NatRepr 32
repr32 = knownNat

repr64 :: NatRepr 64
repr64 = knownNat

repr128 :: NatRepr 128
repr128 = knownNat
