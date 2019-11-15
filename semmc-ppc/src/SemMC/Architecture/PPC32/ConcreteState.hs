{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module SemMC.Architecture.PPC32.ConcreteState (
  zeroState,
  randomState,
  serialize,
  deserialize,
  interestingStates
  ) where

import Data.Proxy
import           GHC.TypeLits

import           Control.Monad.Trans ( liftIO )
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.Int ( Int32 )
import           Data.Parameterized.Classes ( testEquality )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Serialize.Get as G
import qualified Data.Word.Indexed as W

import           What4.BaseTypes

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture.Value as V

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.PPC.Shared as PPCS
import           SemMC.Architecture.PPC.Location

type ConcreteState ppc = MapF.MapF (Location ppc) V.Value

-- | FIXME: Does not include memory
randomState :: (KnownNat (A.RegWidth ppc), A.RegWidth ppc ~ 32) => DA.Gen -> IO (ConcreteState ppc)
randomState gen = St.execStateT randomize MapF.empty
  where
    randomize = do
      mapM_ addRandomBV gprs
      -- mapM_ addRandomBV64 frs
      -- Note: we don't use the vsrs yet so we fill them with zeros.
      mapM_ addZeroBV vsrs
      mapM_ addZeroBV specialRegs
--      St.modify' $ MapF.insert LocMem (V.ValueMem (B.replicate 64 0))

    -- | Create a random 128 bit bitvector with the high 64 bits as zero.  We
    -- want this for the FRs, which would normally overlap with the VSRs.  If we
    -- had the VSRs, then we would want to generate full 128 bit values instead.
    -- addRandomBV64 :: Location ppc (BaseBVType 128) -> St.StateT (ConcreteState ppc) IO ()
    -- addRandomBV64 loc = do
    --   bv :: V.Value (BaseBVType 64)
    --      <- V.ValueBV <$> liftIO (DA.arbitrary gen)
    --   St.modify' $ MapF.insert loc (PPCS.extendBV bv)

    addRandomBV :: (1 <= n, KnownNat n) => Location ppc (BaseBVType n) -> St.StateT (ConcreteState ppc) IO ()
    addRandomBV loc = do
      bv <- V.ValueBV <$> liftIO (DA.arbitrary gen)
      St.modify' $ MapF.insert loc bv

    addZeroBV :: (1 <= n, KnownNat n) => Location ppc (BaseBVType n) -> St.StateT (ConcreteState ppc) IO ()
    addZeroBV loc = do
      let bv = V.ValueBV (W.w 0)
      St.modify' $ MapF.insert loc bv

-- | States that include (pairs of) registers with interesting bit patterns.
-- For each pair of registers, combinations of interesting bit patterns are
-- chosen.  The other registers all have zeros.
--
-- FIXME: Doesn't include FP registers yet.  We'll want NaN and INF values there
interestingStates :: (KnownNat (A.RegWidth ppc), A.RegWidth ppc ~ 32) => [ConcreteState ppc]
interestingStates = gprStates -- ++ fprStates
  where
    i32Min :: Int32
    i32Min = minBound
    i32Max :: Int32
    i32Max = maxBound
    bvVals = [ V.ValueBV (W.w 0)
             , V.ValueBV (W.w 1)
             , V.ValueBV (W.w (fromIntegral i32Min))
             , V.ValueBV (W.w (fromIntegral i32Max))
             ]
    gprStates = [ mkState r1 v1 r2 v2
                | r1 <- gprs
                , r2 <- gprs
                , Nothing == testEquality r1 r2
                , v1 <- bvVals
                , v2 <- bvVals
                ]
    mkState r1 v1 r2 v2 =
      MapF.insert r1 v1 $ MapF.insert r2 v2 zeroState

-- | FIXME: Does not include memory
zeroState :: (KnownNat (A.RegWidth ppc), A.RegWidth ppc ~ 32) => ConcreteState ppc
zeroState = St.execState addZeros MapF.empty
  where
    addZero :: (1 <= n, KnownNat n) => Location ppc (BaseBVType n) -> St.State (ConcreteState ppc) ()
    addZero loc = St.modify' $ MapF.insert loc (V.ValueBV (W.w 0))
    addZeros = do
      mapM_ addZero gprs
      mapM_ addZero vsrs
      mapM_ addZero specialRegs
--      St.modify' $ MapF.insert LocMem (V.ValueMem (B.replicate 64 0))

-- | Convert a machine state to the wire protocol.
--
-- Note that we perform a byte swap to put data in big endian so that the
-- machine on the receiving end doesn't need to do anything special besides map
-- the data.
serialize :: (KnownNat (A.RegWidth ppc), A.RegWidth ppc ~ 32)
          => ConcreteState ppc
          -> B.ByteString
serialize s = LB.toStrict (B.toLazyByteString b)
  where
    b = mconcat [ mconcat (map (PPCS.serializeSymVal (B.word32BE . fromInteger)) (extractLocs s gprs))
                , mconcat (map (PPCS.serializeSymVal (B.word32BE . fromInteger)) (extractLocs s specialRegs))
                , mconcat (map (PPCS.serializeSymVal PPCS.serializeVec) (extractLocs s vsrs))
--                , mconcat (map serializeMem (extractLocs s [LocMem]))
                ]

-- serializeMem :: V.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)) -> B.Builder
-- serializeMem val =
--   case val of
--     V.ValueMem bs -> B.byteString bs

extractLocs :: ConcreteState ppc
            -> [Location ppc tp]
            -> [V.Value tp]
extractLocs s locs = map extractLoc locs
  where
    extractLoc l =
      let Just v = MapF.lookup l s
      in v

deserialize :: ( ArchRepr ppc
               , KnownNat (A.RegWidth ppc)
               , A.RegWidth ppc ~ 32
               ) => B.ByteString -> Maybe (ConcreteState ppc)
deserialize bs =
  case G.runGet getArchState bs of
    Left _ -> Nothing
    Right s -> Just s

getArchState :: forall ppc . ( ArchRepr ppc
                             , KnownNat (A.RegWidth ppc)
                             , A.RegWidth ppc ~ 32
                             ) => G.Get (ConcreteState ppc)
getArchState = do
  gprs' <- mapM (getWith (PPCS.getValue G.getWord32be (regWidthRepr (Proxy @ppc)))) gprs
  spregs' <- mapM (getWith (PPCS.getValue G.getWord32be PPCS.repr32)) specialRegs
--  frs' <- mapM (getWith (PPCS.getValue (PPCS.getWord128be PPCS.IgnoreHighBits) PPCS.repr128)) frs
  vsrs' <- mapM (getWith (PPCS.getValue (PPCS.getWord128be PPCS.KeepHighBits) PPCS.repr128)) vsrs
--  mem' <- getBS
  return (St.execState (addLocs gprs' spregs' ({-frs' ++-} vsrs') {- >> addLoc (LocMem, mem') -}) MapF.empty)
  where
    addLoc :: forall tp . (Location ppc tp, V.Value tp) -> St.State (ConcreteState ppc) ()
    addLoc (loc, v) = St.modify' $ MapF.insert loc v

    addLocs gprs' spregs' vsrs' = do
      mapM_ addLoc gprs'
      mapM_ addLoc spregs'
      mapM_ addLoc vsrs'

getWith :: G.Get (V.Value tp)
        -> Location ppc tp
        -> G.Get (Location ppc tp, V.Value tp)
getWith g loc = do
  w <- g
  return (loc, w)

-- getBS :: G.Get (V.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)))
-- getBS = V.ValueMem <$> G.getBytes 64

gprs :: [Location ppc (BaseBVType (A.RegWidth ppc))]
gprs = fmap (LocGPR . PPC.GPR) [0..31]

vsrs :: [Location ppc (BaseBVType 128)]
vsrs = fmap (LocVSR . PPC.VSReg) [0..63]

-- These are all of the special registers that we support; they are all 32 bits
-- in 32 bit mode.
--
-- The order must match the order expected by the remote runner.
--
-- We don't send an MSR because randomly generating that might do odd things
-- (like switch endianness).
specialRegs :: (A.RegWidth ppc ~ 32) => [Location ppc (BaseBVType 32)]
specialRegs = [ LocCTR
              , LocLNK
              , LocCR
              , LocFPSCR
              , LocVSCR
              , LocXER
              ]
