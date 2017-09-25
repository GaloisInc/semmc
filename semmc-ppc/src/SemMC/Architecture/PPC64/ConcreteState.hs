{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC64.ConcreteState (
  zeroState,
  randomState,
  serialize,
  deserialize,
  interestingStates,
  uninterpretedFunctions
  ) where

import           GHC.TypeLits ( KnownNat )

import           Control.Monad.Trans ( liftIO )
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.Int ( Int64 )
import           Data.Parameterized.Classes ( testEquality )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Serialize.Get as G
import qualified Data.Word.Indexed as W

import           Lang.Crucible.BaseTypes

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.PPC as PPC

import qualified SemMC.Concrete.State as CS

import qualified SemMC.Architecture.PPC.Shared as PPCS
import           SemMC.Architecture.PPC64.Location

type ConcreteState = MapF.MapF Location CS.Value

-- | FIXME: Does not include memory
randomState :: DA.Gen -> IO ConcreteState
randomState gen = St.execStateT randomize MapF.empty
  where
    randomize = do
      mapM_ addRandomBV gprs
      mapM_ addRandomBV64 frs
      mapM_ addRandomBV vrs
      mapM_ addZeroBV specialRegs32
      mapM_ addZeroBV specialRegs64
--      St.modify' $ MapF.insert LocMem (CS.ValueMem (B.replicate 64 0))

    -- | Create a random 128 bit bitvector with the high 64 bits as zero.  We
    -- want this for the FRs, which would normally overlap with the VSRs.  If we
    -- had the VSRs, then we would want to generate full 128 bit values instead.
    addRandomBV64 :: Location (BaseBVType 128) -> St.StateT ConcreteState IO ()
    addRandomBV64 loc = do
      bv :: CS.Value (BaseBVType 64)
         <- CS.ValueBV <$> liftIO (DA.arbitrary gen)
      St.modify' $ MapF.insert loc (PPCS.extendBV bv)

    addRandomBV :: (KnownNat n) => Location (BaseBVType n) -> St.StateT ConcreteState IO ()
    addRandomBV loc = do
      bv <- CS.ValueBV <$> liftIO (DA.arbitrary gen)
      St.modify' $ MapF.insert loc bv

    addZeroBV :: (KnownNat n) => Location (BaseBVType n) -> St.StateT ConcreteState IO ()
    addZeroBV loc = do
      let bv = CS.ValueBV (W.w 0)
      St.modify' $ MapF.insert loc bv

-- | States that include (pairs of) registers with interesting bit patterns.
-- For each pair of registers, combinations of interesting bit patterns are
-- chosen.  The other registers all have zeros.
--
-- FIXME: Doesn't include FP registers yet.  We'll want NaN and INF values there
interestingStates :: [ConcreteState]
interestingStates = gprStates -- ++ fprStates
  where
    i64Min :: Int64
    i64Min = minBound
    i64Max :: Int64
    i64Max = maxBound
    bvVals = [ CS.ValueBV (W.w 0)
             , CS.ValueBV (W.w 1)
             , CS.ValueBV (W.w (fromIntegral i64Min))
             , CS.ValueBV (W.w (fromIntegral i64Max))
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
zeroState :: ConcreteState
zeroState = St.execState addZeros MapF.empty
  where
    addZero :: KnownNat n => Location (BaseBVType n) -> St.State ConcreteState ()
    addZero loc = St.modify' $ MapF.insert loc (CS.ValueBV (W.w 0))
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
    b = mconcat [ mconcat (map (PPCS.serializeSymVal (B.word32BE . fromInteger)) (extractLocs s gprs))
                , mconcat (map (PPCS.serializeSymVal (B.word32BE . fromInteger)) (extractLocs s specialRegs32))
                , mconcat (map (PPCS.serializeSymVal (B.word64BE . fromInteger)) (extractLocs s specialRegs64))
                , mconcat (map (PPCS.serializeSymVal PPCS.serializeVec) (extractLocs s vsrs))
--                , mconcat (map serializeMem (extractLocs s [LocMem]))
                ]

serializeMem :: CS.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)) -> B.Builder
serializeMem val =
  case val of
    CS.ValueMem bs -> B.byteString bs

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
  gprs' <- mapM (getWith (PPCS.getValue G.getWord32be PPCS.repr64)) gprs
  spregs32' <- mapM (getWith (PPCS.getValue G.getWord32be PPCS.repr32)) specialRegs32
  spregs64' <- mapM (getWith (PPCS.getValue G.getWord64be PPCS.repr64)) specialRegs64
  frs' <- mapM (getWith (PPCS.getValue (PPCS.getWord128be PPCS.IgnoreHighBits) PPCS.repr128)) frs
  vrs' <- mapM (getWith (PPCS.getValue (PPCS.getWord128be PPCS.KeepHighBits) PPCS.repr128)) vrs
--  mem' <- getBS
  return (St.execState (addLocs gprs' spregs32' spregs64' (frs' ++ vrs') {- >> addLoc (LocMem, mem') -}) MapF.empty)
  where
    addLoc :: forall tp . (Location tp, CS.Value tp) -> St.State ConcreteState ()
    addLoc (loc, v) = St.modify' $ MapF.insert loc v

    addLocs gprs' spregs32' spregs64' vsrs' = do
      mapM_ addLoc gprs'
      mapM_ addLoc spregs32'
      mapM_ addLoc spregs64'
      mapM_ addLoc vsrs'

getWith :: G.Get (CS.Value tp)
        -> Location tp
        -> G.Get (Location tp, CS.Value tp)
getWith g loc = do
  w <- g
  return (loc, w)

getBS :: G.Get (CS.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)))
getBS = CS.ValueMem <$> G.getBytes 64

gprs :: [Location (BaseBVType 64)]
gprs = fmap (LocGPR . PPC.GPR) [0..31]

vsrs :: [Location (BaseBVType 128)]
vsrs = fmap (LocVSR . PPC.VSReg) [0..63]

frs :: [Location (BaseBVType 128)]
frs = fmap (LocVSR . PPC.VSReg) [0..31]

vrs :: [Location (BaseBVType 128)]
vrs = fmap (LocVSR . PPC.VSReg) [32..63]

specialRegs32 :: [Location (BaseBVType 32)]
specialRegs32 = [ LocFPSCR
                , LocCR
                  -- Lets not randomly generate an MSR.  That would
                  -- be problematic (e.g., it would switch endianness)
                  --
                  -- , LocMSR
                ]

specialRegs64 :: [Location (BaseBVType 64)]
specialRegs64 = [ LocCTR
                , LocLNK
                , LocXER
                ]

uninterpretedFunctions :: [(String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)]
uninterpretedFunctions =
  [ ("fp.add64",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.add32",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 32 Ctx.::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.sub64",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.sub32",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 32 Ctx.::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  ]
