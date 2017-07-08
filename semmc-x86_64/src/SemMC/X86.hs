-- | Representations of the x86_64 architecture for semantics learning
{-# LANGUAGE DataKinds #-}
module SemMC.X86 (
  MachineState(..),
  YMM(..)
  ) where

import Control.Monad ( replicateM )
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Sized as V
import Data.Word ( Word8, Word64 )

import qualified SemMC.Stochastic.Remote as R

data YMM = YMM Word64 Word64 Word64 Word64
  deriving (Show)

data MachineState =
  MachineState { gprs :: V.Vector 16 Word64
               -- ^ 16 general purpose registers
               , gprs_mask :: V.Vector 16 Word64
               , eflags :: Word64
               , fprs :: V.Vector 8 Word64
               -- ^ 8 64 bit values
               , vrs :: V.Vector 16 YMM
               -- ^ 16 vector regs
               , mem1 :: V.Vector 32 Word8
               -- ^ 32 bytes
               , mem2 :: V.Vector 32 Word8
               -- ^ 32 bytes
               }
  deriving (Show)

instance R.MachineState MachineState where
  flattenMachineState ms = LB.toStrict (B.toLazyByteString bld)
    where
      bld = mconcat [ mconcat (map B.word64LE (V.toList (gprs ms)))
                    , mconcat (map B.word64LE (V.toList (gprs_mask ms)))
                    , B.word64LE (eflags ms)
                    , mconcat (map B.word64LE (V.toList (fprs ms)))
                    , mconcat (map ymmBytes (V.toList (vrs ms)))
                    , mconcat (map B.word8 (V.toList (mem1 ms)))
                    , mconcat (map B.word8 (V.toList (mem2 ms)))
                    ]
  parseMachineState bs =
    case G.pushChunk (G.runGetIncremental getMachineState) bs of
      G.Done _ _ ms -> Just ms
      G.Fail {} -> Nothing
      G.Partial {} -> Nothing

ymmBytes :: YMM -> B.Builder
ymmBytes (YMM w1 w2 w3 w4) =
  mconcat [B.word64LE w1, B.word64LE w2, B.word64LE w3, B.word64LE w4]

getMachineState :: G.Get MachineState
getMachineState = do
  Just grs <- V.fromList <$> replicateM 16 G.getWord64le
  -- Note that we have to parse out the mask, even though it isn't populated
  -- here.
  Just grs_mask <- V.fromList <$> replicateM 16 G.getWord64le
  flags <- G.getWord64le
  Just frs <- V.fromList <$> replicateM 8 G.getWord64le
  Just vs <- V.fromList <$> replicateM 16 getYMM
  Just m1 <- V.fromList <$> replicateM 32 G.getWord8
  Just m2 <- V.fromList <$> replicateM 32 G.getWord8
  return MachineState { gprs = grs
                      , gprs_mask = grs_mask
                      , eflags = flags
                      , fprs = frs
                      , vrs = vs
                      , mem1 = m1
                      , mem2 = m2
                      }

getYMM :: G.Get YMM
getYMM = YMM <$> G.getWord64le <*> G.getWord64le <*> G.getWord64le <*> G.getWord64le
