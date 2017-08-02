-- | Representations of the ARM architecture for semantics learning
{-# LANGUAGE DataKinds #-}
module SemMC.ARM (
  MachineState(..),
  machineState
  ) where

import Control.Monad ( replicateM )
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Sized as V
import Data.Word ( Word8, Word32, Word64 )

import qualified SemMC.Stochastic.Remote as R

data MachineState =
  MachineState { gprs :: V.Vector 16 Word32
               -- ^ 16 general purpose registers
               , gprs_mask :: V.Vector 16 Word32
               , fprs :: V.Vector 32 Word32
               -- ^ 32 32-bit locations
               , mem1 :: V.Vector 32 Word8
               -- ^ 32 bytes
               , mem2 :: V.Vector 32 Word8
               -- ^ 32 bytes
               }
  deriving (Show)

machineState :: R.MachineState MachineState
machineState = R.MachineState { R.flattenMachineState = toBS
                              , R.parseMachineState = fromBS
                              }

toBS :: MachineState -> B.ByteString
toBS ms = LB.toStrict (B.toLazyByteString bld)
  where
    bld = mconcat [ mconcat (map B.word32LE (V.toList (gprs ms)))
                  , mconcat (map B.word32LE (V.toList (gprs_mask ms)))
                  , mconcat (map B.word32LE (V.toList (fprs ms)))
                  , mconcat (map B.word8 (V.toList (mem1 ms)))
                  , mconcat (map B.word8 (V.toList (mem2 ms)))
                  ]

fromBS :: B.ByteString -> Maybe MachineState
fromBS bs =
  case G.pushChunk (G.runGetIncremental getMachineState) bs of
    G.Done _ _ ms -> Just ms
    G.Fail {} -> Nothing
    G.Partial {} -> Nothing

getMachineState :: G.Get MachineState
getMachineState = do
  Just grs <- V.fromList <$> replicateM 16 G.getWord32le
  -- Note that we have to parse out the mask, even though it isn't populated
  -- here.
  Just grs_mask <- V.fromList <$> replicateM 16 G.getWord32le
  Just frs <- V.fromList <$> replicateM 32 G.getWord32le
  Just m1 <- V.fromList <$> replicateM 32 G.getWord8
  Just m2 <- V.fromList <$> replicateM 32 G.getWord8
  return MachineState { gprs = grs
                      , gprs_mask = grs_mask
                      , fprs = frs
                      , mem1 = m1
                      , mem2 = m2
                      }
