{-# LANGUAGE ExistentialQuantification #-}
-- | The definition of the backend implementation that must be provided for each
-- architecture
module SemMC.Backend (
  Backend(..)
  ) where

import qualified Data.ByteString as BS
import qualified System.Random.MWC as R

import qualified Dismantle.Instruction as D

data Backend opcode operand =
  forall a .
  Backend { mkTestCase :: R.GenIO -> IO a
          , testCaseCost :: a -> a -> Double
          , serializeTestCase :: a -> BS.ByteString
          -- ^ Serialize for wire transmission
          , deserializeTestCase :: BS.ByteString -> Maybe a
          -- ^ Read a result state back
          , allOpcodes :: [D.SomeOpcode opcode operand]
          }
