{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.Random (
  randomArchState
  ) where

import GHC.TypeLits
import qualified Data.Foldable as F

import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.BaseTypes as C

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC

import SemMC.Architecture
import SemMC.ConcreteState ( ConcreteState, Value(ValueBV) )
import qualified SemMC.Architecture.PPC as PPC

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
