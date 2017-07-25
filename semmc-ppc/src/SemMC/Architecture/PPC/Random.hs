{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.Random (
  randomArchState
  ) where

import GHC.TypeLits
import qualified Data.Foldable as F
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

randomArchState :: A.Gen -> Sym t -> IO (ArchState (Sym t) PPC.PPC)
randomArchState gen backend = do
  m0 <- F.foldlM (addRandomBV gen backend repr32) MapF.empty gprs
  m1 <- F.foldlM (addRandomBV gen backend repr64) m0 fprs
  m2 <- F.foldlM (addRandomBV gen backend repr32) m1 [ PPC.LocMSR
                                                     , PPC.LocCTR
                                                     , PPC.LocLNK
                                                     , PPC.LocXER
                                                     , PPC.LocCR
                                                     ]
  m3 <- F.foldlM (addRandomBV gen backend repr64) m2 [ PPC.LocFPSCR ]
  m4 <- F.foldlM (addRandomBV gen backend repr128) m3 vrs
  return m4

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
