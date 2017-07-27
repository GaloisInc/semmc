{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.Location (
  Location(..)
  ) where

import qualified Data.Parameterized.Ctx as Ctx
import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Data.Parameterized.Some
import           Text.PrettyPrint.HughesPJClass ( pPrint )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture as A

data Location :: BaseType -> * where
  LocGPR :: PPC.GPR -> Location (BaseBVType 32)
  LocIP :: Location (BaseBVType 32)
  LocMSR :: Location (BaseBVType 32)
  LocCTR :: Location (BaseBVType 32)
  LocLNK :: Location (BaseBVType 32)
  LocXER :: Location (BaseBVType 32)
  LocCR :: Location (BaseBVType 32)
  LocFR :: PPC.FR -> Location (BaseBVType 64)
  LocFPSCR :: Location (BaseBVType 64)
  LocVR :: PPC.VR -> Location (BaseBVType 128)
  LocMem :: Location (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8))

instance Show (Location tp) where
  show (LocGPR gpr) = show (pPrint gpr)
  show LocIP = "IP"
  show LocMSR = "MSR"
  show LocCTR = "CTR"
  show LocLNK = "LNK"
  show LocXER = "XER"
  show LocCR = "CR"
  show (LocFR fr) = show (pPrint fr)
  show LocFPSCR = "FPSCR"
  show (LocVR vr) = show (pPrint vr)
  show LocMem = "Mem"
instance ShowF Location

$(return [])

fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
fakeTestEq x y = if x == y
                 then Just Refl
                 else Nothing

instance TestEquality Location where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [ (ConType [t|PPC.GPR|], [|fakeTestEq|])
                   , (ConType [t|PPC.FR|], [|fakeTestEq|])
                   , (ConType [t|PPC.VR|], [|fakeTestEq|])
                   ]
                  )

fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
fakeCompareF x y = fromOrdering (compare x y)

instance OrdF Location where
  compareF = $(structuralTypeOrd [t|Location|]
               [ (ConType [t|PPC.GPR|], [|fakeCompareF|])
               , (ConType [t|PPC.FR|], [|fakeCompareF|])
               , (ConType [t|PPC.VR|], [|fakeCompareF|])
               ]
              )

instance A.IsLocation Location where
  readLocation s
    | s `elem` ["r" ++ show i | i <- [(0 :: Int)..31]] =
      (Just . Some . LocGPR . PPC.GPR . read . tail) s
    | s == "ip" = Just (Some LocIP)
    | s == "msr" = Just (Some LocMSR)
    | s == "ctr" = Just (Some LocCTR)
    | s == "lnk" = Just (Some LocLNK)
    | s == "xer" = Just (Some LocXER)
    | s == "cr" = Just (Some LocCR)
    | s `elem` ["f" ++ show i | i <- [(0 :: Int)..31]] =
      (Just . Some . LocFR . PPC.FR . read . tail) s
    | s == "fpscr" = Just (Some LocFPSCR)
    | s `elem` ["vr" ++ show i | i <- [(0 :: Int)..31]] =
      (Just . Some . LocVR . PPC.VR . read . tail . tail) s
    | s == "mem" = Just (Some LocMem)
    | otherwise = Nothing

  locationType (LocGPR _) = knownRepr
  locationType LocIP = knownRepr
  locationType LocMSR = knownRepr
  locationType LocCTR = knownRepr
  locationType LocLNK = knownRepr
  locationType LocXER = knownRepr
  locationType LocCR = knownRepr
  locationType (LocFR _) = knownRepr
  locationType LocFPSCR = knownRepr
  locationType (LocVR _) = knownRepr
  locationType LocMem = knownRepr

  defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocIP = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMSR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCTR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocLNK = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocXER = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocFR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocFPSCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocVR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMem =
    S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0
