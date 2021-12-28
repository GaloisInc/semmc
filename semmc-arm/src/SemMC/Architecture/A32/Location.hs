{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.A32.Location
    ( Location(..)
    )
    where

import           Data.Kind ( Type )
import           Data.Parameterized.Classes
import           Data.Parameterized.Ctx
import           Data.Parameterized.TH.GADT
import           Data.Semigroup
import qualified Dismantle.ARM.Operands as ARMOprnds
import           SemMC.Architecture.ARM.BaseSemantics.Registers ( GPRIdent )
import           What4.BaseTypes

import           Prelude

-- ----------------------------------------------------------------------

-- note: R13 = SP, R14 = LR, and R15 = usually PC
--
-- SP and LR are just referenced as R13 and R14 respectively so that
-- their values do not have to be synchronously maintained, but since
-- R15 is sometimes not the PC value, it is separately managed.

data Location arm :: BaseType -> Type where
  LocGPR :: GPRIdent -> Location arm (BaseBVType 32)
  LocGPRMask :: GPRIdent -> Location arm (BaseBVType 32)
  LocFPR :: GPRIdent -> Location arm (BaseBVType 32)
  LocPC :: Location arm (BaseBVType 32)
  LocCPSR :: Location arm (BaseBVType 32)
  LocMem1 :: Location arm (BaseArrayType (SingleCtx (BaseBVType 32)) (BaseBVType 8))
  LocMem2 :: Location arm (BaseArrayType (SingleCtx (BaseBVType 32)) (BaseBVType 8))

instance Show (Location arm tp) where
  show (LocGPR gpr) = case gpr of
                        10 -> "sl"
                        11 -> "fp"
                        12 -> "ip"
                        13 -> "sp"
                        14 -> "lr"
                        15 -> "pc"
                        _ -> "R" <> show gpr
  show LocPC = "PC"
  show LocCPSR = "CPSR"
  show LocMem1 = "Mem"
  show LocMem2 = "Mem2"
  show (LocGPRMask gpr) = "Mask_" <> show (LocGPR gpr)
  show (LocFPR fpr) = "S" <> show fpr

instance ShowF (Location arm)

$(return [])

fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
fakeTestEq x y = if x == y then Just Refl else Nothing

instance TestEquality (Location arm) where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [ (ConType [t|ARMOprnds.GPR|], [|fakeTestEq|])
                   ]
                  )

fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
fakeCompareF x y = fromOrdering (compare x y)

instance OrdF (Location arm) where
  compareF = $(structuralTypeOrd [t|Location|]
               [ (ConType [t|ARMOprnds.GPR|], [|fakeCompareF|])
               ]
              )
