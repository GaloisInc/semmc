{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.ARM.Location
    ( Location(..)
    , ArchRepr(..)
    )
    where

import           Data.Parameterized.Classes
import           Data.Parameterized.Ctx
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Data.Semigroup
import qualified Dismantle.ARM.Operands as ARMOprnds
import qualified Dismantle.Thumb.Operands as ThumbOprnds
import qualified SemMC.Architecture as A
import           SemMC.Architecture.ARM.BaseSemantics.Registers ( GPRIdent )
import           What4.BaseTypes

import           Prelude

class ArchRepr arch where
  regWidthRepr :: proxy arch -> NatRepr (A.RegWidth arch)

-- ----------------------------------------------------------------------

-- note: R13 = SP, R14 = LR, and R15 = usually PC
--
-- SP and LR are just referenced as R13 and R14 respectively so that
-- their values do not have to be synchronously maintained, but since
-- R15 is sometimes not the PC value, it is separately managed.

data Location arm :: BaseType -> * where
  LocGPR :: GPRIdent -> Location arm (BaseBVType (A.RegWidth arm))
  LocPC :: Location arm (BaseBVType (A.RegWidth arm))
  LocCPSR :: Location arm (BaseBVType (A.RegWidth arm))
  LocMem :: Location arm (BaseArrayType (SingleCtx (BaseBVType (A.RegWidth arm))) (BaseBVType 8))

instance Show (Location arm tp) where
  show (LocGPR gpr) = case gpr of
                        10 -> "sl"
                        11 -> "fp"
                        12 -> "ip"
                        13 -> "sp"
                        14 -> "lr"
                        15 -> "pc"
                        _ -> "r" <> show gpr
  show LocPC = "PC"
  show LocCPSR = "CPSR"
  show LocMem = "Mem"

instance ShowF (Location arm)

$(return [])

fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
fakeTestEq x y = if x == y then Just Refl else Nothing

instance TestEquality (Location arm) where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [ (ConType [t|ARMOprnds.GPR|], [|fakeTestEq|])
                   , (ConType [t|ThumbOprnds.GPR|], [|fakeTestEq|])
                   , (ConType [t|ThumbOprnds.LowGPR|], [|fakeTestEq|])
                   ]
                  )

fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
fakeCompareF x y = fromOrdering (compare x y)

instance OrdF (Location arm) where
  compareF = $(structuralTypeOrd [t|Location|]
               [ (ConType [t|ARMOprnds.GPR|], [|fakeCompareF|])
               , (ConType [t|ThumbOprnds.GPR|], [|fakeCompareF|])
               , (ConType [t|ThumbOprnds.LowGPR|], [|fakeCompareF|])
               ]
              )
