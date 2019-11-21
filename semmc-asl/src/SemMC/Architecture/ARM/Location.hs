{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module SemMC.Architecture.ARM.Location
    ( Location(..)
    , PSTATE(..)
    , ArchRegWidth
    , ArchRepr(..)
    , A32
    , T32
    )
    where

import           Data.Parameterized.Classes
import           Data.Parameterized.Ctx
import           Data.Parameterized.Context ( pattern (:>), pattern Empty,  pattern EmptyCtx, pattern (::>) )
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Data.Semigroup
import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.T32 as T32
import           GHC.TypeLits
import qualified Data.Word.Indexed as W
import           What4.BaseTypes
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Architecture as A
import qualified What4.BaseTypes as WT
import           GHC.TypeNats as TypeNats

import           Prelude

data A32
data T32

type family ArchRegWidth arch :: Nat

class ArchRepr arch where
  regWidthRepr :: proxy arch -> NatRepr (ArchRegWidth arch)

-- ----------------------------------------------------------------------

-- note: R13 = SP, R14 = LR, and R15 = usually PC
--
-- SP and LR are just referenced as R13 and R14 respectively so that
-- their values do not have to be synchronously maintained, but since
-- R15 is sometimes not the PC value, it is separately managed.

data Location arm :: BaseType -> * where
  LocGPR :: W.W 4 -> Location arm (BaseBVType (ArchRegWidth arm))
  LocPC :: Location arm (BaseBVType (ArchRegWidth arm))
  LocPSTATE :: PSTATE n -> Location arm (BaseBVType n)
  LocMem :: Location arm (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8))


data PSTATE (n :: TypeNats.Nat) where
  PSTATE_N :: PSTATE 1
  PSTATE_Z :: PSTATE 1
  PSTATE_C :: PSTATE 1
  PSTATE_V :: PSTATE 1
  PSTATE_D :: PSTATE 1
  PSTATE_A :: PSTATE 1
  PSTATE_I :: PSTATE 1
  PSTATE_F :: PSTATE 1
  PSTATE_PAN :: PSTATE 1
  PSTATE_UAO :: PSTATE 1
  PSTATE_DIT :: PSTATE 1
  PSTATE_TCO :: PSTATE 1
  PSTATE_BTYPE :: PSTATE 2
  PSTATE_SS :: PSTATE 1
  PSTATE_IL :: PSTATE 1
  PSTATE_EL :: PSTATE 2
  PSTATE_nRW :: PSTATE 1
  PSTATE_SP :: PSTATE 1
  PSTATE_Q :: PSTATE 1
  PSTATE_GE :: PSTATE 4
  PSTATE_SSBS :: PSTATE 1
  PSTATE_IT :: PSTATE 8
  PSTATE_J :: PSTATE 1
  PSTATE_T :: PSTATE 1
  PSTATE_E :: PSTATE 1
  PSTATE_M :: PSTATE 5

deriving instance Ord (PSTATE n)
deriving instance Eq (PSTATE n)
deriving instance Show (PSTATE n)

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
  show LocMem = "Mem"
  show (LocPSTATE ps) = show ps

instance ShowF (Location arm)

type instance A.OperandType A32 "Bv1" = WT.BaseBVType 1
type instance A.OperandType A32 "Bv2" = WT.BaseBVType 2
type instance A.OperandType A32 "Bv3" = WT.BaseBVType 3
type instance A.OperandType A32 "Bv4" = WT.BaseBVType 4
type instance A.OperandType A32 "Bv5" = WT.BaseBVType 5
type instance A.OperandType A32 "Bv6" = WT.BaseBVType 6
type instance A.OperandType A32 "Bv7" = WT.BaseBVType 7
type instance A.OperandType A32 "Bv8" = WT.BaseBVType 8
type instance A.OperandType A32 "Bv9" = WT.BaseBVType 9
type instance A.OperandType A32 "Bv10" = WT.BaseBVType 10
type instance A.OperandType A32 "Bv11" = WT.BaseBVType 11
type instance A.OperandType A32 "Bv12" = WT.BaseBVType 12
type instance A.OperandType A32 "Bv13" = WT.BaseBVType 13
type instance A.OperandType A32 "Bv14" = WT.BaseBVType 14
type instance A.OperandType A32 "Bv15" = WT.BaseBVType 15
type instance A.OperandType A32 "Bv16" = WT.BaseBVType 16
type instance A.OperandType A32 "Bv17" = WT.BaseBVType 17
type instance A.OperandType A32 "Bv18" = WT.BaseBVType 18
type instance A.OperandType A32 "Bv19" = WT.BaseBVType 19
type instance A.OperandType A32 "Bv20" = WT.BaseBVType 20
type instance A.OperandType A32 "Bv21" = WT.BaseBVType 21
type instance A.OperandType A32 "Bv22" = WT.BaseBVType 22
type instance A.OperandType A32 "Bv23" = WT.BaseBVType 23
type instance A.OperandType A32 "Bv24" = WT.BaseBVType 24

type instance A.OperandType A32 "R" = WT.BaseStructType (EmptyCtx ::> WT.BaseBVType 32 ::> WT.BaseBVType 4)

type instance ArchRegWidth A32 = 32

$(return [])

instance TestEquality (PSTATE) where
  testEquality = $(structuralTypeEquality [t|PSTATE|] [])

instance TestEquality (Location arm) where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [(TypeApp (ConType [t|PSTATE|]) AnyType, [|testEquality|])
                   ])

instance OrdF (PSTATE) where
  compareF = $(structuralTypeOrd [t|PSTATE|] [])

instance OrdF (Location arm) where
  compareF = $(structuralTypeOrd [t|Location|]
               [(TypeApp (ConType [t|PSTATE|]) AnyType, [|compareF|])
               ])

type instance L.Location A32 = Location A32
type instance L.Location T32 = Location T32
