{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.ARM.Location
    ( Location(..)
    , ArchRegWidth
    , ArchRepr(..)
    )
    where

import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import qualified Dismantle.ARM as ARM
import qualified Dismantle.ARM.Operands as ARMOprnds
import           GHC.TypeLits
import           Lang.Crucible.BaseTypes
import           Text.PrettyPrint.HughesPJClass ( pPrint )


type family ArchRegWidth arch :: Nat

class ArchRepr arch where
  regWidthRepr :: proxy arch -> NatRepr (ArchRegWidth arch)

-- ----------------------------------------------------------------------

data Location arm :: BaseType -> * where
  -- LocGPR :: ARM.GPR x -> Location arm (BaseBVType (ArchRegWidth arm))
  LocGPR :: ARMOprnds.GPR -> Location arm (BaseBVType (ArchRegWidth arm))
  LocIP :: Location arm (BaseBVType (ArchRegWidth arm))

instance Show (Location arm tp) where
  show (LocGPR gpr) = show (pPrint gpr)
  show LocIP = "IP"

instance ShowF (Location arm)

$(return [])

fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
fakeTestEq x y = if x == y then Just Refl else Nothing

instance TestEquality (Location arm) where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [ (ConType [t|ARMOprnds.GPR|], [|fakeTestEq|])
                     -- , (ConType [t|ARM.GPR|], [|fakeTestEq|])
                   ]
                  )

fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
fakeCompareF x y = fromOrdering (compare x y)

instance OrdF (Location arm) where
  compareF = $(structuralTypeOrd [t|Location|]
               [ (ConType [t|ARMOprnds.GPR|], [|fakeCompareF|])
                 -- , (ConType [t|ARM.GPR|], [|fakeCompareF|])
               ]
              )
