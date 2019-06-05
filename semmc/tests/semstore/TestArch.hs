{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-| Minimal definition of a test architecture which allows various
  testing to be performed.  Not a real architecture at all... not even
  a little bit.
|-}

module TestArch
where

import           Data.Kind ( Type )
import           Data.Parameterized.Classes
import           Numeric.Natural
import qualified SemMC.Architecture.Location as L
import           What4.BaseTypes


data TestGenArch

----------------------------------------------------------------------
-- Location

data TestLocation :: BaseType -> Type where
  TestNatLoc :: Natural -> TestLocation BaseNatType
  TestIntLoc :: Integer -> TestLocation BaseIntegerType
  -- TBD: more basetype locations
  -- TBD: some memory locations
  -- MemLoc :: Mem -> Location (BaseBVType 32)

deriving instance Show (TestLocation tp)
deriving instance Eq (TestLocation tp)
deriving instance Ord (TestLocation tp)

instance ShowF TestLocation where
  showF = show

instance TestEquality TestLocation where
  TestNatLoc l1 `testEquality` TestNatLoc l2 | l1 == l2 = Just Refl
                                             | otherwise = Nothing
  TestIntLoc l1 `testEquality` TestIntLoc l2 | l1 == l2 = Just Refl
                                             | otherwise = Nothing
  _ `testEquality` _ = Nothing

instance OrdF TestLocation where
  TestNatLoc l1 `compareF` TestNatLoc l2 = fromOrdering $ l1 `compare` l2
  TestIntLoc l1 `compareF` TestIntLoc l2 = fromOrdering $ l1 `compare` l2
  -- _ `compareF` _ = LTF

instance L.IsLocation TestLocation where
  locationType (TestNatLoc _) = BaseNatRepr
  locationType (TestIntLoc _) = BaseIntegerRepr

  isMemoryLocation _ = False


type instance L.Location TestGenArch = TestLocation
