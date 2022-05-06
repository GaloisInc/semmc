{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParameterTests where

import           Data.Parameterized.Classes
import           Test.Tasty
import qualified SemMC.Formula.Formula as F
import           TestArchPropGen
import           TestUtils
import           What4.BaseTypes
import           Hedgehog

import           Prelude


parameterTests :: [TestTree]
parameterTests = [
  testGroup "Parameter" $

    [ testGroup "Integer" $
      [ testProperty "parameter type" $
        property $ do p <- forAll $ genIntParameter
                      case testEquality (F.paramType p) BaseIntegerRepr of
                        Just Refl -> success
                        Nothing -> assert False
      -- TBD: needs other tests
      ]

    , testGroup "Box (32-bit BV)" $
      [ testProperty "parameter type" $
        property $ do p <- forAll $ genBoxParameter
                      let aBV32 = BaseBVRepr knownNat :: BaseTypeRepr (BaseBVType 32)
                      case testEquality (F.paramType p) aBV32 of
                        Just Refl -> success
                        Nothing -> assert False
        -- TBD: needs other tests
      ]

    ]

  ]
