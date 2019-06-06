{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParameterTests where

import           Data.Parameterized.Classes
import           Test.Tasty
import           Test.Tasty.Hedgehog
import qualified SemMC.Formula.Formula as F
import           TestArchPropGen
import           What4.BaseTypes
import           Hedgehog

import           Prelude


parameterTests :: [TestTree]
parameterTests = [
  testGroup "Parameter" $

    [ testGroup "Nat" $
      [ testProperty "parameter type" $
        property $ do p <- forAll genNatParameter
                      case testEquality (F.paramType p) BaseNatRepr of
                        Just Refl -> success
                        Nothing -> assert False
      -- TBD: needs other tests
      ]

    , testGroup "Integer" $
      [ testProperty "parameter type" $
        property $ do p <- forAll $ genIntParameter
                      case testEquality (F.paramType p) BaseIntegerRepr of
                        Just Refl -> success
                        Nothing -> assert False
      -- TBD: needs other tests
      ]
    ]

  ]
