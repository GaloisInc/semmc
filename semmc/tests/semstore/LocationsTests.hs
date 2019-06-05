{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocationsTests where

import           Data.Parameterized.Classes
import qualified SemMC.Architecture.Location as L
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           TestArch
import           TestArchPropGen
import           What4.BaseTypes
import           Hedgehog

import           Prelude


locationTests :: [TestTree]
locationTests = [
  testGroup "Location" $

    [ testGroup "Nat" $
      [ testProperty "location value distribution" $ -- test generator validity
        property $ do l <- forAll genNatLocation
                      let locNat = case l of { TestNatLoc n -> n }
                      -- classify "natloc 0" (locNat == 0)
                      -- classify "natloc single" (locNat == 1)
                      -- classify "natloc multiple" (locNat >= 2)
                      cover 1 "natloc 0" $ locNat == 0
                      cover 1 "natloc single" $ locNat == 1
                      success
      , testProperty "location type" $
        property $ do l <- forAll genNatLocation
                      case testEquality (L.locationType l) BaseNatRepr of
                          Just Refl -> success
                          Nothing -> assert False
      , testProperty "is mem location" $
        property $ do l <- forAll genNatLocation
                      assert $ not $ L.isMemoryLocation l
      -- TBD: needs other tests
      ]

    , testGroup "Integer" $
      [ testProperty "location value distribution" $ -- test generator validity
        property $ do l <- forAll genIntLocation
                      let locVal = case l of { TestIntLoc n -> n }
                      cover 1 "intloc 0" $ locVal == 0
                      cover 1 "intloc single" $ locVal == 1
                      cover 10 "intloc negative" $ locVal < 0
                      success
      , testProperty "location type" $
        property $ do l <- forAll $ genIntLocation
                      case testEquality (L.locationType l) BaseIntegerRepr of
                        Just Refl -> success
                        Nothing -> assert False
      , testProperty "is mem location" $
        property $ do l <- forAll genNatLocation
                      assert $ not $ L.isMemoryLocation l
      -- TBD: needs other tests
      ]
    ]

  ]
