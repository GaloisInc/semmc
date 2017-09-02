{-# LANGUAGE TypeApplications #-}
module Stochastic ( tests ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified SemMC.ToyExample.Tests as TT

tests :: T.TestTree
tests = T.testGroup "Synthesis" [ rightValueWrongPlace ]

rightValueWrongPlace :: T.TestTree
rightValueWrongPlace = T.testCase "rightValueWrongPlace" $ do
  (expectedWeight, actualWeight) <- TT.rightValueWrongPlace
  T.assertEqual "weight" expectedWeight actualWeight
