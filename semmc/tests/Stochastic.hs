{-# LANGUAGE TypeApplications #-}
module Stochastic ( tests ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified SemMC.ToyExample.Tests as TT
import qualified SemMC.Util as U

tests :: U.HasLogCfg => T.TestTree
tests = T.testGroup "Synthesis" [ rightValueWrongPlace ]

rightValueWrongPlace :: U.HasLogCfg => T.TestTree
rightValueWrongPlace = T.testCase "rightValueWrongPlace" $ do
  (expectedWeight, actualWeight) <- TT.test_rightValueWrongPlace
  T.assertEqual "weight" expectedWeight actualWeight
