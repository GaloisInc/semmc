{-# LANGUAGE TypeApplications #-}
module Stochastic ( tests ) where

import qualified Data.Maybe as D

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified SemMC.Toy.Tests as TT
import qualified SemMC.Util as U

tests :: U.HasLogCfg => T.TestTree
tests = T.testGroup "Synthesis" [ rightValueWrongPlace
                                , rvwpOptimizationApplies
                                , rvwpOptimizationDoesntApply
                                , synthesizeCandidate ]

rightValueWrongPlace :: U.HasLogCfg => T.TestTree
rightValueWrongPlace = T.testCase "rightValueWrongPlace" $ do
  (expectedWeight, actualWeight) <- TT.test_rightValueWrongPlace
  T.assertEqual "weight" expectedWeight actualWeight

rvwpOptimizationApplies :: U.HasLogCfg => T.TestTree
rvwpOptimizationApplies = T.testCase "rvwpOptimizationApplies" $ do
  (doesOptimizationApply, howManyPlaces, expectedNumberOfPlaces) <-
    TT.test_rvwpOptimizationApplies
  T.assertEqual "number of places it applies" expectedNumberOfPlaces howManyPlaces
  T.assertBool "optimization applies" doesOptimizationApply

rvwpOptimizationDoesntApply :: U.HasLogCfg => T.TestTree
rvwpOptimizationDoesntApply = T.testCase "rvwpOptimizationDoesntApply" $ do
  (doesOptimizationApply, howManyPlaces, expectedNumberOfPlaces) <-
    TT.test_rvwpOptimizationDoesntApply
  T.assertEqual "number of places it applies" expectedNumberOfPlaces howManyPlaces
  T.assertBool "optimization doesn't apply" (not doesOptimizationApply)

synthesizeCandidate :: U.HasLogCfg => T.TestTree
synthesizeCandidate  = T.testCase "synthesizeCandidate (SubRr)" $ do
  mProg <- TT.test_synthesizeCandidate
  T.assertBool "found a candidate" (D.isJust mProg)
