{-# LANGUAGE TypeApplications #-}
module Stochastic ( tests ) where

import qualified Data.Maybe as D

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified SemMC.Toy.Tests as TT
import qualified SemMC.Util as U

tests :: U.HasLogCfg => T.TestTree
tests = T.testGroup "Synthesis" [ rightValueWrongPlace
                                , synthesizeCandidate ]

rightValueWrongPlace :: U.HasLogCfg => T.TestTree
rightValueWrongPlace = T.testCase "rightValueWrongPlace" $ do
  (expectedWeight, actualWeight) <- TT.test_rightValueWrongPlace
  T.assertEqual "weight" expectedWeight actualWeight

synthesizeCandidate :: U.HasLogCfg => T.TestTree
synthesizeCandidate  = T.testCase "synthesizeCandidate" $ do
  mProg <- TT.test_synthesizeCandidate
  T.assertBool "found a candidate" (D.isJust mProg)
