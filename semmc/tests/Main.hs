module Main ( main ) where

import qualified Test.Tasty as T

import Formula
import Stochastic

allTests :: T.TestTree
allTests = T.testGroup "SemMC" [ Formula.tests
                               , Stochastic.tests ]

main :: IO ()
main = do
  T.defaultMain allTests
