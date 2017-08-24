module Main ( main ) where

import qualified Test.Tasty as T

import Formula ( formulaTests )

allTests :: T.TestTree
allTests = T.testGroup "SemMC" [ formulaTests ]

main :: IO ()
main = do
  T.defaultMain allTests
