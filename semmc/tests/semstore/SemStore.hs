module Main where

import           LocationsTests
import           ParameterTests
import           ParamFormulaTests
import           Test.Tasty

import           Prelude

main :: IO ()
main = defaultMain $ testGroup "Storable Semantics" $
       locationTests <> parameterTests <> parameterizedFormulaTests
