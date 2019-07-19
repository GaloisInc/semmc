module Main where

import LocationsTests
import ParamFormulaTests
import ParameterTests
import Test.Tasty
import TestUtils

import Prelude

main :: IO ()
main = do debugReset
          defaultMain $ testGroup "Storable Semantics" $
            locationTests <> parameterTests <> parameterizedFormulaTests
