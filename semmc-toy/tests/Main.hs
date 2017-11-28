{-# LANGUAGE ImplicitParams #-}
module Main ( main ) where

import qualified Control.Concurrent.Async as C
import qualified Test.Tasty as T

import qualified SemMC.Util as U

import Formula
import Stochastic

allTests :: (U.HasLogCfg) => T.TestTree
allTests = T.testGroup "SemMC" [ Formula.tests
                               , Stochastic.tests ]

main :: IO ()
main = do
  logCfg <- U.mkLogCfg "main"
  let ?logCfg = logCfg
  loggerThread <- C.async $ U.tmpFileLogEventConsumer logCfg
  C.link loggerThread

  T.defaultMain allTests
