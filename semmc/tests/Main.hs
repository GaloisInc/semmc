{-# LANGUAGE ImplicitParams #-}
module Main ( main ) where

import qualified Test.Tasty as T
import qualified Control.Exception as CE

import qualified SemMC.Util as U

import Util

allTests :: (U.HasLogCfg) => T.TestTree
allTests = T.testGroup "SemMC" [ Util.tests ]

main :: IO ()
main = do
  logCfg <- U.mkLogCfg "main"
  let ?logCfg = logCfg
  U.withAsyncLinked (U.tmpFileLogEventConsumer logCfg) $ const $
    T.defaultMain allTests `CE.finally` U.logEndWith logCfg
