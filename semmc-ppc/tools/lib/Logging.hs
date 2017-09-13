module Logging (
  printLogMessages,
  dumpRemoteRunnerLog
  ) where

import qualified Control.Concurrent as C
import qualified Data.Time.Format as T
import           Text.Printf ( printf )

import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Log as L

dumpRemoteRunnerLog :: C.Chan CE.LogMessage -> IO ()
dumpRemoteRunnerLog c = do
  _msg <- C.readChan c
  dumpRemoteRunnerLog c

printLogMessages :: L.LogCfg -> C.Chan CE.LogMessage -> IO ()
printLogMessages logCfg c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (CE.lmTime msg)
  L.logIOWith logCfg L.Debug $ printf "%s[%s]: %s" fmtTime (CE.lmHost msg) (CE.lmMessage msg)
  printLogMessages logCfg c
