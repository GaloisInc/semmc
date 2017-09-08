module Logging (
  printLogMessages,
  dumpLog
  ) where

import qualified Control.Concurrent as C
import qualified Data.Time.Format as T
import qualified System.IO as IO
import           Text.Printf ( printf )

import qualified SemMC.Concrete.Execution as CE

dumpLog :: C.Chan CE.LogMessage -> IO ()
dumpLog c = do
  _msg <- C.readChan c
  dumpLog c

printLogMessages :: C.Chan CE.LogMessage -> IO ()
printLogMessages c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (CE.lmTime msg)
  IO.hPutStrLn IO.stderr $ printf "%s[%s]: %s" fmtTime (CE.lmHost msg) (CE.lmMessage msg)
  printLogMessages c
