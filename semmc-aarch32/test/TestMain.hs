{-# LANGUAGE NondecreasingIndentation #-}
module Main ( main ) where

import qualified Control.Concurrent as C
import           Data.Bits
import           Data.Word
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import           Text.Printf ( printf )

import qualified SemMC.Concrete.Execution as CE
import           SemMC.Architecture.ARM.MachineState ( MachineState(..), Instruction, testSerializer )
import qualified SemMC.Util as U

main :: IO ()
main = do
  [hostname] <- E.getArgs
  caseChan <- C.newChan
  resChan <- C.newChan
  lcfg <- U.mkLogCfg "main"
  U.withLogCfg lcfg $ do
  U.withAsyncLinked (testRunner caseChan resChan) $ \_ -> do
  CE.runRemote Nothing hostname Nothing testSerializer caseChan resChan

testRunner :: C.Chan (Maybe [CE.TestCase MachineState Instruction])
           -> C.Chan (CE.ResultOrError MachineState)
           -> IO ()
testRunner caseChan resChan = do
  mapM_ doTest []
  C.writeChan caseChan Nothing
  where
    doTest vec = do
      C.writeChan caseChan (Just vec)
      res <- C.readChan resChan
      case res of
        CE.InvalidTag t -> do
          IO.hPutStrLn IO.stderr $ printf "Invalid tag: %d" t
          IO.exitFailure
        CE.TestContextParseFailure -> do
          IO.hPutStrLn IO.stderr "Test context parse failure"
          IO.exitFailure
        CE.TestSignalError nonce sig -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with unexpected signal (%d) on test case %d" sig nonce
          IO.exitFailure
        CE.TestReadError tag -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with a read error (%d)" tag
          IO.exitFailure
        CE.TestSuccess tr -> do
          printf "Received test result with nonce %d\n" (CE.resultNonce tr)
          print (CE.resultContext tr)
