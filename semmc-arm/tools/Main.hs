module Main ( main ) where

import qualified Control.Concurrent as C
import qualified Data.Time.Format as T
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import Text.Printf ( printf )

import qualified SemMC.Concrete.Execution as CE
import Tests.StoreTest ( storeTests )
import Tests.CMNTest ( cmnTests )
import Tests.LoadTest ( loadTests )
import SemMC.ARM ( MachineState(..), Instruction, testSerializer )

main :: IO ()
main = do
  [hostname] <- E.getArgs
  logChan <- C.newChan
  caseChan <- C.newChan
  resChan <- C.newChan
  _ <- C.forkIO (printLogMessages logChan)
  _ <- C.forkIO (testRunner caseChan resChan)
  merr <- CE.runRemote hostname testSerializer caseChan resChan logChan
  case merr of
    Just err -> do
      IO.hPutStrLn IO.stderr $ printf "SSH Error: %s" (show err)
      IO.exitFailure
    Nothing -> return ()

testRunner :: C.Chan (Maybe (CE.TestCase MachineState Instruction))
           -> C.Chan (CE.ResultOrError MachineState)
           -> IO ()
testRunner caseChan resChan = do
  mapM_ doTest (cmnTests ++ storeTests ++ loadTests)
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

{-
-- | Data representation of CPSR flags
data Flag = N | Z | C | V | Q
  deriving (Show, Eq)

-- | Given a list of flags initializes a CPSR set to user mode
mkCPSR :: [Flag] -> Word32
mkCPSR flags = foldr (.|.) 16 [n,z,c,v,q]
  where n = if N `elem` flags then (2 ^ (31 :: Word32)) else 0
        z = if Z `elem` flags then (2 ^ (30 :: Word32)) else 0
        c = if C `elem` flags then (2 ^ (29 :: Word32)) else 0
        v = if V `elem` flags then (2 ^ (28 :: Word32)) else 0
        q = if Q `elem` flags then (2 ^ (27 :: Word32)) else 0
-}

printLogMessages :: C.Chan CE.LogMessage -> IO ()
printLogMessages c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (CE.lmTime msg)
  IO.hPutStrLn IO.stderr $ printf "%s[%s]: %s" fmtTime (CE.lmHost msg) (CE.lmMessage msg)
  printLogMessages c
