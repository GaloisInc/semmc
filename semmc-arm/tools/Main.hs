module Main ( main ) where

import qualified Control.Concurrent as C
import qualified Data.ByteString as B
import qualified Data.Time.Format as T
import qualified Data.Vector.Sized as V
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import Text.Printf ( printf )

import qualified SemMC.Stochastic.Remote as R
import SemMC.ARM ( MachineState(..), machineState )

main :: IO ()
main = do
  [hostname] <- E.getArgs
  logChan <- C.newChan
  caseChan <- C.newChan
  resChan <- C.newChan
  _ <- C.forkIO (printLogMessages logChan)
  _ <- C.forkIO (testRunner caseChan resChan)
  merr <- R.runRemote hostname machineState caseChan resChan logChan
  case merr of
    Just err -> do
      IO.hPutStrLn IO.stderr $ printf "SSH Error: %s" (show err)
      IO.exitFailure
    Nothing -> return ()

testRunner :: C.Chan (Maybe (R.TestCase MachineState))
           -> C.Chan (R.ResultOrError MachineState)
           -> IO ()
testRunner caseChan resChan = do
  mapM_ doTest [testVector1, testVector2]
  C.writeChan caseChan Nothing
  where
    doTest vec = do
      C.writeChan caseChan (Just vec)
      res <- C.readChan resChan
      case res of
        R.InvalidTag t -> do
          IO.hPutStrLn IO.stderr $ printf "Invalid tag: %d" t
          IO.exitFailure
        R.TestContextParseFailure -> do
          IO.hPutStrLn IO.stderr "Test context parse failure"
          IO.exitFailure
        R.TestSignalError nonce sig -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with unexpected signal (%d) on test case %d" sig nonce
          IO.exitFailure
        R.TestReadError tag -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with a read error (%d)" tag
          IO.exitFailure
        R.TestSuccess tr -> do
          printf "Received test result with nonce %d\n" (R.resultNonce tr)
          print (R.resultContext tr)

testVector1 :: R.TestCase MachineState
testVector1 = R.TestCase { R.testNonce = 11
                         , R.testContext = ctx
                         -- add r1, r2?
                         , R.testProgram = B.pack [0x02, 0x10, 0x81, 0xE0]
                         }
  where
    ctx = MachineState { gprs = grs
                       , gprs_mask = mask
                       , fprs = frs
                       , mem1 = m1
                       , mem2 = m1
                       }
    Just grs = V.fromList [ 11, 15, 25 , 0, 0
                          , 20, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]
    Just mask = V.fromList (replicate 16 0)
    Just frs = V.fromList (replicate 32 0)
    Just m1 = V.fromList (replicate 32 0)

testVector2 :: R.TestCase MachineState
testVector2 = testVector1 { R.testNonce = 22
                          , R.testContext = (R.testContext testVector1) { gprs = grs }
                          , R.testProgram = B.pack [0x02, 0x10, 0x81, 0xE0]
                          }
  where
    Just grs = V.fromList [ 200, 100, 9, 0, 0
                          , 99, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]

printLogMessages :: C.Chan R.LogMessage -> IO ()
printLogMessages c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (R.lmTime msg)
  IO.hPutStrLn IO.stderr $ printf "%s[%s]: %s" fmtTime (R.lmHost msg) (R.lmMessage msg)
  printLogMessages c
