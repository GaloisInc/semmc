module Main ( main ) where

import qualified Control.Concurrent as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.Time.Format as T
import qualified Data.Vector.Sized as V
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import Text.Printf ( printf )

import qualified SemMC.Concrete.Execution as CE
import SemMC.X86 ( Instruction, MachineState(..), testSerializer, YMM(..) )

main :: IO ()
main = do
  [hostname] <- E.getArgs
  logChan <- C.newChan
  caseChan <- C.newChan
  resChan <- C.newChan
  _ <- C.forkIO (printLogMessages logChan)
  _ <- C.forkIO (testRunner caseChan resChan)
  merr <- CE.runRemote Nothing hostname testSerializer caseChan resChan logChan
  case merr of
    Just err -> do
      IO.hPutStrLn IO.stderr $ printf "SSH Error: %s" (show err)
      IO.exitFailure
    Nothing -> return ()

testRunner :: C.Chan (Maybe [CE.TestCase MachineState Instruction])
           -> C.Chan (CE.ResultOrError MachineState)
           -> IO ()
testRunner caseChan resChan = do
  mapM_ doTest [testVector1, testVector2]
  C.writeChan caseChan Nothing
  where
    doTest vec = do
      C.writeChan caseChan (Just [vec])
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

testVector1 :: CE.TestCase MachineState Instruction
testVector1 = CE.TestCase { CE.testNonce = 11
                         , CE.testContext = ctx
                         -- mov %rax, %rsi
                         , CE.testProgram = [LB.pack [0x48, 0x89, 0xc6]]
                         }
  where
    ctx = MachineState { gprs = grs
                       , gprs_mask = mask
                       , eflags = 0
                       , fprs = frs
                       , vrs = vs
                       , mem1 = m1
                       , mem2 = m1
                       }
    Just grs = V.fromList [ 11, 0, 0 , 0, 0
                          , 20, 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          ]
    Just mask = V.fromList (replicate 16 0)
    Just frs = V.fromList (replicate 8 0)
    Just vs = V.fromList (replicate 16 ymm0)
    Just m1 = V.fromList (replicate 32 0)
    ymm0 = YMM 0 0 0 0

testVector2 :: CE.TestCase MachineState Instruction
testVector2 = testVector1 { CE.testNonce = 22
                          , CE.testContext = (CE.testContext testVector1) { gprs = grs }
                          }
  where
    Just grs = V.fromList [ 200, 0, 0 , 0, 0
                          , 99, 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          ]

printLogMessages :: C.Chan CE.LogMessage -> IO ()
printLogMessages c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (CE.lmTime msg)
  IO.hPutStrLn IO.stderr $ printf "%s[%s]: %s" fmtTime (CE.lmHost msg) (CE.lmMessage msg)
  printLogMessages c
