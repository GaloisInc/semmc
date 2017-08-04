{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Concurrent as C
import Data.Proxy ( Proxy(..) )
import qualified Data.Time.Format as T
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import Text.Printf ( printf )

import qualified Data.Word.Indexed as W
import qualified Dismantle.PPC as PPC
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Stochastic.Remote as R
import SemMC.Architecture.PPC

type PPCState = CS.ConcreteState PPC

main :: IO ()
main = do
  [hostname] <- E.getArgs
  logChan <- C.newChan
  caseChan <- C.newChan
  resChan <- C.newChan
  _ <- C.forkIO (printLogMessages logChan)
  _ <- C.forkIO (testRunner caseChan resChan)
  merr <- R.runRemote hostname testSerializer caseChan resChan logChan
  case merr of
    Just err -> do
      IO.hPutStrLn IO.stderr $ printf "SSH Error: %s" (show err)
      IO.exitFailure
    Nothing -> return ()

testRunner :: C.Chan (Maybe (R.TestCase PPCState PPC.Instruction))
           -> C.Chan (R.ResultOrError PPCState)
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

testVector1 :: R.TestCase PPCState PPC.Instruction
testVector1 =
  R.TestCase { R.testNonce = 11
             , R.testContext = ctx2
             , R.testProgram = [i]
             }
  where
    ctx0 = CS.zeroState (Proxy @PPC)
    ctx1 = CS.pokeMS ctx0 v2 (CS.ValueBV (W.W 21))
    ctx2 = CS.pokeMS ctx1 v3 (CS.ValueBV (W.W 45))
    i = PPC.Instruction PPC.ADD4 (PPC.Gprc r1 PPC.:> PPC.Gprc r2 PPC.:> PPC.Gprc r3 PPC.:> PPC.Nil)

r1 :: PPC.GPR
r1 = PPC.GPR 1
r2 :: PPC.GPR
r2 = PPC.GPR 2
r3 :: PPC.GPR
r3 = PPC.GPR 3

v2 :: CS.View PPC 32
v2 = CS.trivialView (Proxy @PPC) (LocGPR r2)

v3 :: CS.View PPC 32
v3 = CS.trivialView (Proxy @PPC) (LocGPR r3)

testVector2 :: R.TestCase PPCState PPC.Instruction
testVector2 = testVector1 { R.testNonce = 22
                          , R.testContext = ctx2
                          }
  where
    ctx0 = CS.zeroState (Proxy @PPC)
    ctx1 = CS.pokeMS ctx0 v2 (CS.ValueBV (W.W 1))
    ctx2 = CS.pokeMS ctx1 v3 (CS.ValueBV (W.W 5))

printLogMessages :: C.Chan R.LogMessage -> IO ()
printLogMessages c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (R.lmTime msg)
  IO.hPutStrLn IO.stderr $ printf "%s[%s]: %s" fmtTime (R.lmHost msg) (R.lmMessage msg)
  printLogMessages c
