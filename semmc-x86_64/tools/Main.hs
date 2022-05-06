{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Concurrent as C
import qualified Data.ByteString.Lazy as LB
import           Data.Proxy ( Proxy(..) )
import qualified Data.Vector.Sized as V
import           GHC.TypeNats ( KnownNat, natVal )
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import           Text.Printf ( printf )

import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Util as U
import           SemMC.X86 ( Instruction, MachineState(..), testSerializer, YMM(..) )

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
    grs = sizedVecFromList [ 11, 0, 0 , 0, 0
                           , 20, 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           ]
    mask = sizedVecFromList (replicate 16 0)
    frs = sizedVecFromList (replicate 8 0)
    vs = sizedVecFromList (replicate 16 ymm0)
    m1 = sizedVecFromList (replicate 32 0)
    ymm0 = YMM 0 0 0 0

testVector2 :: CE.TestCase MachineState Instruction
testVector2 =
  testVector1 { CE.testNonce = 22
              , CE.testContext = (CE.testContext testVector1) { gprs = grs }
              }
  where
    grs = sizedVecFromList [ 200, 0, 0 , 0, 0
                           , 99, 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           ]

-- | Convert a list to a 'V.Vector' of length @n@. If the length of the list is
-- not equal to @n@, throw an exception.
sizedVecFromList :: forall n a. KnownNat n => [a] -> V.Vector n a
sizedVecFromList l =
  case V.fromList l of
    Just v -> v
    Nothing -> error $ "sizedVecFromList: Expected list of length "
                    ++ show (natVal (Proxy @n))
                    ++ ", received list of length "
                    ++ show (length l)
