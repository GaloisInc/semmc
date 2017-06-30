-- | A module implementing communication with a remote oracle for machine
-- instructions
module SemMC.Stochastic.Remote (
  runRemote,
  TestCase(..),
  TestResult(..),
  ResultOrError(..),
  LogMessage(..)
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Conduit as P
import qualified Data.Conduit.Serialization.Binary as P
import Data.Int ( Int32 )
import qualified Data.Time.Clock as T
import Data.Word ( Word16, Word64 )
import qualified System.IO as IO

import qualified SemMC.Stochastic.Remote.SSH as SSH

data TestCase =
  TestCase { testNonce :: Word64
           , testContext :: B.ByteString
           , testContextMask :: B.ByteString
           , testMem1 :: B.ByteString
           , testMem2 :: B.ByteString
           , testProgram :: B.ByteString
           }

data TestResult =
  TestResult { resultNonce :: Word64
             , resultContext :: B.ByteString
             , resultMem1 :: B.ByteString
             , resultMem2 :: B.ByteString
             }

data LogMessage = LogMessage { lmTime :: T.UTCTime
                             , lmHost :: String
                             , lmMessage :: String
                             }
                deriving (Eq, Ord, Show)

-- | Spawn threads to manage a remote test runner.
--
-- The runner will read tests off of the first channel and write results to the
-- second channel.  'TestCase's and 'TestResult's are connected by their 64 bit
-- nonce.
--
-- The runs are done on a remote host (specified by hostname).  The @ssh@ binary
-- is expected to be in the @PATH@, and password-less auth is assumed.  It is
-- also assumed that the @remote-runner@ executable is in the @PATH@ on the
-- remote machine.
runRemote :: String
          -- ^ The hostname to run test cases on
          -> C.Chan (Maybe TestCase)
          -- ^ A channel with test cases to be run; a 'Nothing' indicates that
          -- the stream should be terminated.
          -> C.Chan ResultOrError
          -- ^ The channel that results are written to (can include error cases)
          -> C.Chan LogMessage
          -- ^ A channel to record log messages on; these include the stderr
          -- output from the runner process (there shouldn't really be much, but
          -- it is better to collect it than discard it or just dump it to
          -- stderr)
          -> IO (Maybe SSH.SSHError)
runRemote hostName testCases testResults logMessages = do
  ehdl <- SSH.ssh SSH.defaultSSHConfig hostName ["remote-runner"]
  case ehdl of
    Left err -> return (Just err)
    Right sshHdl -> do
      logger <- A.async (logRemoteStderr logMessages hostName (SSH.sshStderr sshHdl))
      sendCases <- A.async (sendTestCases testCases (SSH.sshStdin sshHdl))
      recvResults <- A.async (recvTestResults testResults (SSH.sshStdout sshHdl))
      -- We only want to end when the receive end finishes (i.e., when the
      -- receive handle is closed due to running out of input).  If we end when
      -- the send end finishes, we might miss some results.
      _ <- A.wait recvResults
      A.cancel logger
      A.cancel sendCases
      return Nothing

logRemoteStderr :: C.Chan LogMessage -> String -> IO.Handle -> IO ()
logRemoteStderr logMessages host h = do
  l <- IO.hGetLine h
  t <- T.getCurrentTime
  let lm = LogMessage { lmTime = t
                      , lmHost = host
                      , lmMessage = l
                      }
  C.writeChan logMessages lm
  logRemoteStderr logMessages host h

sendTestCases :: C.Chan (Maybe TestCase) -> IO.Handle -> IO ()
sendTestCases c h = do
  IO.hSetBinaryMode h True
  IO.hSetBuffering h (IO.BlockBuffering Nothing)
  go
  where
    go = do
      mtc <- C.readChan c
      case mtc of
        Nothing -> do
          B.hPutBuilder h (B.word16BE 1)
          IO.hFlush h
        Just tc -> do
          let bs = mconcat [ B.word16BE 0
                           , B.word64LE (testNonce tc)
                           , B.word16BE (fromIntegral (B.length (testContext tc)))
                           , B.byteString (testContext tc)
                           , B.byteString (testContextMask tc)
                           , B.byteString (testMem1 tc)
                           , B.byteString (testMem2 tc)
                           , B.word16BE (fromIntegral (B.length (testProgram tc)))
                           , B.byteString (testProgram tc)
                           ]
          B.hPutBuilder h bs
          IO.hFlush h
          go

recvTestResults :: C.Chan ResultOrError -> IO.Handle -> IO ()
recvTestResults c h = do
  IO.hSetBinaryMode h True
  P.runConduit (P.sourceHandle h P.=$= P.conduitGet getTestResultOrError P.$$ P.mapM_C (P.liftBase . C.writeChan c))
  IO.hClose h

data ResultOrError = TestReadError Word16
                   | TestSignalError Word64 Int32
                   -- ^ (Nonce, Signum)
                   | TestMapFailed Word64
                   -- ^ Nonce
                   | TestSuccess TestResult
                   | InvalidTag Word16
                   -- ^ Tag value

getTestResultOrError :: G.Get ResultOrError
getTestResultOrError = do
  tag <- G.getWord16be
  case tag of
    0 -> TestSuccess <$> getTestResult
    1 -> TestReadError <$> G.getWord16be
    2 -> TestSignalError <$> G.getWord64le <*> G.getInt32be
    3 -> TestMapFailed <$> G.getWord64le
    _ -> return (InvalidTag tag)

getTestResult :: G.Get TestResult
getTestResult = do
  nonce <- G.getWord64le
  ctxSize <- G.getWord16be
  ctx <- G.getByteString (fromIntegral ctxSize)
  memLen <- G.getWord16be
  mem1 <- G.getByteString (fromIntegral memLen)
  mem2 <- G.getByteString (fromIntegral memLen)
  return TestResult { resultNonce  = nonce
                    , resultContext = ctx
                    , resultMem1 = mem1
                    , resultMem2 = mem2
                    }
