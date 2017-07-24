{-# LANGUAGE FlexibleContexts #-}
-- | A module implementing communication with a remote oracle for machine
-- instructions
module SemMC.Stochastic.Remote (
  runRemote,
  TestCase(..),
  TestResult(..),
  ResultOrError(..),
  LogMessage(..),
  MachineState(..)
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Data.Int ( Int32 )
import qualified Data.Time.Clock as T
import Data.Word ( Word8, Word16, Word64 )
import qualified System.IO as IO

import qualified SemMC.Stochastic.Remote.SSH as SSH

data MachineState a =
  MachineState { flattenMachineState :: a -> B.ByteString
               , parseMachineState :: B.ByteString -> Maybe a
               }

data TestCase a =
  TestCase { testNonce :: Word64
           , testContext :: a
           , testProgram :: B.ByteString
           }

data TestResult a =
  TestResult { resultNonce :: Word64
             , resultContext :: a
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
          -> MachineState a
          -- ^ Functions for converting to and from machine states
          -> C.Chan (Maybe (TestCase a))
          -- ^ A channel with test cases to be run; a 'Nothing' indicates that
          -- the stream should be terminated.
          -> C.Chan (ResultOrError a)
          -- ^ The channel that results are written to (can include error cases)
          -> C.Chan LogMessage
          -- ^ A channel to record log messages on; these include the stderr
          -- output from the runner process (there shouldn't really be much, but
          -- it is better to collect it than discard it or just dump it to
          -- stderr)
          -> IO (Maybe SSH.SSHError)
runRemote hostName ms testCases testResults logMessages = do
  ehdl <- SSH.ssh SSH.defaultSSHConfig hostName ["remote-runner"]
  case ehdl of
    Left err -> return (Just err)
    Right sshHdl -> do
      logger <- A.async (logRemoteStderr logMessages hostName (SSH.sshStderr sshHdl))
      sendCases <- A.async (sendTestCases ms testCases (SSH.sshStdin sshHdl))
      recvResults <- A.async (recvTestResults ms testResults (SSH.sshStdout sshHdl))
      -- We only want to end when the receive end finishes (i.e., when the
      -- receive handle is closed due to running out of input).  If we end when
      -- the send end finishes, we might miss some results.
      _ <- A.wait recvResults
      A.cancel logger
      A.cancel sendCases
      SSH.killConnection sshHdl
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

sendTestCases :: MachineState a -> C.Chan (Maybe (TestCase a)) -> IO.Handle -> IO ()
sendTestCases ms c h = do
  IO.hSetBinaryMode h True
  IO.hSetBuffering h (IO.BlockBuffering Nothing)
  go
  where
    go = do
      mtc <- C.readChan c
      case mtc of
        Nothing -> do
          B.hPutBuilder h (B.word8 1)
          IO.hFlush h
        Just tc -> do
          let regStateBytes = flattenMachineState ms (testContext tc)
          let bs = mconcat [ B.word8 0
                           , B.word64LE (testNonce tc)
                           , B.word16BE (fromIntegral (B.length regStateBytes))
                           , B.byteString regStateBytes
                           , B.word16BE (fromIntegral (B.length (testProgram tc)))
                           , B.byteString (testProgram tc)
                           ]
          B.hPutBuilder h bs
          IO.hFlush h
          go

recvTestResults :: MachineState a -> C.Chan (ResultOrError a) -> IO.Handle -> IO ()
recvTestResults ms c h = do
  IO.hSetBinaryMode h True
  start
  IO.hClose h
  where
    tryRead hdl = do
      bs <- B.hGetSome hdl 4096
      case B.null bs of
        True -> return Nothing
        False -> return (Just bs)
    start = do
      mbs <- tryRead h
      case mbs of
        Nothing -> return ()
        Just bs -> go (G.runGetIncremental (getTestResultOrError ms) `G.pushChunk` bs)
    go d =
      case d of
        G.Fail _ _ msg -> fail msg
        G.Done rest _ res -> do
          C.writeChan c res
          case B.null rest of
            True -> start
            False -> go (G.runGetIncremental (getTestResultOrError ms) `G.pushChunk` rest)
        G.Partial f -> tryRead h >>= (go . f)

data ResultOrError a = TestReadError Word16
                     | TestSignalError Word64 Int32
                     -- ^ (Nonce, Signum)
                     | TestSuccess (TestResult a)
                     | TestContextParseFailure
                     | InvalidTag Word8
                     -- ^ Tag value

getTestResultOrError :: MachineState a -> G.Get (ResultOrError a)
getTestResultOrError ms = do
  tag <- G.getWord8
  case tag of
    0 -> do
      mtr <- getTestResult ms
      case mtr of
        Just tr -> return (TestSuccess tr)
        Nothing -> return TestContextParseFailure
    1 -> TestReadError <$> G.getWord16be
    2 -> TestSignalError <$> G.getWord64le <*> G.getInt32be
    _ -> return (InvalidTag tag)

getTestResult :: MachineState a -> G.Get (Maybe (TestResult a))
getTestResult ms = do
  nonce <- G.getWord64le
  ctxSize <- G.getWord16be
  ctxbs <- G.getByteString (fromIntegral ctxSize)
  case parseMachineState ms ctxbs of
    Just ctx -> do
      let tr = TestResult { resultNonce = nonce
                          , resultContext = ctx
                          }
      return (Just tr)
    Nothing -> return Nothing
