{-# LANGUAGE FlexibleContexts #-}
-- | A module implementing communication with a remote oracle for machine
-- instructions
module SemMC.Concrete.Execution (
  runRemote,
  TestCase(..),
  TestResult(..),
  ResultOrError(..),
  LogMessage(..),
  TestSerializer(..),
  TestRunner
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.Int ( Int32 )
import qualified Data.Time.Clock as T
import           Data.Word ( Word8, Word16, Word64 )
import qualified System.IO as IO

import qualified SemMC.Concrete.Execution.SSH as SSH

-- | Functions for converting a 'TestCase' to binary and parsing
-- binary machine state.
data TestSerializer c{-^machine state/context-} i{-^instruction-} =
  TestSerializer { flattenMachineState :: c -> B.ByteString
                 , parseMachineState :: B.ByteString -> Maybe c
                 , flattenProgram :: [i] -> LB.ByteString -- ^ This means assemble in practice.
                 }

data TestCase c i =
  TestCase { testNonce :: Word64
           , testContext :: c -- ^ The machine state to start the test in.
           , testProgram :: [i]
           }
  deriving (Show)

data TestResult c =
  TestResult { resultNonce :: Word64
             , resultContext :: c -- ^ The final machine state.
             }
  deriving (Show)

data LogMessage = LogMessage { lmTime :: T.UTCTime
                             , lmHost :: String
                             , lmMessage :: String
                             }
                deriving (Eq, Ord, Show)

-- | The 'runRemote' below provides the main implementation of
-- 'TestRunner'. The Toy architecture uses a simpler 'TestRunner',
-- that does everything locally.
type TestRunner c i
  =  C.Chan (Maybe (TestCase c i))
  -- ^ A channel with test cases to be run; a 'Nothing' indicates that
  -- the stream should be terminated.
  -> C.Chan (ResultOrError c)
  -- ^ The channel that results are written to (can include error cases)
  -> C.Chan LogMessage
  -- ^ A channel to record log messages on; these include the stderr
  -- output from the runner process (there shouldn't really be much, but
  -- it is better to collect it than discard it or just dump it to
  -- stderr)
  -> IO (Maybe SSH.SSHError)
  -- ^ Errors raised by SSH. Not meaningful for test runners that
  -- don't use SSH, e.g. Toy arch test runner.

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
          -> TestSerializer c i
          -- ^ Functions for converting to and from machine states
          -> TestRunner c i
runRemote hostName ts testCases testResults logMessages = do
  ehdl <- SSH.ssh SSH.defaultSSHConfig hostName ["remote-runner"]
  case ehdl of
    Left err -> return (Just err)
    Right sshHdl -> do
      logger <- A.async (logRemoteStderr logMessages hostName (SSH.sshStderr sshHdl))
      sendCases <- A.async (sendTestCases ts testCases (SSH.sshStdin sshHdl))
      recvResults <- A.async (recvTestResults ts testResults (SSH.sshStdout sshHdl))
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

sendTestCases :: TestSerializer c i -> C.Chan (Maybe (TestCase c i)) -> IO.Handle -> IO ()
sendTestCases ts c h = do
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
        Just tc -> do -- convert to binary
          let regStateBytes = flattenMachineState ts (testContext tc)
          let asm = flattenProgram ts $ testProgram tc
          let bs = mconcat [ B.word8 0
                           , B.word64LE (testNonce tc)
                           , B.word16BE (fromIntegral (B.length regStateBytes))
                           , B.byteString regStateBytes
                           , B.word16BE (fromIntegral (LB.length asm))
                           , B.lazyByteString asm
                           ]
          B.hPutBuilder h bs
          IO.hFlush h
          go

recvTestResults :: TestSerializer c i -> C.Chan (ResultOrError c) -> IO.Handle -> IO ()
recvTestResults ts c h = do
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
        Just bs -> go (G.runGetIncremental (getTestResultOrError ts) `G.pushChunk` bs)
    go d =
      case d of
        G.Fail _ _ msg -> fail msg
        G.Done rest _ res -> do
          C.writeChan c res
          case B.null rest of
            True -> start
            False -> go (G.runGetIncremental (getTestResultOrError ts) `G.pushChunk` rest)
        G.Partial f -> tryRead h >>= (go . f)

data ResultOrError c = TestReadError Word16
                     | TestSignalError Word64 Int32
                     -- ^ (Nonce, Signum)
                     | TestSuccess (TestResult c)
                     | TestContextParseFailure
                     | InvalidTag Word8
                     -- ^ Tag value

getTestResultOrError :: TestSerializer c i -> G.Get (ResultOrError c)
getTestResultOrError ts = do
  tag <- G.getWord8
  case tag of
    0 -> do
      mtr <- getTestResult ts
      case mtr of
        Just tr -> return (TestSuccess tr)
        Nothing -> return TestContextParseFailure
    1 -> TestReadError <$> G.getWord16be
    2 -> TestSignalError <$> G.getWord64le <*> G.getInt32be
    _ -> return (InvalidTag tag)

getTestResult :: TestSerializer c i -> G.Get (Maybe (TestResult c))
getTestResult ts = do
  nonce <- G.getWord64le
  ctxSize <- G.getWord16be
  ctxbs <- G.getByteString (fromIntegral ctxSize)
  case parseMachineState ts ctxbs of
    Just ctx -> do
      let tr = TestResult { resultNonce = nonce
                          , resultContext = ctx
                          }
      return (Just tr)
    Nothing -> return Nothing
