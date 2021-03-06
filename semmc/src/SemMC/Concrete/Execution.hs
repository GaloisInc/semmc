{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
-- | A module implementing communication with a remote oracle for machine
-- instructions
module SemMC.Concrete.Execution (
  runRemote,
  withTestResults,
  ResultIndex(..),
  emptyResultIndex,
  indexResults,
  TestCase(..),
  TestResult(..),
  ResultOrError(..),
  RunnerResultError(..),
  asResultOrError,
  TestSerializer(..),
  TestRunner
  ) where

import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified System.IO.Error as E
import           Control.Monad ( replicateM )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable as F
import           Data.Int ( Int32 )
import qualified Data.Map.Strict as M
import           Data.Maybe ( fromMaybe )
import           Data.Word ( Word8, Word16, Word64 )
import qualified System.IO as IO
import           Text.Printf

import qualified SemMC.Concrete.Execution.SSH as SSH
import qualified SemMC.Util as U

-- | Functions for converting a 'TestCase' to binary and parsing
-- binary machine state.
data TestSerializer c i =
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

-- | The 'runRemote' below provides the main implementation of
-- 'TestRunner'. The Toy architecture uses a simpler 'TestRunner',
-- that does everything locally.
--
-- The test runner may raise exceptions, e.g. related to failed SSH
-- connections.
type TestRunner c i
  =  C.Chan (Maybe [TestCase c i])
  -- ^ A channel with test cases to be run; a 'Nothing' indicates that
  -- the stream should be terminated.
  -> C.Chan (ResultOrError c)
  -- ^ The channel that results are written to (can include error cases)
  -> IO ()

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
runRemote :: (U.HasLogCfg)
          => Maybe FilePath
          -- ^ Optionally, a different name for the remote runner executable
          -> String
          -- ^ The hostname to run test cases on
          -> Maybe String
          -- ^ The username to use to connect to the remote host
          -- (default: $USER)
          -> TestSerializer c i
          -- ^ Functions for converting to and from machine states
          -> TestRunner c i
runRemote mexe hostName mUser ts testCases testResults = do
  let cfg = SSH.defaultSSHConfig { SSH.sshUsername = mUser
                                 }
  sshHdl <- SSH.ssh cfg hostName [fromMaybe "remote-runner" mexe]
  let logger = logRemoteStderr hostName (SSH.sshStderr sshHdl)
  let sendCases = sendTestCases ts testCases (SSH.sshStdin sshHdl)
  U.withAsyncLinked logger $ \_ -> do
  U.withAsyncLinked sendCases $ \_ -> do
  -- We only want to end when the receive end finishes (i.e., when the
  -- receive handle is closed due to running out of input).  If we end when
  -- the send end finishes, we might miss some results.
  recvTestResults ts testResults (SSH.sshStdout sshHdl)

-- | Log the stderr output of the remote runner.
logRemoteStderr :: (U.HasLogCfg) => String -> IO.Handle -> IO ()
logRemoteStderr host h =
    let go = do
          l <- E.try $ IO.hGetLine h
          case l of
              Right line -> do
                  let msg = printf "remote runner stderr @%s: %s" host line
                  U.logIO U.Warn msg
                  go
              Left e
                  | E.isEOFError e -> return ()
                  | otherwise ->
                      U.logIO U.Error $ printf "exception when reading from runner stderr: %s" (show e)
    in go

sendTestCases :: TestSerializer c i -> C.Chan (Maybe [TestCase c i]) -> IO.Handle -> IO ()
sendTestCases ts c h = do
  IO.hSetBinaryMode h True
  IO.hSetBuffering h (IO.BlockBuffering Nothing)
  go
  where
    go = do
      mtc <- C.readChan c
      case mtc of
        Nothing -> do
          -- Here 1 means "no more work items" to the remote runner.
          B.hPutBuilder h (B.word8 1)
          IO.hFlush h
        Just tcs -> do -- convert to binary
          F.forM_ tcs $ \tc -> do
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
                     deriving (Show)

asResultOrError :: ResultOrError c -> Either RunnerResultError (TestResult c)
asResultOrError r =
  case r of
    TestSuccess tr -> Right tr
    TestReadError w -> Left (RunnerReadError w)
    TestSignalError n s -> Left (RunnerSignalError n s)
    TestContextParseFailure -> Left RunnerContextParseFailure
    InvalidTag t -> Left (RunnerInvalidTag t)

data RunnerResultError = RunnerReadError Word16
                       | RunnerSignalError Word64 Int32
                       | RunnerContextParseFailure
                       | RunnerInvalidTag Word8
                       deriving (Show)

instance E.Exception RunnerResultError

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

data ResultIndex a = ResultIndex { riExitedWithSignal :: !(M.Map Word64 Int32)
                                 -- ^ A set of nonces for tests that failed with a signal
                                 , riSuccesses :: !(M.Map Word64 (TestResult a))
                                 -- ^ The results of tests, keyed by nonce
                                 }

emptyResultIndex :: ResultIndex a
emptyResultIndex = ResultIndex { riExitedWithSignal = M.empty
                               , riSuccesses = M.empty
                               }

indexResults :: [ResultOrError c] -> ResultIndex c
indexResults = F.foldl' addResult emptyResultIndex

addResult :: ResultIndex c -> ResultOrError c -> ResultIndex c
addResult ri res =
  case res of
    TestReadError {} -> ri
    TestSignalError trNonce trSignum ->
      ri { riExitedWithSignal = M.insert trNonce trSignum (riExitedWithSignal ri) }
    TestContextParseFailure -> ri
    InvalidTag {} -> ri
    TestSuccess tr ->
      ri { riSuccesses = M.insert (resultNonce tr) tr (riSuccesses ri) }


withTestResults :: (MonadIO m)
                => C.Chan (Maybe [TestCase c i])
                -> C.Chan (ResultOrError c)
                -> [TestCase c i]
                -> m [ResultOrError c]
withTestResults testChan resChan tests = do
  liftIO (C.writeChan testChan (Just tests))
  liftIO $ replicateM (length tests) $ C.readChan resChan
