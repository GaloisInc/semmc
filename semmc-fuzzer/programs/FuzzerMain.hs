module Main where

-- TODO:
--
-- Command line options type
--
-- How do tests get spread over remote hosts? Need remote host list for
-- each ISA. Probably want to have one worker thread per host, with a
-- manager thread per ISA giving out work to do.
--
-- Testing loop:
--   Inputs: ISA, N, list of opcodes (or all) OS
--   For each opcode O in OS:
--     Generate a chunk of N pairs of (random initial state, random instruction for O)
--     Submit chunk to remote host for execution
--     Evaluate chunk final states and evaluate instructions against semantics and initial states, compare results
--     Emit chunk result summary to event log
--       (coalesce successes)

-- Generation of random instructions:
--   Dismantle.Instruction.Random.randomInstruction
--     randomInstruction :: (ArbitraryOperands c o, OrdF (c o))
--                       => A.Gen
--                       -> NES.Set (Some (c o))
--                       -> IO (I.GenericInstruction c o)
--   example: SemMC.Stochastic.Instantiate
--
-- Generation of random tests:
--   SemMC.Architecture.Concrete, randomState method of Architecture class
--     randomState :: proxy arch -> DA.Gen -> IO (ConcreteState arch)
--   see also serialize/deserialize class methods
--     serialize :: proxy arch -> ConcreteState arch -> B.ByteString
--     deserialize :: proxy arch -> B.ByteString -> Maybe (ConcreteState arch)
--
-- Running on remote hosts:
--   SemMC.Concrete.Execution, runRemote
--     Make a remote runner for a given host:
--     runRemote :: (U.HasLogCfg)
--               => Maybe FilePath
--               -- ^ Optionally, a different name for the remote runner executable
--               -> String
--               -- ^ The hostname to run test cases on
--               -> TestSerializer c i
--               -- ^ Functions for converting to and from machine states
--               -> TestRunner c i
--
--    Returns a TestRunner, so we have to then bind a
--      Chan (Maybe [TestCase c i])
--    to send it test cases (Nothing means stop) and then bind a
--      Chan (ResultOrError c)
--    to get results from it.
--    The 'c' is a machine state.
--    The 'i' is an instruction.
--
--    Seems to raise an exception if for some reason the connection to
--    the remote host (or invocation of the runner) fails.
--
-- Remote runner: semmc/tools/remote-runner.c
--   Needs to be built on each remote host and needs to be in the PATH
--   of the account used for testing
--   Note: defaults to $USER on remote host and defaults to using a
--   local SSH key + remote authorized_keys to authenticate.
--
-- Helpers:
--   SemMC.Stochastic.Monad.runConcreteTests (interacts with remote-runner)
--   semmc/semmc-ppc/tools/Test.hs
--
-- Instantiating semantics:
--   semmc-ppc/tools/SynthDemo.hs

data FuzzerConfiguration =
    FuzzerConfiguration { fuzzerArchitectures :: [FuzzerArchConfig]
                        , fuzzerOutputDirectory :: FilePath
                        }
                        deriving (Show)

data FuzzerArchConfig =
    FuzzerArchConfig { fuzzerArchName :: String
                     , fuzzerArchTestingHosts :: [FuzzerTestHost]
                     }
                     deriving (Show)

data FuzzerTestHost =
    FuzzerTestHost { fuzzerTestHostname :: String
                   , fuzzerTestRunners :: Int
                   , fuzzerTestChunkSize :: Int
                   }
                   deriving (Show)

-- import qualified Dismantle.Arbitrary as DA
-- gen <- DA.createGen
-- let op = an opcode
-- inst <- D.randomInstruction gen (NES.singleton (Some op))

-- Need a list of known architectures. What do we need for each?
--  * Something from dismantle
--  * Something from semmc
-- knownArchitectures :: []
-- knownArchitectures =
--     [
--     ]
--

ppcRunnerFilename :: FilePath
ppcRunnerFilename = "remote-runner.ppc32"

hostname :: String
hostname = "helium.proj.galois.com"

import qualified Control.Concurrent as C
import qualified System.Exit as IO
import qualified System.IO as IO

import qualified Dismantle.PPC as PPC
import           SemMC.Architecture.PPC32 as PPCS

import qualified SemMC.Concrete.Execution as CE

doTesting :: IO ()
doTesting = do
  caseChan <- C.newChan
  resChan <- C.newChan

  logCfg <- L.mkLogCfg "main"
  void $ C.forkIO $ stdErrLogEventConsumer (const True) logCfg

  void $ C.forkIO $ testRunner caseChan resChan

  L.withLogCfg logCfg $
    CE.runRemote (Just ppcRunnerFilename) hostname PPCS.testSerializer caseChan resChan

testRunner :: C.Chan (Maybe [CE.TestCase PPCS.PPCState PPC.Instruction])
           -> C.Chan (CE.ResultOrError PPCS.PPCState)
           -> IO ()
testRunner caseChan resChan = do
   let i = PPC.Instruction PPC.ADD4 (PPC.Gprc r28 PPC.:< PPC.Gprc r17 PPC.:< PPC.Gprc r25 PPC.:< PPC.Nil)
       testVector1 :: CE.TestCase PPCS.PPCState PPC.Instruction
       testVector1 = CE.TestCase { CE.testNonce = 11
                                 , CE.testProgram = [i]
                                 , CE.testContext = sbase
                                 }

  doTest Nothing testVector1
  C.writeChan caseChan Nothing

  where
    doTest mr vec = do
      -- Write a test case
      C.writeChan caseChan (Just [vec])
      -- Get the result
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
          case mr of
            Just oldRes -> do
              when (oldRes /= CE.resultContext tr) $ do
                IO.hPutStrLn IO.stderr "ERROR: Context mismatch"
            Nothing -> return ()

          return (CE.resultContext tr)

main :: IO ()
main = doTesting
