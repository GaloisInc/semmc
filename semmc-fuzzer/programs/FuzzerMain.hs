{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Control.Concurrent as C
import           Control.Monad (when, void)
import qualified Data.Set.NonEmpty as NES
import           Data.Proxy (Proxy(Proxy))
import           Text.Printf (printf)
import qualified System.Exit as IO
import qualified System.IO as IO

import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Nonce as N

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Log as L
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as C
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Architecture.View as V
import qualified SemMC.Architecture.PPC32 as PPCS
import           SemMC.Architecture.PPC32.Opcodes as PPCS

type PPCState = V.ConcreteState PPCS.PPC

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

-- Generation of random tests:
--   SemMC.Architecture.Concrete, randomState method of Architecture class
--     randomState :: proxy arch -> DA.Gen -> IO (ConcreteState arch)
--   see also serialize/deserialize class methods
--     serialize :: proxy arch -> ConcreteState arch -> B.ByteString
--     deserialize :: proxy arch -> B.ByteString -> Maybe (ConcreteState arch)
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

doTesting :: IO ()
doTesting = do
  caseChan <- C.newChan
  resChan <- C.newChan

  logCfg <- L.mkLogCfg "main"
  void $ C.forkIO $ L.stdErrLogEventConsumer (const True) logCfg

  opcodes <- case PPCS.allOpcodes of
      (o:os) -> return $ NES.fromList o os
      _ -> do
          IO.hPutStrLn IO.stderr "Bug: empty opcodes list"
          IO.exitFailure

  void $ C.forkIO $
      N.withIONonceGenerator $
          L.withLogCfg logCfg $
              testRunner (Proxy @PPCS.PPC) opcodes caseChan resChan

  L.withLogCfg logCfg $
    CE.runRemote (Just ppcRunnerFilename) hostname PPCS.testSerializer caseChan resChan

testRunner :: forall proxy arch s .
              ( A.Architecture arch
              , C.ConcreteArchitecture arch
              , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
              , L.HasLogCfg
              )
           => proxy arch
           -> NES.Set (Some ((A.Opcode arch) (A.Operand arch)))
           -> C.Chan (Maybe [CE.TestCase (V.ConcreteState arch) (A.Instruction arch)])
           -> C.Chan (CE.ResultOrError (V.ConcreteState arch))
           -> N.NonceGenerator IO s
           -> IO ()
testRunner proxy opcodes caseChan resChan nonceGen = do
  gen <- DA.createGen

  inst <- D.randomInstruction gen opcodes
  state <- C.randomState proxy gen
  nonce <- N.indexValue <$> N.freshNonce nonceGen

  let testCase = CE.TestCase { CE.testNonce = nonce
                             , CE.testProgram = [inst]
                             , CE.testContext = state
                             }

  void $ doTest Nothing testCase
  C.writeChan caseChan Nothing

  where
    doTest mr vec = do
      -- Write a test case
      C.writeChan caseChan (Just [vec])
      -- Get the result
      res <- C.readChan resChan

      case res of
        CE.InvalidTag t -> do
          L.logIO L.Error $ printf "Invalid tag: %d" t
          IO.exitFailure
        CE.TestContextParseFailure -> do
          L.logIO L.Error "Test context parse failure"
          IO.exitFailure
        CE.TestSignalError nonce sig -> do
          L.logIO L.Error $ printf "Failed with unexpected signal (%d) on test case %d" sig nonce
          IO.exitFailure
        CE.TestReadError tag -> do
          L.logIO L.Error $ printf "Failed with a read error (%d)" tag
          IO.exitFailure
        CE.TestSuccess tr -> do
          case mr of
            Just oldRes -> do
              when (oldRes /= CE.resultContext tr) $ do
                L.logIO L.Error "ERROR: Context mismatch"
            Nothing -> return ()

          return (CE.resultContext tr)

main :: IO ()
main = doTesting
