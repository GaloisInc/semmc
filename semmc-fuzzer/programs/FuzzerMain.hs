{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Control.Concurrent as C
import           Control.Monad (void, replicateM_)
import qualified Control.Exception as E
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Set.NonEmpty as NES
import           Data.EnumF (EnumF)
import           Data.Proxy (Proxy(Proxy))
import           Text.Printf (printf)
import qualified System.Exit as IO
import qualified System.IO as IO

import           Data.Parameterized.Some (Some(..))
import qualified Data.Parameterized.Nonce as N
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.HasRepr (HasRepr)
import qualified Data.Parameterized.List as L

import qualified Lang.Crucible.Solver.SimpleBackend as SB

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Log as L
import qualified SemMC.Formula as F
import qualified SemMC.Concrete.Execution as CE
import           SemMC.Architecture.Evaluate (evaluateInstruction)
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as C
import qualified SemMC.Architecture.View as V
import qualified SemMC.Architecture.PPC32 as PPCS
import           SemMC.Architecture.PPC32.Opcodes as PPCS
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch
                                          , unTemplate, TemplatableOperand
                                          , TemplatedOperand
                                          )

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

startThread :: IO () -> IO (IO ())
startThread body = do
    mv <- C.newEmptyMVar
    void $ C.forkIO $ body `E.finally` (C.putMVar mv ())
    return $ C.takeMVar mv

doTesting :: IO ()
doTesting = do
  caseChan <- C.newChan
  resChan <- C.newChan

  logCfg <- L.mkLogCfg "main"

  logWaiter <- startThread $ do
      L.stdErrLogEventConsumer (const True) logCfg

  L.withLogCfg logCfg $ L.logIO L.Info $ printf "Starting up"

  opcodes <- case PPCS.allOpcodes of
      (o:os) -> return $ NES.fromList o os
      _ -> do
          IO.hPutStrLn IO.stderr "Bug: empty opcodes list"
          IO.exitFailure

  runnerWaiter <- startThread $ do
      L.withLogCfg logCfg $
          testRunner (Proxy @PPCS.PPC) opcodes PPCS.allSemantics caseChan resChan

  L.withLogCfg logCfg $
      CE.runRemote (Just ppcRunnerFilename) hostname PPCS.testSerializer caseChan resChan

  runnerWaiter

  L.logEndWith logCfg
  logWaiter

makePlain :: forall arch sym
           . (MapF.OrdF (A.Opcode arch (A.Operand arch)),
              MapF.OrdF (A.Location arch))
          => BaseSet sym arch
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
makePlain = MapF.foldrWithKey f MapF.empty
  where f :: forall sh
           . A.Opcode arch (A.Operand arch) sh
          -> F.ParameterizedFormula sym (TemplatedArch arch) sh
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
        f op pf = MapF.insert op (unTemplate pf)

testRunner :: forall proxy arch .
              ( TemplatableOperand arch
              , A.Architecture arch
              , C.ConcreteArchitecture arch
              , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
              , L.HasLogCfg
              , MapF.OrdF (A.Opcode arch (TemplatedOperand arch))
              , MapF.ShowF (A.Opcode arch (TemplatedOperand arch))
              , EnumF (A.Opcode arch (TemplatedOperand arch))
              , HasRepr (A.Opcode arch (A.Operand arch)) (L.List (A.OperandTypeRepr arch))
              )
           => proxy arch
           -> NES.Set (Some ((A.Opcode arch) (A.Operand arch)))
           -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
           -> C.Chan (Maybe [CE.TestCase (V.ConcreteState arch) (A.Instruction arch)])
           -> C.Chan (CE.ResultOrError (V.ConcreteState arch))
           -> IO ()
testRunner proxy opcodes semantics caseChan resChan = do
    let chunkSize = 10000

    N.withIONonceGenerator $ \nonceGen -> do
      sym :: SB.SimpleBackend s
          <- SB.newSimpleBackend nonceGen

      baseSet <- F.loadFormulas sym semantics
      let plainBaseSet :: MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (SB.SimpleBackend s) arch)
          plainBaseSet = makePlain baseSet

      -- Submit the test cases
      replicateM_ chunkSize $ do
          gen <- DA.createGen
          inst <- D.randomInstruction gen opcodes
          initialState <- C.randomState proxy gen
          nonce <- N.indexValue <$> N.freshNonce nonceGen
          evalResult <- evaluateInstruction sym plainBaseSet inst initialState

          case evalResult of
              Left err -> do
                  L.logIO L.Error $ printf "Error evaluating instruction: %s" err

              Right finalState -> do
                  let testCase = CE.TestCase { CE.testNonce = nonce
                                             , CE.testProgram = [inst]
                                             , CE.testContext = initialState
                                             }

                  doTest testCase finalState

      C.writeChan caseChan Nothing
      L.logIO L.Info "Runner done"

      where
        doTest tc expectedFinal = do
          -- Send a test case
          C.writeChan caseChan (Just [tc])
          -- Get the result
          res <- C.readChan resChan

          case res of
            CE.InvalidTag t -> do
              L.logIO L.Error $ printf "Invalid tag: %d" t
            CE.TestContextParseFailure -> do
              L.logIO L.Error "Test context parse failure"
            CE.TestSignalError nonce sig -> do
              L.logIO L.Error $ printf "Failed with unexpected signal (%d) on test case %d" sig nonce
            CE.TestReadError tag -> do
              L.logIO L.Error $ printf "Failed with a read error (%d)" tag
            CE.TestSuccess tr -> do
              if (expectedFinal /= CE.resultContext tr)
                 then L.logIO L.Error $ printf "ERROR: Context mismatch: expected %s, got %s" (show expectedFinal) (show $ CE.resultContext tr)
                 else L.logIO L.Info "SUCCESS"

main :: IO ()
main = doTesting
