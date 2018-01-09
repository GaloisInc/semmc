{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import           Control.Monad (replicateM_, replicateM)
import qualified Control.Exception as E
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Set.NonEmpty as NES
import qualified Data.Map as M
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
import           Dismantle.Instruction (GenericInstruction)
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
ppcRunnerFilename = "/home/cygnus/bin/remote-runner.ppc32"

hostname :: String
hostname = "helium.proj.galois.com"

doTesting :: IO ()
doTesting = do
  caseChan <- C.newChan
  resChan <- C.newChan

  logCfg <- L.mkLogCfg "main"

  logThread <- CA.async $ do
      L.stdErrLogEventConsumer (const True) logCfg

  L.withLogCfg logCfg $ L.logIO L.Info $ printf "Starting up"

  opcodes <- case PPCS.allOpcodes of
      (o:os) -> return $ NES.fromList o os
      _ -> do
          IO.hPutStrLn IO.stderr "Bug: empty opcodes list"
          IO.exitFailure

  runThread <- CA.async $ do
      L.withLogCfg logCfg $
          testRunner (Proxy @PPCS.PPC) opcodes PPCS.allSemantics caseChan resChan

  CA.link runThread

  L.withLogCfg logCfg $
      CE.runRemote (Just ppcRunnerFilename) hostname PPCS.testSerializer caseChan resChan

  CA.wait runThread

  L.logEndWith logCfg
  CA.wait logThread

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
              , Show (GenericInstruction (A.Opcode arch) (A.Operand arch))
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
    let chunkSize = 100

    N.withIONonceGenerator $ \nonceGen -> do
      gen <- DA.createGen
      sym :: SB.SimpleBackend s
          <- SB.newSimpleBackend nonceGen

      baseSet <- F.loadFormulas sym semantics
      let plainBaseSet :: MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (SB.SimpleBackend s) arch)
          plainBaseSet = makePlain baseSet
          generateTestCase = go
              where
                  go = do
                    inst <- D.randomInstruction gen opcodes
                    initialState <- C.randomState proxy gen
                    nonce <- N.indexValue <$> N.freshNonce nonceGen
                    evalResult <- E.try $ evaluateInstruction sym plainBaseSet inst initialState

                    case evalResult of
                        Left (e::E.SomeException) -> do
                            L.logIO L.Error $ printf "Exception evaluating instruction %s: %s" (show inst) (show e)
                            go
                        Right (Left _) -> go
                        Right (Right finalState) -> do
                            return ( CE.TestCase { CE.testNonce = nonce
                                                 , CE.testProgram = [inst]
                                                 , CE.testContext = initialState
                                                 }
                                   , finalState
                                   )

      cases <- replicateM chunkSize generateTestCase

      let caseMap = M.fromList [ (CE.testNonce (fst c), snd c) | c <- cases ]

      -- Send test cases
      C.writeChan caseChan (Just $ fst <$> cases)
      C.writeChan caseChan Nothing

      -- Process results
      replicateM_ (length cases) (handleResult caseMap)

      where
        handleResult caseMap = do
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
              let Just expectedFinal = M.lookup (CE.resultNonce tr) caseMap
              if (expectedFinal /= CE.resultContext tr)
                 then L.logIO L.Error $ printf "ERROR: Context mismatch: expected %s, got %s" (show expectedFinal) (show $ CE.resultContext tr)
                 else L.logIO L.Info "SUCCESS"

main :: IO ()
main = doTesting
