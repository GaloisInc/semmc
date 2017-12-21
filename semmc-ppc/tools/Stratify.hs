{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Concurrent.Async as A
import           Control.Monad
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import qualified Options.Applicative as O
import qualified System.Directory as DIR
import qualified System.Exit as IO
import           Text.Printf ( printf )

import qualified Data.Parameterized.Nonce as N
import           Data.Parameterized.Some ( Some(..) )

import qualified Lang.Crucible.Solver.SimpleBackend as SB

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Log as L
import qualified SemMC.Stochastic.IORelation as IOR
import qualified SemMC.Stochastic.Strata as SST

import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Util as U

import qualified OpcodeLists as OL
import qualified Util as Util

data Options = Options { oRelDir :: FilePath
                       , oBaseDir :: FilePath
                       , oPseudoDir :: FilePath
                       , oLearnedDir :: FilePath
                       , oStatisticsFile :: FilePath
                       , oLogFile :: Maybe FilePath
                       , oProgramCount :: Int
                       , oRandomTests :: Int
                       , oParallelOpcodes :: Int
                       , oParallelSynth :: Int
                       , oOpcodeTimeoutSeconds :: Int
                       , oRemoteTimeoutSeconds :: Int
                       , oRemoteRunner :: FilePath
                       , oRemoteHost :: String
                       }

optionsParser :: O.Parser Options
optionsParser = Options <$> O.strOption ( O.long "relation-directory"
                                        <> O.short 'r'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store learned IO relations" )
                        <*> O.strOption ( O.long "base-directory"
                                        <> O.short 'b'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to find the base set of semantics" )
                        <*> O.strOption ( O.long "pseudo-directory"
                                        <> O.short 'p'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to find the pseudo ops" )
                        <*> O.strOption ( O.long "learned-directory"
                                        <> O.short 'l'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store learned semantics" )
                        <*> O.strOption ( O.long "statistics-file"
                                        <> O.short 's'
                                        <> O.metavar "FILE"
                                        <> O.help "The file in which to persist search statistics" )
                        <*> O.optional ( O.strOption ( O.long "log-file"
                                                     <> O.short 'L'
                                                     <> O.metavar "FILE"
                                                     <> O.help "A file to store the log in.  The default (with no option) is stderr." ))
                        <*> O.option O.auto ( O.long "program-threshold"
                                            <> O.short 'P'
                                            <> O.value 5
                                            <> O.showDefault
                                            <> O.metavar "COUNT"
                                            <> O.help "The number of candidate programs to find before extracting a formula" )
                        <*> O.option O.auto ( O.long "random-test-count"
                                            <> O.short 'R'
                                            <> O.metavar "COUNT"
                                            <> O.value 1000
                                            <> O.showDefault
                                            <> O.help "The number of random test vectors to generate" )
                        <*> O.option O.auto ( O.long "parallel-opcodes"
                                            <> O.short 'O'
                                            <> O.metavar "THREADS"
                                            <> O.value 1
                                            <> O.showDefault
                                            <> O.help "The number of opcodes to process in parallel" )
                        <*> O.option O.auto ( O.long "parallel-synth"
                                            <> O.short 'S'
                                            <> O.metavar "THREADS"
                                            <> O.help "The number of threads to run in parallel to find candidate programs (for each opcode)" )
                        <*> O.option O.auto (  O.long "opcode-timeout"
                                            <> O.short 't'
                                            <> O.metavar "SECONDS"
                                            <> O.value 1200
                                            <> O.showDefault
                                            <> O.help "The number of seconds to wait before giving up on learning a program for an opcode" )
                        <*> O.option O.auto ( O.long "remote-timeout"
                                            <> O.short 'T'
                                            <> O.metavar "SECONDS"
                                            <> O.help "The number of seconds to wait for all responses from the remote runner" )
                        <*> O.strOption ( O.long "remote-runner"
                                        <> O.metavar "EXE"
                                        <> O.help "The name of the remote runner executable (remote-runner.ppc32 or remote-runner.ppc64)" )
                        <*> O.strOption ( O.long "remote-host"
                                        <> O.short 'H'
                                        <> O.metavar "HOST"
                                        <> O.help "The host to run the remote work on" )

main :: IO ()
main = O.execParser optParser >>= mainWithOptions
 where
   optParser = O.info (optionsParser O.<**> O.helper)
     ( O.fullDesc
     <> O.progDesc "Learn semantics for PPC instructions"
     <> O.header "semmc-ppc-stratify - learn semantics for each instruction"
     <> O.footer "See semmc-ppc/README.md for a detailed usage example.")

mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  when (oParallelOpcodes opts < 1 || oParallelSynth opts < 1) $ do
    IO.die $ printf "Invalid thread count: %d / %d\n" (oParallelOpcodes opts) (oParallelSynth opts)

  iorels <- IOR.loadIORelations (Proxy @PPC32.PPC) (oRelDir opts) Util.toIORelFP OL.allOpcodes32

  rng <- DA.createGen
  let testGenerator = AC.randomState (Proxy @PPC32.PPC) rng
  Some ng <- N.newIONonceGenerator
  sym <- SB.newSimpleBackend ng
  let serializer = CE.TestSerializer { CE.flattenMachineState = AC.serialize (Proxy @PPC32.PPC)
                                     , CE.parseMachineState = AC.deserialize (Proxy @PPC32.PPC)
                                     , CE.flattenProgram = mconcat . map PPC.assembleInstruction
                                     }

  lcfg <- L.mkLogCfg "main"
  L.withLogCfg lcfg $ do
  logThread <- case oLogFile opts of
    Nothing -> U.asyncLinked (L.stdErrLogEventConsumer (const True) lcfg)
    Just logFile -> U.asyncLinked (L.fileLogEventConsumer logFile (const True) lcfg)

  stThread <- SST.newStatisticsThread (oStatisticsFile opts)

  DIR.createDirectoryIfMissing True (oLearnedDir opts)
  let cfg = SST.Config { SST.baseSetDir = oBaseDir opts
                       , SST.pseudoSetDir = oPseudoDir opts
                       , SST.learnedSetDir = oLearnedDir opts
                       , SST.programCountThreshold = oProgramCount opts
                       , SST.randomTestCount = oRandomTests opts
                       , SST.remoteRunnerTimeoutSeconds = oRemoteTimeoutSeconds opts
                       , SST.opcodeTimeoutSeconds = oOpcodeTimeoutSeconds opts
                       , SST.parallelOpcodes = oParallelOpcodes opts
                       , SST.parallelSynth = oParallelSynth opts
                       , SST.testRunner = CE.runRemote (Just (oRemoteRunner opts)) (oRemoteHost opts) serializer
                       , SST.logConfig = lcfg
                       , SST.statsThread = stThread
                       }
  let opcodes :: [Some (PPC.Opcode PPC.Operand)]
      -- In production we want to know and target as many opcodes as
      -- possible, but while developing mcmc synthesis it's simpler to
      -- only consider a few instructions at a time. At least some of
      -- the targets should be expressible in terms of the base set
      -- (@opcodes@) and the pseudo ops, but I'm not sure if that's
      -- true right now.

      -- opcodes = C.weakenConstraints (C.Sub C.Dict) OL.allOpcodes32
      opcodes = [ Some PPC.ADD4 ]
      targets :: [Some (PPC.Opcode PPC.Operand)]
      -- targets = C.weakenConstraints (C.Sub C.Dict) OL.allOpcodes
      targets = [ Some PPC.ADD4o ]
  senv <- SST.loadInitialState cfg sym testGenerator initialTestCases opcodes OL.pseudoOps32 targets iorels
  _ <- SST.stratifiedSynthesis senv

  L.logEndWith lcfg
  A.wait logThread

  SST.terminateStatisticsThread stThread
  where
    initialTestCases = AC.heuristicallyInterestingStates (Proxy @PPC32.PPC)
