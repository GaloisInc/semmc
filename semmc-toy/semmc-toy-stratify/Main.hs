{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Concurrent.Async as CC
import qualified Control.Exception as CC
import           Control.Monad
-- Do we actually care about this for Toy arch?
import qualified Options.Applicative as O
import qualified System.Directory as DIR
import qualified System.Exit as IO
import           Text.Printf ( printf )

import           Data.Parameterized.Some ( Some(..) )

import qualified SemMC.Log as L
import qualified SemMC.Stochastic.Strata as SST

import qualified SemMC.Toy as Toy
import qualified SemMC.Toy.Tests as Toy
import qualified SemMC.Util as U


-- TODO(conathan): I copied the source for @semmc-ppc-stratify@ to
-- create @semmc-toy-stratify@ (this file), and it probably makes
-- sense to factor out common stuff into @semmc@. For example, the
-- option parsing should be mostly the same for all arch specific
-- stratifiers (but e.g. for the Toy arch we may not want to specify a
-- remote runner, or if we do actually implement a remote runner for
-- Toy, we'll also want a way to ignore it and use an in process fast
-- path when our goal isn't testing the remote runner code).
--
-- Look into this deduplication after @semmc-toy-stratify@ is working,
-- so that we have at least two examples to generalize from.
data Options = Options { _oRelDir :: FilePath
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
                       , _oRemoteRunner :: FilePath
                       , _oRemoteHost :: String
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
                                        <> O.help "The name of the remote runner executable (TODO(conathan) remote-runner.ppc32 or remote-runner.ppc64)" )
                        <*> O.strOption ( O.long "remote-host"
                                        <> O.short 'H'
                                        <> O.metavar "HOST"
                                        <> O.help "The host to run the remote work on" )

main :: IO ()
main = do
  opts <- O.execParser optParser

  lcfg <- L.mkLogCfg "main"
  logThread <- case oLogFile opts of
    Nothing -> U.asyncLinked (L.stdErrLogEventConsumer (const True) lcfg)
    Just logFile -> U.asyncLinked (L.fileLogEventConsumer logFile (const True) lcfg)
  let shutdownLogger = L.logEndWith lcfg >> CC.wait logThread

  L.withLogCfg lcfg (mainWithOptions opts) `CC.finally` shutdownLogger
 where
   optParser = O.info (optionsParser O.<**> O.helper)
     ( O.fullDesc
     <> O.progDesc "Learn semantics for Toy instructions"
     <> O.header "semmc-toy-stratify - learn semantics for each instruction"
     <> O.footer "See semmc-toy/README.md for a detailed usage example.")

mainWithOptions :: (L.HasLogCfg) => Options -> IO ()
mainWithOptions opts = do
  when (oParallelOpcodes opts < 1 || oParallelSynth opts < 1) $ do
    IO.die $ printf "Invalid thread count: %d / %d\n" (oParallelOpcodes opts) (oParallelSynth opts)

  -- iorels <- IOR.loadIORelations (Proxy @PPC32.PPC) (oRelDir opts) Util.toIORelFP (C.weakenConstraints (C.Sub C.Dict) OL.allOpcodes32)

  -- No serializer for Toy arch at this time.
  {-
  import qualified SemMC.Architecture.Concrete as AC
  let serializer = CE.TestSerializer { CE.flattenMachineState = AC.serialize (Proxy @Toy.Toy)
                                     , CE.parseMachineState = AC.deserialize (Proxy @Toy.Toy)
                                     , CE.flattenProgram = mconcat . map Toy.assembleInstruction
                                     }
  -}

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
                       -- , SST.testRunner = CE.runRemote (Just (oRemoteRunner opts)) (oRemoteHost opts) Nothing serializer
                       , SST.testRunner = Toy.toyTestRunnerBackend 0
                       , SST.logConfig = L.getLogCfg
                       , SST.statsThread = stThread
                       }
  let opcodes :: [Some (Toy.Opcode Toy.Operand)]
      opcodes = [ Some Toy.NegR
                , Some Toy.SubRr ]
  let pseudoOpcodes = Toy.allPseudoOpcodes
  let targetOpcodes = [ Some Toy.AddRr ]
  let iorels = Toy.ioRelations

  SST.withInitialState cfg opcodes pseudoOpcodes targetOpcodes iorels $ \senv -> do
  _ <- SST.stratifiedSynthesis senv
  SST.terminateStatisticsThread stThread
