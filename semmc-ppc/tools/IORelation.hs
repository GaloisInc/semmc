{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NondecreasingIndentation #-}
-- | A tool for learning IORelations of PPC instructions
module Main ( main ) where

import qualified Control.Concurrent.Async as A
import           Control.Monad
import qualified Data.Foldable as F
import           Data.Proxy ( Proxy(..) )
import qualified Options.Applicative as O
import qualified System.Directory as DIR
import qualified System.Exit as IO
import qualified System.IO as IO
import           Text.Printf ( printf )

import           Prelude

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Stochastic.IORelation as IOR
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Log as L
import qualified SemMC.Util as U

import qualified OpcodeLists as OL
import qualified Util as U

data Options = Options { oRelDir :: FilePath
                       , oNumThreads :: Int
                       , oTimeoutSeconds :: Int
                       , oRemoteRunner :: FilePath
                       , oRemoteHost :: String
                       }

optionsParser :: O.Parser Options
optionsParser = Options <$> O.strOption ( O.long "relation-directory"
                                        <> O.short 'r'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store learned IO relations" )
                        <*> O.option O.auto ( O.long "num-threads"
                                            <> O.short 'N'
                                            <> O.metavar "THREADS"
                                            <> O.help "The number of executor threads to run" )
                        <*> O.option O.auto ( O.long "timeout"
                                            <> O.short 't'
                                            <> O.metavar "SECONDS"
                                            <> O.help "The number of seconds to wait for all responses from the remote runner" )
                        <*> O.strOption ( O.long "remote-runner"
                                        <> O.short 'R'
                                        <> O.metavar "EXE"
                                        <> O.help "The name of the remote runner (remote-runner.ppc32 or remote-runner.ppc64)" )
                        <*> O.strOption ( O.long "remote-host"
                                        <> O.short 'H'
                                        <> O.metavar "HOST"
                                        <> O.help "The host to run the remote work on" )

main :: IO ()
main = O.execParser optParser >>= mainWithOptions
 where
   optParser = O.info (optionsParser O.<**> O.helper)
     ( O.fullDesc
     <> O.progDesc "Learn IORelations for PPC"
     <> O.header "semmc-ppc-iorels - learn the input and output operands for each instruction")

mainWithOptions :: Options -> IO ()
mainWithOptions opt = do
  when (oNumThreads opt < 1) $ do
    IO.hPutStr IO.stderr $ printf "Invalid thread count: %d\n" (oNumThreads opt)
    IO.exitFailure
  when (oTimeoutSeconds opt < 1) $ do
    IO.hPutStr IO.stderr $ printf "Invalid timeout: %d\n" (oNumThreads opt)
    IO.exitFailure

  gen <- A.createGen
  logCfg <- L.mkLogCfg "main"
  L.withLogCfg logCfg $ do
  let cfg = IOR.LearningConfig { IOR.lcIORelationDirectory = oRelDir opt
                               , IOR.lcNumThreads = oNumThreads opt
                               , IOR.lcAssemble = PPC.assembleInstruction
                               , IOR.lcTestGen = AC.randomState (Proxy @PPC32.PPC) gen
                               , IOR.lcTimeoutSeconds = oTimeoutSeconds opt
                               , IOR.lcTestRunner =
                                   CE.runRemote (Just (oRemoteRunner opt)) (oRemoteHost opt) Nothing PPC32.testSerializer
                               , IOR.lcLogCfg = logCfg
                               }
  DIR.createDirectoryIfMissing True (oRelDir opt)
  logThread <- U.asyncLinked (L.stdErrLogEventConsumer (const True) logCfg)

  (_iorels, failures) <- IOR.learnIORelations cfg (Proxy @PPC32.PPC) U.toIORelFP OL.allOpcodes32
  unless (F.null failures) $ do
    putStrLn "Failed opcodes:"
    putStrLn (unlines (map show (F.toList failures)))

  L.logEndWith logCfg
  A.wait logThread
