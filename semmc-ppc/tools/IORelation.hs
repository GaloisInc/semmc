{-# LANGUAGE TypeApplications #-}
-- | A tool for learning IORelations of PPC instructions
module Main ( main ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import           Control.Monad ( when, unless )
import qualified Data.Constraint as C
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import qualified Options.Applicative as O
import qualified System.Directory as DIR
import qualified System.Exit as IO
import qualified System.IO as IO
import           Text.Printf ( printf )

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Constraints as C
import qualified SemMC.Stochastic.IORelation as IOR
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Architecture.PPC as PPC
import qualified SemMC.Log as L

import qualified Logging as L
import qualified OpcodeLists as OL
import qualified Util as U

data Logging = Verbose | Quiet

data Options = Options { oRelDir :: FilePath
                       , oNumThreads :: Int
                       , oTimeoutSeconds :: Int
                       , oRemoteHost :: String
                       , oPrintLog :: Logging
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
                        <*> O.strOption ( O.long "remote-host"
                                        <> O.short 'H'
                                        <> O.metavar "HOST"
                                        <> O.help "The host to run the remote work on" )
                        <*> O.flag Quiet Verbose ( O.long "verbose"
                                                 <> O.short 'V'
                                                 <> O.help "Print log messages from the remote runner" )

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

  logChan <- C.newChan
  gen <- A.createGen
  let cfg = IOR.LearningConfig { IOR.lcIORelationDirectory = oRelDir opt
                               , IOR.lcNumThreads = oNumThreads opt
                               , IOR.lcAssemble = PPC.assembleInstruction
                               , IOR.lcTestGen = CS.randomState (Proxy @PPC.PPC) gen
                               , IOR.lcTimeoutSeconds = oTimeoutSeconds opt
                               , IOR.lcTestRunner = CE.runRemote (oRemoteHost opt) PPC.testSerializer
                               , IOR.lcLog = logChan
                               }
  DIR.createDirectoryIfMissing True (oRelDir opt)
  logCfg <- L.mkLogCfg
  logger <- case oPrintLog opt of
    Verbose -> A.async (L.printLogMessages logCfg logChan)
    Quiet -> A.async (L.dumpRemoteRunnerLog logChan)
  A.link logger
  logThread <- A.async (L.stdErrLogEventConsumer logCfg)
  A.link logThread

  (_iorels, failures) <- IOR.learnIORelations cfg (Proxy @PPC.PPC) U.toIORelFP (C.weakenConstraints (C.Sub C.Dict) OL.allOpcodes)
  unless (F.null failures) $ do
    putStrLn "Failed opcodes:"
    putStrLn (unlines (map show (F.toList failures)))
  return ()

