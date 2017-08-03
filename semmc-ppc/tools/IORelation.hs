-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- | A tool for learning IORelations of PPC instructions
module Main ( main ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import Control.Monad ( when )
import Data.Monoid
import Data.Proxy ( Proxy(..) )
import qualified Data.Time.Format as T
import qualified Options.Applicative as O
import qualified System.Directory as DIR
import qualified System.Exit as IO
import qualified System.IO as IO
import Text.Printf ( printf )

import qualified Data.Parameterized.Classes as P
import Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Unfold as U
import Data.Parameterized.Witness ( Witness(..) )

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC
import Dismantle.PPC.Random ()
-- import qualified Dismantle.Tablegen.TH as DT
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Stochastic.IORelation as IOR
import qualified SemMC.Stochastic.Remote as R
import qualified SemMC.Architecture.PPC as PPC

data Logging = Verbose | Quiet

data Options = Options { oRelDir :: FilePath
                       , oNumThreads :: Int
                       , oTimeoutSeconds :: Int
                       , oRemoteHost :: String
                       , oPrintLog :: Logging
                       }

optionsParser :: O.Parser Options
optionsParser = Options <$> O.strOption ( O.long "relation-directory"
                                        <> O.short 'd'
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
                                        <> O.short 'h'
                                        <> O.metavar "HOST"
                                        <> O.help "The host to run the remote work on" )
                        <*> O.flag Quiet Verbose ( O.long "verbose"
                                                 <> O.short 'v'
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
                               , IOR.lcMachineState = PPC.machineState
                               , IOR.lcTimeoutSeconds = oTimeoutSeconds opt
                               , IOR.lcRemoteHost = oRemoteHost opt
                               , IOR.lcLog = logChan
                               }
  DIR.createDirectoryIfMissing True (oRelDir opt)
  logger <- case oPrintLog opt of
    Verbose -> A.async $ printLogMessages logChan

    Quiet -> A.async $ dumpLog logChan
  A.link logger
  _iorels <- IOR.learnIORelations cfg (Proxy @PPC.PPC) toFP allOps
  return ()

dumpLog :: C.Chan R.LogMessage -> IO ()
dumpLog c = do
  _msg <- C.readChan c
  dumpLog c

printLogMessages :: C.Chan R.LogMessage -> IO ()
printLogMessages c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (R.lmTime msg)
  IO.hPutStrLn IO.stderr $ printf "%s[%s]: %s" fmtTime (R.lmHost msg) (R.lmMessage msg)
  printLogMessages c

toFP :: PPC.Opcode PPC.Operand sh -> FilePath
toFP op = printf "%s.sem" (P.showF op)

allOps :: [Some (Witness U.UnfoldShape (PPC.Opcode PPC.Operand))]
allOps = [Some (Witness PPC.ADD4)
         ]
  -- $(DT.captureDictionaries ''PPC.Opcode)
