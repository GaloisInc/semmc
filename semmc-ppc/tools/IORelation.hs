-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- | A tool for learning IORelations of PPC instructions
module Main ( main ) where

import Control.Monad ( when )
import Data.Monoid
import Data.Proxy ( Proxy(..) )
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
import qualified SemMC.Architecture.PPC as PPC

data Options = Options { oRelDir :: FilePath
                       , oNumThreads :: Int
                       , oTimeoutSeconds :: Int
                       , oRemoteHost :: String
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
  let cfg = IOR.LearningConfig { IOR.lcIORelationDirectory = oRelDir opt
                               , IOR.lcNumThreads = oNumThreads opt
                               , IOR.lcAssemble = PPC.assembleInstruction
                               , IOR.lcTestGen = CS.randomState (Proxy @PPC.PPC) gen
                               , IOR.lcMachineState = PPC.machineState
                               , IOR.lcTimeoutSeconds = oTimeoutSeconds opt
                               , IOR.lcRemoteHost = oRemoteHost opt
                               }
  DIR.createDirectoryIfMissing True (oRelDir opt)
  _iorels <- IOR.learnIORelations cfg (Proxy @PPC.PPC) toFP allOps
  return ()


toFP :: PPC.Opcode PPC.Operand sh -> FilePath
toFP op = printf "%s.sem" (P.showF op)

allOps :: [Some (Witness U.UnfoldShape (PPC.Opcode PPC.Operand))]
allOps = [Some (Witness PPC.ADD4)
         ]
  -- $(DT.captureDictionaries ''PPC.Opcode)
