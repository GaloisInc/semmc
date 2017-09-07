{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import           Control.Monad ( when, unless )
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import qualified Data.Time.Format as T
import qualified Options.Applicative as O
import qualified System.Directory as DIR
import qualified System.Exit as IO
import qualified System.IO as IO
import           Text.Printf ( printf )

import qualified Data.Parameterized.Nonce as N
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Unfold as U
import           Data.Parameterized.Witness ( Witness(..) )

import qualified Lang.Crucible.Solver.SimpleBackend as SB

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()
import qualified Dismantle.Tablegen.TH as DT
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Stochastic.IORelation as IOR
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Stochastic.Strata as SST
import qualified SemMC.Stochastic.Pseudo as P

import qualified SemMC.Architecture.PPC as PPC

import           Util

data Logging = Verbose | Quiet

data Options = Options { oRelDir :: FilePath
                       , oBaseDir :: FilePath
                       , oPseudoDir :: FilePath
                       , oLearnedDir :: FilePath
                       , oProgramCount :: Int
                       , oRandomTests :: Int
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
                        <*> O.option O.auto ( O.long "program-threshold"
                                            <> O.short 'P'
                                            <> O.value 10
                                            <> O.showDefault
                                            <> O.metavar "COUNT"
                                            <> O.help "The number of candidate programs to find before extracting a formula" )
                        <*> O.option O.auto ( O.long "random-test-count"
                                            <> O.short 'R'
                                            <> O.metavar "COUNT"
                                            <> O.value 1000
                                            <> O.showDefault
                                            <> O.help "The number of random test vectors to generate" )
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
     <> O.progDesc "Learn semantics for PPC instructions"
     <> O.header "semmc-ppc-stratify - learn semantics for each instruction")

allOps :: [Some (Witness (F.BuildOperandList PPC.PPC) (PPC.Opcode PPC.Operand))]
allOps = undefined

allOpsRel :: [Some (Witness U.UnfoldShape (PPC.Opcode PPC.Operand))]
allOpsRel = undefined

pseudoOps :: [Some (Witness (F.BuildOperandList PPC.PPC) ((P.Pseudo PPC.PPC) PPC.Operand))]
pseudoOps = undefined

targetOpcodes :: [Some (Witness (F.BuildOperandList PPC.PPC) (PPC.Opcode PPC.Operand))]
targetOpcodes = undefined

mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  Some ng <- N.newIONonceGenerator
  sym <- SB.newSimpleBackend ng
  iorels <- IOR.loadIORelations (Proxy @PPC.PPC) (oRelDir opts) toIORelFP allOpsRel
  senv <- SST.loadInitialState cfg sym generator initialTestCases allOps pseudoOps targetOpcodes iorels
  undefined
  where
    generator = undefined
    initialTestCases = undefined
    cfg = SST.Config { SST.baseSetDir = oBaseDir opts
                     , SST.pseudoSetDir = oPseudoDir opts
                     , SST.learnedSetDir = oLearnedDir opts
                     , SST.statisticsFile = undefined
                     , SST.programCountThreshold = oProgramCount opts
                     , SST.randomTestCount = oRandomTests opts
                     , SST.threadCount = oNumThreads opts
                     , SST.testRunner = undefined
                     }
