module Main ( main ) where

import qualified Data.Foldable as F
import           Data.Monoid
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import qualified System.Directory as D
import           System.FilePath ( (<.>), (</>) )

import qualified SemMC.DSL as DSL
import qualified SemMC.Architecture.PPC.Base as B

data Options = Options { oManualDir :: FilePath
                       , oBaseDir :: FilePath
                       }

optionsParser :: O.Parser Options
optionsParser = Options <$> O.strOption ( O.long "manual"
                                        <> O.short 'M'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store manual formulas in" )
                        <*> O.strOption ( O.long "base"
                                        <> O.short 'B'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store base formulas in" )

main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = O.info (optionsParser O.<**> O.helper)
                ( O.fullDesc
                <> O.progDesc "Print the base and manual formula sets for PPC"
                <> O.header "semmc-ppc-genbase - Print out manually-written formulas"
                )

mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  D.createDirectoryIfMissing True (oManualDir opts)
  D.createDirectoryIfMissing True (oBaseDir opts)
  F.forM_ B.manual $ \(opName, def) -> do
    TIO.writeFile (oManualDir opts </> opName <.> "sem") (DSL.printDefinition def)
  F.forM_ B.base $ \(opName, def) -> do
    TIO.writeFile (oBaseDir opts </> opName <.> "sem") (DSL.printDefinition def)
