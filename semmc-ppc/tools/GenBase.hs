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
                       , oPseudoDir :: FilePath
                       , oBitSize :: Int
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
                        <*> O.strOption ( O.long "pseudo"
                                        <> O.short 'P'
                                        <> O.metavar "DIR"
                                        <> O.help "The directory to store pseudo-operation formulas in" )
                        <*> O.option O.auto ( O.long "bit-size"
                                            <> O.short 's'
                                            <> O.metavar "INT"
                                            <> O.help "The bit size to use (32 or 64)" )

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
  D.createDirectoryIfMissing True (oPseudoDir opts)
  F.forM_ (B.manual (oBitSize opts)) $ \(opName, def) -> do
    TIO.writeFile (oManualDir opts </> opName <.> "sem") (DSL.printDefinition def)
  F.forM_ (B.base (oBitSize opts)) $ \(opName, def) -> do
    TIO.writeFile (oBaseDir opts </> opName <.> "sem") (DSL.printDefinition def)
  F.forM_ (B.pseudo (oBitSize opts)) $ \(opName, def) -> do
    TIO.writeFile (oPseudoDir opts </> opName <.> "sem") (DSL.printDefinition def)
