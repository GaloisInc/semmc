module Main ( main ) where

import qualified Data.Foldable as F
import           Data.Monoid
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import qualified System.Directory as D
import           System.FilePath ( (<.>), (</>) )

import qualified SemMC.Architecture.PPC.Base as B
import qualified SemMC.Architecture.PPC32.Opcodes as PPC32
import qualified SemMC.Architecture.PPC64.Opcodes as PPC64
import qualified SemMC.DSL as DSL


data Options = Options { oManualDir :: FilePath
                       , oBaseDir :: FilePath
                       , oPseudoDir :: FilePath
                       , oBitSize :: B.BitSize
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
                                            <> O.metavar "SIZE"
                                            <> O.help "The bit size to use (Size32 or Size64)" )

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
  let sz = oBitSize opts
  genTo (oManualDir opts) sz (B.manual sz)
  genTo (oBaseDir   opts) sz (B.base   sz)
  genTo (oPseudoDir opts) sz (B.pseudo sz)
      where genTo d sz l = do (s, e) <- genOpDefs d sz l
                              putStrLn $ "Wrote " <> show s <> " files to " <> d <>
                                           (if 0 == e then "" else " (" <> show e <> " errors!)")

genOpDefs :: FilePath -> B.BitSize -> [(String, DSL.Definition)] -> IO (Int, Int)
genOpDefs d sz l = F.foldlM writeDef (0, 0) l
    where writeDef (s,e) (opName, def) =
              if opName `elem` opcodes
              then do TIO.writeFile (d </> opName <.> "sem") (DSL.printDefinition def)
                      return (s+1, e)
              else do putStrLn $ "Warning: unknown DSL defined opcode \"" <>
                               opName <> "\" (-> " <> d <> ")"
                      TIO.writeFile (d </> opName <.> "sem") (DSL.printDefinition def)
                      return (s+1, e+1)
          opcodes = map show $ case sz of
                                 B.Size32 -> PPC32.allOpcodes
                                 B.Size64 -> PPC64.allOpcodes
