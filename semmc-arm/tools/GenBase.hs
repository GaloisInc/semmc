module Main where

import qualified Data.Foldable as F
import           Data.Monoid
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import qualified Options.Applicative.Help as OH
import qualified SemMC.Architecture.ARM.BaseSemantics as B
import           SemMC.Architecture.ARM.Opcodes ( allA32Opcodes, allT32Opcodes )
import qualified SemMC.DSL as DSL
import qualified System.Directory as D
import           System.FilePath ( (<.>), (</>) )


data Options = Options { oRootDir   :: FilePath
                       }

optionsParser :: O.Parser Options
optionsParser = Options
                <$> O.strArgument ( O.metavar "ROOTDIR"
                                  <> O.help "Root directory under which output files will be \
                                            \ generated (\"data/sem\" is recommended).")


main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = (O.info (optionsParser O.<**> O.helper)
                 ( O.fullDesc
                 <> O.header "semmc-arm-genbase - Print out manually-written formulas"
                 )) { O.infoProgDesc = OH.vsepChunks
                      [ OH.paragraph
                        "Generates the base and manual formula set files for ARM.\
                        \ The semmc tool will learn semantics for many instructions,\
                        \ but it needs to start with a base set, and there are some\
                        \ that it cannot learn and which must be specified manually."
                      , OH.paragraph
                        "This tool is used to generate those base and manual formulas,\
                        \ as written in the DSL specifications in the\
                        \ SemMC.Architecture.ARM.BaseSemantics module."
                      ]
                    }


mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  let odir = oRootDir opts
      genTo d (t,l) = do
        (s, e) <- genOpDefs d l
        putStrLn $ "Wrote " <> (show s) <> " " <> t <> " semantics files to " <> d <>
                       (if 0 == e then "" else " (" <> show e <> " errors!)")
  D.createDirectoryIfMissing True odir
  F.mapM_ (genTo odir) B.semdefs


genOpDefs :: FilePath -> [(String, DSL.Definition)] -> IO (Int, Int)
genOpDefs d l = F.foldlM writeDef (0, 0) l
    where writeDef (s,e) (opName, def) =
              if opName `elem` opcodes
              then do TIO.writeFile (d </> opName <.> "sem") (DSL.printDefinition def)
                      return (s+1, e)
              else do putStrLn $ "ERR: ignoring unknown DSL defined opcode \"" <>
                               opName <> "\" (-> " <> d <> ")"
                      return (s, e+1)
          opcodes = map show allA32Opcodes ++ map show allT32Opcodes
