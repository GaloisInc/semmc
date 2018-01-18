module Main where

import qualified Data.Foldable as F
import           Data.Monoid
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import qualified Options.Applicative.Help as OH
import qualified SemMC.Architecture.ARM.BaseSemantics as B
import           SemMC.Architecture.ARM.Opcodes ( allOpcodes )
import qualified SemMC.DSL as DSL
import qualified System.Directory as D
import           System.FilePath ( (<.>), (</>) )


data Options = Options { oRootDir   :: FilePath
                       , oManualDir :: FilePath
                       , oBaseDir   :: FilePath
                       , oPseudoDir :: FilePath
                       }

optionsParser :: O.Parser Options
optionsParser = Options
                <$> O.strArgument ( O.metavar "ROOTDIR"
                                  <> O.help "Root directory under which output files will be generated.")
                <*> O.strOption ( O.long "manual"
                                <> O.short 'M'
                                <> O.metavar "DIR"
                                <> O.showDefault <> O.value "manual"
                                <> O.help ("The directory to store manual formulas in.  If this is not an\
                                           \ absolute path then it will be relative to the BASEDIR."))
                <*> O.strOption ( O.long "base"
                                <> O.short 'B'
                                <> O.metavar "DIR"
                                <> O.showDefault <> O.value "base"
                                <> O.help ("The directory to store base formulas in.  If this is not an\
                                           \ absolute path then it will be relative to the BASEDIR."))
                <*> O.strOption ( O.long "pseudo"
                                <> O.short 'P'
                                <> O.metavar "DIR"
                                <> O.showDefault <> O.value "pseudo"
                                <> O.help ("The directory to store pseudo-operation formulas in.\
                                           \ If this is not an absolute path then it will be relative\
                                           \ to the BASEDIR."))


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
  let mdir = oRootDir opts </> oManualDir opts
      bdir = oRootDir opts </> oBaseDir opts
      pdir = oRootDir opts </> oPseudoDir opts
      genTo d l = do
        (s, e) <- genOpDefs d l
        putStrLn $ "Wrote " <> (show s) <> " files to " <> d <>
                       (if 0 == e then "" else " (" <> show e <> " errors!)")
  F.mapM_ (D.createDirectoryIfMissing True) [ mdir, bdir, pdir ]
  genTo mdir B.manual
  genTo bdir B.base
  genTo pdir B.pseudo


genOpDefs :: FilePath -> [(String, DSL.Definition)] -> IO (Int, Int)
genOpDefs d l = F.foldlM writeDef (0, 0) l
    where writeDef (s,e) (opName, def) =
              if opName `elem` opcodes
              then do TIO.writeFile (d </> opName <.> "sem") (DSL.printDefinition def)
                      return (s+1, e)
              else do putStrLn $ "ERR: ignoring unknown DSL defined opcode \"" <>
                               opName <> "\" (-> " <> d <> ")"
                      return (s, e+1)
          opcodes = map show allOpcodes
