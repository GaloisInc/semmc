-- | Tool to run a 'cmp' of two semantics files to see if they are
-- equivalent.  At present, this requires full equivalence without
-- renaming, but comparison can be made over whitespace and
-- let-binding changes.

module Main where

import           Data.Semigroup
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import qualified Options.Applicative.Help as OH
import           System.Exit

import           What4.Serialize.Parser ( parseSExpr )

import           Prelude


data Options = Options { fileA :: FilePath
                       , fileB :: FilePath
                       }

optionsParser :: O.Parser Options
optionsParser = Options
                <$> O.strArgument ( O.metavar "FILE_A"
                                  <> O.help "Original semantics file")
                <*> O.strArgument ( O.metavar "FILE_B"
                                  <> O.help "Newer semantics file")


main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = (O.info (optionsParser O.<**> O.helper)
                 ( O.fullDesc
                 <> O.header "cmpsem - Compare two semantics files for equivalence."
                 )) { O.infoProgDesc = OH.vsepChunks
                      [ OH.paragraph
                        "Compares two semantics definition files for equivalence.\
                        \ Ignores comments, whitespace changes, and let-binding, but\
                        \ the (non-let) variable names must be the same.  Equivalent \
                        \ files should define identical semantics."
                      , OH.paragraph
                        "Prints a message regarding the equivalence and exits with\
                        \ a zero value if the files are equivalent or a non-zero\
                        \ exit value if they are different.  Also returns non-zero\
                        \ if either file cannot be parsed."
                      ]
                    }


mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
  atxt <- TIO.readFile (fileA opts)
  btxt <- TIO.readFile (fileB opts)
  let same = do a <- parseSExpr atxt
                b <- parseSExpr btxt
                return $ a == b
      (msg, rval) = case same of
                      Right True -> ("semantically equivalent.", ExitSuccess)
                      Right False -> ("DIFFERENT", ExitFailure 1)
                      Left e -> ("unparseable: " <> e, ExitFailure 2)
  putStrLn $ "Files " <> fileA opts <> " and " <> fileB opts <> " are " <> msg
  exitWith rval
