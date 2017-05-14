-- | A parser for an s-expression representation of formulas
module SemMC.Formula.Parser (
  parseFormula,
  parseFormulaFile
  ) where

import qualified Data.SCargot as SC
import qualified Data.Text as T
import qualified Data.Text.IO as T

import SemMC.Formula

parseFormulaFile :: FilePath -> IO (Either String Formula)
parseFormulaFile fp = parseFormula <$> T.readFile fp

parseFormula :: T.Text -> Either String Formula
parseFormula = SC.decodeOne p

p :: SC.SExprParser atom Formula
p = undefined
