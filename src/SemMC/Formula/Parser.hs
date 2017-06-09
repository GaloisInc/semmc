{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A parser for an s-expression representation of formulas
module SemMC.Formula.Parser (
  Atom(..),
  parseLL,
  readFormula
  -- parseFormula,
  -- parseFormulaFile
  ) where

import qualified Data.Map as M
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.SCargot.Language.HaskLike (parseHaskellInt)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Parameterized.Classes (ShowF)
import Data.Parameterized.Some (Some(..))
import Data.Parameterized.NatRepr (NatRepr, someNat, isPosNat, withLeqProof, type (<=))
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import Lang.Crucible.BaseTypes
import Lang.Crucible.Solver.Symbol (emptySymbol)
import qualified Lang.Crucible.Solver.Interface as S

-- import SemMC.Formula

-- parseFormulaFile :: FilePath -> IO (Either String Formula)
-- parseFormulaFile fp = parseFormula <$> T.readFile fp

-- parseFormula :: T.Text -> Either String Formula
-- parseFormula = SC.decodeOne p

data Atom = AIdent T.Text
          | AQuoted T.Text
          | AInt Integer
          | ABV Int Integer
          deriving (Show)

parseIdent :: Parser T.Text
parseIdent = T.pack <$> ((:) <$> (letter <|> oneOf "+-=<>") <*> many (letter <|> digit <|> oneOf "+-=<>"))

parseBV :: Parser (Int, Integer)
parseBV = char '#' >> ((char 'b' >> parseBin) <|> (char 'x' >> parseHex))
  where parseBin = oneOf "10" >>= \d -> parseBin' (1, if d == '1' then 1 else 0)
        parseBin' :: (Int, Integer) -> Parser (Int, Integer)
        parseBin' (bits, x) = do
          optionMaybe (oneOf "10") >>= \case
            Just d -> parseBin' (bits + 1, x * 2 + (if d == '1' then 1 else 0))
            Nothing -> return (bits, x)
        parseHex = (\s -> (length s * 4, read ("0x" ++ s))) <$> many1 digit

parseAtom :: Parser Atom
parseAtom
  =   AIdent      <$> parseIdent
  <|> AQuoted     <$> (char '\'' >> parseIdent)
  <|> AInt         .  read <$> many1 digit
  <|> uncurry ABV <$> parseBV

parser :: SC.SExprParser Atom (SC.SExpr Atom)
parser = SC.mkParser parseAtom

parseLL :: T.Text -> Either String (SC.SExpr Atom)
parseLL = SC.decodeOne parser

readOperands :: SC.SExpr Atom -> Maybe [(T.Text, T.Text)]
readOperands SC.SNil = Just []
readOperands (SC.SAtom _) = Nothing
readOperands (SC.SCons s rest) = do
  let SC.SCons (SC.SAtom (AIdent operand)) (SC.SAtom (AQuoted ty)) = s
  rest' <- readOperands rest
  return $ (operand, ty) : rest'

data Parameter = Formal T.Text
               | Literal T.Text
               deriving (Show, Eq, Ord)

readParameter :: Atom -> Maybe Parameter
readParameter (AIdent name) = Just (Formal name)
readParameter (AQuoted name) = Just (Literal name)
readParameter (AInt _) = Nothing
readParameter (ABV _ _) = Nothing

readInputs :: SC.SExpr Atom -> Maybe [Parameter]
readInputs SC.SNil = Just []
readInputs (SC.SAtom _) = Nothing
readInputs (SC.SCons s rest) = do
  let SC.SAtom p = s
  p' <- readParameter p
  rest' <- readInputs rest
  return $ p' : rest'

-- Type of function to lookup the SymExpr for a parameter -- for use during
-- constructing the SymExpr representing each definition.
type ParameterLookup sym = Parameter -> Maybe (Some (S.SymExpr sym))

readIdent :: (Monad m) => Parameter -> SC.SExpr Atom -> ExceptT String m T.Text
readIdent def SC.SNil = throwE $ "In parsing definition of \"" ++ show def ++ "\": found nil where expected ident"
readIdent def (SC.SCons _ _) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found cons where expected ident"
readIdent def (SC.SAtom (AQuoted _)) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found quote where expected ident"
readIdent def (SC.SAtom (AInt _)) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found int where expected ident"
readIdent def (SC.SAtom (ABV _ _)) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found BV where expected ident"
readIdent     _ (SC.SAtom (AIdent ident)) = return ident

readParameter' :: (Monad m) => Parameter -> Atom -> ExceptT String m Parameter
readParameter' def (AInt _) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found int where expected parameter"
readParameter' def (ABV _ _) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found int where expected parameter"
readParameter'   _ (AIdent ident) = return (Formal ident)
readParameter'   _ (AQuoted quoted) = return (Literal quoted)

mkSomeBvLit :: (S.IsExprBuilder sym) => sym -> Int -> Integer -> IO (Some (S.SymExpr sym))
mkSomeBvLit sym len val = maybe (error "lit BV must have length >= 1") id $ do
  Some lenRepr <- someNat (toInteger len)
  pf <- isPosNat lenRepr
  return $ withLeqProof pf (Some <$> S.bvLit sym lenRepr val)

readExpr :: (S.IsExprBuilder sym)
         => sym
         -> ParameterLookup sym
         -> Parameter
         -- ^ Solely for use in error messages.
         -> SC.SExpr Atom
         -> ExceptT String IO (Some (S.SymExpr sym))
readExpr _ _ def SC.SNil = throwE $ "In parsing definition of \"" ++ show def ++ "\": found nil where expected an expression"
readExpr _ paramExpr def (SC.SAtom (AInt _)) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found int where expected an expression"
readExpr sym paramExpr def (SC.SAtom (ABV len val)) = liftIO $ mkSomeBvLit sym len val
readExpr _ paramExpr def (SC.SAtom paramRaw) = do
  param <- readParameter' def paramRaw
  case paramExpr param of
    Just expr -> return expr
    Nothing -> throwE $ "In parsing definition of \"" ++ show def ++ "\": found unknown parameter " ++ show param
readExpr sym paramExpr def (SC.SCons opRaw argsRaw) = do
  op <- readIdent def opRaw
  args <- readExprs sym paramExpr def argsRaw
  liftIO $ mkSomeBvLit sym 16 42

readExprs :: (S.IsExprBuilder sym)
          => sym
          -> ParameterLookup sym
          -> Parameter
          -- ^ Solely for use in error messages.
          -> SC.SExpr Atom
          -> ExceptT String IO [Some (S.SymExpr sym)]
readExprs _ _ _ SC.SNil = return []
readExprs _ _ def (SC.SAtom _) = throwE $ "In parsing definition of \"" ++ show def ++ "\": found atom where expected a list of expressions"
readExprs sym paramExpr def (SC.SCons e rest) = do
  e' <- readExpr sym paramExpr def e
  rest' <- readExprs sym paramExpr def rest
  return $ e' : rest'

readDefs :: (S.IsExprBuilder sym)
         => sym
         -> ParameterLookup sym
         -> SC.SExpr Atom
         -> ExceptT String IO [(Parameter, Some (S.SymExpr sym))]
readDefs _ _ SC.SNil = return []
readDefs _ _ (SC.SAtom _) = throwE $ "Found an atom where a cons cell was expected, in parsing defs"
readDefs sym paramExpr (SC.SCons s rest) = do
  (param, defRaw) <-
    case s of
      SC.SCons (SC.SAtom p) defRaw ->
        case readParameter p of
          Just param -> return (param, defRaw)
          Nothing -> throwE "invalid parameter on LHS of defs"
      _ -> throwE "top-level structure of defs is wacky"
  def <- readExpr sym paramExpr param defRaw
  rest' <- readDefs sym paramExpr rest
  return $ (param, def) : rest'

-- readDefs :: (S.IsExprBuilder sym)
--          => sym
--          -> ParameterLookup sym
--          -> SC.SExpr Atom
--          -> IO (Either String [(Parameter, Some (S.SymExpr sym))])
-- readDefs sym paramExpr e = runExceptT $ readDefs' sym paramExpr e

data Formula' sym =
  Formula'
  { formOperands :: [(T.Text, T.Text)]
  , formUses :: [Parameter]
  , formParamExprs :: [(Parameter, Some (S.SymExpr sym))]
  , formDefs :: [(Parameter, Some (S.SymExpr sym))]
  }

instance (ShowF (S.SymExpr sym)) => Show (Formula' sym) where
  show (Formula' { formOperands = ops
                 , formUses = uses
                 , formParamExprs = paramExprs
                 , formDefs = defs
                 }) = "Formula' { formOperands = " ++ show ops ++ ", formUses = " ++ show uses ++ ", formParamExprs = " ++ show paramExprs ++ ", formDefs = " ++ show defs ++ " }"

readFormula' :: (S.IsExprBuilder sym,
                 S.IsSymInterface sym)
             => sym
             -> T.Text
             -> ExceptT String IO (Formula' sym)
readFormula' sym text = do
  sexpr <- ExceptT $ return $ parseLL text
  (opsRaw, inputsRaw, defsRaw) <- case sexpr of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "operands")) ops)
      (SC.SCons (SC.SCons (SC.SAtom (AIdent "in")) inputs)
       (SC.SCons (SC.SCons (SC.SAtom (AIdent "defs")) defs)
         SC.SNil))
      -> return (ops, inputs, defs)
    _ -> throwE "invalid top-level structure"
  ops <- maybe (throwE "invalid operand structure") return (readOperands opsRaw)
  inputs <- maybe (throwE "invalid inputs structure") return (readInputs inputsRaw)
  paramExprs <- liftIO $ mapM (\p -> (p,) . Some <$> S.freshConstant sym emptySymbol (knownRepr :: BaseTypeRepr (BaseBVType 32))) inputs
  defs <- readDefs sym (flip lookup paramExprs) defsRaw
  return $ Formula' ops inputs paramExprs defs

readFormula :: (S.IsExprBuilder sym,
                S.IsSymInterface sym)
            => sym
            -> T.Text
            -> IO (Either String (Formula' sym))
readFormula sym text = runExceptT $ readFormula' sym text
