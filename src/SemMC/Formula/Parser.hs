{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A parser for an s-expression representation of formulas
module SemMC.Formula.Parser (
  Atom(..),
  parseLL,
  readFormula
  -- parseFormula,
  -- parseFormulaFile
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.Foldable (asum)
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Text (Parser)

import           Data.Parameterized.NatRepr (NatRepr, someNat, isPosNat, withLeqProof)
import           Data.Parameterized.Some (Some(..))
import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.Symbol (emptySymbol)

import SemMC.Formula

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
parseIdent = T.pack <$> ((:) <$> (letter <|> oneOf "+-=<>_") <*> many (letter <|> digit <|> oneOf "+-=<>_"))

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

-- type SParsec m = ParsecT (SC.SExpr Atom) () m

-- instance (Monad m) => Stream (SC.SExpr Atom) m (SC.SExpr Atom) where
--   uncons SC.SNil = return Nothing
--   uncons s@(SC.SAtom _) = return $ Just (s, SC.SNil)
--   uncons (SC.SCons hd tl) = return $ Just (hd, tl)

readOperands :: SC.SExpr Atom -> Maybe [(T.Text, T.Text)]
readOperands SC.SNil = Just []
readOperands (SC.SAtom _) = Nothing
readOperands (SC.SCons s rest) = do
  let SC.SCons (SC.SAtom (AIdent operand)) (SC.SAtom (AQuoted ty)) = s
  rest' <- readOperands rest
  return $ (operand, ty) : rest'

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

data ParameterInfo sym = ParameterInfo
                         { getSym :: sym
                         , getLookup :: Parameter -> Maybe (Some (S.SymExpr sym))
                         }

-- Why is the error type a list? So that (<|>) will show the errors for all the
-- possibilities, rather than just the first or last.
-- type ParameterM sym = ReaderT (ParameterInfo sym) (ExceptT [String] IO)

-- -- Type of function to lookup the SymExpr for a parameter -- for use during
-- -- constructing the SymExpr representing each definition.
-- type ParameterLookup sym = Parameter -> Maybe (Some (S.SymExpr sym))

readIdent :: (Monad m, MonadError String m) => SC.SExpr Atom -> m T.Text
readIdent SC.SNil = throwError "found nil where expected ident"
readIdent (SC.SCons _ _) = throwError "found cons where expected ident"
readIdent (SC.SAtom (AQuoted _)) = throwError "found quote where expected ident"
readIdent (SC.SAtom (AInt _)) = throwError "found int where expected ident"
readIdent (SC.SAtom (ABV _ _)) = throwError "found BV where expected ident"
readIdent (SC.SAtom (AIdent ident)) = return ident

readParameter' :: (Monad m, MonadError String m) => Atom -> m Parameter
readParameter' (AInt _) = throwError "found int where expected parameter"
readParameter' (ABV _ _) = throwError "found int where expected parameter"
readParameter' (AIdent ident) = return (Formal ident)
readParameter' (AQuoted quoted) = return (Literal quoted)

mkSomeBvLit :: (S.IsExprBuilder sym) => sym -> Int -> Integer -> IO (Some (S.SymExpr sym))
mkSomeBvLit sym len val = maybe (error "lit BV must have length >= 1") id $ do
  Some lenRepr <- someNat (toInteger len)
  pf <- isPosNat lenRepr
  return $ withLeqProof pf (Some <$> S.bvLit sym lenRepr val)

prefixError :: (Monoid e, MonadError e m) => e -> m a -> m a
prefixError prefix act = catchError act (throwError . mappend prefix)

-- getBVProof :: (S.IsExpr ex,
--               MonadError String m)
--            => ex tp
--            -> m (tp :~: BaseBVType )

getBVLengthM :: (S.IsExpr ex,
                 MonadError String m)
             => Some ex
             -> m (Some NatRepr)
getBVLengthM (Some expr) = do
  case S.exprType expr of
    BaseBVRepr lenRepr -> return $ Some lenRepr
    t -> throwError $ "expected BV expression, found " ++ show t

intToNatM :: (MonadError String m) => Integer -> m (Some NatRepr)
intToNatM = maybe (throwError "integer must be non-negative to be a nat") return . someNat

readExtract :: (S.IsExprBuilder sym,
                MonadReader (ParameterInfo sym) m,
                MonadError String m,
                MonadIO m)
            => SC.SExpr Atom
            -> [Some (S.SymExpr sym)]
            -> m (Maybe (Some (S.SymExpr sym)))
readExtract (SC.SCons (SC.SAtom (AIdent "_"))
             (SC.SCons (SC.SAtom (AIdent "extract"))
              (SC.SCons (SC.SAtom (AInt iInt))
               (SC.SCons (SC.SAtom (AInt jInt))
                SC.SNil))))
            args = prefixError "in reading extract expression: " $ do
  when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
  sym <- reader getSym
  let nInt = iInt - jInt + 1
      idxInt = jInt
  Some nNat <- prefixError "in calculating extract length: " $ intToNatM nInt
  Some idxNat <- prefixError "in extract lower bound: " $ intToNatM idxInt
  nPositive <- maybe (throwError "extract length must be positive") return $ isPosNat nNat
  Some arg <- return $ head args
  -- ^ required to be bound (rather then let-destructured) so GHC's brain doesn't explode
  -- Some mNat <- getBVLengthM arg
  case S.exprType arg of
    BaseBVRepr lenNat -> do
      idxCheck <- maybe (throwError "invalid extract for given bitvector") return $
        testLeq (addNat idxNat nNat) lenNat
      -- finally!
      liftIO $ withLeqProof nPositive $ withLeqProof idxCheck $
        (Just <$> Some <$> S.bvSelect sym idxNat nNat arg)
    t -> throwError $ "expected BV expression, found " ++ show t
readExtract _ _ = return Nothing

readExtend :: (S.IsExprBuilder sym,
               MonadError String m,
               MonadReader (ParameterInfo sym) m,
               MonadIO m)
           => SC.SExpr Atom
           -> [Some (S.SymExpr sym)]
           -> m (Maybe (Some (S.SymExpr sym)))
readExtend (SC.SCons (SC.SAtom (AIdent "_"))
             (SC.SCons (SC.SAtom (AIdent extend))
               (SC.SCons (SC.SAtom (AInt iInt))
                SC.SNil)))
           args
  | extend == "zero_extend" ||
    extend == "sign_extend" = prefixError ("in reading " ++ T.unpack extend ++ " expression: ") $ do
      -- let op = if extend == "zero_extend" then S.bvZext else S.bvSext
      when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
      sym <- reader getSym
      Some iNat <- intToNatM iInt
      iPositive <- maybe (throwError "must extend by a positive length") return $ isPosNat iNat
      Some arg <- return $ head args
      -- ^ required to be bound (rather then let-destructured) so GHC's brain doesn't explode
      case S.exprType arg of
        BaseBVRepr lenNat ->
          let newLen = addNat lenNat iNat
          in liftIO $ withLeqProof (leqAdd2 (leqRefl lenNat) iPositive) $
               let op = if extend == "zero_extend" then S.bvZext else S.bvSext
               in Just <$> Some <$> op sym newLen arg
        t -> throwError $ "expected BV expression, found " ++ show t
  | otherwise = return Nothing
readExtend _ _ = return Nothing

-- withBVM :: (S.IsExpr ex,
--             MonadError String m)
--         => Some ex
--         -> (forall w . (1 <= w) => ex (BaseBVType w) -> NatRepr w -> m a)
--         -> m a
-- withBVM (Some expr) f =
--   case S.exprType expr of
--     BaseBVRepr (lenNat :: NatRepr w) -> f (expr :: ex (BaseBVType w)) lenNat
--     _ -> throwError $ "expected BV expression, found " ++ show (S.exprType expr)

-- bvUnop :: forall sym . T.Text -> Maybe (forall w . (1 <= w) => sym -> S.SymBV sym w -> IO (S.SymBV sym w))
-- ^ That's the type signature I want. Unfortunately, due to GHC's lack of
-- impredicative polymorphism, I can't write that...
bvUnop :: T.Text -> Bool
bvUnop "bvneg" = True
bvUnop "bvnot" = True
bvUnop       _ = False

readBVUnop :: (S.IsExprBuilder sym,
               MonadError String m,
               MonadReader (ParameterInfo sym) m,
               MonadIO m)
           => SC.SExpr Atom
           -> [Some (S.SymExpr sym)]
           -> m (Maybe (Some (S.SymExpr sym)))
readBVUnop (SC.SAtom (AIdent ident)) args
  | bvUnop ident = prefixError ("in reading bvnot expression: ") $ do
      when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
      sym <- reader getSym
      Some expr <- return $ head args
      case S.exprType expr of
        BaseBVRepr _ ->
          let op = case ident of
                "bvneg" -> S.bvNeg
                "bvnot" -> S.bvNotBits
                _ -> undefined
          in liftIO (Just . Some <$> op sym expr)
        t -> throwError $ "expected BV expression, found " ++ show t
readBVUnop _ _ = return Nothing

readExpr :: (S.IsExprBuilder sym,
             Monad m,
             MonadError String m,
             MonadReader (ParameterInfo sym) m,
             MonadIO m)
         => SC.SExpr Atom
         -> m (Some (S.SymExpr sym))
readExpr SC.SNil = throwError "found nil where expected an expression"
readExpr (SC.SAtom (AInt _)) = throwError "found int where expected an expression"
readExpr (SC.SAtom (ABV len val)) = do
  sym <- reader getSym
  liftIO $ mkSomeBvLit sym len val
readExpr (SC.SAtom paramRaw) = do
  param <- readParameter' paramRaw
  paramExpr <- reader getLookup
  case paramExpr param of
    Just expr -> return expr
    Nothing -> throwError $ "found unknown parameter " ++ show param
readExpr (SC.SCons opRaw argsRaw) = do
  args <- readExprs argsRaw
  parseTries <- sequence $ map (\f -> f opRaw args)
    -- [readExtract, readExtend, readRotate, readUnop, readBinop, readTriop]
    [readExtract, readExtend, readBVUnop]
  case asum parseTries of
    Just expr -> return expr
    Nothing -> throwError $ "couldn't parse expression"

readExprs :: (S.IsExprBuilder sym,
              Monad m,
              MonadError String m,
              MonadReader (ParameterInfo sym) m,
              MonadIO m)
          => SC.SExpr Atom
          -> m [Some (S.SymExpr sym)]
readExprs SC.SNil = return []
readExprs (SC.SAtom _) = throwError "found atom where expected a list of expressions"
readExprs (SC.SCons e rest) = do
  e' <- readExpr e
  rest' <- readExprs rest
  return $ e' : rest'

readDefs :: (S.IsExprBuilder sym,
             Monad m,
             MonadError String m,
             MonadReader (ParameterInfo sym) m,
             MonadIO m)
         => SC.SExpr Atom
         -> m [(Parameter, Some (S.SymExpr sym))]
readDefs SC.SNil = return []
readDefs (SC.SAtom _) = throwError "Found an atom where a cons cell was expected, in parsing defs"
readDefs (SC.SCons s rest) = do
  (param, defRaw) <-
    case s of
      SC.SCons (SC.SAtom p) defRaw ->
        case readParameter p of
          Just param -> return (param, defRaw)
          Nothing -> throwError "invalid parameter on LHS of defs"
      _ -> throwError "top-level structure of defs is wacky"
  def <- readExpr defRaw
  rest' <- readDefs rest
  return $ (param, def) : rest'

readFormula' :: (S.IsExprBuilder sym,
                 S.IsSymInterface sym,
                 MonadError String m,
                 MonadIO m)
             => sym
             -> T.Text
             -> m (Formula sym)
readFormula' sym text = do
  sexpr <- case parseLL text of
             Left err -> throwError err
             Right res -> return res
  (opsRaw, inputsRaw, defsRaw) <- case sexpr of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "operands")) ops)
      (SC.SCons (SC.SCons (SC.SAtom (AIdent "in")) inputs)
       (SC.SCons (SC.SCons (SC.SAtom (AIdent "defs")) defs)
         SC.SNil))
      -> return (ops, inputs, defs)
    _ -> throwError "invalid top-level structure"
  ops <- maybe (throwError "invalid operand structure") return (readOperands opsRaw)
  inputs <- maybe (throwError "invalid inputs structure") return (readInputs inputsRaw)
  paramExprs <- liftIO $ mapM (\p -> (p,) . Some <$> S.freshConstant sym emptySymbol (knownRepr :: BaseTypeRepr (BaseBVType 32))) inputs
  defs <- runReaderT (readDefs defsRaw) $ ParameterInfo sym (flip lookup paramExprs)
  return $ Formula ops inputs paramExprs defs

readFormula :: (S.IsExprBuilder sym,
                S.IsSymInterface sym)
            => sym
            -> T.Text
            -> IO (Either String (Formula sym))
readFormula sym text = runExceptT $ readFormula' sym text
