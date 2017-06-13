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
{-# LANGUAGE GADTs #-}

-- | A parser for an s-expression representation of formulas
module SemMC.Formula.Parser (
  TaggedParameter(..),
  readFormula,
  readFormulaFromFile
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.Foldable (asum)
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Parsec
import           Text.Parsec.Text (Parser)

import           Data.Parameterized.NatRepr (NatRepr, someNat, isPosNat, withLeqProof)
import           Data.Parameterized.Some (Some(..))
import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.Symbol (userSymbol, SolverSymbol)

import SemMC.Formula


-- * First pass of parsing: turning the raw text into s-expressions

data Atom = AIdent T.Text
          | AQuoted T.Text
          | AInt Integer
          | ABV Int Integer
          deriving (Show)

parseIdent :: Parser T.Text
parseIdent = T.pack <$> ((:) <$> first <*> many rest)
  where first = letter <|> oneOf "+-=<>_"
        rest = letter <|> digit <|> oneOf "+-=<>_"

parseBV :: Parser (Int, Integer)
parseBV = char '#' >> ((char 'b' >> parseBin) <|> (char 'x' >> parseHex))
  where parseBin = oneOf "10" >>= \d -> parseBin' (1, if d == '1' then 1 else 0)

        parseBin' :: (Int, Integer) -> Parser (Int, Integer)
        parseBin' (bits, x) = do
          optionMaybe (oneOf "10") >>= \case
            Just d -> parseBin' (bits + 1, x * 2 + (if d == '1' then 1 else 0))
            Nothing -> return (bits, x)

        parseHex = (\s -> (length s * 4, read ("0x" ++ s))) <$> many1 hexDigit

parseAtom :: Parser Atom
parseAtom
  =   AIdent      <$> parseIdent
  <|> AQuoted     <$> (char '\'' >> parseIdent)
  <|> AInt         .  read <$> many1 digit
  <|> uncurry ABV <$> parseBV

parserLL :: SC.SExprParser Atom (SC.SExpr Atom)
parserLL = SC.mkParser parseAtom

parseLL :: T.Text -> Either String (SC.SExpr Atom)
parseLL = SC.decodeOne parserLL

-- * Second pass of parsing: turning the s-expressions into symbolic expressions
-- and the overall templated formula

-- | Utility function for contextualizing errors. Prepends the given prefix
-- whenever an error is thrown.
prefixError :: (Monoid e, MonadError e m) => e -> m a -> m a
prefixError prefix act = catchError act (throwError . mappend prefix)

-- | Parses the operands part of the semantics definition. Each operand has both
-- a name and a (quoted) type in a dotted pair. For example:
--
-- > ((ra . 'Gprc)
-- >  (n . 'Imm16)
-- >  (rt . 'Gprc))
--
readOperands :: SC.SExpr Atom -> Maybe [(T.Text, T.Text)]
readOperands SC.SNil = Just []
readOperands (SC.SAtom _) = Nothing
readOperands (SC.SCons s rest) = do
  let SC.SCons (SC.SAtom (AIdent operand)) (SC.SAtom (AQuoted ty)) = s
  rest' <- readOperands rest
  return $ (operand, ty) : rest'

-- | Parses a parameter.
readParameter :: (MonadError String m) => Atom -> m Parameter
readParameter (AIdent name)
  | Right _ <- userSymbol ("op" ++ T.unpack name) = return (Operand name)
  | otherwise = throwError $ T.unpack name ++ " is not a valid parameter name"
readParameter (AQuoted name)
  | Right _ <- userSymbol ("lit" ++ T.unpack name) = return (Literal name)
  | otherwise = throwError $ T.unpack name ++ " is not a valid parameter name"
readParameter a = throwError $ "expected parameter, found " ++ show a

-- | Parses the input list, e.g., @(ra rb 'ca)@
readInputs :: (MonadError String m) => SC.SExpr Atom -> m [Parameter]
readInputs SC.SNil = return []
readInputs (SC.SCons (SC.SAtom p) rest) = do
  p' <- readParameter p
  rest' <- readInputs rest
  return $ p' : rest'
readInputs _ = throwError "malformed input list"

-- | "Global" data stored in the Reader monad throughout most of the parsing.
data ParameterInfo sym = ParameterInfo
                         { getSym :: sym
                         -- ^ SymInterface/ExprBuilder used to build up symbolic
                         -- expressions while parsing the definitions.
                         , getLookup :: Parameter -> Maybe (Some (S.SymExpr sym))
                         -- ^ Function used to retrieve the expression
                         -- corresponding to a given parameter.
                         }

-- | Stores a NatRepr along with proof that its type parameter is a bitvector of
-- that length.
data BVProof tp where
  BVProof :: forall n tp . (1 <= n) => NatRepr n -> tp :~: BaseBVType n -> BVProof tp

-- | Given an expression, monadically either returns proof that it is a
-- bitvector or throws an error.
getBVProof :: (S.IsExpr ex, MonadError String m) => ex tp -> m (BVProof tp)
getBVProof expr =
  case S.exprType expr of
    BaseBVRepr n -> return $ BVProof n Refl
    t -> throwError $ "expected BV, found " ++ show t

-- | Type of the various different handlers for building up expressions formed
-- by applying arguments to some function.
type ExprParser sym m = (S.IsExprBuilder sym,
                         MonadError String m,
                         MonadReader (ParameterInfo sym) m,
                         MonadIO m)
                      => SC.SExpr Atom
                      -> [Some (S.SymExpr sym)]
                      -> m (Maybe (Some (S.SymExpr sym)))

-- | Parse an expression of the form @(concat x y)@.
readConcat :: ExprParser sym m
readConcat (SC.SAtom (AIdent "concat")) args =
  prefixError ("in reading concat expression: ") $ do
    when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
    sym <- reader getSym
    Some arg1 <- return $ head args
    Some arg2 <- return $ head (tail args)
    BVProof _ Refl <- prefixError ("in arg 1: ") $ getBVProof arg1
    BVProof _ Refl <- prefixError ("in arg 2: ") $ getBVProof arg2
    liftIO (Just . Some <$> S.bvConcat sym arg1 arg2)
readConcat _ _ = return Nothing

-- | Try converting an 'Integer' to a 'NatRepr' or throw an error if not
-- possible.
intToNatM :: (MonadError String m) => Integer -> m (Some NatRepr)
intToNatM = maybe (throwError "integer must be non-negative to be a nat") return . someNat

-- | Parse an expression of the form @((_ extract i j) x)@.
readExtract :: ExprParser sym m
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
  BVProof lenNat Refl <- getBVProof arg
  idxCheck <- maybe (throwError "invalid extract for given bitvector") return $
    testLeq (addNat idxNat nNat) lenNat
  liftIO $ withLeqProof nPositive $ withLeqProof idxCheck $
    (Just <$> Some <$> S.bvSelect sym idxNat nNat arg)
readExtract _ _ = return Nothing

-- | Parse an expression of the form @((_ zero_extend i) x)@ or @((_ sign_extend i) x)@.
readExtend :: ExprParser sym m
readExtend (SC.SCons (SC.SAtom (AIdent "_"))
             (SC.SCons (SC.SAtom (AIdent extend))
               (SC.SCons (SC.SAtom (AInt iInt))
                SC.SNil)))
           args
  | extend == "zero_extend" ||
    extend == "sign_extend" = prefixError ("in reading " ++ T.unpack extend ++ " expression: ") $ do
      when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
      sym <- reader getSym
      Some iNat <- intToNatM iInt
      iPositive <- maybe (throwError "must extend by a positive length") return $ isPosNat iNat
      Some arg <- return $ head args
      -- ^ required to be bound (rather then let-destructured) so GHC's brain doesn't explode
      BVProof lenNat Refl <- getBVProof arg
      let newLen = addNat lenNat iNat
      liftIO $ withLeqProof (leqAdd2 (leqRefl lenNat) iPositive) $
        let op = if extend == "zero_extend" then S.bvZext else S.bvSext
        in Just <$> Some <$> op sym newLen arg
readExtend _ _ = return Nothing

-- | Encapsulating type for a unary operation that takes one bitvector and
-- returns another (in IO).
data BVUnop sym where
  BVUnop :: (forall w . (1 <= w) => sym -> S.SymBV sym w -> IO (S.SymBV sym w)) -> BVUnop sym

-- | Look up a unary bitvector operation by name.
bvUnop :: (S.IsExprBuilder sym) => T.Text -> Maybe (BVUnop sym)
bvUnop "bvneg" = Just $ BVUnop S.bvNeg
bvUnop "bvnot" = Just $ BVUnop S.bvNotBits
bvUnop       _ = Nothing

-- | Parse an expression of the form @(f x)@, where @f@ operates on bitvectors.
readBVUnop :: forall sym m . ExprParser sym m
readBVUnop (SC.SAtom (AIdent ident)) args
  | Just (BVUnop op) :: Maybe (BVUnop sym) <- bvUnop ident =
      prefixError ("in reading " ++ T.unpack ident ++ " expression: ") $ do
        when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
        sym <- reader getSym
        Some expr <- return $ head args
        BVProof _ Refl <- getBVProof expr
        liftIO (Just . Some <$> op sym expr)
readBVUnop _ _ = return Nothing

-- | Encapsulating type for a binary operation that takes two bitvectors of the
-- same length.
data BVBinop sym where
  -- | Binop with a bitvector return type, e.g., addition or bitwise operations.
  BinopBV :: (forall w . (1 <= w) => sym -> S.SymBV sym w -> S.SymBV sym w -> IO (S.SymBV sym w)) -> BVBinop sym

  -- | Binop with a boolean return type, i.e., comparison operators.
  BinopBool :: (forall w . (1 <= w) => sym -> S.SymBV sym w -> S.SymBV sym w -> IO (S.Pred sym)) -> BVBinop sym

-- | Look up a binary bitvector operation by name.
bvBinop :: (S.IsExprBuilder sym) => T.Text -> Maybe (BVBinop sym)
bvBinop "bvand"  = Just $ BinopBV S.bvAndBits
bvBinop "bvor"   = Just $ BinopBV S.bvOrBits
bvBinop "bvadd"  = Just $ BinopBV S.bvAdd
bvBinop "bvmul"  = Just $ BinopBV S.bvMul
bvBinop "bvudiv" = Just $ BinopBV S.bvUdiv
bvBinop "bvurem" = Just $ BinopBV S.bvUrem
bvBinop "bvshl"  = Just $ BinopBV S.bvShl
bvBinop "bvlshr" = Just $ BinopBV S.bvLshr
bvBinop "bvnand" = Just $ BinopBV $ \sym arg1 arg2 -> S.bvNotBits sym =<< S.bvAndBits sym arg1 arg2
bvBinop "bvnor"  = Just $ BinopBV $ \sym arg1 arg2 -> S.bvNotBits sym =<< S.bvOrBits sym arg1 arg2
bvBinop "bvxor"  = Just $ BinopBV S.bvXorBits
bvBinop "bvxnor" = Just $ BinopBV $ \sym arg1 arg2 -> S.bvNotBits sym =<< S.bvXorBits sym arg1 arg2
bvBinop "bvsub"  = Just $ BinopBV S.bvSub
bvBinop "bvsdiv" = Just $ BinopBV S.bvSdiv
bvBinop "bvsrem" = Just $ BinopBV S.bvSrem
bvBinop "bvsmod" = error "bvsmod is not implemented"
bvBinop "bvashr" = Just $ BinopBV S.bvAshr
bvBinop "bvult"  = Just $ BinopBool S.bvUlt
bvBinop "bvule"  = Just $ BinopBool S.bvUle
bvBinop "bvugt"  = Just $ BinopBool S.bvUgt
bvBinop "bvuge"  = Just $ BinopBool S.bvUge
bvBinop "bvslt"  = Just $ BinopBool S.bvSlt
bvBinop "bvsle"  = Just $ BinopBool S.bvSle
bvBinop "bvsgt"  = Just $ BinopBool S.bvSgt
bvBinop "bvsge"  = Just $ BinopBool S.bvSge
bvBinop        _ = Nothing

-- | Parse an expression of the form @(f x y)@, where @f@ is a binary operation
-- on bitvectors.
readBVBinop :: forall sym m . ExprParser sym m
readBVBinop (SC.SAtom (AIdent ident)) args
  | Just op :: Maybe (BVBinop sym) <- bvBinop ident =
      prefixError ("in reading " ++ T.unpack ident ++ " expression: ") $ do
        when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
        sym <- reader getSym
        Some arg1 <- return $ head args
        Some arg2 <- return $ head (tail args)
        BVProof m Refl <- prefixError ("in arg 1: ") $ getBVProof arg1
        BVProof n Refl <- prefixError ("in arg 2: ") $ getBVProof arg2
        case testEquality m n of
          Just Refl -> liftIO $ Just <$>
            case op of
              BinopBV op' -> Some <$> op' sym arg1 arg2
              BinopBool op' -> Some <$> op' sym arg1 arg2
          Nothing -> throwError $ "arguments to " ++ T.unpack ident ++
            " must be the same length, but arg 1 has length " ++ show m ++
            " and arg 2 has length " ++ show n
readBVBinop _ _ = return Nothing

-- | Parse an expression of the form @(= x y)@.
readEq :: ExprParser sym m
readEq (SC.SAtom (AIdent "=")) args =
  prefixError ("in reading '=' expression: ") $ do
    when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
    sym <- reader getSym
    Some arg1 <- return $ head args
    Some arg2 <- return $ head (tail args)
    case testEquality (S.exprType arg1) (S.exprType arg2) of
      Just Refl -> liftIO (Just . Some <$> S.isEq sym arg1 arg2)
      Nothing -> throwError $ "arguments must have same types; instead, got for arg 1 " ++
        show (S.exprType arg1) ++ " and for arg 2 " ++ show (S.exprType arg2)
readEq _ _ = return Nothing

-- | Parse an expression of the form @(ite b x y)@
readIte :: ExprParser sym m
readIte (SC.SAtom (AIdent "ite")) args =
  prefixError ("in reading ite expression: ") $ do
    when (length args /= 3) (throwError $ "expecting 3 arguments, got " ++ show (length args))
    sym <- reader getSym
    Some test <- return $ args !! 0
    Some then_ <- return $ args !! 1
    Some else_ <- return $ args !! 2
    case S.exprType test of
      BaseBoolRepr ->
        case testEquality (S.exprType then_) (S.exprType else_) of
          Just Refl -> liftIO (Just . Some <$> S.baseTypeIte sym test then_ else_)
          Nothing -> throwError $ "then and else branches must have same type; got " ++
            show (S.exprType then_) ++ " for then and " ++ show (S.exprType else_) ++ " for else"
      _ -> throwError "test expression must be a boolean"
readIte _ _ = return Nothing

-- | Parse an arbitrary expression.
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
  Just (Some lenRepr) <- return $ someNat (toInteger len)
  let Just pf = isPosNat lenRepr
  -- ^ The above two patterns should never fail, given that during parsing we
  -- can only construct BVs with positive length.
  liftIO $ withLeqProof pf (Some <$> S.bvLit sym lenRepr val)
readExpr (SC.SAtom paramRaw) = do
  param <- readParameter paramRaw
  paramExpr <- reader getLookup
  case paramExpr param of
    Just expr -> return expr
    Nothing -> throwError $ "found unknown parameter " ++ show param
readExpr (SC.SCons opRaw argsRaw) = do
  args <- readExprs argsRaw
  parseTries <- sequence $ map (\f -> f opRaw args)
    [readConcat, readExtract, readExtend, readBVUnop, readBVBinop, readEq, readIte]
  case asum parseTries of
    Just expr -> return expr
    Nothing -> throwError $ "couldn't parse expression " ++ show opRaw

-- | Parse multiple expressions in a list.
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

-- | Parse the whole definitions expression, e.g.:
--
-- > ((rt . (bvadd ra rb))
-- >  ('ca . #b1))
--
readDefs :: (S.IsExprBuilder sym,
             Monad m,
             MonadError String m,
             MonadReader (ParameterInfo sym) m,
             MonadIO m)
         => SC.SExpr Atom
         -> m [(Parameter, Some (S.SymExpr sym))]
readDefs SC.SNil = return []
readDefs (SC.SCons (SC.SCons (SC.SAtom p) defRaw) rest) = do
  param <- readParameter p
  def <- readExpr defRaw
  rest' <- readDefs rest
  return $ (param, def) : rest'
readDefs _ = throwError "invalid defs structure"

-- | Parameter where a formal parameter also has what type it must be.
data TaggedParameter = TaggedOperand T.Text T.Text
                     | TaggedLiteral T.Text
                     deriving (Show)

parameterToTagged :: [(T.Text, T.Text)] -> Parameter -> Maybe TaggedParameter
parameterToTagged m (Operand name) = TaggedOperand name <$> lookup name m
parameterToTagged _ (Literal name) = Just $ TaggedLiteral name

parameterSymbol :: Parameter -> SolverSymbol
parameterSymbol (Operand name)
  | Right symbol <- userSymbol ("op" ++ T.unpack name) = symbol
parameterSymbol (Literal name)
  | Right symbol <- userSymbol ("lit" ++ T.unpack name) = symbol
parameterSymbol _ = error "parameters should only be valid symbols"
                    -- ^ This invariant should be checked in readParameter

-- | Parse the whole definition of a templated formula, inside an appropriate
-- monad.
readFormula' :: (S.IsExprBuilder sym,
                 S.IsSymInterface sym,
                 MonadError String m,
                 MonadIO m)
             => sym
             -> (TaggedParameter -> Maybe (Some BaseTypeRepr))
             -> T.Text
             -> m (TemplatedFormula sym)
readFormula' sym typer text = do
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
  inputs <- readInputs inputsRaw
  let makeVar param
        | Just tagged <- parameterToTagged ops param =
            case typer tagged of
              Just (Some tp) -> liftIO (Some <$> S.freshConstant sym (parameterSymbol param) tp)
              Nothing -> throwError $ "could not determine the appropriate type of " ++ show param
        | otherwise = throwError $ show param ++ " is listed as an input, but it is not an operand"
  paramExprs <- mapM (\p -> (p,) <$> makeVar p) inputs
  defs <- runReaderT (readDefs defsRaw) $ ParameterInfo sym (flip lookup paramExprs)
  return $ TemplatedFormula ops inputs paramExprs defs

-- | Parse the definition of a templated formula.
readFormula :: (S.IsExprBuilder sym,
                S.IsSymInterface sym)
            => sym
            -> (TaggedParameter -> Maybe (Some BaseTypeRepr))
            -> T.Text
            -> IO (Either String (TemplatedFormula sym))
readFormula sym typer text = runExceptT $ readFormula' sym typer text

-- | Read a templated formula definition from file, then parse it.
readFormulaFromFile :: (S.IsExprBuilder sym,
                        S.IsSymInterface sym)
                    => sym
                    -> (TaggedParameter -> Maybe (Some BaseTypeRepr))
                    -> FilePath
                    -> IO (Either String (TemplatedFormula sym))
readFormulaFromFile sym typer fp = readFormula sym typer =<< T.readFile fp
