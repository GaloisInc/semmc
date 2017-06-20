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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A parser for an s-expression representation of formulas
module SemMC.Formula.Parser (
  readFormula,
  readFormulaFromFile
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Reader
import           Data.Foldable ( asum, foldrM )
import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Parsec
import           Text.Parsec.Text ( Parser )
import qualified Data.Set as Set
import           GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import           Data.Proxy

import           Data.Parameterized.NatRepr ( NatRepr, someNat, isPosNat, withLeqProof )
import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.Symbol ( userSymbol )

import           SemMC.Formula
import           SemMC.Architecture

import           Dismantle.Instruction ( OperandList(..), traverseOperandList )

-- * First pass of parsing: turning the raw text into s-expressions

data Atom = AIdent String
          | AQuoted String
          | AInt Integer
          | ABV Int Integer
          deriving (Show)

parseIdent :: Parser String
parseIdent = (:) <$> first <*> many rest
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

data OpData (arch :: *) (s :: Symbol) = OpData String (BaseTypeRepr (OperandType arch s))

class BuildOperandList (arch :: *) (tps :: [Symbol]) where
  -- | Parses the operands part of the semantics definition. Each operand has both
  -- a name and a (quoted) type in a dotted pair. For example:
  --
  -- > ((ra . 'Gprc)
  -- >  (n . 'Imm16)
  -- >  (rt . 'Gprc))
  --
  buildOperandList :: SC.SExpr Atom -> Maybe (OperandList (OpData arch) tps)

instance BuildOperandList arch '[] where
  buildOperandList SC.SNil = Just Nil
  buildOperandList       _ = Nothing

-- externally defined to work around typeck limitations
bolCons :: forall arch tp tps.
           (KnownSymbol tp,
            KnownRepr BaseTypeRepr (OperandType arch tp),
            BuildOperandList arch tps)
        => SC.SExpr Atom
        -> Maybe (OperandList (OpData arch) (tp ': tps))
bolCons SC.SNil = Nothing
bolCons (SC.SAtom _) = Nothing
bolCons (SC.SCons s rest) = do
  let SC.SCons (SC.SAtom (AIdent operand)) (SC.SAtom (AQuoted ty)) = s
  when (symbolVal (Proxy :: Proxy tp) /= ty) Nothing
  rest' <- buildOperandList rest
  let repr = knownRepr :: BaseTypeRepr (OperandType arch tp)
  -- let repr = undefined
  return $ (OpData operand repr) :> rest'

instance (KnownSymbol tp, KnownRepr BaseTypeRepr (OperandType arch tp), BuildOperandList arch tps) => BuildOperandList arch (tp ': tps) where
  buildOperandList = bolCons

-- readOperands' :: SC.SExpr Atom
--               -> Maybe (Some (OperandList (Const T.Text)))
-- readOperands' SC.SNil = 
--   case testEquality p (Proxy :: Proxy '[]) of
--     Just Refl -> Just Nil
--     Nothing -> Nothing
-- readOperands' _ _ = undefined
-- readOperands' size (SC.SCons s rest) = do
--   let SC.SCons (SC.SAtom (AIdent operand)) (SC.SAtom (AQuoted ty)) = s
--   Ctx.IncSize size' <- return $ Ctx.viewSize size
--   rest' <- readOperands' size' rest
--   return $ Ctx.extend rest' (Const operand)
-- readOperands' _ _ = Nothing

data RawParameter = RawOperand String
                  | RawLiteral String
                  deriving (Show, Eq, Ord)

-- | Parses a parameter.
readRawParameter :: (MonadError String m) => Atom -> m RawParameter
readRawParameter (AIdent name)
  | Right _ <- userSymbol ("op" ++ name) = return (RawOperand name)
  | otherwise = throwError $ name ++ " is not a valid parameter name"
readRawParameter (AQuoted name)
  | Right _ <- userSymbol ("lit" ++ name) = return (RawLiteral name)
  | otherwise = throwError $ name ++ " is not a valid parameter name"
readRawParameter a = throwError $ "expected parameter, found " ++ show a

data IndexWithType arch sh s where
  IndexWithType :: BaseTypeRepr (OperandType arch s) -> Index sh s -> IndexWithType arch sh s

findOpListIndex :: String -> OperandList (OpData arch) sh -> Maybe (Some (IndexWithType arch sh))
findOpListIndex _ Nil = Nothing
findOpListIndex x ((OpData name tpRepr) :> rest)
  | x == name = Just $ Some (IndexWithType tpRepr IndexHere)
  | otherwise = mapSome (\(IndexWithType tpRepr' idx) -> IndexWithType tpRepr' (IndexThere idx)) <$> findOpListIndex x rest

readParameter :: (MonadError String m, Architecture arch) => OperandList (OpData arch) sh -> Atom -> m (Some (Parameter arch sh))
readParameter oplist atom =
  readRawParameter atom >>= \case
    RawOperand op ->
      maybe (throwError $ "couldn't find operand " ++ show op)
            (return . viewSome (\(IndexWithType tpRepr idx) -> Some $ Operand tpRepr idx))
            (findOpListIndex op oplist)
    RawLiteral lit ->
      maybe (throwError $ show lit ++ " is an invalid literal for this arch")
            (\(StateVarDesc repr var) -> return $ Some (Literal repr var))
            (readStateVar (lit))

-- | Parses the input list, e.g., @(ra rb 'ca)@
readInputs :: (MonadError String m, Architecture arch) => OperandList (OpData arch) sh -> SC.SExpr Atom -> m [Some (Parameter arch sh)]
readInputs _ SC.SNil = return []
readInputs oplist (SC.SCons (SC.SAtom p) rest) = do
  p' <- readParameter oplist p
  rest' <- readInputs oplist rest
  return $ p' : rest'
readInputs _ _ = throwError "malformed input list"

-- | "Global" data stored in the Reader monad throughout most of the parsing.
data DefsInfo sym arch sh = DefsInfo
                            { getSym :: sym
                            -- ^ SymInterface/ExprBuilder used to build up symbolic
                            -- expressions while parsing the definitions.
                            , getLitLookup :: forall tp. StateVar arch tp -> Maybe (S.SymExpr sym tp)
                            -- ^ Function used to retrieve the expression
                            -- corresponding to a given literal.
                            , getOpVarList :: OperandList (BoundVar sym arch) sh
                            -- ^ OperandList used to retrieve the variable
                            -- corresponding to a given literal.
                            , getOpNameList :: OperandList (OpData arch) sh
                            -- ^ OperandList used to look up the index given an
                            -- operand's name.
                            }

-- | Stores a NatRepr along with proof that its type parameter is a bitvector of
-- that length.
data BVProof tp where
  BVProof :: forall n. (1 <= n) => NatRepr n -> BVProof (BaseBVType n)

-- | Given an expression, monadically either returns proof that it is a
-- bitvector or throws an error.
getBVProof :: (S.IsExpr ex, MonadError String m) => ex tp -> m (BVProof tp)
getBVProof expr =
  case S.exprType expr of
    BaseBVRepr n -> return $ BVProof n
    t -> throwError $ "expected BV, found " ++ show t

-- | Type of the various different handlers for building up expressions formed
-- by applying arguments to some function.
--
-- ...yes, it's a lot of type parameters.
type ExprParser sym arch sh m = (S.IsExprBuilder sym,
                                 MonadError String m,
                                 MonadReader (DefsInfo sym arch sh) m,
                                 MonadIO m)
                              => SC.SExpr Atom
                              -> [Some (S.SymExpr sym)]
                              -> m (Maybe (Some (S.SymExpr sym)))

-- | Parse an expression of the form @(concat x y)@.
readConcat :: ExprParser sym arch sh m
readConcat (SC.SAtom (AIdent "concat")) args =
  prefixError ("in reading concat expression: ") $ do
    when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
    sym <- reader getSym
    Some arg1 <- return $ head args
    Some arg2 <- return $ head (tail args)
    BVProof _ <- prefixError ("in arg 1: ") $ getBVProof arg1
    BVProof _ <- prefixError ("in arg 2: ") $ getBVProof arg2
    liftIO (Just . Some <$> S.bvConcat sym arg1 arg2)
readConcat _ _ = return Nothing

-- | Try converting an 'Integer' to a 'NatRepr' or throw an error if not
-- possible.
intToNatM :: (MonadError String m) => Integer -> m (Some NatRepr)
intToNatM = maybe (throwError "integer must be non-negative to be a nat") return . someNat

-- | Parse an expression of the form @((_ extract i j) x)@.
readExtract :: ExprParser sym arch sh m
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
  BVProof lenNat <- getBVProof arg
  idxCheck <- maybe (throwError "invalid extract for given bitvector") return $
    testLeq (addNat idxNat nNat) lenNat
  liftIO $ withLeqProof nPositive $ withLeqProof idxCheck $
    (Just <$> Some <$> S.bvSelect sym idxNat nNat arg)
readExtract _ _ = return Nothing

-- | Parse an expression of the form @((_ zero_extend i) x)@ or @((_ sign_extend i) x)@.
readExtend :: ExprParser sym arch sh m
readExtend (SC.SCons (SC.SAtom (AIdent "_"))
             (SC.SCons (SC.SAtom (AIdent extend))
               (SC.SCons (SC.SAtom (AInt iInt))
                SC.SNil)))
           args
  | extend == "zero_extend" ||
    extend == "sign_extend" = prefixError ("in reading " ++ extend ++ " expression: ") $ do
      when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
      sym <- reader getSym
      Some iNat <- intToNatM iInt
      iPositive <- maybe (throwError "must extend by a positive length") return $ isPosNat iNat
      Some arg <- return $ head args
      -- ^ required to be bound (rather then let-destructured) so GHC's brain doesn't explode
      BVProof lenNat <- getBVProof arg
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
bvUnop :: (S.IsExprBuilder sym) => String -> Maybe (BVUnop sym)
bvUnop "bvneg" = Just $ BVUnop S.bvNeg
bvUnop "bvnot" = Just $ BVUnop S.bvNotBits
bvUnop       _ = Nothing

-- | Parse an expression of the form @(f x)@, where @f@ operates on bitvectors.
readBVUnop :: forall sym arch sh m. ExprParser sym arch sh m
readBVUnop (SC.SAtom (AIdent ident)) args
  | Just (BVUnop op) :: Maybe (BVUnop sym) <- bvUnop ident =
      prefixError ("in reading " ++ ident ++ " expression: ") $ do
        when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
        sym <- reader getSym
        Some expr <- return $ head args
        BVProof _ <- getBVProof expr
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
bvBinop :: (S.IsExprBuilder sym) => String -> Maybe (BVBinop sym)
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
readBVBinop :: forall sym arch sh m. ExprParser sym arch sh m
readBVBinop (SC.SAtom (AIdent ident)) args
  | Just op :: Maybe (BVBinop sym) <- bvBinop ident =
      prefixError ("in reading " ++ ident ++ " expression: ") $ do
        when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
        sym <- reader getSym
        Some arg1 <- return $ head args
        Some arg2 <- return $ head (tail args)
        BVProof m <- prefixError ("in arg 1: ") $ getBVProof arg1
        BVProof n <- prefixError ("in arg 2: ") $ getBVProof arg2
        case testEquality m n of
          Just Refl -> liftIO $ Just <$>
            case op of
              BinopBV op' -> Some <$> op' sym arg1 arg2
              BinopBool op' -> Some <$> op' sym arg1 arg2
          Nothing -> throwError $ "arguments to " ++ ident ++
            " must be the same length, but arg 1 has length " ++ show m ++
            " and arg 2 has length " ++ show n
readBVBinop _ _ = return Nothing

-- | Parse an expression of the form @(= x y)@.
readEq :: ExprParser sym arch sh m
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
readIte :: ExprParser sym arch sh m
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
             S.IsSymInterface sym,
             Monad m,
             MonadError String m,
             Architecture arch,
             MonadReader (DefsInfo sym arch sh) m,
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
  DefsInfo { getOpNameList = opNames
                , getSym = sym
                , getOpVarList = opVars
                , getLitLookup = litLookup
                } <- ask
  param <- readParameter opNames paramRaw
  case param of
    Some (Operand _ idx) -> return . Some . S.varExpr sym . unBoundVar $ indexOpList opVars idx
    Some (Literal _ lit) -> maybe (throwError "not declared as input") (return . Some) $ litLookup lit
readExpr (SC.SCons opRaw argsRaw) = do
  args <- readExprs argsRaw
  parseTries <- sequence $ map (\f -> f opRaw args)
    [readConcat, readExtract, readExtend, readBVUnop, readBVBinop, readEq, readIte]
  case asum parseTries of
    Just expr -> return expr
    Nothing -> throwError $ "couldn't parse expression " ++ show opRaw

-- | Parse multiple expressions in a list.
readExprs :: (S.IsExprBuilder sym,
              S.IsSymInterface sym,
              Monad m,
              MonadError String m,
              Architecture arch,
              MonadReader (DefsInfo sym arch sh) m,
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
             S.IsSymInterface sym,
             Monad m,
             MonadError String m,
             Architecture arch,
             MonadReader (DefsInfo sym arch sh) m,
             MonadIO m)
         => SC.SExpr Atom
         -> m (MapF.MapF (Parameter arch sh) (S.SymExpr sym))
readDefs SC.SNil = return MapF.empty
readDefs (SC.SCons (SC.SCons (SC.SAtom p) defRaw) rest) = do
  oplist <- reader getOpNameList
  Some param <- readParameter oplist p
  Some def <- readExpr defRaw
  Refl <- case testEquality (paramType param) (S.exprType def) of
    Just pf -> return pf
    Nothing -> throwError $ "mismatching types of parameter and expression for " ++ showF param
  rest' <- readDefs rest
  return $ MapF.insert param def rest'
readDefs _ = throwError "invalid defs structure"

-- | Parse the whole definition of a templated formula, inside an appropriate
-- monad.
readFormula' :: forall sym arch sh m.
                (S.IsExprBuilder sym,
                 S.IsSymInterface sym,
                 MonadError String m,
                 MonadIO m,
                 Architecture arch,
                 BuildOperandList arch sh)
             => sym
             -- -> (TaggedParameter -> Maybe (Some BaseTypeRepr))
             -> T.Text
             -> m (ParameterizedFormula sym arch sh)
readFormula' sym text = do
  sexpr <- case parseLL text of
             Left err -> throwError err
             Right res -> return res
  -- Extract the raw s-expressions for the three components.
  (opsRaw, inputsRaw, defsRaw) <- case sexpr of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "operands")) ops)
      (SC.SCons (SC.SCons (SC.SAtom (AIdent "in")) inputs)
       (SC.SCons (SC.SCons (SC.SAtom (AIdent "defs")) defs)
         SC.SNil))
      -> return (ops, inputs, defs)
    _ -> throwError "invalid top-level structure"

  -- Most of the following bindings have type annotations not because inference
  -- fails, but because with all the type-level hackery we have going on, it
  -- greatly helps human comprehension.

  -- Build the operand list from the given s-expression, validating that it
  -- matches the correct shape as we go.
  operands :: OperandList (OpData arch) sh
    <- maybe (throwError "invalid operand structure") return (buildOperandList opsRaw)

  inputs :: [Some (Parameter arch sh)]
    <- readInputs operands inputsRaw

  let mkOperandVar :: forall s. OpData arch s -> m (BoundVar sym arch s)
      mkOperandVar (OpData name tpRepr) =
        case userSymbol ("op_" ++ name) of
          Right symbol -> BoundVar <$> (liftIO $ S.freshBoundVar sym symbol tpRepr)
          Left _ -> throwError $ "invalid operand name " ++ name

  opVarList :: OperandList (BoundVar sym arch) sh
    <- traverseOperandList mkOperandVar operands

  -- NOTE: At the moment, we just trust that the semantics definition declares
  -- the correct input operands; instead of validating it, we generate BoundVars
  -- for *all* inputs (partially because it is unclear how to treat immediates
  -- -- do they count as inputs?).

  let mkLiteralVar :: forall tp. BaseTypeRepr tp -> StateVar arch tp -> m (S.BoundVar sym tp)
      mkLiteralVar tpRepr var =
        case userSymbol (showF var) of
          Right symbol -> liftIO $ S.freshBoundVar sym symbol tpRepr
          Left _ -> throwError $ "invalid var name!! " ++ showF var

      buildLitVarMap :: Some (Parameter arch sh)
                     -> MapF.MapF (StateVar arch) (S.BoundVar sym)
                     -> m (MapF.MapF (StateVar arch) (S.BoundVar sym))
      buildLitVarMap (Some (Literal tpRepr var)) m = (\v -> MapF.insert var v m) <$> mkLiteralVar tpRepr var
      buildLitVarMap (Some (Operand _ _))        m = return m

  litVars :: MapF.MapF (StateVar arch) (S.BoundVar sym)
    <- foldrM buildLitVarMap MapF.empty inputs

  defs <- runReaderT (readDefs defsRaw) $
    DefsInfo { getSym = sym
             , getLitLookup = \var -> S.varExpr sym <$> flip MapF.lookup litVars var
             , getOpVarList = opVarList
             , getOpNameList = operands
             }
  return $
    -- ParameterizedFormula ops (Set.fromList inputs) paramVars defs
    ParameterizedFormula { pfUses = Set.fromList inputs
                         , pfOperandVars = opVarList
                         , pfLiteralVars = litVars
                         , pfDefs = defs
                         }

-- | Parse the definition of a templated formula.
readFormula :: (S.IsExprBuilder sym,
                S.IsSymInterface sym,
                Architecture arch,
                BuildOperandList arch sh)
            => sym
            -- -> (TaggedParameter -> Maybe (Some BaseTypeRepr))
            -> T.Text
            -> IO (Either String (ParameterizedFormula sym arch sh))
readFormula sym text = runExceptT $ readFormula' sym text

-- | Read a templated formula definition from file, then parse it.
readFormulaFromFile :: (S.IsExprBuilder sym,
                        S.IsSymInterface sym,
                        Architecture arch,
                        BuildOperandList arch sh)
                    => sym
                    -- -> (TaggedParameter -> Maybe (Some BaseTypeRepr))
                    -> FilePath
                    -> IO (Either String (ParameterizedFormula sym arch sh))
readFormulaFromFile sym fp = readFormula sym =<< T.readFile fp
