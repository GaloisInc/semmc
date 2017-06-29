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
module SemMC.Formula.Parser
  ( Atom(..)
  , operandVarPrefix
  , literalVarPrefix
  , readFormula
  , readFormulaFromFile
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

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Util

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

-- ** Utility functions

-- | Utility function for contextualizing errors. Prepends the given prefix
-- whenever an error is thrown.
prefixError :: (Monoid e, MonadError e m) => e -> m a -> m a
prefixError prefix act = catchError act (throwError . mappend prefix)

-- | Utility function for lifting a 'Maybe' into a 'MonadError'
fromMaybeError :: (MonadError e m) => e -> Maybe a -> m a
fromMaybeError err = maybe (throwError err) return

-- ** Parsing operands

-- | Data about the operands pertinent after parsing: their name and their type.
data OpData (arch :: *) (s :: Symbol) where
  OpData :: String -> BaseTypeRepr (OperandType arch s) -> OpData arch s

-- | How to parse an operand list for a given architecture and shape. The
-- architecture is necessary in order to know how to map a symbol representing
-- operand type to a Crucible expression type.
--
-- This isn't intended to have any implementers outside the following two, so it
-- isn't exported. However, it is required on the signature of 'parseFormula',
-- so GHC has to be able to match the shape to the given instances.
class BuildOperandList (arch :: *) (tps :: [Symbol]) where
  -- | Parses the operands part of the semantics definition. Each operand has both
  -- a name and a (quoted) type in a dotted pair. For example:
  --
  -- > ((ra . 'Gprc)
  -- >  (n . 'Imm16)
  -- >  (rt . 'Gprc))
  --
  buildOperandList :: SC.SExpr Atom -> Maybe (OperandList (OpData arch) tps)

-- nil case...
instance BuildOperandList arch '[] where
  buildOperandList SC.SNil = Just Nil
  buildOperandList       _ = Nothing

-- ...and cons case. Sorry for the type operator screwing up indentation for the
-- rest of the file.
instance (KnownSymbol tp,
          KnownRepr BaseTypeRepr (OperandType arch tp),
          BuildOperandList arch tps)
       => BuildOperandList arch (tp ': tps) where
  buildOperandList SC.SNil = Nothing
  buildOperandList (SC.SAtom _) = Nothing
  buildOperandList (SC.SCons s rest) = do
    -- This is in the Maybe monad.
    let SC.SCons (SC.SAtom (AIdent operand)) (SC.SAtom (AQuoted ty)) = s
    when (symbolVal (Proxy :: Proxy tp) /= ty) Nothing
    rest' <- buildOperandList rest
    let repr = knownRepr :: BaseTypeRepr (OperandType arch tp)
    return $ (OpData operand repr) :> rest'

-- ** Parsing parameters
--
-- By which I mean, handling occurrences in expressions of either operands or
-- literals.

-- | Low-level representation of a parameter: no checking done yet on whether
-- they're valid yet or not.
data RawParameter = RawOperand String
                  | RawLiteral String
                  deriving (Show, Eq, Ord)

operandVarPrefix :: String
operandVarPrefix = "op_"

literalVarPrefix :: String
literalVarPrefix = "lit_"

-- | Parses the name of a parameter and whether it's an operand or a literal.
readRawParameter :: (MonadError String m) => Atom -> m RawParameter
readRawParameter (AIdent name)
  | Right _ <- userSymbol (operandVarPrefix ++ name) = return (RawOperand name)
  | otherwise = throwError $ name ++ " is not a valid parameter name"
readRawParameter (AQuoted name)
  | Right _ <- userSymbol (literalVarPrefix ++ name) = return (RawLiteral name)
  | otherwise = throwError $ name ++ " is not a valid parameter name"
readRawParameter a = throwError $ "expected parameter, found " ++ show a

-- | Short-lived type that just stores an index with its corresponding type
-- representation, with the type parameter ensuring they correspond to one another.
data IndexWithType (arch :: *) (sh :: [Symbol]) (s :: Symbol) where
  IndexWithType :: BaseTypeRepr (OperandType arch s) -> Index sh s -> IndexWithType arch sh s

-- | Look up a name in the given operand list, returning its index and type if found.
findOpListIndex :: String -> OperandList (OpData arch) sh -> Maybe (Some (IndexWithType arch sh))
findOpListIndex _ Nil = Nothing
findOpListIndex x ((OpData name tpRepr) :> rest)
  | x == name = Just $ Some (IndexWithType tpRepr IndexHere)
  | otherwise = mapSome incrIndex <$> findOpListIndex x rest
      where incrIndex (IndexWithType tpRepr' idx) = IndexWithType tpRepr' (IndexThere idx)

-- | Parse a single parameter, given the list of operands to use as a lookup.
readParameter :: (MonadError String m, Architecture arch) => OperandList (OpData arch) sh -> Atom -> m (Some (Parameter arch sh))
readParameter oplist atom =
  readRawParameter atom >>= \case
    RawOperand op ->
      maybe (throwError $ "couldn't find operand " ++ op)
            (viewSome (\(IndexWithType tpRepr idx) -> return $ Some (Operand tpRepr idx)))
            (findOpListIndex op oplist)
    RawLiteral lit ->
      maybe (throwError $ lit ++ " is an invalid literal for this arch")
            (return . viewSome (Some . Literal))
            (readLocation lit)

-- | Parses the input list, e.g., @(ra rb 'ca)@
readInputs :: (MonadError String m,
               Architecture arch)
           => OperandList (OpData arch) sh
           -> SC.SExpr Atom
           -> m [Some (Parameter arch sh)]
readInputs _ SC.SNil = return []
readInputs oplist (SC.SCons (SC.SAtom p) rest) = do
  p' <- readParameter oplist p
  rest' <- readInputs oplist rest
  return $ p' : rest'
readInputs _ _ = throwError "malformed input list"

-- ** Parsing definitions

-- | "Global" data stored in the Reader monad throughout parsing the definitions.
data DefsInfo sym arch sh = DefsInfo
                            { getSym :: sym
                            -- ^ SymInterface/ExprBuilder used to build up symbolic
                            -- expressions while parsing the definitions.
                            , getLitLookup :: forall tp. Location arch tp -> Maybe (S.SymExpr sym tp)
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
-- that length. Used for easy pattern matching on the LHS of a binding in a
-- do-expression to extract the proof.
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
-- Why is it both in 'MonadError' and return a 'Maybe'? An error is thrown if it
-- looks like a given handler should be able to handle an expression, but
-- there's some fault somewhere. 'Nothing' is returned if the handler can't
-- handle expressions looking like the form given to it.
--
-- Unrelated to the type, in many of these functions, there are statements of
-- the form @Some x <- return y@. Why do a binding from a direct return? Because
-- GHC cannot do let-destructuring when existentials are involved.
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
  prefixError "in reading concat expression: " $ do
    when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
    sym <- reader getSym
    Some arg1 <- return $ args !! 0
    Some arg2 <- return $ args !! 1
    BVProof _ <- prefixError "in arg 1: " $ getBVProof arg1
    BVProof _ <- prefixError "in arg 2: " $ getBVProof arg2
    liftIO (Just . Some <$> S.bvConcat sym arg1 arg2)
readConcat _ _ = return Nothing

-- | Try converting an 'Integer' to a 'NatRepr' or throw an error if not
-- possible.
intToNatM :: (MonadError String m) => Integer -> m (Some NatRepr)
intToNatM = fromMaybeError "integer must be non-negative to be a nat" . someNat

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
  -- The SMT-LIB spec represents extracts differently than Crucible does. Per
  -- SMT: "extraction of bits i down to j from a bitvector of size m to yield a
  -- new bitvector of size n, where n = i - j + 1". Per Crucible:
  --
  -- > -- | Select a subsequence from a bitvector.
  -- > bvSelect :: (1 <= n, idx + n <= w)
  -- >          => sym
  -- >          -> NatRepr idx  -- ^ Starting index, from 0 as least significant bit
  -- >          -> NatRepr n    -- ^ Number of bits to take
  -- >          -> SymBV sym w  -- ^ Bitvector to select from
  -- >          -> IO (SymBV sym n)
  --
  -- The "starting index" seems to be from the bottom, so that (in slightly
  -- pseudocode)
  --
  -- > > bvSelect sym 0 8 (0x01020304:[32])
  -- > 0x4:[8]
  -- > > bvSelect sym 24 8 (0x01020304:[32])
  -- > 0x1:[8]
  --
  -- Thus, n = i - j + 1, and idx = j.
  let nInt = iInt - jInt + 1
      idxInt = jInt
  Some nNat <- prefixError "in calculating extract length: " $ intToNatM nInt
  Some idxNat <- prefixError "in extract lower bound: " $ intToNatM idxInt
  LeqProof <- fromMaybeError "extract length must be positive" $ isPosNat nNat
  Some arg <- return $ args !! 0
  BVProof lenNat <- getBVProof arg
  LeqProof <- fromMaybeError "invalid extract for given bitvector" $
    testLeq (addNat idxNat nNat) lenNat
  liftIO (Just <$> Some <$> S.bvSelect sym idxNat nNat arg)
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
      iPositive <- fromMaybeError "must extend by a positive length" $ isPosNat iNat
      Some arg <- return $ args !! 0
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
  | Just (BVUnop op :: BVUnop sym) <- bvUnop ident =
      prefixError ("in reading " ++ ident ++ " expression: ") $ do
        when (length args /= 1) (throwError $ "expecting 1 argument, got " ++ show (length args))
        sym <- reader getSym
        Some expr <- return $ args !! 0
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
  | Just (op :: BVBinop sym) <- bvBinop ident =
      prefixError ("in reading " ++ ident ++ " expression: ") $ do
        when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
        sym <- reader getSym
        Some arg1 <- return $ args !! 0
        Some arg2 <- return $ args !! 1
        BVProof m <- prefixError ("in arg 1: ") $ getBVProof arg1
        BVProof n <- prefixError ("in arg 2: ") $ getBVProof arg2
        case testEquality m n of
          Just Refl -> liftIO $ Just <$>
            case op of
              BinopBV op' -> Some <$> op' sym arg1 arg2
              BinopBool op' -> Some <$> op' sym arg1 arg2
          Nothing -> throwError $ unwords
                       ["arguments to",
                        ident,
                        "must be the same length, but arg 1 has length",
                        show m,
                        "and arg 2 has length",
                        show n]
readBVBinop _ _ = return Nothing

-- | Parse an expression of the form @(= x y)@.
readEq :: ExprParser sym arch sh m
readEq (SC.SAtom (AIdent "=")) args =
  prefixError ("in reading '=' expression: ") $ do
    when (length args /= 2) (throwError $ "expecting 2 arguments, got " ++ show (length args))
    sym <- reader getSym
    Some arg1 <- return $ args !! 0
    Some arg2 <- return $ args !! 1
    case testEquality (S.exprType arg1) (S.exprType arg2) of
      Just Refl -> liftIO (Just . Some <$> S.isEq sym arg1 arg2)
      Nothing -> throwError $ unwords
                   ["arguments must have same types; instead, got for arg 1",
                    show (S.exprType arg1),
                    "and for arg 2 got",
                    show (S.exprType arg2)]
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
          Nothing -> throwError $ unwords
                       ["then and else branches must have same type; got",
                        show (S.exprType then_),
                        "for then and",
                        show (S.exprType else_),
                        "for else"]
      tp -> throwError $ "test expression must be a boolean; got " ++ show tp
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
readExpr (SC.SAtom (AInt _)) = throwError "found int where expected an expression; perhaps you wanted a bitvector?"
readExpr (SC.SAtom (ABV len val)) = do
  -- This is a bitvector literal.
  sym <- reader getSym
  Just (Some lenRepr) <- return $ someNat (toInteger len)
  let Just pf = isPosNat lenRepr
  -- ^ The above two patterns should never fail, given that during parsing we
  -- can only construct BVs with positive length.
  liftIO $ withLeqProof pf (Some <$> S.bvLit sym lenRepr val)
readExpr (SC.SAtom paramRaw) = do
  -- This is a parameter (i.e., variable).
  DefsInfo { getOpNameList = opNames
           , getSym = sym
           , getOpVarList = opVars
           , getLitLookup = litLookup
           } <- ask
  param <- readParameter opNames paramRaw
  case param of
    Some (Operand _ idx) -> return . Some . S.varExpr sym . unBoundVar $ indexOpList opVars idx
    Some (Literal lit) -> maybe (throwError "not declared as input") (return . Some) $ litLookup lit
readExpr (SC.SCons opRaw argsRaw) = do
  -- This is a function application.
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
readDefs (SC.SCons (SC.SCons (SC.SAtom p) (SC.SCons defRaw SC.SNil)) rest) = do
  oplist <- reader getOpNameList
  Some param <- readParameter oplist p
  Some def <- readExpr defRaw
  Refl <- fromMaybeError ("mismatching types of parameter and expression for " ++ showF param) $
            testEquality (paramType param) (S.exprType def)
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
             -> T.Text
             -> m (ParameterizedFormula sym arch sh)
readFormula' sym text = do
  sexpr <- case parseLL text of
             Left err -> throwError err
             Right res -> return res
  -- Extract the raw s-expressions for the three components.
  (opsRaw, inputsRaw, defsRaw) <- case sexpr of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "operands")) (SC.SCons ops SC.SNil))
      (SC.SCons (SC.SCons (SC.SAtom (AIdent "in")) (SC.SCons inputs SC.SNil))
       (SC.SCons (SC.SCons (SC.SAtom (AIdent "defs")) (SC.SCons defs SC.SNil))
         SC.SNil))
      -> return (ops, inputs, defs)
    _ -> throwError "invalid top-level structure"

  -- Most of the following bindings have type annotations not because inference
  -- fails, but because with all the type-level hackery we have going on, it
  -- greatly helps human comprehension.

  -- Build the operand list from the given s-expression, validating that it
  -- matches the correct shape as we go.
  operands :: OperandList (OpData arch) sh
    <- fromMaybeError "invalid operand structure" (buildOperandList opsRaw)

  inputs :: [Some (Parameter arch sh)]
    <- readInputs operands inputsRaw

  let mkOperandVar :: forall s. OpData arch s -> m (BoundVar sym arch s)
      mkOperandVar (OpData name tpRepr) =
        let symbol = makeSymbol (operandVarPrefix ++ name)
        in BoundVar <$> (liftIO $ S.freshBoundVar sym symbol tpRepr)

  opVarList :: OperandList (BoundVar sym arch) sh
    <- traverseOperandList mkOperandVar operands

  -- NOTE: At the moment, we just trust that the semantics definition declares
  -- the correct input operands; instead of validating it, we generate BoundVars
  -- for *all* inputs (partially because it is unclear how to treat immediates
  -- -- do they count as inputs?).

  let mkLiteralVar :: forall tp. BaseTypeRepr tp -> Location arch tp -> m (S.BoundVar sym tp)
      mkLiteralVar tpRepr loc =
        let symbol = makeSymbol (literalVarPrefix ++ showF loc)
        in liftIO $ S.freshBoundVar sym symbol tpRepr

      buildLitVarMap :: Some (Parameter arch sh)
                     -> MapF.MapF (Location arch) (S.BoundVar sym)
                     -> m (MapF.MapF (Location arch) (S.BoundVar sym))
      buildLitVarMap (Some (Literal loc)) m = (\v -> MapF.insert loc v m) <$> mkLiteralVar (locationType loc) loc
      buildLitVarMap (Some (Operand _ _))        m = return m

  litVars :: MapF.MapF (Location arch) (S.BoundVar sym)
    <- foldrM buildLitVarMap MapF.empty inputs

  defs <- runReaderT (readDefs defsRaw) $
    DefsInfo { getSym = sym
             , getLitLookup = \loc -> S.varExpr sym <$> flip MapF.lookup litVars loc
             , getOpVarList = opVarList
             , getOpNameList = operands
             }

  return $
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
            -> T.Text
            -> IO (Either String (ParameterizedFormula sym arch sh))
readFormula sym text = runExceptT $ readFormula' sym text

-- | Read a templated formula definition from file, then parse it.
readFormulaFromFile :: (S.IsExprBuilder sym,
                        S.IsSymInterface sym,
                        Architecture arch,
                        BuildOperandList arch sh)
                    => sym
                    -> FilePath
                    -> IO (Either String (ParameterizedFormula sym arch sh))
readFormulaFromFile sym fp = readFormula sym =<< T.readFile fp
