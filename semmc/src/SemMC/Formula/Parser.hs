{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | A parser for an s-expression representation of formulas
module SemMC.Formula.Parser
  ( operandVarPrefix
  , literalVarPrefix
  , argumentVarPrefix
  , readFormula
  , readFormulaFromFile
  , readDefinedFunction
  , readDefinedFunctionFromFile
  ) where

import qualified Control.Monad.Except as E
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Control.Monad.Reader as MR
import           Control.Monad ( when )
import           Data.Foldable ( foldrM )
import qualified Data.Map as Map
import qualified Data.SCargot.Repr as SC
import           Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Printf ( printf )
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )
import           Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Classes
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..), mapSome, viewSome )
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.TraversableFC ( traverseFC, allFC )
import qualified Data.Parameterized.Map as MapF
import           What4.BaseTypes
import qualified Lang.Crucible.Backend as S
import qualified What4.Interface as S
import           What4.Symbol ( userSymbol )

import qualified Data.Type.List as TL -- in this package
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Env ( FormulaEnv(..), SomeSome(..) )
import           SemMC.Formula.Formula
import           SemMC.Formula.SETokens
import qualified SemMC.Util as U

import           Prelude

data OperandTypeWrapper (arch :: *) :: TL.TyFun Symbol BaseType -> *
type instance TL.Apply (OperandTypeWrapper arch) s = A.OperandType arch s
type OperandTypes arch sh = TL.Map (OperandTypeWrapper arch) sh

-- | A counterpart to 'SemMC.Formula.Parameter' for use in the parser, where we
-- might know only a parameter's base type (such as when parsing a defined
-- function).
data ParsedParameter arch (tps :: [BaseType]) (tp :: BaseType) where
  ParsedOperandParameter :: BaseTypeRepr tp -> SL.Index tps tp
                         -> ParsedParameter arch tps tp
  ParsedLiteralParameter :: L.Location arch tp -> ParsedParameter arch tps tp

-- Translating from 'SL.Index' on 'BaseType' to 'SL.Index' on 'Symbol' is
-- tricky.  Need this view to show that when we translate some @SL.Index tps tp@
-- to an @SL.Index sh s@, the symbol @s@ maps to the base type @tp@ (assuming
-- that @tps ~ OperandTypes arch sh@).
data IndexByArchType arch sh tp where
  IndexByArchType :: A.OperandType arch s ~ tp => SL.Index sh s -> IndexByArchType arch sh tp

-- TODO This is all very silly. It's an expensive identity function.
indexByArchType :: proxy arch
                -> A.ShapeRepr arch sh
                -> SL.Index (OperandTypes arch sh) tp
                -> IndexByArchType arch sh tp
indexByArchType _ SL.Nil _ = error "impossible"
indexByArchType _ (_ SL.:< _) SL.IndexHere = IndexByArchType SL.IndexHere
indexByArchType p (_ SL.:< shapeRepr) (SL.IndexThere ix) =
  case indexByArchType p shapeRepr ix of
    IndexByArchType ix' -> IndexByArchType (SL.IndexThere ix')

toParameter :: forall arch sh tp
             . A.ShapeRepr arch sh
            -> ParsedParameter arch (OperandTypes arch sh) tp
            -> Parameter arch sh tp
toParameter shapeRepr (ParsedOperandParameter tpRepr ix) =
  case indexByArchType (Proxy @arch) shapeRepr ix of
    IndexByArchType ix' -> OperandParameter tpRepr ix'
toParameter _ (ParsedLiteralParameter loc) =
  LiteralParameter loc

-- * First pass of parsing turns the raw text into s-expressions.
--   This pass is handled by the code in SemMC.Formula.SELang

-- * Second pass of parsing: turning the s-expressions into symbolic expressions
-- and the overall templated formula

-- ** Utility functions

-- | Utility function for contextualizing errors. Prepends the given prefix
-- whenever an error is thrown.
prefixError :: (Monoid e, E.MonadError e m) => e -> m a -> m a
prefixError prefix act = E.catchError act (E.throwError . mappend prefix)

-- | Utility function for lifting a 'Maybe' into a 'MonadError'
fromMaybeError :: (E.MonadError e m) => e -> Maybe a -> m a
fromMaybeError err = maybe (E.throwError err) return

-- ** Parsing operands

-- | Data about the operands pertinent after parsing: their name and their type.
data OpData (tp :: BaseType) where
  OpData :: String -> BaseTypeRepr tp -> OpData tp

buildOperandList' :: forall m proxy arch tps
                   . (E.MonadError String m, A.Architecture arch)
                  => proxy arch
                  -> A.ShapeRepr arch tps
                  -> SC.SExpr FAtom
                  -> m (SL.List OpData (OperandTypes arch tps))
buildOperandList' proxy rep atm =
  case rep of
    SL.Nil ->
      case atm of
        SC.SNil -> return SL.Nil
        _ -> E.throwError $ "Expected Nil but got " ++ show atm
    r SL.:< rep' ->
      case atm of
        SC.SNil -> E.throwError $ "Expected entry but got Nil"
        SC.SAtom _ -> E.throwError $ "Expected SCons but got SAtom: " ++ show atm
        SC.SCons s rest -> do
          let SC.SCons (SC.SAtom (AIdent operand)) (SC.SAtom (AQuoted ty)) = s
          when (A.operandTypeReprSymbol proxy r /= ty) $
            E.throwError $ "unknown reference: " ++ show ty
          rest' <- buildOperandList' proxy rep' rest
          let tyRepr = A.shapeReprToTypeRepr proxy r
          return $ (OpData operand tyRepr) SL.:< rest'

buildArgumentList' :: forall m
                    . (E.MonadError String m)
                   => SC.SExpr FAtom
                   -> m (Some (SL.List OpData))
buildArgumentList' sexpr =
  case sexpr of
    SC.SNil -> return $ Some (SL.Nil)
    SC.SAtom _ -> E.throwError $ "Expected SNil or SCons but got SAtom: " ++ show sexpr
    SC.SCons s rest -> do
      (operand, tyRaw) <- case s of
        SC.SCons (SC.SAtom (AIdent operand)) (SC.SCons ty SC.SNil)
          -> return (operand, ty)
        _ -> E.throwError $ "Expected (operand . 'type) pair: " ++ show s
      Some tp <- readBaseType tyRaw
      Some rest' <- buildArgumentList' rest
      return $ Some ((OpData operand tp) SL.:< rest')

readBaseType :: forall m
              . (E.MonadError String m)
             => SC.SExpr FAtom
             -> m (Some BaseTypeRepr)
readBaseType sexpr =
  case sexpr of
    SC.SAtom (AQuoted atom) ->
      case atom of
        "bool" -> return $ Some BaseBoolRepr
        "nat" -> return $ Some BaseNatRepr
        "int" -> return $ Some BaseIntegerRepr
        "real" -> return $ Some BaseRealRepr
        "string" -> return $ Some BaseStringRepr
        "complex" -> return $ Some BaseComplexRepr
        _ -> panic
    SC.SCons (SC.SAtom (AQuoted "bv")) (SC.SCons (SC.SAtom (AInt w)) SC.SNil)
      | Just (Some wRepr) <- someNat w
      , Just LeqProof <- testLeq (knownNat @1) wRepr
        -> return $ Some (BaseBVRepr wRepr)
      | otherwise
        -> panic
    SC.SCons (SC.SAtom (AQuoted "struct")) (SC.SCons args SC.SNil) -> do
      Some tps <- readBaseTypes args
      return $ Some (BaseStructRepr tps)
    SC.SCons (SC.SAtom (AQuoted "array"))
             (SC.SCons ixArgs (SC.SCons tpArg SC.SNil)) -> do
      Some ixs <- readBaseTypes ixArgs
      Some tp <- readBaseType tpArg
      case Ctx.viewAssign ixs of
        Ctx.AssignEmpty -> E.throwError $ "array type has no indices: " ++ show sexpr
        Ctx.AssignExtend _ _ -> return $ Some (BaseArrayRepr ixs tp)
    _ -> panic
  where
    panic = E.throwError $ "unknown base type: " ++ show sexpr

readBaseTypes :: forall m
              . (E.MonadError String m)
              => SC.SExpr FAtom
              -> m (Some (Ctx.Assignment BaseTypeRepr))
readBaseTypes sexpr0 =
  go sexpr0 Ctx.empty
  where
    -- Be tail recursive to reverse list
    go :: SC.SExpr FAtom
       -> Ctx.Assignment BaseTypeRepr sh
       -> m (Some (Ctx.Assignment BaseTypeRepr))
    go sexpr acc =
      case sexpr of
        SC.SNil -> return (Some acc)
        SC.SCons s rest -> do
          Some tp <- readBaseType s
          go rest (Ctx.extend acc tp)
        SC.SAtom _ -> E.throwError $
          "expected list of base types: " ++ show sexpr

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

argumentVarPrefix :: String
argumentVarPrefix = "arg_"

-- | Parses the name of a parameter and whether it's an operand or a literal.
readRawParameter :: (E.MonadError String m) => FAtom -> m RawParameter
readRawParameter (AIdent name)
  | Right _ <- userSymbol (operandVarPrefix ++ name) = return (RawOperand name)
  | otherwise = E.throwError $ printf "%s is not a valid parameter name" name
readRawParameter (AQuoted name)
  | Right _ <- userSymbol (literalVarPrefix ++ name) = return (RawLiteral name)
  | otherwise = E.throwError $ printf "%s is not a valid parameter name" name
readRawParameter a = E.throwError $ printf "expected parameter, found %s" (show a)

-- | Short-lived type that just stores an index with its corresponding type
-- representation, with the type parameter ensuring they correspond to one another.
data IndexWithType (sh :: [BaseType]) (tp :: BaseType) where
  IndexWithType :: BaseTypeRepr tp -> SL.Index sh tp -> IndexWithType sh tp

-- | Look up a name in the given operand list, returning its index and type if found.
findOpListIndex :: String -> SL.List OpData sh -> Maybe (Some (IndexWithType sh))
findOpListIndex _ SL.Nil = Nothing
findOpListIndex x ((OpData name tpRepr) SL.:< rest)
  | x == name = Just $ Some (IndexWithType tpRepr SL.IndexHere)
  | otherwise = mapSome incrIndex <$> findOpListIndex x rest
      where incrIndex (IndexWithType tpRepr' idx) = IndexWithType tpRepr' (SL.IndexThere idx)

-- | Parse a single parameter, given the list of operands to use as a lookup.
readParameter :: (E.MonadError String m, A.Architecture arch) => proxy arch -> SL.List OpData tps -> FAtom -> m (Some (ParsedParameter arch tps))
readParameter _ oplist atom =
  readRawParameter atom >>= \case
    RawOperand op ->
      maybe (E.throwError $ printf "couldn't find operand %s" op)
            (viewSome (\(IndexWithType tpRepr idx) -> return $ Some (ParsedOperandParameter tpRepr idx)))
            (findOpListIndex op oplist)
    RawLiteral lit ->
      maybe (E.throwError $ printf "%s is an invalid literal for this arch" lit)
            (return . viewSome (Some . ParsedLiteralParameter))
            (A.readLocation lit)

-- | Parses the input list, e.g., @(ra rb 'ca)@
readInputs :: forall m (arch :: *) (tps :: [BaseType])
            . (E.MonadError String m,
               A.Architecture arch)
           => SL.List OpData tps
           -> SC.SExpr FAtom
           -> m [Some (ParsedParameter arch tps)]
readInputs _ SC.SNil = return []
readInputs oplist (SC.SCons (SC.SAtom p) rest) = do
  p' <- readParameter Proxy oplist p
  rest' <- readInputs oplist rest
  return $ p' : rest'
readInputs _ _ = E.throwError "malformed input list"

-- ** Parsing definitions

-- | "Global" data stored in the Reader monad throughout parsing the definitions.
data DefsInfo sym arch tps = DefsInfo
                             { getSym :: sym
                             -- ^ SymInterface/ExprBuilder used to build up symbolic
                             -- expressions while parsing the definitions.
                             , getEnv :: FormulaEnv sym arch
                             -- ^ Global formula environment
                             , getLitLookup :: forall tp. A.Location arch tp -> Maybe (S.SymExpr sym tp)
                             -- ^ Function used to retrieve the expression
                             -- corresponding to a given literal.
                             , getOpVarList :: SL.List (S.BoundVar sym) tps
                             -- ^ ShapedList used to retrieve the variable
                             -- corresponding to a given literal.
                             , getOpNameList :: SL.List OpData tps
                             -- ^ ShapedList used to look up the index given an
                             -- operand's name.
                             }

-- | Stores a NatRepr along with proof that its type parameter is a bitvector of
-- that length. Used for easy pattern matching on the LHS of a binding in a
-- do-expression to extract the proof.
data BVProof tp where
  BVProof :: forall n. (1 <= n) => NatRepr n -> BVProof (BaseBVType n)

-- | Given an expression, monadically either returns proof that it is a
-- bitvector or throws an error.
getBVProof :: (S.IsExpr ex, E.MonadError String m) => ex tp -> m (BVProof tp)
getBVProof expr =
  case S.exprType expr of
    BaseBVRepr n -> return $ BVProof n
    t -> E.throwError $ printf "expected BV, found %s" (show t)

data BoolProof tp where
  BoolProof :: BoolProof BaseBoolType

getBoolProof :: (S.IsExpr ex, E.MonadError String m) => ex tp -> m (BoolProof tp)
getBoolProof  expr =
  case S.exprType expr of
    BaseBoolRepr -> return BoolProof
    t -> E.throwError $ printf "expected a bool type, found %s" (show t)

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
type ExprParser sym arch sh m = (S.IsSymExprBuilder sym,
                                 E.MonadError String m,
                                 MR.MonadReader (DefsInfo sym arch sh) m,
                                 MonadIO m)
                              => SC.SExpr FAtom
                              -> [Some (S.SymExpr sym)]
                              -> m (Maybe (Some (S.SymExpr sym)))

-- | Parse an expression of the form @(concat x y)@.
readConcat :: ExprParser sym arch sh m
readConcat (SC.SAtom (AIdent "concat")) args =
  prefixError "in reading concat expression: " $ do
    when (length args /= 2) (E.throwError $ printf "expecting 2 arguments, got %d" (length args))
    sym <- MR.reader getSym
    Some arg1 <- return $ args !! 0
    Some arg2 <- return $ args !! 1
    BVProof _ <- prefixError "in arg 1: " $ getBVProof arg1
    BVProof _ <- prefixError "in arg 2: " $ getBVProof arg2
    liftIO (Just . Some <$> S.bvConcat sym arg1 arg2)
readConcat _ _ = return Nothing

-- | Try converting an 'Integer' to a 'NatRepr' or throw an error if not
-- possible.
intToNatM :: (E.MonadError String m) => Integer -> m (Some NatRepr)
intToNatM = fromMaybeError "integer must be non-negative to be a nat" . someNat

-- | Parse an expression of the form @((_ extract i j) x)@.
readExtract :: ExprParser sym arch sh m
readExtract (SC.SCons (SC.SAtom (AIdent "_"))
             (SC.SCons (SC.SAtom (AIdent "extract"))
              (SC.SCons (SC.SAtom (AInt iInt))
               (SC.SCons (SC.SAtom (AInt jInt))
                SC.SNil))))
            args = prefixError "in reading extract expression: " $ do
  when (length args /= 1) (E.throwError $ printf "expecting 1 argument, got %d" (length args))
  sym <- MR.reader getSym
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
    extend == "sign_extend" = prefixError (printf "in reading %s expression: " extend) $ do
      when (length args /= 1) (E.throwError $ printf "expecting 1 argument, got %d" (length args))
      sym <- MR.reader getSym
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
readBVUnop (SC.SAtom (AIdent idnt)) args
  | Just (BVUnop op :: BVUnop sym) <- bvUnop idnt =
      prefixError (printf "in reading %s expression: " idnt) $ do
        when (length args /= 1) (E.throwError $ printf "expecting 1 argument, got %d" (length args))
        sym <- MR.reader getSym
        Some expr <- return $ args !! 0
        BVProof _ <- getBVProof expr
        liftIO (Just . Some <$> op sym expr)
readBVUnop _ _ = return Nothing

-- | Encapsulating type for a binary operation that takes two bitvectors of the
-- same length.
data BVBinop sym where
  -- | Binop with a bitvector return type, e.g., addition or bitwise operations.
  BinopBV :: (forall w . (1 <= w) => sym -> S.SymBV sym w -> S.SymBV sym w -> IO (S.SymBV sym w)) -> BVBinop sym
  -- | Bitvector binop with a boolean return type, i.e., comparison operators.
  BinopBoolBV :: (forall w . (1 <= w) => sym -> S.SymBV sym w -> S.SymBV sym w -> IO (S.Pred sym)) -> BVBinop sym
  -- A binary operator of booleans
--  BinopBool :: sym -> S.Pred sym -> S.Pred sym -> BVBinop

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
bvBinop "bvult"  = Just $ BinopBoolBV S.bvUlt
bvBinop "bvule"  = Just $ BinopBoolBV S.bvUle
bvBinop "bvugt"  = Just $ BinopBoolBV S.bvUgt
bvBinop "bvuge"  = Just $ BinopBoolBV S.bvUge
bvBinop "bvslt"  = Just $ BinopBoolBV S.bvSlt
bvBinop "bvsle"  = Just $ BinopBoolBV S.bvSle
bvBinop "bvsgt"  = Just $ BinopBoolBV S.bvSgt
bvBinop "bvsge"  = Just $ BinopBoolBV S.bvSge
bvBinop "bveq"   = Just $ BinopBoolBV S.bvEq
bvBinop "bvne"   = Just $ BinopBoolBV S.bvNe
bvBinop        _ = Nothing

data BoolBinop sym where
  BoolBinop :: (sym -> S.Pred sym -> S.Pred sym -> IO (S.Pred sym)) -> BoolBinop sym

boolBinop :: (S.IsExprBuilder sym) => String -> Maybe (BoolBinop sym)
boolBinop s =
  case s of
    "andp" -> Just $ BoolBinop S.andPred
    "orp"  -> Just $ BoolBinop S.orPred
    "xorp" -> Just $ BoolBinop S.xorPred
    _ -> Nothing

data BoolUnop sym where
  BoolUnop :: (sym -> S.Pred sym -> IO (S.Pred sym)) -> BoolUnop sym

boolUnop :: (S.IsExprBuilder sym) => String -> Maybe (BoolUnop sym)
boolUnop s =
  case s of
    "notp" -> Just $ BoolUnop S.notPred
    _ -> Nothing

-- | Parse an expression of the form @(f x y)@, where @f@ is a binary operation
-- on bitvectors.
readBVBinop :: forall sym arch sh m. ExprParser sym arch sh m
readBVBinop (SC.SAtom (AIdent idnt)) args
  | Just (op :: BVBinop sym) <- bvBinop idnt =
      prefixError (printf "in reading %s expression: " idnt) $ do
        when (length args /= 2) (E.throwError $ printf "expecting 2 arguments, got %d" (length args))
        sym <- MR.reader getSym
        Some arg1 <- return $ args !! 0
        Some arg2 <- return $ args !! 1
        BVProof m <- prefixError "in arg 1: " $ getBVProof arg1
        BVProof n <- prefixError "in arg 2: " $ getBVProof arg2
        case testEquality m n of
          Just Refl -> liftIO $ Just <$>
            case op of
              BinopBV op' -> Some <$> op' sym arg1 arg2
              BinopBoolBV op' -> Some <$> op' sym arg1 arg2
          Nothing -> E.throwError $ printf "arguments to %s must be the same length, \
                                         \but arg 1 has length %s \
                                         \and arg 2 has length %s"
                                         idnt
                                         (show m)
                                         (show n)
readBVBinop _ _ = return Nothing

readBoolBinop :: forall sym arch sh m . ExprParser sym arch sh m
readBoolBinop (SC.SAtom (AIdent idnt)) args
  | Just (op :: BoolBinop sym) <- boolBinop idnt =
      prefixError (printf "in reading %s expression: " idnt) $ do
        when (length args /= 2) (E.throwError $ printf "expecting 2 arguments, got %d" (length args))
        sym <- MR.reader getSym
        Some arg1 <- return (args !! 0)
        Some arg2 <- return (args !! 1)
        BoolProof <- prefixError "in arg1: " $ getBoolProof arg1
        BoolProof <- prefixError "in arg2: " $ getBoolProof arg2
        case op of
          BoolBinop op' -> liftIO (Just <$> Some <$> op' sym arg1 arg2)
readBoolBinop _ _ = return Nothing

readBoolUnop :: forall sym arch sh m . ExprParser sym arch sh m
readBoolUnop (SC.SAtom (AIdent idnt)) args
  | Just (op :: BoolUnop sym) <- boolUnop idnt =
      prefixError (printf "in reading %s expression: " idnt) $ do
        when (length args /= 1) (E.throwError $ printf "expecting 1 argument, got %d" (length args))
        sym <- MR.reader getSym
        Some arg1 <- return (args !! 0)
        BoolProof <- prefixError "in arg1: " $ getBoolProof arg1
        case op of
          BoolUnop op' -> liftIO (Just <$> Some <$> op' sym arg1)
readBoolUnop _ _ = return Nothing

data TypeEqProof tp1 tp2 where
  TypeEqProof :: TypeEqProof tp tp

checkExprType
  :: (S.IsExpr ex, E.MonadError String m)
  => ex tp1
  -> BaseTypeRepr tp2
  -> m (TypeEqProof tp1 tp2)
checkExprType expr tp = case testEquality (S.exprType expr) tp of
  Just Refl -> return TypeEqProof
  Nothing   -> E.throwError $ printf "unexpected type: expected %s, found %s"
                                     (show tp)
                                     (show (S.exprType expr))

-- | Encapsulating type for a unary operation.
data FPUnOp sym where
  -- | Unary operation that takes one float.
  FPUnOp
    :: FloatPrecisionRepr fpp
    -> (sym -> S.SymFloat sym fpp -> IO (S.SymExpr sym tp))
    -> FPUnOp sym
  -- | Unary operation that takes one bitvector.
  FPBVUnOp
    :: (1 <= w)
    => NatRepr w
    -> (sym -> S.SymBV sym w -> IO (S.SymExpr sym tp))
    -> FPUnOp sym

-- | Look up a unary float operation by name.
fpUnOp :: (S.IsExprBuilder sym) => String -> Maybe (FPUnOp sym)
fpUnOp "fnegd" = Just $ FPUnOp knownRepr $ S.floatNeg @_ @Prec64
fpUnOp "fnegs" = Just $ FPUnOp knownRepr $ S.floatNeg @_ @Prec32
fpUnOp "fabsd" = Just $ FPUnOp knownRepr $ S.floatAbs @_ @Prec64
fpUnOp "fabss" = Just $ FPUnOp knownRepr $ S.floatAbs @_ @Prec32
fpUnOp "fsqrt" =
  Just $ FPUnOp knownRepr $ \sym -> S.floatSqrt @_ @Prec64 sym S.RNE
fpUnOp "fsqrts" =
  Just $ FPUnOp knownRepr $ \sym -> S.floatSqrt @_ @Prec32 sym S.RNE
fpUnOp "fnand" = Just $ FPUnOp knownRepr $ S.floatIsNaN @_ @Prec64
fpUnOp "fnans" = Just $ FPUnOp knownRepr $ S.floatIsNaN @_ @Prec32
fpUnOp "frsp" = Just $ FPUnOp knownRepr $ \sym ->
  S.floatCast @_ @Prec32 @Prec64 sym knownRepr S.RNE
fpUnOp "fp_single_to_double" = Just $ FPUnOp knownRepr $ \sym ->
  S.floatCast @_ @Prec64 @Prec32 sym knownRepr S.RNE
fpUnOp "fp_binary_to_double" =
  Just $ FPBVUnOp knownNat $ \sym -> S.floatFromBinary @_ @11 @53 sym knownRepr
fpUnOp "fp_binary_to_single" =
  Just $ FPBVUnOp knownNat $ \sym -> S.floatFromBinary @_ @8 @24 sym knownRepr
fpUnOp "fp_double_to_binary" =
  Just $ FPUnOp knownRepr $ S.floatToBinary @_ @11 @53
fpUnOp "fp_single_to_binary" =
  Just $ FPUnOp knownRepr $ S.floatToBinary @_ @8 @24
fpUnOp "fctid" = Just $ FPUnOp knownRepr $ \sym ->
  S.floatToSBV @_ @64 @Prec64 sym knownRepr S.RNE
fpUnOp "fctidu" = Just $ FPUnOp knownRepr $ \sym ->
  S.floatToBV @_ @64 @Prec64 sym knownRepr S.RNE
fpUnOp "fctiw" = Just $ FPUnOp knownRepr $ \sym ->
  S.floatToSBV @_ @32 @Prec64 sym knownRepr S.RNE
fpUnOp "fctiwu" = Just $ FPUnOp knownRepr $ \sym ->
  S.floatToBV @_ @32 @Prec64 sym knownRepr S.RNE
fpUnOp "fcfid" = Just $ FPBVUnOp knownRepr $ \sym ->
  S.sbvToFloat @_ @64 @Prec64 sym knownRepr S.RNE
fpUnOp "fcfids" = Just $ FPBVUnOp knownRepr $ \sym ->
  S.sbvToFloat @_ @64 @Prec32 sym knownRepr S.RNE
fpUnOp "fcfidu" = Just $ FPBVUnOp knownRepr $ \sym ->
  S.bvToFloat @_ @64 @Prec64 sym knownRepr S.RNE
fpUnOp "fcfidus" = Just $ FPBVUnOp knownRepr $ \sym ->
  S.bvToFloat @_ @64 @Prec32 sym knownRepr S.RNE
fpUnOp _ = Nothing

-- | Parse an expression of the form @(f x)@, where @f@ operates on floats.
readFPUnOp :: forall sym arch sh m . ExprParser sym arch sh m
readFPUnOp (SC.SAtom (AIdent idnt)) args
  | Just (fpop :: FPUnOp sym) <- fpUnOp idnt
  = prefixError (printf "in reading %s expression: " idnt) $ do
    when (length args /= 1)
         (E.throwError $ printf "expecting 1 argument, got %d" (length args))
    sym       <- MR.reader getSym
    Some arg0 <- return $ args !! 0
    case fpop of
      FPUnOp fpp op -> do
        TypeEqProof <-
          prefixError "in arg 1: " $ checkExprType arg0 $ BaseFloatRepr fpp
        liftIO (Just . Some <$> op sym arg0)
      FPBVUnOp w op -> do
        TypeEqProof <-
          prefixError "in arg 1: " $ checkExprType arg0 $ BaseBVRepr w
        liftIO (Just . Some <$> op sym arg0)
readFPUnOp _ _ = return Nothing

-- | Encapsulating type for a binary operation.
data FPBinOp sym where
  -- | Binary operation that takes two floats with the same precission.
  FPBinOp
    :: FloatPrecisionRepr fpp
    -> (  sym
       -> S.SymFloat sym fpp
       -> S.SymFloat sym fpp
       -> IO (S.SymExpr sym tp)
       )
    -> FPBinOp sym

-- | Look up a binary float operation by name.
fpBinOp :: (S.IsExprBuilder sym) => String -> Maybe (FPBinOp sym)
fpBinOp "fadd" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatAdd @_ @Prec64 sym S.RNE
fpBinOp "fadds" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatAdd @_ @Prec32 sym S.RNE
fpBinOp "fsub" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatSub @_ @Prec64 sym S.RNE
fpBinOp "fsubs" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatSub @_ @Prec32 sym S.RNE
fpBinOp "fmul" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatMul @_ @Prec64 sym S.RNE
fpBinOp "fmuls" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatMul @_ @Prec32 sym S.RNE
fpBinOp "fdiv" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatDiv @_ @Prec64 sym S.RNE
fpBinOp "fdivs" =
  Just $ FPBinOp knownRepr $ \sym -> S.floatDiv @_ @Prec32 sym S.RNE
fpBinOp "fltd" = Just $ FPBinOp knownRepr $ S.floatLt @_ @Prec64
fpBinOp "flts" = Just $ FPBinOp knownRepr $ S.floatLt @_ @Prec32
fpBinOp "feqd" = Just $ FPBinOp knownRepr $ S.floatFpEq @_ @Prec64
fpBinOp "feqs" = Just $ FPBinOp knownRepr $ S.floatFpEq @_ @Prec32
fpBinOp _ = Nothing

-- | Parse an expression of the form @(f x y)@, where @f@ is a binary operation
-- on floats.
readFPBinOp :: forall sym arch sh m . ExprParser sym arch sh m
readFPBinOp (SC.SAtom (AIdent idnt)) args
  | Just (fpop :: FPBinOp sym) <- fpBinOp idnt
  = prefixError (printf "in reading %s expression: " idnt) $ do
    when (length args /= 2)
         (E.throwError $ printf "expecting 2 arguments, got %d" (length args))
    sym       <- MR.reader getSym
    Some arg0 <- return $ args !! 0
    Some arg1 <- return $ args !! 1
    case fpop of
      FPBinOp fpp op -> do
        TypeEqProof <-
          prefixError "in arg 1: " $ checkExprType arg0 $ BaseFloatRepr fpp
        TypeEqProof <-
          prefixError "in arg 2: " $ checkExprType arg1 $ BaseFloatRepr fpp
        liftIO (Just . Some <$> op sym arg0 arg1)
readFPBinOp _ _ = return Nothing

-- | Encapsulating type for a ternary operation.
data FPTernOp sym where
  -- | Ternary operation that takes three floats.
  FPTernOp
    :: FloatPrecisionRepr fpp
    -> (  sym
       -> S.SymFloat sym fpp
       -> S.SymFloat sym fpp
       -> S.SymFloat sym fpp
       -> IO (S.SymExpr sym tp)
       )
    -> FPTernOp sym

-- | Look up a ternary float operation by name.
fpTernOp :: (S.IsExprBuilder sym) => String -> Maybe (FPTernOp sym)
fpTernOp "ffma" =
  Just $ FPTernOp knownRepr $ \sym -> S.floatFMA @_ @Prec64 sym S.RNE
fpTernOp "ffmas" =
  Just $ FPTernOp knownRepr $ \sym -> S.floatFMA @_ @Prec32 sym S.RNE
fpTernOp _ = Nothing

-- | Parse an expression of the form @(f x y z)@, where @f@ is a ternary
-- operation on floats.
readFPTernOp :: forall sym arch sh m . ExprParser sym arch sh m
readFPTernOp (SC.SAtom (AIdent idnt)) args
  | Just (fpop :: FPTernOp sym) <- fpTernOp idnt
  = prefixError (printf "in reading %s expression: " idnt) $ do
    when (length args /= 3)
         (E.throwError $ printf "expecting 3 arguments, got %d" (length args))
    sym       <- MR.reader getSym
    Some arg0 <- return $ args !! 0
    Some arg1 <- return $ args !! 1
    Some arg2 <- return $ args !! 1
    case fpop of
      FPTernOp fpp op -> do
        TypeEqProof <-
          prefixError "in arg 1: " $ checkExprType arg0 $ BaseFloatRepr fpp
        TypeEqProof <-
          prefixError "in arg 2: " $ checkExprType arg1 $ BaseFloatRepr fpp
        TypeEqProof <-
          prefixError "in arg 3: " $ checkExprType arg2 $ BaseFloatRepr fpp
        liftIO (Just . Some <$> op sym arg0 arg1 arg2)
readFPTernOp _ _ = return Nothing


-- | Parse an expression of the form @(= x y)@.
readEq :: ExprParser sym arch sh m
readEq (SC.SAtom (AIdent "=")) args =
  prefixError ("in reading '=' expression: ") $ do
    when (length args /= 2) (E.throwError $ printf "expecting 2 arguments, got %d" (length args))
    sym <- MR.reader getSym
    Some arg1 <- return $ args !! 0
    Some arg2 <- return $ args !! 1
    case testEquality (S.exprType arg1) (S.exprType arg2) of
      Just Refl -> liftIO (Just . Some <$> S.isEq sym arg1 arg2)
      Nothing -> E.throwError $ printf "arguments must have same types, \
                                     \but arg 1 has type %s \
                                     \and arg 2 has type %s"
                                     (show (S.exprType arg1))
                                     (show (S.exprType arg2))
readEq _ _ = return Nothing

-- | Parse an expression of the form @(ite b x y)@
readIte :: ExprParser sym arch sh m
readIte (SC.SAtom (AIdent "ite")) args =
  prefixError ("in reading ite expression: ") $ do
    when (length args /= 3) (E.throwError $ printf "expecting 3 arguments, got %d" (length args))
    sym <- MR.reader getSym
    Some test <- return $ args !! 0
    Some then_ <- return $ args !! 1
    Some else_ <- return $ args !! 2
    case S.exprType test of
      BaseBoolRepr ->
        case testEquality (S.exprType then_) (S.exprType else_) of
          Just Refl -> liftIO (Just . Some <$> S.baseTypeIte sym test then_ else_)
          Nothing -> E.throwError $ printf "then and else branches must have same type, \
                                         \but then has type %s \
                                         \and else has type %s"
                                         (show (S.exprType then_))
                                         (show (S.exprType else_))
      tp -> E.throwError $ printf "test expression must be a boolean; got %s" (show tp)
readIte _ _ = return Nothing

data ArrayJudgment :: BaseType -> BaseType -> * where
  ArraySingleDim :: forall idx res.
                    BaseTypeRepr res
                 -> ArrayJudgment idx (BaseArrayType (Ctx.SingleCtx idx) res)

expectArrayWithIndex :: (E.MonadError String m) => BaseTypeRepr tp1 -> BaseTypeRepr tp2 -> m (ArrayJudgment tp1 tp2)
expectArrayWithIndex dimRepr (BaseArrayRepr idxTpReprs resRepr) =
  case Ctx.viewAssign idxTpReprs of
    Ctx.AssignExtend rest idxTpRepr ->
      case Ctx.viewAssign rest of
        Ctx.AssignEmpty ->
          case testEquality idxTpRepr dimRepr of
            Just Refl -> return $ ArraySingleDim resRepr
            Nothing -> E.throwError $ unwords ["Array index type", show idxTpRepr,
                                             "does not match", show dimRepr]
        _ -> E.throwError "multidimensional arrays are not supported"
expectArrayWithIndex _ repr = E.throwError $ unwords ["expected an array, got", show repr]

-- | Parse an expression of the form @(select arr i)@
readSelect :: ExprParser sym arch sh m
readSelect (SC.SAtom (AIdent "select")) args =
  prefixError "in reading select expression: " $ do
    when (length args /= 2) (E.throwError $ printf "expecting 2 arguments, got %d" (length args))
    sym <- MR.reader getSym
    Some arr <- return $ args !! 0
    Some idx <- return $ args !! 1
    ArraySingleDim _ <- expectArrayWithIndex (S.exprType idx) (S.exprType arr)
    let idx' = Ctx.empty Ctx.:> idx
    liftIO (Just . Some <$> S.arrayLookup sym arr idx')
readSelect _ _ = return Nothing

-- | Parse an expression of the form @(store arr i e)@
readStore :: ExprParser sym arch sh m
readStore (SC.SAtom (AIdent "store")) args =
  prefixError "in reading store expression: " $ do
    when (length args /= 3) (E.throwError $ printf "expecting 3 arguments, got %d" (length args))
    sym <- MR.reader getSym
    Some arr <- return $ args !! 0
    Some idx <- return $ args !! 1
    Some expr <- return $ args !! 2
    ArraySingleDim resRepr <- expectArrayWithIndex (S.exprType idx) (S.exprType arr)
    case testEquality resRepr (S.exprType expr) of
      Just Refl ->
        let idx' = Ctx.empty Ctx.:> idx
        in liftIO (Just . Some <$> S.arrayUpdate sym arr idx' expr)
      Nothing -> E.throwError $ printf "Array result type %s does not match %s"
                                     (show resRepr)
                                     (show (S.exprType expr))
readStore _ _ = return Nothing

exprAssignment' :: (E.MonadError String m,
                    S.IsExpr ex)
                => Ctx.Assignment BaseTypeRepr ctx
                -> [Some ex]
                -> m (Ctx.Assignment ex ctx)
exprAssignment' (Ctx.viewAssign -> Ctx.AssignEmpty) [] = return Ctx.empty
exprAssignment' (Ctx.viewAssign -> Ctx.AssignExtend restTps tp) (Some e : restExprs) = do
  Refl <- case testEquality tp (S.exprType e) of
            Just pf -> return pf
            Nothing -> E.throwError ("unexpected type, assigning to: " ++ show tp ++ " from expr: " ++ show (S.exprType e))
  restAssn <- exprAssignment' restTps restExprs
  return $ restAssn Ctx.:> e
exprAssignment' _ _ = E.throwError "mismatching numbers of arguments"

exprAssignment :: (E.MonadError String m,
                   S.IsExpr ex)
               => Ctx.Assignment BaseTypeRepr ctx
               -> [Some ex]
               -> m (Ctx.Assignment ex ctx)
exprAssignment tpAssn exs = exprAssignment' tpAssn (reverse exs)

-- | Parse an expression of the form:
--
-- > ((_ call "undefined") "bv" size)
--
-- This has to be separate from the normal uninterpreted functions because the
-- type is determined by the arguments.
readUndefined :: forall sym arch sh m . ExprParser sym arch sh m
readUndefined (SC.SCons (SC.SAtom (AIdent "_"))
                (SC.SCons (SC.SAtom (AIdent "call"))
                  (SC.SCons (SC.SAtom (AString "uf.undefined"))
                    SC.SNil))) args =
  case args of
    [Some ex] ->
      case S.exprType ex of
        BaseBVRepr {}
          | Just size <- S.asUnsignedBV ex -> do
              sym <- MR.reader getSym
              case NR.someNat (fromIntegral size) of
                Just (Some nr) -> mkUndefined nr sym
                Nothing -> E.throwError $ printf "Invalid size for undefined value: %d" size
        ety -> E.throwError $ printf "Invalid expr type: %s" (show ety)
    _ -> E.throwError $ printf "Invalid argument list for undefined"
  where
    mkUndefined :: forall n . NR.NatRepr n -> sym -> m (Maybe (Some (S.SymExpr sym)))
    mkUndefined nr sym = do
      case NR.testLeq (knownNat @1) nr of
        Just NR.LeqProof -> do
          let rty = BaseBVRepr nr
          fn <- liftIO (S.freshTotalUninterpFn sym (U.makeSymbol "uf.undefined") Ctx.empty rty)
          assn <- exprAssignment (S.fnArgTypes fn) []
          (Just . Some) <$> liftIO (S.applySymFn sym fn assn)
        Nothing -> E.throwError $ printf "Invalid size for undefined value: %d" (NR.widthVal nr)
readUndefined _ _ = return Nothing

-- | Parse an expression of the form @((_ call "foo") x y ...)@
readCall :: ExprParser sym arch sh m
readCall (SC.SCons (SC.SAtom (AIdent "_"))
            (SC.SCons (SC.SAtom (AIdent "call"))
               (SC.SCons (SC.SAtom (AString fnName))
                  SC.SNil))) args =
  prefixError ("in reading call '" <> fnName <> "' expression: ") $ do
    sym <- MR.reader getSym
    fns <- MR.reader (envFunctions . getEnv)
    SomeSome fn <- case Map.lookup fnName fns of
      Just (fn, _) -> return fn
      Nothing ->
        let template = case take 3 fnName of
              "uf." -> "uninterpreted function '%s' is not defined"
              "df." -> "library function '%s' is not defined"
              _     -> "unrecognized function prefix: '%s'"
        in E.throwError $ printf template fnName
    assn <- exprAssignment (S.fnArgTypes fn) args
    liftIO (Just . Some <$> S.applySymFn sym fn assn)
readCall _ _ = return Nothing

-- | Parse an arbitrary expression.
readExpr :: forall sym m arch sh
          . (S.IsSymExprBuilder sym,
             Monad m,
             E.MonadError String m,
             A.Architecture arch,
             MR.MonadReader (DefsInfo sym arch sh) m,
             MonadIO m)
         => SC.SExpr FAtom
         -> m (Some (S.SymExpr sym))
readExpr SC.SNil = E.throwError "found nil where expected an expression"
readExpr (SC.SAtom (AInt _)) = E.throwError "found int where expected an expression; perhaps you wanted a bitvector?"
readExpr (SC.SAtom (AString op)) = do
  -- This is an uninterpreted function.
  sym <- MR.reader getSym
  case userSymbol op of
    Right opSym -> do
      e <- liftIO $ S.freshTotalUninterpFn sym opSym Ctx.empty (BaseStructRepr Ctx.empty)
      f <- liftIO $ S.applySymFn sym e Ctx.empty
      return $ Some f
    Left _ -> E.throwError $ printf "couldn't parse expression %s" (show op)
readExpr (SC.SAtom (ABV len val)) = do
  -- This is a bitvector literal.
  sym <- MR.reader getSym
  -- The following two patterns should never fail, given that during parsing we
  -- can only construct BVs with positive length.
  Just (Some lenRepr) <- return $ someNat (toInteger len)
  let Just pf = isPosNat lenRepr
  liftIO $ withLeqProof pf (Some <$> S.bvLit sym lenRepr val)
readExpr (SC.SAtom paramRaw) = do
  -- This is a parameter (i.e., variable).
  DefsInfo { getOpNameList = opNames
           , getSym = sym
           , getOpVarList = opVars
           , getLitLookup = litLookup
           } <- MR.ask
  param <- readParameter (Proxy @arch) opNames paramRaw
  case param of
    Some (ParsedOperandParameter _ idx) -> return . Some . S.varExpr sym $ (opVars SL.!! idx)
    Some (ParsedLiteralParameter lit) -> maybe (E.throwError ("not declared as input but saw unknown literal param: " ++ showF lit))
                                   (return . Some) $ litLookup lit
readExpr (SC.SCons opRaw argsRaw) = do
  -- This is a function application.
  args <- readExprs argsRaw
  parseAttempt <- U.sequenceMaybes $ map (\f -> f opRaw args)
    [ readConcat
    , readExtract
    , readExtend
    , readBVUnop
    , readBVBinop
    , readBoolUnop
    , readBoolBinop
    , readFPUnOp
    , readFPBinOp
    , readFPTernOp
    , readEq
    , readIte
    , readSelect
    , readStore
    , readUndefined
    , readCall
    ]
  case parseAttempt of
    Just expr -> return expr
    Nothing -> E.throwError $ printf "couldn't parse expression %s" (show opRaw)

-- | Parse multiple expressions in a list.
readExprs :: (S.IsSymExprBuilder sym,
              Monad m,
              E.MonadError String m,
              A.Architecture arch,
              MR.MonadReader (DefsInfo sym arch sh) m,
              MonadIO m)
          => SC.SExpr FAtom
          -> m [Some (S.SymExpr sym)]
readExprs SC.SNil = return []
readExprs (SC.SAtom _) = E.throwError "found atom where expected a list of expressions"
readExprs (SC.SCons e rest) = do
  e' <- readExpr e
  rest' <- readExprs rest
  return $ e' : rest'

-- | Parse the whole definitions expression, e.g.:
--
-- > ((rt . (bvadd ra rb))
-- >  ('ca . #b1))
--
readDefs :: forall sym m arch sh
          . (S.IsSymExprBuilder sym,
             Monad m,
             E.MonadError String m,
             A.Architecture arch,
             MR.MonadReader (DefsInfo sym arch (OperandTypes arch sh)) m,
             MonadIO m,
             ShowF (S.SymExpr sym))
         => A.ShapeRepr arch sh
         -> SC.SExpr FAtom
         -> m (MapF.MapF (Parameter arch sh) (S.SymExpr sym))
readDefs _ SC.SNil = return MapF.empty
readDefs shapeRepr (SC.SCons (SC.SCons (SC.SAtom p) (SC.SCons defRaw SC.SNil)) rest) = do
  oplist <- MR.reader getOpNameList
  Some param <- mapSome (toParameter shapeRepr) <$> readParameter (Proxy @arch) oplist p
  Some def <- readExpr defRaw
  Refl <- fromMaybeError ("mismatching types of parameter and expression for " ++ showF param) $
            testEquality (paramType param) (S.exprType def)
  rest' <- prefixError (", defining " <> showF def <> " ... ") $ readDefs shapeRepr rest
  return $ MapF.insert param def rest'
readDefs shapeRepr (SC.SCons (SC.SCons (SC.SCons mUF (SC.SCons (SC.SAtom p) SC.SNil)) (SC.SCons defRaw SC.SNil)) rest)
  | Just funcName <- matchUF mUF = prefixError (", processing uninterpreted function " <> show funcName <> " ... ") $ do
    oplist <- MR.reader getOpNameList
    Some param <- mapSome (toParameter shapeRepr) <$> readParameter (Proxy @arch) oplist p
    fns <- MR.reader (envFunctions . getEnv)
    case Map.lookup funcName fns of
      Just (_, Some rep) -> do
        Some def <- readExpr defRaw
        Refl <- fromMaybeError ("mismatching types of parameter and expression for " ++ showF param) $
                  testEquality rep (S.exprType def)
        rest' <- readDefs shapeRepr rest
        case param of
          LiteralParameter {} -> E.throwError "Literals are not allowed as arguments to parameter functions"
          FunctionParameter {} -> E.throwError "Nested parameter functions are not allowed"
          OperandParameter orep oix ->
            return $ MapF.insert (FunctionParameter funcName (WrappedOperand orep oix) rep) def rest'
      _ -> E.throwError ("Missing type repr for uninterpreted function " ++ show funcName)
readDefs _ _ = E.throwError "invalid defs structure"

matchUF :: SC.SExpr FAtom -> Maybe String
matchUF se =
  case se of
    SC.SCons (SC.SAtom (AIdent "_"))
             (SC.SCons (SC.SAtom (AIdent "call"))
                       (SC.SCons (SC.SAtom (AString fnName))
                                 SC.SNil)) -> Just fnName
    _ -> Nothing

-- | Parse the whole definition of a templated formula, inside an appropriate
-- monad.
readFormula' :: forall sym arch (sh :: [Symbol]) m.
                (S.IsSymExprBuilder sym,
                 E.MonadError String m,
                 MonadIO m,
                 A.Architecture arch,
                 ShowF (S.SymExpr sym),
                 U.HasLogCfg)
             => sym
             -> FormulaEnv sym arch
             -> A.ShapeRepr arch sh
             -> T.Text
             -> m (ParameterizedFormula sym arch sh)
readFormula' sym env repr text = do
  sexpr <- case parseLL text of
             Left err -> E.throwError err
             Right res -> return res
  let firstLine = show $ fmap T.unpack $ take 1 $ T.lines text
  liftIO $ U.logIO U.Info $ "readFormula' of " ++ (show $ T.length text) ++ " bytes " ++ firstLine
  liftIO $ U.logIO U.Debug $ "readFormula' shaperepr " ++ (A.showShapeRepr (Proxy @arch) repr)
  -- Extract the raw s-expressions for the three components.
  (opsRaw, inputsRaw, defsRaw) <- case sexpr of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "operands")) (SC.SCons ops SC.SNil))
      (SC.SCons (SC.SCons (SC.SAtom (AIdent "in")) (SC.SCons inputs SC.SNil))
       (SC.SCons (SC.SCons (SC.SAtom (AIdent "defs")) (SC.SCons defs SC.SNil))
         SC.SNil))
      -> return (ops, inputs, defs)
    _ -> E.throwError "invalid top-level structure"

  -- Most of the following bindings have type annotations not because inference
  -- fails, but because with all the type-level hackery we have going on, it
  -- greatly helps human comprehension.

  -- Build the operand list from the given s-expression, validating that it
  -- matches the correct shape as we go.
  let strShape = A.showShapeRepr (Proxy @arch) repr
  operands :: SL.List OpData (OperandTypes arch sh)
    <- prefixError ("invalid operand structure (expected " ++ strShape ++
                    ") from " ++ show opsRaw) $
       buildOperandList' (Proxy @arch) repr opsRaw

  inputs :: [Some (ParsedParameter arch (OperandTypes arch sh))]
    <- readInputs @m @arch @(OperandTypes arch sh) operands inputsRaw

  let mkOperandVar :: forall tp. OpData tp -> m (S.BoundVar sym tp)
      mkOperandVar (OpData name tpRepr) =
        let symbol = U.makeSymbol (operandVarPrefix ++ name)
        in liftIO $ S.freshBoundVar sym symbol tpRepr

  opVarList :: SL.List (S.BoundVar sym) (OperandTypes arch sh)
    <- traverseFC mkOperandVar @(OperandTypes arch sh) operands

  -- NOTE: At the moment, we just trust that the semantics definition declares
  -- the correct input operands; instead of validating it, we generate BoundVars
  -- for *all* inputs (partially because it is unclear how to treat immediates
  -- -- do they count as inputs?).

  let mkLiteralVar :: forall tp. BaseTypeRepr tp -> A.Location arch tp -> m (S.BoundVar sym tp)
      mkLiteralVar tpRepr loc =
        let symbol = U.makeSymbol (literalVarPrefix ++ showF loc)
        in liftIO $ S.freshBoundVar sym symbol tpRepr

      buildLitVarMap :: Some (ParsedParameter arch (OperandTypes arch sh))
                     -> MapF.MapF (A.Location arch) (S.BoundVar sym)
                     -> m (MapF.MapF (A.Location arch) (S.BoundVar sym))
      buildLitVarMap (Some (ParsedLiteralParameter loc)) m = (\v -> MapF.insert loc v m) <$> mkLiteralVar (A.locationType loc) loc
      buildLitVarMap (Some (ParsedOperandParameter _ _))        m = return m

  litVars :: MapF.MapF (A.Location arch) (S.BoundVar sym)
    <- foldrM buildLitVarMap MapF.empty inputs

  defs <- MR.runReaderT (readDefs repr defsRaw) $
            (DefsInfo { getSym = sym
                      , getEnv = env
                      , getLitLookup = \loc -> S.varExpr sym <$> flip MapF.lookup litVars loc
                      , getOpVarList = opVarList
                      , getOpNameList = operands
                      })

  let finalInputs :: [Some (Parameter arch sh)]
      finalInputs = mapSome (toParameter repr) <$> inputs
      finalOpVarList :: SL.List (BV.BoundVar sym arch) sh
      finalOpVarList =
        -- Wrap each operand variable using 'BV.BoundVar'
        TL.mapFromMapped (Proxy @(OperandTypeWrapper arch)) BV.BoundVar repr opVarList

  return $
    ParameterizedFormula { pfUses = Set.fromList finalInputs
                         , pfOperandVars = finalOpVarList
                         , pfLiteralVars = litVars
                         , pfDefs = defs
                         }

-- | Parse the definition of a templated formula.
readFormula :: (S.IsSymExprBuilder sym,
                A.Architecture arch,
                ShowF (S.SymExpr sym),
                U.HasLogCfg)
            => sym
            -> FormulaEnv sym arch
            -> A.ShapeRepr arch sh
            -> T.Text
            -> IO (Either String (ParameterizedFormula sym arch sh))
readFormula sym env repr text = E.runExceptT $ readFormula' sym env repr text

-- | Read a templated formula definition from file, then parse it.
readFormulaFromFile :: (S.IsSymExprBuilder sym,
                        A.Architecture arch,
                        ShowF (S.SymExpr sym),
                        U.HasLogCfg)
                    => sym
                    -> FormulaEnv sym arch
                    -> A.ShapeRepr arch sh
                    -> FilePath
                    -> IO (Either String (ParameterizedFormula sym arch sh))
readFormulaFromFile sym env repr fp = do
  liftIO $ U.logIO U.Info $ "readFormulaFromFile " ++ fp
  readFormula sym env repr =<< T.readFile fp

-- | Parse the whole definition of a defined function, inside an appropriate
-- monad.
readDefinedFunction' :: forall sym arch m.
                        (S.IsExprBuilder sym,
                         S.IsSymInterface sym,
                         E.MonadError String m,
                         MonadIO m,
                         A.Architecture arch,
                         ShowF (S.SymExpr sym),
                         U.HasLogCfg)
                     => sym
                     -> FormulaEnv sym arch
                     -> T.Text
                     -> m (Some (FunctionFormula sym))
readDefinedFunction' sym env text = do
  sexpr <- case parseLL text of
             Left err -> E.throwError err
             Right res -> return res
  let firstLine = show $ fmap T.unpack $ take 1 $ T.lines text
  liftIO $ U.logIO U.Info $
    "readDefinedFunction' of " ++ (show $ T.length text) ++ " bytes " ++ firstLine
  -- Extract the raw s-expressions for the four components.
  (name, argsRaw, retTypeRaw, bodyRaw) <- case sexpr of
    SC.SCons (SC.SCons (SC.SAtom (AIdent "function"))
              (SC.SCons (SC.SAtom (AIdent name)) SC.SNil))
      (SC.SCons (SC.SCons (SC.SAtom (AIdent "arguments")) (SC.SCons args SC.SNil))
       (SC.SCons (SC.SCons (SC.SAtom (AIdent "return")) (SC.SCons retType SC.SNil))
         (SC.SCons (SC.SCons (SC.SAtom (AIdent "body")) (SC.SCons body SC.SNil))
           SC.SNil)))
      -> return (name, args, retType, body)
    _ -> E.throwError "invalid top-level structure"

  -- Most of the following bindings have type annotations not because inference
  -- fails, but because with all the type-level hackery we have going on, it
  -- greatly helps human comprehension.

  -- Build the argument list from the given s-expression. Unlike with a formula,
  -- we don't know the arguments' types beforehand.
  Some (arguments :: SL.List OpData sh)
    <- buildArgumentList' argsRaw

  Some (retTypeRepr :: BaseTypeRepr retType)
    <- readBaseType retTypeRaw

  let mkArgumentVar :: forall tp. OpData tp -> m (S.BoundVar sym tp)
      mkArgumentVar (OpData varName tpRepr) =
        let symbol = U.makeSymbol (argumentVarPrefix ++ varName)
        in liftIO $ S.freshBoundVar sym symbol tpRepr

  argVarList :: SL.List (S.BoundVar sym) sh
    <- traverseFC mkArgumentVar arguments

  argTypeReprs :: SL.List BaseTypeRepr sh
    <- traverseFC (\(OpData _ tpRepr) -> return tpRepr) arguments

  Some body <- MR.runReaderT (readExpr bodyRaw) $
    DefsInfo { getSym = sym
             , getEnv = env
             , getLitLookup = const Nothing
             , getOpVarList = argVarList
             , getOpNameList = arguments
             }

  let actualTypeRepr = S.exprType body
  Refl <- case testEquality retTypeRepr actualTypeRepr of
    Just r -> return r
    Nothing -> E.throwError $
      "Body has wrong type: expected " ++ show retTypeRepr ++
      " but found " ++ show actualTypeRepr

  let symbol = U.makeSymbol name
      argVarAssignment = TL.toAssignmentFwd argVarList
      expand args = allFC S.baseIsConcrete args

  symFn <- liftIO $ S.definedFn sym symbol argVarAssignment body expand
  return $ Some (FunctionFormula { ffName = name
                                 , ffArgTypes = argTypeReprs
                                 , ffArgVars = argVarList
                                 , ffRetType = retTypeRepr
                                 , ffDef = symFn })

-- | Parse the definition of a templated formula.
readDefinedFunction :: (S.IsExprBuilder sym,
                        S.IsSymInterface sym,
                        A.Architecture arch,
                        ShowF (S.SymExpr sym),
                        U.HasLogCfg)
                    => sym
                    -> FormulaEnv sym arch
                    -> T.Text
                    -> IO (Either String (Some (FunctionFormula sym)))
readDefinedFunction sym env text = E.runExceptT $ readDefinedFunction' sym env text

-- | Read a defined function definition from a file, then parse it.
readDefinedFunctionFromFile :: (S.IsExprBuilder sym,
                                S.IsSymInterface sym,
                                A.Architecture arch,
                                ShowF (S.SymExpr sym),
                                U.HasLogCfg)
                    => sym
                    -> FormulaEnv sym arch
                    -> FilePath
                    -> IO (Either String (Some (FunctionFormula sym)))
readDefinedFunctionFromFile sym env fp = do
  liftIO $ U.logIO U.Info $ "readDefinedFunctionFromFile " ++ fp
  readDefinedFunction sym env =<< T.readFile fp
