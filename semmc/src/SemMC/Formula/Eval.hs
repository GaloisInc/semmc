{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Definitions of evaluators over parameterized formulas
--
-- This module defines a function that can be used to evaluate uninterpreted
-- functions at formula instantiation time.  This is meant to be used to
-- eliminate uninterpreted functions that can be evaluated statically.  For
-- example, the PowerPC semantics refer to a few uninterpreted functions that
-- split memory reference operands into a base register and an offset.  These
-- values are known at formula instantiation time (which can be considered
-- "static").
module SemMC.Formula.Eval (
  Evaluator(..),
  lookupVarInFormulaOperandList,
  exprForRegister,
  evaluateFunctions
  ) where

import           Control.Arrow                      ( first )
import           Control.Monad.State
import           Data.Functor.Product  ( Product(Pair) )
import qualified Data.Parameterized.Context         as Ctx
import qualified Data.Parameterized.List            as SL
import qualified Data.Parameterized.Map             as M
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC

import qualified Data.Text                          as T
import           What4.Interface as S
import qualified What4.Expr.Builder as S
import qualified What4.Symbol as S
import           Lang.Crucible.Types
import qualified SemMC.Architecture.Internal        as A
import           SemMC.Architecture.Location        as A
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Formula as F
import           SemMC.Util ( makeSymbol )

type Sym t st = S.ExprBuilder t st

type Literals arch sym = M.MapF (Location arch) (BoundVar sym)

-- | This type encapsulates an evaluator for operations represented as
-- uninterpreted functions in semantics.  It may seem strange to interpret
-- "uninterpreted functions" (UFs); we use UFs to represent operations in the
-- semantics that can't be expressed using more typical SMT operations.  The most
-- common examples in the codebase are:
--
-- 1) Extracting sub-components from compound operands in instructions (like a
--    literal bundled with a shift amount)
-- 2) Testing the number of a register (e.g., testing if a register is r0)
--
-- While the type isn't much of an abstraction barrier, it is convenient to hide
-- the forall under a data constructor rather than a type alias.
--
-- * The 'Sym' is a symbolic expression builder from the what4 library
-- * The 'ParameterizedFormula' is the formula whose semantics we are currently evaluating
-- * The 'SL.List' contains the concrete operands to the instruction whose semantics we are evaluating
-- * The 'Ctx.Assignment' is the list of operands of the uninterpreted function being interpreted
-- * The 'BaseTypeRepr' is the expected return type of the uninterpreted function
--
-- Note that the type parameters for the *instruction* operand list and the
-- *uninterpreted function* operand list (@sh@ and @u@, respectively) explicitly
-- do /not/ match up, as the UF and instructions take different operands.
--
-- We need to pass the return type 'BaseTypeRepr' in so that we can know at the
-- call site that the expression produced by the evaluator is correctly-typed.
data Evaluator arch t =
  Evaluator (forall tp u st sh
               . Sym t st
              -> ParameterizedFormula (Sym t st) arch sh
              -> SL.List (A.Operand arch) sh
              -> Ctx.Assignment (S.Expr t) u
              -> BaseTypeRepr tp
              -> IO (S.Expr t tp, Literals arch (Sym t st)))

-- | Given a 'S.BoundVar', attempt to find its index in the operand list for the
-- 'F.ParameterizedFormula'
--
-- This is meant to be used in definitions of 'Evaluator's, where we need to
-- match actual operands to their bound variables in a parameterized formula.
lookupVarInFormulaOperandList :: (TestEquality (S.BoundVar sym))
                              => S.BoundVar sym tp
                              -> F.ParameterizedFormula sym arch sh
                              -> Maybe (Some (SL.Index sh))
lookupVarInFormulaOperandList b pf =
  b `elemIndexSL` F.pfOperandVars pf

elemIndexSL :: forall sym arch sh tp . (TestEquality (S.BoundVar sym)) => S.BoundVar sym tp -> SL.List (BV.BoundVar sym arch) sh -> Maybe (Some (SL.Index sh))
elemIndexSL target = go . SL.imap Pair
  where
    go :: forall sh' . SL.List (Product (SL.Index sh) (BV.BoundVar sym arch)) sh' -> Maybe (Some (SL.Index sh))
    go l =
      case l of
        SL.Nil -> Nothing
        Pair idx (BV.BoundVar elt) SL.:< rest
          | Just Refl <- testEquality elt target -> Just (Some idx)
          | otherwise -> go rest

-- | Find a symbolic expression that represents the register passed in (as an 'A.Location')
--
-- This function checks to see if the register has already been referenced in
-- the formula literals map or the operand list.  If it has, the corresponding
-- expression is re-used.
--
-- If the register hasn't yet been assigned an expression, this function
-- allocates a new symbolic expression for it and maps the register to the new
-- expression in the returned map of locations to bound variables.  The returned
-- map is merged into the literals map in the outer evaluation logic.
exprForRegister :: forall arch t st sh tp tp0
                 . (A.IsLocation (A.Location arch))
                => S.ExprBuilder t st
                -- ^ The symbolic backend
                -> F.ParameterizedFormula (S.ExprBuilder t st) arch sh
                -- ^ The semantics formula for the instruction being interpreted
                -> SL.List (A.Operand arch) sh
                -- ^ The actual arguments we are instantiating the parameterized formula with
                -> (forall tp1 . A.Operand arch tp1 -> Bool)
                -- ^ A predicate to test whether the 'A.Location' (below)
                -- matches an 'A.Operand' in the operands list; this is an
                -- opaque predicate because translating a 'A.Location' into an
                -- 'A.Operand' is inherently architecture-specific, and must be
                -- done by an architecture-specific backend.
                -> A.Location arch tp0
                -- ^ The register whose value (expression) should be looked up
                -> BaseTypeRepr tp
                -- ^ The return type of the 'S.Expr' that the caller is expecting.
                -> IO (S.Expr t tp, M.MapF (A.Location arch) (S.BoundVar (S.ExprBuilder t st)))
exprForRegister sym pf operands test reg resultRepr =
  case M.lookup reg (F.pfLiteralVars pf) of
    -- If the variable has already been allocated a LiteralVar (in
    -- pfLiteralVars), we use that variable as our expression.
    Just bvar ->
      case testEquality resultRepr (S.bvarType bvar) of
        Just Refl -> return (S.varExpr sym bvar, M.empty)
        Nothing -> error ("The expression for " ++ M.showF reg ++ " has the wrong type; the caller expected " ++ show resultRepr)
    -- Otherwise, We check the operands list to see if there was a variable
    -- allocated in there for the same register.  If not, we allocate a fresh
    -- variable and return it in the extra mapping that will be merged with the
    -- literal vars.  Note that this is the only case where we actually return a
    -- non-empty map.
    Nothing
      | Just (Some (BV.BoundVar bvar)) <- findOperandVarForRegister pf operands test ->
        case testEquality (S.bvarType bvar) resultRepr of
          Just Refl -> return (S.varExpr sym bvar, M.empty)
          Nothing -> error ("Register operand has type " ++ show (S.bvarType bvar) ++ " but the caller expected " ++ show resultRepr)
      | otherwise -> do
          let usym = makeSymbol ("reg_" ++ M.showF reg)
          s <- S.freshBoundVar sym usym (A.locationType reg)
          case testEquality (S.bvarType s) resultRepr of
            Just Refl -> return (S.varExpr sym s, M.singleton reg s)
            Nothing -> error ("Created a fresh variable of type " ++ show (S.bvarType s) ++ " but the caller expected " ++ show resultRepr)

-- | Look a 'A.Location' in the operand list (@'SL.List' ('A.Operand' arch) sh@)
-- and, if it is found, return the corresponding 'BV.BoundVar' from the
-- 'F.ParameterizedFormula'
--
-- Because we cannot generically convert a 'A.Location' to an 'A.Operand', we
-- pass in a predicate to test if an 'A.Operand' matches the target
-- 'A.Location'.  This predicate must be provided by an architecture-specific
-- backend.
findOperandVarForRegister :: forall t st arch sh
                           . F.ParameterizedFormula (S.ExprBuilder t st) arch sh
                          -> SL.List (A.Operand arch) sh
                          -> (forall tp1 . A.Operand arch tp1 -> Bool)
                          -> Maybe (Some (BV.BoundVar (S.ExprBuilder t st) arch))
findOperandVarForRegister pf operands0 test = go (SL.imap Pair operands0)
  where
    go :: forall sh' . SL.List (Product (SL.Index sh) (A.Operand arch)) sh' -> Maybe (Some (BV.BoundVar (S.ExprBuilder t st) arch))
    go operands =
      case operands of
        SL.Nil -> Nothing
        Pair idx op SL.:< rest
          | test op -> Just (Some (F.pfOperandVars pf SL.!! idx))
          | otherwise -> go rest

-- | See `evaluateFunctions'`
evaluateFunctions
  :: M.OrdF (Location arch)
  => Sym t st
  -> ParameterizedFormula (Sym t st) arch sh
  -> SL.List (A.Operand arch) sh
  -> [(String, Evaluator arch t)]
  -> S.Expr t tp
  -> IO (S.Expr t tp, M.MapF (Location arch) (S.ExprBoundVar t))
evaluateFunctions sym pf operands rewriters elt =
  flip runStateT M.empty
    (evaluateFunctions' sym pf operands rewriters elt)

-- | Recursively applies rewrite rules to all uninterpreted functions present in a formula.
evaluateFunctions'
  :: M.OrdF (Location arch)
  => Sym t st
  -> ParameterizedFormula (Sym t st) arch sh
  -> SL.List (A.Operand arch) sh
  -> [(String, Evaluator arch t)]
  -> S.Expr t tp
  -> StateT (Literals arch (Sym t st)) IO (S.Expr t tp)
evaluateFunctions' sym pf operands rewriters e =
  case e of
    S.SemiRingLiteral {} -> return e
    S.BVExpr {} -> return e
    S.BoundVarExpr {} -> return e
    S.AppExpr a -> do
      app <- S.traverseApp (evaluateFunctions' sym pf operands rewriters) (S.appExprApp a)
      liftIO $ S.sbMakeExpr sym app
    S.NonceAppExpr nonceApp -> do
      case S.nonceExprApp nonceApp of
        S.Forall{} -> error "evaluateFunctions: Forall Not implemented"
        S.Exists{} -> error "evaluateFunctions: Exists Not implemented"
        S.ArrayFromFn{} ->
          error "evaluateFunctions: ArrayFromFn Not implemented"
        S.MapOverArrays{} ->
          error "evaluateFunctions: MapOverArrays Not implemented"
        S.ArrayTrueOnEntries{} ->
          error "evaluateFunctions: ArrayTrueOnEntries Not implemented"
        S.FnApp symFun assignment -> do
          let key = T.unpack $ S.solverSymbolAsText (S.symFnName symFun)
              rs = first normalizeUFName <$> rewriters
          assignment' <- traverseFC (evaluateFunctions' sym pf operands rs) assignment
          case lookup key rewriters of
            Just (Evaluator evaluator) -> do
              (e',m') <- liftIO $ evaluator sym pf operands assignment' (S.exprType e)
              modify' (m' `M.union`)
              pure e'
            Nothing ->
              liftIO $ applySymFn sym symFun assignment

-- | Normalize the name of an uninterpreted function to the SimpleBuilder-friendly form
--
-- Our mapping from uninterpreted function names to evaluators uses names in the
-- format they appear in our semantics files.  Those names sometimes contain
-- periods.
--
-- SimpleBuilder has a restriction where user-provided uninterpreted function
-- names cannot contain periods (it replaces all periods it finds with
-- underscores).  As we traverse an expression (an 'S.Elt'), we will find
-- uninterpreted functions with names in the SimpleBuilder format.  To correctly
-- match the functions we find in expressions to the evaluators in the table, we
-- need to /normalize/ the names in the table so that they are consistent with
-- the names in the expression tree.  To do that, we need to replace every
-- period in the name with an underscore.
normalizeUFName :: String -> String
normalizeUFName = map (\c -> if c == '.' then '_' else c)
