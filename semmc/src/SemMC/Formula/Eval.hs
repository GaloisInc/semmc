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
  lookupVarInFormulaOperandList,
  evalBitvectorExtractor,
  evalBitvectorExtractorWith,
  evalRegExtractor,
  evaluateFunctions
  ) where

import           Control.Arrow                      ( first )
import           Data.Functor.Product  ( Product(Pair) )
import qualified Data.Parameterized.Context         as Ctx
import qualified Data.Parameterized.List            as SL
import qualified Data.Parameterized.Map             as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC
import           GHC.Stack ( HasCallStack )

import qualified Data.Text                          as T
import           What4.Interface as S
import qualified What4.Expr.Builder as S
import qualified What4.Symbol as S
import qualified SemMC.Architecture as A
import           SemMC.Architecture.Location        as A
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Formula as F

type Sym t st fs = S.ExprBuilder t st fs

-- | Given a 'S.BoundVar', attempt to find its index in the operand list for the
-- 'F.ParameterizedFormula'
--
-- This is meant to be used in definitions of 'A.Evaluator's, where we need to
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

evalRegExtractor :: (MapF.ShowF (A.Operand arch), A.IsLocation (A.Location arch), MapF.ShowF (A.OperandComponents arch (S.ExprBuilder t st fs)))
                 => String
                 -> (forall s . A.AllocatedOperand arch (Sym t st fs) s -> Maybe (Some (A.Location arch)))
                 -> A.Evaluator arch t st fs
evalRegExtractor operationName match =
  A.Evaluator $ \_sym pf operands ufArguments locExpr resultRepr ->
    case ufArguments of
      Ctx.Empty Ctx.:> S.BoundVarExpr ufArg ->
        case ufArg `lookupVarInFormulaOperandList` pf of
          Nothing -> error ("Argument to " ++ operationName ++ " is not a formula parameter")
          Just (Some idx) -> do
            let operand = operands SL.!! idx
            case match operand of
              Nothing -> error ("Unexpected operand type in " ++ operationName ++ ": " ++ show operand)
              Just (Some reg) -> do
                rexp <- locExpr reg
                case testEquality (S.exprType rexp) resultRepr of
                  Just Refl -> return rexp
                  Nothing -> error ("Unexpected operand type in " ++ operationName ++ ": " ++ MapF.showF (S.exprType rexp) ++ " while the caller expected " ++ MapF.showF resultRepr)
      _ -> error ("Unexpected argument list to " ++ operationName)

-- | A generic skeleton for evaluation functions that extract bitvector fields from operands
--
-- This isn't suitable for the versions that extract registers
evalBitvectorExtractor :: ( 1 <= n, HasCallStack
                          , MapF.ShowF (A.Operand arch)
                          , MapF.ShowF (A.OperandComponents arch (Sym t st fs))
                          )
                       => String
                       -> NatRepr n
                       -> (forall x . A.OperandComponents arch (Sym t st fs) x -> NatRepr n -> Maybe (S.SymExpr (Sym t st fs) (BaseBVType n)))
                       -> A.Evaluator arch t st fs
evalBitvectorExtractor =
  evalBitvectorExtractorWith identityTransform

identityTransform :: (1 <= n) => sym -> S.SymExpr sym (BaseBVType n) -> IO (S.SymExpr sym (BaseBVType n))
identityTransform _ e = return e

evalBitvectorExtractorWith :: ( HasCallStack, 1 <= n
                              , MapF.ShowF (A.Operand arch)
                              , MapF.ShowF (A.OperandComponents arch (Sym t st fs))
                              )
                           => (Sym t st fs -> S.SymExpr (Sym t st fs) (BaseBVType n) -> IO (S.SymExpr (Sym t st fs) tp))
                           -> String
                           -> NatRepr n
                           -> (forall s . A.OperandComponents arch (Sym t st fs) s -> NatRepr n -> Maybe (S.SymExpr (Sym t st fs) (BaseBVType n)))
                           -> A.Evaluator arch t st fs
evalBitvectorExtractorWith wrapResultWith operationName litRep match =
  A.Evaluator $ \sym pf operands ufArguments _locExpr resultRepr ->
    case ufArguments of
      Ctx.Empty Ctx.:> S.BoundVarExpr ufArg ->
        case ufArg `lookupVarInFormulaOperandList` pf of
          Nothing -> error ("Argument to " ++ operationName ++ " is not a formula parameter: " ++ MapF.showF ufArg)
          Just (Some idx) -> do
            let op = operands SL.!! idx
            case op of
              A.ValueOperand se
                | Just Refl <- testEquality (S.exprType se) (BaseBVRepr litRep) -> do
                  res <- wrapResultWith sym se
                  case testEquality (S.exprType res) resultRepr of
                    Just Refl -> return res
                    Nothing -> error (operationName ++ " returns a " ++ show (S.exprType res) ++ " but the caller expected " ++ show resultRepr)
                | otherwise -> error ("Invalid operand type in " ++ operationName ++ ": expected " ++ show (BaseBVRepr litRep) ++ " but got " ++ show (S.exprType se))
              A.LocationOperand _ se
                | Just Refl <- testEquality (S.exprType se) (BaseBVRepr litRep) -> do
                  res <- wrapResultWith sym se
                  case testEquality (S.exprType res) resultRepr of
                    Just Refl -> return res
                    Nothing -> error (operationName ++ " returns a " ++ show (S.exprType res) ++ " but the caller expected " ++ show resultRepr)
                | otherwise -> error ("Invalid operand type in " ++ operationName ++ ": expected " ++ show (BaseBVRepr litRep) ++ " but got " ++ show (S.exprType se))
              A.CompoundOperand oc ->
                case match oc litRep of
                  Just se -> do
                    res <- wrapResultWith sym se
                    case testEquality (S.exprType res) resultRepr of
                      Just Refl -> return res
                      Nothing -> error (operationName ++ " returns a " ++ show (S.exprType res) ++ " but the caller expected " ++ show resultRepr)
                  Nothing -> error (operationName ++ " has an unexpected operand type: " ++ MapF.showF oc)
      _ -> error ("Unexpected argument list to " ++ operationName)


-- | See `evaluateFunctions'`
evaluateFunctions
  :: MapF.OrdF (Location arch)
  => Sym t st fs
  -> ParameterizedFormula (Sym t st fs) arch sh
  -> SL.List (A.AllocatedOperand arch (Sym t st fs)) sh
  -> (forall ltp . A.Location arch ltp -> IO (S.Expr t ltp))
  -> [(String, A.Evaluator arch t st fs)]
  -> S.Expr t tp
  -> IO (S.Expr t tp)
evaluateFunctions sym pf operands locExpr rewriters elt =
  evaluateFunctions' sym pf operands locExpr rewriters elt

-- | Recursively applies rewrite rules to all uninterpreted functions present in a formula.
evaluateFunctions'
  :: MapF.OrdF (Location arch)
  => Sym t st fs
  -> ParameterizedFormula (Sym t st fs) arch sh
  -> SL.List (A.AllocatedOperand arch (Sym t st fs)) sh
  -> (forall ltp . A.Location arch ltp -> IO (S.Expr t ltp))
  -> [(String, A.Evaluator arch t st fs)]
  -> S.Expr t tp
  -> IO (S.Expr t tp)
evaluateFunctions' sym pf operands locExpr rewriters e =
  case e of
    S.SemiRingLiteral {} -> return e
    S.BoundVarExpr {} -> return e
    S.StringExpr {} -> return e
    S.BoolExpr {} -> return e
    S.AppExpr a -> do
      app <- S.traverseApp (evaluateFunctions' sym pf operands locExpr rewriters) (S.appExprApp a)
      S.sbMakeExpr sym app
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
          assignment' <- traverseFC (evaluateFunctions' sym pf operands locExpr rs) assignment
          case lookup key  rewriters of
            Just (A.Evaluator evaluator) ->
              evaluator sym pf operands assignment' locExpr (S.exprType e)
            Nothing ->
              applySymFn sym symFun assignment'

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
