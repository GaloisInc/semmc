{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
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
  evaluateFunctions
  ) where

import           Control.Arrow                      (first, (&&&))
import           Control.Monad.State
import qualified Data.Parameterized.Context         as Ctx
import qualified Data.Parameterized.List            as SL
import qualified Data.Parameterized.Map             as M
import           Data.Parameterized.TraversableFC

import qualified Data.Text                          as T
import           Lang.Crucible.Solver.Interface
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Symbol        as S
import           Lang.Crucible.Types
import qualified SemMC.Architecture.Internal        as A
import           SemMC.Architecture.Location
import           SemMC.Formula.Formula

type Sym t st = S.SimpleBuilder t st

type Literals arch sym = M.MapF (Location arch) (BoundVar sym)

-- | Type used to encapsulate rewriting of formulas related to register unpacking across different ISAs.
data Evaluator arch t =
  Evaluator (forall tp u st sh
               . Sym t st
              -> ParameterizedFormula (Sym t st) arch sh
              -> SL.List (A.Operand arch) sh
              -> Ctx.Assignment (S.Elt t) u
              -> BaseTypeRepr tp
              -> IO (S.Elt t tp, Literals arch (Sym t st)))

-- | See `evaluateFunctions'`
evaluateFunctions
  :: M.OrdF (Location arch)
  => Sym t st
  -> ParameterizedFormula (Sym t st) arch sh
  -> SL.List (A.Operand arch) sh
  -> [(String, Evaluator arch t)]
  -> S.Elt t tp
  -> IO (S.Elt t tp, M.MapF (Location arch) (S.SimpleBoundVar t))
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
  -> S.Elt t tp
  -> StateT (Literals arch (Sym t st)) IO (S.Elt t tp)
evaluateFunctions' sym pf operands rewriters e =
  case e of
    S.SemiRingLiteral {} -> return e
    S.BVElt {} -> return e
    S.BoundVarElt {} -> return e
    S.AppElt a -> do
      app <- S.traverseApp (evaluateFunctions' sym pf operands rewriters) (S.appEltApp a)
      liftIO $ S.sbMakeElt sym app
    S.NonceAppElt nonceApp -> do
      case S.nonceEltApp nonceApp of
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
              rs = first renameUninterpretedFunction <$> rewriters
          assignment' <- traverseFC (evaluateFunctions' sym pf operands rs) assignment
          case lookup key rewriters of
            Just (Evaluator evaluator) -> do
              (e',m') <- liftIO $ evaluator sym pf operands assignment' (S.exprType e)
              modify' (m' `M.union`)
              pure e'
            Nothing ->
              liftIO $ applySymFn sym symFun assignment

-- | By default, symbolic function names hold to a different naming convention than what is defined
-- in instruction semantics. In order to ensure we can lookup a function by its name,
-- we modify it in our list of rewriters (e.g. `ppc.memrr_base` to `ppc_memrr_base`).
renameUninterpretedFunction :: [Char] -> [Char]
renameUninterpretedFunction ks = xs ++ "_" ++ ys
  where
    (xs,ys) =
      takeWhile (/='.') &&&
        drop 1 . dropWhile (/='.') $ ks
