{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
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

import           GHC.Prim                           ( RealWorld )
import           Control.Monad.ST                   ( stToIO )

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context         as Ctx
import qualified Data.Parameterized.HashTable       as PH
import qualified Data.Parameterized.List            as SL
import qualified Data.Parameterized.Map             as MapF
import qualified Data.Parameterized.Nonce           as PN
import qualified Lang.Crucible.Solver.Interface     as SI
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import           Lang.Crucible.Types

import qualified SemMC.Architecture.Internal        as A
import qualified SemMC.BoundVar                     as BV
import           SemMC.Formula.Formula

type Sym t st = S.SimpleBuilder t st

data Evaluator arch t =
  Evaluator (forall tp u st sh
               . Sym t st
              -> ParameterizedFormula (Sym t st) arch sh
              -> SL.List (A.Operand arch) sh
              -> Ctx.Assignment (S.Elt t) u
              -> BaseTypeRepr tp
              -> IO (S.Elt t tp))

evaluateFunctions :: Sym t st
                  -> ParameterizedFormula (Sym t st) arch sh
                  -> SL.List (A.Operand arch) sh
                  -> S.Elt t ret
                  -> [(String, Evaluator arch t)]
                  -> IO (S.Elt t ret)
evaluateFunctions sym pf operands e0 rewriters = do
  tbl <- stToIO (PH.newSized 100)
  fnTab <- stToIO PH.new
  let tbls = EvalHashTables { eltTable = tbl
                            , fnTable = fnTab
                            }
  evaluateFunctions' tbls operandIndex sym e0
  where
    operandIndex = SL.ifoldr (indexOperands operands) MapF.empty (pfOperandVars pf)

indexOperands :: (OrdF (SI.BoundVar (Sym t st)))
              => SL.List (A.Operand arch) sh
              -> SL.Index sh tp
              -> BV.BoundVar (Sym t st) arch tp
              -> MapF.MapF (BV.BoundVar (Sym t st) arch) (A.Operand arch)
              -> MapF.MapF (BV.BoundVar (Sym t st) arch) (A.Operand arch)
indexOperands operands ix opVar = MapF.insert opVar (operands SL.!! ix)

data CachedSymFn t c where
  CachedSymFn :: (c ~ (a Ctx.::> r)) => CachedSymFn Bool (S.SimpleSymFn t a r)

data EvalHashTables t =
  EvalHashTables { eltTable :: PH.HashTable RealWorld (S.Elt t) (S.Elt t)
                 , fnTable :: PH.HashTable RealWorld (PN.Nonce t) (CachedSymFn t)
                 }

evaluateFunctions' :: EvalHashTables t
                   -> MapF.MapF (BV.BoundVar (Sym t st) arch) (A.Operand arch)
                   -> Sym t st
                   -> S.Elt t ret
                   -> IO (S.Elt t ret)
evaluateFunctions' tbls operandIndex sym e0 =
  case e0 of
    S.SemiRingLiteral {} -> return e0
    S.BVElt {} -> return e0
    _ -> undefined
