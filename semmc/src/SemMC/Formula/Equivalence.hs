{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module SemMC.Formula.Equivalence
  ( EquivalenceResult(..)
  , checkSatZ3
  , formulasEquiv
  , formulasEquivSym
  , formulasEquivConcrete
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Data.Foldable ( foldrM )
import           Data.Maybe ( fromJust )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableF ( traverseF )
import qualified Data.Set as Set
import qualified Data.Word.Indexed as W
import qualified System.IO as IO

import           What4.BaseTypes
import           What4.Solver.Adapter
import qualified What4.Interface as S
import           What4.SatResult
import           Lang.Crucible.Backend.Simple
import           What4.Expr.GroundEval
import           What4.Solver.Z3
import           What4.Expr.Builder
import           Lang.Crucible.Utils.MonadVerbosity ( MonadVerbosity, withVerbosity )

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Formula.Formula as F
import qualified SemMC.Formula.Instantiate as FI
import qualified SemMC.Util as U

-- | The result of an equivalence check.
data EquivalenceResult arch ex
  = Equivalent
    -- ^ The two formulas are equivalent (or, if you want to be pedantic,
    -- equisatisfiable).
  | DifferentBehavior (L.ArchState arch ex)
    -- ^ The two formulas are non-trivially different, i.e., the SAT solver was
    -- needed to show difference. The 'ArchState' is a machine state that is a
    -- counterexample to their equivalence.
  | Timeout
  -- ^ The solver timed out
deriving instance (ShowF (A.Location arch), ShowF ex) => Show (EquivalenceResult arch ex)

-- | Check the equivalence of two formulas. The counterexample values are 'Elt's.
formulasEquivSym :: forall arch t.
                    (A.Architecture arch)
                 => SimpleBackend t
                 -> F.Formula (SimpleBackend t) arch
                 -> F.Formula (SimpleBackend t) arch
                 -> IO (EquivalenceResult arch (Expr t))
formulasEquivSym sym =
  let eval :: forall tp. GroundEvalFn t -> Expr t tp -> IO (Expr t tp)
      eval (GroundEvalFn evalFn) e = U.groundValToExpr sym (S.exprType e) =<< evalFn e
  in formulasEquiv eval sym

-- | Check the equivalence of two formulas. The counterexample values are 'Value's.
formulasEquivConcrete :: forall arch t.
                         (A.Architecture arch)
                      => SimpleBackend t
                      -> F.Formula (SimpleBackend t) arch
                      -> F.Formula (SimpleBackend t) arch
                      -> IO (EquivalenceResult arch V.Value)
formulasEquivConcrete =
  let eval :: forall tp. GroundEvalFn t -> Expr t tp -> IO (V.Value tp)
      eval (GroundEvalFn evalFn) e =
        case S.exprType e of
          BaseBVRepr w -> V.ValueBV . W.wRep w <$> evalFn e
          _ -> error "formulasEquivConcrete: only BVs supported"
  in formulasEquiv eval

allPairwiseEquality :: (S.IsExprBuilder sym)
                    => sym
                    -> [Pair (S.SymExpr sym) (S.SymExpr sym)]
                    -> IO (S.Pred sym)
allPairwiseEquality sym = foldrM andPairEquality (S.truePred sym)
  where andPairEquality (Pair e1 e2) accum = S.andPred sym accum =<< S.isEq sym e1 e2

-- | Check the equivalence of two formulas, using the first parameter to extract
-- expression values for the counterexample.
formulasEquiv :: forall t arch ex.
                 (A.Architecture arch)
              => (forall tp. GroundEvalFn t -> Expr t tp -> IO (ex tp))
              -> SimpleBackend t
              -> F.Formula (SimpleBackend t) arch
              -> F.Formula (SimpleBackend t) arch
              -> IO (EquivalenceResult arch ex)
formulasEquiv
  eval
  sym
  f1@(F.Formula { F.formParamVars = bvars1, F.formDefs = defs1 } )
  f2@(F.Formula { F.formParamVars = bvars2, F.formDefs = defs2 } ) =
  do
    -- Create constants for each of the bound variables, then replace them in
    -- each of the definitions. This way, the equations in the different
    -- formulas refer to the same input variables.
    --
    -- Here 'constant' does not mean a literal, like '5'. Instead, it means a
    -- variable in SMT land that isn't meant to be bound.
    let allInputLocs = F.formInputs f1 `Set.union` F.formInputs f2
        allOutputLocs = F.formOutputs f1 `Set.union` F.formOutputs f2
        allLocs = allInputLocs `Set.union` allOutputLocs
        mkConstant (Some loc) m =
          fmap (\e -> MapF.insert loc e m)
               (S.freshConstant sym (U.makeSymbol (showF loc)) (A.locationType loc))

    varConstants <- foldrM mkConstant MapF.empty allLocs
    let -- This 'fromJust' is total because all of the used variables are in
        -- 'varConstants'.
        varLookup :: forall tp . A.Location arch tp -> IO (Expr t tp)
        varLookup = return . fromJust . flip MapF.lookup varConstants
        replaceVars vars = traverseF (FI.replaceLitVars sym varLookup vars)

    defs1' <- replaceVars bvars1 defs1
    defs2' <- replaceVars bvars2 defs2

    let lookupDefn :: MapF.MapF (A.Location arch) (Expr t)
                   -> A.Location arch tp
                   -> Expr t tp
        lookupDefn defs loc =
          case MapF.lookup loc defs of
            Just defn -> defn
            -- If this formula doesn't explicitly define this location, then it
            -- implicitly preserves the same value. This 'fromJust' is total
            -- because 'varConstants' has all locations ever mentioned (that is,
            -- both uses and definitions) in the formula in it.
            Nothing -> fromJust (MapF.lookup loc varConstants)
        lookupDefns (Some loc) =
          Pair (lookupDefn defs1' loc) (lookupDefn defs2' loc)

    -- Next, we build up equalities between each of the individual definitions
    -- in both of the formulas.
    let defnPairs = foldr ((:) . lookupDefns) [] allOutputLocs
    -- (d1 = d1') /\ (d2 = d2') /\ ... /\ (dm = dm')
    allDefsEqual <- allPairwiseEquality sym defnPairs

    -- Finally, we ask, "Is it possible that all the variables are equal to each
    -- other, but not all the definitions are equal to each other?"
    -- ~((d1 = d1') /\ ... /\ (dm = dm'))
    testExpr <- S.notPred sym allDefsEqual

    let handler (Sat (evalFn, _)) = do
          -- Extract the failing test case.
          DifferentBehavior <$> traverseF (eval evalFn) varConstants
        handler Unsat = return Equivalent
        handler Unknown = return Timeout

    checkSatZ3 sym testExpr handler

-- | Check satisfiability using Z3.
--
-- The @handler@ receives the result of the satisfiability check.
checkSatZ3 :: forall t a.
              SimpleBackend t
           -> BoolExpr t
           -> (SatResult (GroundEvalFn t, Maybe (ExprRangeBindings t)) -> IO a)
           -> IO a
checkSatZ3 sym testExpr handler = do
  withVerbosity IO.stderr 1 check
  where
    -- Without factoring this out and giving it an explicit type sig
    -- for use with the explicit type application below I get errors
    -- related to the monad @m@ here being ambiguous.
    check :: forall m. MonadVerbosity m => m a
    check = liftIO $ solver_adapter_check_sat z3Adapter sym (\_ _ -> return ()) testExpr handler
