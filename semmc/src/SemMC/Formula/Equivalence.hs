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
  , formulasEquivConcrete
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Data.Foldable ( foldrM )
import           Data.Maybe ( fromJust )
import           Data.Parameterized.Some
import           Data.Parameterized.Classes
import           Data.Parameterized.TraversableF
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr ( withKnownNat )
import qualified Data.Set as Set
import           System.IO ( stderr )

import           Lang.Crucible.BaseTypes
import           Lang.Crucible.Config ( initialConfig )
import           Lang.Crucible.Solver.Adapter
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SatResult
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import           Lang.Crucible.Solver.SimpleBackend.Z3
import           Lang.Crucible.Solver.SimpleBuilder
import           Lang.Crucible.Utils.MonadVerbosity ( MonadVerbosity, withVerbosity )

import           SemMC.Architecture
import           SemMC.ConcreteState ( Value(..) )
import           SemMC.Formula.Formula
import           SemMC.Formula.Instantiate
import           SemMC.Util

data EquivalenceResult arch ex = Equivalent
                               | Mismatching
                               | DifferentBehavior (ArchState arch ex)
deriving instance (ShowF (Location arch), ShowF ex) => Show (EquivalenceResult arch ex)

formulasEquiv :: forall arch t.
                 (Architecture arch)
              => SimpleBackend t
              -> Formula (SimpleBackend t) arch
              -> Formula (SimpleBackend t) arch
              -> IO (EquivalenceResult arch (Elt t))
formulasEquiv sym =
  let eval :: forall tp. GroundEvalFn t -> Elt t tp -> IO (Elt t tp)
      eval (GroundEvalFn evalFn) e = groundValToExpr sym (S.exprType e) =<< evalFn e
  in formulasEquiv' eval sym

formulasEquivConcrete :: forall arch t.
                         (Architecture arch)
                      => SimpleBackend t
                      -> Formula (SimpleBackend t) arch
                      -> Formula (SimpleBackend t) arch
                      -> IO (EquivalenceResult arch Value)
formulasEquivConcrete =
  let eval :: forall tp. GroundEvalFn t -> Elt t tp -> IO (Value tp)
      eval (GroundEvalFn evalFn) e =
        case S.exprType e of
          BaseBVRepr w -> withKnownNat w (ValueBV . fromInteger <$> evalFn e)
          _ -> error "formulasEquivConcrete: only BVs supported"
  in formulasEquiv' eval

formulasEquiv' :: (Architecture arch)
               => (forall tp. GroundEvalFn t -> Elt t tp -> IO (ex tp))
               -> SimpleBackend t
               -> Formula (SimpleBackend t) arch
               -> Formula (SimpleBackend t) arch
               -> IO (EquivalenceResult arch ex)
formulasEquiv'
  eval
  sym
  f1@(Formula { formDefs = defs1 })
  f2@(Formula { formDefs = defs2 }) =
  if not (formInputs f1 == formInputs f2 &&
          -- Map.keys returns a list in a unique (increasing) order, so we don't
          -- need to turn it into a list first.
          mapFKeys defs1 == mapFKeys defs2)
  then return Mismatching
  else formulasEquiv'' eval sym f1 f2

formulasEquiv'' :: forall t arch ex.
                   (Architecture arch)
                => (forall tp. GroundEvalFn t -> Elt t tp -> IO (ex tp))
                -> SimpleBackend t
                -> Formula (SimpleBackend t) arch
                -> Formula (SimpleBackend t) arch
                -> IO (EquivalenceResult arch ex)
formulasEquiv''
  eval
  sym
  (Formula {formParamVars = bvars1, formDefs = defs1})
  (Formula {formParamVars = bvars2, formDefs = defs2}) =
  do
    -- Create constants for each of the bound variables, then replace them in
    -- each of the definitions. This way, the equations in the different
    -- formulas refer to the same input variables.
    --
    -- Here 'constant' does not mean a literal, like '5'. Instead, it means a
    -- variable in SMT land that isn't meant to be bound.
    let allVars = Set.union (Set.fromList (mapFKeys bvars1)) (Set.fromList (mapFKeys bvars2))
        mkConstant (Some loc) m =
          fmap (\e -> MapF.insert loc e m)
               (S.freshConstant sym (makeSymbol (showF loc)) (locationType loc))
    varConstants <- foldrM mkConstant MapF.empty allVars
    let replaceVars vars =
          traverseF (replaceLitVars sym (return . fromJust . flip MapF.lookup varConstants) vars)
    defs1' <- replaceVars bvars1 defs1
    defs2' <- replaceVars bvars2 defs2

    let matchingPair :: MapF.MapF (Location arch) (Elt t)
                     -> Location arch tp
                     -> Elt t tp
                     -> [MapF.Pair (Elt t) (Elt t)]
                     -> [MapF.Pair (Elt t) (Elt t)]
        matchingPair table var e1 = (:) $ MapF.Pair e1 (fromJust $ MapF.lookup var table)

        andPairEquality :: MapF.Pair (Elt t) (Elt t) -> BoolElt t -> IO (BoolElt t)
        andPairEquality (MapF.Pair e1 e2) accum = do
          S.andPred sym accum =<< S.isEq sym e1 e2

    -- Next, we build up equalities between each of the individual definitions
    -- in both of the formulas.
    let defsPairs = MapF.foldrWithKey (matchingPair defs2') [] defs1'
    -- (d1 = d1') /\ (d2 = d2') /\ ... /\ (dm = dm')
    allDefsEqual <- foldrM andPairEquality (S.truePred sym) defsPairs

    -- Finally, we ask, "Is it possible that all the variables are equal to each
    -- other, but not all the definitions are equal to each other?"
    -- ~((d1 = d1') /\ ... /\ (dm = dm'))
    testExpr <- S.notPred sym allDefsEqual

    let handler (Sat (evalFn, _)) = do
          -- Extract the failing test case.
          DifferentBehavior <$> traverseF (eval evalFn) varConstants
        handler Unsat = return Equivalent
        handler Unknown = fail "Got Unknown result when checking sat-ness"

    checkSatZ3 sym testExpr handler

-- | Check satisfiability using Z3.
--
-- The @handler@ receives the result of the satisfiability check.
checkSatZ3 :: forall t a.
              SimpleBackend t
           -> BoolElt t
           -> (SatResult (GroundEvalFn t, Maybe (EltRangeBindings t)) -> IO a)
           -> IO a
checkSatZ3 sym testExpr handler = do
  withVerbosity stderr 1 check
  where
    -- Without factoring this out and giving it an explicit type sig
    -- for use with the explicit type application below I get errors
    -- related to the monad @m@ here being ambiguous.
    check :: forall m. MonadVerbosity m => m a
    check = do
      cfg <- liftIO $ (initialConfig @ m) 1 z3Options
      liftIO $ solver_adapter_check_sat z3Adapter sym cfg (\_ _ -> return ()) testExpr handler
