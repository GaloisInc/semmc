{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}


module SemMC.Formula.Equivalence
  ( EquivalenceResult(..)
  , checkSat
  , formulasEquiv
  , formulasEquivSym
  , formulasEquivSymWithCondition
  , formulasEquivConcrete
  ) where

import           Data.Foldable ( foldrM )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableF ( traverseF )
import qualified Data.Set as Set
import qualified Data.Word.Indexed as W

import           What4.BaseTypes
import qualified What4.Interface as S
import           What4.SatResult
import qualified Lang.Crucible.Backend.Online as CBO
import           What4.Expr.GroundEval
import           What4.Expr.Builder
import qualified What4.Expr as WE
import qualified What4.Protocol.Online as WPO
import qualified What4.Protocol.SMTWriter as WPS


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
formulasEquivSym :: forall arch t solver fs .
                    (A.Architecture arch, WPO.OnlineSolver t solver)
                 => CBO.OnlineBackend t solver fs
                 -> [Integer]
                 -- ^ Only check equivalence of memory at these addresses.
                 -> F.Formula (CBO.OnlineBackend t solver fs) arch
                 -> F.Formula (CBO.OnlineBackend t solver fs) arch
                 -> IO (EquivalenceResult arch (Expr t))
formulasEquivSym sym indices =
  let eval :: forall tp. GroundEvalFn t -> Expr t tp -> IO (Expr t tp)
      eval (GroundEvalFn evalFn) e = do
        e' <- evalFn e
        U.groundValToExpr sym indices (S.exprType e) e'
  in formulasEquiv eval sym

-- | Check the equivalence of two formulas, under some condition of the final
-- predicate. The counterexample values are 'Elt's.
formulasEquivSymWithCondition :: forall arch t solver fs .
                    (A.Architecture arch, WPO.OnlineSolver t solver)
                 => CBO.OnlineBackend t solver fs
                 -> (Expr t BaseBoolType -> IO (Expr t BaseBoolType))
                 -- ^ This predicate must hold of the resulting equation
                 -> (MapF.MapF (L.Location arch) (Expr t) -> IO (Expr t BaseBoolType))
                 -- ^ For locations @ls@ occurring in formula, add the additional
                 -- condition @checkLoc ls@ to the resulting predicate.
                 -> [Integer]
                 -- ^ Only check equivalence of memory at these addresses.
                 -> F.Formula (CBO.OnlineBackend t solver fs) arch
                 -> F.Formula (CBO.OnlineBackend t solver fs) arch
                 -> IO (EquivalenceResult arch (Expr t))
formulasEquivSymWithCondition sym resCheck locCheck indices =
  let eval :: forall tp. GroundEvalFn t -> Expr t tp -> IO (Expr t tp)
      eval (GroundEvalFn evalFn) e = do
        e' <- evalFn e
        U.groundValToExpr sym indices (S.exprType e) e'
  in formulasEquivWithCondition eval sym resCheck locCheck



-- | Check the equivalence of two formulas. The counterexample values are 'Value's.
formulasEquivConcrete :: forall arch t solver fs .
                         (A.Architecture arch, WPO.OnlineSolver t solver)
                      => CBO.OnlineBackend t solver fs
                      -> F.Formula (CBO.OnlineBackend t solver fs) arch
                      -> F.Formula (CBO.OnlineBackend t solver fs) arch
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
--
-- The first output is a predicate expressing if the formula @f1@ is equivalent
-- to the formula @f2@. The second output is a map from locations used in @f1@
-- or @f2@ to the symbolic variables representing them in the predicate.
formulasEquivPred :: forall t solver fs arch.
                 (A.Architecture arch, WPO.OnlineSolver t solver)
              => CBO.OnlineBackend t solver fs
              -> F.Formula (CBO.OnlineBackend t solver fs) arch
              -> F.Formula (CBO.OnlineBackend t solver fs) arch
              -> IO ( S.Pred (CBO.OnlineBackend t solver fs)
                    , MapF.MapF (L.Location arch) (Expr t))
formulasEquivPred
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
        varLookup = return . U.fromJust' "varLookup" . flip MapF.lookup varConstants
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
            Nothing -> U.fromJust' "lookupDefn" (MapF.lookup loc varConstants)
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

    return (testExpr, varConstants)



-- | Check the equivalence of two formulas, using the first parameter to extract
-- expression values for the counterexample.
formulasEquiv :: forall t solver fs arch ex.
                 (A.Architecture arch, WPO.OnlineSolver t solver)
              => (forall tp. GroundEvalFn t -> Expr t tp -> IO (ex tp))
              -> CBO.OnlineBackend t solver fs
              -> F.Formula (CBO.OnlineBackend t solver fs) arch
              -> F.Formula (CBO.OnlineBackend t solver fs) arch
              -> IO (EquivalenceResult arch ex)
formulasEquiv eval sym f1 f2 = do
    (testExpr,varConstants) <- formulasEquivPred sym f1 f2
    checkSatEvalFn eval sym varConstants testExpr

-- | Check the equivalence of two formulas, using the first parameter to extract
-- expression values for the counterexample. Also assert that the resulting
-- predicate satisfies the condition given.
formulasEquivWithCondition :: forall t solver fs arch ex sym.
                 (A.Architecture arch, WPO.OnlineSolver t solver, sym ~ CBO.OnlineBackend t solver fs)
              => (forall tp. GroundEvalFn t -> Expr t tp -> IO (ex tp))
              -> sym
              -> (Expr t BaseBoolType -> IO (Expr t BaseBoolType))
              -- ^ This predicate must hold of the resulting equation
              -> (MapF.MapF (L.Location arch) (Expr t) -> IO (Expr t BaseBoolType))
              -- ^ For locations @ls@ occurring in formula, add the additional
              -- condition @checkLoc ls@ to the resulting predicate.
              -> F.Formula sym arch
              -> F.Formula sym arch
              -> IO (EquivalenceResult arch ex)
formulasEquivWithCondition eval sym checkRes checkLoc f1 f2 = do
    (testExpr,varConstants) <- formulasEquivPred sym f1 f2
    locCheck <- checkLoc varConstants
    resCheck <- checkRes testExpr
    testExprWithCheck <- S.andPred sym testExpr =<< S.andPred sym locCheck resCheck
    checkSatEvalFn eval sym varConstants testExprWithCheck


-- | Check the satisfiability of a formula using the given solver backend.
--
-- We are requiring the online solver here, as most of this library requires
-- lots of small queries.
checkSat :: (WPO.OnlineSolver t solver)
         => CBO.OnlineBackend t solver fs
         -> WE.BoolExpr t
         -> (SatResult (GroundEvalFn t) () -> IO a)
         -> IO a
checkSat sym testExpr handler = do
  sp <- CBO.getSolverProcess sym
  let conn = WPO.solverConn sp
  WPO.inNewFrame sp $ do
    f <- WPS.mkFormula conn testExpr
    WPS.assumeFormula conn f
    res <- WPO.checkAndGetModel sp "semmc equivalence formula"
    handler res

checkSatEvalFn :: (WPO.OnlineSolver t solver)
          => (forall tp. GroundEvalFn t -> Expr t tp -> IO (ex tp))
          -> CBO.OnlineBackend t solver fs
          -> MapF.MapF (L.Location arch) (Expr t)
          -> WE.BoolExpr t
          -> IO (EquivalenceResult arch ex)
checkSatEvalFn eval sym varConstants test =
    let handler (Sat evalFn) = do
          -- Extract the failing test case.
          DifferentBehavior <$> traverseF (eval evalFn) varConstants
        handler Unsat{} = return Equivalent
        handler Unknown = return Timeout
    in checkSat sym test handler
