{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module SemMC.Formula.Equivalence (
  formulasEquiv
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           Data.Type.Equality (testEquality)
import           System.IO (stderr)

import           Lang.Crucible.Config (initialConfig, setConfigValue)
import           Lang.Crucible.Solver.Adapter
import           Lang.Crucible.Solver.Interface
import           Lang.Crucible.Solver.SatResult
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.Solver.SimpleBackend.Z3
import           Lang.Crucible.Solver.SimpleBuilder
import           Lang.Crucible.Utils.MonadVerbosity (withVerbosity)

import           SemMC.Formula

formulasEquiv :: SimpleBackend t -> Formula (SimpleBackend t) -> Formula (SimpleBackend t) -> IO Bool
formulasEquiv
  sym
  f1@(Formula {formUses = uses1, formDefs = defs1})
  f2@(Formula {formUses = uses2, formDefs = defs2}) =
  if not (Set.fromList uses1 == Set.fromList uses2 &&
          -- Map.keys returns a list in a unique (increasing) order, so we don't
          -- need to turn it into a list first.
          Map.keys defs1 == Map.keys defs2)
  then return False
  else formulasEquiv' sym f1 f2

formulasEquiv' :: forall t . SimpleBackend t -> Formula (SimpleBackend t) -> Formula (SimpleBackend t) -> IO Bool
formulasEquiv'
  sym
  (Formula {formParamExprs = exprs1, formDefs = defs1})
  (Formula {formParamExprs = exprs2, formDefs = defs2}) =
  do
    let matchingPair :: Map.Map FormulaVar (Some (Elt t))
                     -> FormulaVar
                     -> Some (Elt t)
                     -> [(Some (Elt t), Some (Elt t))]
        matchingPair table var e1 = [(e1, fromJust $ Map.lookup var table)]

        andPairEquality :: (Some (Elt t), Some (Elt t)) -> BoolElt t -> IO (BoolElt t)
        andPairEquality (e1, e2) b = do
          Some e1' <- return e1
          Some e2' <- return e2
          Just Refl <- return $ testEquality (exprType e1') (exprType e2')
          -- ^ This is partial. It fails when the same 'FormulaVar' in different
          -- formulas have different types of corresponding expressions, which
          -- should never happen. If it does, an IO exception is thrown.
          eq <- isEq sym e1' e2'
          andPred sym b eq

    -- First, we form equalities between each of variable expressions in the
    -- first formula and the corresponding expressions in the second.
    let varPairs = Map.foldMapWithKey (matchingPair exprs2) exprs1
    -- (v1 = v1') /\ (v2 = v2') /\ ... /\ (vn = vn')
    allPairsEqual <- foldrM andPairEquality (truePred sym) varPairs

    -- Next, we build up a similar expression, but for the definitions this
    -- time.
    let defsPairs = Map.foldMapWithKey (matchingPair defs2) defs1
    -- (d1 = d1') /\ (d2 = d2') /\ ... /\ (dm = dm')
    allDefsEqual <- foldrM andPairEquality (truePred sym) defsPairs

    -- Finally, we ask, "Is it possible that all the variables are equal to each
    -- other, but not all the definitions are equal to each other?"
    -- ((v1 = v1') /\ ... /\ (vn = vn')) /\ ~((d1 = d1') /\ ... /\ (dm = dm'))
    testExpr <- notPred sym allDefsEqual >>= andPred sym allPairsEqual

    result <- withVerbosity stderr 1 $ do
      cfg <- liftIO $ initialConfig 1 z3Options
      setConfigValue z3Path cfg "/usr/local/bin/z3"
      liftIO $ solver_adapter_check_sat z3Adapter sym cfg (const . const $ return ()) testExpr return

    case result of
      Sat _ -> return False
      Unsat -> return True
      Unknown -> fail "Got Unknown result when checking sat-ness"
