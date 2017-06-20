{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module SemMC.Formula.Equivalence (
  formulasEquiv
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Parameterized.Some
import           Data.Parameterized.Classes
import           Data.Parameterized.TraversableF
import qualified Data.Set as Set
import qualified Data.Parameterized.Map as MapF
import           Data.Type.Equality (testEquality)
import           System.IO (stderr)

import           Lang.Crucible.Config (initialConfig, setConfigValue)
import           Lang.Crucible.Solver.Adapter
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SatResult
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.Solver.SimpleBackend.Z3
import           Lang.Crucible.Solver.SimpleBuilder
import           Lang.Crucible.Utils.MonadVerbosity (withVerbosity)

import           SemMC.Formula
import           SemMC.Architecture

mapFKeys :: MapF.MapF k a -> [Some k]
mapFKeys = MapF.foldrWithKey (\k _ l -> Some k : l) []

formulasEquiv :: (Architecture arch) => SimpleBackend t -> Formula (SimpleBackend t) arch -> Formula (SimpleBackend t) arch -> IO Bool
formulasEquiv
  sym
  f1@(Formula {formUses = uses1, formDefs = defs1})
  f2@(Formula {formUses = uses2, formDefs = defs2}) =
  if not (uses1 == uses2 &&
          -- Map.keys returns a list in a unique (increasing) order, so we don't
          -- need to turn it into a list first.
          mapFKeys defs1 == mapFKeys defs2)
  then return False
  else formulasEquiv' sym f1 f2

varToExpr :: (S.IsSymInterface sym) => sym -> BoundVar sym arch op -> S.SymExpr sym (OperandType arch op)
varToExpr sym = S.varExpr sym . unBoundVar

data PairF :: (k -> *) -> (k -> *) -> k -> * where
  PairF :: forall a b t. a t -> b t -> PairF a b t

formulasEquiv' :: forall t arch. (Architecture arch) => SimpleBackend t -> Formula (SimpleBackend t) arch -> Formula (SimpleBackend t) arch -> IO Bool
formulasEquiv'
  sym
  (Formula {formParamVars = bvars1, formDefs = defs1})
  (Formula {formParamVars = bvars2, formDefs = defs2}) =
  do
    let matchingPair :: MapF.MapF (StateVar arch) (Elt t)
                     -> StateVar arch tp
                     -> Elt t tp
                     -> [MapF.Pair (Elt t) (Elt t)]
                     -> [MapF.Pair (Elt t) (Elt t)]
        matchingPair table var e1 accum = (MapF.Pair e1 (fromJust $ MapF.lookup var table)) : accum
        -- matchingPair table var e1 = (e1, fromJust $ MapF.lookup var table)
        -- matchingPair table var e1 = [(e1, fromJust $ Map.lookup var table)]

        andPairEquality :: MapF.Pair (Elt t) (Elt t) -> BoolElt t -> IO (BoolElt t)
        andPairEquality (MapF.Pair e1 e2) b = do
          -- Some e1' <- return e1
          -- Some e2' <- return e2
          -- Just Refl <- return $ testEquality (exprType e1') (exprType e2')
          -- -- ^ This is partial. It fails when the same 'FormulaVar' in different
          -- -- formulas have different types of corresponding expressions, which
          -- -- should never happen. If it does, an IO exception is thrown.
          eq <- S.isEq sym e1 e2
          S.andPred sym b eq

    -- First, we form equalities between each of variable expressions in the
    -- first formula and the corresponding expressions in the second.
    let varPairs = MapF.foldrWithKey (matchingPair (fmapF (S.varExpr sym) bvars2)) [] (fmapF (S.varExpr sym) bvars1)
    -- (v1 = v1') /\ (v2 = v2') /\ ... /\ (vn = vn')
    allPairsEqual <- foldrM andPairEquality (S.truePred sym) varPairs

    -- Next, we build up a similar expression, but for the definitions this
    -- time.
    let defsPairs = MapF.foldrWithKey (matchingPair defs2) [] defs1
    -- (d1 = d1') /\ (d2 = d2') /\ ... /\ (dm = dm')
    allDefsEqual <- foldrM andPairEquality (S.truePred sym) defsPairs

    -- Finally, we ask, "Is it possible that all the variables are equal to each
    -- other, but not all the definitions are equal to each other?"
    -- ((v1 = v1') /\ ... /\ (vn = vn')) /\ ~((d1 = d1') /\ ... /\ (dm = dm'))
    testExpr <- S.notPred sym allDefsEqual >>= S.andPred sym allPairsEqual

    result <- withVerbosity stderr 1 $ do
      cfg <- liftIO $ initialConfig 1 z3Options
      setConfigValue z3Path cfg "/usr/local/bin/z3"
      liftIO $ solver_adapter_check_sat z3Adapter sym cfg (const . const $ return ()) testExpr return

    case result of
      Sat _ -> return False
      Unsat -> return True
      Unknown -> fail "Got Unknown result when checking sat-ness"
