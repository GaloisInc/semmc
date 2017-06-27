{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module SemMC.Formula.Equivalence (
  formulasEquiv
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Data.Foldable ( foldrM )
import           Data.Maybe ( fromJust )
import           Data.Parameterized.Some
import           Data.Parameterized.Classes
import           Data.Parameterized.TraversableF
import qualified Data.Parameterized.Map as MapF
import qualified Data.Set as Set
import           System.IO ( stderr )

import           Lang.Crucible.BaseTypes
import           Lang.Crucible.Config ( initialConfig, setConfigValue )
import           Lang.Crucible.Solver.Adapter
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SatResult
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import           Lang.Crucible.Solver.SimpleBackend.Z3
import           Lang.Crucible.Solver.SimpleBuilder
import           Lang.Crucible.Utils.MonadVerbosity ( withVerbosity )

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Formula.Instantiate
import           SemMC.Util

mapFKeys :: forall (key :: k -> *) (value :: k -> *). MapF.MapF key value -> [Some key]
mapFKeys = MapF.foldrWithKey (\k _ l -> Some k : l) []

formulasEquiv :: (Architecture arch) => SimpleBackend t -> Formula (SimpleBackend t) arch -> Formula (SimpleBackend t) arch -> IO (Either (ArchState (SimpleBackend t) arch) ())
formulasEquiv
  sym
  f1@(Formula {formUses = uses1, formDefs = defs1})
  f2@(Formula {formUses = uses2, formDefs = defs2}) =
  if not (uses1 == uses2 &&
          -- Map.keys returns a list in a unique (increasing) order, so we don't
          -- need to turn it into a list first.
          mapFKeys defs1 == mapFKeys defs2)
  then return (Left MapF.empty)
  else formulasEquiv' sym f1 f2

formulasEquiv' :: forall t arch. (Architecture arch) => SimpleBackend t -> Formula (SimpleBackend t) arch -> Formula (SimpleBackend t) arch -> IO (Either (ArchState (SimpleBackend t) arch) ())
formulasEquiv'
  sym
  (Formula {formParamVars = bvars1, formDefs = defs1})
  (Formula {formParamVars = bvars2, formDefs = defs2}) =
  do
    -- Create constants for each of the bound variables, then replace them in
    -- each of the definitions. This way, the equations in the different
    -- formulas refer to the same input variables.
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

    let handler (Sat (GroundEvalFn evalFn, _)) = do
          -- Extract the failing test case.
          let eval :: forall tp. Location arch tp -> Elt t tp -> IO (Elt t tp)
              eval loc e = groundValToExpr sym (locationType loc) =<< evalFn e
          Left <$> MapF.traverseWithKey eval varConstants
        handler Unsat = return (Right ())
        handler Unknown = fail "Got Unknown result when checking sat-ness"

    withVerbosity stderr 1 $ do
      cfg <- liftIO $ initialConfig 1 z3Options
      -- TODO: make this configurable
      setConfigValue z3Path cfg "/usr/local/bin/z3"
      liftIO $ solver_adapter_check_sat z3Adapter sym cfg (\_ _ -> return ()) testExpr handler
