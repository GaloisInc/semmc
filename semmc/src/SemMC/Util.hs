{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module SemMC.Util
  ( groundValToExpr
  , makeSymbol
  , mapFReverse
  , sequenceMaybes
  , allBoundVars
  , extractUsedLocs
  ) where

import           Control.Applicative ( Const(..) )
import           Control.Monad.ST ( runST )
import qualified Data.HashTable.Class as H
import           Data.Maybe ( fromJust )
import           Data.Monoid ( (<>) )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableFC
import qualified Data.Set as Set
import           Text.Printf

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Utils.Hashable as Hash
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import           Lang.Crucible.Solver.Symbol ( SolverSymbol, userSymbol )

-- | Try converting any 'String' into a 'SolverSymbol'. If it is an invalid
-- symbol, then error.
makeSymbol :: String -> SolverSymbol
makeSymbol name = case userSymbol sanitizedName of
                    Right symbol -> symbol
                    Left _ -> error $ printf "tried to create symbol with bad name: %s (%s)"
                                             name sanitizedName
  where
    sanitizedName = map (\c -> case c of ' ' -> '_'; '.' -> '_'; _ -> c) name

-- | Convert a 'GroundValue' (a primitive type that represents the given
-- Crucible type) back into a symbolic expression, just as a literal.
groundValToExpr :: forall sym tp.
                   (S.IsExprBuilder sym)
                => sym
                -> BaseTypeRepr tp
                -> GroundValue tp
                -> IO (S.SymExpr sym tp)
groundValToExpr sym BaseBoolRepr True = return (S.truePred sym)
groundValToExpr sym BaseBoolRepr False = return (S.falsePred sym)
groundValToExpr sym (BaseBVRepr w) val = S.bvLit sym w val
groundValToExpr sym BaseNatRepr val = S.natLit sym val
groundValToExpr sym BaseIntegerRepr val = S.intLit sym val
groundValToExpr sym BaseRealRepr val = S.realLit sym val
groundValToExpr sym BaseComplexRepr val = S.mkComplexLit sym val
groundValToExpr sym (BaseArrayRepr idxTp elemTp) (ArrayConcrete base m) = do
  base' <- groundValToExpr sym elemTp base
  entries <- Hash.mkMap <$> traverse (groundValToExpr sym elemTp) m
  S.arrayFromMap sym idxTp entries base'
groundValToExpr _ (BaseArrayRepr _ _) (ArrayMapping _) = error "groundValToExpr: ArrayMapping not handled"
groundValToExpr _ (BaseStructRepr _) _ = error "groundValToExpr: struct type isn't handled yet"

-- | Reverse a MapF, so that the old keys are the new values and the old values
-- are the new keys.
mapFReverse :: (OrdF value) => MapF.MapF key value -> MapF.MapF value key
mapFReverse = MapF.foldrWithKey (flip MapF.insert) MapF.empty

-- | Run the monadic actions in order, returning the first 'Just' value.
sequenceMaybes :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
sequenceMaybes [] = return Nothing
sequenceMaybes (x : xs) = x >>= maybe (sequenceMaybes xs) (return . Just)

-- | Find all the bound variables in a symbolic expression.
allBoundVars :: S.Elt t tp -> Set.Set (Some (S.SimpleBoundVar t))
allBoundVars e = runST (S.boundVars e >>= H.foldM f Set.empty)
  where f s (_, v) = return (Set.union s v)

-- | Given a map from location to bound variable, return all of the locations
-- that are actually used in an expression (along with their corresponding
-- variables).
extractUsedLocs :: (OrdF loc)
                => MapF.MapF loc (S.SimpleBoundVar t)
                -> S.Elt t tp
                -> MapF.MapF loc (S.SimpleBoundVar t)
extractUsedLocs locMapping = foldr f MapF.empty . allBoundVars
  where reversed = mapFReverse locMapping
        f (Some var) = MapF.insert (fromJust $ MapF.lookup var reversed) var
