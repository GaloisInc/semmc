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
  , mapFMapBothM
  , filterMapF
  ) where

import           Control.Monad.ST ( runST )
import qualified Data.HashTable.Class as H
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Set as Set
import           Text.Printf ( printf )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Utils.Hashable as Hash
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBackend.GroundEval as GE
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
                -> GE.GroundValue tp
                -> IO (S.SymExpr sym tp)
groundValToExpr sym BaseBoolRepr True = return (S.truePred sym)
groundValToExpr sym BaseBoolRepr False = return (S.falsePred sym)
groundValToExpr sym (BaseBVRepr w) val = S.bvLit sym w val
groundValToExpr sym BaseNatRepr val = S.natLit sym val
groundValToExpr sym BaseIntegerRepr val = S.intLit sym val
groundValToExpr sym BaseRealRepr val = S.realLit sym val
groundValToExpr sym BaseComplexRepr val = S.mkComplexLit sym val
groundValToExpr sym (BaseArrayRepr idxTp elemTp) (GE.ArrayConcrete base m) = do
  base' <- groundValToExpr sym elemTp base
  entries <- Hash.mkMap <$> traverse (groundValToExpr sym elemTp) m
  S.arrayFromMap sym idxTp entries base'
groundValToExpr _ (BaseArrayRepr _ _) (GE.ArrayMapping _) = error "groundValToExpr: ArrayMapping not handled"
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
extractUsedLocs :: forall t loc tp
                 . (OrdF loc)
                => MapF.MapF loc (S.SimpleBoundVar t)
                -> S.Elt t tp
                -> MapF.MapF loc (S.SimpleBoundVar t)
extractUsedLocs locMapping expr = MapF.mapMaybe keepIfNeeded locMapping
  where
    keepIfNeeded :: forall tp' . S.SimpleBoundVar t tp' -> Maybe (S.SimpleBoundVar t tp')
    keepIfNeeded bv' =
      case Set.member (Some bv') bvs of
        False -> Nothing
        True -> Just bv'
    bvs = allBoundVars expr

-- | Monadically map both keys and values of a 'MapF.MapF'.
mapFMapBothM :: forall k1 v1 k2 v2 m.
                (OrdF k2, Monad m)
             => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp))
             -> MapF.MapF k1 v1
             -> m (MapF.MapF k2 v2)
mapFMapBothM f = MapF.foldrWithKey f' (return MapF.empty)
  where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
        f' k v wrappedM = do
          (k', v') <- f k v
          m <- wrappedM
          return $ MapF.insert k' v' m

-- | Filter the elements of a 'MapF.MapF'.
filterMapF :: forall k v. (OrdF k) => (forall tp. k tp -> v tp -> Bool) -> MapF.MapF k v -> MapF.MapF k v
filterMapF f = MapF.foldrWithKey go MapF.empty
  where go :: forall tp. k tp -> v tp -> MapF.MapF k v -> MapF.MapF k v
        go key value m
          | f key value = MapF.insert key value m
          | otherwise   = m
