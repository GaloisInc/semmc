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
  , mapFKeys
  , mapFReverse
  , Witness (..)
  , sequenceMaybes
  , walkElt
  , extractUsedLocs
  , Equal
  ) where

import           Control.Applicative ( Const(..) )
import           Control.Monad.ST ( runST )
import           Data.Foldable ( foldrM )
import qualified Data.HashTable.Class as H
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromJust )
import           Data.Monoid ( (<>) )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableFC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           GHC.Exts ( Constraint )
import           Text.Printf

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Utils.Hashable as Hash
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import           Lang.Crucible.Solver.Symbol ( SolverSymbol, userSymbol )

data Witness (c :: k -> Constraint) (f :: k -> *) (x :: k) where
  Witness :: (c x) => f x -> Witness c f x

instance (TestEquality f) => TestEquality (Witness c f) where
  testEquality (Witness x) (Witness y) = (\Refl -> Refl) <$> testEquality x y

instance (OrdF f) => OrdF (Witness c f) where
  compareF (Witness x) (Witness y) =
    case compareF x y of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

instance (Show (f x)) => Show (Witness c f x) where
  show (Witness d) = "Witness (" ++ show d ++ ")"

witnessWithShow :: forall p c f q tp a. (ShowF f) => p (Witness c f) -> q tp -> (Show (Witness c f tp) => a) -> a
witnessWithShow _ _ = withShow (Proxy @f) (Proxy @tp)

instance (ShowF f) => ShowF (Witness c f) where
  withShow = witnessWithShow

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

-- * MapF Utilities

-- | Extract the keys of a 'MapF'.
mapFKeys :: forall (key :: k -> *) (value :: k -> *). MapF.MapF key value -> [Some key]
mapFKeys = MapF.foldrWithKey (\k _ l -> Some k : l) []

-- | Reverse a MapF, so that the old keys are the new values and the old values
-- are the new keys.
mapFReverse :: (OrdF value) => MapF.MapF key value -> MapF.MapF value key
mapFReverse = MapF.foldrWithKey (flip MapF.insert) MapF.empty

-- This short-circuits, useful for long (or infinite) lists.
sequenceMaybes :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
sequenceMaybes [] = return Nothing
sequenceMaybes (x : xs) = x >>= maybe (sequenceMaybes xs) (return . Just)

-- | Walk the tree of the expression, collecting results in the monoid. Visits
-- nodes before leaves.
walkElt :: (Monoid m) => (forall tp'. S.Elt t tp' -> m) -> S.Elt t tp -> m
walkElt f e@(S.AppElt appElt) = f e <> down
  where down = getConst (traverseFC_ (Const . walkElt f) (S.appEltApp appElt))
walkElt f e@(S.NonceAppElt nonceAppElt) = f e <> down
  where down = getConst (traverseFC_ (Const . walkElt f) (S.nonceEltApp nonceAppElt))
walkElt f e = f e

allBoundVars :: S.Elt t tp -> Set.Set (Some (S.SimpleBoundVar t))
allBoundVars e = runST (S.boundVars e >>= H.foldM f Set.empty)
  where f s (_, v) = return (Set.union s v)

extractUsedLocs :: (OrdF loc)
                => MapF.MapF loc (S.SimpleBoundVar t)
                -> S.Elt t tp
                -> MapF.MapF loc (S.SimpleBoundVar t)
extractUsedLocs locMapping = foldr f MapF.empty . allBoundVars
  where reversed = mapFReverse locMapping
        f (Some var) = MapF.insert (fromJust $ MapF.lookup var reversed) var

type family Equal (a :: k1) (b :: k2) :: Bool where
  Equal a a = 'True
  Equal a b = 'False
