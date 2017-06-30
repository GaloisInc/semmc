{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module SemMC.Util
  ( groundValToExpr
  , makeSymbol
  , mapFKeys
  , mapFReverse
  , Witness (..)
  , sequenceMaybes
  , walkElt
  ) where

import Text.Printf

import           Control.Applicative ( Const(..) )
import           Data.Monoid ( (<>) )
import           Data.Parameterized.Classes
import           Data.Parameterized.TraversableFC
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           GHC.Exts ( Constraint )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
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

makeSymbol :: String -> SolverSymbol
makeSymbol name = case userSymbol sanitizedName of
                    Right symbol -> symbol
                    Left _ -> error $ printf "tried to create symbol with bad name: %s (%s)"
                                             name sanitizedName
  where
    sanitizedName = map (\c -> case c of ' ' -> '_'; _ -> c) name

-- | Convert a 'GroundValue' (a primitive type that represents the given
-- Crucible type) back into a symbolic expression, just as a literal.
groundValToExpr :: (S.IsExprBuilder sym)
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
groundValToExpr _ (BaseArrayRepr _ _) _ = error "groundValToExpr: array type isn't handled yet"
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
