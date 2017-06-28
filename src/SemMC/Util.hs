{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module SemMC.Util
  ( groundValToExpr
  , makeSymbol
  , mapFKeys
  , Witness (..)
  ) where

import Text.Printf

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           GHC.Exts ( Constraint )

import           Lang.Crucible.BaseTypes
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

mapFKeys :: forall (key :: k -> *) (value :: k -> *). MapF.MapF key value -> [Some key]
mapFKeys = MapF.foldrWithKey (\k _ l -> Some k : l) []
