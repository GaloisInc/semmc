{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
module SemMC.Util
  ( groundValToExpr
  , makeSymbol
  , mapFKeys
  ) where

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import           Lang.Crucible.Solver.Symbol ( SolverSymbol, userSymbol )

makeSymbol :: String -> SolverSymbol
makeSymbol name = case userSymbol name of
                    Right symbol -> symbol
                    Left _ -> error "tried to create symbol with bad name"

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
