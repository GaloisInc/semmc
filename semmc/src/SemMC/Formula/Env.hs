{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module SemMC.Formula.Env
  ( SomeSome(..)
  , UninterpretedFunctions
  , FormulaEnv(..)
  ) where

import qualified Data.Map.Strict as Map

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

data SomeSome (f :: k1 -> k2 -> *) = forall x y. SomeSome (f x y)

type UninterpretedFunctions sym = Map.Map String (SomeSome (S.SymFn sym))

data FormulaEnv sym arch =
  FormulaEnv { envFunctions :: UninterpretedFunctions sym
             , envUndefinedBit :: S.SymExpr sym (BaseBVType 1)
             }
