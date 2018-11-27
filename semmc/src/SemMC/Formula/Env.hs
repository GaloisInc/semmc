{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module SemMC.Formula.Env
  ( SomeSome(..)
  , Functions
  , FormulaEnv(..)
  , addLibrary
  ) where

import           Data.Kind
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )

import           What4.BaseTypes
import qualified What4.Interface as S

import SemMC.Formula.Formula

-- | Like 'Data.Parameterized.Some.Some', but for doubly-parameterized types.
data SomeSome (f :: k1 -> k2 -> Type) = forall x y. SomeSome (f x y)

type Functions sym arch = Map.Map String (SomeSome (S.SymFn sym), Some BaseTypeRepr)

-- | The environment in which formulas are parsed and interpreted. It contains
-- global information that must be shared across multiple formulas.
data FormulaEnv sym arch =
  FormulaEnv { envFunctions :: Functions sym arch
               -- ^ A mapping of all functions (either uninterpreted or defined)
               -- to be used in the formulas. This is necessary so that
               -- different formulas still refer to the same underlying
               -- function.
             , envUndefinedBit :: S.SymExpr sym (BaseBVType 1)
               -- ^ A 1-bit wide bitvector representing undefined parts of
               -- registers after some operation. This is globally necessary so
               -- that, when checking equality, if two different instructions
               -- set parts of registers to be undefined, they can still compare
               -- to be equal.
             }

addLibrary :: FormulaEnv sym arch -> Library sym -> FormulaEnv sym arch
addLibrary env funs =
  env { envFunctions = MapF.foldrWithKey addFun (envFunctions env) funs }
  where
    addFun :: FunctionRef sig -> FunctionFormula sym sig
           -> Functions sym arch -> Functions sym arch
    addFun (FunctionRef name _ _) ff m =
      Map.insert ("df." ++ name) (SomeSome (ffDef ff), Some (ffRetType ff)) m
