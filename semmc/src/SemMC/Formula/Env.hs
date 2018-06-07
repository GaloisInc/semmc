{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module SemMC.Formula.Env
  ( SomeSome(..)
  , Functions
  , FormulaEnv(..)
  ) where

import qualified Data.Map.Strict as Map

import           What4.BaseTypes
import qualified What4.Interface as S

-- | Like 'Data.Parameterized.Some.Some', but for doubly-parameterized types.
data SomeSome (f :: k1 -> k2 -> *) = forall x y. SomeSome (f x y)

type Functions sym arch = Map.Map String (SomeSome (S.SymFn sym), Some BaseTypeRepr)

-- | The environment in which formulas are parsed and interpreted. It contains
-- global information that must be shared across multiple formulas.
data FormulaEnv sym arch =
  FormulaEnv { envFunctions :: Functions sym arch
               -- ^ A mapping of all functions (either uninterpreted or defined) to
               -- be used in the formulas. This is necessary so that different
               -- formulas still refer to the same underlying function.
             , envUndefinedBit :: S.SymExpr sym (BaseBVType 1)
               -- ^ A 1-bit wide bitvector representing undefined parts of
               -- registers after some operation. This is globally necessary so
               -- that, when checking equality, if two different instructions
               -- set parts of registers to be undefined, they can still compare
               -- to be equal.
             }
