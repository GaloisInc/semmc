{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module SemMC.Architecture.AllocatedOperand (
  AllocatedOperand(..),
  OperandComponents
  ) where

import           GHC.TypeLits ( Symbol )
import           Data.Parameterized.Classes ( ShowF, showF )

import qualified What4.BaseTypes as WT
import qualified What4.Interface as WI

import           SemMC.Architecture.Internal ( OperandType )
import           SemMC.Architecture.Location ( Location, IsLocation )

-- | This is a deconstructed version of an operand that contains symbolic
-- expressions ('WI.SymExpr') for each constituent component of the operand.  It
-- is intended to be the payload of the 'TaggedExpr' for each instance of the
-- 'Architecture'.
data AllocatedOperand arch sym (s :: Symbol) where
  -- | A simple operand that represents an immediate value
  ValueOperand :: (OperandType arch s ~ WT.BaseBVType n)
               => WI.SymExpr sym (OperandType arch s)
               -> AllocatedOperand arch sym s
  -- | A value representing an operand backed by a Location
  LocationOperand :: (OperandType arch s ~ WT.BaseBVType n)
                  => Location arch (OperandType arch s)
                  -> WI.SymExpr sym (OperandType arch s)
                  -> AllocatedOperand arch sym s
  -- | A compound operand with an arch-specific representation
  CompoundOperand :: OperandComponents arch sym s -> AllocatedOperand arch sym s

instance (WI.IsExprBuilder sym, IsLocation (Location arch), ShowF (OperandComponents arch sym)) => Show (AllocatedOperand arch sym s) where
  show ao =
    case ao of
      ValueOperand s -> "ValueOperand " ++ show (WI.printSymExpr s)
      LocationOperand l s -> "LocationOperand " ++ showF l ++ " " ++ show (WI.printSymExpr s)
      CompoundOperand oc -> "CompoundOperand " ++ showF oc

-- | A data type that contains broken out /symbolic/ components for each operand type.
--
-- This is used for instantiating formulas and during evaluation of functions
-- embedded in instruction semantics (e.g., the helpers that de-construct
-- compound data types).
--
-- This type is also closely tied to the instruction templates used for synthesis.
--
-- Each operand type should have a corresponding constructor in this type
-- where /concrete/ operand components are stored alongside the symbolic
-- values that correspond to them.  In the case of register values (i.e.,
-- Locations), these will be symbolic expressions that stand for those
-- locations (uniquely allocated per-instruction).  For immediates held in
-- operands, there are two cases. In the case of concrete instructions, these
-- will just be literal SymExprs.  For instruction templates used in
-- synthesis, they will be symbolic values (which are also SymExprs).
type family OperandComponents arch sym :: Symbol -> *
