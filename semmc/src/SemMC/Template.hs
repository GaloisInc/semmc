{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Template (
  RecoverOperandFn(..),
  TemplatedOperandFn,
  TemplatedOperand(..),
  TemplatableOperand(..)
  ) where

import           Data.Kind ( Type )
import           Data.Parameterized.Classes ( ShowF(..) )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )
import qualified What4.Interface as WI
import qualified What4.Expr.GroundEval as WEG

import qualified SemMC.Architecture as A

-- | A function that allows you to recover the concrete value of a templated
-- operand given a concrete evaluation function, typically provided as the model
-- from an SMT solver.
newtype RecoverOperandFn sym op =
  RecoverOperandFn { unRecOpFn :: (forall tp. WI.SymExpr sym tp -> IO (WEG.GroundValue tp)) -> IO op }

-- | The bulk of what a 'TemplatedOperand' is. Reading off the type in English:
-- given a symbolic expression builder and a mapping from machine location to
-- symbolic expression, return (in IO) both an expression representing the
-- templated operand and a way to recover a concrete operand.
--
-- The idea is that the expression has all the register-related information
-- filled in directly, but all immediates are symbolic. The reason this is a
-- function and not the expression/recovery function themselves is that we need
-- a separation between "template possibilities" generation time and actual
-- formula generation time.
type TemplatedOperandFn arch s = forall sym.
                                 (WI.IsExprBuilder sym,
                                  WI.IsSymExprBuilder sym)
                              => sym
                              -> (forall tp. A.Location arch tp -> IO (WI.SymExpr sym tp))
                              -> IO ( A.AllocatedOperand arch sym s
                                    , RecoverOperandFn sym (A.Operand arch s)
                                    )

-- | An operand for 'TemplatedArch'.
data TemplatedOperand (arch :: Type) (s :: Symbol) =
  TemplatedOperand { templOpLocation :: Maybe (A.Location arch (A.OperandType arch s))
                   -- ^ If this operand represents a location, this is it.
                   , templUsedLocations :: Set.Set (Some (A.Location arch))
                   -- ^ Locations used by this operand.
                   , templOpFn :: TemplatedOperandFn arch s
                   -- ^ How to get an expression and recovery function for this
                   -- operand.
                   }

instance ShowF (A.Location arch) => Show (TemplatedOperand arch s) where
  show op = show (templUsedLocations op)


instance (ShowF (A.Operand arch), ShowF (A.Location arch)) => ShowF (TemplatedOperand arch)

instance A.IsOperand (TemplatedOperand arch)

-- | A specific type of operand of which you can generate templates.
class TemplatableOperand (arch :: Type) where
  -- | All possible templates of an operand. In a nutshell, fill in register
  -- parts, leave immediate parts symbolic.
  opTemplates :: A.OperandTypeRepr arch s -> [TemplatedOperand arch s]
