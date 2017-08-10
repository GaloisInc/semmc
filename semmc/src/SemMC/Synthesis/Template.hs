{-|
Module  : SemMC.Synthesis.Template

Provides a method of working with "templated" instructions. These are
instructions that have concrete register operands, but abstract immediate
operands. This allows an SMT solver to later "fill in" the abstract operands,
given constraints.

-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Synthesis.Template
  ( BaseSet
  , TemplatedOperandFn
  , TemplatedOperand(..)
  , WrappedRecoverOperandFn(..)
  , TemplatedArch
  , TemplatedFormula(..)
  , TemplatableOperand(..)
  , TemplatableOperands
  , TemplatableOpcode
  , TemplatedInstructionFormula(..)
  , tifFormula
  , tfOperandExprs
  , tfFormula
  , templatizeFormula
  , templatizeFormula'
  , templatedInstructions
  , recoverOperands
  , unTemplateUnsafe
  , unTemplateSafe
  , unTemplate
  ) where

import           Control.Monad ( join )
import           Data.EnumF
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.Some
import           Data.Parameterized.ShapedList ( ShapedList(..) )
import           Data.Parameterized.TraversableFC ( FunctorFC(..) )
import           Data.Parameterized.Witness ( Witness(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.TypeLits ( Symbol )
import           Unsafe.Coerce ( unsafeCoerce )

import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import qualified Lang.Crucible.Solver.SimpleBuilder as S

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Formula.Instantiate

type RecoverOperandFn sym op = (forall tp. S.SymExpr sym tp -> IO (GroundValue tp)) -> IO op

newtype WrappedRecoverOperandFn sym op =
  WrappedRecoverOperandFn { unWrappedRecOpFn :: RecoverOperandFn sym op }

type TemplatedOperandFn arch s = forall sym.
                                 (S.IsExprBuilder sym,
                                  S.IsSymInterface sym)
                              => sym
                              -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
                              -> IO (S.SymExpr sym (OperandType arch s),
                                     WrappedRecoverOperandFn sym (Operand arch s))

data TemplatedOperand (arch :: *) (s :: Symbol) =
  TemplatedOperand (Maybe (Location arch (OperandType arch s))) (TemplatedOperandFn arch s)

instance Show (TemplatedOperand arch s) where
  show _ = "some weird templated operand"

instance (ShowF (Operand arch)) => ShowF (TemplatedOperand arch)

instance IsOperand (TemplatedOperand arch)


-- | Phantom architecture used to signal we want to use template operands when
-- instantiating a formula, rather than all concrete operands.
data TemplatedArch (arch :: *)

type instance Operand (TemplatedArch arch) = TemplatedOperand arch
type instance Opcode (TemplatedArch arch) = Opcode arch
type instance OperandType (TemplatedArch arch) s = OperandType arch s
type instance Location (TemplatedArch arch) = Location arch

type TemplateConstraints arch = (Architecture arch,
                                 Typeable arch,
                                 OrdF ((Opcode arch) (TemplatedOperand arch)),
                                 ShowF ((Opcode arch) (TemplatedOperand arch)),
                                 EnumF ((Opcode arch) (TemplatedOperand arch)))

instance (TemplateConstraints arch) => Architecture (TemplatedArch arch) where
  data TaggedExpr (TemplatedArch arch) sym s =
    TaggedExpr { taggedExpr :: S.SymExpr sym (OperandType arch s)
               , taggedRecover :: WrappedRecoverOperandFn sym (Operand arch s)
               }

  unTagged = taggedExpr

  operandValue _ sym locLookup (TemplatedOperand _ f) = do
    (expr, recover) <- f sym locLookup
    return (TaggedExpr expr recover)

  operandToLocation _ (TemplatedOperand loc _ ) = loc

instance Show (S.SymExpr sym (OperandType arch s)) => Show (TaggedExpr (TemplatedArch arch) sym s) where
  show (TaggedExpr expr _) = "TaggedExpr (" ++ show expr ++ ")"

taggedWithShow :: forall sym arch p q s a. (ShowF (S.SymExpr sym)) => p (TaggedExpr (TemplatedArch arch) sym) -> q s -> (Show (TaggedExpr (TemplatedArch arch) sym s) => a) -> a
taggedWithShow _ _ = withShow (Proxy @(S.SymExpr sym)) (Proxy @(OperandType arch s))

instance ShowF (S.SymExpr sym) => ShowF (TaggedExpr (TemplatedArch arch) sym) where
  withShow = taggedWithShow

-- | Convert a 'ParameterizedFormula' that was created using a 'TemplatedArch'
-- to the base architecture, using 'unsafeCoerce'.
--
-- The only difference between 'TemplatedArch arch' and 'arch' is in 'Operand',
-- which 'ParameterizedFormula' doesn't use, so 'unsafeCoerce' should be safe.
-- I'm not entirely sure, though.
unTemplateUnsafe :: ParameterizedFormula sym (TemplatedArch arch) sh
                 -> ParameterizedFormula sym arch sh
unTemplateUnsafe = unsafeCoerce

-- | Convert a 'ParameterizedFormula' that was created using a 'TemplatedArch'
-- to the base architecture, using a manual mapping.
--
-- This is a safe version, but it's probably a lot slower.
unTemplateSafe :: forall sym arch sh.
                  (OrdF (Location arch))
               => ParameterizedFormula sym (TemplatedArch arch) sh
               -> ParameterizedFormula sym arch sh
unTemplateSafe (ParameterizedFormula { pfUses = uses
                                     , pfOperandVars = opVars
                                     , pfLiteralVars = litVars
                                     , pfDefs = defs
                                     }) =
  ParameterizedFormula { pfUses = newUses
                       , pfOperandVars = newOpVars
                       , pfLiteralVars = litVars
                       , pfDefs = newDefs
                       }
  where newUses = Set.map (mapSome coerceParameter) uses
        newOpVars = fmapFC coerceBoundVar opVars
        newDefs = MapF.foldrWithKey (MapF.insert . coerceParameter) MapF.empty defs
        coerceParameter :: forall tp. Parameter (TemplatedArch arch) sh tp -> Parameter arch sh tp
        coerceParameter (Operand tpRepr idx) = Operand tpRepr idx
        coerceParameter (Literal loc) = Literal loc
        coerceBoundVar :: forall op. BoundVar sym (TemplatedArch arch) op -> BoundVar sym arch op
        coerceBoundVar (BoundVar var) = BoundVar var

-- | Convert a 'ParameterizedFormula' that was created using a 'TemplatedArch'
-- to the base architecture, using 'unsafeCoerce'.
--
-- This is exactly 'unTemplateUnsafe', but here in case we want to switch the
-- default to the safe implementation.
unTemplate :: (OrdF (Location arch))
           => ParameterizedFormula sym (TemplatedArch arch) sh
           -> ParameterizedFormula sym arch sh
unTemplate = unTemplateUnsafe


-- | 'Formula' along with the expressions that correspond to each operand (for
-- pulling out the values of immediates after solving, primarily).
data TemplatedFormula sym arch sh =
  TemplatedFormula { tfOperandExprs :: ShapedList (TaggedExpr (TemplatedArch arch) sym) sh
                   , tfFormula :: Formula sym (TemplatedArch arch)
                   }
deriving instance (ShowF (Operand arch), ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => Show (TemplatedFormula sym arch sh)

instance (ShowF (Operand arch), ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => ShowF (TemplatedFormula sym arch) where
  showF = show

class TemplatableOperand (arch :: *) (s :: Symbol) where
  opTemplates :: [TemplatedOperand arch s]

-- | The only way to define structure-dependent operations on type-level lists...
class TemplatableOperands arch sh where
  -- | For a given shape, generate all possible templated operand lists.
  makeTemplatedOpLists :: [ShapedList (TemplatedOperand arch) sh]
  -- | Recover the resulting concrete operands once the SMT solver has run.
  recoverOperands :: (forall tp. S.SymExpr sym tp -> IO (GroundValue tp))
                  -> ShapedList (TaggedExpr (TemplatedArch arch) sym) sh
                  -> IO (ShapedList (Operand arch) sh)

instance TemplatableOperands arch '[] where
  makeTemplatedOpLists = [Nil]
  recoverOperands _ Nil = return Nil

instance (Architecture arch,
          TemplatableOperand arch s,
          TemplatableOperands arch sh) =>
         TemplatableOperands arch (s ': sh) where
  makeTemplatedOpLists =
    -- We're in the List applicative.
    (:>) <$> opTemplates
         <*> makeTemplatedOpLists

  recoverOperands evalFn (TaggedExpr _ (WrappedRecoverOperandFn recover) :> restExprs) =
    -- Now we're in IO.
    (:>) <$> recover evalFn
         <*> recoverOperands evalFn restExprs

type TemplatableOpcode arch = Witness (TemplatableOperands arch) ((Opcode arch) (Operand arch))

type BaseSet sym arch = MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula sym (TemplatedArch arch))

-- | Make a list of all possible 'TemplatedFormula's from a given
-- 'ParameterizedFormula'.
templatizeFormula :: (TemplateConstraints arch,
                      TemplatableOperands arch sh)
                  => S.SimpleBuilder t st
                  -> ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch) sh
                  -> [IO (TemplatedFormula (S.SimpleBuilder t st) arch sh)]
templatizeFormula sym pf = map mkFormula makeTemplatedOpLists
  where mkFormula ol = uncurry TemplatedFormula <$> instantiateFormula sym pf ol

templatizeFormula' :: (TemplateConstraints arch,
                       TemplatableOperands arch sh)
                   => S.SimpleBuilder t st
                   -> ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch) sh
                   -> IO [TemplatedFormula (S.SimpleBuilder t st) arch sh]
templatizeFormula' sym = sequence . templatizeFormula sym

-- | An opcode along with a 'TemplatedFormula' that implements it for specific
-- templated operands.
data TemplatedInstructionFormula sym arch where
  TemplatedInstructionFormula :: (TemplatableOperands arch sh)
                              => (Opcode arch) (Operand arch) sh
                              -> TemplatedFormula sym arch sh
                              -> TemplatedInstructionFormula sym arch

instance (ShowF ((Opcode arch) (Operand arch)),
          ShowF (TemplatedFormula sym arch))
       => Show (TemplatedInstructionFormula sym arch) where
  show (TemplatedInstructionFormula op tf) =
    unwords ["TemplatedInstructionFormula", showF op, showF tf]

tifFormula :: TemplatedInstructionFormula sym arch -> Formula sym arch
tifFormula (TemplatedInstructionFormula _ tf) = coerceFormula (tfFormula tf)

-- | A list of all possible templated instructions, given some opcodes.
templatedInstructions :: (TemplateConstraints arch)
                      => S.SimpleBuilder t st
                      -> BaseSet (S.SimpleBuilder t st) arch
                      -> IO [TemplatedInstructionFormula (S.SimpleBuilder t st) arch]
templatedInstructions sym m = join <$> mapM f (MapF.toList m)
  where f (Pair (Witness op) pf) = fmap (map (TemplatedInstructionFormula op)) (templatizeFormula' sym pf)
