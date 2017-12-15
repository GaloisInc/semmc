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
  , TemplateConstraints
  , TemplatedOperandFn
  , TemplatedOperand(..)
  , WrappedRecoverOperandFn(..)
  , TemplatedArch
  , TemplatedFormula(..)
  , TemplatableOperand(..)
  , TemplatableOperands
  , TemplatableOpcode
  , TemplatedInstruction(..)
  , TemplatedInstructionFormula(..)
  , tifFormula
  -- , templatizeFormula
  -- , templatizeFormula'
  , templatedInstructions
  , templatedInputs
  , templatedOutputs
  , genTemplatedFormula
  , recoverOperands
  , unTemplateUnsafe
  , unTemplateSafe
  , unTemplate
  ) where

import           Data.EnumF
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.Some
import qualified Data.Parameterized.List as SL
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
import qualified SemMC.BoundVar as BV
import           SemMC.Formula

--
-- First, an overview about how the templating system works.
--
-- There are two stages of templating. The first stage generates 'TemplatedInstruction's. These associate an opcode

-- | A function that allows you to recover the concrete value of a templated
-- operand given a concrete evaluation function, typically provided as the model
-- from an SMT solver.
type RecoverOperandFn sym op = (forall tp. S.SymExpr sym tp -> IO (GroundValue tp)) -> IO op

-- | Just what it sounds like. Haskell doesn't deal that well with RankNTypes.
newtype WrappedRecoverOperandFn sym op =
  WrappedRecoverOperandFn { unWrappedRecOpFn :: RecoverOperandFn sym op }

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
                                 (S.IsExprBuilder sym,
                                  S.IsSymInterface sym)
                              => sym
                              -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
                              -> IO (S.SymExpr sym (OperandType arch s),
                                     WrappedRecoverOperandFn sym (Operand arch s))

-- | An operand for 'TemplatedArch'.
data TemplatedOperand (arch :: *) (s :: Symbol) =
  TemplatedOperand { templOpLocation :: Maybe (Location arch (OperandType arch s))
                   -- ^ If this operand represents a location, this is it.
                   , templUsedLocations :: Set.Set (Some (Location arch))
                   -- ^ Locations used by this operand.
                   , templOpFn :: TemplatedOperandFn arch s
                   -- ^ How to get an expression and recovery function for this
                   -- operand.
                   }

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

-- | Necessary constraints for 'TemplatedArch' to be valid.
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

  uninterpretedFunctions _ = uninterpretedFunctions (Proxy @arch)

  operandValue _ sym locLookup (TemplatedOperand _ _ f) =
    uncurry TaggedExpr <$> f sym locLookup

  operandToLocation _ (TemplatedOperand loc _ _) = loc
  shapeReprToTypeRepr _ = shapeReprToTypeRepr (Proxy @arch)
  locationFuncInterpretation _ = error "locationFuncInterpretation shouldn't need to be used from a TemplatedArch"

instance Show (S.SymExpr sym (OperandType arch s)) => Show (TaggedExpr (TemplatedArch arch) sym s) where
  show (TaggedExpr expr _) = "TaggedExpr (" ++ show expr ++ ")"

instance ShowF (S.SymExpr sym) => ShowF (TaggedExpr (TemplatedArch arch) sym) where
  withShow (_ :: p (TaggedExpr (TemplatedArch arch) sym)) (_ :: q s) x =
    withShow (Proxy @(S.SymExpr sym)) (Proxy @(OperandType arch s)) x

-- | Convert a 'ParameterizedFormula' that was created using a 'TemplatedArch'
-- to the base architecture, using 'unsafeCoerce'.
--
-- The only difference between 'TemplatedArch arch' and 'arch' is in 'Operand',
-- which 'ParameterizedFormula' doesn't use, so 'unsafeCoerce' should be safe.
-- I'm not entirely sure, though.
unTemplateUnsafe :: ParameterizedFormula sym (TemplatedArch arch) sh
                 -> ParameterizedFormula sym arch sh
unTemplateUnsafe = unsafeCoerce

coerceParameter :: Parameter (TemplatedArch arch) sh tp -> Parameter arch sh tp
coerceParameter (OperandParameter tp idx) = OperandParameter tp idx
coerceParameter (LiteralParameter loc) = LiteralParameter loc
coerceParameter (FunctionParameter name (WrappedOperand orep oix) r) = FunctionParameter name (WrappedOperand orep oix) r

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
        coerceBoundVar :: forall op. BV.BoundVar sym (TemplatedArch arch) op -> BV.BoundVar sym arch op
        coerceBoundVar (BV.BoundVar var) = BV.BoundVar var

-- | Convert a 'ParameterizedFormula' that was created using a 'TemplatedArch'
-- to the base architecture, using 'unsafeCoerce'.
--
-- This is exactly 'unTemplateUnsafe', but here in case we want to switch the
-- default to the safe implementation.
unTemplate :: (OrdF (Location arch))
           => ParameterizedFormula sym (TemplatedArch arch) sh
           -> ParameterizedFormula sym arch sh
unTemplate = unTemplateSafe

-- | 'Formula' along with the expressions that correspond to each operand (for
-- pulling out the values of immediates after solving, primarily).
data TemplatedFormula sym arch sh =
  TemplatedFormula { tfOperandExprs :: SL.List (TaggedExpr (TemplatedArch arch) sym) sh
                   , tfFormula :: Formula sym (TemplatedArch arch)
                   }
deriving instance (ShowF (Operand arch), ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => Show (TemplatedFormula sym arch sh)

instance (ShowF (Operand arch), ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => ShowF (TemplatedFormula sym arch) where
  showF = show

-- | A specific type of operand of which you can generate templates.
class TemplatableOperand (arch :: *) (s :: Symbol) where
  -- | All possible templates of an operand. In a nutshell, fill in register
  -- parts, leave immediate parts symbolic.
  opTemplates :: [TemplatedOperand arch s]

-- | The only way to define structure-dependent operations on type-level lists...
class TemplatableOperands arch sh where
  -- | For a given shape, generate all possible templated operand lists.
  makeTemplatedOpLists :: [SL.List (TemplatedOperand arch) sh]
  -- | Recover the resulting concrete operands once the SMT solver has run.
  recoverOperands :: (forall tp. S.SymExpr sym tp -> IO (GroundValue tp))
                  -> SL.List (TaggedExpr (TemplatedArch arch) sym) sh
                  -> IO (SL.List (Operand arch) sh)

instance TemplatableOperands arch '[] where
  makeTemplatedOpLists = [SL.Nil]
  recoverOperands _ SL.Nil = return SL.Nil

instance (Architecture arch,
          TemplatableOperand arch s,
          TemplatableOperands arch sh) =>
         TemplatableOperands arch (s ': sh) where
  makeTemplatedOpLists =
    -- We're in the List applicative.
    (SL.:<) <$> opTemplates
            <*> makeTemplatedOpLists

  recoverOperands evalFn (TaggedExpr _ (WrappedRecoverOperandFn recover) SL.:< restExprs) =
    -- Now we're in IO.
    (SL.:<) <$> recover evalFn
            <*> recoverOperands evalFn restExprs

type TemplatableOpcode arch = Witness (TemplatableOperands arch) ((Opcode arch) (Operand arch))

type BaseSet sym arch = MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula sym (TemplatedArch arch))

data TemplatedInstruction sym arch sh where
  TemplatedInstruction :: (TemplatableOperands arch sh)
                       => (Opcode arch) (Operand arch) sh
                       -> ParameterizedFormula sym (TemplatedArch arch) sh
                       -> SL.List (TemplatedOperand arch) sh
                       -> TemplatedInstruction sym arch sh

deriving instance (Show ((Opcode arch) (Operand arch) sh),
                   Show (ParameterizedFormula sym (TemplatedArch arch) sh),
                   Show (SL.List (TemplatedOperand arch) sh))
  => Show (TemplatedInstruction sym arch sh)

instance (ShowF ((Opcode arch) (Operand arch)),
          ShowF (ParameterizedFormula sym (TemplatedArch arch)),
          ShowF (TemplatedOperand arch))
  => ShowF (TemplatedInstruction sym arch) where
  withShow (_ :: p (TemplatedInstruction sym arch)) (_ :: q sh) x =
    withShow (Proxy @((Opcode arch) (Operand arch))) (Proxy @sh) $
      withShow (Proxy @(ParameterizedFormula sym (TemplatedArch arch ))) (Proxy @sh) $
        withShow (Proxy @(SL.List (TemplatedOperand arch))) (Proxy @sh) $
          x

-- | Get the set of locations that a 'TemplatedInstruction' uses.
templatedInputs :: (OrdF (Location arch))
                => TemplatedInstruction sym arch sh
                -> Set.Set (Some (Location arch))
templatedInputs (TemplatedInstruction _ pf oplist) =
  mconcat (map paramUses (Set.toList (pfUses pf)))
  where paramUses (Some param) =
          case param of
            OperandParameter _ idx ->
              let TemplatedOperand _ uses _ = oplist SL.!! idx
              in uses
            LiteralParameter loc -> Set.singleton (Some loc)
            FunctionParameter {} -> error "Function parameters are not actually inputs: they can only be defined"

-- | Get the set of locations that a 'TemplatedInstruction' defines.
templatedOutputs :: (OrdF (Location arch))
                 => TemplatedInstruction sym arch sh
                 -> Set.Set (Some (Location arch))
templatedOutputs (TemplatedInstruction _ pf oplist) =
  mconcat (map paramDefs (MapF.keys (pfDefs pf)))
  where paramDefs (Some param) =
          case param of
            OperandParameter _ idx ->
              case oplist SL.!! idx of
                TemplatedOperand (Just loc) _ _ -> Set.singleton (Some loc)
                _ -> Set.empty
            LiteralParameter loc -> Set.singleton (Some loc)

templatedInstructions :: (TemplateConstraints arch)
                      => BaseSet sym arch
                      -> [Some (TemplatedInstruction sym arch)]
templatedInstructions baseSet = do
  Pair (Witness opcode) pf <- MapF.toList baseSet
  oplist <- makeTemplatedOpLists
  return . Some $! TemplatedInstruction opcode pf oplist

-- | An opcode along with a 'TemplatedFormula' that implements it for specific
-- templated operands.
data TemplatedInstructionFormula sym arch where
  TemplatedInstructionFormula :: TemplatedInstruction sym arch sh
                              -> TemplatedFormula sym arch sh
                              -> TemplatedInstructionFormula sym arch

tifFormula :: TemplatedInstructionFormula sym arch -> Formula sym arch
tifFormula (TemplatedInstructionFormula _ tf) = coerceFormula (tfFormula tf)

genTemplatedFormula :: (TemplateConstraints arch)
                    => S.SimpleBuilder t st
                    -> TemplatedInstruction (S.SimpleBuilder t st) arch sh
                    -> IO (TemplatedInstructionFormula (S.SimpleBuilder t st) arch)
genTemplatedFormula sym ti@(TemplatedInstruction _ pf oplist) =
  TemplatedInstructionFormula ti . uncurry TemplatedFormula <$> instantiateFormula sym pf oplist
