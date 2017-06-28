{-|
Module  : SemMC.Synthesis.Template

Provides a method of working with "templated" instructions. These are
instructions that have concrete register operands, but abstract immediate
operands. This allows an SMT solver to later "fill in" the abstract operands,
given constraints.

-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
module SemMC.Synthesis.Template
  ( TemplatedArch
  , TemplatedFormula
  , TemplatableOpcode
  , TemplatedInstructionFormula(..)
  , tfOperandList
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
import           Data.Parameterized.Some
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.TypeLits ( KnownSymbol, sameSymbol, Symbol, symbolVal )
import           Unsafe.Coerce ( unsafeCoerce )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBackend as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import qualified Lang.Crucible.Solver.SimpleBuilder as S

import           Dismantle.Instruction ( mapOperandList, OperandList(..) )

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Formula.Instantiate
import           SemMC.Util

-- | Type used in an 'OperandList' for a templated instruction.
data TemplatedOperand (arch :: *) (s :: Symbol) where
  -- | A filled-in operand -- in practice, a particular register.
  Concrete :: Operand arch s -> TemplatedOperand arch s
  -- | A blank operand -- an immediate.
  Abstract :: (KnownSymbol s) => BaseTypeRepr (OperandType arch s) -> TemplatedOperand arch s

instance (ShowF (Operand arch)) => ShowF (TemplatedOperand arch) where
  showF (Concrete op) = "Concrete (" ++ showF op ++ ")"
  showF (Abstract _) = "Abstract"

instance (TestEquality (Operand arch)) => TestEquality (TemplatedOperand arch) where
  Concrete op1 `testEquality` Concrete op2 = (\Refl -> Refl) <$> op1 `testEquality` op2
  (Abstract _ :: TemplatedOperand arch s1) `testEquality` (Abstract _ :: TemplatedOperand arch s2) =
    (\Refl -> Refl) <$> (Proxy :: Proxy s1) `sameSymbol` (Proxy :: Proxy s2)
  _            `testEquality` _            = Nothing

instance (IsOperand (Operand arch)) => IsOperand (TemplatedOperand arch)

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
  operandValue _ sym newVars (Concrete op) = operandValue (Proxy :: Proxy arch) sym newVars op
  operandValue _ sym _ (Abstract tpRepr :: TemplatedOperand arch s) =
    S.freshConstant sym (makeSymbol (symbolVal (Proxy :: Proxy s))) tpRepr

  operandToLocation _ (Concrete op) = operandToLocation (Proxy :: Proxy arch) op
  operandToLocation _ (Abstract _)  = Nothing

  -- ?
  valueToOperand = undefined

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
        newOpVars = mapOperandList coerceBoundVar opVars
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
  TemplatedFormula { tfOperandList :: OperandList (TemplatedOperand arch) sh
                   , tfOperandExprs :: OperandList (WrappedExpr sym (TemplatedArch arch)) sh
                   , tfFormula :: Formula sym (TemplatedArch arch)
                   }
deriving instance (ShowF (Operand arch), ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => Show (TemplatedFormula sym arch sh)

instance (ShowF (Operand arch), ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => ShowF (TemplatedFormula sym arch) where
  showF = show

class KnownBool (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance KnownBool 'False where
  boolVal _ = False

instance KnownBool 'True where
  boolVal _ = True

-- | The only way to define structure-dependent operations on type-level lists...
class TemplatableOperands arch sh where
  -- | For a given shape, generate all possible templated operand lists.
  makeTemplatedOpLists :: [OperandList (TemplatedOperand arch) sh]
  -- | Recover the resulting concrete operands once the SMT solver has run.
  recoverOperands :: GroundEvalFn t
                  -> OperandList (TemplatedOperand arch) sh
                  -> OperandList (WrappedExpr (S.SimpleBackend t) (TemplatedArch arch)) sh
                  -> IO (OperandList (Operand arch) sh)

instance TemplatableOperands arch '[] where
  makeTemplatedOpLists = [Nil]
  recoverOperands _ Nil Nil = return Nil

instance (Architecture arch,
          KnownSymbol s,
          IsSpecificOperand (Operand arch) s,
          KnownBool (IsReg arch s),
          KnownRepr BaseTypeRepr (OperandType arch s),
          TemplatableOperands arch sh) =>
         TemplatableOperands arch (s ': sh) where
  makeTemplatedOpLists =
    -- We're in the List monad.
    -- This first @makeTemplatedOpLists@ call is to the oplist tail.
    makeTemplatedOpLists >>= \opList ->
      -- Ideally, this @boolVal@ would instead be at compile-time, but I
      -- couldn't figure out how to finagle the types to do that.
      case boolVal (Proxy :: Proxy (IsReg arch s)) of
        True -> map (\op -> Concrete op :> opList) allOperandValues
        False -> return $ Abstract (knownRepr :: BaseTypeRepr (OperandType arch s)) :> opList

  recoverOperands evalFn (Concrete op :> restOps) (_ :> restExprs) =
    (op :>) <$> recoverOperands evalFn restOps restExprs
  recoverOperands evalFn (Abstract _ :> restOps) (WrappedExpr expr :> restExprs) =
    (:>) <$> (valueToOperand (Proxy :: Proxy arch) <$> groundEval evalFn expr)
         <*> recoverOperands evalFn restOps restExprs

-- | Make a list of all possible 'TemplatedFormula's from a given
-- 'ParameterizedFormula'.
templatizeFormula :: (TemplateConstraints arch,
                      TemplatableOperands arch sh)
                  => S.SimpleBuilder t st
                  -> ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch) sh
                  -> [IO (TemplatedFormula (S.SimpleBuilder t st) arch sh)]
templatizeFormula sym pf = map mkFormula makeTemplatedOpLists
  where mkFormula ol = uncurry (TemplatedFormula ol) <$> instantiateFormula sym pf ol

templatizeFormula' :: (TemplateConstraints arch,
                       TemplatableOperands arch sh)
                   => S.SimpleBuilder t st
                   -> ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch) sh
                   -> IO [TemplatedFormula (S.SimpleBuilder t st) arch sh]
templatizeFormula' sym = sequence . templatizeFormula sym

type TemplatableOpcode arch = Witness (TemplatableOperands arch) ((Opcode arch) (Operand arch))

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

-- XXX: ...what should I name this?
foo :: forall a. [a] -> [[a]]
foo xs = join [foo' i | i <- [1..]]
  where foo' :: Int -> [[a]]
        foo' 1 = map pure xs
        foo' n = [x : xs' | x <- xs, xs' <- foo' (n-1)]

-- | An infinite stream of templated instruction sequences.
templatedInstructions :: (TemplateConstraints arch)
                      => S.SimpleBuilder t st
                      -> MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch))
                      -> IO [[TemplatedInstructionFormula (S.SimpleBuilder t st) arch]]
templatedInstructions sym m = foo . join <$> mapM f (MapF.toList m)
  where f (MapF.Pair (Witness op) pf) = fmap (map (TemplatedInstructionFormula op)) (templatizeFormula' sym pf)
