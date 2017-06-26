{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Definitions of formulas
module SemMC.Formula (
  ParameterizedFormula(..),
  Formula(..),
  emptyFormula,
  coerceFormula,
  Parameter(..),
  paramType,
  BaseSet(..),
  Index(..),
  indexOpList,
  -- FormulaVar(..),
  loadBaseSet,
  formulaFromProgram
  ) where

import           Control.Monad ( foldM )
import qualified Control.Monad.Catch as C
import           Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )

import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.BaseTypes

import qualified Dismantle.Instruction as I

import           SemMC.Architecture

-- | Represents an index into a type-level list. Used in place of integers to
--   1. ensure that the given index *does* exist in the list
--   2. guarantee that it has the given kind
data Index :: [k] -> k -> * where
  IndexHere :: forall x sh. Index (x ': sh) x
  IndexThere :: forall x x' sh. Index sh x -> Index (x' ': sh) x
deriving instance Eq (Index sh x)
deriving instance Show (Index sh x)

instance TestEquality (Index sh) where
  IndexHere `testEquality` IndexHere = Just Refl
  IndexThere idx1 `testEquality` IndexThere idx2 = testEquality idx1 idx2
  _ `testEquality` _ = Nothing

instance OrdF (Index sh) where
  IndexHere `compareF` IndexHere = EQF
  IndexHere `compareF` IndexThere _ = LTF
  IndexThere _ `compareF` IndexHere = GTF
  IndexThere idx1 `compareF` IndexThere idx2 =
    case idx1 `compareF` idx2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

instance Ord (Index sh x) where
  x `compare` y = toOrdering $ x `compareF` y

-- | Evaluate an index for a given operand list.
indexOpList :: I.OperandList f sh -> Index sh s -> f s
-- Why not destructure @vals@ in the argument position? GHC gives a warning
-- about not handling the Nil case of vals. This way, GHC verifies that the
-- pattern-matching is exhaustive.
indexOpList vals IndexHere = case vals of x I.:> _ -> x
indexOpList vals (IndexThere th) = case vals of _ I.:> rest -> indexOpList rest th

data Parameter arch (sh :: [Symbol]) (tp :: BaseType) where
  Operand :: BaseTypeRepr (OperandType arch s) -> Index sh s -> Parameter arch sh (OperandType arch s)
  Literal :: Location arch tp -> Parameter arch sh tp

deriving instance Show (Location arch tp) => Show (Parameter arch sh tp)

instance ShowF (Location arch) => ShowF (Parameter arch sh) where
  showF (Operand repr idx) = unwords ["Operand", "(" ++ show repr ++ ")", show idx]
  showF (Literal var) = unwords ["Literal", showF var]

instance TestEquality (Location arch) => TestEquality (Parameter arch sh) where
  Operand _ idx1 `testEquality` Operand _ idx2 = (\Refl -> Refl) <$> testEquality idx1 idx2
  Literal   var1 `testEquality` Literal   var2 = (\Refl -> Refl) <$> testEquality var1 var2
  _              `testEquality`              _ = Nothing

instance Eq (Location arch tp) => Eq (Parameter arch sh tp) where
  Operand _ idx1 == Operand _ idx2 = isJust $ testEquality idx1 idx2
  Literal   var1 == Literal   var2 = var1 == var2
  _              ==              _ = False

instance OrdF (Location arch) => OrdF (Parameter arch sh) where
  Operand _ _ `compareF` Literal   _ = LTF
  Literal   _ `compareF` Operand _ _ = GTF
  Operand _ idx1 `compareF` Operand _ idx2 =
    case idx1 `compareF` idx2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF
  Literal var1 `compareF` Literal var2 =
    case var1 `compareF` var2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

paramType :: (Architecture arch) => Parameter arch sh tp -> BaseTypeRepr tp
paramType (Operand repr _) = repr
paramType (Literal loc) = locationType loc

-- | A "templated" or "parameterized" formula, i.e., a formula that has holes
-- that need to be filled in before it represents an actual concrete
-- instruction.
data ParameterizedFormula sym arch (sh :: [Symbol]) =
  ParameterizedFormula { pfUses :: Set.Set (Some (Parameter arch sh))
                       , pfOperandVars :: I.OperandList (BoundVar sym arch) sh
                       , pfLiteralVars :: MapF.MapF (Location arch) (S.BoundVar sym)
                       , pfDefs :: MapF.MapF (Parameter arch sh) (S.SymExpr sym)
                       }

-- | Copied from 'I', which defines the identical instance for
-- @dismantle@'s @Data.ShowF@ class.
instance (ShowF o) => ShowF (I.OperandList o) where
  showF l =
    case l of
      I.Nil -> "Nil"
      (elt I.:> rest) -> showF elt ++ " :> " ++ showF rest

instance ShowF o => Show (I.OperandList o sh) where
  show = showF

deriving instance (ShowF (Location arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym)) => Show (ParameterizedFormula sym arch sh)

instance (ShowF (Location arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym)) => ShowF (ParameterizedFormula sym arch) where
  showF = show

-- | A formula representing a concrete instruction.
data Formula sym arch =
  Formula { formUses :: Set.Set (Some (Location arch))
          , formParamVars :: MapF.MapF (Location arch) (S.BoundVar sym)
          , formDefs :: MapF.MapF (Location arch) (S.SymExpr sym)
          }
deriving instance (ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => Show (Formula sym arch)

emptyFormula :: Formula sym arch
emptyFormula = Formula { formUses = Set.empty, formParamVars = MapF.empty, formDefs = MapF.empty }

coerceFormula :: (Location arch1 ~ Location arch2) => Formula sym arch1 -> Formula sym arch2
coerceFormula f =
  Formula { formUses = formUses f
          , formParamVars = formParamVars f
          , formDefs = formDefs f
          }

-- replaceVarExpression :: sym -> Formula sym -> FormulaVar -> Some (S.SymExpr sym) -> IO (Formula sym)

newtype BaseSet sym arch = BaseSet { unBaseSet :: MapF.MapF ((Opcode arch) (Operand arch)) (ParameterizedFormula sym arch) }
deriving instance (ShowF (BoundVar sym arch),
                   ShowF (S.SymExpr sym),
                   ShowF (S.BoundVar sym),
                   ShowF (Location arch),
                   ShowF ((Opcode arch) (Operand arch)))
                => Show (BaseSet sym arch)

loadBaseSet :: sym -> FilePath -> IO (BaseSet sym arch)
loadBaseSet = undefined

data FormulaError opcode operand = NoFormulaForOpcode (I.SomeOpcode opcode operand)
  deriving (Show)

lookupSemantics :: (C.MonadThrow m, MonadIO m, Architecture arch)
                => sym
                -> BaseSet sym arch
                -> Instruction arch
                -> m (Formula sym arch)
lookupSemantics sym (BaseSet m) i =
  case i of
    I.Instruction op _ ->
      case MapF.lookup op m of
        Just f -> liftIO $ undefined sym f i
        -- Nothing -> C.throwM (NoFormulaForOpcode (I.SomeOpcode op))
        Nothing -> C.throwM (undefined :: C.SomeException)

sequenceFormulas :: a
sequenceFormulas = undefined

formulaFromProgram :: (C.MonadThrow m, MonadIO m, Architecture arch)
                   => sym
                   -- ^ The SymInterface used to build expressions
                   -> BaseSet sym arch
                   -- ^ The formulas for instructions with known semantics
                   -> [Instruction arch]
                   -- ^ The program to create a formula for
                   -> m (Formula sym arch)
formulaFromProgram sym base p = do
  sems <- mapM (lookupSemantics sym base) p
  liftIO $ foldM (sequenceFormulas sym) emptyFormula sems
