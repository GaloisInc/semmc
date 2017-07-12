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
  ) where

import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )
import           Text.Printf ( printf )

import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.BaseTypes

import qualified Dismantle.Instruction as I

import           SemMC.Architecture

data Parameter arch (sh :: [Symbol]) (tp :: BaseType) where
  Operand :: BaseTypeRepr (OperandType arch s) -> I.Index sh s -> Parameter arch sh (OperandType arch s)
  Literal :: Location arch tp -> Parameter arch sh tp

instance ShowF (Location arch) => Show (Parameter arch sh tp) where
  show (Operand repr idx) = printf "Operand (%s) (%s)" (show repr) (show idx)
  show (Literal var) = unwords ["Literal", showF var]

instance (ShowF (Location arch)) => ShowF (Parameter arch sh)

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

deriving instance (ShowF (Location arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym)) => Show (ParameterizedFormula sym arch sh)

instance (ShowF (Location arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym)) => ShowF (ParameterizedFormula sym arch)

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
