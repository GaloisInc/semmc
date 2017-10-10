{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions of formulas
module SemMC.Formula.Formula
  ( -- * Parameter
    Parameter(..)
  , paramType
    -- * ParameterizedFormula
  , ParameterizedFormula(..)
    -- * Formula
  , Formula(..)
  , formInputs
  , formOutputs
  , validFormula
  , emptyFormula
  , coerceFormula
  ) where

import           GHC.TypeLits ( Symbol )

import           Control.Monad ( guard )
import qualified Data.Set as Set
import           Text.Printf ( printf )

import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..), viewSome )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.ShapedList as SL
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import           Lang.Crucible.BaseTypes

import qualified SemMC.Architecture as A
import qualified SemMC.Util as U

-- | A parameter for use in the 'ParameterizedFormula' below.
data Parameter arch (sh :: [Symbol]) (tp :: BaseType) where
  -- | A parameter that will be filled in at instantiation time. For example, if
  -- you have the x86 opcode @call r32@, an 'Operand' would be used to represent
  -- the @r32@ hole. It could also represent an immediate.
  Operand :: BaseTypeRepr (A.OperandType arch s) -> SL.Index sh s -> Parameter arch sh (A.OperandType arch s)
  -- | A parameter that always represents a particular machine location. For
  -- example, if you have the x86 opcode @call r32@, a 'Literal' would be used
  -- to represent the implicit @esp@ register used.
  Literal :: A.Location arch tp -> Parameter arch sh tp
  -- | A function from one location to another.  The string specifies the name
  -- of the function, which is interpreted on a per-architecture basis.
  --
  -- This is intended to be evaluated at formula instantiation time and is
  -- designed to accommodate cases like the PowerPC @memri@ operand type, which
  -- actually stands for two different operands bundled together: a base and an
  -- offset.  In some instructions, we have to define a part of the @memri@ (the
  -- base register) as part of the semantics of an instruction, so we need a way
  -- to refer to part of the parameter.
  Function :: String
           -> BaseTypeRepr tp'
           -> Parameter arch sh tp'
           -> BaseTypeRepr tp
           -> Parameter arch sh tp

instance ShowF (A.Location arch) => Show (Parameter arch sh tp) where
  show (Operand repr idx) = printf "Operand (%s) (%s)" (show repr) (show idx)
  show (Literal var) = unwords ["Literal", showF var]
  show (Function fnName _ p _) = printf "%s(%s)" fnName (show p)

instance (ShowF (A.Location arch)) => ShowF (Parameter arch sh)

instance TestEquality (A.Location arch) => TestEquality (Parameter arch sh) where
  Operand _ idx1 `testEquality` Operand _ idx2 = (\Refl -> Refl) <$> testEquality idx1 idx2
  Literal   var1 `testEquality` Literal   var2 = (\Refl -> Refl) <$> testEquality var1 var2
  -- Function r1 fname1 p1 `testEquality` Function r2 fname2 p2 = do
  --   guard (fname1 == fname2)
  --   Refl <- testEquality p1 p2
  --   testEquality r1 r2
--    return Refl
  _              `testEquality`              _ = Nothing

instance (Eq (A.Location arch tp), TestEquality (A.Location arch)) => Eq (Parameter arch sh tp) where
  Operand _ idx1 == Operand _ idx2 = isJust $ testEquality idx1 idx2
  Literal   var1 == Literal   var2 = var1 == var2
  -- This isn't quite true - they could be equal after normalization
  -- Function fnName1 p1 == Function fnName2 p2 =
  --   fnName1 == fnName2 && isJust (testEquality p1 p2)
  _              ==              _ = False

instance OrdF (A.Location arch) => OrdF (Parameter arch sh) where
  Function {} `compareF` Literal {} = LTF
  Literal {} `compareF` Function {} = GTF
  Function  {}`compareF` Operand {} = LTF
  Operand {} `compareF` Function {} = GTF
  Operand _ _ `compareF` Literal   _ = LTF
  Literal   _ `compareF` Operand _ _ = GTF
  -- Function fnName1 p1 `compareF` Function fnName2 p2 =
  --   case fnName1 `compare` fnName2 of
  --     LT -> LTF
  --     GT -> GTF
  --     EQ ->
  --       case p1 `compareF` p2 of
  --         LTF -> LTF
  --         GTF -> GTF
  --         EQF -> EQF
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

-- | Get a representation of the 'BaseType' this formula parameter is
-- type-parameterized over.
paramType :: (A.Architecture arch) => Parameter arch sh tp -> BaseTypeRepr tp
paramType (Operand repr _) = repr
paramType (Literal loc) = A.locationType loc
paramType (Function _ _ _ repr) = repr

-- | A "parameterized" formula, i.e., a formula that has holes for operands that
-- need to be filled in before it represents an actual concrete instruction.
data ParameterizedFormula sym arch (sh :: [Symbol]) =
  ParameterizedFormula { pfUses :: Set.Set (Some (Parameter arch sh))
                       -- ^ All parameters *used* within the definitions. One
                       -- might ask, "aren't all parameters used, lest they be
                       -- useless?" No -- some parameters may be only used as
                       -- outputs (locations being defined).
                       , pfOperandVars :: SL.ShapedList (A.BoundVar sym arch) sh
                       -- ^ Bound variables for each of the operands; used in
                       -- the expressions of the definitions.
                       , pfLiteralVars :: MapF.MapF (A.Location arch) (S.BoundVar sym)
                       -- ^ Bound variables for each of the locations; used in
                       -- the expressions of the definitions.
                       , pfDefs :: MapF.MapF (Parameter arch sh) (S.SymExpr sym)
                       -- ^ Expressions for each of the output parameters of
                       -- this formula. Note that a 'Parameter' could possibly
                       -- be an operand that does not represent a location, but
                       -- instead an immediate; however, that shouldn't show up
                       -- in here!
                       }

deriving instance (ShowF (A.Location arch),
                   ShowF (S.SymExpr sym),
                   ShowF (S.BoundVar sym))
                  => Show (ParameterizedFormula sym arch sh)

instance (ShowF (A.Location arch),
          ShowF (S.SymExpr sym),
          ShowF (S.BoundVar sym))
         => ShowF (ParameterizedFormula sym arch)

-- | A formula representing a concrete instruction.
--
-- The structure of this is a little odd due to there being no perfect ways of
-- representing ASTs that depend on variables. 'formParamVars' is a mapping of
-- all locations *used* in the formula to "bound variables" -- basically
-- variables that must be replaced before SMT checking or it'll fail.
-- 'formDefs', then, is a mapping of all locations *defined* by the formula to
-- expressions that use the aforementioned bound variables.
--
-- Invariants:
-- 1. The set of bound variables used in 'formDefs' should be exactly the set of
--    bound variables in 'formParamVars'. This is checkable using the
--    'validFormula' function.
-- 2. All bound variables in 'formParamVars' should not appear in any other
--    formula. Yes, this breaks the notion of referential transparency in many
--    ways, but it was the least bad solution.
-- 3. 'formParamVars' should be a one-to-one correspondence, in the sense that a
--    bound variable should uniquely identify a location. In combination with
--    (1), this means that only locations actually used in the definitions
--    should be present as keys in 'formParamVars'.
data Formula sym arch =
  Formula { formParamVars :: MapF.MapF (A.Location arch) (S.BoundVar sym)
          , formDefs :: MapF.MapF (A.Location arch) (S.SymExpr sym)
          }
deriving instance (ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (A.Location arch)) => Show (Formula sym arch)

-- | Get the locations used by a formula.
formInputs :: (OrdF (A.Location arch)) => Formula sym arch -> Set.Set (Some (A.Location arch))
formInputs = Set.fromList . MapF.keys . formParamVars

-- | Get the locations modified by a formula.
formOutputs :: (OrdF (A.Location arch)) => Formula sym arch -> Set.Set (Some (A.Location arch))
formOutputs = Set.fromList . MapF.keys . formDefs

-- | Check if a given 'Formula' obeys the stated invariant.
validFormula :: Formula (S.SimpleBuilder t st) arch -> Bool
validFormula (Formula { formParamVars = paramVars, formDefs = defs }) =
  mconcat (map (viewSome U.allBoundVars) (MapF.elems defs))
  == Set.fromAscList (MapF.elems paramVars)

-- | A formula that uses no variables and changes no variables.
emptyFormula :: Formula sym arch
emptyFormula = Formula { formParamVars = MapF.empty, formDefs = MapF.empty }

-- | Turn a formula from one architecture into that of another, assuming the
-- location types of the architectures are the same.
coerceFormula :: (A.Location arch1 ~ A.Location arch2) => Formula sym arch1 -> Formula sym arch2
coerceFormula f =
  Formula { formParamVars = formParamVars f
          , formDefs = formDefs f
          }
