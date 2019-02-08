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
  , WrappedOperand(..)
  , LocationFuncInterp(..)
    -- * ParameterizedFormula
  , ParameterizedFormula(..)
    -- * Formula
  , Formula(..)
  , formInputs
  , formOutputs
  , validFormula
  , emptyFormula
  , coerceFormula
  , formStripIP
    -- * Functions and libraries
  , FunctionFormula(..)
  , FunctionRef(..)
  , functionRef
  , Library
  , emptyLibrary
  ) where

import           GHC.TypeLits ( Symbol )

import           Control.Monad ( guard )
import qualified Data.Set as Set
import           Text.Printf ( printf )

import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..), viewSome )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as SL
import qualified What4.Interface as S
import qualified What4.Expr as S
import           What4.BaseTypes

import           Data.Type.List ( ToContextFwd )
import qualified SemMC.Architecture.AllocatedOperand as AO
import qualified SemMC.Architecture.Internal as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.BoundVar as BV
import qualified SemMC.Util as U

-- | A parameter for use in the 'ParameterizedFormula' below.
data Parameter arch (sh :: [Symbol]) (tp :: BaseType) where
  -- | A parameter that will be filled in at instantiation time. For example, if
  -- you have the x86 opcode @call r32@, an 'Operand' would be used to represent
  -- the @r32@ hole. It could also represent an immediate.
  OperandParameter :: BaseTypeRepr (A.OperandType arch s) -> SL.Index sh s -> Parameter arch sh (A.OperandType arch s)
  -- | A parameter that always represents a particular machine location. For
  -- example, if you have the x86 opcode @call r32@, a 'LiteralParameter' would be used
  -- to represent the implicit @esp@ register used.
  LiteralParameter :: L.Location arch tp -> Parameter arch sh tp
  -- | A function from one location to another.  The string specifies the name
  -- of the function, which is interpreted on a per-architecture basis.
  --
  -- This is intended to be evaluated at formula instantiation time and is
  -- designed to accommodate cases like the PowerPC @memri@ operand type, which
  -- actually stands for two different operands bundled together: a base and an
  -- offset.  In some instructions, we have to define a part of the @memri@ (the
  -- base register) as part of the semantics of an instruction, so we need a way
  -- to refer to part of the parameter.
  FunctionParameter :: String
                    -- The name of the uninterpreted function
                    -> WrappedOperand arch sh s
                    -- The operand we are calling the function on (this is a newtype so
                    -- we don't need an extra typerepr)
                    -> BaseTypeRepr tp
                    -- The typerepr for the return type of the function
                    -> Parameter arch sh tp

-- | This is a wrapper around the contents of an 'Operand' parameter that is
-- embedded in a 'FunctionParameter' parameter.  Functions are only allowed to be called
-- on Operands.  The new type here helps avoid an extra 'BaseTypeRepr' in
-- 'FunctionParameter'.
data WrappedOperand arch sh s where
  WrappedOperand :: BaseTypeRepr (A.OperandType arch s) -> SL.Index sh s -> WrappedOperand arch sh s

-- | A wrapper around a function that can be called to simplify a 'FunctionParameter'
-- parameter into a 'L.Location'.  These are defined per-architecture and are
-- invoked by 'paramToLocation' during formula instantiation.
data LocationFuncInterp t st fs arch where
  LocationFuncInterp :: ( forall sh s tp . SL.List (AO.AllocatedOperand arch (S.ExprBuilder t st fs)) sh -> WrappedOperand arch sh s -> BaseTypeRepr tp -> Maybe (L.Location arch tp))
                     -> LocationFuncInterp t st fs arch

instance ShowF (L.Location arch) => Show (Parameter arch sh tp) where
  show (OperandParameter repr idx) = printf "OperandParameter (%s) (%s)" (show repr) (show idx)
  show (LiteralParameter var) = unwords ["LiteralParameter", showF var]
  show (FunctionParameter fnName (WrappedOperand rep ix) _) =
    printf "%s(operand %s@%s)" fnName (show rep) (show ix)

instance (ShowF (L.Location arch)) => ShowF (Parameter arch sh)

instance TestEquality (WrappedOperand arch sh) where
  WrappedOperand r1 ix1 `testEquality` WrappedOperand r2 ix2 = do
    Refl <- testEquality r1 r2
    Refl <- testEquality ix1 ix2
    return Refl

instance OrdF (WrappedOperand arch sh) where
  compareF (WrappedOperand r1 ix1) (WrappedOperand r2 ix2) =
    case compareF r1 r2 of
      LTF -> LTF
      GTF -> GTF
      EQF -> case compareF ix1 ix2 of
        LTF -> LTF
        GTF -> GTF
        EQF -> EQF

instance TestEquality (L.Location arch) => TestEquality (Parameter arch sh) where
  OperandParameter _ idx1 `testEquality` OperandParameter _ idx2 = (\Refl -> Refl) <$> testEquality idx1 idx2
  LiteralParameter   var1 `testEquality` LiteralParameter   var2 = (\Refl -> Refl) <$> testEquality var1 var2
  FunctionParameter fname1 wo1 r1 `testEquality` FunctionParameter fname2 wo2 r2 = do
    guard (fname1 == fname2)
    Refl <- testEquality wo1 wo2
    Refl <- testEquality r1 r2
    return Refl
  _              `testEquality`              _ = Nothing

instance (Eq (L.Location arch tp), TestEquality (L.Location arch)) => Eq (Parameter arch sh tp) where
  OperandParameter _ idx1 == OperandParameter _ idx2 = isJust $ testEquality idx1 idx2
  LiteralParameter   var1 == LiteralParameter   var2 = var1 == var2
  -- NOTE: This isn't quite true - they could be equal after normalization.  Be careful...
  --
  -- It isn't clear when this might be used in practice, and if such a
  -- difference will matter.  Given that we can't evaluate the function until we
  -- instantiate a formula, it is only the case that a function parameter /MAY/
  -- equal another parameter after normalization.
  f1@(FunctionParameter {}) == f2@(FunctionParameter {}) =
    isJust (testEquality f1 f2)
  _              ==              _ = False

instance OrdF (L.Location arch) => OrdF (Parameter arch sh) where
  FunctionParameter {} `compareF` LiteralParameter {} = LTF
  LiteralParameter {} `compareF` FunctionParameter {} = GTF
  FunctionParameter  {}`compareF` OperandParameter {} = LTF
  OperandParameter {} `compareF` FunctionParameter {} = GTF
  OperandParameter _ _ `compareF` LiteralParameter   _ = LTF
  LiteralParameter   _ `compareF` OperandParameter _ _ = GTF
  FunctionParameter fnName1 wo1 r1 `compareF` FunctionParameter fnName2 wo2 r2 =
    case fnName1 `compare` fnName2 of
      LT -> LTF
      GT -> GTF
      EQ ->
        case wo1 `compareF` wo2 of
          LTF -> LTF
          GTF -> GTF
          EQF -> case r1 `compareF` r2 of
                   LTF -> LTF
                   GTF -> GTF
                   EQF -> EQF

  OperandParameter _ idx1 `compareF` OperandParameter _ idx2 =
    case idx1 `compareF` idx2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF
  LiteralParameter var1 `compareF` LiteralParameter var2 =
    case var1 `compareF` var2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

-- | Get a representation of the 'BaseType' this formula parameter is
-- type-parameterized over.
paramType :: (L.IsLocation (L.Location arch)) => Parameter arch sh tp -> BaseTypeRepr tp
paramType (OperandParameter repr _) = repr
paramType (LiteralParameter loc) = L.locationType loc
paramType (FunctionParameter _ _ repr) = repr

-- | A "parameterized" formula, i.e., a formula that has holes for operands that
-- need to be filled in before it represents an actual concrete instruction.
data ParameterizedFormula sym arch (sh :: [Symbol]) =
  ParameterizedFormula { pfUses :: Set.Set (Some (Parameter arch sh))
                       -- ^ All parameters *used* within the definitions. One
                       -- might ask, "aren't all parameters used, lest they be
                       -- useless?" No -- some parameters may be only used as
                       -- outputs (locations being defined).
                       , pfOperandVars :: SL.List (BV.BoundVar sym arch) sh
                       -- ^ Bound variables for each of the operands; used in
                       -- the expressions of the definitions.
                       , pfLiteralVars :: MapF.MapF (L.Location arch) (S.BoundVar sym)
                       -- ^ Bound variables for each of the locations; used in
                       -- the expressions of the definitions.
                       , pfDefs :: MapF.MapF (Parameter arch sh) (S.SymExpr sym)
                       -- ^ Expressions for each of the output parameters of
                       -- this formula. Note that a 'Parameter' could possibly
                       -- be an operand that does not represent a location, but
                       -- instead an immediate; however, that shouldn't show up
                       -- in here!
                       }

deriving instance (ShowF (L.Location arch),
                   ShowF (S.SymExpr sym),
                   ShowF (S.BoundVar sym))
                  => Show (ParameterizedFormula sym arch sh)

instance (ShowF (L.Location arch),
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
  Formula { formParamVars :: MapF.MapF (L.Location arch) (S.BoundVar sym)
          , formDefs :: MapF.MapF (L.Location arch) (S.SymExpr sym)
          }
deriving instance (ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (L.Location arch)) => Show (Formula sym arch)

-- | Get the locations used by a formula.
formInputs :: (OrdF (L.Location arch)) => Formula sym arch -> Set.Set (Some (L.Location arch))
formInputs = Set.fromList . MapF.keys . formParamVars

-- | Get the locations modified by a formula.
formOutputs :: (OrdF (L.Location arch)) => Formula sym arch -> Set.Set (Some (L.Location arch))
formOutputs = Set.fromList . MapF.keys . formDefs

-- | Check if a given 'Formula' obeys the stated invariant.
validFormula :: Formula (S.ExprBuilder t st fs) arch -> Bool
validFormula (Formula { formParamVars = paramVars, formDefs = defs }) =
  mconcat (map (viewSome U.allBoundVars) (MapF.elems defs))
  == Set.fromAscList (MapF.elems paramVars)

-- | A formula that uses no variables and changes no variables.
emptyFormula :: Formula sym arch
emptyFormula = Formula { formParamVars = MapF.empty, formDefs = MapF.empty }

-- | Turn a formula from one architecture into that of another, assuming the
-- location types of the architectures are the same.
coerceFormula :: (L.Location arch1 ~ L.Location arch2) => Formula sym arch1 -> Formula sym arch2
coerceFormula f =
  Formula { formParamVars = formParamVars f
          , formDefs = formDefs f
          }

-- | Remove the instruction pointer location from the definitions in a formula
--
-- Note that this could break the property that `formParamVars` contains exactly
-- the bound variables in a formula
formStripIP :: L.IsLocation (L.Location arch)
            => Formula sym arch -> Formula sym arch
formStripIP (Formula vars defs) = Formula (go vars) (go defs)
  where
    go = MapF.filterWithKey (\l _ -> not (L.isIP l))

-- | A formula representing a defined function.
data FunctionFormula sym (sig :: ([BaseType], BaseType)) where
  FunctionFormula :: { ffName :: String
                     , ffArgTypes :: SL.List BaseTypeRepr tps
                     , ffArgVars :: SL.List (S.BoundVar sym) tps
                     , ffRetType :: BaseTypeRepr tp
                     , ffDef :: S.SymFn sym (ToContextFwd tps) tp
                     } -> FunctionFormula sym '(tps, tp)

data FunctionRef (sig :: ([BaseType], BaseType)) where
  FunctionRef :: { frName :: String
                 , frArgTypes :: SL.List BaseTypeRepr tps
                 , frRetType :: BaseTypeRepr tp
                 } -> FunctionRef '(tps, tp)

deriving instance Show (FunctionRef sig)

instance Eq (FunctionRef sig) where
  FunctionRef n1 ats1 rt1 == FunctionRef n2 ats2 rt2 =
    n1 == n2 &&
    isJust (testEquality ats1 ats2) &&
    isJust (testEquality rt1 rt2)

instance TestEquality FunctionRef where
  testEquality (FunctionRef n1 ats1 rt1) (FunctionRef n2 ats2 rt2) = do
    guard (n1 == n2)
    Refl <- testEquality ats1 ats2
    Refl <- testEquality rt1 rt2
    return Refl

instance OrdF FunctionRef where
  FunctionRef n1 ats1 rt1 `compareF` FunctionRef n2 ats2 rt2 =
    case n1 `compare` n2 of
      EQ -> case ats1 `compareF` ats2 of
        EQF -> case rt1 `compareF` rt2 of
          EQF -> EQF; LTF -> LTF; GTF -> GTF
        LTF -> LTF; GTF -> GTF
      LT -> LTF; GT -> GTF

functionRef :: FunctionFormula sym sig -> FunctionRef sig
functionRef (FunctionFormula { ffName = name
                             , ffArgTypes = argTypes
                             , ffRetType = retType }) =
  FunctionRef { frName = name
              , frArgTypes = argTypes
              , frRetType = retType }

type Library sym = MapF.MapF FunctionRef (FunctionFormula sym)

emptyLibrary :: Library sym
emptyLibrary = MapF.empty
