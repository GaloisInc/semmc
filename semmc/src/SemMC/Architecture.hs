{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture (
  Architecture(..),
  AllocatedOperand(..),
  Location,
  IsLocation(..),
  Evaluator(..),
  FunctionInterpretation(..),
  Instruction,
  Operand,
  IsOperand,
  Opcode,
  IsOpcode,
  OperandType,
  IsOperandTypeRepr(..),
  ArchRepr,
  ShapeRepr,
  showShapeRepr,
  createSymbolicEntries
  ) where

import           Data.EnumF
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.HasRepr as HR
import           Data.Proxy ( Proxy(..) )
import           Data.Typeable ( Typeable )
import           GHC.TypeLits ( Symbol )
import qualified Language.Haskell.TH as TH

import           What4.BaseTypes
import qualified What4.Interface as S
import qualified What4.Expr as S

import           SemMC.Architecture.Internal
import           SemMC.Architecture.Location
import           SemMC.Formula.Formula ( LocationFuncInterp, ParameterizedFormula )

type Sym t st = S.ExprBuilder t st

type ShapeRepr arch = SL.List (OperandTypeRepr arch)

type ArchRepr arch = (HR.HasRepr (Opcode arch (Operand arch)) (ShapeRepr arch))

-- | This is a deconstructed version of an operand that contains symbolic
-- expressions ('S.SymExpr') for each constituent component of the operand.  It
-- is intended to be the payload of the 'TaggedExpr' for each instance of the
-- 'Architecture'.
data AllocatedOperand arch sym (s :: Symbol) where
  -- | A simple operand that represents an immediate value
  ValueOperand :: (OperandType arch s ~ BaseBVType n)
               => S.SymExpr sym (OperandType arch s)
               -> AllocatedOperand arch sym s
  -- | A value representing an operand backed by a Location
  --
  -- FIXME: Can we add a constraint indicating that this really is a bitvector type?
  LocationOperand :: (OperandType arch s ~ BaseBVType n)
                  => Location arch (OperandType arch s)
                  -> S.SymExpr sym (OperandType arch s)
                  -> AllocatedOperand arch sym s
  -- | A compound operand with an arch-specific representation
  CompoundOperand :: OperandComponents arch sym s -> AllocatedOperand arch sym s

instance (S.IsExprBuilder sym, IsLocation (Location arch), ShowF (OperandComponents arch sym)) => Show (AllocatedOperand arch sym s) where
  show ao =
    case ao of
      ValueOperand s -> "ValueOperand " ++ show (S.printSymExpr s)
      LocationOperand l s -> "LocationOperand " ++ showF l ++ " " ++ show (S.printSymExpr s)
      CompoundOperand oc -> "CompoundOperand " ++ showF oc

-- | An architecture is the top-level interface for specifying a semantics
-- implementation. It has specific operands, opcodes, and state variables.
class (IsOperand (Operand arch),
       IsOpcode (Opcode arch),
       IsLocation (Location arch),
       IsOperandTypeRepr arch,
       Show (Instruction arch),
       ShowF (Operand arch),
       Typeable arch,
       OrdF (OperandTypeRepr arch),
       ShowF (Operand arch),
       OrdF (Opcode arch (Operand arch)),
       ShowF (Opcode arch (Operand arch)),
       EnumF (Opcode arch (Operand arch)))
      => Architecture arch where
  -- | Tagged expression type for this architecture.
  --
  -- This is a bit of a hack to add extra metadata needed for the templating stuff.
  data TaggedExpr arch sym :: Symbol -> *

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
  type OperandComponents arch sym :: Symbol -> *

  -- | Untag a tagged expression.
  unTagged :: TaggedExpr arch sym s -> Maybe (S.SymExpr sym (OperandType arch s))

  -- | Extract the 'AllocatedOperand' from a 'TaggedExpr'
  taggedOperand :: TaggedExpr arch sym s -> AllocatedOperand arch sym s

  -- | The uninterpreted functions referred to by this architecture
  uninterpretedFunctions :: proxy arch -> [(String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)]

  -- | Map an operand to a Crucible expression, given a mapping from each state
  -- variable to a Crucible variable.
  --
  -- This is used during formula instantiation to find a symbolic expression for
  -- each operand.
  operandValue :: forall proxy sym s.
                  (S.IsSymExprBuilder sym,
                   S.IsExprBuilder sym)
               => proxy arch
               -> sym
               -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
               -> Operand arch s
               -> IO (TaggedExpr arch sym s)

  -- |
  --
  -- FIXME: Add a way to allocate (sharable) exprs for non-locations
  allocateSymExprsForOperand :: forall proxy sym s
                              . (S.IsSymExprBuilder sym, S.IsExprBuilder sym)
                             => proxy arch
                             -> sym
                             -> (forall tp . Location arch tp -> IO (S.SymExpr sym tp))
                             -- ^ A function to return the allocate a
                             -- 'S.SymExpr' for a Location, which can (and
                             -- should) return a previously allocated version
                             -- (if there was one).
                             -> Operand arch s
                             -> IO (TaggedExpr arch sym s)

  -- | Map an operand to a specific state variable, if possible.
  operandToLocation :: forall proxy s.
                       proxy arch
                    -> Operand arch s
                    -> Maybe (Location arch (OperandType arch s))

  -- | Functions used to simplify defined locations in parameterized formulas
  -- that are defined as functions of an input parameter into a concrete
  -- location
  locationFuncInterpretation :: proxy arch -> [(String, FunctionInterpretation t st arch)]

  shapeReprToTypeRepr :: proxy arch -> OperandTypeRepr arch s -> BaseTypeRepr (OperandType arch s)


showShapeRepr :: forall arch sh. (IsOperandTypeRepr arch) => Proxy arch -> ShapeRepr arch sh -> String
showShapeRepr _ rep =
    case rep of
      SL.Nil -> ""
      (r SL.:< rep') -> let showr = operandTypeReprSymbol (Proxy @arch) r
                       in showr  ++ " " ++ (showShapeRepr (Proxy @arch) rep')

-- | This type encapsulates an evaluator for operations represented as
-- uninterpreted functions in semantics.  It may seem strange to interpret
-- "uninterpreted functions" (UFs); we use UFs to represent operations in the
-- semantics that can't be expressed using more typical SMT operations.  The most
-- common examples in the codebase are:
--
-- 1) Extracting sub-components from compound operands in instructions (like a
--    literal bundled with a shift amount)
-- 2) Testing the number of a register (e.g., testing if a register is r0)
--
-- While the type isn't much of an abstraction barrier, it is convenient to hide
-- the forall under a data constructor rather than a type alias.
--
-- * The 'Sym' is a symbolic expression builder from the what4 library
-- * The 'ParameterizedFormula' is the formula whose semantics we are currently evaluating
-- * The 'SL.List' contains the concrete operands to the instruction whose semantics we are evaluating
-- * The 'Ctx.Assignment' is the list of operands of the uninterpreted function being interpreted
-- * The 'BaseTypeRepr' is the expected return type of the uninterpreted function
--
-- Note that the type parameters for the *instruction* operand list and the
-- *uninterpreted function* operand list (@sh@ and @u@, respectively) explicitly
-- do /not/ match up, as the UF and instructions take different operands.
--
-- We need to pass the return type 'BaseTypeRepr' in so that we can know at the
-- call site that the expression produced by the evaluator is correctly-typed.
data Evaluator arch t st =
  Evaluator (forall tp u sh
               . Sym t st
              -> ParameterizedFormula (Sym t st) arch sh
              -> SL.List (AllocatedOperand arch (Sym t st)) sh
              -> Ctx.Assignment (S.Expr t) u
              -> (forall ltp . Location arch ltp -> IO (S.Expr t ltp))
              -> BaseTypeRepr tp
              -> IO (S.Expr t tp))

data FunctionInterpretation t st arch =
  FunctionInterpretation { locationInterp :: LocationFuncInterp arch
                         -- ^ The function interpretation to apply to functions
                         -- appearing in location definition contexts (i.e., the
                         -- 'F.Parameter' function type).
                         , exprInterpName :: TH.Name
                         -- ^ The (template haskell) 'TH.Name' of the function
                         -- to apply statically during formula translation (at
                         -- the value level) to eliminate an uninterpreted
                         -- function appearing in a semantics expression.
                         , exprInterp :: Evaluator arch t st
                         -- ^ The evaluator to apply to uninterpreted functions
                         -- during formula instantiation (in Formula.Instantiate)
                         }

-- | Uninterpreted function names are mangled in SimpleBuilder, so we need to
-- create extra entries to match their mangled names.
--
-- In particular, periods in names are converted to underscores.
--
-- This function creates copies of entries with periods in their names with the
-- escaped version as it appears in a SimpleBuilder symbolic function.  For
-- example, if there is an entry with the name @arm.foo@, this function retains
-- that entry in the input list and adds an additional entry under @arm_foo@.
--
-- We also prepend a "uf_"/"uf." prefix to the names of the function to
-- distinguish it from defined functions.
createSymbolicEntries :: [(String, a)] -> [(String, a)]
createSymbolicEntries = foldr duplicateIfDotted []
  where
    duplicateIfDotted (s, e) acc =
      case '.' `elem` s of
        False -> acc
        True ->
          let newElt = ("uf_" ++ map (\c -> if c == '.' then '_' else c) s, e)
          in newElt : ("uf." ++ s, e) : acc

