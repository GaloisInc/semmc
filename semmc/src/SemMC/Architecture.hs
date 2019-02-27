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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}

module SemMC.Architecture (
  Architecture(..),
  AllocatedOperand(..),
  OperandComponents,
  Location,
  IsLocation(..),
  allLocations,
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
  RegWidth,
  ShapeRepr,
  UninterpFn(..),
  mkUninterpFn,
  getUninterpFn,
  AccessData(..),
  LLVM.EndianForm(..),
  MemType,
  memTypeRepr,
  accessAddr,
  showShapeRepr,
  createSymbolicEntries,
  createSymbolicName,
  regWidth,
  taggedExprImmediate
  ) where

import           Data.EnumF
import           Data.Kind
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.HasRepr as HR
import           Data.Proxy ( Proxy(..) )
import           Data.Typeable ( Typeable )
import           GHC.TypeLits ( Symbol, Nat, KnownNat )
import qualified Language.Haskell.TH as TH

import           What4.BaseTypes
import qualified What4.Interface as S
import qualified What4.Expr as S

import qualified Lang.Crucible.LLVM.DataLayout as LLVM

import           SemMC.Architecture.AllocatedOperand
import           SemMC.Architecture.Internal
import           SemMC.Architecture.Location
import           SemMC.Formula.Formula ( LocationFuncInterp, ParameterizedFormula )

type Sym t st fs = S.ExprBuilder t st fs

type ShapeRepr arch = SL.List (OperandTypeRepr arch)

type ArchRepr arch = (HR.HasRepr (Opcode arch (Operand arch)) (ShapeRepr arch))

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
       EnumF (Opcode arch (Operand arch)),
       HasRegWidth arch)
      => Architecture arch where

  -- | Tagged expression type for this architecture.
  --
  -- This wrapper allows the templated architecture to store some extra metadata
  -- needed for re-constructing concrete instructions from templates (based on
  -- models returned from the SMT solver).
  --
  -- For all non-templated architectures, this should probably just be @'AllocatedOperand' arch sym@
  data TaggedExpr arch sym :: Symbol -> Type

  -- | Project a 'S.SymExpr' from the 'TaggedExpr' (which is basically an 'AllocatedOperand') if possible
  --
  -- This is possible for simple cases where the 'AllocatedOperand' is a simple
  -- value or location (but not possible if the operand is "compound" (i.e., it
  -- has more than one logical component, like a memory reference that is a base
  -- register plus an offset).
  unTagged :: TaggedExpr arch sym s -> Maybe (S.SymExpr sym (OperandType arch s))

  -- | Extract the 'AllocatedOperand' from a 'TaggedExpr'
  taggedOperand :: TaggedExpr arch sym s -> AllocatedOperand arch sym s

  -- | Extract expressions corresponding to the symbolic offset/immediate value of an OperandComponent
  operandComponentsImmediate :: proxy sym -> OperandComponents arch sym s -> Maybe (Some (S.SymExpr sym))

  -- | The uninterpreted functions referred to by this architecture. These
  -- should include readMemUF and writeMemUF
  uninterpretedFunctions :: proxy arch -> [UninterpFn arch]
  readMemUF :: Integer -- ^ Number of bits to read, undefined if not in 'uninterpretedFunctions'
            -> String
  writeMemUF :: Integer -- ^ Number of bits to read, undefined if not in 'uninterpretedFunctions'
             -> String

  -- | Map an operand to a Crucible expression, given a mapping from each state
  -- variable to a Crucible variable.
  --
  -- This is used during formula instantiation to find a symbolic expression for
  -- each operand.
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
  locationFuncInterpretation :: proxy arch -> [(String, FunctionInterpretation t st fs arch)]


  -- | Whether the architecture writes data in big-endian or little-endian form, by default
  archEndianForm :: proxy arch -> LLVM.EndianForm

  shapeReprToTypeRepr :: proxy arch -> OperandTypeRepr arch s -> BaseTypeRepr (OperandType arch s)


regWidth :: Architecture arch => NatRepr (RegWidth arch)
regWidth = knownNat

showShapeRepr :: forall arch sh. (IsOperandTypeRepr arch) => Proxy arch -> ShapeRepr arch sh -> String
showShapeRepr _ rep =
    case rep of
      SL.Nil -> ""
      (r SL.:< rep') -> let showr = operandTypeReprSymbol (Proxy @arch) r
                       in showr  ++ " " ++ (showShapeRepr (Proxy @arch) rep')

type family RegWidth arch :: Nat

type HasRegWidth arch = (1 <= RegWidth arch, 16 <= RegWidth arch, KnownNat (RegWidth arch))

data UninterpFn arch where
  MkUninterpFn :: forall arch args ty.
                  { uninterpFnName :: String
                  , uninterpFnArgs :: Ctx.Assignment BaseTypeRepr args
                  , uninterpFnRes  :: BaseTypeRepr ty
                  , uninterpFnLive :: forall sym.
                                      Ctx.Assignment (S.SymExpr sym) args 
                                   -> [AccessData sym arch]
                  -- ^ Given some arguments, identify the arguments that might touch memory.
                  } -> UninterpFn arch
instance Show (UninterpFn arch) where
  show (MkUninterpFn name args res _) = name ++ " [ " ++ show args ++ " => " ++ show res ++ " ]"


mkUninterpFn :: forall (args :: Ctx.Ctx BaseType) (ty :: BaseType) arch.
              ( KnownRepr (Ctx.Assignment BaseTypeRepr) args
              , KnownRepr BaseTypeRepr ty)
             => String 
             -> (forall sym. Ctx.Assignment (S.SymExpr sym) args 
                          -> [AccessData sym arch])
             -> UninterpFn arch
mkUninterpFn name liveness = MkUninterpFn name (knownRepr :: Ctx.Assignment BaseTypeRepr args) 
                                                      (knownRepr :: BaseTypeRepr ty)
                                                      liveness

-- | Get the representation of the uninterpreted function with the name corresponding to the given string out of the list `uninterpretedfunctions'
getUninterpFn :: forall arch.
                 Architecture arch
              => String
              -> Maybe (UninterpFn arch)
getUninterpFn s = go $ uninterpretedFunctions (Proxy @arch)
  where
    go :: [UninterpFn arch] -> Maybe (UninterpFn arch)
    go [] = Nothing
    go (f@(MkUninterpFn _ _ _ _) : fs) = if s == createSymbolicName (uninterpFnName f)
                                              then Just f
                                              else go fs


data AccessData sym arch where
  ReadData  :: S.SymExpr sym (S.BaseBVType (RegWidth arch)) -> AccessData sym arch
  WriteData :: 1 <= v
            => S.SymExpr sym (S.BaseBVType (RegWidth arch))
            -> S.SymExpr sym (S.BaseBVType v)
            -> AccessData sym arch

instance S.TestEquality (S.SymExpr sym) => Eq (AccessData sym arch) where
  ReadData e == ReadData e'        | Just _ <- S.testEquality e e' = True
  WriteData i v == WriteData i' v' | Just _ <- S.testEquality i i' 
                                   , Just _ <- S.testEquality v v' = True
  _ == _ = False

instance OrdF (S.SymExpr sym) => Ord (AccessData sym arch) where
  ReadData e    <= ReadData e'     = e `leqF` e'
  WriteData e a <= WriteData e' a' = e `ltF` e' || (e `leqF` e' && a `leqF` a')
  ReadData _    <= WriteData _ _   = True
  WriteData _ _ <= ReadData _      = False

instance ShowF (S.SymExpr sym) => Show (AccessData sym arch) where
  show (ReadData i)    = "Read " ++ showF i
  show (WriteData i v) = "Wrote " ++ showF v ++ " to " ++ showF i

accessAddr :: AccessData sym arch -> S.SymBV sym (RegWidth arch)
accessAddr (ReadData i) = i
accessAddr (WriteData i _) = i

-- | A type synonym for the type of memory
type MemType arch = S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType (RegWidth arch))) (S.BaseBVType 8)

memTypeRepr :: forall arch.
               Architecture arch
            => S.BaseTypeRepr (MemType arch)
memTypeRepr = S.knownRepr


taggedExprImmediate :: forall sym arch s proxy.
                       Architecture arch
                    => proxy sym
                    -> TaggedExpr arch sym s
                    -> Maybe (Some (S.SymExpr sym))
taggedExprImmediate _   (taggedOperand -> ValueOperand imm)      = Just (Some imm)
taggedExprImmediate _   (taggedOperand -> LocationOperand _ _)   = Nothing
taggedExprImmediate sym (taggedOperand -> CompoundOperand oComp) = operandComponentsImmediate @arch sym oComp
taggedExprImmediate _ _ = error "Other tagged operand"

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
data Evaluator arch t st fs =
  Evaluator (forall tp u sh
               . Sym t st fs
              -> ParameterizedFormula (Sym t st fs) arch sh
              -> SL.List (AllocatedOperand arch (Sym t st fs)) sh
              -> Ctx.Assignment (S.Expr t) u
              -> (forall ltp . Location arch ltp -> IO (S.Expr t ltp))
              -> BaseTypeRepr tp
              -> IO (S.Expr t tp))

data FunctionInterpretation t st fs arch =
  FunctionInterpretation { locationInterp :: LocationFuncInterp t st fs arch
                         -- ^ The function interpretation to apply to functions
                         -- appearing in location definition contexts (i.e., the
                         -- 'F.Parameter' function type).
                         , exprInterpName :: TH.Name
                         -- ^ The (template haskell) 'TH.Name' of the function
                         -- to apply statically during formula translation (at
                         -- the value level) to eliminate an uninterpreted
                         -- function appearing in a semantics expression.
                         , exprInterp :: Evaluator arch t st fs
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

createSymbolicName :: String -> String
createSymbolicName s = case '.' `elem` s of
                          False -> s
                          True  -> "uf_" ++ map (\c -> if c == '.' then '_' else c) s
