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
  ( Semantics
  , TemplatedSemantics
  , BaseSet
  , TemplateConstraints
  , TemplatedOperandFn
  , TemplatedOperand(..)
  , RecoverOperandFn(..)
  , TemplatedArch
  , TemplatedFormula(..)
  , TemplatableOperand(..)
  , TemplatedInstruction(..)
  , TemplatedInstructionFormula(..)
  , tifFormula
  , templatedInstructions
  , templatedInputs
  , templatedOutputs
  , genTemplatedFormula
  , recoverOperands
  , unTemplateUnsafe
  , unTemplateSafe
  , unTemplate
  , fromTemplatedSemantics
  , toTemplatedSemantics
  , toBaseSet
  ) where

import           Data.EnumF
import           Data.Kind
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.Some
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.TraversableFC ( FunctorFC(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.Stack ( HasCallStack )
import           GHC.TypeLits ( Symbol )
import           Unsafe.Coerce ( unsafeCoerce )

import qualified What4.BaseTypes as WT
import qualified What4.Interface as S
import           What4.Expr.GroundEval
import qualified What4.Expr.Builder as S

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
newtype RecoverOperandFn sym op =
  RecoverOperandFn { unRecOpFn :: (forall tp. S.SymExpr sym tp -> IO (GroundValue tp)) -> IO op }

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
                                  S.IsSymExprBuilder sym)
                              => sym
                              -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
                              -> IO ( AllocatedOperand arch sym s
                                    , RecoverOperandFn sym (Operand arch s)
                                    )

-- | An operand for 'TemplatedArch'.
data TemplatedOperand (arch :: Type) (s :: Symbol) =
  TemplatedOperand { templOpLocation :: Maybe (Location arch (OperandType arch s))
                   -- ^ If this operand represents a location, this is it.
                   , templUsedLocations :: Set.Set (Some (Location arch))
                   -- ^ Locations used by this operand.
                   , templOpFn :: TemplatedOperandFn arch s
                   -- ^ How to get an expression and recovery function for this
                   -- operand.
                   }

instance ShowF (Location arch) => Show (TemplatedOperand arch s) where
  show op = show (templUsedLocations op)


instance (ShowF (Operand arch), ShowF (Location arch)) => ShowF (TemplatedOperand arch)

instance IsOperand (TemplatedOperand arch)

-- | Phantom architecture used to signal we want to use template operands when
-- instantiating a formula, rather than all concrete operands.
data TemplatedArch (arch :: Type)

type instance OperandComponents (TemplatedArch arch) sym = OperandComponents arch sym
type instance Operand (TemplatedArch arch) = TemplatedOperand arch
type instance Opcode (TemplatedArch arch) = Opcode arch
type instance OperandType (TemplatedArch arch) s = OperandType arch s
type instance Location (TemplatedArch arch) = Location arch
instance (IsOperandTypeRepr arch) => IsOperandTypeRepr (TemplatedArch arch) where
  type OperandTypeRepr (TemplatedArch arch) = OperandTypeRepr arch
  operandTypeReprSymbol _ = operandTypeReprSymbol (Proxy @arch)
type instance RegWidth (TemplatedArch arch) = RegWidth arch

templateAccessData :: AccessData sym arch -> AccessData sym (TemplatedArch arch)
templateAccessData (ReadData e) = ReadData e
templateAccessData (WriteData e v) = WriteData e v


-- | Necessary constraints for 'TemplatedArch' to be valid.
type TemplateConstraints arch = (Architecture arch,
                                 Typeable arch,
                                 TemplatableOperand arch,
                                 IsOperandTypeRepr arch,
                                 OrdF ((Opcode arch) (TemplatedOperand arch)),
                                 ShowF ((Opcode arch) (TemplatedOperand arch)),
                                 EnumF ((Opcode arch) (TemplatedOperand arch)))

instance (TemplateConstraints arch) => Architecture (TemplatedArch arch) where
  data TaggedExpr (TemplatedArch arch) sym s =
    TaggedExpr { taggedExpr :: AllocatedOperand arch sym s
               , taggedRecover :: RecoverOperandFn sym (Operand arch s)
               }

  unTagged te = case taggedExpr te of
    ValueOperand se -> Just se
    LocationOperand _ se -> Just se
    CompoundOperand {} -> Nothing

  taggedOperand = toTemplatedOperand . taggedExpr

  uninterpretedFunctions _ = do 
    MkUninterpFn name args res live <- uninterpretedFunctions (Proxy @arch)
    return $ MkUninterpFn name args res (fmap templateAccessData . live)
  readMemUF n = readMemUF @arch n
  writeMemUF n = writeMemUF @arch n

  allocateSymExprsForOperand _ sym locLookup (TemplatedOperand _ _ f) = do
    (e, r) <- f sym locLookup
    return TaggedExpr { taggedExpr = e, taggedRecover = r }

  operandToLocation _ (TemplatedOperand loc _ _) = loc
  shapeReprToTypeRepr _ = shapeReprToTypeRepr (Proxy @arch)
  locationFuncInterpretation _ = [ (funcName, templatizeInterp fi)
                                 | (funcName, fi) <- locationFuncInterpretation (Proxy @arch)
                                 ]

  archEndianForm _ = archEndianForm (Proxy @arch)

  operandComponentsImmediate = operandComponentsImmediate @arch

-- | This function unwraps 'FunctionInterpretation's and then rewraps them to
-- change the @arch@ of the interpretation.  This lets us re-use the
-- interpretation in the templated architecture without actually having to
-- rewrite them by hand twice (once normally and once templated).
templatizeInterp :: (HasCallStack, OrdF (Location arch), Location arch ~ Location (TemplatedArch arch))
                 => FunctionInterpretation t st fs arch
                 -> FunctionInterpretation t st fs (TemplatedArch arch)
templatizeInterp fi =
  FunctionInterpretation { locationInterp = LocationFuncInterp (templatedLocationInterp (locationInterp fi))
                         , exprInterpName = exprInterpName fi
                         , exprInterp = Evaluator (templatedEvaluator (exprInterp fi))
                         }

templatedLocationInterp :: LocationFuncInterp t st fs arch
                        -> SL.List (AllocatedOperand (TemplatedArch arch) (S.ExprBuilder t st fs)) sh
                        -> WrappedOperand (TemplatedArch arch) sh s
                        -> WT.BaseTypeRepr tp
                        -> Maybe (Location (TemplatedArch arch) tp)
templatedLocationInterp (LocationFuncInterp fi) operands (WrappedOperand orep ix) rep = do
  let ops' = fmapFC fromTemplatedOperand operands
  fi ops' (WrappedOperand orep ix) rep

templatedEvaluator :: forall arch t st fs sh u tp
                    . (OrdF (Location arch))
                   => Evaluator arch t st fs
                   -> S.ExprBuilder t st fs
                   -> ParameterizedFormula (S.ExprBuilder t st fs) (TemplatedArch arch) sh
                   -> SL.List (AllocatedOperand (TemplatedArch arch) (S.ExprBuilder t st fs)) sh
                   -> Ctx.Assignment (S.Expr t) u
                   -> (forall ltp . Location (TemplatedArch arch) ltp -> IO (S.Expr t ltp))
                   -> WT.BaseTypeRepr tp
                   -> IO (S.Expr t tp)
templatedEvaluator (Evaluator e0) = \sym pf ops actuals locExpr tp -> do
  let ops' = fmapFC fromTemplatedOperand ops
  e0 sym (unTemplate pf) ops' actuals locExpr tp

toTemplatedOperand :: AllocatedOperand arch sym s
                   -> AllocatedOperand (TemplatedArch arch) sym s
toTemplatedOperand top =
  case top of
    ValueOperand s -> ValueOperand s
    LocationOperand l s -> LocationOperand l s
    CompoundOperand oc -> CompoundOperand oc

fromTemplatedOperand :: AllocatedOperand (TemplatedArch arch) sym s
                     -> AllocatedOperand arch sym s
fromTemplatedOperand top =
  case top of
    ValueOperand s -> ValueOperand s
    LocationOperand l s -> LocationOperand l s
    CompoundOperand oc -> CompoundOperand oc

instance (S.IsExprBuilder sym, IsLocation (Location arch), ShowF (OperandComponents arch sym)) => Show (TaggedExpr (TemplatedArch arch) sym s) where
  show (TaggedExpr expr _) = "TaggedExpr (" ++ show expr ++ ")"

instance (S.IsExprBuilder sym, IsLocation (Location arch), ShowF (S.SymExpr sym), ShowF (OperandComponents arch sym)) => ShowF (TaggedExpr (TemplatedArch arch) sym) where
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

uncoerceParameter :: Parameter arch sh tp -> Parameter (TemplatedArch arch) sh tp
uncoerceParameter (OperandParameter tp idx) = OperandParameter tp idx
uncoerceParameter (LiteralParameter loc) = LiteralParameter loc
uncoerceParameter (FunctionParameter name (WrappedOperand orep oix) r) = FunctionParameter name (WrappedOperand orep oix) r

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

templateSafe :: forall arch sym sh
              . (OrdF (Location arch))
             => ParameterizedFormula sym arch sh
             -> ParameterizedFormula sym (TemplatedArch arch) sh
templateSafe ParameterizedFormula { pfUses = uses
                                  , pfOperandVars = opVars
                                  , pfLiteralVars = litVars
                                  , pfDefs = defs
                                  } =
  ParameterizedFormula { pfUses = newUses
                       , pfOperandVars = newOpVars
                       , pfLiteralVars = litVars
                       , pfDefs = newDefs
                       }
  where newUses = Set.map (mapSome uncoerceParameter) uses
        newOpVars = fmapFC coerceBoundVar opVars
        newDefs = MapF.foldrWithKey (MapF.insert . uncoerceParameter) MapF.empty defs
        coerceBoundVar :: forall op. BV.BoundVar sym arch op -> BV.BoundVar sym (TemplatedArch arch) op
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
deriving instance ( IsLocation (Location arch)
                  , S.IsExprBuilder sym
                  , ShowF (OperandComponents arch sym)
                  , ShowF (Operand arch)
                  , ShowF (TemplatedOperand arch)
                  , ShowF (S.SymExpr sym)
                  , ShowF (S.BoundVar sym)
                  , ShowF (Location arch))
                 => Show (TemplatedFormula sym arch sh)

instance ( IsLocation (Location arch)
         , S.IsExprBuilder sym
         , ShowF (OperandComponents arch sym)
         , ShowF (Operand arch)
         , ShowF (TemplatedOperand arch)
         , ShowF (S.SymExpr sym)
         , ShowF (S.BoundVar sym)
         , ShowF (Location arch))
        => ShowF (TemplatedFormula sym arch) where
  showF = show

-- | A specific type of operand of which you can generate templates.
class TemplatableOperand (arch :: Type) where
  -- | All possible templates of an operand. In a nutshell, fill in register
  -- parts, leave immediate parts symbolic.
  opTemplates :: OperandTypeRepr arch s -> [TemplatedOperand arch s]

makeTemplatedOpLists :: (TemplatableOperand arch)
                     => ShapeRepr arch sh
                     -> [SL.List (TemplatedOperand arch) sh]
makeTemplatedOpLists rep0 =
  case rep0 of
    SL.Nil -> [SL.Nil]
    rep SL.:< reps -> (SL.:<) <$> opTemplates rep <*> makeTemplatedOpLists reps

recoverOperands :: ShapeRepr arch sh
                -> (forall tp . S.SymExpr sym tp -> IO (GroundValue tp))
                -> SL.List (TaggedExpr (TemplatedArch arch) sym) sh
                -> IO (SL.List (Operand arch) sh)
recoverOperands rep0 evalFn taggedExprs =
  case rep0 of
    SL.Nil -> return SL.Nil
    _rep SL.:< reps ->
      case taggedExprs of
        TaggedExpr _ (RecoverOperandFn recover) SL.:< restExprs ->
          (SL.:<) <$> recover evalFn <*> recoverOperands reps evalFn restExprs


-- | The type of opcodes associated with an architecture, of kind *
type Opcodes arch = (Opcode arch (Operand arch))

-- | The type of semantics; maps from opcodes to (parameterized) formulas for
-- that architecture
type Semantics sym arch = MapF.MapF
                            (Opcodes arch)
                            (ParameterizedFormula sym arch)

-- | The type of templated semantics; maps from opcodes to (parameterized,
-- phantom) formulas for that architecture. 'TemplatedSemantics' differs from
-- 'Semantics' in that the resulting formulas are associated with the phantom
-- 'TemplatedArch'. Replaces 'BaseSet'.
type TemplatedSemantics sym arch = MapF.MapF
                            (Opcodes arch)
                            (ParameterizedFormula sym (TemplatedArch arch))

-- | Renamed to 'TemplatedSemantics'.
type BaseSet sym arch = TemplatedSemantics sym arch

-- | Convert a plain semantics into a templated semantics
toTemplatedSemantics :: (OrdF (Opcodes arch), OrdF (Location arch))
                     => Semantics sym arch
                     -> TemplatedSemantics sym arch
toTemplatedSemantics m = MapF.fromList [ MapF.Pair o (templateSafe pf)
                            | MapF.Pair o pf <- MapF.toList m
                            ]


-- | Convert a plain semantics into a 'BaseSet'. Depricated in favor of
-- 'toTemplatedSemantics'.
toBaseSet :: (OrdF (Opcodes arch), OrdF (Location arch))
          => Semantics sym arch
          -> BaseSet sym arch
toBaseSet = toTemplatedSemantics


-- | Convert a templated semantics into a plain semantics
fromTemplatedSemantics :: forall arch sym. 
                          (OrdF (Location arch), OrdF (Opcodes arch))
                       => TemplatedSemantics sym arch
                       -> Semantics sym arch
fromTemplatedSemantics = MapF.foldrWithKey f MapF.empty
  where f :: (OrdF (Location arch))
          => Opcode arch (Operand arch) sh
          -> ParameterizedFormula sym (TemplatedArch arch) sh
          -> Semantics sym arch
          -> Semantics sym arch
        f op pf = MapF.insert op (unTemplate pf)


data TemplatedInstruction sym arch sh where
  TemplatedInstruction :: (Opcode arch) (Operand arch) sh
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
            FunctionParameter {} -> Set.empty -- error "Function parameters are not actually outputs"
              -- TODO: is throwing an error the correct behavior here?

templatedInstructions :: (TemplateConstraints arch, ArchRepr arch)
                      => TemplatedSemantics sym arch
                      -> [Some (TemplatedInstruction sym arch)]
templatedInstructions baseSet = do
  Pair opcode pf <- MapF.toList baseSet
  oplist <- makeTemplatedOpLists (HR.typeRepr opcode)
  return . Some $! TemplatedInstruction opcode pf oplist

-- | An opcode along with a 'TemplatedFormula' that implements it for specific
-- templated operands.
data TemplatedInstructionFormula sym arch where
  TemplatedInstructionFormula :: TemplatedInstruction sym arch sh
                              -> TemplatedFormula sym arch sh
                              -> TemplatedInstructionFormula sym arch

tifFormula :: TemplatedInstructionFormula sym arch -> Formula sym arch
tifFormula (TemplatedInstructionFormula _ tf) = coerceFormula (tfFormula tf)

genTemplatedFormula :: (TemplateConstraints arch
                       , S.IsSymExprBuilder (S.ExprBuilder t st fs))
                    => S.ExprBuilder t st fs
                    -> TemplatedInstruction (S.ExprBuilder t st fs) arch sh
                    -> IO (TemplatedInstructionFormula (S.ExprBuilder t st fs) arch)
genTemplatedFormula sym ti@(TemplatedInstruction _ pf oplist) =
  TemplatedInstructionFormula ti . uncurry TemplatedFormula <$> instantiateFormula sym pf oplist

{- Note [Evaluating Functions on Templated Operands]

The problem here is that we need to be able to call the evaluator on
TemplatedFormulas.  In this case, the *operands* list is a list of
TemplatedOperand, which is not very closely related to a normal operand, and in
fact might be missing entirely for symbolic operands.

That said, we might be able to evaluate the necessary cases.

UFs should *only* appear in cases where the TemplatedOperand actually has a
concrete location.  SO.  Can we provide some extra argument or infrastructure to
unwrap the TemplatedOperand (assert that it has a Just Location of some kind)
and then apply the non-templated evaluator?

It doesn't seem like it will be possible to write the "location to operand"
translator for types like Memri, which are logically a Gpr + offset bundle.  The
templated operand doesn't have a simple Location that we can translate (it
actually has a Nothing).  Is there more information we can include in the
templated operand to make this possible?  It seems like we could include the
actual Operand type that we could just grab from the TemplatedOperand and use
in-place.  If there is none, we could have a more structured representation with
a good error message (that should never be seen if all else goes according to
plan).  There is enough information at the template definition sites to actually
construct this value.

Note: templOpLocation doesn't seem to be used, so replacing that with something
more useful here would probably be great.  Actually, it is used in a few places,
but they could easily be replaced with a call to operandToLocation (of the
underlying architecture) to achieve the same effect.

It looks like there is a more fundamental problem: the evaluation of functions
inside of the formula instantiator happens *before* the IO action to allocate
symbolic variables in templates (operandValue) is ever executed.  Fundamentally,
this means that we can't implement the interpreters for templated instructions.
However, it looks like we can re-arrange instantiateFormula to call
buildOpAsignment earlier - this would let us fully instantiate the symbolic
variables that make up a templated instruction *before* we do our function
evaluation.  We still need a place to store the generated variables, but we
should be able to modify the definition of OperandValue to support saving a
parallel structure to each operand that will let us store symbolic variables
that represent e.g. offsets in compound operands.

-}
