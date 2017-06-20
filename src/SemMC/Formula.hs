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
  Parameter(..),
  paramType,
  BaseSet(..),
  Index(..),
  idxToInt,
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
import qualified Data.ShowF as SF

import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.BaseTypes

import qualified Dismantle.Instruction as I

import           SemMC.Architecture

data Index :: [k] -> k -> * where
  IndexHere :: forall x sh. Index (x ': sh) x
  IndexThere :: forall x x' sh. Index sh x -> Index (x' ': sh) x
deriving instance Eq (Index sh x)
deriving instance Show (Index sh x)

instance TestEquality (Index sh) where
  IndexHere `testEquality` IndexHere = Just Refl
  IndexThere idx1 `testEquality` IndexThere idx2 = testEquality idx1 idx2
  _ `testEquality` _ = Nothing

idxToInt :: Index sh x -> Int
idxToInt IndexHere = 0
idxToInt (IndexThere th) = 1 + idxToInt th

instance Ord (Index sh x) where
  x `compare` y = idxToInt x `compare` idxToInt y

instance OrdF (Index sh) where
  IndexHere `compareF` IndexHere = EQF
  IndexHere `compareF` IndexThere _ = LTF
  IndexThere _ `compareF` IndexHere = GTF
  IndexThere idx1 `compareF` IndexThere idx2 =
    case idx1 `compareF` idx2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

indexOpList :: I.OperandList f sh -> Index sh s -> f s
-- Why not destructure @vals@ in the argument position? This way, GHC verifies
-- that the pattern-matching is exhaustive.
indexOpList vals IndexHere = case vals of x I.:> _ -> x
indexOpList vals (IndexThere th) = case vals of _ I.:> rest -> indexOpList rest th

-- | Input variables in a templated formula.
-- data Parameter arch (sh :: [Symbol]) (tp :: BaseType) = Operand (I.Index sh tp)
--                                                       -- TODO: ensure this has the right type (tp)
--                                                       -- ^ "Templated" part of an instruction, like an operand
--                                                       | Literal (StateVar arch tp)
--                                                       -- ^ A literal value, like a concrete register ("r8")
--                                                       deriving (Show, Eq, Ord)
data Parameter arch (sh :: [Symbol]) (tp :: BaseType) where
  Operand :: BaseTypeRepr (OperandType arch s) -> Index sh s -> Parameter arch sh (OperandType arch s)
  Literal :: BaseTypeRepr tp -> StateVar arch tp -> Parameter arch sh tp

deriving instance Show (StateVar arch tp) => Show (Parameter arch sh tp)

instance ShowF (StateVar arch) => ShowF (Parameter arch sh) where
  showF (Operand repr idx) = unwords ["Operand", "(" ++ show repr ++ ")", show idx]
  showF (Literal repr var) = unwords ["Literal", "(" ++ show repr ++ ")", showF var]

instance TestEquality (StateVar arch) => TestEquality (Parameter arch sh) where
  Operand _ idx1 `testEquality` Operand _ idx2 = (\Refl -> Refl) <$> testEquality idx1 idx2
  Literal _ var1 `testEquality` Literal _ var2 = (\Refl -> Refl) <$> testEquality var1 var2
  _              `testEquality`              _ = Nothing

instance Eq (StateVar arch tp) => Eq (Parameter arch sh tp) where
  Operand _ idx1 == Operand _ idx2 = isJust $ testEquality idx1 idx2
  Literal _ var1 == Literal _ var2 = var1 == var2
  _              ==              _ = False

instance OrdF (StateVar arch) => OrdF (Parameter arch sh) where
  Operand _ _ `compareF` Literal _ _ = LTF
  Literal _ _ `compareF` Operand _ _ = GTF
  Operand _ idx1 `compareF` Operand _ idx2 =
    case idx1 `compareF` idx2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF
  Literal _ var1 `compareF` Literal _ var2 =
    case var1 `compareF` var2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

paramType :: Parameter arch sh tp -> BaseTypeRepr tp
paramType (Operand repr _) = repr
paramType (Literal repr _) = repr

-- instance Ord (StateVar arch tp) => Ord (Parameter arch sh tp) where
--   Operand _ `compare` Literal _ = Lt

-- | A "templated" or "parameterized" formula, i.e., a formula that has holes
-- that need to be filled in before it represents an actual concrete
-- instruction.
data ParameterizedFormula sym arch (sh :: [Symbol]) =
  ParameterizedFormula { pfUses :: Set.Set (Some (Parameter arch sh))
                       , pfOperandVars :: I.OperandList (BoundVar sym arch) sh
                       , pfLiteralVars :: MapF.MapF (StateVar arch) (S.BoundVar sym)
                       , pfDefs :: MapF.MapF (Parameter arch sh) (S.SymExpr sym)
                       }
deriving instance (SF.ShowF (BoundVar sym arch), ShowF (StateVar arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym)) => Show (ParameterizedFormula sym arch sh)

instance (SF.ShowF (BoundVar sym arch), ShowF (StateVar arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym)) => ShowF (ParameterizedFormula sym arch) where
  showF = show

-- | Represents an input or output of a formula, i.e. a specific register, flag,
-- or memory. Eventually, this should probably be replaced with an
-- architecture-specific ADT, but we have this for now.
-- newtype FormulaVar = FormulaVar { unFormulaVar :: T.Text }
--   deriving (Show, Ord, Eq)

-- | A formula representing a concrete instruction.
data Formula sym arch =
  Formula { formUses :: Set.Set (Some (StateVar arch))
          , formParamVars :: MapF.MapF (StateVar arch) (S.BoundVar sym)
          , formDefs :: MapF.MapF (StateVar arch) (S.SymExpr sym)
          }
deriving instance (ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (StateVar arch)) => Show (Formula sym arch)

emptyFormula :: Formula sym arch
emptyFormula = Formula { formUses = Set.empty, formParamVars = MapF.empty, formDefs = MapF.empty }

-- | Combine two formulas in sequential execution
sequenceFormulas :: sym -> Formula sym arch -> Formula sym arch -> IO (Formula sym arch)
sequenceFormulas = undefined

-- replaceVarExpression :: sym -> Formula sym -> FormulaVar -> Some (S.SymExpr sym) -> IO (Formula sym)

newtype BaseSet sym arch = BaseSet { unBaseSet :: MapF.MapF ((Opcode arch) (Operand arch)) (ParameterizedFormula sym arch) }
deriving instance (SF.ShowF (BoundVar sym arch),
                   ShowF (S.SymExpr sym),
                   ShowF (S.BoundVar sym),
                   ShowF (StateVar arch),
                   ShowF ((Opcode arch) (Operand arch)))
                => Show (BaseSet sym arch)

loadBaseSet :: sym -> FilePath -> IO (BaseSet sym arch)
loadBaseSet = undefined

data FormulaError opcode operand = NoFormulaForOpcode (I.SomeOpcode opcode operand)
  deriving (Show)

-- instance (I.OpcodeConstraints opcode operand) => C.Exception (FormulaError opcode operand)
-- instance (I.OpcodeConstraints opcode operand) => C.Exception (FormulaError opcode operand)

-- | Instantiate a templated formula for a given instruction
-- this definition is currently bogus
-- instantiateFormula :: sym -> ParameterizedFormula sym arch sh -> I.GenericInstruction (Opcode arch) (Operand arch) -> IO (Formula sym arch)
-- instantiateFormula = undefined
-- instantiateFormula _ pf _ = return $ Formula (Set.map p2FV $ pfUses pf) (Map.mapKeys p2FV $ pfParamVars pf) (Map.mapKeys p2FV $ pfDefs pf)
--   where p2FV (Operand op) = FormulaVar ("op_" <> op)
--         p2FV (Literal lit) = FormulaVar ("lit_" <> lit)

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
