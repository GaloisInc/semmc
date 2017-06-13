{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Definitions of formulas
module SemMC.Formula (
  TemplatedFormula(..),
  Formula(..),
  Parameter(..),
  BaseSet(..),
  loadBaseSet,
  formulaFromProgram
  ) where

import           Control.Monad (foldM)
import qualified Control.Monad.Catch as C
import           Control.Monad.IO.Class (MonadIO(..))
-- import qualified Data.Foldable as F
import qualified Data.Map as M
-- import           Data.Monoid
import qualified Data.Text as T

import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.Instruction as I

-- | Input variables in a templated formula.
data Parameter = Operand T.Text
               -- ^ "Templated" part of an instruction, like an operand
               | Literal T.Text
               -- ^ A literal value, like a concrete register ("r8")
               deriving (Show, Eq, Ord)

-- | A "templated" or "parameterized" formula, i.e., a formula that has holes
-- that need to be filled in before it represents an actual concrete
-- instruction.
data TemplatedFormula sym =
  TemplatedFormula { tfOperands :: [(T.Text, T.Text)]
                   , tfUses :: [Parameter]
                   , tfParamExprs :: [(Parameter, Some (S.SymExpr sym))]
                   , tfDefs :: [(Parameter, Some (S.SymExpr sym))]
                   }

instance (ShowF (S.SymExpr sym)) => Show (TemplatedFormula sym) where
  show (TemplatedFormula { tfOperands = ops
                         , tfUses = uses
                         , tfParamExprs = paramExprs
                         , tfDefs = defs
                         }) = "TemplatedFormula { tfOperands = " ++ show ops ++ ", tfUses = " ++ show uses ++ ", tfParamExprs = " ++ show paramExprs ++ ", tfDefs = " ++ show defs ++ " }"

-- | Represents an input or output of a formula, i.e. a specific register, flag,
-- or memory. Eventually, this should probably be replaced with an
-- architecture-specific ADT, but we have this for now.
newtype FormulaVar = FormulaVar { unFormulaVar :: T.Text }
  deriving (Show)

-- | A formula representing a concrete instruction.
data Formula sym =
  Formula { formUses :: [FormulaVar]
          , formParamExprs :: [(FormulaVar, Some (S.SymExpr sym))]
          , formDefs :: [(FormulaVar, Some (S.SymExpr sym))]
          }

instance (ShowF (S.SymExpr sym)) => Show (Formula sym) where
  show (Formula { formUses = uses
                , formParamExprs = paramExprs
                , formDefs = defs
                }) = "Formula { formUses = " ++ show uses ++ ", formParamExprs = " ++ show paramExprs ++ ", formDefs = " ++ show defs ++ " }"

emptyFormula :: Formula sym
emptyFormula = Formula { formUses = [], formParamExprs = [], formDefs = [] }

-- | Combine two formulas in sequential execution
sequenceFormulas :: sym -> Formula sym -> Formula sym -> IO (Formula sym)
sequenceFormulas = undefined

newtype BaseSet sym opcode operand = BaseSet { unBaseSet :: M.Map (I.SomeOpcode opcode operand) (TemplatedFormula sym) }

instance (ShowF (S.SymExpr sym), Show (I.SomeOpcode opcode operand)) => Show (BaseSet sym opcode operand) where
  show (BaseSet m) = "BaseSet { unBaseSet = " ++ show m ++ " }"

loadBaseSet :: sym -> FilePath -> IO (BaseSet sym opcode operand)
loadBaseSet = undefined

data FormulaError opcode operand = NoFormulaForOpcode (I.SomeOpcode opcode operand)
  deriving (Show)

instance (I.OpcodeConstraints opcode operand) => C.Exception (FormulaError opcode operand)

-- | Instantiate a templated formula for a given instruction
instantiateFormula :: sym -> TemplatedFormula sym -> I.GenericInstruction opcode operand -> IO (Formula sym)
instantiateFormula = undefined

lookupSemantics :: (C.MonadThrow m, MonadIO m, I.OpcodeConstraints opcode operand)
                => sym
                -> BaseSet sym opcode operand
                -> I.GenericInstruction opcode operand
                -> m (Formula sym)
lookupSemantics sym (BaseSet m) i =
  case i of
    I.Instruction op _ ->
      case M.lookup (I.SomeOpcode op) m of
        Just f -> liftIO $ instantiateFormula sym f i
        Nothing -> C.throwM (NoFormulaForOpcode (I.SomeOpcode op))

formulaFromProgram :: (C.MonadThrow m, MonadIO m, I.OpcodeConstraints opcode operand)
                   => sym
                   -- ^ The SymInterface used to build expressions
                   -> BaseSet sym opcode operand
                   -- ^ The formulas for instructions with known semantics
                   -> [I.GenericInstruction opcode operand]
                   -- ^ The program to create a formula for
                   -> m (Formula sym)
formulaFromProgram sym base p = do
  sems <- mapM (lookupSemantics sym base) p
  liftIO $ foldM (sequenceFormulas sym) emptyFormula sems
