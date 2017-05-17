-- | Definitions of formulas
module SemMC.Formula (
  Formula(..),
  SV(..),
  formulaToSBV,
  formulaFromProgram
  ) where

import qualified Control.Monad.Catch as C
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid
import qualified Data.SBV.Dynamic as SBV

import Prelude

import qualified Dismantle.Instruction as I

-- | Formulas with parameters that can be instantiated with concrete operand
-- values
data ParameterizedFormula = ParameterizedFormula
  deriving (Show)

-- | Formulas with only concrete values
data Formula =
  Formula { inputs :: [String]
          -- ^ The variables that serve as inputs to the formula
          , defs :: [(String, SV)]
          -- ^ The variables defined by the formula (with definitions in terms
          -- of the input variables)
          }
  deriving (Show)

emptyFormula :: Formula
emptyFormula = Formula { inputs = [], defs = [] }

-- | Combine two formulas in sequential execution
sequenceFormulas :: Formula -> Formula -> Formula
sequenceFormulas = undefined

instance Monoid Formula where
  mempty = emptyFormula
  mappend = sequenceFormulas

data SV = BV_ADD SV SV
        | BV_SUB SV SV
        | BV_LIT Word
        | VAR String
  deriving (Eq, Ord, Show)

formulaToSBV :: Formula -> SBV.SVal
formulaToSBV = undefined svToSBV

svToSBV :: SV -> SBV.SVal
svToSBV = undefined

newtype BaseSet opcode operand = BaseSet { unBaseSet :: M.Map (I.SomeOpcode opcode operand) ParameterizedFormula }
  deriving (Show)

data FormulaError opcode operand = NoFormulaForOpcode (I.SomeOpcode opcode operand)
  deriving (Show)

instance (I.OpcodeConstraints opcode operand) => C.Exception (FormulaError opcode operand)

-- | Instantiate a parameterized formula for a given instruction
instantiateFormula :: ParameterizedFormula -> I.GenericInstruction opcode operand -> Formula
instantiateFormula = undefined

lookupSemantics :: (C.MonadThrow m, I.OpcodeConstraints opcode operand)
                => BaseSet opcode operand
                -> I.GenericInstruction opcode operand
                -> m Formula
lookupSemantics (BaseSet m) i =
  case i of
    I.Instruction op _ ->
      case M.lookup (I.SomeOpcode op) m of
        Just f -> return (instantiateFormula f i)
        Nothing -> C.throwM (NoFormulaForOpcode (I.SomeOpcode op))

formulaFromProgram :: (C.MonadThrow m, I.OpcodeConstraints opcode operand)
                   => BaseSet opcode operand
                   -- ^ The formulas for instructions with known semantics
                   -> [I.GenericInstruction opcode operand]
                   -- ^ The program to create a formula for
                   -> m Formula
formulaFromProgram base p = do
  sems <- mapM (lookupSemantics base) p
  return (F.foldl' sequenceFormulas emptyFormula sems)
