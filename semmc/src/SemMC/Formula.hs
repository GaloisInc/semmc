-- | This module exports all of the tools required for manipulating our
-- representation of formulas
--
-- Import this module unless you are implementing new functions in
-- @SemMC.Formula.*@; those modules only exist to keep module sizes down.
module SemMC.Formula
  (
    -- * SemMC.Formula.Formula
    ParameterizedFormula(..)
  , Formula(..)
  , formInputs
  , formOutputs
  , emptyFormula
  , coerceFormula
  , Parameter(..)
  , paramType

  -- * Use and manipulation
  , instantiateFormula
  , sequenceFormulas
  , copyFormula
  , condenseFormulas
  , replaceLitVars
  , replaceVars

  -- * Equivalence Checking
  , EquivalenceResult(..)
  , formulasEquiv
  , formulasEquivConcrete
  , formulasEquivSym
  , checkSatZ3

    -- * SemMC.Formula.Env
  , FormulaEnv(..)

    -- * SemMC.Formula.Parser
  , loadFormulas
  , readFormula
  , readFormulaFromFile

    -- * SemMC.Formula.Printer
  , printFormula

  -- * Classes
  , BuildOperandList
  , ConvertShape
  ) where

import SemMC.Formula.Formula
  ( ParameterizedFormula(..)
  , Formula(..)
  , formInputs
  , formOutputs
  , emptyFormula
  , coerceFormula
  , Parameter(..)
  , paramType
  )
import SemMC.Formula.Env ( FormulaEnv(..) )
import SemMC.Formula.Instantiate ( instantiateFormula,
                                   sequenceFormulas,
                                   copyFormula,
                                   condenseFormulas,
                                   replaceVars,
                                   replaceLitVars
                                 )
import SemMC.Formula.Load ( loadFormulas )
import SemMC.Formula.Parser ( readFormula, readFormulaFromFile, BuildOperandList )
import SemMC.Formula.Printer ( printFormula, ConvertShape )
import SemMC.Formula.Equivalence ( EquivalenceResult(..),
                                   formulasEquiv,
                                   formulasEquivConcrete,
                                   formulasEquivSym,
                                   checkSatZ3
                                 )
