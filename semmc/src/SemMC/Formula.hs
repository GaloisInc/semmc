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
  , WrappedOperand(..)
  , LocationFuncInterp(..)

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

  -- * Functions and libraries
  , FunctionFormula(..)
  , FunctionRef(..)
  , functionRef
  , Library
  , emptyLibrary

    -- * SemMC.Formula.Env
  , FormulaEnv(..)

    -- * SemMC.Formula.Parser
  , loadFormulas
  , loadFormulasFromFiles
  , readFormula
  , readFormulaFromFile
  , loadLibrary
  , loadLibraryFromFiles
  , readDefinedFunction
  , readDefinedFunctionFromFile

    -- * SemMC.Formula.Printer
  , printFormula
  , printParameterizedFormula
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
  , WrappedOperand(..)
  , LocationFuncInterp(..)
  , FunctionFormula(..)
  , FunctionRef(..)
  , functionRef
  , Library
  , emptyLibrary
  )
import SemMC.Formula.Env ( FormulaEnv(..) )
import SemMC.Formula.Instantiate ( instantiateFormula,
                                   sequenceFormulas,
                                   copyFormula,
                                   condenseFormulas,
                                   replaceVars,
                                   replaceLitVars
                                 )
import SemMC.Formula.Load ( loadFormulas, loadFormulasFromFiles
                          , loadLibrary, loadLibraryFromFiles )
import SemMC.Formula.Parser ( readFormula, readFormulaFromFile,
                              readDefinedFunction, readDefinedFunctionFromFile )
import SemMC.Formula.Printer ( printParameterizedFormula, printFormula )
import SemMC.Formula.Equivalence ( EquivalenceResult(..),
                                   formulasEquiv,
                                   formulasEquivConcrete,
                                   formulasEquivSym,
                                   checkSatZ3
                                 )
