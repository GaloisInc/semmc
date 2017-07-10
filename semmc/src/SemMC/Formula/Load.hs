{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities for loading formulas from disk
module SemMC.Formula.Load ( loadFormulas ) where

import qualified Data.Foldable as F
import System.FilePath ( (</>), (<.>) )

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )
import qualified Lang.Crucible.Solver.Interface as CRU

import SemMC.Architecture ( Architecture, Opcode, Operand )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as FP
import SemMC.Util ( Witness(..) )

-- | Load formulas from disk
--
-- This function assumes that all of the formulas to be loaded live in a single
-- directory (passed in as a 'FilePath' and have the @.sem@ extension.
--
-- Only formulas for the opcodes in the provided list will be loaded; this is
-- because we require the shape parameter attached to each opcode to
-- successfully parse a formula.
loadFormulas :: forall sym arch
              . (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, Architecture arch, MapF.OrdF (Opcode arch (Operand arch)))
             => sym
             -> [Some (Witness (FP.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
             -> FilePath
             -> IO (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch))
loadFormulas sym allOpcodes formulaDir = F.foldlM (\m (Some (Witness oc)) -> go m oc) MapF.empty allOpcodes
  where
    go :: (FP.BuildOperandList arch sh, MapF.OrdF (Opcode arch (Operand arch)))
       => MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
       -> Opcode arch (Operand arch) sh
       -> IO (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch))
    go m oc = do
      mpf <- readFormulaForOpcode oc
      case mpf of
        Nothing -> return m
        Just pf -> return (MapF.insert oc pf m)

    readFormulaForOpcode :: (FP.BuildOperandList arch sh)
                         => Opcode arch (Operand arch) sh
                         -> IO (Maybe (F.ParameterizedFormula sym arch sh))
    readFormulaForOpcode oc = do
      let fp = formulaDir </> P.showF oc <.> "sem"
      ef <- FP.readFormulaFromFile sym fp
      case ef of
        Left _ -> return Nothing
        Right f -> return (Just f)
