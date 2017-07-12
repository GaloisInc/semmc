{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities for loading formulas from disk
module SemMC.Formula.Load ( loadFormulas ) where

import qualified Data.Foldable as F

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )
import qualified Lang.Crucible.Solver.Interface as CRU

import SemMC.Architecture ( Architecture )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as FP
import SemMC.Util ( Witness(..) )

-- | Load formulas from disk
--
-- Only formulas for the provided values with shape parameters will be loaded;
-- this is because we require the shape parameter attached to each value to
-- successfully parse a formula.
--
-- The provided function is called on each shaped value to produce the
-- 'FilePath' to load.
loadFormulas :: forall sym arch a
              . (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, Architecture arch, MapF.OrdF a)
             => sym
             -> (forall sh' . a sh' -> FilePath)
             -> [Some (Witness (FP.BuildOperandList arch) a)]
             -> IO (MapF.MapF a (F.ParameterizedFormula sym arch))
loadFormulas sym toFP shapes =
  F.foldlM (\m (Some (Witness oc)) -> addIfJust readFormulaForOpcode m oc) MapF.empty shapes
  where
    readFormulaForOpcode :: (FP.BuildOperandList arch sh)
                         => a sh
                         -> IO (Maybe (F.ParameterizedFormula sym arch sh))
    readFormulaForOpcode a = do
      ef <- FP.readFormulaFromFile sym (toFP a)
      case ef of
        Left _ -> return Nothing
        Right f -> return (Just f)

addIfJust :: (MapF.OrdF k, Monad m)
          => (k tp -> m (Maybe (a tp)))
          -> MapF.MapF k a
          -> k tp
          -> m (MapF.MapF k a)
addIfJust f m a = do
  mval <- f a
  case mval of
    Nothing -> return m
    Just val -> return (MapF.insert a val m)
