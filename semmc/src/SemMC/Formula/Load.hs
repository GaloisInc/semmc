{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | Utilities for loading formulas from disk
module SemMC.Formula.Load ( loadFormulas ) where

import qualified GHC.Err.Located as L

import qualified Data.Constraint as C
import qualified Data.Foldable as F
import           System.Directory ( doesFileExist )

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Lang.Crucible.Solver.Interface as CRU

import           SemMC.Architecture ( Architecture )
import           SemMC.Formula.Env ( FormulaEnv )
import qualified SemMC.Formula.Formula as F
import qualified SemMC.Formula.Parser as FP

-- | Load formulas from disk
--
-- Only formulas for the provided values with shape parameters will be loaded;
-- this is because we require the shape parameter attached to each value to
-- successfully parse a formula.
--
-- The provided function is called on each shaped value to produce the
-- 'FilePath' to load.
--
-- The shapes argument specifies a superset of possible opcodes to
-- load formulas for. If the file for an opcode doesn't exist then we
-- skip it. So, the list of shapes can simply be all possible opcodes,
-- and what files actually exist on disk determine what we actually
-- load.
loadFormulas :: forall sym c arch a
              . (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, Architecture arch, MapF.OrdF a)
             => sym
             -> (forall sh' . a sh' -> FilePath)
             -> FormulaEnv sym arch
             -> (forall sh . c sh C.:- FP.BuildOperandList arch sh)
             -> [Some (Witness c a)]
             -> IO (MapF.MapF (Witness c a) (F.ParameterizedFormula sym arch))
loadFormulas sym toFP env impl shapes =
  F.foldlM (\m (Some (w@(Witness _op) :: Witness c a sh)) -> addIfJust readFormulaForOpcode m w C.\\ impl @sh) MapF.empty shapes
  where
    readFormulaForOpcode :: (FP.BuildOperandList arch sh)
                         => Witness c a sh
                         -> IO (Maybe (F.ParameterizedFormula sym arch sh))
    readFormulaForOpcode (Witness a) = do
      let file = toFP a
      fileExists <- doesFileExist file
      if fileExists
      then do
        debug $ "loading file: "++file
        ef <- FP.readFormulaFromFile sym env file
        case ef of
          Left err -> L.error $ "Failed to parse "++file++": "++err
          Right f -> return (Just f)
      else do
        debug $ "skipping non-existent file: "++file
        return Nothing
    debug msg = putStrLn msg

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
