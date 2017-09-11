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
import qualified Data.Map.Strict as Map
import           Data.Proxy ( Proxy(..) )
import           System.Directory ( doesFileExist )

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )
import           Lang.Crucible.BaseTypes

import qualified Lang.Crucible.Solver.Interface as CRU

import qualified SemMC.Architecture as A
import qualified SemMC.Formula.Env as FE
import qualified SemMC.Formula.Formula as F
import qualified SemMC.Formula.Parser as FP
import qualified SemMC.Util as U

formulaEnv :: forall sym arch
            . (A.Architecture arch, CRU.IsExprBuilder sym, CRU.IsSymInterface sym)
           => Proxy arch
           -> sym
           -> IO (FE.FormulaEnv sym arch)
formulaEnv proxy sym = do
  undefinedBit <- CRU.freshConstant sym (U.makeSymbol "undefined_bit") knownRepr
  ufs <- Map.fromList <$> mapM toUF (A.uninterpretedFunctions proxy)
  return FE.FormulaEnv { FE.envFunctions = ufs
                       , FE.envUndefinedBit = undefinedBit
                       }
  where
    toUF :: (String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)
         -> IO (String, FE.SomeSome (CRU.SymFn sym))
    toUF (name, Some args, Some ret) = do
      uf <- FE.SomeSome <$> CRU.freshTotalUninterpFn sym (U.makeSymbol name) args ret
      return (name, uf)

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
              . (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, A.Architecture arch, MapF.OrdF a)
             => sym
             -> (forall sh' . a sh' -> FilePath)
             -> (forall sh . c sh C.:- FP.BuildOperandList arch sh)
             -> [Some (Witness c a)]
             -> IO (MapF.MapF (Witness c a) (F.ParameterizedFormula sym arch))
loadFormulas sym toFP impl shapes = do
  env <- formulaEnv (Proxy @arch) sym
  F.foldlM (\m (Some (w@(Witness _op) :: Witness c a sh)) -> addIfJust (readFormulaForOpcode env) m w C.\\ impl @sh) MapF.empty shapes
  where
    readFormulaForOpcode :: (FP.BuildOperandList arch sh)
                         => FE.FormulaEnv sym arch
                         -> Witness c a sh
                         -> IO (Maybe (F.ParameterizedFormula sym arch sh))
    readFormulaForOpcode env (Witness a) = do
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
