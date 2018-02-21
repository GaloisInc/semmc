{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | Utilities for loading formulas from disk
module SemMC.Formula.Load (
  loadFormulas,
  loadFormulasFromFiles,
  FormulaParseError(..)
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text.Encoding as T
import qualified System.Directory as S

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Lang.Crucible.BaseTypes

import qualified Lang.Crucible.Solver.Interface as CRU

import qualified SemMC.Architecture as A
import qualified SemMC.Formula.Env as FE
import qualified SemMC.Formula.Formula as F
import qualified SemMC.Formula.Parser as FP
import qualified SemMC.Log as L
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
         -> IO (String, (FE.SomeSome (CRU.SymFn sym), Some BaseTypeRepr))
    toUF (name, Some args, retRep@(Some ret)) = do
      uf <- FE.SomeSome <$> CRU.freshTotalUninterpFn sym (U.makeSymbol name) args ret
      return (name, (uf, retRep))

data FormulaParseError = FormulaParseError String String
  deriving (Show)

instance E.Exception FormulaParseError

-- | Load formulas when formula contents are already held in memory.
--
-- This will throw an exception if any of the formula strings is malformed
loadFormulas :: forall sym arch a
                . ( CRU.IsExprBuilder sym
                  , CRU.IsSymInterface sym
                  , A.Architecture arch
                  , HR.HasRepr a (A.ShapeRepr arch)
                  , ShowF a
                  , ShowF (CRU.SymExpr sym)
                  , OrdF a
                  , U.HasCallStack)
                => sym
             -> [(Some a, BS.ByteString)]
             -> IO (MapF.MapF a (F.ParameterizedFormula sym arch))
loadFormulas sym contents = do
  env <- formulaEnv (Proxy @arch) sym
  F.foldlM (parseFormulaBS env) MapF.empty contents
  where
    parseFormulaBS :: FE.FormulaEnv sym arch
                   -> MapF.MapF a (F.ParameterizedFormula sym arch)
                   -> (Some a, BS.ByteString)
                   -> IO (MapF.MapF a (F.ParameterizedFormula sym arch))
    parseFormulaBS env m (Some (op :: a sh), bs) = do
      ef <- FP.readFormula sym env (HR.typeRepr op) (T.decodeUtf8 bs)
      case ef of
        Right f -> return (MapF.insert op f m)
        Left e -> E.throwIO (FormulaParseError (showF op) e)

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
loadFormulasFromFiles :: forall sym arch a
                       . ( CRU.IsExprBuilder sym
                         , CRU.IsSymInterface sym
                         , A.Architecture arch
                         , HR.HasRepr a (A.ShapeRepr arch)
                         , MapF.OrdF a
                         , ShowF (CRU.SymExpr sym)
                         , U.HasCallStack
                         , L.HasLogCfg )
                      => sym
                      -> (forall sh' . a sh' -> FilePath)
                      -> [Some a]
                      -> IO (MapF.MapF a (F.ParameterizedFormula sym arch))
loadFormulasFromFiles sym toFP shapes = do
  env <- formulaEnv (Proxy @arch) sym
  F.foldlM (\m (Some (op :: a sh)) -> addIfJust (readFormulaForOpcode env) m op) MapF.empty shapes
  where
    readFormulaForOpcode :: FE.FormulaEnv sym arch
                         -> a sh
                         -> IO (Maybe (F.ParameterizedFormula sym arch sh))
    readFormulaForOpcode env a = do
      file <- S.canonicalizePath $ toFP a
      fileExists <- S.doesFileExist file
      if fileExists
      then do
        U.logIO U.Info $ "loading file: "++file
        ef <- FP.readFormulaFromFile sym env (HR.typeRepr a) file
        case ef of
          Left err -> do
            L.logIO L.Error ("Failed to parse " ++ file ++ ": " ++ err)
            error $ "Failed to parse "++file++": "++err
          Right f -> return (Just f)
      else do
        return Nothing

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
