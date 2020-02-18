{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
-- | Utilities for loading formulas from disk
module SemMC.Formula.Load (
  formulaEnv,
  loadFormulas,
  loadFormulasFromFiles,
  loadLibrary,
  loadLibraryFromFiles,
  listFunctionFiles,
  FormulaParseError(..),
  formulaEnv
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text.Encoding as T
import qualified System.Directory as S
import qualified System.FilePath as S

import           Data.Parameterized.Classes
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Pair as Pair
import           Data.Parameterized.Some ( Some(..) )
import           What4.BaseTypes

import qualified What4.Interface as CRU
import qualified Lang.Crucible.Backend as CRUB

import qualified SemMC.Architecture as A
import qualified SemMC.Formula.Env as FE
import qualified SemMC.Formula.Formula as F
import qualified SemMC.Formula.Parser as FP
import qualified SemMC.Log as L
import qualified SemMC.Util as U

formulaEnv :: forall sym arch
            . (A.Architecture arch, CRU.IsSymExprBuilder sym)
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
    toUF :: A.UninterpFn arch
         -> IO (String, (FE.SomeSome (CRU.SymFn sym), Some BaseTypeRepr))
    toUF (A.MkUninterpFn name args ret _) = do
      uf <- FE.SomeSome <$> CRU.freshTotalUninterpFn sym (U.makeSymbol ("uf." ++ name)) args ret
      return (("uf." ++ name), (uf, Some ret))

data FormulaParseError = FormulaParseError String String
  deriving (Show)

instance E.Exception FormulaParseError

-- | Load formulas when formula contents are already held in memory.
--
-- This will throw an exception if any of the formula strings is malformed
loadFormulas :: forall sym arch a
                . ( CRU.IsSymExprBuilder sym
                  , A.Architecture arch
                  , HR.HasRepr a (A.ShapeRepr arch)
                  , ShowF a
                  , ShowF (CRU.SymExpr sym)
                  , OrdF a
                  , U.HasCallStack
                  , L.HasLogCfg )
                => sym
             -> FE.FormulaEnv sym arch
             -> F.Library sym
             -> [(Some a, BS.ByteString)]
             -> IO (MapF.MapF a (F.ParameterizedFormula sym arch))
loadFormulas sym initEnv lib contents = do
  let env = FE.addLibrary initEnv lib
  F.foldlM (parseFormulaBS env) MapF.empty contents
  where
    parseFormulaBS :: FE.FormulaEnv sym arch
                   -> MapF.MapF a (F.ParameterizedFormula sym arch)
                   -> (Some a, BS.ByteString)
                   -> IO (MapF.MapF a (F.ParameterizedFormula sym arch))
    parseFormulaBS env m (Some (op :: a sh), bs) = do
      U.logIO U.Info $ "reading formula for opcode " ++ showF op
      ef <- FP.readFormula sym env (HR.typeRepr op) (T.decodeUtf8 bs)
      case ef of
        Right f -> return (MapF.insert op f m)
        Left e -> putStrLn "Trying to load formulas" >> E.throwIO (FormulaParseError (showF op) e)

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
--
-- The funDir argument gives a directory whose defined functions will be
-- loaded. All files with extension `.fun` in this directory will be loaded.
loadFormulasFromFiles :: forall sym arch a
                       . ( CRU.IsSymExprBuilder sym
                         , A.Architecture arch
                         , HR.HasRepr a (A.ShapeRepr arch)
                         , MapF.OrdF a
                         , ShowF (CRU.SymExpr sym)
                         , U.HasCallStack
                         , L.HasLogCfg )
                      => sym
                      -> FE.FormulaEnv sym arch
                      -> F.Library sym
                      -> (forall sh' . a sh' -> FilePath)
                      -> [Some a]
                      -> IO (MapF.MapF a (F.ParameterizedFormula sym arch))
loadFormulasFromFiles sym initEnv lib toFP shapes = do
--  initEnv <- formulaEnv (Proxy @arch) sym
  let env = FE.addLibrary initEnv lib
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

-- | Load library when contents are already held in memory.
--
-- This will throw an exception if any of the strings is malformed
loadLibrary :: forall sym arch
             . ( CRU.IsExprBuilder sym
               , CRUB.IsSymInterface sym
               , A.Architecture arch
               , ShowF (CRU.SymExpr sym)
               , U.HasCallStack
               , L.HasLogCfg )
            => Proxy arch
            -> sym
            -> FE.FormulaEnv sym arch
            -> [(String, BS.ByteString)]
            -> IO (F.Library sym)
loadLibrary _ sym env contents = do
  -- TODO Allow functions to call other functions by somehow loading in
  -- dependency order and adding to the environment as we go. For now, for
  -- predictability's sake, we load everything in the initial environment.
--  env <- formulaEnv proxy sym
  MapF.fromList <$> mapM parseFunctionBS contents
  where
    parseFunctionBS :: (String, BS.ByteString)
                    -> IO (Pair.Pair F.FunctionRef (F.FunctionFormula sym))
    parseFunctionBS (name, bs) = do
      U.logIO U.Info $ "reading formula for defined function " ++ show name
      ef <- FP.readDefinedFunction sym env (T.decodeUtf8 bs)
      case ef of
        Right (Some ff) ->
          return $ Pair.Pair (F.functionRef ff) ff
        Left e -> putStrLn "Trying to load library" >> E.throwIO (FormulaParseError name e)

loadLibraryFromFiles :: forall sym arch
                      . ( CRU.IsExprBuilder sym
                        , CRUB.IsSymInterface sym
                        , A.Architecture arch
                        , ShowF (CRU.SymExpr sym)
                        , U.HasCallStack
                        , L.HasLogCfg )
                     => Proxy arch
                     -> sym
                     -> FE.FormulaEnv sym arch
                     -> FilePath
                     -> IO (F.Library sym)
loadLibraryFromFiles _ sym env dir = do
  files <- listFunctionFiles dir
--  env <- formulaEnv proxy sym
  -- TODO Allow functions to call other functions by somehow loading in
  -- dependency order and adding to the environment as we go. For now, for
  -- predictability's sake, we load everything in the initial environment.
  MapF.fromList <$> mapM loadFile files
  where
    loadFile :: FilePath
             -> IO (Pair.Pair F.FunctionRef (F.FunctionFormula sym))
    loadFile origFile = do
      file <- S.canonicalizePath origFile
      U.logIO U.Info $ "loading function file: "++file
      ef <- FP.readDefinedFunctionFromFile sym env file
      case ef of
        Left err -> do
          let msg = "Failed to parse function from " ++ file ++ ": " ++ err
          L.logIO L.Error msg
          error msg
        Right (Some fun) ->
          return $ Pair.Pair (F.functionRef fun) fun

listFunctionFiles :: FilePath -> IO [FilePath]
listFunctionFiles dir =
  (fmap $ S.combine dir) <$>
  filter isFunctionFile <$>
    E.catch (S.listDirectory dir) (\(_::E.SomeException) -> return [])
  where isFunctionFile f = snd (S.splitExtension f) == ".fun"
