{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM_)
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as Rd
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Language.ASL.Parser as AS
import qualified Language.ASL.Syntax as AS
import System.IO as IO
import System.IO (FilePath)
import System.Exit (exitFailure)

import Lang.Crucible.CFG.Expr as CCE
import Lang.Crucible.CFG.Generator as CCG
import Lang.Crucible.Types as CT
import Lang.Crucible.Backend.Simple as CBS
import Lang.Crucible.FunctionHandle as CFH
import What4.Interface as WI

import SemMC.ASL
import SemMC.ASL.Crucible
import SemMC.ASL.Translation
import SemMC.ASL.Translation.Preprocess

defsFilePath :: FilePath
defsFilePath = "test/defs.parsed"

functions :: [(T.Text, Int)]
functions =  [("A32ExpandImm_C", 2)]

overrides :: Overrides arch
overrides = Overrides {..}
  where overrideStmt stmt = Nothing
        overrideExpr expr = Nothing

main :: IO ()
main = do
  putStrLn "----------------------------------------------"
  putStrLn "Loading ASL definitions..."
  eASLDefs <- AS.parseAslDefsFile defsFilePath
  case eASLDefs of
    Left err -> putStrLn $ "Error loading ASL definitions: " ++ show err
    Right aslDefs -> do
      putStrLn $ "Loaded " ++ show (length aslDefs) ++ " definitions."
      let eDef = computeDefinitions functions aslDefs
      case eDef of
        Left err -> do
          putStrLn $ "Error computing signatures: " ++ show err
          putStrLn "----------------------------------------------"
          exitFailure
        Right defs -> do
          putStrLn "----------------------------------------------"
          forM_ functions $ \(fName, fArity) -> do
            case Map.lookup (mkFunctionName fName fArity) (defSignatures defs) of
              Just (SomeFunctionSignature sig, stmts) -> do
                print sig
                handleAllocator <- CFH.newHandleAllocator
                f <- functionToCrucible defs sig handleAllocator stmts
                backend <- CBS.newSimpleBackend globalNonceGenerator
                let cfg :: SimulatorConfig (SimpleBackend GlobalNonceGenerator (Flags FloatIEEE))
                      = SimulatorConfig { simOutputHandle = IO.stdout
                                        , simHandleAllocator = handleAllocator
                                        , simSym = backend
                                        }
                symFn <- simulateFunction cfg f
                return ()
              _ -> return ()
