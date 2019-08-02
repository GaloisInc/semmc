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

instsFilePath :: FilePath
instsFilePath = "test/insts.parsed"

functions :: [(T.Text, Int)]
functions =  [ ("AddWithCarry", 3)
             ]

instructions :: [(T.Text, T.Text)]
instructions = []

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
          -- forM_ functions $ \(fName, fArity) -> processFunction fName fArity defs

          eASLInsts <- AS.parseAslInstsFile instsFilePath
          case eASLInsts of
            Left err -> putStrLn $ "Error loading ASL instructions: " ++ show err
            Right aslInsts -> do
              putStrLn $ "Loaded " ++ show (length aslInsts) ++ " instructions."
              case computeInstructionSignature "aarch32_ADC_r_A" "aarch32_ADC_r_A1_A" aslInsts aslDefs of
                Left err -> print err
                Right (Some (SomeProcedureSignature iSig)) -> do
                  print iSig
                  -- iProc <- procedureToCrucible aslDefs iSig
                _ -> error "Panic"

processFunction :: T.Text -> Int -> Definitions arch -> IO ()
processFunction fName fArity defs =
  case Map.lookup (mkFunctionName fName fArity) (defSignatures defs) of
  Just (Some (SomeFunctionSignature sig), stmts) -> do
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
  Just (Some (SomeProcedureSignature sig), stmts) -> do
    handleAllocator <- CFH.newHandleAllocator
    p <- procedureToCrucible defs sig handleAllocator stmts
    backend <- CBS.newSimpleBackend globalNonceGenerator
    let cfg :: SimulatorConfig (SimpleBackend GlobalNonceGenerator (Flags FloatIEEE))
          = SimulatorConfig { simOutputHandle = IO.stdout
                            , simHandleAllocator = handleAllocator
                            , simSym = backend
                            }
    symProc <- simulateProcedure cfg p
    return ()
