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
import SemMC.ASL.Translation.Signature

defsFilePath :: FilePath
defsFilePath = "test/defs.parsed"

callables :: [(T.Text, Int)]
callables =  [
--    ("DecodeImmShift", 2)
    -- , ("DecodeRegShift", 1)
  -- , ("Unreachable", 0)
  ("HaveEL", 1)
  -- , ("RBankSelect", 8)
  -- , ("Shift_C", 4)
  ]

overrides :: Overrides arch
overrides = Overrides {..}
  where overrideStmt stmt = Nothing
        overrideExpr expr = Nothing

main :: IO ()
main = do
  putStrLn "----------------------------------------------"
  putStrLn "Loading ASL definitions..."
  eDefs <- AS.parseAslDefsFile defsFilePath
  case eDefs of
    Left err -> putStrLn $ "Error loading ASL definitions: " ++ show err
    Right defs -> do
      putStrLn $ "Loaded " ++ show (length defs) ++ " definitions."
      let eSigs = execSigM defs $ do
            forM_ callables $ \(name, arity) -> computeSignature name arity
            st <- St.get
            env <- Rd.ask
            return ( callableSignatureMap st
                   , callableGlobalsMap st
                   , userTypes st
                   , enums env
                   , consts env)
      case eSigs of
        Left (err, finalState) -> do
          putStrLn $ "Error computing signatures: " ++ show err
          putStrLn $ "\nUser types found:"
          forM_ (Map.toList (userTypes finalState)) $ \(name, tp) ->
            putStrLn $ "  " ++ show name ++ ": " ++ show tp
          putStrLn $ "\nGlobals found:"
          forM_ (Map.toList (callableGlobalsMap finalState)) $ \(name, globals) ->
            putStrLn $ "  " ++ show name ++ ": " ++ intercalate ", " (show <$> fst <$> globals)
          putStrLn $ "\nSignatures found:"
          forM_ (Map.toList (callableSignatureMap finalState)) $ \(name, sig) ->
            putStrLn $ "  " ++ show name ++ ": " ++ show (fst sig)
          putStrLn $ "\nUnfound callables:"
          forM_ (toList (unfoundCallables finalState)) $ \name ->
            putStrLn $ "  " ++ show name
          putStrLn "----------------------------------------------"
          exitFailure
        Right (sigs, globals, userTypes, enums, consts) -> do
          putStrLn $ "Computed " ++ show (length sigs) ++ " signatures."
          forM_ (Map.toList sigs) $ \(name, sig) ->
            putStrLn $ "  " ++ show name ++ ": " ++ show (fst sig)
          putStrLn $ "\nFound globals for " ++ show (length globals) ++ " callables."
          forM_ (Map.toList globals) $ \(name, _globals) ->
            putStrLn $ "  " ++ show name
          putStrLn "----------------------------------------------"
          let definitions = Definitions
                { defSignatures = (fst <$> sigs)
                , defTypes = userTypes
                , defEnums = enums
                , defConsts = consts
                , defOverrides = overrides
                }
          forM_ sigs $ \(sig, c) -> do
            case sig of
              SomeFunctionSignature sig -> do
                handleAllocator <- CFH.newHandleAllocator
                f <- functionToCrucible definitions sig handleAllocator (callableStmts c)
                backend <- CBS.newSimpleBackend globalNonceGenerator
                let cfg :: SimulatorConfig (SimpleBackend GlobalNonceGenerator (Flags FloatIEEE))
                      = SimulatorConfig { simOutputHandle = IO.stdout
                                        , simHandleAllocator = handleAllocator
                                        , simSym = backend
                                        }
                symExpr <- simulateFunction cfg f
                return ()
              _ -> return ()
