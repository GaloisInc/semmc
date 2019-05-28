{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import qualified Control.Monad.State as St
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Language.ASL.Parser as AS
import qualified Language.ASL.Syntax as AS
import System.IO (FilePath)
import System.Exit (exitFailure)

import SemMC.ASL.Crucible.TranslateSig

defsFilePath :: FilePath
defsFilePath = "test/defs.parsed"

callables :: [(T.Text, Int)]
callables = [ ("HasArchVersion", 1)
            , ("HaveEL", 1)
            , ("HaveAnyAArch32", 0)
            , ("HighestELUsingAArch32", 0)
            , ("IsSecureBelowEL3", 0)
            , ("ConstrainUnpredictable", 1)
            , ("ConstrainUnpredictableBool", 1)
            , ("Unreachable", 0)
            , ("RBankSelect", 8)
            , ("LookUpRIndex", 2)
            , ("HighestEL", 0)
            , ("HaveAArch32EL", 1)
            , ("BadMode", 1)
            , ("UsingAArch32", 0)
            , ("IsSecure", 0)
            , ("S1TranslationRegime", 1)
            , ("S1TranslationRegime", 0)
            , ("CurrentCond", 0)
            ]

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
            return (callableSignatureMap st, callableGlobalsMap st)
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
            putStrLn $ "  " ++ show name ++ ": " ++ show sig
          putStrLn $ "\nUnfound callables:"
          forM_ (toList (unfoundCallables finalState)) $ \name ->
            putStrLn $ "  " ++ show name
          putStrLn "----------------------------------------------"
          exitFailure
        Right (sigs, globals) -> do
          putStrLn $ "Computed " ++ show (length sigs) ++ " signatures."
          forM_ (Map.toList sigs) $ \(name, sig) ->
            putStrLn $ "  " ++ show name ++ ": " ++ show sig
          putStrLn $ "\nFound globals for " ++ show (length globals) ++ " callables."
          forM_ (Map.toList globals) $ \(name, _globals) ->
            putStrLn $ "  " ++ show name
          putStrLn "----------------------------------------------"
