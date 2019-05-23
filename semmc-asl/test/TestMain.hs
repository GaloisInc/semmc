module Main where

import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Language.ASL.Parser as AS
import qualified Language.ASL.Syntax as AS
import System.IO (FilePath)

import SemMC.ASL.Crucible.TranslateSig

defsFilePath :: FilePath
defsFilePath = "test/defs.parsed"

main :: IO ()
main = do
  putStrLn "Loading ASL definitions..."
  eDefs <- AS.parseAslDefsFile defsFilePath
  case eDefs of
    Left err -> putStrLn $ "Error loading ASL definitions: " ++ show err
    Right defs -> do
      putStrLn $ "Loaded " ++ show (length defs) ++ " definitions."
      let eSigs = computeSignatures defs
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
        Right sigs -> do
          putStrLn $ "Computed " ++ show (length sigs) ++ " signatures."
          forM_ (Map.toList sigs) $ \(name, sig) ->
            putStrLn $ "  " ++ show name ++ ": " ++ show sig
