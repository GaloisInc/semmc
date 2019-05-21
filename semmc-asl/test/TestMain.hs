module Main where

import qualified Language.ASL.Parser as AS
import qualified Language.ASL.Syntax as AS
import System.IO (FilePath)

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

