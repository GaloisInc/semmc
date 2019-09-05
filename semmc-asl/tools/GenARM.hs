{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import qualified Control.Exception as X
import           Control.Monad (forM_, foldM)
import qualified Control.Monad.State.Lazy as MSS
import qualified Data.Map as Map
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.List as List
import qualified Lang.Crucible.Backend.Simple as CBS
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Language.ASL.Parser as AP
import qualified Language.ASL.Syntax as AS
import System.Exit (exitFailure)
import qualified System.IO as IO

import SemMC.ASL
import SemMC.ASL.Crucible
import SemMC.ASL.Translation
import SemMC.ASL.Translation.Preprocess
import SemMC.ASL.Signature
import SemMC.ASL.Types

instsFilePath :: FilePath
instsFilePath = "test/insts.parsed"

defsFilePath :: FilePath
defsFilePath = "test/defs.parsed"


collectInstructions :: [AS.Instruction] -> [(T.Text, T.Text)]
collectInstructions aslInsts =
  List.concat $ map (\(AS.Instruction nm encs _ _) ->
                       map (\(AS.InstructionEncoding {AS.encName=encName}) ->
                              (nm, encName)) encs) aslInsts
ignoredDefs :: [T.Text]
ignoredDefs = ["AArch64_BranchAddr_1"]

main :: IO ()
main = do
  (aslInsts, aslDefs) <- getASL
  putStrLn $ "Loaded " ++ show (length aslInsts) ++ " instructions and " ++ show (length aslDefs) ++ " definitions."
  let instrs = [collectInstructions aslInsts !! 4]
  --let instrs = [("aarch32_REV_A","aarch32_REV_T2_A")]
  --let instrs = [("aarch32_ADC_i_A","aarch32_ADC_i_T1_A")]
  
  MSS.evalStateT (forM_ instrs (\(instr, enc) -> runTranslation instr enc aslInsts aslDefs)) Map.empty
   

runTranslation :: T.Text -> T.Text -> [AS.Instruction] -> [AS.Definition] -> MSS.StateT SigMap IO ()
runTranslation instr enc aslInsts aslDefs = do
  MSS.lift $ putStrLn $ "Computing instruction signature for: " ++ show instr ++ " " ++ show enc
  case computeInstructionSignature instr enc aslInsts aslDefs of
    Left err -> do
      MSS.lift $ putStrLn $ "Error computing instruction signature: " ++ show err
      MSS.lift $ exitFailure
    Right (Some (SomeProcedureSignature iSig), instStmts, sigMap) -> do
      MSS.lift $ putStrLn $ "Instruction signature:"
      MSS.lift $ print iSig
      --Just mySig <- return $ Map.lookup "Unreachable_0" sigMap
      --sigMap <- return $ Map.fromList [("Unreachable_0", mySig)]

      case computeDefinitions (Map.keys sigMap) aslDefs of
        Left err -> do
          MSS.lift $ putStrLn $ "Error computing ASL definitions: " ++ show err
          MSS.lift $ exitFailure
        Right defs -> do
          MSS.lift $ putStrLn $ "Translating instruction: " ++ T.unpack instr ++ " " ++ T.unpack enc
          MSS.lift $ putStrLn $ (show iSig)
          deps <- MSS.lift $ processInstruction iSig instStmts defs

          MSS.lift $ putStrLn $ "--------------------------------"
          MSS.lift $ putStrLn "Translating functions: "
          mapM_ (translationLoop defs) (Map.assocs deps)
    _ -> error "Panic"

type SigMap = Map.Map T.Text (Some (SomeSignature))

translationLoop :: Definitions arch -> (T.Text, TypeEnvir) -> MSS.StateT SigMap IO ()
translationLoop defs (fnname, env) = do
  let finalName =  (mkFinalFunctionName env fnname)
  if List.elem finalName ignoredDefs
  then MSS.lift $ putStrLn $ "SKIPPING: " <> show finalName
  else do
    sigs <- MSS.get
    case Map.lookup finalName sigs of
      Just _ -> return ()
      _ -> do
         MSS.lift $ putStrLn $ show finalName ++ " definition:"
         case Map.lookup fnname (defSignatures defs) of
             Just (ssig, stmts) | Some sig <- mkSignature defs env ssig -> do
                   MSS.lift $ putStrLn $ "--------------------------------"
                   MSS.modify' $ Map.insert finalName (Some sig)
                   deps <- MSS.lift $ processFunction (someSigName sig) sig stmts defs
                   MSS.lift $ putStrLn $ "--------------------------------"
                   mapM_ (translationLoop defs) (Map.assocs deps)
             _ -> error $ "Missing definition for:" <> (show fnname)

-- Debugging function for determining what syntactic forms
-- actually exist
queryASL :: (T.Text -> AS.Expr -> b -> b) ->
            (T.Text -> AS.LValExpr -> b -> b) ->
            (T.Text -> AS.Stmt -> b -> b) -> b -> IO b
queryASL f h g b = do
  (aslInsts, aslDefs) <- getASL
  return $ foldASL f h g aslDefs aslInsts b

getASL :: IO ([AS.Instruction], [AS.Definition])
getASL = do
  eAslDefs <- AP.parseAslDefsFile defsFilePath
  eAslInsts <- AP.parseAslInstsFile instsFilePath
  case (eAslInsts, eAslDefs) of
    (Left err, _) -> do
      putStrLn $ "Error loading ASL instructions: " ++ show err
      exitFailure
    (_, Left err) -> do
      putStrLn $ "Error loading ASL definitions: " ++ show err
      exitFailure
    (Right aslInsts', Right aslDefs') -> do
      return $ prepASL (aslInsts', aslDefs')

processInstruction :: ProcedureSignature globals init -> [AS.Stmt] -> Definitions arch -> IO (Map.Map T.Text TypeEnvir)
processInstruction pSig stmts defs = do
  handleAllocator <- CFH.newHandleAllocator
  p <- procedureToCrucible defs pSig handleAllocator stmts
  backend <- CBS.newSimpleBackend globalNonceGenerator
  let cfg :: SimulatorConfig (CBS.SimpleBackend GlobalNonceGenerator (CBS.Flags CBS.FloatIEEE))
        = SimulatorConfig { simOutputHandle = IO.stdout
                          , simHandleAllocator = handleAllocator
                          , simSym = backend
                          }
  symFn <- simulateProcedure cfg p
  return (procDepends p)

processFunction :: T.Text -> SomeSignature ret -> [AS.Stmt] -> Definitions arch -> IO (Map.Map T.Text TypeEnvir)
processFunction fnName sig stmts defs =
  case sig of
    SomeFunctionSignature fSig -> do
      handleAllocator <- CFH.newHandleAllocator
      f <- functionToCrucible defs fSig handleAllocator stmts
      backend <- CBS.newSimpleBackend globalNonceGenerator
      let cfg :: SimulatorConfig (CBS.SimpleBackend GlobalNonceGenerator (CBS.Flags CBS.FloatIEEE))
            = SimulatorConfig { simOutputHandle = IO.stdout
                              , simHandleAllocator = handleAllocator
                              , simSym = backend
                              }
      symFn <- simulateFunction cfg f
      return (funcDepends f)
    SomeProcedureSignature pSig -> do
      handleAllocator <- CFH.newHandleAllocator
      p <- procedureToCrucible defs pSig handleAllocator stmts
      backend <- CBS.newSimpleBackend globalNonceGenerator
      let cfg :: SimulatorConfig (CBS.SimpleBackend GlobalNonceGenerator (CBS.Flags CBS.FloatIEEE))
            = SimulatorConfig { simOutputHandle = IO.stdout
                              , simHandleAllocator = handleAllocator
                              , simSym = backend
                              }
      symFn <- simulateProcedure cfg p
      return (procDepends p)
