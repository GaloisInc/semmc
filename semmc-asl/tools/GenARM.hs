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

ignoredInstrs :: [(T.Text, T.Text)]
ignoredInstrs =
  [("aarch32_VADDL_A", "aarch32_VADDL_T1A1_A") -- unexpected getter structure
  ,("aarch32_QSUB16_A", "aarch32_QSUB16_A1_A") -- "R" unbound
  ,("aarch32_QSUB16_A", "aarch32_QSUB16_T1_A") -- "R" unbound
  ,("aarch32_SHA256H2_A", "aarch32_SHA256H2_A1_A") -- unexpected getter structure
  ,("aarch32_SHA256H2_A","aarch32_SHA256H2_T1_A") -- unexpected getter structure
  ,("aarch32_SEL_A", "aarch32_SEL_A1_A") -- "R" unbound
  ,("aarch32_SEL_A", "aarch32_SEL_T1_A") -- "R" unbound
  ,("aarch32_SMLAWB_A", "aarch32_SMLAWB_A1_A") -- type mismatch
  ,("aarch32_SMLAWB_A", "aarch32_SMLAWB_T1_A") -- type mismatch
  ,("aarch32_ERET_AS", "aarch32_ERET_A1_A") -- unbound global
  ,("aarch32_ERET_AS", "aarch32_ERET_T1_A") -- unbound global
  ,("aarch32_VPADD_f_A", "aarch32_VPADD_f_A1_A") -- unexpected getter structure
  ,("aarch32_VPADD_f_A", "aarch32_VPADD_f_T1_A") -- unexpected getter structure
  ,("aarch32_VSTM_A", "aarch32_VSTM_T1A1_A") -- unexpected getter structure
  ,("aarch32_VCMP_A", "aarch32_VCMP_A1_A") -- unexpected getter structure
  ] 
  

collectInstructions :: [AS.Instruction] -> [(T.Text, T.Text)]
collectInstructions aslInsts = do
  let l = List.concat $ map (\(AS.Instruction nm encs _ _) ->
                          map (\(AS.InstructionEncoding {AS.encName=encName}) ->
                                 (nm, encName)) encs) aslInsts
  [ x | x <- l, not (List.elem x ignoredInstrs)]


ignoredDefs :: [T.Text]
ignoredDefs =
  ["AArch64_BranchAddr_1",
   "AArch32_CheckBreakpoint_2", -- stalls
   "AArch32_VCRMatch_1", -- bad slice
   "AArch32_CheckWatchpoint_4", -- stalls
   "AArch32_CheckDomain_5", -- bad slice
   "AArch32_ExecutingATS1xPInstr_0", -- SliceOffset
   "AArch32_ExecutingLSMInstr_0", -- SliceOffset
   "AArch32_TranslationTableWalkLD_7", -- bad slice
   "AArch32_TranslationTableWalkSD_4", -- bad slice
   "AArch64_CheckAndUpdateDescriptor_8", -- SSA conversion failure
   "AArch64_TranslationTableWalk_7", -- type mismatch
   "AArch64_CheckBreakpoint_2", -- stalls
   "AArch64_CheckWatchpoint_4", -- stalls
   "AArch64_ExecutingATS1xPInstr_0", -- bad slice
   "AArch64_TranslateAddressS1Off_3", -- bad slice
   "ProcessorID_0", -- simulation abort
   "Shift_C_4N_32", -- bad extended type information
   "LSL_2N_32", -- bad extended type information
   "LSR_2N_32", -- bad extended type information
   "AArch32_IsExclusiveVA_3", -- simulation abort
   "IsExclusiveGlobal_3", -- simulation abort
   "IsExclusiveLocal_3" -- simulation abort
  ]

testInstrs :: [(T.Text, T.Text)]
testInstrs =
  [("aarch32_STRH_r_A", "aarch32_STRH_r_T1_A"),
   ("aarch32_REV_A","aarch32_REV_T2_A"),
   ("aarch32_ADC_i_A","aarch32_ADC_i_T1_A")]

main :: IO ()
main = do
  (aslInsts, aslDefs) <- getASL
  putStrLn $ "Loaded " ++ show (length aslInsts) ++ " instructions and " ++ show (length aslDefs) ++ " definitions."
  let instrs = collectInstructions aslInsts
  --let instrs = testInstrs
  
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
