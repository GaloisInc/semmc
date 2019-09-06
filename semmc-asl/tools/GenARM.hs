{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import qualified Control.Exception as X
import           Control.Monad (forM_, foldM)
import qualified Control.Monad.State.Lazy as MSS
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Except as E
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.List as List
import           Data.List.Index (imap)
import qualified Lang.Crucible.Backend.Simple as CBS
import qualified Lang.Crucible.Backend.Online as CBO
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
  ,("aarch32_QSUB16_A", "aarch32_QSUB16_A1_A") -- can't slice lval for setters
  ,("aarch32_QSUB16_A", "aarch32_QSUB16_T1_A") -- can't slice lval for setters
  ,("aarch32_SEL_A", "aarch32_SEL_A1_A") -- can't slice lval for setters
  ,("aarch32_SEL_A", "aarch32_SEL_T1_A") -- can't slice lval for setters
  ,("aarch32_SMLAWB_A", "aarch32_SMLAWB_A1_A") -- type mismatch
  ,("aarch32_SMLAWB_A", "aarch32_SMLAWB_T1_A") -- type mismatch
  ,("aarch32_ERET_AS", "aarch32_ERET_A1_A") -- unbound global
  ,("aarch32_ERET_AS", "aarch32_ERET_T1_A") -- unbound global
  ,("aarch32_VPADD_f_A", "aarch32_VPADD_f_A1_A") -- TypeNotFound "real"
  ,("aarch32_VCMP_A", "aarch32_VCMP_A2_A") -- TypeNotFound "real"
  ,("aarch32_VCMP_A", "aarch32_VCMP_T1_A") -- TypeNotFound "real"
  ,("aarch32_VCMP_A", "aarch32_VCMP_T2_A") -- TypeNotFound "real"
  ,("aarch32_VRINTX_asimd_A", "aarch32_VRINTX_asimd_A1_A") -- TypeNotFound "real"
  ,("aarch32_VRINTX_asimd_A", "aarch32_VRINTX_asimd_T1_A") -- TypeNotFound "real"
  ,("aarch32_VLDR_A", "aarch32_VLDR_A1_A") -- missing "HaveFP16Ext_0"
  ,("aarch32_VLDR_A", "aarch32_VLDR_T1_A") -- missing "HaveFP16Ext_0"
  ,("aarch32_VORN_r_A", "aarch32_VORN_r_T1A1_A") -- implicit integer cast
  ,("aarch32_VQDMLAL_A", "aarch32_VQDMLAL_T1A1_A") -- implicit integer cast
  ,("aarch32_VQDMLAL_A", "aarch32_VQDMLAL_T2A2_A") -- can't monomorphize Elem
  ,("aarch32_SSUB8_A", "aarch32_SSUB8_A1_A") -- can't slice lval for setters
  ,("aarch32_SSUB8_A", "aarch32_SSUB8_T1_A") -- can't slice lval for setters
  ,("aarch32_SASX_A", "aarch32_SASX_A1_A") -- can't slice lval for setters
  ,("aarch32_SASX_A", "aarch32_SASX_T1_A") -- can't slice lval for setters
  ,("aarch32_STRHT_A", "aarch32_STRHT_A1_A") --  encoding is missing "m" field
  ,("aarch32_STRHT_A", "aarch32_STRHT_A2_A") --  encoding is missing "imm32" field
  ,("aarch32_STRHT_A", "aarch32_STRHT_T1_A") --  encoding is missing "m" field
  ,("aarch32_VRINTA_vfp_A", "aarch32_VRINTA_vfp_A1_A") -- TypeNotFound "real"
  ,("aarch32_VRINTA_vfp_A", "aarch32_VRINTA_vfp_T1_A") -- TypeNotFound "real"
  ,("aarch32_CPS_AS", "aarch32_CPS_T1_AS") -- encoding is missing "mode" field
  ,("aarch32_VMAXNM_A", "aarch32_VMAXNM_A1_A") -- TypeNotFound "real"
  ,("aarch32_VMAXNM_A", "aarch32_VMAXNM_A2_A") -- TypeNotFound "real"
  ,("aarch32_VMAXNM_A", "aarch32_VMAXNM_T1_A") -- TypeNotFound "real"
  ,("aarch32_VSTM_A", "aarch32_VSTM_T1A1_A") -- division not supported
  ,("aarch32_VMAXNM_A", "aarch32_VMAXNM_T2_A") -- TypeNotFound "real"
  ,("aarch32_VPMAX_f_A", "aarch32_VPMAX_f_A1_A") -- TypeNotFound "real"
  ] 
  

collectInstructions :: [AS.Instruction] -> [(AS.Instruction, T.Text)]
collectInstructions aslInsts = do
  List.concat $
    map (\instr -> map (\(AS.InstructionEncoding {AS.encName=encName}) ->
                          (instr, encName)) (AS.instEncodings instr)) aslInsts

ignoredDefs :: [T.Text]
ignoredDefs =
  ["AArch64_BranchAddr_1", -- bad slice
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
  ,"AArch32_WatchpointMatch_5" -- global array
  ,"AArch32_CheckAdvSIMDOrFPEnabled_2" -- bitmask pattern unimplemented
  ,"GETTER_D_1" -- division unimplemented, slicing arithmetic
  ,"SETTER_D_2" -- division unimplemented, slicing arithmetic
  ]

stalledDefs :: [T.Text]
stalledDefs =
  ["AArch32_CheckBreakpoint_2"
  ,"AArch32_CheckWatchpoint_4"
  ,"AArch64_CheckBreakpoint_2"
  ,"AArch64_CheckWatchpoint_4"
  ]

testInstrs :: [(T.Text, T.Text)]
testInstrs =
  [("aarch32_STRH_r_A", "aarch32_STRH_r_T1_A"),
   ("aarch32_REV_A","aarch32_REV_T2_A"),
   ("aarch32_ADC_i_A","aarch32_ADC_i_T1_A")]

main :: IO ()
main = do
  _ <- runWithFilters filterStalls 0
  return ()

testDefinition :: String -> Int -> IO ()
testDefinition s startidx = do
  let nm = T.pack s
  sig <- runWithFilters (translateOnlyFun nm) startidx
  case Map.lookup nm (sMap sig) of
    Just _ -> return ()
    Nothing -> error $ "Translation failed to discover function: " <> show nm

testInstruction :: String -> String -> IO ()
testInstruction instr enc = do
  _ <- runWithFilters (translateOnlyInstr (T.pack instr, T.pack enc)) 0
  return ()

translateAll :: Int -> IO ()
translateAll startidx = do
  _ <- runWithFilters filterOnlyTranslate startidx
  return ()

filterOnlyTranslate :: Filters
filterOnlyTranslate = Filters
  (\_ -> True)
  (\_ -> True)
  (\_ -> False)
  (\_ -> False)
    
filterStalls :: Filters
filterStalls = Filters
  (\nm -> not $ List.elem nm stalledDefs)
  (\_ -> True)
  (\_ -> True)
  (\_ -> True)

filterIgnored :: Filters
filterIgnored = Filters
  (\nm -> not $ List.elem nm ignoredDefs)
  (\nm -> not $ List.elem nm ignoredInstrs)
  (\_ -> True) (\_ -> True)

translateOnlyFun :: T.Text -> Filters
translateOnlyFun fnm = Filters
  (\nm -> (nm == fnm) || (not $ List.elem nm ignoredDefs))
  (\nm -> not $ List.elem nm ignoredInstrs)
  (\nm -> nm == fnm) (\_ -> False)

translateOnlyInstr :: (T.Text, T.Text) -> Filters
translateOnlyInstr fnm = Filters
  (\nm -> False)
  (\nm -> (nm == fnm))
  (\nm -> False) (\nm -> nm == fnm)

runWithFilters :: Filters -> Int -> IO (SigMap)
runWithFilters filters startidx = do
  (aslInsts, aslDefs) <- getASL
  putStrLn $ "Loaded " ++ show (length aslInsts) ++ " instructions and " ++ show (length aslDefs) ++ " definitions."
  let instrs = imap (\i -> \nm -> (i,nm)) $ collectInstructions aslInsts
  let (sigEnv, sigState) = buildSigState aslDefs
  sm <- MSS.execStateT (forM_ (drop startidx $ instrs) (\(i, (instr, enc)) -> do
                                  MSS.lift $ putStrLn $ "Processing instruction: " ++ show i ++
                                                         "/" ++ show (length instrs)
                                  runTranslation instr enc))
    (SigMap Map.empty Map.empty Map.empty filters sigState sigEnv Map.empty Map.empty)
  reportStats sm
  return sm

reportStats :: SigMap -> IO ()
reportStats sm = do
  putStrLn $ "Number of instructions which raised exceptions: \n" <> show (length $ Map.keys $ instrExcepts sm)
  putStrLn $ "Instructions with no errors in any dependent functions:"
  _ <- Map.traverseWithKey (\(instr, enc) -> \deps -> do
    if not (Map.member (instr, enc) (instrExcepts sm)) &&
       Set.null (Set.filter (\dep -> Map.member dep (funExcepts sm)) deps) 
    then putStrLn $ show instr ++ " " ++ show enc
    else return ()) (instrDeps sm)
  return ()

  
data TranslatorException =
    TExcept TranslationException
  | SExcept SigException
  | SomeExcept X.SomeException
  
deriving instance Show TranslatorException
instance X.Exception TranslatorException

runTranslation :: AS.Instruction -> T.Text -> MSS.StateT SigMap IO ()
runTranslation instruction@AS.Instruction{..} enc = do
  let instr = instName
  test <- MSS.gets (instrFilter . sFilters)
  if not $ test (instr, enc)
  then MSS.lift $ putStrLn $ "SKIPPING instruction: " ++ show instr ++ " " ++ show enc
  else do
    MSS.lift $ putStrLn $ "Computing instruction signature for: " ++ show instr ++ " " ++ show enc
    result <- liftSigM (KeyInstr instr enc) $ computeInstructionSignature' instruction enc
    case result of
      Left err -> do
        MSS.lift $ putStrLn $ "Error computing instruction signature: " ++ show err
      Right (Some (SomeProcedureSignature iSig), instStmts) -> do
        MSS.lift $ putStrLn $ "Instruction signature:"
        MSS.lift $ print iSig
        defs <- liftSigM (KeyInstr instr enc) $ getDefinitions
        case defs of
          Left err -> do
            MSS.lift $ putStrLn $ "Error computing ASL definitions: " ++ show err
          Right defs -> do
            MSS.lift $ putStrLn $ "Translating instruction: " ++ T.unpack instr ++ " " ++ T.unpack enc
            MSS.lift $ putStrLn $ (show iSig)
            deps <- processInstruction instr enc iSig instStmts defs

            MSS.lift $ putStrLn $ "--------------------------------"
            MSS.lift $ putStrLn "Translating functions: "
            alldeps <- mapM (translationLoop defs) (Map.assocs deps)
            let alldepsSet = Set.union (Set.unions alldeps) (Map.keysSet deps)
            MSS.modify' $ \s -> s { instrDeps = Map.insert (instr, enc) alldepsSet (instrDeps s) }
      _ -> error "Panic"

data Filters = Filters { funFilter :: T.Text -> Bool
                       , instrFilter :: (T.Text, T.Text) -> Bool
                       , funTranslationFilter :: T.Text -> Bool
                       , instrTranslationFilter :: (T.Text, T.Text) -> Bool
                       }

data ElemKey =
   KeyInstr T.Text T.Text
 | KeyFun T.Text


data SigMap = SigMap { sMap :: Map.Map T.Text (Some (SomeSignature))
                     , instrExcepts :: Map.Map (T.Text, T.Text) TranslatorException
                     , funExcepts :: Map.Map T.Text TranslatorException
                     , sFilters :: Filters
                     , sigState :: SigState
                     , sigEnv :: SigEnv
                     , instrDeps :: Map.Map (T.Text, T.Text) (Set.Set T.Text)
                     , funDeps :: Map.Map T.Text (Set.Set T.Text)
                     }
              
liftSigM :: ElemKey -> SigM ext f a -> MSS.StateT SigMap IO (Either SigException a)
liftSigM k f = do
  state <- MSS.gets sigState
  env <- MSS.gets sigEnv
  let (result, state') = runSigM env state f
  MSS.modify' $ \s -> s { sigState = state' }
  case result of
    Right a -> return $ Right a
    Left err -> do
      collectExcept k (SExcept err)
      return $ Left err

collectExcept :: ElemKey -> TranslatorException -> MSS.StateT SigMap IO ()
collectExcept k e = case k of
  KeyInstr instr enc -> MSS.modify' $ \s -> s { instrExcepts = Map.insert (instr,enc) e (instrExcepts s) }
  KeyFun fun -> MSS.modify' $ \s -> s { funExcepts = Map.insert fun e (funExcepts s) }

catchIO :: ElemKey -> IO a -> MSS.StateT SigMap IO (Maybe a)
catchIO k f = do
  a <- MSS.lift ((Left <$> f)
                  `X.catch` (\(e :: TranslationException) -> return $ Right (TExcept e))
                  `X.catch` (\(e :: X.SomeException) -> return $ Right (SomeExcept e)))
  case a of
    Left r -> return (Just r)
    Right err -> (\_ -> Nothing) <$> collectExcept k err
  

translationLoop :: Definitions arch -> (T.Text, TypeEnvir) -> MSS.StateT SigMap IO (Set.Set T.Text)
translationLoop defs (fnname, env) = do
  let finalName = (mkFinalFunctionName env fnname)
  test <- MSS.gets (funFilter . sFilters)
  if not $ test finalName
  then do
    MSS.lift $ putStrLn $ "SKIPPING definition: " <> show finalName
    return Set.empty
  else do
    fdeps <- MSS.gets funDeps
    case Map.lookup finalName fdeps of
      Just deps -> return deps
      _ -> do
         MSS.lift $ putStrLn $ show finalName ++ " definition:"
         case Map.lookup fnname (defSignatures defs) of
             Just (ssig, stmts) | Some sig <- mkSignature defs env ssig -> do
                   MSS.lift $ putStrLn $ "--------------------------------"
                   MSS.modify' $ \s -> s { sMap = Map.insert finalName (Some sig) (sMap s) }
                   deps <- processFunction finalName sig stmts defs
                   MSS.lift $ putStrLn $ "--------------------------------"
                   MSS.modify' $ \s -> s { funDeps = Map.insert finalName Set.empty (funDeps s) }
                   alldeps <- mapM (translationLoop defs) (Map.assocs deps)
                   let alldepsSet = Set.union (Set.unions alldeps) (Map.keysSet deps)
                   MSS.modify' $ \s -> s { funDeps = Map.insert finalName alldepsSet (funDeps s) }
                   return alldepsSet
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

processInstruction :: T.Text
                   -> T.Text
                   -> ProcedureSignature globals init
                   -> [AS.Stmt] -> Definitions arch
                   -> MSS.StateT SigMap IO (Map.Map T.Text TypeEnvir)
processInstruction instr enc pSig stmts defs = do
  handleAllocator <- CFH.newHandleAllocator
  mp <- catchIO (KeyInstr instr enc) $ procedureToCrucible defs pSig handleAllocator stmts
  case mp of
    Just p -> do
      test <- MSS.gets (instrTranslationFilter . sFilters)
      if not $ test (instr, enc)
      then do
        MSS.lift $ putStrLn $ "SKIPPING translation for instruction: " ++ show instr ++ " " ++ show enc
        return (procDepends p)
      else do
        mdep <- catchIO (KeyInstr instr enc) $ CBO.withYicesOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
          let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                  , simHandleAllocator = handleAllocator
                                  , simSym = backend
                                  }
          symFn <- simulateProcedure cfg p
          return (procDepends p)
        return $ fromMaybe Map.empty mdep
    Nothing -> return Map.empty

processFunction :: T.Text -> SomeSignature ret -> [AS.Stmt] -> Definitions arch -> MSS.StateT SigMap IO (Map.Map T.Text TypeEnvir)
processFunction fnName sig stmts defs =
  case sig of
    SomeFunctionSignature fSig -> do
      handleAllocator <- CFH.newHandleAllocator
      mf <- catchIO (KeyFun fnName) $ functionToCrucible defs fSig handleAllocator stmts
      case mf of
        Just f -> do
          test <- MSS.gets (funTranslationFilter . sFilters)
          if not $ test fnName
          then do
            MSS.lift $ putStrLn $ "SKIPPING translation for definition: " <> show fnName
            return (funcDepends f)
          else do
            mdep <- catchIO (KeyFun fnName) $ CBO.withYicesOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
              let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                      , simHandleAllocator = handleAllocator
                                      , simSym = backend
                                      }
              symFn <- simulateFunction cfg f
              return (funcDepends f)
            return $ fromMaybe Map.empty mdep
        Nothing -> return Map.empty
    SomeProcedureSignature pSig -> do
      handleAllocator <- CFH.newHandleAllocator
      mp <- catchIO (KeyFun fnName) $ procedureToCrucible defs pSig handleAllocator stmts
      case mp of
        Just p -> do
          test <- MSS.gets (funTranslationFilter . sFilters)
          if not $ test fnName
          then do
            MSS.lift $ putStrLn $ "SKIPPING translation for definition: " <> show fnName
            return (procDepends p)
          else do
            mdep <- catchIO (KeyFun fnName) $ CBO.withYicesOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
              let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                      , simHandleAllocator = handleAllocator
                                      , simSym = backend
                                      }
              symFn <- simulateProcedure cfg p
              return (procDepends p)
            return $ fromMaybe Map.empty mdep
        Nothing -> return Map.empty
