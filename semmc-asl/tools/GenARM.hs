{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main ( main ) where

import qualified Control.Exception as X
import           Control.Monad (forM_, foldM)
import qualified Control.Monad.State.Lazy as MSS
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Except as E
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe ( fromMaybe, catMaybes )
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
import Control.Concurrent
import Control.Concurrent.MVar

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
  

collectInstructions :: [AS.Instruction] -> [(AS.Instruction, T.Text, AS.InstructionSet)]
collectInstructions aslInsts = do
  List.concat $
    map (\instr -> map (\(AS.InstructionEncoding {AS.encName=encName, AS.encInstrSet=iset}) ->
                          (instr, encName, iset)) (AS.instEncodings instr)) aslInsts

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
  sm <- parTranslateAll defaultOptions 200
  reportStats sm

testDefinition :: String -> Int -> IO ()
testDefinition s startidx = do
  let nm = T.pack s
  let opts = defaultOptions 
  sig <- runWithFilters (opts  { optFilters = translateOnlyFun nm } )
  case Map.lookup nm (sMap sig) of
    Just _ -> return ()
    Nothing -> error $ "Translation failed to discover function: " <> show nm

testInstruction :: String -> String -> IO ()
testInstruction instr enc = do
  _ <- runWithFilters (defaultOptions { optFilters = translateOnlyInstr (T.pack instr, T.pack enc) })
  return ()

translateAll :: IO (SigMap)
translateAll = do
  runWithFilters defaultOptions


parTranslateAll :: TranslatorOptions -> Int -> IO (SigMap)
parTranslateAll opts chunksz = do
  (aslInsts, aslDefs) <- getASL
  let allInstrs = imap (\i -> \nm -> (i,nm)) $ collectInstructions aslInsts
  let (nchunks, _) = length allInstrs `divMod` chunksz

  let runFun chunk = do
        let opts' = opts { optVerbose = False,
                           optStartIndex = (chunk * chunksz),
                           optNumberOfInstructions = Just chunksz } 
        runWithFilters' opts' aslInsts aslDefs
        
  mvs <- mapM (\chunk -> doFork (runFun chunk)) [0 .. nchunks]
  sms <- mapM collapseResult mvs
  let opts' = opts { optStartIndex = (nchunks * chunksz), optNumberOfInstructions = Nothing }
  sm <- runWithFilters' opts' aslInsts aslDefs
  E.when (optVerbose opts) $ mapM_ (putStrLn . T.unpack) (reverse $ sLog sm)
  let sm' = mergeInstrExceptions (sm : sms)
  return sm'

  where
    collapseResult mv = do
      result <- takeMVar mv
      case result of
        Left e -> X.throw e
        Right sm -> do
          E.when (optVerbose opts) $ mapM_ (putStrLn . T.unpack) (reverse $ sLog sm)
          return sm         
    
    doFork f = do
      mv <- newEmptyMVar
      _ <- forkFinally f (\r -> putMVar mv r)
      return mv
      

mergeInstrExceptions :: [SigMap] -> SigMap
mergeInstrExceptions sms@(s : _) =
  SigMap { instrExcepts = Map.unions (map instrExcepts sms)
         , funExcepts = Map.unions (map funExcepts sms)
         , instrDeps = Map.unions (map instrDeps sms)
         , funDeps = Map.unions (map funDeps sms)
         , sMap = Map.unions (map sMap sms)
         , sigState = sigState s
         , sigEnv = sigEnv s
         }

 

filterOnlyTranslate :: Filters
filterOnlyTranslate = Filters
  (\_ -> True)
  (\_ -> True)
  (\_ -> False)
  (\_ -> False)
    
filterStalls :: Filters
filterStalls = Filters
  (\_ -> True)
  (\_ -> True)
  (\nm -> not $ List.elem nm stalledDefs)
  (\_ -> True)

filterIgnored :: Filters
filterIgnored = Filters
  (\nm -> not $ List.elem nm ignoredDefs)
  (\(InstructionIdent nm enc _) -> not $ List.elem (nm, enc) ignoredInstrs)
  (\_ -> True) (\_ -> True)

translateOnlyFun :: T.Text -> Filters
translateOnlyFun fnm = Filters
  (\nm -> (nm == fnm) || (not $ List.elem nm ignoredDefs))
  (\(InstructionIdent nm enc _) -> not $ List.elem (nm, enc) ignoredInstrs)
  (\nm -> nm == fnm) (\_ -> False)

translateOnlyInstr :: (T.Text, T.Text) -> Filters
translateOnlyInstr fnm = Filters
  (\nm -> False)
  (\(InstructionIdent nm enc _) -> (nm, enc) == fnm)
  (\nm -> False) (\(InstructionIdent nm enc _) -> (nm, enc) == fnm)

logMsg :: String -> MSS.StateT SigMap IO ()
logMsg msg = do
  MSS.modify' $ \s -> s { sLog = T.pack msg : sLog s }
  verbose <- MSS.gets (optVerbose . sOptions)
  E.when verbose $ MSS.lift $ putStrLn msg
  

isFunFilteredOut :: T.Text -> MSS.StateT SigMap IO Bool
isFunFilteredOut fnm = do
  test <- MSS.gets (funFilter . optFilters . sOptions)
  return $ not $ test fnm

isInstrFilteredOut :: InstructionIdent -> MSS.StateT SigMap IO Bool
isInstrFilteredOut inm = do
  test <- MSS.gets (instrFilter . optFilters . sOptions)
  return $ not $ test inm

isFunTransFilteredOut :: T.Text -> MSS.StateT SigMap IO Bool
isFunTransFilteredOut fnm = do
  test <- MSS.gets (funTranslationFilter . optFilters . sOptions)
  skipTranslation <- MSS.gets (optSkipTranslation . sOptions)
  return $ (not $ test fnm) || skipTranslation

isInstrTransFilteredOut :: InstructionIdent -> MSS.StateT SigMap IO Bool
isInstrTransFilteredOut inm = do
  test <- MSS.gets (instrTranslationFilter . optFilters . sOptions)
  skipTranslation <- MSS.gets (optSkipTranslation . sOptions)
  return $ (not $ test inm) || skipTranslation

runWithFilters' :: TranslatorOptions
                -> [AS.Instruction]
                -> [AS.Definition]
                -> IO (SigMap)
runWithFilters' opts aslInsts aslDefs = do
  let startidx = optStartIndex opts
  let numInstrs = optNumberOfInstructions opts
  let allInstrs = imap (\i -> \nm -> (i,nm)) $ collectInstructions aslInsts
  let (sigEnv, sigState) = buildSigState aslDefs
  let instrs = case numInstrs of {Just i -> take i (drop startidx allInstrs); _ -> drop startidx allInstrs}
  sm <- MSS.execStateT (forM_ instrs (\(i, (instr, enc, iset)) -> do
    let ident = instrToIdent instr enc iset
    filteredOut <- isInstrFilteredOut ident
    if filteredOut then return ()
    else do
     logMsg $ "Processing instruction: " ++ show i ++ "/" ++ show (length allInstrs)
     runTranslation instr ident))
    (SigMap Map.empty Map.empty Map.empty sigState sigEnv Map.empty Map.empty [] opts)
  return sm



runWithFilters :: TranslatorOptions -> IO (SigMap)
runWithFilters opts = do
  (aslInsts, aslDefs) <- getASL
  putStrLn $ "Loaded " ++ show (length aslInsts) ++ " instructions and " ++ show (length aslDefs) ++ " definitions."
  runWithFilters' opts aslInsts aslDefs
                                                  

reportStats :: SigMap -> IO ()
reportStats sm = do
  putStrLn $ "Failing instructions:"
  _ <- Map.traverseWithKey (\ident -> \deps ->
    case Map.lookup ident (instrExcepts sm) of
      Just e -> do
       putStrLn $ show ident ++ " failed to translate:"
       putStrLn $ show e
      Nothing -> case Map.lookup ident (instrDeps sm) of
        Just deps -> do
          let errs = catMaybes $ (\dep -> (\x -> (dep,x)) <$> Map.lookup dep (funExcepts sm)) <$> (Set.toList deps)
          if null errs then return ()
          else do
            putStrLn $ show ident ++ " has failing dependencies:"
            mapM_ (\(dep, err) -> putStrLn $ show dep <> ":" <> show err) errs
        _ -> return ()) (instrDeps sm)
  putStrLn $ "Total instructions inspected: \n" <> show (Map.size $ instrDeps sm)
  putStrLn $ "Number of instructions which raised exceptions: " <> show (Map.size $ instrDeps sm)
  putStrLn $ "Instructions with no errors in any dependent functions:"
  _ <- Map.traverseWithKey (\ident -> \deps -> do
    if not (Map.member ident (instrExcepts sm)) &&
       Set.null (Set.filter (\dep -> Map.member dep (funExcepts sm)) deps) 
    then putStrLn $ show ident
    else return ()) (instrDeps sm)
  return ()
  
data TranslatorException =
    TExcept TranslationException
  | SExcept SigException
  | SomeExcept X.SomeException

deriving instance Show TranslatorException
instance X.Exception TranslatorException

data InstructionIdent =
  InstructionIdent { iName :: T.Text, iEnc :: T.Text, iSet :: AS.InstructionSet }
  deriving (Eq, Show)

deriving instance Ord AS.InstructionSet
deriving instance Ord InstructionIdent

instrToIdent :: AS.Instruction -> T.Text -> AS.InstructionSet -> InstructionIdent
instrToIdent AS.Instruction{..} enc iset = InstructionIdent instName enc iset


runTranslation :: AS.Instruction -> InstructionIdent -> MSS.StateT SigMap IO ()
runTranslation instruction@AS.Instruction{..} instrIdent = do
  logMsg $ "Computing instruction signature for: " ++ show instrIdent
  result <- liftSigM (KeyInstr instrIdent) $
    computeInstructionSignature' instruction (iEnc instrIdent) (iSet instrIdent)
  case result of
    Left err -> do
      logMsg $ "Error computing instruction signature: " ++ show err
    Right (Some (SomeProcedureSignature iSig), instStmts) -> do
      defs <- liftSigM (KeyInstr instrIdent) $ getDefinitions
      case defs of
        Left err -> do
          logMsg $ "Error computing ASL definitions: " ++ show err
        Right defs -> do
          logMsg $ "Translating instruction: " ++ show instrIdent
          logMsg $ (show iSig)
          deps <- processInstruction instrIdent iSig instStmts defs

          logMsg $ "--------------------------------"
          logMsg "Translating functions: "
          alldeps <- mapM (translationLoop defs) (Map.assocs deps)
          let alldepsSet = Set.union (Set.unions alldeps) (Map.keysSet deps)
          MSS.modify' $ \s -> s { instrDeps = Map.insert instrIdent alldepsSet (instrDeps s) }
    _ -> error "Panic"

data Filters = Filters { funFilter :: T.Text -> Bool
                       , instrFilter :: InstructionIdent -> Bool
                       , funTranslationFilter :: T.Text -> Bool
                       , instrTranslationFilter :: InstructionIdent -> Bool
                       }

data ElemKey =
   KeyInstr InstructionIdent
 | KeyFun T.Text

data TranslatorOptions = TranslatorOptions
  { optVerbose :: Bool
  , optStartIndex :: Int
  , optNumberOfInstructions :: Maybe Int
  , optFilters :: Filters
  , optSkipTranslation :: Bool
  }

defaultOptions :: TranslatorOptions
defaultOptions = TranslatorOptions
  { optVerbose = True
  , optStartIndex = 0
  , optNumberOfInstructions = Nothing
  , optFilters = filterStalls -- avoid known bad functions
  , optSkipTranslation = True
  }

-- FIXME: Seperate this into RWS
data SigMap = SigMap { sMap :: Map.Map T.Text (Some (SomeSignature))
                     , instrExcepts :: Map.Map InstructionIdent TranslatorException
                     , funExcepts :: Map.Map T.Text TranslatorException                    
                     , sigState :: SigState
                     , sigEnv :: SigEnv
                     , instrDeps :: Map.Map InstructionIdent (Set.Set T.Text)
                     , funDeps :: Map.Map T.Text (Set.Set T.Text)
                     , sLog :: [T.Text]
                     , sOptions :: TranslatorOptions
                     }
              
              
liftSigM :: ElemKey -> SigM ext f a -> MSS.StateT SigMap IO (Either SigException a)
liftSigM k f = do
  state <- MSS.gets sigState
  env <- MSS.gets sigEnv
  let (result, state' ) = runSigM env state f
  case result of
    Right a -> do
      MSS.modify' $ \s -> s { sigState = state' }
      return $ Right a
    Left err -> do
      collectExcept k (SExcept err)
      return $ Left err

collectExcept :: ElemKey -> TranslatorException -> MSS.StateT SigMap IO ()
collectExcept k e = case k of
  KeyInstr ident -> MSS.modify' $ \s -> s { instrExcepts = Map.insert ident e (instrExcepts s) }
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
  filteredOut <- isFunFilteredOut finalName
  if filteredOut
  then return Set.empty
  else do
    fdeps <- MSS.gets funDeps
    case Map.lookup finalName fdeps of
      Just deps -> return deps
      _ -> do
         logMsg $ show finalName ++ " definition:"
         case Map.lookup fnname (defSignatures defs) of
             Just (ssig, stmts) | Some sig <- mkSignature defs env ssig -> do
                   logMsg $ "--------------------------------"
                   MSS.modify' $ \s -> s { sMap = Map.insert finalName (Some sig) (sMap s) }
                   deps <- processFunction finalName sig stmts defs
                   logMsg $ "--------------------------------"
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

processInstruction :: InstructionIdent
                   -> ProcedureSignature globals init
                   -> [AS.Stmt] -> Definitions arch
                   -> MSS.StateT SigMap IO (Map.Map T.Text TypeEnvir)
processInstruction instr pSig stmts defs = do
  handleAllocator <- CFH.newHandleAllocator
  mp <- catchIO (KeyInstr instr) $ procedureToCrucible defs pSig handleAllocator stmts
  case mp of
    Just p -> do
      filteredOut <- isInstrTransFilteredOut instr
      if filteredOut
      then return (procDepends p)
      else do
        mdep <- catchIO (KeyInstr instr) $
          CBO.withYicesOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
            let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                      , simHandleAllocator = handleAllocator
                                      , simSym = backend
                                      }
            symFn <- simulateProcedure cfg p
            return (procDepends p)
        return $ fromMaybe Map.empty mdep
    Nothing -> return Map.empty

processFunction :: T.Text
                -> SomeSignature ret
                -> [AS.Stmt]
                -> Definitions arch
                -> MSS.StateT SigMap IO (Map.Map T.Text TypeEnvir)
processFunction fnName sig stmts defs =
  case sig of
    SomeFunctionSignature fSig -> do
      handleAllocator <- CFH.newHandleAllocator
      mf <- catchIO (KeyFun fnName) $ functionToCrucible defs fSig handleAllocator stmts
      case mf of
        Just f -> do
          filteredOut <- isFunTransFilteredOut fnName
          if filteredOut
          then return (funcDepends f)
          else do
            mdep <- catchIO (KeyFun fnName) $
              CBO.withYicesOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
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
          filteredOut <- isFunTransFilteredOut fnName
          if filteredOut
          then return (procDepends p)
          else do
            mdep <- catchIO (KeyFun fnName) $
              CBO.withYicesOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
                let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                        , simHandleAllocator = handleAllocator
                                        , simSym = backend
                                        }
                symFn <- simulateProcedure cfg p
                return (procDepends p)
            return $ fromMaybe Map.empty mdep
        Nothing -> return Map.empty
