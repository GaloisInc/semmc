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
import           Data.Maybe ( fromMaybe, catMaybes, listToMaybe )
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
import SemMC.ASL.Exceptions

import Control.Concurrent
import Control.Concurrent.MVar

import qualified SemMC.ASL.SyntaxTraverse as ASLT

instsFilePath :: FilePath
instsFilePath = "test/insts.parsed"

defsFilePath :: FilePath
defsFilePath = "test/defs.parsed"

regsFilePath :: FilePath
regsFilePath = "test/regs.parsed"

supportFilePath :: FilePath
supportFilePath = "test/support.parsed"

extraDefsFilePath :: FilePath
extraDefsFilePath = "test/extradefs.parsed"

collectInstructions :: [AS.Instruction] -> [(AS.Instruction, T.Text, AS.InstructionSet)]
collectInstructions aslInsts = do
  List.concat $
    map (\instr -> map (\(AS.InstructionEncoding {AS.encName=encName, AS.encInstrSet=iset}) ->
                          (instr, encName, iset)) (AS.instEncodings instr)) aslInsts

stalledDefs :: [T.Text]
stalledDefs =
  ["AArch32_CheckBreakpoint_2"
  ,"AArch32_CheckWatchpoint_4"
  ,"AArch64_CheckBreakpoint_2"
  ,"AArch64_CheckWatchpoint_4"
  ]

main :: IO ()
main = do
  sm <- runWithFilters defaultOptions { optVerbose = False}
  let opts = defaultStatOptions {
        reportKnownExceptions = True,
        reportSucceedingInstructions = True}
  reportStats opts sm

testDefinition :: String -> Int -> IO (SigMap)
testDefinition s startidx = do
  let nm = T.pack s
  let opts = defaultOptions 
  sig <- runWithFilters (opts  { optFilters = translateOnlyFun nm } )
  case Map.lookup nm (sMap sig) of
    Just _ -> return sig
    Nothing -> error $ "Translation failed to discover function: " <> show nm

testInstruction :: TranslatorOptions -> String -> String -> IO (SigMap)
testInstruction opts instr enc = do
  runWithFilters (opts { optFilters = translateOnlyInstr (T.pack instr, T.pack enc) })

translateAll :: IO (SigMap)
translateAll = do
  runWithFilters defaultOptions


parTranslateAll :: TranslatorOptions -> Int -> IO (SigMap)
parTranslateAll opts chunksz = do
  spec <- getASL
  let allInstrs = imap (\i -> \nm -> (i,nm)) $ collectInstructions (aslInstructions spec)
  let (nchunks, _) = length allInstrs `divMod` chunksz
  let (sigEnv, sigState) = buildSigState spec
  let runFun chunk = do
        let opts' = opts { optVerbose = False,
                           optStartIndex = (chunk * chunksz),
                           optNumberOfInstructions = Just chunksz } 
        runWithFilters' opts' spec sigEnv sigState
        
  mvs <- mapM (\chunk -> doFork (runFun chunk)) [0 .. nchunks]
  sms <- mapM collapseResult mvs
  let opts' = opts { optStartIndex = (nchunks * chunksz), optNumberOfInstructions = Nothing }
  sm <- runWithFilters' opts' spec sigEnv sigState
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

noFilter :: Filters
noFilter = Filters (\_ -> \_ -> True) (\_ -> True) (\_ -> \_ -> True) (\_ -> True)
    
filterStalls :: Filters
filterStalls = Filters
  (\_ -> \_ -> True)
  (\_ -> True)
  (\_ -> \nm -> not $ List.elem nm stalledDefs)
  (\_ -> True)

translateOnlyFun :: T.Text -> Filters
translateOnlyFun fnm = Filters
  (\_ -> \nm -> (nm == fnm))
  (\(InstructionIdent nm enc _) -> True)
  (\_ -> \nm -> nm == fnm) (\_ -> False)

translateOnlyInstr :: (T.Text, T.Text) -> Filters
translateOnlyInstr inm = Filters
  (\(InstructionIdent nm enc _) -> \_ -> inm == (nm, enc))
  (\(InstructionIdent nm enc _) -> (nm, enc) == inm)
  (\(InstructionIdent nm enc _) -> \_ -> inm == (nm, enc))
  (\(InstructionIdent nm enc _) -> (nm, enc) == inm)

logMsg :: String -> MSS.StateT SigMap IO ()
logMsg msg = do
  MSS.modify' $ \s -> s { sLog = T.pack msg : sLog s }
  verbose <- MSS.gets (optVerbose . sOptions)
  E.when verbose $ MSS.lift $ putStrLn msg
  

isFunFilteredOut :: InstructionIdent -> T.Text -> MSS.StateT SigMap IO Bool
isFunFilteredOut inm fnm = do
  test <- MSS.gets (funFilter . optFilters . sOptions)
  return $ not $ test inm fnm

isInstrFilteredOut :: InstructionIdent -> MSS.StateT SigMap IO Bool
isInstrFilteredOut inm = do
  test <- MSS.gets (instrFilter . optFilters . sOptions)
  return $ not $ test inm

isFunTransFilteredOut :: InstructionIdent -> T.Text -> MSS.StateT SigMap IO Bool
isFunTransFilteredOut inm fnm = do
  test <- MSS.gets (funTranslationFilter . optFilters . sOptions)
  skipTranslation <- MSS.gets (optSkipTranslation . sOptions)
  return $ (not $ test inm fnm) || skipTranslation

isInstrTransFilteredOut :: InstructionIdent -> MSS.StateT SigMap IO Bool
isInstrTransFilteredOut inm = do
  test <- MSS.gets (instrTranslationFilter . optFilters . sOptions)
  skipTranslation <- MSS.gets (optSkipTranslation . sOptions)
  return $ (not $ test inm) || skipTranslation

runWithFilters' :: TranslatorOptions
                -> ASLSpec
                -> SigEnv
                -> SigState
                -> IO (SigMap)
runWithFilters' opts spec sigEnv sigState = do
  let startidx = optStartIndex opts
  let numInstrs = optNumberOfInstructions opts
  let allInstrs = imap (\i -> \nm -> (i,nm)) $ collectInstructions (aslInstructions spec)
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
  spec <- getASL
  putStrLn $ "Loaded "
    ++ show (length $ aslInstructions spec) ++ " instructions and "
    ++ show (length $ aslDefinitions spec) ++ " definitions and "
    ++ show (length $ aslSupportDefinitions spec) ++ " support definitions and "
    ++ show (length $ aslExtraDefinitions spec) ++ " extra definitions and "
    ++ show (length $ aslRegisterDefinitions spec) ++ " register definitions."
  let (sigEnv, sigState) = buildSigState spec
  runWithFilters' opts spec sigEnv sigState

data ExpectedException =
    CannotMonomorphize T.Text
  | SymbolicArguments T.Text
  | NotImplementedYet String
  | LValSlicedSetters
  | GlobalArrayOfStructs
  | MissingFunction String
  | BadInstructionSpecification T.Text
  | MissingGlobalDeclaration T.Text
  | InsufficientStaticTypeInformation
  | BoolComparisonUnsupported
  | RealValuesUnsupported
  | LValSliceUnsupported
  | ExponentiationUnsupported
  | ParserError
  deriving (Eq, Ord, Show)

expectedExceptions :: ElemKey -> TranslatorException -> Maybe ExpectedException
expectedExceptions k ex = case ex of
  SExcept (SigException _ (UnsupportedSigExpr (AS.ExprMemberBits (AS.ExprBinOp _ _ _) _))) -> Just $ ParserError
  SExcept (SigException _ (TypeNotFound "real")) -> Just $ RealValuesUnsupported
  TExcept (CannotMonomorphizeFunctionCall f) -> Just $ CannotMonomorphize f
  TExcept (CannotMonomorphizeOverloadedFunctionCall f _) -> Just $ CannotMonomorphize f
  TExcept (UnsupportedSlice (AS.SliceSingle _)) -> Just $ SymbolicArguments "Slice"
  TExcept (UnsupportedSlice (AS.SliceRange _ _)) -> Just $ SymbolicArguments "Slice"
  TExcept (RequiredConcreteValue nm _) -> Just $ SymbolicArguments nm
  TExcept (UnsupportedLVal (AS.LValSlice _)) -> Just $ LValSliceUnsupported
  TExcept (UNIMPLEMENTED msg) -> Just $ NotImplementedYet msg
  TExcept (UnboundName "R") -> Just $ LValSlicedSetters
  TExcept (UnboundName "D") -> Just $ LValSlicedSetters
  TExcept (UnboundName nm) ->
    if List.elem nm ["imm32", "index", "m", "mode", "regs", "sz", "carry", "add", "tag_checked"]
    then Just $ BadInstructionSpecification nm
    else Nothing
  TExcept (UnsupportedBinaryOperator AS.BinOpPow) -> Just $ ExponentiationUnsupported
  TExcept (CannotStaticallyEvaluateType _ _) -> Just $ InsufficientStaticTypeInformation
  TExcept (UnsupportedComparisonType (AS.ExprVarRef (AS.QualifiedIdentifier _ _)) _) -> Just $ BoolComparisonUnsupported
  _ -> Nothing

isUnexpectedException :: ElemKey -> TranslatorException -> Bool
isUnexpectedException k e = expectedExceptions k e == Nothing

data StatOptions = StatOptions
  { reportKnownExceptions :: Bool
  , reportSucceedingInstructions :: Bool
  , reportAllExceptions :: Bool
  }

defaultStatOptions :: StatOptions
defaultStatOptions = StatOptions
  { reportKnownExceptions = False
  , reportSucceedingInstructions = False
  , reportAllExceptions = False
  }

reportStats :: StatOptions -> SigMap -> IO ()
reportStats sopts sm = do
  putStrLn $ "Unexpected exceptions:"
  _ <- Map.traverseWithKey (\ident -> \e ->
      E.when (unexpected (KeyInstr ident) e) $ do
        putStrLn $ prettyIdent ident ++ " failed to translate:"
        putStrLn $ show e) (instrExcepts sm)
  putStrLn "----------------------"     
  _ <- Map.traverseWithKey (\ident -> \deps -> do
          let errs' = catMaybes $ (\dep -> (\x -> (dep,x)) <$> Map.lookup dep (funExcepts sm)) <$> (Set.toList deps)
          let errs = filter (\(dep, x) -> unexpected (KeyFun dep) x) errs'
          if null errs then return ()
          else do
            putStrLn $ prettyIdent ident ++ " has failing dependencies:"
            mapM_ (\(dep, err) -> putStrLn $ show dep <> ":" <> show err) errs) (instrDeps sm)
  putStrLn "----------------------"
  E.when (reportKnownExceptions sopts) $ do
    let expectedInstrs = Map.foldrWithKey (addExpected . KeyInstr) Map.empty (instrExcepts sm) 
    let expected = Map.foldrWithKey (addExpected . KeyFun) expectedInstrs (funExcepts sm)

    _ <- Map.traverseWithKey (\ex -> \ks -> do
         putStrLn $ "Failures due to known exception: " <> show ex
         putStrLn "----------------------"
         mapM_ (\k -> putStrLn $ prettyKey k) ks
         putStrLn "") expected
    return ()
  
  putStrLn $ "Total instructions inspected: " <> show (Map.size $ instrDeps sm)
  putStrLn $ "Number of instructions which raised exceptions: " <> show (Map.size $ instrExcepts sm)
  putStrLn "----------------------"
  E.when (reportSucceedingInstructions sopts) $
    putStrLn $ "Instructions with no errors in any dependent functions:"
  r <- Map.traverseMaybeWithKey (\ident -> \deps -> do
    if not (Map.member ident (instrExcepts sm)) &&
       Set.null (Set.filter (\dep -> Map.member dep (funExcepts sm)) deps) 
    then do
      E.when (reportSucceedingInstructions sopts) $ putStrLn $ prettyIdent ident
      return $ Just ident
    else return Nothing) (instrDeps sm)
  putStrLn $ "Number of successfully translated functions: " <> show (Map.size $ r)

  where
    prettyIdent (InstructionIdent nm enc iset) = show nm <> " " <> show enc <> " " <> show iset
    prettyKey k = case k of
      KeyInstr ident -> "Instruction: " <> prettyIdent ident
      KeyFun nm -> "Function: " <> show nm
    unexpected k err =
      if reportAllExceptions sopts then True else isUnexpectedException k err
    addExpected nm err = case expectedExceptions nm err of
      Just e -> Map.insertWith Set.union e (Set.singleton nm)
      Nothing -> id
  
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
          alldeps <- mapM (translationLoop instrIdent defs) (Map.assocs deps)
          let alldepsSet = Set.union (Set.unions alldeps) (Map.keysSet deps)
          MSS.modify' $ \s -> s { instrDeps = Map.insert instrIdent alldepsSet (instrDeps s) }
    _ -> error "Panic"

data Filters = Filters { funFilter :: InstructionIdent -> T.Text -> Bool
                       , instrFilter :: InstructionIdent -> Bool
                       , funTranslationFilter :: InstructionIdent -> T.Text -> Bool
                       , instrTranslationFilter :: InstructionIdent -> Bool
                       }

data ElemKey =
   KeyInstr InstructionIdent
 | KeyFun T.Text
 deriving (Eq, Ord, Show)

data TranslatorOptions = TranslatorOptions
  { optVerbose :: Bool
  , optStartIndex :: Int
  , optNumberOfInstructions :: Maybe Int
  , optFilters :: Filters
  , optSkipTranslation :: Bool
  , optCollectExceptions :: Bool
  , optThrowUnexpectedExceptions :: Bool
  }

defaultOptions :: TranslatorOptions
defaultOptions = TranslatorOptions
  { optVerbose = True
  , optStartIndex = 0
  , optNumberOfInstructions = Nothing
  , optFilters = noFilter
  , optSkipTranslation = True
  , optCollectExceptions = True
  , optThrowUnexpectedExceptions = True
  }

doTranslationOptions :: TranslatorOptions
doTranslationOptions = defaultOptions {optFilters = filterStalls, optSkipTranslation = False}


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
collectExcept k e = do
  collectExceptions <- MSS.gets (optCollectExceptions . sOptions)
  throwUnexpectedExceptions <- MSS.gets (optThrowUnexpectedExceptions . sOptions)
  if (not collectExceptions || (isUnexpectedException k e && throwUnexpectedExceptions))
    then X.throw e
  else case k of
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
  

translationLoop :: InstructionIdent
                -> Definitions arch
                -> (T.Text, TypeEnvir)
                -> MSS.StateT SigMap IO (Set.Set T.Text)
translationLoop fromInstr defs (fnname, env) = do
  let finalName = (mkFinalFunctionName env fnname)
  fdeps <- MSS.gets funDeps
  case Map.lookup finalName fdeps of
    Just deps -> return deps
    _ -> do
      filteredOut <- isFunFilteredOut fromInstr finalName
      if filteredOut
      then return Set.empty
      else do
       case Map.lookup fnname (defSignatures defs) of
           Just (ssig, stmts) | Some sig <- mkSignature defs env ssig -> do
                 MSS.modify' $ \s -> s { sMap = Map.insert finalName (Some sig) (sMap s) }
                 logMsg $ "Translating function: " ++ show finalName ++ "\n" ++ show sig ++ "\n"
                 deps <- processFunction fromInstr finalName sig stmts defs
                 MSS.modify' $ \s -> s { funDeps = Map.insert finalName Set.empty (funDeps s) }
                 alldeps <- mapM (translationLoop fromInstr defs) (Map.assocs deps)
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
  ASLSpec aslInsts aslDefs aslDefs' aslDefs'' _ <- getASL
  return $ ASLT.foldASL f h g (aslDefs ++ aslDefs' ++ aslDefs'') aslInsts b

getASL :: IO (ASLSpec)
getASL = do
  eAslDefs <- AP.parseAslDefsFile defsFilePath
  eAslSupportDefs <- AP.parseAslDefsFile supportFilePath
  eAslExtraDefs <- AP.parseAslDefsFile extraDefsFilePath
  eAslInsts <- AP.parseAslInstsFile instsFilePath
  eAslRegs <- AP.parseAslRegsFile regsFilePath
  case (eAslInsts, eAslDefs, eAslRegs, eAslExtraDefs, eAslSupportDefs) of
    (Left err, _, _, _, _) -> do
      putStrLn $ "Error loading ASL instructions: " ++ show err
      exitFailure
    (_, Left err, _, _, _) -> do
      putStrLn $ "Error loading ASL definitions: " ++ show err
      exitFailure
    (_, _, Left err ,_, _) -> do
      putStrLn $ "Error loading ASL registers: " ++ show err
      exitFailure
    (_, _, _ , Left err, _) -> do
      putStrLn $ "Error loading extra ASL definitions: " ++ show err
      exitFailure
    (_, _, _ , _, Left err) -> do
      putStrLn $ "Error loading ASL support definitions: " ++ show err
      exitFailure
    (Right aslInsts, Right aslDefs, Right aslRegs, Right aslExtraDefs, Right aslSupportDefs) -> do
      return $ prepASL $ ASLSpec aslInsts aslDefs aslSupportDefs aslExtraDefs aslRegs

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

processFunction :: InstructionIdent
                -> T.Text
                -> SomeSignature ret
                -> [AS.Stmt]
                -> Definitions arch
                -> MSS.StateT SigMap IO (Map.Map T.Text TypeEnvir)
processFunction fromInstr fnName sig stmts defs =
  case sig of
    SomeFunctionSignature fSig -> do
      handleAllocator <- CFH.newHandleAllocator
      mf <- catchIO (KeyFun fnName) $ functionToCrucible defs fSig handleAllocator stmts
      case mf of
        Just f -> do
          filteredOut <- isFunTransFilteredOut fromInstr fnName
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
          filteredOut <- isFunTransFilteredOut fromInstr fnName
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
