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
import           Data.Maybe ( fromMaybe, catMaybes, listToMaybe, mapMaybe )
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Nonce
import           Data.Bits( (.|.) )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as List
import qualified Data.Tree as Tree
import           Data.List.Index (imap)
import qualified Lang.Crucible.Backend.Simple as CBS
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Language.ASL.Parser as AP
import qualified Language.ASL.Syntax as AS
import System.Exit (exitFailure)
import qualified System.IO as IO
import           Panic hiding (panic)
import           Lang.Crucible.Panic ( Crucible )

import SemMC.ASL
import SemMC.ASL.Crucible
import SemMC.ASL.Translation
import SemMC.ASL.Translation.Preprocess
import SemMC.ASL.Signature
import SemMC.ASL.Types
import SemMC.ASL.StaticExpr
import SemMC.ASL.Exceptions

import Control.Concurrent
import Control.Concurrent.MVar

import qualified What4.Interface as WI
import qualified What4.Solver.Yices as Yices
import qualified What4.Config as WC
import           What4.ProblemFeatures

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

targetInstsFilePath :: FilePath
targetInstsFilePath = "test/translated_instructions.txt"

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
  , optFilters = defaultFilter
  , optSkipTranslation = True
  , optCollectExceptions = True
  , optThrowUnexpectedExceptions = True
  }


doTranslationOptions :: TranslatorOptions
doTranslationOptions = defaultOptions {optFilters = filterStalls, optSkipTranslation = False}

main :: IO ()
main = execMainAt 0

execMainAt :: Int -> IO ()
execMainAt startIdx = do
  targetInsts <- getTargetInstrs
  let filter = defaultFilter { instrFilter = \ident -> Set.member ident targetInsts }
  let translateOpts = defaultOptions {
          optFilters = filter
        , optSkipTranslation = False
        , optCollectExceptions = True
        , optStartIndex = startIdx
        , optVerbose = False
        }
  sm <- runWithFilters translateOpts
  
  let reportOpts = defaultStatOptions {
        reportKnownExceptions = True,
        reportSucceedingInstructions = True,
        reportFunctionDependencies = True}
  reportStats reportOpts sm

getTargetFilter :: IO (Filters)
getTargetFilter = do
  targetInsts <- getTargetInstrs
  let filter = defaultFilter { instrFilter = \ident -> Set.member ident targetInsts }
  return filter

testDefinition :: TranslatorOptions -> String -> String -> String -> IO (SigMap)
testDefinition opts inm' encnm' defnm' = do
  let defnm = T.pack defnm'
  let inm = T.pack inm'
  let encnm = T.pack encnm'
  let filter = translateOnlyFun (inm, encnm) defnm
  sig <- runWithFilters (opts  { optFilters = filter} )
  case Map.lookup defnm (sMap sig) of
    Just _ -> return sig
    Nothing -> error $ "Translation failed to discover function: " <> show defnm

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

defaultFilter :: Filters
defaultFilter = Filters
  (\_ -> \_ -> True)
  (\_ -> True)
  (\_ -> \_ -> True)
  (\_ -> True)
    
filterStalls :: Filters
filterStalls = Filters
  (\_ -> \_ -> True)
  (\_ -> True)
  (\_ -> \nm -> not $ List.elem nm stalledDefs)
  (\_ -> True)

translateOnlyFun :: (T.Text, T.Text) -> T.Text -> Filters
translateOnlyFun inm fnm = Filters
  (\(InstructionIdent nm enc _) -> \_ -> (nm, enc) == inm)
  (\(InstructionIdent nm enc _) -> (nm, enc) == inm)
  (\_ -> \nm -> nm == fnm)
  (\_ -> False)

translateOnlyInstr :: (T.Text, T.Text) -> Filters
translateOnlyInstr inm = Filters
  (\(InstructionIdent nm enc _) -> \_ -> inm == (nm, enc))
  (\(InstructionIdent nm enc _) -> (nm, enc) == inm)
  (\(InstructionIdent nm enc _) -> \_ -> False)
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
  let getInstr (instr, enc, iset) = do
        let test = instrFilter $ optFilters $ opts
        let ident = instrToIdent instr enc iset
        if test ident then Just (ident, instr) else Nothing
  let allInstrs = imap (\i -> \nm -> (i,nm)) $ mapMaybe getInstr (collectInstructions (aslInstructions spec))
  let instrs = case numInstrs of {Just i -> take i (drop startidx allInstrs); _ -> drop startidx allInstrs}
  sm <- MSS.execStateT (forM_ instrs (\(i, (ident, instr)) -> do
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
    UnsupportedInstruction
  | InsufficientStaticTypeInformation
  | CruciblePanic
  | UnsupportedNonlinearArithmetic
-- data ExpectedException =
--     CannotMonomorphize T.Text
--   | SymbolicArguments T.Text
--   | NotImplementedYet String
--   | GlobalArrayOfStructs
--   | MissingFunction String
--   | BadInstructionSpecification T.Text
--   | MissingGlobalDeclaration T.Text
--   | InsufficientStaticTypeInformation
--   | CannotSignExtendUnknownBVSize
--   | BoolComparisonUnsupported
--   | RealValuesUnsupported
--   | LValSliceUnsupported
--   | ExponentiationUnsupported
--   | RmemUnsupported
--   | ParserError
--   | UnsupportedInstruction
   deriving (Eq, Ord, Show)

-- badStaticInstructions :: [T.Text]
-- badStaticInstructions =
--   ["aarch32_VDUP_r_A", "aarch32_CRC32_A", "aarch32_VSLI_A", "aarch32_VSRI_A"
--   ,"PFALSE_P__", "SETFFR_F__"]

expectedExceptions :: ElemKey -> TranslatorException -> Maybe ExpectedException
expectedExceptions k ex = case ex of
  TExcept _ (InstructionUnsupported) -> Just $ UnsupportedInstruction
  SExcept (SigException _ (TypeNotFound "real")) -> Just $ UnsupportedInstruction
  TExcept _ (CannotMonomorphizeFunctionCall _ _) -> Just $ InsufficientStaticTypeInformation
  TExcept _ (CannotStaticallyEvaluateType _ _) -> Just $ InsufficientStaticTypeInformation
  TExcept _ (CannotDetermineBVLength _ _) -> Just $ InsufficientStaticTypeInformation
  SomeExcept e
    | Just (Panic (_ :: Crucible) _ _ _) <- X.fromException e
    , KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` ["aarch32_WFE_A", "aarch32_WFI_A"] ->
      Just $ CruciblePanic
  SomeExcept e
    | Just (SimulationAbort _ _) <- X.fromException e
    , KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` [ "aarch32_SMUAD_A", "aarch32_SMLAD_A"
                , "aarch32_SMLSD_A", "aarch32_SMLAWB_A"
                , "aarch32_SMLABB_A"] ->
      Just $ UnsupportedNonlinearArithmetic
  _ -> Nothing
-- expectedExceptions k ex = case ex of
--   SExcept (SigException _ (UnsupportedSigExpr (AS.ExprMemberBits (AS.ExprBinOp _ _ _) _))) -> Just $ ParserError
--   SExcept (SigException _ (TypeNotFound "real")) -> Just $ UnsupportedInstruction
--   TExcept _ (CannotMonomorphizeFunctionCall f _) -> Just $ CannotMonomorphize f
--   TExcept _ (CannotMonomorphizeOverloadedFunctionCall f _) -> Just $ CannotMonomorphize f
--   TExcept _ (CannotDetermineBVLength _ _) -> case k of
--    KeyInstr (InstructionIdent nm _ _)
--      | nm `elem` badStaticInstructions ->
--      Just $ InsufficientStaticTypeInformation
--    _ -> Nothing
--   TExcept _ (UnsupportedSlice (AS.SliceRange _ _) _) -> case k of
--     KeyInstr (InstructionIdent nm _ _)
--       | nm `elem` badStaticInstructions ->
--       Just $ InsufficientStaticTypeInformation
--     KeyInstr (InstructionIdent "aarch32_SBFX_A" _ _) ->
--       Just $ CannotSignExtendUnknownBVSize
--     _ -> Nothing
--   TExcept _ (RequiredConcreteValue nm _) -> Just $ SymbolicArguments nm
--   TExcept _ (UnsupportedLVal (AS.LValSlice _)) -> Just $ LValSliceUnsupported
--   TExcept _ (UNIMPLEMENTED msg) -> Just $ NotImplementedYet msg
--   TExcept _ (CannotStaticallyEvaluateType _ _) -> Just $ InsufficientStaticTypeInformation
--   TExcept _ (ExpectedBVType _ _) -> Just $ ParserError
--   TExcept _ (InstructionUnsupported) -> case k of
--     KeyInstr _ -> Just $ UnsupportedInstruction
--     _ -> Nothing
--   _ -> Nothing



isUnexpectedException :: ElemKey -> TranslatorException -> Bool
isUnexpectedException k e = expectedExceptions k e == Nothing

data StatOptions = StatOptions
  { reportKnownExceptions :: Bool
  , reportSucceedingInstructions :: Bool
  , reportAllExceptions :: Bool
  , reportKnownExceptionFilter :: ExpectedException -> Bool
  , reportFunctionDependencies :: Bool
  }

defaultStatOptions :: StatOptions
defaultStatOptions = StatOptions
  { reportKnownExceptions = False
  , reportSucceedingInstructions = False
  , reportAllExceptions = False
  , reportKnownExceptionFilter = (\_ -> True)
  , reportFunctionDependencies = False
  }

reportAllOptions :: StatOptions
reportAllOptions = StatOptions
  { reportKnownExceptions = True
  , reportSucceedingInstructions = True
  , reportAllExceptions = True
  , reportKnownExceptionFilter = (\_ -> True)
  , reportFunctionDependencies = True
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
         mapM_ printKey ks
         putStrLn "") expected
    return ()
  
  putStrLn $ "Total instructions inspected: " <> show (Map.size $ instrDeps sm)
  putStrLn $ "Total functions inspected: " <> show (Map.size $ funDeps sm)
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
    reverseDependencyMap =
        Map.fromListWith (++) $ concat $ map (\(instr, funs) -> map (\fun -> (fun, [instr])) (Set.toList funs))
           (Map.assocs (instrDeps sm))
    printKey k = case k of
      KeyInstr ident -> putStrLn $ "Instruction: " <> prettyIdent ident
      KeyFun nm -> do
        putStrLn $ "Function: " <> show nm
        E.when (reportFunctionDependencies sopts) $ do
          putStrLn $ "Which is depended on by: "
          case Map.lookup nm reverseDependencyMap of
            Just instrs -> mapM_ (\ident -> putStrLn $  "    " <> prettyIdent ident) instrs
            _ -> return ()
    unexpected k err =
      if reportAllExceptions sopts then True else isUnexpectedException k err
    addExpected nm err = case expectedExceptions nm err of
      Just e -> if (reportKnownExceptionFilter sopts e)
                then Map.insertWith Set.union e (Set.singleton nm)
                else id
      Nothing -> id

prettyIdent :: InstructionIdent -> String
prettyIdent (InstructionIdent nm enc iset) = show nm <> " " <> show enc <> " " <> show iset

data TranslatorException =
    TExcept (T.Text, StaticValues, [AS.Stmt], [(AS.Expr, TypeConstraint)]) TranslationException
  | SExcept SigException
  | BadTranslatedInstructionsFile
  | SomeExcept X.SomeException

instance Show TranslatorException where
  show e = case e of
    TExcept (nm, env, stmts, exprs) te ->
      "Translator exception\n"
      ++ "Statement call stack:\n"
      ++ unlines (map (\stmt -> withStar $ prettyStmt 3 stmt) (List.reverse stmts))
      ++ "\n Expression call stack:\n"
      ++ unlines (map (\expr -> "*  " ++ prettyExprConstraint expr) (List.reverse exprs))
      ++ "Static variable environment:\n"
      ++ show env ++ "\n"
      ++ "\n ** Exception in: " ++ T.unpack nm ++ "\n"
      ++ show te

    SExcept se -> "Signature exception:\n" ++ show se
    SomeExcept err -> "General exception:\n" ++ show err

withStar :: String -> String
withStar (' ' : rest) = '*' : rest
withStar s = s

atDepth :: Int -> String -> String
atDepth depth s = concat (replicate depth " ") ++ s

withLines :: [String] -> String
withLines strs = List.intercalate "\n" strs

prettyStmt :: Int -> AS.Stmt -> String
prettyStmt depth stmt = case stmt of
  AS.StmtIf tests melse ->
    atDepth depth "StmtIf: " ++
    unlines (map (\(test, stmts) ->
           prettyExpr test ++ "\n"
           ++ withLines (map (prettyStmt $ depth + 1) stmts)) tests)
    ++
    case melse of
      Just stmts -> (atDepth depth "Else\n") ++ withLines (map (prettyStmt $ depth + 1) stmts)
      Nothing -> ""
  AS.StmtFor var range stmts ->
    atDepth depth "StmtFor: " ++ show var ++ show range ++ "\n"
      ++ withLines (map (prettyStmt $ depth + 1) stmts)
  AS.StmtRepeat stmts test ->
    atDepth depth "StmtRepeat: " ++ prettyExpr test ++ "\n"
    ++ withLines (map (prettyStmt $ depth + 1) stmts)
  _ -> atDepth depth $ show stmt

prettyExprConstraint :: (AS.Expr, TypeConstraint) -> String
prettyExprConstraint (expr, constraint) = show expr ++ "\n   :: " ++ show constraint

prettyExpr :: AS.Expr -> String
prettyExpr expr = show expr

instance X.Exception TranslatorException

data InstructionIdent =
  InstructionIdent { iName :: T.Text, iEnc :: T.Text, iSet :: AS.InstructionSet }
  deriving (Eq, Show)

deriving instance Ord AS.InstructionSet
deriving instance Ord InstructionIdent

instrToIdent :: AS.Instruction -> T.Text -> AS.InstructionSet -> InstructionIdent
instrToIdent AS.Instruction{..} enc iset = InstructionIdent instName enc iset

finalDepsOf :: Map.Map T.Text StaticValues -> Set.Set T.Text
finalDepsOf deps = Set.fromList (map (\(nm, env) -> mkFinalFunctionName env nm) (Map.assocs deps))

runTranslation :: AS.Instruction -> InstructionIdent -> MSS.StateT SigMap IO ()
runTranslation instruction@AS.Instruction{..} instrIdent = do
  logMsg $ "Computing instruction signature for: " ++ show instrIdent
  result <- liftSigM (KeyInstr instrIdent) $
    computeInstructionSignature' instruction (iEnc instrIdent) (iSet instrIdent)
  case result of
    Left err -> do
      logMsg $ "Error computing instruction signature: " ++ show err
    Right (Some (SomeFunctionSignature iSig), instStmts) -> do
      defs <- liftSigM (KeyInstr instrIdent) $ getDefinitions
      case defs of
        Left err -> do
          logMsg $ "Error computing ASL definitions: " ++ show err
        Right defs -> do
          logMsg $ "Translating instruction: " ++ prettyIdent instrIdent
          logMsg $ (show iSig)
          deps <- processInstruction instrIdent iSig instStmts defs

          logMsg $ "--------------------------------"
          logMsg "Translating functions: "
          alldeps <- mapM (translationLoop instrIdent [] defs) (Map.assocs deps)
          let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)
          MSS.modify' $ \s -> s { instrDeps = Map.insert instrIdent alldepsSet (instrDeps s) }

data Filters = Filters { funFilter :: InstructionIdent -> T.Text -> Bool
                       , instrFilter :: InstructionIdent -> Bool
                       , funTranslationFilter :: InstructionIdent -> T.Text -> Bool
                       , instrTranslationFilter :: InstructionIdent -> Bool
                       }

data ElemKey =
   KeyInstr InstructionIdent
 | KeyFun T.Text
 deriving (Eq, Ord, Show)


-- FIXME: Seperate this into RWS
data SigMap = SigMap { sMap :: Map.Map T.Text (Some (SomeFunctionSignature))
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
                  `X.catch` (\(e :: TranslationException) -> return $ Right (TExcept (T.empty,Map.empty,[],[]) e))
                  `X.catch` (\(e :: TracedTranslationException) -> return $ Right (tracedException e))
                  `X.catch` (\(e :: X.SomeException) -> return $ Right (SomeExcept e)))
  case a of
    Left r -> return (Just r)
    Right err -> (\_ -> Nothing) <$> collectExcept k err
  where
    tracedException (TracedTranslationException nm env stmts exprs e) = TExcept (nm, env, stmts, exprs) e
  

translationLoop :: InstructionIdent
                -> [T.Text]
                -> Definitions arch
                -> (T.Text, StaticValues)
                -> MSS.StateT SigMap IO (Set.Set T.Text)
translationLoop fromInstr callStack defs (fnname, env) = do
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
                 logMsg $ "Translating function: " ++ show finalName ++ " for instruction: "
                    ++ prettyIdent fromInstr
                    ++ "\n CallStack: " ++ show callStack
                    ++ "\n" ++ show sig ++ "\n"
                 deps <- processFunction fromInstr finalName sig stmts defs
                 MSS.modify' $ \s -> s { funDeps = Map.insert finalName Set.empty (funDeps s) }
                 alldeps <- mapM (translationLoop fromInstr (finalName : callStack) defs) (Map.assocs deps)
                 let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)

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

textToISet :: T.Text -> Maybe AS.InstructionSet
textToISet t = case t of
  "A32" -> Just $ AS.A32
  "T32" -> Just $ AS.T32
  "T16" -> Just $ AS.T16
  "A64" -> Just $ AS.A64
  _ -> Nothing

getTargetInstrs :: IO (Set.Set InstructionIdent)
getTargetInstrs = do
  t <- TIO.readFile targetInstsFilePath
  return $ Set.fromList (map getTriple (T.lines t))
  where
    isQuote '\"' = True
    isQuote _ = False
    
    getTriple l = case T.words l of
      [instr, enc, iset]
        | Just is <- textToISet iset ->
        InstructionIdent
          { iName = T.dropAround isQuote instr
          , iEnc = T.dropAround isQuote enc
          , iSet = is
          }
      _ -> X.throw $ BadTranslatedInstructionsFile

withOnlineBackend :: forall fs scope a.
                          NonceGenerator IO scope
                       -> CBO.UnsatFeatures
                       -> (CBO.YicesOnlineBackend scope fs -> IO a)
                       -> IO a
withOnlineBackend gen unsatFeat action =
  let feat = Yices.yicesDefaultFeatures .|. CBO.unsatFeaturesToProblemFeatures unsatFeat in
  CBO.withOnlineBackend gen feat $ \sym ->
    do WC.extendConfig Yices.yicesOptions (WI.getConfiguration sym)
       action sym

processInstruction :: InstructionIdent
                   -> FunctionSignature globalReads globalWrites init tps
                   -> [AS.Stmt]
                   -> Definitions arch
                   -> MSS.StateT SigMap IO (Map.Map T.Text StaticValues)
processInstruction instr sig stmts defs = do
  handleAllocator <- MSS.lift $ CFH.newHandleAllocator
  mp <- catchIO (KeyInstr instr) $ functionToCrucible defs sig handleAllocator stmts
  case mp of
    Just p -> do
      filteredOut <- isInstrTransFilteredOut instr
      if filteredOut
      then return (funcDepends p)
      else do
        logMsg $ "Simulating instruction: " ++ prettyIdent instr
        mdep <- catchIO (KeyInstr instr) $
          withOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
            let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                      , simHandleAllocator = handleAllocator
                                      , simSym = backend
                                      }
            symFn <- simulateFunction cfg p
            return (funcDepends p)
        return $ fromMaybe Map.empty mdep
    Nothing -> return Map.empty

processFunction :: InstructionIdent
                -> T.Text
                -> SomeFunctionSignature ret
                -> [AS.Stmt]
                -> Definitions arch
                -> MSS.StateT SigMap IO (Map.Map T.Text StaticValues)
processFunction fromInstr fnName sig stmts defs =
  case sig of
    SomeFunctionSignature fSig -> do
      handleAllocator <- MSS.lift $ CFH.newHandleAllocator
      mf <- catchIO (KeyFun fnName) $ functionToCrucible defs fSig handleAllocator stmts
      case mf of
        Just f -> do
          filteredOut <- isFunTransFilteredOut fromInstr fnName
          if filteredOut
          then return (funcDepends f)
          else do
            logMsg $ "Simulating function: " ++ show fnName
            mdep <- catchIO (KeyFun fnName) $
              withOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
                let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                        , simHandleAllocator = handleAllocator
                                        , simSym = backend
                                        }
                symFn <- simulateFunction cfg f
                return (funcDepends f)
            return $ fromMaybe Map.empty mdep
        Nothing -> return Map.empty
