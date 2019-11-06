{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

module Main ( main ) where

import qualified Control.Exception as X
import           Data.Monoid
import           Control.Monad.Identity
import           Control.Monad (forM_, foldM, when)
import qualified Control.Monad.State.Lazy as MSS
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Except as E
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe ( fromMaybe, catMaybes, listToMaybe, mapMaybe )
import           Text.Read (readMaybe)
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Nonce
import           Data.Bits( (.|.) )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Tree as Tree
import           Data.List.Index (imap)
import qualified Lang.Crucible.Backend.Simple as CBS
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Language.ASL.Parser as AP
import qualified Language.ASL.Syntax as AS
import System.Exit (exitFailure)

import qualified System.Exit as IO
import qualified System.Environment as IO
import qualified System.IO as IO
import           System.Console.GetOpt
import           Panic hiding (panic)
import           Lang.Crucible.Panic ( Crucible )


import qualified SemMC.ASL.SyntaxTraverse as TR

import qualified SemMC.Formula as SF

import SemMC.Formula.Printer as FP
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

data TranslatorOptions = TranslatorOptions
  { optVerbose :: Bool
  , optStartIndex :: Int
  , optNumberOfInstructions :: Maybe Int
  , optFilters :: Filters
  , optSkipTranslation :: Bool
  , optCollectAllExceptions :: Bool
  , optCollectExpectedExceptions :: Bool
  , optASLSpecFilePath :: FilePath
  , optTranslationTask :: TranslationTask
  }

data TranslationTask = TranslateAll
                     | TranslateInstruction String String

instsFilePath :: FilePath
instsFilePath = "insts.parsed"

defsFilePath :: FilePath
defsFilePath = "defs.parsed"

regsFilePath :: FilePath
regsFilePath = "regs.parsed"

supportFilePath :: FilePath
supportFilePath = "support.parsed"

extraDefsFilePath :: FilePath
extraDefsFilePath = "extradefs.parsed"

targetInstsFilePath :: FilePath
targetInstsFilePath = "translated_instructions.txt"

defaultOptions :: TranslatorOptions
defaultOptions = TranslatorOptions
  { optVerbose = False
  , optStartIndex = 0
  , optNumberOfInstructions = Nothing
  , optFilters = noFilter
  , optSkipTranslation = False
  , optCollectAllExceptions = False
  , optCollectExpectedExceptions = False
  , optASLSpecFilePath = "./test/"
  , optTranslationTask = TranslateAll
  }

data StatOptions = StatOptions
  { reportKnownExceptions :: Bool
  , reportSucceedingInstructions :: Bool
  , reportAllExceptions :: Bool
  , reportKnownExceptionFilter :: ExpectedException -> Bool
  , reportFunctionDependencies :: Bool
  , reportFunctionFormulas :: Bool
  }

defaultStatOptions :: StatOptions
defaultStatOptions = StatOptions
  { reportKnownExceptions = False
  , reportSucceedingInstructions = False
  , reportAllExceptions = False
  , reportKnownExceptionFilter = (\_ -> True)
  , reportFunctionDependencies = False
  , reportFunctionFormulas = False
  }

arguments :: [OptDescr (Either (TranslatorOptions -> TranslatorOptions) (StatOptions -> StatOptions))]
arguments =
  [ Option "a" ["asl-spec"] (ReqArg (\f -> Left (\opts -> opts { optASLSpecFilePath = f })) "PATH")
    ("Path to parsed ASL specification. Requires: " ++ instsFilePath ++ " " ++ defsFilePath
      ++ " " ++ regsFilePath ++ " " ++ supportFilePath ++ " " ++ extraDefsFilePath
      ++ " " ++ targetInstsFilePath)

  , Option "s" ["skip-simulation"] (NoArg (Left (\opts -> opts { optSkipTranslation = True })))
    "Skip symbolic execution step after translating into Crucible"

  , Option "c" ["collect-exceptions"] (NoArg (Left (\opts -> opts { optCollectAllExceptions = True })))
    "Handle and collect all exceptions thrown during translation"

  , Option [] ["collect-expected-exceptions"] (NoArg (Left (\opts -> opts { optCollectExpectedExceptions = True })))
    "Handle and collect exceptions for known issues thrown during translation"

  , Option "v" ["verbose"] (NoArg (Left (\opts -> opts { optVerbose = True })))
    "Verbose output during translation"

  , Option [] ["offset"] (ReqArg (\f -> Left (\opts -> opts {optStartIndex = read f})) "INT")
    "Start processing instructions at the given offset"

  , Option [] ["report-success"] (NoArg (Right (\opts -> opts { reportSucceedingInstructions = True })))
    "Print list of successfully translated instructions"

  , Option [] ["report-deps"] (NoArg (Right (\opts -> opts { reportFunctionDependencies = True })))
    "Print ancestors of functions when reporting exceptions"

  , Option [] ["report-exceptions"] (NoArg (Right (\opts -> opts { reportAllExceptions = True })))
    "Print all collected exceptions thrown during translation (requires collect-exceptions or collect-expected-exceptions)"

  , Option [] ["report-expected-exceptions"] (NoArg (Right (\opts -> opts {reportKnownExceptions = True })))
    "Print collected exceptions for known issues thrown during translation (requires collect-exceptions or collect-expected-exceptions)"

  , Option [] ["report-formulas"] (NoArg (Right (\opts -> opts { reportFunctionFormulas = True })))
    "Print function formulas for all successfully simulated functions."

  , Option [] ["translate-instruction"]
    (ReqArg (\instrAndEnc -> case List.splitOn "/" instrAndEnc of
                [instr, enc] -> Left (\opts -> opts { optTranslationTask = TranslateInstruction instr enc })
                _ -> error "bad instruction/encoding") "INST")
    ("Name of particular instruction to translate. Format should be <INSTRUCTION>/<ENCODING>, where the two components correspond to instruction and encoding definitions in the ASL file.")
  ]

usage :: IO ()
usage = do
  pn <- IO.getProgName
  let msg = "Usage: " <> pn <> " [options]"
  putStrLn $ usageInfo msg arguments

main :: IO ()
main = do
  stringArgs <- IO.getArgs
  let (args, rest, errs) = getOpt Permute arguments stringArgs

  when (not $ null $ errs <> rest) $ do
    usage
    exitFailure

  let (opts', statOpts) = foldl applyOption (defaultOptions, defaultStatOptions) args
  filter <- getTargetFilter opts'
  let opts = opts' { optFilters = filter }
  sm <- case optTranslationTask opts of
    TranslateAll -> runWithFilters opts
    TranslateInstruction inst enc -> testInstruction opts inst enc

  reportStats statOpts sm

  where
    applyOption (opts, statOpts) arg = case arg of
      Left f -> (f opts, statOpts)
      Right f -> (opts, f statOpts)

runWithFilters :: TranslatorOptions -> IO (SigMap)
runWithFilters opts = do
  spec <- getASL opts
  putStrLn $ "Loaded "
    ++ show (length $ aslInstructions spec) ++ " instructions and "
    ++ show (length $ aslDefinitions spec) ++ " definitions and "
    ++ show (length $ aslSupportDefinitions spec) ++ " support definitions and "
    ++ show (length $ aslExtraDefinitions spec) ++ " extra definitions and "
    ++ show (length $ aslRegisterDefinitions spec) ++ " register definitions."
  let (sigEnv, sigState) = buildSigState spec
  runWithFilters' opts spec sigEnv sigState

collectInstructions :: [AS.Instruction] -> [(AS.Instruction, T.Text, AS.InstructionSet)]
collectInstructions aslInsts = do
  List.concat $
    map (\instr -> map (\(AS.InstructionEncoding {AS.encName=encName, AS.encInstrSet=iset}) ->
                          (instr, encName, iset)) (AS.instEncodings instr)) aslInsts

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
    (SigMap Map.empty Map.empty Map.empty sigState sigEnv Map.empty Map.empty Map.empty [] opts)
  return sm

runTranslation :: AS.Instruction -> InstructionIdent -> MSS.StateT SigMap IO ()
runTranslation instruction@AS.Instruction{..} instrIdent = do
  logMsg $ "Computing instruction signature for: " ++ show instrIdent
  result <- liftSigM (KeyInstr instrIdent) $
    computeInstructionSignature instruction (iEnc instrIdent) (iSet instrIdent)
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
          deps <- processFunction instrIdent (KeyInstr instrIdent) iSig instStmts defs

          logMsg $ "--------------------------------"
          logMsg "Translating functions: "
          alldeps <- mapM (translationLoop instrIdent [] defs) (Map.assocs deps)
          let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)
          MSS.modify' $ \s -> s { instrDeps = Map.insert instrIdent alldepsSet (instrDeps s) }

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
           Just (ssig, stmts) -> do
                 result <- liftSigM (KeyFun finalName) $ mkSignature env ssig
                 case result of
                   Left err -> do
                     logMsg $ "Error computing final function signature: " ++ show err
                     return Set.empty
                   Right (Some ssig@(SomeFunctionSignature sig)) -> do
                     MSS.modify' $ \s -> s { sMap = Map.insert finalName (Some ssig) (sMap s) }
                     logMsg $ "Translating function: " ++ show finalName ++ " for instruction: "
                        ++ prettyIdent fromInstr
                        ++ "\n CallStack: " ++ show callStack
                        ++ "\n" ++ show sig ++ "\n"
                     deps <- processFunction fromInstr (KeyFun finalName) sig stmts defs
                     MSS.modify' $ \s -> s { funDeps = Map.insert finalName Set.empty (funDeps s) }
                     alldeps <- mapM (translationLoop fromInstr (finalName : callStack) defs) (Map.assocs deps)
                     let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)

                     MSS.modify' $ \s -> s { funDeps = Map.insert finalName alldepsSet (funDeps s) }
                     return alldepsSet
           _ -> error $ "Missing definition for:" <> (show fnname)

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

-- Extremely vague measure of function body size
measureStmts :: [AS.Stmt] -> Int
measureStmts stmts = getSum $ runIdentity $ mconcat <$> traverse (TR.collectSyntax doCollect) stmts
  where
    doCollect :: TR.KnownSyntaxRepr t => t -> Identity (Sum Int)
    doCollect _ = return 1

processFunction :: InstructionIdent
                -> ElemKey
                -> FunctionSignature globalReads globalWrites init tps
                -> [AS.Stmt]
                -> Definitions arch
                -> MSS.StateT SigMap IO (Map.Map T.Text StaticValues)
processFunction fromInstr key sig stmts defs = do
  handleAllocator <- MSS.lift $ CFH.newHandleAllocator
  mp <- catchIO key $ functionToCrucible defs sig handleAllocator stmts
  case mp of
    Just p -> do
      filteredOut <- case key of
        KeyInstr instr -> isInstrTransFilteredOut instr
        KeyFun fnName -> isFunTransFilteredOut fromInstr fnName
      if filteredOut
      then return (funcDepends p)
      else do
        logMsg $ "Simulating: " ++ prettyKey key
        logMsg $ "Rough function body size:" ++ show (measureStmts stmts)
        mdep <- catchIO key $
          withOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
            let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                      , simHandleAllocator = handleAllocator
                                      , simSym = backend
                                      }
            symFn <- simulateFunction cfg p
            return (funcDepends p, FP.printFunctionFormula symFn)
        case mdep of
          Just (dep, formula) -> do
            logMsg "Simulation succeeded!"
            MSS.modify $ \s -> s { sFormulas = Map.insert key formula (sFormulas s) }
            return dep
          _ -> do
            logMsg "Simulation failed."
            return Map.empty
    Nothing -> return Map.empty

getASL :: TranslatorOptions -> IO (ASLSpec)
getASL opts = do
  let getPath file = (optASLSpecFilePath opts ++ file)
  eAslDefs <- AP.parseAslDefsFile (getPath defsFilePath)
  eAslSupportDefs <- AP.parseAslDefsFile (getPath supportFilePath)
  eAslExtraDefs <- AP.parseAslDefsFile (getPath extraDefsFilePath)
  eAslInsts <- AP.parseAslInstsFile (getPath instsFilePath)
  eAslRegs <- AP.parseAslRegsFile (getPath regsFilePath)
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

data ExpectedException =
    UnsupportedInstruction
  | InsufficientStaticTypeInformation
  | CruciblePanic
  | ASLSpecMissingZeroCheck
   deriving (Eq, Ord, Show)

expectedExceptions :: ElemKey -> TranslatorException -> Maybe ExpectedException
expectedExceptions k ex = case ex of
  TExcept _ (TR.SyntaxTraceError InstructionUnsupported _) -> Just $ UnsupportedInstruction
  SExcept _ (TR.SyntaxTraceError (TypeNotFound "real") _) -> Just $ UnsupportedInstruction
  -- TExcept _ (CannotMonomorphizeFunctionCall _ _) -> Just $ InsufficientStaticTypeInformation
  -- TExcept _ (CannotStaticallyEvaluateType _ _) -> Just $ InsufficientStaticTypeInformation
  -- TExcept _ (CannotDetermineBVLength _ _) -> Just $ InsufficientStaticTypeInformation
  TExcept _ (TR.SyntaxTraceError (UnsupportedType (AS.TypeFun "bits" (AS.ExprLitInt 0))) _)
    | KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` ["aarch32_USAT16_A", "aarch32_USAT_A"] ->
      Just $ ASLSpecMissingZeroCheck
  SomeExcept e
    | Just (Panic (_ :: Crucible) _ _ _) <- X.fromException e
    , KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` ["aarch32_WFE_A", "aarch32_WFI_A", "aarch32_VTBL_A", "aarch32_VTST_A"] ->
      Just $ CruciblePanic
  SomeExcept e
    | Just (Panic (_ :: Crucible) _ _ _) <- X.fromException e
    , KeyFun nm <- k
    , nm `elem` ["AArch32_ExclusiveMonitorsPass_2"] ->
      Just $ CruciblePanic
  SomeExcept e
    | Just (SimulationAbort _ _) <- X.fromException e
    , KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` [ "aarch32_VTBL_A" ] ->
      Just $ CruciblePanic
  _ -> Nothing

isUnexpectedException :: ElemKey -> TranslatorException -> Bool
isUnexpectedException k e = expectedExceptions k e == Nothing

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
  E.when (reportFunctionFormulas sopts) $ do
    putStrLn $ "Successfully simulated functions"
    putStrLn $ "-------------------------------"
    void $ Map.traverseWithKey (\k formula -> do
      putStrLn $ prettyKey k
      putStrLn $ T.unpack $ formula) (sFormulas sm)
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

prettyKey :: ElemKey -> String
prettyKey (KeyInstr ident) = prettyIdent ident
prettyKey (KeyFun fnName) = T.unpack fnName

data TranslatorException =
    TExcept ElemKey TracedTranslationException
  | SExcept ElemKey TracedSigException
  | BadTranslatedInstructionsFile
  | SomeExcept X.SomeException

instance Show TranslatorException where
  show e = case e of
    TExcept k te -> "Translator exception in: " ++ prettyKey k ++ "\n" ++ show te
    SExcept k se -> "Signature exception in:" ++ prettyKey k ++ "\n" ++ show se
    SomeExcept err -> "General exception:\n" ++ show err

instance X.Exception TranslatorException

data InstructionIdent =
  InstructionIdent { iName :: T.Text, iEnc :: T.Text, iSet :: AS.InstructionSet }
  deriving (Eq, Show)

deriving instance Ord AS.InstructionSet
deriving instance Read AS.InstructionSet
deriving instance Ord InstructionIdent

instrToIdent :: AS.Instruction -> T.Text -> AS.InstructionSet -> InstructionIdent
instrToIdent AS.Instruction{..} enc iset = InstructionIdent instName enc iset

finalDepsOf :: Map.Map T.Text StaticValues -> Set.Set T.Text
finalDepsOf deps = Set.fromList (map (\(nm, env) -> mkFinalFunctionName env nm) (Map.assocs deps))


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
                     , sFormulas :: Map.Map ElemKey T.Text
                     , sLog :: [T.Text]
                     , sOptions :: TranslatorOptions
                     }


liftSigM :: ElemKey -> SigM ext f a -> MSS.StateT SigMap IO (Either TracedSigException a)
liftSigM k f = do
  state <- MSS.gets sigState
  env <- MSS.gets sigEnv
  let (result, state' ) = runSigM env state f
  case result of
    Right a -> do
      MSS.modify' $ \s -> s { sigState = state' }
      return $ Right a
    Left err -> do
      collectExcept k (SExcept k err)
      return $ Left err

collectExcept :: ElemKey -> TranslatorException -> MSS.StateT SigMap IO ()
collectExcept k e = do
  collectAllExceptions <- MSS.gets (optCollectAllExceptions . sOptions)
  collectExpectedExceptions <- MSS.gets (optCollectExpectedExceptions . sOptions)
  if (collectAllExceptions || ((not $ isUnexpectedException k e) && collectExpectedExceptions))
  then case k of
    KeyInstr ident -> MSS.modify' $ \s -> s { instrExcepts = Map.insert ident e (instrExcepts s) }
    KeyFun fun -> MSS.modify' $ \s -> s { funExcepts = Map.insert fun e (funExcepts s) }
  else X.throw e

catchIO :: ElemKey -> IO a -> MSS.StateT SigMap IO (Maybe a)
catchIO k f = do
  a <- MSS.lift ((Left <$> f)
                  `X.catch` (\(e :: TranslationException) -> return $ Right $ TExcept k $ TR.SyntaxTraceError e (TR.SyntaxTraceStack (\_ -> [])))
                  `X.catch` (\(e :: TracedTranslationException) -> return $ Right $ TExcept k e)
                  `X.catch` (\(e :: X.SomeException) -> return $ Right (SomeExcept e)))
  case a of
    Left r -> return (Just r)
    Right err -> (\_ -> Nothing) <$> collectExcept k err

-- Unused currently
type InstructionFlag = ()

getTargetInstrs :: TranslatorOptions -> IO (Map.Map InstructionIdent InstructionFlag)
getTargetInstrs opts = do
  t <- TIO.readFile ((optASLSpecFilePath opts) ++ targetInstsFilePath)
  return $ Map.fromList (map getTriple (T.lines t))
  where
    isQuote '\"' = True
    isQuote _ = False

    getFlag [s] = readMaybe (T.unpack s)
    getFlag [] = Just $ ()
    getFlag _ = Nothing

    getTriple l =
      if | (instr : enc : iset : rest) <- T.words l
         , Just is <- readMaybe (T.unpack iset)
         , Just flag <- getFlag rest ->
           (InstructionIdent
             { iName = T.dropAround isQuote instr
             , iEnc = T.dropAround isQuote enc
             , iSet = is
             }, flag)
         | otherwise -> X.throw $ BadTranslatedInstructionsFile


getTargetFilter :: TranslatorOptions -> IO (Filters)
getTargetFilter opts = do
  targetInsts <- getTargetInstrs opts
  let filter = noFilter { instrFilter = \ident -> Map.member ident targetInsts }
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

noFilter :: Filters
noFilter = Filters
  (\_ -> \_ -> True)
  (\_ -> True)
  (\_ -> \_ -> True)
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
  (\(InstructionIdent nm enc _) -> \_ -> inm == (nm, enc))
  (\(InstructionIdent nm enc _) -> (nm, enc) == inm)
