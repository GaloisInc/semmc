{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main ) where

import qualified Control.Exception as X
import           Data.Monoid
import           Control.Monad.Identity
import           Control.Monad.ST ( stToIO )
import           Control.Monad (forM_, foldM, when)
import qualified Control.Monad.State.Lazy as MSS
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Except as E
import           Control.Monad.IO.Class
import           Data.Map ( Map )
import qualified Data.Map.Strict as Map
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Maybe ( fromMaybe, catMaybes, listToMaybe, mapMaybe )
import           Text.Read (readMaybe)
import           Data.Parameterized.Classes
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
import           System.IO (hPutStrLn, stderr)
import           System.Console.GetOpt
import           Panic hiding (panic)
import           Lang.Crucible.Panic ( Crucible )

-- import           SemMC.ASL.SyntaxTraverse ( logMsg, indentLog )
import qualified SemMC.ASL.SyntaxTraverse as AS  ( pattern VarName )
import qualified SemMC.ASL.SyntaxTraverse as TR

import qualified SemMC.Formula as SF
import qualified What4.Expr.Builder as B

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

import qualified SemMC.Architecture.Location as L
import           SemMC.Architecture.ARM.Location ( A32, T32 )
import qualified SemMC.Architecture.ARM.Location as AL

data TranslatorOptions = TranslatorOptions
  { optVerbosity :: Integer
  , optStartIndex :: Int
  , optNumberOfInstructions :: Maybe Int
  , optFilters :: Filters
  , optSkipTranslation :: Bool
  , optCollectAllExceptions :: Bool
  , optCollectExpectedExceptions :: Bool
  , optASLSpecFilePath :: FilePath
  , optTranslationTask :: TranslationTask
  , optTranslationDepth :: TranslationDepth
  }


data TranslationDepth = TranslateRecursive
                      | TranslateShallow

data TranslationTask = TranslateAll
                     | TranslateTargets
                     | TranslateNoArch64
                     | TranslateArch32
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
  { optVerbosity = 1
  , optStartIndex = 0
  , optNumberOfInstructions = Nothing
  , optFilters = noFilter
  , optSkipTranslation = False
  , optCollectAllExceptions = False
  , optCollectExpectedExceptions = True
  , optASLSpecFilePath = "./test/"
  , optTranslationDepth = TranslateRecursive
  , optTranslationTask = TranslateArch32
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

arguments :: [OptDescr (Either (TranslatorOptions -> Maybe TranslatorOptions) (StatOptions -> Maybe StatOptions))]
arguments =
  [ Option "a" ["asl-spec"] (ReqArg (\f -> Left (\opts -> Just $ opts { optASLSpecFilePath = f })) "PATH")
    ("Path to parsed ASL specification. Requires: " ++ instsFilePath ++ " " ++ defsFilePath
      ++ " " ++ regsFilePath ++ " " ++ supportFilePath ++ " " ++ extraDefsFilePath
      ++ " " ++ targetInstsFilePath)

  , Option "s" ["skip-simulation"] (NoArg (Left (\opts -> Just $ opts { optSkipTranslation = True })))
    "Skip symbolic execution step after translating into Crucible"

  , Option "c" ["collect-exceptions"] (NoArg (Left (\opts -> Just $ opts { optCollectAllExceptions = True })))
    "Handle and collect all exceptions thrown during translation"

  , Option [] ["collect-expected-exceptions"] (NoArg (Left (\opts -> Just $ opts { optCollectExpectedExceptions = True })))
    "Handle and collect exceptions for known issues thrown during translation"

  , Option "v" ["verbosity"] (ReqArg (\f -> (Left (\opts -> Just $ opts { optVerbosity = read f }))) "INT")
    ("Output verbosity during translation:\n" ++
    "0 - minimal output.\n" ++
    "-1 - no translation output.\n" ++
    "1 (default) - translator/simulator log.\n" ++
    "2 - translator trace (on exception).\n" ++
    "3 - instruction post-processing trace (on exception).\n4 - globals collection trace.\n" ++
    "6 - translator and globals collection trace (always).")

  , Option [] ["offset"] (ReqArg (\f -> Left (\opts -> Just $ opts {optStartIndex = read f})) "INT")
    "Start processing instructions at the given offset"

  , Option [] ["report-success"] (NoArg (Right (\opts -> Just $ opts { reportSucceedingInstructions = True })))
    "Print list of successfully translated instructions"

  , Option [] ["report-deps"] (NoArg (Right (\opts -> Just $ opts { reportFunctionDependencies = True })))
    "Print ancestors of functions when reporting exceptions"

  , Option [] ["report-exceptions"] (NoArg (Right (\opts -> Just $ opts { reportAllExceptions = True })))
    "Print all collected exceptions thrown during translation (requires collect-exceptions or collect-expected-exceptions)"

  , Option [] ["report-expected-exceptions"] (NoArg (Right (\opts -> Just $ opts {reportKnownExceptions = True })))
    "Print collected exceptions for known issues thrown during translation (requires collect-exceptions or collect-expected-exceptions)"

  , Option [] ["report-formulas"] (NoArg (Right (\opts -> Just $ opts { reportFunctionFormulas = True })))
    "Print function formulas for all successfully simulated functions."

  , Option [] ["translation-mode"] (ReqArg (\mode -> Left (\opts -> do
      task <- case mode of
        "all" -> return $ TranslateAll
        "targets" -> return $ TranslateTargets
        "noArch64" -> return $ TranslateNoArch64
        "Arch32" -> return $ TranslateArch32
        _ -> case List.splitOn "/" mode of
          [instr, enc] -> return $ TranslateInstruction instr enc
          _ -> fail ""
      return $ opts { optTranslationTask = task })) "MODE")
    ("Filter instructions according to MODE: \n" ++
     "all - translate all instructions from " ++ instsFilePath ++ ".\n" ++
     "targets - translate instructions filtered by " ++ targetInstsFilePath ++ ".\n" ++
     "noArch64 - translate T16, T32 and A32 instructions.\n" ++
     "Arch32 - translate T32 and A32 instructions.\n" ++
     "<INSTRUCTION>/<ENCODING> - translate a single instruction/encoding pair.")

  , Option [] ["no-dependencies"] (NoArg (Left (\opts -> Just $ opts { optTranslationDepth = TranslateShallow } )))
    "Don't recursively translate function dependencies."
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

  case foldl applyOption (Just (defaultOptions, defaultStatOptions)) args of
    Nothing -> do
      usage
      exitFailure
    Just (opts, statOpts) -> do
      sm <- case optTranslationTask opts of
        TranslateTargets -> do
          filter <- getTargetFilter opts
          runWithFilters (opts { optFilters = filter })
        TranslateAll -> runWithFilters opts
        TranslateInstruction inst enc -> testInstruction opts inst enc
        TranslateNoArch64 -> runWithFilters (opts { optFilters = translateNoArch64 } )
        TranslateArch32 -> runWithFilters (opts { optFilters = translateArch32 } )
      reportStats statOpts sm

  where
    applyOption (Just (opts, statOpts)) arg = case arg of
      Left f -> do
        opts' <- f opts
        return $ (opts', statOpts)
      Right f -> do
        statOpts' <- f statOpts
        return $ (opts, statOpts')
    applyOption Nothing _ = Nothing

runWithFilters :: TranslatorOptions -> IO (SigMap)
runWithFilters opts = do
  spec <- getASL opts
  logMsgIO opts 0 $ T.pack $ "Loaded "
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
  sm <- execSigMapM (forM_ instrs (\(i, (ident, instr)) -> do
     logMsg 1 $ T.pack $ "Processing instruction: " ++ show i ++ "/" ++ show (length allInstrs)
     runTranslation instr ident))
    (SigMap Map.empty Map.empty Map.empty sigState sigEnv Map.empty Map.empty Map.empty opts)
  return sm

runTranslation :: AS.Instruction -> InstructionIdent -> SigMapM ()
runTranslation instruction@AS.Instruction{..} instrIdent = do
  logMsg 1 $ "Computing instruction signature for: " <> T.pack (show instrIdent)
  result <- liftSigM (KeyInstr instrIdent) $
    computeInstructionSignature instruction (iEnc instrIdent) (iSet instrIdent)
  case result of
    Left err -> do
      logMsg 0 $ "Error computing instruction signature: " <> T.pack (show err)
    Right (Some (SomeFunctionSignature iSig), instStmts) -> do
      defs <- liftSigM (KeyInstr instrIdent) $ getDefinitions
      case defs of
        Left err -> do
          logMsg 0 $ "Error computing ASL definitions: " <> T.pack (show err)
        Right defs -> do
          logMsg 1 $ "Translating instruction: " <> T.pack (prettyIdent instrIdent)
          logMsg 1 $ T.pack $ (show iSig)
          deps <- processFunction instrIdent (KeyInstr instrIdent) iSig instStmts defs
          MSS.gets (optTranslationDepth . sOptions) >>= \case
            TranslateRecursive -> do
              logMsg 1 $ "--------------------------------"
              logMsg 1 $ "Translating functions: "
              alldeps <- mapM (translationLoop instrIdent [] defs) (Map.assocs deps)
              let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)
              MSS.modify' $ \s -> s { instrDeps = Map.insert instrIdent alldepsSet (instrDeps s) }
            TranslateShallow -> return ()

-- FIXME: hack to inject concrete type parameter
toDefinitions32 :: Definitions arch -> Definitions A32
toDefinitions32 (Definitions a b c d e) = Definitions a b c d e

translationLoop :: IsARMArch arch
                => InstructionIdent
                -> [T.Text]
                -> Definitions arch
                -> (T.Text, StaticValues)
                -> SigMapM (Set.Set T.Text)
translationLoop fromInstr callStack defs' (fnname, env) = do
  let defs = toDefinitions32 defs'
  let finalName = (mkFinalFunctionName env fnname)
  fdeps <- MSS.gets funDeps
  case Map.lookup finalName fdeps of
    Just deps -> return deps
    _ -> do
      filteredOut <- isFunFilteredOut fromInstr finalName
      if filteredOut
      then return Set.empty
      else do
        result <- liftSigM (KeyFun finalName) $ do
          case Map.lookup fnname (defSignatures defs) of
             Just (ssig, stmts) -> do
               sig <- mkSignature env ssig
               return (sig, stmts)
             Nothing -> E.throwError $ MissingSigFunctionDefinition finalName
        case result of
          Left err -> do
            return Set.empty
          Right (Some ssig@(SomeFunctionSignature sig), stmts) -> do
            MSS.modify' $ \s -> s { sMap = Map.insert finalName (Some ssig) (sMap s) }
            logMsg 1 $ T.pack $ "Translating function: " ++ show finalName ++ " for instruction: "
               ++ prettyIdent fromInstr
               ++ "\n CallStack: " ++ show callStack
               ++ "\n" ++ show sig ++ "\n"
            deps <- processFunction fromInstr (KeyFun finalName) sig stmts defs
            MSS.modify' $ \s -> s { funDeps = Map.insert finalName Set.empty (funDeps s) }
            alldeps <- mapM (translationLoop fromInstr (finalName : callStack) defs) (Map.assocs deps)
            let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)
            MSS.modify' $ \s -> s { funDeps = Map.insert finalName alldepsSet (funDeps s) }
            return alldepsSet

withOnlineBackend :: forall fm scope a.
                          NonceGenerator IO scope
                       -> CBO.UnsatFeatures
                       -> (CBO.YicesOnlineBackend scope (B.Flags B.FloatReal) -> IO a)
                       -> IO a
withOnlineBackend gen unsatFeat action = do
  -- Some ng <- newSTNonceGenerator
  let feat = Yices.yicesDefaultFeatures .|. CBO.unsatFeaturesToProblemFeatures unsatFeat
  CBO.withOnlineBackend B.FloatRealRepr gen feat $ \sym -> do
    WC.extendConfig Yices.yicesOptions (WI.getConfiguration sym)
    action sym

-- Extremely vague measure of function body size
measureStmts :: [AS.Stmt] -> Int
measureStmts stmts = getSum $ runIdentity $ mconcat <$> traverse (TR.collectSyntax doCollect) stmts
  where
    doCollect :: TR.KnownSyntaxRepr t => t -> Identity (Sum Int)
    doCollect _ = return 1

type IsARMArch arch = (OrdF (L.Location arch))

processFunction :: IsARMArch arch
                => InstructionIdent
                -> ElemKey
                -> FunctionSignature globalReads globalWrites init tps
                -> [AS.Stmt]
                -> Definitions arch
                -> SigMapM (Map.Map T.Text StaticValues)
processFunction fromInstr key sig stmts defs' = do
  let defs = toDefinitions32 defs'
  handleAllocator <- liftIO $ CFH.newHandleAllocator
  logLvl <- MSS.gets (optVerbosity . sOptions)
  mp <- catchIO key $ functionToCrucible defs sig handleAllocator stmts logLvl
  case mp of
    Just p -> do
      filteredOut <- case key of
        KeyInstr instr -> isInstrTransFilteredOut instr
        KeyFun fnName -> isFunTransFilteredOut fromInstr fnName
      if filteredOut
      then return (funcDepends p)
      else do
        logMsg 1 $ T.pack $ "Simulating: " ++ prettyKey key
        logMsg 1 $ T.pack $ "Rough function body size:" ++ show (measureStmts stmts)
        mdep <- catchIO key $
          withOnlineBackend globalNonceGenerator CBO.NoUnsatFeatures $ \backend -> do
            let cfg = SimulatorConfig { simOutputHandle = IO.stdout
                                      , simHandleAllocator = handleAllocator
                                      , simSym = backend
                                      }
            case key of
              KeyInstr _ -> do
                symInstr <- simulateInstruction cfg p
                return (funcDepends p, Nothing)
              KeyFun _ -> do
                symFn <- simulateFunction cfg p
                return (funcDepends p, Just (FP.printFunctionFormula symFn))
        case mdep of
          Just (dep, mformula) -> do
            logMsg 1 "Simulation succeeded!"
            case mformula of
              Just formula ->
                MSS.modify $ \s -> s { sFormulas = Map.insert key formula (sFormulas s) }
              _ -> return ()
            return dep
          _ -> do
            logMsg 1 "Simulation failed."
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
      hPutStrLn stderr $ "Error loading ASL instructions: " ++ show err
      exitFailure
    (_, Left err, _, _, _) -> do
      hPutStrLn stderr $ "Error loading ASL definitions: " ++ show err
      exitFailure
    (_, _, Left err ,_, _) -> do
      hPutStrLn stderr $ "Error loading ASL registers: " ++ show err
      exitFailure
    (_, _, _ , Left err, _) -> do
      hPutStrLn stderr $ "Error loading extra ASL definitions: " ++ show err
      exitFailure
    (_, _, _ , _, Left err) -> do
      hPutStrLn stderr $ "Error loading ASL support definitions: " ++ show err
      exitFailure
    (Right aslInsts, Right aslDefs, Right aslRegs, Right aslExtraDefs, Right aslSupportDefs) -> do
      return $ prepASL $ ASLSpec aslInsts aslDefs aslSupportDefs aslExtraDefs aslRegs

logMsgIO :: TranslatorOptions -> Integer -> T.Text -> IO ()
logMsgIO opts logLvl msg = do
  let verbosity = (optVerbosity opts)
  E.when (verbosity >= logLvl) $ liftIO $ putStrLn (T.unpack msg)


isFunFilteredOut :: InstructionIdent -> T.Text -> SigMapM Bool
isFunFilteredOut inm fnm = do
  test <- MSS.gets (funFilter . optFilters . sOptions)
  return $ not $ test inm fnm

isFunTransFilteredOut :: InstructionIdent -> T.Text -> SigMapM Bool
isFunTransFilteredOut inm fnm = do
  test <- MSS.gets (funTranslationFilter . optFilters . sOptions)
  skipTranslation <- MSS.gets (optSkipTranslation . sOptions)
  return $ (not $ test inm fnm) || skipTranslation

isInstrTransFilteredOut :: InstructionIdent -> SigMapM Bool
isInstrTransFilteredOut inm = do
  test <- MSS.gets (instrTranslationFilter . optFilters . sOptions)
  skipTranslation <- MSS.gets (optSkipTranslation . sOptions)
  return $ (not $ test inm) || skipTranslation

data ExpectedException =
    RealValueUnsupported
  | InsufficientStaticTypeInformation
  | CruciblePanic
  | ASLSpecMissingZeroCheck
  | BVLengthFromGlobalState
   deriving (Eq, Ord, Show)

expectedExceptions :: ElemKey -> TranslatorException -> Maybe ExpectedException
expectedExceptions k ex = case ex of
  SExcept _ (TypeNotFound "real") -> Just $ RealValueUnsupported
  -- TExcept _ (CannotMonomorphizeFunctionCall _ _) -> Just $ InsufficientStaticTypeInformation
  -- TExcept _ (CannotStaticallyEvaluateType _ _) -> Just $ InsufficientStaticTypeInformation
  -- TExcept _ (CannotDetermineBVLength _ _) -> Just $ InsufficientStaticTypeInformation
  TExcept _ (UnsupportedType (AS.TypeFun "bits" (AS.ExprLitInt 0)))
    | KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` ["aarch32_USAT16_A", "aarch32_USAT_A"] ->
      Just $ ASLSpecMissingZeroCheck
  TExcept _ (CannotStaticallyEvaluateType (AS.TypeFun "bits" (AS.ExprCall (AS.VarName fnm) _)) _)
    | fnm `elem` ["BAREGETTER_PL", "BAREGETTER_VL"] ->
      Just $ BVLengthFromGlobalState
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

forMwithKey_ :: Applicative t => Map k a -> (k -> a -> t b) -> t ()
forMwithKey_ m f = void $ Map.traverseWithKey f m

reportStats :: StatOptions -> SigMap -> IO ()
reportStats sopts sm = do
  let expectedInstrs = Map.foldrWithKey (addExpected . KeyInstr) Map.empty (instrExcepts sm)
  let expected = Map.foldrWithKey (addExpected . KeyFun) expectedInstrs (funExcepts sm)
  let unexpectedElems =
        Set.union (Set.map KeyInstr $ Map.keysSet (instrExcepts sm)) (Set.map KeyFun $ Map.keysSet (funExcepts sm))
        Set.\\ (Set.unions $ Map.elems expectedInstrs ++ Map.elems expected)

  when (not (Set.null unexpectedElems)) $ do
    putStrLn $ "Unexpected exceptions:"
    forMwithKey_ (instrExcepts sm) $ \ident -> \e ->
      E.when (unexpected (KeyInstr ident) e) $ do
        putStrLn $ prettyIdent ident ++ " failed to translate:"
        putStrLn $ show e
    putStrLn "----------------------"
    forMwithKey_ (instrDeps sm) $ \ident -> \deps -> do
      let errs' = catMaybes $ (\dep -> (\x -> (dep,x)) <$> Map.lookup dep (funExcepts sm)) <$> (Set.toList deps)
      let errs = filter (\(dep, x) -> unexpected (KeyFun dep) x) errs'
      if null errs then return ()
      else do
        putStrLn $ prettyIdent ident ++ " has failing dependencies:"
        mapM_ (\(dep, err) -> putStrLn $ show dep <> ":" <> show err) errs
    putStrLn "----------------------"
  when (reportKnownExceptions sopts) $ do


    forMwithKey_ expected $ \ex -> \ks -> do
      putStrLn $ "Failures due to known exception: " <> show ex
      putStrLn "----------------------"
      mapM_ printKey ks
      putStrLn ""
    return ()

  putStrLn $ "Total instructions inspected: " <> show (Map.size $ instrDeps sm)
  putStrLn $ "Total functions inspected: " <> show (Map.size $ funDeps sm)
  putStrLn $ "Number of instructions which raised exceptions: " <> show (Map.size $ instrExcepts sm)
  putStrLn "----------------------"
  when (reportSucceedingInstructions sopts) $
    putStrLn $ "Instructions with no errors in any dependent functions:"
  r <- Map.traverseMaybeWithKey (\ident -> \deps -> do
    if not (Map.member ident (instrExcepts sm)) &&
       Set.null (Set.filter (\dep -> Map.member dep (funExcepts sm)) deps)
    then do
      E.when (reportSucceedingInstructions sopts) $ putStrLn $ prettyIdent ident
      return $ Just ident
    else return Nothing) (instrDeps sm)
  putStrLn $ "Number of successfully translated functions: " <> show (Map.size $ r)
  when (reportFunctionFormulas sopts) $ do
    putStrLn $ "Successfully simulated functions"
    putStrLn $ "-------------------------------"
    forMwithKey_ (sFormulas sm) $ \k formula -> do
      putStrLn $ prettyKey k
      putStrLn $ T.unpack $ formula
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
    TExcept ElemKey TranslationException
  | SExcept ElemKey SigException
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
                     , sOptions :: TranslatorOptions
                     }

type SigMapM a = MSS.StateT SigMap IO a

runSigMapM :: SigMapM a -> SigMap -> IO (a, SigMap)
runSigMapM m = MSS.runStateT m

execSigMapM :: SigMapM a -> SigMap -> IO SigMap
execSigMapM m = MSS.execStateT m

--instance TR.MonadLog SigMapM where
logMsg :: Integer -> T.Text -> SigMapM ()
logMsg logLvl msg = do
  verbosity <- MSS.gets (optVerbosity . sOptions)
  when (verbosity >= logLvl) $ liftIO $ putStrLn $ T.unpack $ msg


printLog :: [T.Text] -> SigMapM ()
printLog [] = return ()
printLog log = liftIO $ putStrLn (T.unpack $ T.unlines log)

liftSigM :: ElemKey -> SigM ext f a -> SigMapM (Either SigException a)
liftSigM k f = do
  state <- MSS.gets sigState
  env <- MSS.gets sigEnv
  logLvl <- MSS.gets (optVerbosity . sOptions)
  let ((result, state'), log) = runSigM env state logLvl f
  case result of
    Right a -> do
      when (logLvl >= 5) $ printLog log
      MSS.modify' $ \s -> s { sigState = state' }
      return $ Right a
    Left err -> do
      printLog log
      collectExcept k (SExcept k err)
      return $ Left err

collectExcept :: ElemKey -> TranslatorException -> SigMapM ()
collectExcept k e = do
  collectAllExceptions <- MSS.gets (optCollectAllExceptions . sOptions)
  collectExpectedExceptions <- MSS.gets (optCollectExpectedExceptions . sOptions)
  if (collectAllExceptions || ((not $ isUnexpectedException k e) && collectExpectedExceptions))
  then case k of
    KeyInstr ident -> MSS.modify' $ \s -> s { instrExcepts = Map.insert ident e (instrExcepts s) }
    KeyFun fun -> MSS.modify' $ \s -> s { funExcepts = Map.insert fun e (funExcepts s) }
  else X.throw e

catchIO :: ElemKey -> IO a -> SigMapM (Maybe a)
catchIO k f = do
  a <- liftIO ((Left <$> f)
                  `X.catch` (\(e :: TranslationException) -> return $ Right $ TExcept k e)
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


translateNoArch64 :: Filters
translateNoArch64 = Filters
  (\(InstructionIdent _ _ iset) -> \_ -> iset /= AS.A64 )
  (\(InstructionIdent _ _ iset) -> iset /= AS.A64)
  (\(InstructionIdent _ _ iset) -> \_ -> iset /= AS.A64)
  (\(InstructionIdent _ _ iset) -> iset /= AS.A64)


translateArch32 :: Filters
translateArch32 = Filters
  (\(InstructionIdent _ _ iset) -> \_ -> iset `elem` [AS.A32, AS.T32] )
  (\(InstructionIdent _ _ iset) -> iset `elem` [AS.A32, AS.T32])
  (\(InstructionIdent _ _ iset) -> \_ -> iset `elem` [AS.A32, AS.T32])
  (\(InstructionIdent _ _ iset) -> iset `elem` [AS.A32, AS.T32])
