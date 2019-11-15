{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<|>))
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import           Control.Monad (replicateM, forM_, when, forM)
import qualified Control.Exception as E
import qualified Data.Ini.Config as CI
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.Foldable as F
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.Set.NonEmpty as NES
import qualified Data.Word.Indexed as W
import           Data.Word ( Word64 )
import           Data.Proxy ( Proxy(..) )
import           Data.List (intercalate)
import           Data.Maybe (catMaybes, fromMaybe, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Map as M
import           Data.EnumF (EnumF)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import qualified System.Exit as IO
import qualified System.Environment as IO
import qualified System.IO as IO
import           System.Console.GetOpt
import           System.Posix.User (getLoginName)
import           Network.HostName (getHostName)
import qualified Network.HTTP as H
import           Text.PrettyPrint.HughesPJ (render, Doc)

import           Data.Parameterized.Some (Some(..))
import           Data.Parameterized.Classes (ShowF(..))
import qualified Data.Parameterized.Nonce as N
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.HasRepr (HasRepr)
import qualified Data.Parameterized.List as L

import qualified Lang.Crucible.Backend.Simple as SB
import qualified What4.Expr.Builder as SB

import qualified Dismantle.Arbitrary as DA
import           Dismantle.Instruction (GenericInstruction(Instruction))
import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.PPC as PPC
import qualified Dismantle.ARM as ARMDis
import           Dismantle.ARM.Random ()

import           SemMC.Fuzzer.Types
import           SemMC.Fuzzer.Util
import           SemMC.Fuzzer.Filters

import qualified SemMC.Log as L
import qualified SemMC.Formula as F
import qualified SemMC.Concrete.Execution as CE
import           SemMC.Architecture.Evaluate (evaluateInstruction)
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as C
import qualified SemMC.Architecture.View as V
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.PPC32 as PPCS
import qualified SemMC.Architecture.PPC32.Opcodes as PPCS
import qualified SemMC.Architecture.A32 as A32
import qualified SemMC.Architecture.ARM.Opcodes as ARM
import           SemMC.Synthesis.Template ( TemplatableOperand
                                          , TemplatedOperand
                                          )

import Debug.Trace

data Arg =
    ConfigPath FilePath
    | Help
    | Strategy String
    | RunnerPath FilePath
    | ReportURL String
    | Arch String
    | Host String
    | OpcodeList String
    | ChunkSize String
    | LogLevelStr String
    | NumThreads String
    deriving (Eq, Show)

arguments :: [OptDescr Arg]
arguments =
    [ Option "h" ["help"] (NoArg Help)
      "Show this help"

    , Option "c" ["config-path"] (ReqArg ConfigPath "PATH")
      "Load the specified configuration"

    , Option "a" ["arch"] (ReqArg Arch "ARCHNAME")
      ("The name of the architecture to test (choices: " <>
      intercalate ", " allArchNames <> ")")

    , Option "u" ["report-url"] (ReqArg ReportURL "URL")
      "The base URL of a Fuzzermon instance to use for reporting results"

    , Option "t" ["threads"] (ReqArg Arch "NUM")
      ("The number of local testing threads to run against the remote host (default: " <>
      (show $ configNumThreads defaultConfig) <> ")")

    , Option "H" ["host"] (ReqArg Host "HOSTNAME")
      "The host on which to run tests"

    , Option "r" ["runner-path"] (ReqArg RunnerPath "PATH")
      ("The path to the test runner binary on the remote host (default: " <>
      (show $ configBinaryPath defaultConfig) <> ")")

    , Option "n" ["chunk-size"] (ReqArg ChunkSize "NUM")
      ("The number of test cases to generate in each batch (default: " <>
      show (configChunkSize defaultConfig) <> ")")

    , Option "s" ["strategy"] (ReqArg Strategy "TYPE")
      "The testing strategy to use (choices: random, roundrobin; default: random)"

    , Option "o" ["opcodes"] (ReqArg OpcodeList "OPCODES")
      "A comma-separated list of opcodes to test (default: test all known opcodes)"

    , Option "L" ["log-level"] (ReqArg LogLevelStr "LEVEL")
      ("Maximum log verbosity level (choices: " <> allLogLevels <>
      ", default: " <> show (configLogLevel defaultConfig) <> ")")
    ]

allLogLevels :: String
allLogLevels = intercalate ", " $ show <$> levels
    where levels = [ L.Debug
                   , L.Info
                   , L.Warn
                   , L.Error
                   ]

data Config =
    Config { configPath       :: Maybe FilePath
           , configShowHelp   :: Bool
           , configArchName   :: Maybe String
           , configHost       :: Maybe String
           , configBinaryPath :: FilePath
           , configOpcodes    :: OpcodeMatch
           , configStrategy   :: TestStrategy
           , configChunkSize  :: Int
           , configLogLevel   :: L.LogLevel
           , configNumThreads :: Int
           , configReportURL  :: Maybe String
           , configUsername   :: Maybe String
           , configFilters    :: [String]
           }

defaultConfig :: Config
defaultConfig =
    Config { configPath       = Nothing
           , configShowHelp   = False
           , configArchName   = Nothing
           , configHost       = Nothing
           , configBinaryPath = defaultRunnerPath
           , configOpcodes    = AllOpcodes
           , configStrategy   = Randomized
           , configChunkSize  = 1000
           , configLogLevel   = L.Info
           , configNumThreads = 1
           , configReportURL  = Nothing
           , configUsername   = Nothing
           , configFilters    = []
           }

loadConfig :: FilePath -> IO (Either String FuzzerConfig)
loadConfig cfgPath = do
    txt <- T.readFile cfgPath
    return $ CI.parseIniFile txt parser

hostSectionPrefix :: T.Text
hostSectionPrefix = "host:"

parser :: CI.IniParser FuzzerConfig
parser = do
    (arch, op, strat, ll, url, fs) <- CI.section "fuzzer" $ do
        arch  <- T.unpack <$> CI.field "arch"
        op    <- CI.fieldDefOf "opcodes" CI.readable (configOpcodes defaultConfig)
        strat <- CI.fieldDefOf "strategy" CI.readable (configStrategy defaultConfig)
        url   <- (Just <$> CI.field "report-url") <|>
                 (return $ T.pack <$> configReportURL defaultConfig)
        ll    <- CI.fieldDefOf "log-level" CI.readable (configLogLevel defaultConfig)
        fs    <- CI.fieldDefOf "instruction-state-filters" CI.readable (configFilters defaultConfig)
        return (arch, op, strat, ll, url, fs)

    when (not $ arch `elem` allArchNames) $
        fail $ "Invalid architecture name: " <> arch

    let hostSection = T.stripPrefix hostSectionPrefix
    hosts <- CI.sectionsOf hostSection $ \hostname -> do
        FuzzerTestHost (T.unpack hostname)
            <$> (CI.fieldMbOf "username" CI.string)
            <*> CI.fieldDefOf "chunk-size" CI.readable (configChunkSize defaultConfig)
            <*> (T.unpack <$> CI.fieldDef "runner-path" (T.pack $ configBinaryPath defaultConfig))
            <*> (CI.fieldDefOf "threads" CI.readable (configNumThreads defaultConfig))

    return FuzzerConfig { fuzzerArchName = arch
                        , fuzzerArchTestingHosts = F.toList hosts
                        , fuzzerTestOpcodes = op
                        , fuzzerTestStrategy = strat
                        , fuzzerMaximumLogLevel = ll
                        , fuzzerReportURL = T.unpack <$> url
                        , fuzzerFilters = fs
                        }

ppc32Arch :: ArchImpl
ppc32Arch =
    ArchImpl "ppc32"
             (Proxy @PPCS.PPC)
             PPCS.allOpcodes
             PPCS.allSemantics
             PPCS.allDefinedFunctions
             PPCS.testSerializer
             PPC.ppInstruction
             PPC.assembleInstruction
             ppc32OpcodeFilter

ppc32OpcodeFilter :: Some (PPC.Opcode PPC.Operand) -> Bool
ppc32OpcodeFilter o =
    not $ show o `elem` blacklist
    where
        blacklist = [ "BCL"
                    ]

a32Arch :: ArchImpl
a32Arch =
    ArchImpl "a32"
             (Proxy @A32.A32)
             ARM.a32Opcodes
             ARM.a32Semantics
             ARM.a32DefinedFunctions
             A32.testSerializer
             ARMDis.ppInstruction
             ARMDis.assembleInstruction
             a32OpcodeFilter

a32OpcodeFilter :: Some (ARMDis.Opcode ARMDis.Operand) -> Bool
a32OpcodeFilter = const True

knownArchs :: [ArchImpl]
knownArchs =
    [ ppc32Arch
    , a32Arch
    ]

allArchNames :: [String]
allArchNames = archImplName <$> knownArchs

usage :: IO ()
usage = do
    pn <- IO.getProgName
    putStrLn $ "Usage: " <> pn <> " [options]"
    let msg = unlines [ "\nAt a minimum, specify either a configuration file or the hostname "
                      , "and architecture name options. Command line options are ignored if "
                      , "'-c' is specified."
                      ]
    putStrLn $ usageInfo msg arguments

configFromArgs :: IO Config
configFromArgs = do
    stringArgs <- IO.getArgs
    let (args, rest, errs) = getOpt Permute arguments stringArgs

    when (not $ null $ errs <> rest) $ do
        usage
        IO.exitFailure

    let processArg _ Nothing = Nothing
        processArg arg (Just c) =
            case arg of
                Help ->
                    return $ c { configShowHelp = True }
                ConfigPath p ->
                    return $ c { configPath = Just p }
                Host h ->
                    return $ c { configHost = Just h }
                RunnerPath p ->
                    return $ c { configBinaryPath = p }
                ReportURL u ->
                    return $ c { configReportURL = Just u }
                Arch a ->
                    return $ c { configArchName = Just a }
                ChunkSize s -> do
                    sz <- readMaybe s
                    return $ c { configChunkSize = sz }
                NumThreads s -> do
                    t <- readMaybe s
                    return $ c { configNumThreads = t }
                OpcodeList s -> do
                    let os = T.unpack <$> T.splitOn "," (T.pack s)
                    return $ c { configOpcodes = SpecificOpcodes os }
                LogLevelStr s -> do
                    l <- readMaybe s
                    return $ c { configLogLevel = l }
                Strategy s ->
                    if | s == "random" ->
                           return $ c { configStrategy = Randomized }
                       | s == "roundrobin" ->
                           return $ c { configStrategy = RoundRobin }
                       | otherwise ->
                           Nothing

    case foldr processArg (Just defaultConfig) args of
        Nothing -> usage >> IO.exitFailure
        Just c -> return c

simpleFuzzerConfig :: Config -> Maybe FuzzerConfig
simpleFuzzerConfig cfg = do
    FuzzerConfig <$> configArchName cfg
                 <*> (pure <$> (FuzzerTestHost <$> (configHost cfg)
                                               <*> (pure Nothing)
                                               <*> (pure $ configChunkSize cfg)
                                               <*> (pure $ configBinaryPath cfg)
                                               <*> (pure $ configNumThreads cfg)))
                 <*> (pure $ configOpcodes cfg)
                 <*> (pure $ configStrategy cfg)
                 <*> (pure $ configLogLevel cfg)
                 <*> (pure $ configReportURL cfg)
                 <*> (pure $ configFilters cfg)

mkFuzzerConfig :: Config -> IO FuzzerConfig
mkFuzzerConfig cfg =
    -- Either load a configuration file from disk or build a simple one
    -- from command line arguments.
    case configPath cfg of
        Nothing -> do
            case simpleFuzzerConfig cfg of
                Nothing -> usage >> IO.exitFailure
                Just fc -> return fc
        Just path -> do
            -- Load a configuration from the specified path, then build
            -- a fuzzer config from that.
            result <- E.try $ loadConfig path
            case result of
                Left (e::E.SomeException) -> do
                    putStrLn $ "Error loading config " <> show path <> ": " <> show e
                    IO.exitFailure
                Right (Left e) -> do
                    putStrLn $ "Error loading config " <> show path <> ": " <> e
                    IO.exitFailure
                Right (Right fc) -> do
                    -- The config must specify at least one host.
                    when (null $ fuzzerArchTestingHosts fc) $ do
                        putStrLn $ "Error loading config " <> show path <> ": no hosts specified"
                        IO.exitFailure

                    return fc

main :: IO ()
main = do
    cfg <- configFromArgs

    when (configShowHelp cfg) $
        usage >> IO.exitFailure

    atc <- mkFuzzerConfig cfg

    logCfg <- L.mkLogCfg "main"

    let logFilter = (>= (configLogLevel cfg)) . L.leLevel
    logThread <- CA.async $ do
        L.stdErrLogEventConsumer logFilter logCfg

    let handler (e :: E.SomeException) = do
            L.withLogCfg logCfg $ L.logIO L.Error $
                printf "Exception in 'startHostThreads': %s" (show e)
            L.logEndWith logCfg
            -- I don't understand how this happens, but in practice if
            -- 'startHostThreads' fails with an SSH exception, that
            -- exception gets reraised when we wait on the 'logThread'
            -- here.
            CA.wait logThread `E.catch` \(e :: E.SomeException) ->
                traceIO $ printf "Exception in 'wait logThread': %s"
                (show e)
            IO.exitFailure
    startHostThreads logCfg atc `E.catch` handler
    CA.wait logThread

defaultRunnerPath :: FilePath
defaultRunnerPath = "remote-runner"

filterOpcodes :: forall arch .
                 (ShowF (A.Opcode arch (A.Operand arch)))
              => Proxy arch
              -> OpcodeMatch
              -> [Some ((A.Opcode arch) (A.Operand arch))]
              -> [Some ((A.Opcode arch) (A.Operand arch))]
filterOpcodes _ m os = found
    where
        found = filter matcher os
        matcher = case m of
            AllOpcodes -> const True
            SpecificOpcodes whitelist -> ((`elem` whitelist) . show)

findArch :: String -> Maybe ArchImpl
findArch n =
    case filter ((== n) . archImplName) knownArchs of
        [a] -> return a
        _ -> Nothing

startHostThreads :: L.LogCfg -> FuzzerConfig -> IO ()
startHostThreads logCfg fc = do
  case findArch (fuzzerArchName fc) of
      Nothing -> usage >> IO.exitFailure
      Just arch -> do
          hostThreads <- forM (fuzzerArchTestingHosts fc) $ \hostConfig -> do
              replicateM (fuzzerTestThreads hostConfig) $ do
                  a <- CA.async $ L.named logCfg "testHost" $
                       testHost logCfg fc hostConfig arch
                  CA.link a
                  return a

          mapM_ CA.wait (concat hostThreads)

testHost :: L.LogCfg -> FuzzerConfig -> FuzzerTestHost -> ArchImpl -> IO ()
testHost logCfg mainConfig hostConfig (ArchImpl _ proxy allOpcodes allSemantics allFunctions testSerializer ppInst assemble opcodeFilter) = do
  caseChan <- C.newChan
  resChan <- C.newChan

  let matched = filter opcodeFilter $
                filterOpcodes proxy (fuzzerTestOpcodes mainConfig) allOpcodes

  opcodes <- case matched of
      (o:os) -> return $ NES.fromList o os
      _ -> do
          IO.hPutStrLn IO.stderr $
              "Error: no opcodes in list were found in the ISA for architecture " <>
              show (fuzzerArchName mainConfig)
          IO.exitFailure

  let filterStrs = fuzzerFilters mainConfig
      filters = readFilter proxy <$> filterStrs

  when (any isNothing filters) $ do
      let missing = fst <$> filter (isNothing . snd) (zip (fuzzerFilters mainConfig) filters)
      IO.hPutStrLn IO.stderr $ "Error: could not parse some filters for arch " <>
                               show (fuzzerArchName mainConfig) <> " " <>
                               "(could not parse filters:" <> (show missing) <> ")"
      IO.exitFailure

  let instFilter st = foldl (>>=) (Just st) (catMaybes filters)

  L.withLogCfg logCfg $ L.logIO L.Info $ printf "Filters enabled: %s" (show filterStrs)
  L.withLogCfg logCfg $ L.logIO L.Info $
      printf "Starting up for host %s" (fuzzerTestHostname hostConfig)

  runThread <- CA.async $ L.named logCfg "testRunner" $ do
      L.withLogCfg logCfg $
          testRunner mainConfig hostConfig proxy opcodes (fuzzerTestStrategy mainConfig)
                     allSemantics allFunctions ppInst assemble instFilter caseChan resChan

  CA.link runThread

  L.withLogCfg logCfg $
      CE.runRemote (Just $ fuzzerRunnerPath hostConfig) (fuzzerTestHostname hostConfig)
                   (fuzzerTestUser hostConfig) testSerializer caseChan resChan

  CA.wait runThread

  L.logEndWith logCfg



testRunner :: forall arch .
              ( TemplatableOperand arch
              , A.Architecture arch
              , C.ConcreteArchitecture arch
              , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
              , L.HasLogCfg
              , MapF.OrdF (A.Opcode arch (TemplatedOperand arch))
              , MapF.ShowF (A.Opcode arch (TemplatedOperand arch))
              , Show (GenericInstruction (A.Opcode arch) (A.Operand arch))
              , ShowF (A.Opcode arch (A.Operand arch))
              , EnumF (A.Opcode arch (TemplatedOperand arch))
              , HasRepr (A.Opcode arch (A.Operand arch)) (L.List (A.OperandTypeRepr arch))
              )
           => FuzzerConfig
           -> FuzzerTestHost
           -> Proxy arch
           -> NES.Set (Some ((A.Opcode arch) (A.Operand arch)))
           -> TestStrategy
           -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
           -> [(String, BS.ByteString)]
           -> (GenericInstruction (A.Opcode arch) (A.Operand arch) -> Doc)
           -> (GenericInstruction (A.Opcode arch) (A.Operand arch) -> BSC8.ByteString)
           -> InstFilter arch
           -> C.Chan (Maybe [CE.TestCase (V.ConcreteState arch) (A.Instruction arch)])
           -> C.Chan (CE.ResultOrError (V.ConcreteState arch))
           -> IO ()
testRunner mainConfig hostConfig proxy inputOpcodes strat semantics funcs ppInst assemble ifilt caseChan resChan = do
    self <- getLoginName
    hostname <- getHostName

    let chunkSize = fuzzerTestChunkSize hostConfig
        opcodeSetSequence = case strat of
            Randomized ->
                -- Just use all opcodes on each chunk iteration.
                repeat inputOpcodes
            RoundRobin ->
                -- Make a list of singleton sets, one for each opcode.
                cycle (NES.singleton <$> F.toList inputOpcodes)

    N.withIONonceGenerator $ \nonceGen -> do
      gen <- DA.createGen
      sym :: SB.SimpleBackend s (SB.Flags SB.FloatIEEE)
          <- SB.newSimpleBackend SB.FloatIEEERepr nonceGen
      SB.stopCaching sym

      env <- F.formulaEnv proxy sym
      lib <- F.loadLibrary proxy sym env funcs
      sems <- F.loadFormulas sym env lib semantics
      let generateTestCase fromOpcodes = go maxRetries
              where
                  maxRetries = 100 :: Int
                  go retries | retries < 1 = do
                    L.logIO L.Error $ printf "Giving up after %d retries!" maxRetries
                    return Nothing
                  go retries = do
                    (inst, initialState) <- rejectionSampleMaybe gen ifilt $ \g -> do
                        i <- D.randomInstruction g fromOpcodes
                        s <- C.randomState proxy g
                        return (i, s)

                    let instBytes = assemble inst
                    nonce <- N.indexValue <$> N.freshNonce nonceGen
                    evalResult <- E.try $ evaluateInstruction sym sems inst initialState

                    case evalResult of
                        Left (e::E.SomeException) -> do
                            -- If we get an exception and we're in
                            -- RoundRobin mode, we can't recover!
                            L.logIO L.Error $ printf "Exception evaluating instruction %s: %s (%d retries remaining)" (show inst) (show e) retries
                            if strat == RoundRobin
                               then return Nothing
                               else go (retries-1)
                        Right (Left e) -> do
                            L.logIO L.Error $ printf "Error evaluating instruction %s: %s (%d retries remaining)" (show inst) (show e) retries
                            go (retries-1)
                        Right (Right finalState) -> do
                            return $ Just ( CE.TestCase { CE.testNonce = nonce
                                                        , CE.testProgram = [inst]
                                                        , CE.testContext = initialState
                                                        }
                                          , inst
                                          , instBytes
                                          , initialState
                                          , finalState
                                          )

      forM_ opcodeSetSequence $ \opcodes -> do
          L.logIO L.Info $ printf "Generating %d test cases" chunkSize
          mCases <- replicateM chunkSize (generateTestCase opcodes)

          let caseMap = M.fromList [ (CE.testNonce tc, (inst, instBytes, is, fs)) | (tc, inst, instBytes, is, fs) <- cases ]
              cases = catMaybes mCases

          case null cases of
              True -> do
                  -- In RoundRobin mode, we might hit an opcode for
                  -- which we are unable to ever generate test cases
                  -- (due to semantics issues or evaluator bugs). When
                  -- that happens, cases will be empty and we need to
                  -- just skip that opcode.
                  L.logIO L.Info $ printf "Skipped opcode sequence %s, could not generate any test cases" (show opcodes)
              False -> do
                  -- Send test cases
                  L.logIO L.Info $ printf "Sending %d test cases to remote host" chunkSize
                  C.writeChan caseChan (Just $ (\(tc, _, _, _, _) -> tc) <$> cases)

                  -- Process results
                  L.logIO L.Info "Processing test results"
                  entries <- replicateM (length cases) (handleResult caseMap)

                  -- If a report URL is configured, construct a batch of
                  -- results and upload it to the reporting service.
                  L.logIO L.Info $ "Report URL: " <> show (fuzzerReportURL mainConfig)
                  let numSuccesses = length . filter (\r -> testOutcome r == Success) $
                                     catMaybes entries
                  L.logIO L.Info $ printf "%i/%i test cases succeeded"
                    numSuccesses (length cases)
                  case fuzzerReportURL mainConfig of
                      Nothing -> do
                        -- Report failures to stdout when no fuzzermon
                        forM_ (catMaybes entries) $ \result -> do
                          when (testOutcome result /= Success) $
                            L.logIO L.Warn $ unlines [ "Test failure:"
                                                     , BSC8.unpack $ AE.encodePretty result ]
                      Just reportURL -> do
                          let b = Batch { batchFuzzerHost = hostname
                                        , batchFuzzerUser = fromMaybe self $ fuzzerTestUser hostConfig
                                        , batchTestingHost = fuzzerTestHostname hostConfig
                                        , batchArch = fuzzerArchName mainConfig
                                        , batchEntries = catMaybes entries
                                        }

                          -- Submit batch to report URL
                          let url = reportURL <> "/upload_batch"
                              req = H.postRequestWithBody url "application/json" $
                                    BSC8.unpack $ AE.encode b
                          result <- E.try $ H.simpleHTTP req
                          case result of
                              Left (e::E.SomeException) ->
                                  L.logIO L.Error $ "HTTP exception: " <> show e
                              Right (Left e) ->
                                  L.logIO L.Error $ "HTTP error: " <> show e
                              Right (Right resp) ->
                                  L.logIO L.Debug $ "HTTP response: " <> show resp

      C.writeChan caseChan Nothing

      where
        handleResult caseMap = do
          res <- C.readChan resChan

          case res of
            CE.InvalidTag t -> do
              L.logIO L.Error $ printf "Invalid tag: %d" t
              return Nothing
            CE.TestContextParseFailure -> do
              L.logIO L.Error "Test context parse failure"
              return Nothing
            CE.TestSignalError nonce sig -> do
              let mkOutcome = const $ UnexpectedSignal sig
              return $ Just $ testInfoForNonce proxy ppInst caseMap nonce MapF.empty mkOutcome
            CE.TestReadError tag -> do
              L.logIO L.Error $ printf "Failed with a read error (%d)" tag
              return Nothing
            CE.TestSuccess tr -> do
              let mkOutcome finalSt = if finalSt /= CE.resultContext tr
                                      then Failure
                                      else Success
              return $ Just $ testInfoForNonce proxy ppInst caseMap (CE.resultNonce tr) (CE.resultContext tr) mkOutcome

testInfoForNonce :: forall arch .
                    ( A.Architecture arch
                    , ShowF (A.Opcode arch (A.Operand arch))
                    , ShowF (A.Operand arch)
                    , ShowF (A.Location arch)
                    )
                 => Proxy arch
                 -> (GenericInstruction (A.Opcode arch) (A.Operand arch) -> Doc)
                 -> M.Map Word64 ( GenericInstruction (A.Opcode arch) (A.Operand arch)
                                 , BSC8.ByteString
                                 , MapF.MapF (A.Location arch) V.Value
                                 , MapF.MapF (A.Location arch) V.Value
                                 )
                 -> Word64
                 -> MapF.MapF (A.Location arch) V.Value
                 -> (MapF.MapF (A.Location arch) V.Value -> TestOutcome)
                 -> TestInfo
testInfoForNonce proxy ppInst caseMap nonce actualFinal mkOutcome =
    let Just (inst, instBytes, initialState, expectedFinal) = M.lookup nonce caseMap
    in case inst of
        Instruction opcode operands ->
            let inputs = uncurry TestInput <$> statePairs proxy initialState
                sd = stateDiff proxy expectedFinal actualFinal
                states = catMaybes $ mkState <$> sd
                mkState :: (Some (A.Location arch), (Maybe (Some V.Value), Maybe (Some V.Value))) -> Maybe TestState
                mkState (loc, (Just (Some (V.ValueBV expected)), Just (Some (V.ValueBV actual)))) =
                    Just $ TestState { testLocation = show loc
                                     , testExpected = show $ W.unW expected
                                     , testActual = show $ W.unW actual
                                     }
                mkState _ = Nothing
            in TestInfo { testOutcome = mkOutcome expectedFinal
                        , testInfoOpcode = showF opcode
                        , testInfoRawOperands = show operands
                        , testInfoPretty = render $ ppInst inst
                        , testInfoInstructionBytes = showBS $ BSC8.toStrict instBytes
                        , testInfoStates = states
                        , testInfoInputs = inputs
                        }
