{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Applicative ((<|>))
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import           Control.Monad (replicateM_, replicateM, forM_, when, forM)
import qualified Control.Exception as E
import qualified Data.Ini.Config as CI
import qualified Data.Aeson as AE
import qualified Data.Foldable as F
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
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

import           Data.Parameterized.Some (Some(..))
import           Data.Parameterized.Classes (ShowF(..))
import qualified Data.Parameterized.Nonce as N
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.HasRepr (HasRepr)
import qualified Data.Parameterized.List as L

import qualified Lang.Crucible.Solver.SimpleBackend as SB
import qualified Lang.Crucible.Solver.Interface as SB

import qualified Dismantle.Arbitrary as DA
import           Dismantle.Instruction (GenericInstruction)
import qualified Dismantle.Instruction.Random as D

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
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch
                                          , unTemplate, TemplatableOperand
                                          , TemplatedOperand
                                          )

data OpcodeMatch =
    AllOpcodes
    | SpecificOpcodes [String]
    deriving (Show, Eq, Read)

data TestStrategy =
    RoundRobin
    | Randomized
    deriving (Show, Eq, Read)

data FuzzerConfig =
    FuzzerConfig { fuzzerArchName :: String
                 , fuzzerArchTestingHosts :: [FuzzerTestHost]
                 , fuzzerTestOpcodes :: OpcodeMatch
                 , fuzzerTestStrategy :: TestStrategy
                 , fuzzerMaximumLogLevel :: L.LogLevel
                 , fuzzerReportURL :: Maybe String
                 }
                 deriving (Show)

data FuzzerTestHost =
    FuzzerTestHost { fuzzerTestHostname :: String
                   , fuzzerTestChunkSize :: Int
                   , fuzzerRunnerPath :: FilePath
                   , fuzzerTestThreads :: Int
                   }
                   deriving (Show)

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
           }

loadConfig :: FilePath -> IO (Either String FuzzerConfig)
loadConfig cfgPath = do
    txt <- T.readFile cfgPath
    return $ CI.parseIniFile txt parser

hostSectionPrefix :: T.Text
hostSectionPrefix = "host:"

parser :: CI.IniParser FuzzerConfig
parser = do
    (arch, op, strat, ll, url) <- CI.section "fuzzer" $ do
        arch  <- T.unpack <$> CI.field "arch"
        op    <- CI.fieldDefOf "opcodes" CI.readable (configOpcodes defaultConfig)
        strat <- CI.fieldDefOf "strategy" CI.readable (configStrategy defaultConfig)
        url   <- (Just <$> CI.field "report-url") <|>
                 (return $ T.pack <$> configReportURL defaultConfig)
        ll    <- CI.fieldDefOf "log-level" CI.readable (configLogLevel defaultConfig)
        return (arch, op, strat, ll, url)

    when (not $ arch `elem` allArchNames) $
        fail $ "Invalid architecture name: " <> arch

    let hostSection = T.stripPrefix hostSectionPrefix
    hosts <- CI.sectionsOf hostSection $ \hostname -> do
        FuzzerTestHost (T.unpack hostname)
            <$> CI.fieldDefOf "chunk-size" CI.readable (configChunkSize defaultConfig)
            <*> (T.unpack <$> CI.fieldDef "runner-path" (T.pack $ configBinaryPath defaultConfig))
            <*> (CI.fieldDefOf "threads" CI.readable (configNumThreads defaultConfig))

    return FuzzerConfig { fuzzerArchName = arch
                        , fuzzerArchTestingHosts = F.toList hosts
                        , fuzzerTestOpcodes = op
                        , fuzzerTestStrategy = strat
                        , fuzzerMaximumLogLevel = ll
                        , fuzzerReportURL = T.unpack <$> url
                        }

data ArchImpl where
    ArchImpl :: forall proxy arch .
                ( TemplatableOperand arch
                , A.Architecture arch
                , C.ConcreteArchitecture arch
                , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
                , MapF.OrdF (A.Opcode arch (TemplatedOperand arch))
                , MapF.ShowF (A.Opcode arch (TemplatedOperand arch))
                , Show (GenericInstruction (A.Opcode arch) (A.Operand arch))
                , EnumF (A.Opcode arch (TemplatedOperand arch))
                , HasRepr (A.Opcode arch (A.Operand arch)) (L.List (A.OperandTypeRepr arch))
                )
             => String
             -> proxy arch
             -> [Some ((A.Opcode arch) (A.Operand arch))]
             -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
             -> CE.TestSerializer (V.ConcreteState arch) (A.Instruction arch)
             -> ArchImpl

ppc32Arch :: ArchImpl
ppc32Arch =
    ArchImpl "ppc32"
             (Proxy @PPCS.PPC)
             PPCS.allOpcodes
             PPCS.allSemantics
             PPCS.testSerializer

knownArchs :: [ArchImpl]
knownArchs =
    [ ppc32Arch
    ]

allArchNames :: [String]
allArchNames = archImplName <$> knownArchs

archImplName :: ArchImpl -> String
archImplName (ArchImpl n _ _ _ _) = n

usage :: IO ()
usage = do
    pn <- IO.getProgName
    putStrLn $ "Usage: " <> pn <> " [options]"
    let msg = unlines [ "\nAt a minimum, specify either a configuration file or the hostname "
                      , "and architecture name options."
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
simpleFuzzerConfig cfg =
    FuzzerConfig <$> configArchName cfg
                 <*> (pure <$> (FuzzerTestHost <$> (configHost cfg)
                                               <*> (pure $ configChunkSize cfg)
                                               <*> (pure $ configBinaryPath cfg)
                                               <*> (pure $ configNumThreads cfg)))
                 <*> (pure $ configOpcodes cfg)
                 <*> (pure $ configStrategy cfg)
                 <*> (pure $ configLogLevel cfg)
                 <*> (pure $ configReportURL cfg)

mkFuzzerConfig :: Config -> IO FuzzerConfig
mkFuzzerConfig cfg =
    -- Either load a configuration file from disk or build a simple one
    -- from command line arguments.
    case configPath cfg of
        Nothing -> case simpleFuzzerConfig cfg of
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

    startHostThreads logCfg atc

    CA.wait logThread

defaultRunnerPath :: FilePath
defaultRunnerPath = "remote-runner"

filterOpcodes :: forall proxy arch .
                 (ShowF (A.Opcode arch (A.Operand arch)))
              => proxy arch
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
                  a <- CA.async $ testHost logCfg fc hostConfig arch
                  CA.link a
                  return a

          mapM_ CA.wait (concat hostThreads)

testHost :: L.LogCfg -> FuzzerConfig -> FuzzerTestHost -> ArchImpl -> IO ()
testHost logCfg mainConfig hostConfig (ArchImpl _ proxy allOpcodes allSemantics testSerializer) = do
  caseChan <- C.newChan
  resChan <- C.newChan

  L.withLogCfg logCfg $ L.logIO L.Info $
      printf "Starting up for host %s" (fuzzerTestHostname hostConfig)

  let matched = filterOpcodes proxy (fuzzerTestOpcodes mainConfig) allOpcodes

  opcodes <- case matched of
      (o:os) -> return $ NES.fromList o os
      _ -> do
          IO.hPutStrLn IO.stderr "BUG: empty opcode list"
          IO.exitFailure

  runThread <- CA.async $ do
      L.withLogCfg logCfg $
          testRunner hostConfig proxy opcodes (fuzzerTestStrategy mainConfig)
                     allSemantics caseChan resChan

  CA.link runThread

  L.withLogCfg logCfg $
      CE.runRemote (Just $ fuzzerRunnerPath hostConfig) (fuzzerTestHostname hostConfig)
                   testSerializer caseChan resChan

  CA.wait runThread

  L.logEndWith logCfg

makePlain :: forall arch sym
           . (MapF.OrdF (A.Opcode arch (A.Operand arch)),
              MapF.OrdF (A.Location arch))
          => BaseSet sym arch
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
makePlain = MapF.foldrWithKey f MapF.empty
  where f :: forall sh
           . A.Opcode arch (A.Operand arch) sh
          -> F.ParameterizedFormula sym (TemplatedArch arch) sh
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
          -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula sym arch)
        f op pf = MapF.insert op (unTemplate pf)

testRunner :: forall proxy arch .
              ( TemplatableOperand arch
              , A.Architecture arch
              , C.ConcreteArchitecture arch
              , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
              , L.HasLogCfg
              , MapF.OrdF (A.Opcode arch (TemplatedOperand arch))
              , MapF.ShowF (A.Opcode arch (TemplatedOperand arch))
              , Show (GenericInstruction (A.Opcode arch) (A.Operand arch))
              , EnumF (A.Opcode arch (TemplatedOperand arch))
              , HasRepr (A.Opcode arch (A.Operand arch)) (L.List (A.OperandTypeRepr arch))
              )
           => FuzzerTestHost
           -> proxy arch
           -> NES.Set (Some ((A.Opcode arch) (A.Operand arch)))
           -> TestStrategy
           -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
           -> C.Chan (Maybe [CE.TestCase (V.ConcreteState arch) (A.Instruction arch)])
           -> C.Chan (CE.ResultOrError (V.ConcreteState arch))
           -> IO ()
testRunner hostConfig proxy inputOpcodes strat semantics caseChan resChan = do
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
      sym :: SB.SimpleBackend s
          <- SB.newSimpleBackend nonceGen
      SB.stopCaching sym

      baseSet <- F.loadFormulas sym semantics
      let plainBaseSet :: MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (SB.SimpleBackend s) arch)
          plainBaseSet = makePlain baseSet

          generateTestCase fromOpcodes = go
              where
                  go = do
                    inst <- D.randomInstruction gen fromOpcodes
                    initialState <- C.randomState proxy gen
                    nonce <- N.indexValue <$> N.freshNonce nonceGen
                    evalResult <- E.try $ evaluateInstruction sym plainBaseSet inst initialState

                    case evalResult of
                        Left (e::E.SomeException) -> do
                            -- If we get an exception and we're in
                            -- RoundRobin mode, we can't recover!
                            L.logIO L.Error $ printf "Exception evaluating instruction %s: %s" (show inst) (show e)
                            if strat == RoundRobin
                               then return Nothing
                               else do
                                   L.logIO L.Error $ printf "Exception evaluating instruction %s: %s" (show inst) (show e)
                                   go
                        Right (Left e) -> do
                            L.logIO L.Error $ printf "Error evaluating instruction %s: %s" (show inst) (show e)
                            go
                        Right (Right finalState) -> do
                            return $ Just ( CE.TestCase { CE.testNonce = nonce
                                                        , CE.testProgram = [inst]
                                                        , CE.testContext = initialState
                                                        }
                                          , inst
                                          , finalState
                                          )

      forM_ opcodeSetSequence $ \opcodes -> do
          L.logIO L.Info $ printf "Generating %d test cases" chunkSize
          mCases <- replicateM chunkSize (generateTestCase opcodes)

          let caseMap = M.fromList [ (CE.testNonce tc, (inst, fs)) | (tc, inst, fs) <- cases ]
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
                  C.writeChan caseChan (Just $ (\(tc, _, _) -> tc) <$> cases)

                  -- Process results
                  L.logIO L.Info "Processing test results"
                  replicateM_ (length cases) (handleResult caseMap)

      C.writeChan caseChan Nothing

      where
        handleResult caseMap = do
          res <- C.readChan resChan

          case res of
            CE.InvalidTag t -> do
              L.logIO L.Error $ printf "Invalid tag: %d" t
            CE.TestContextParseFailure -> do
              L.logIO L.Error "Test context parse failure"
            CE.TestSignalError nonce sig -> do
              L.logIO L.Error $ printf "Failed with unexpected signal (%d) on test case %d" sig nonce
            CE.TestReadError tag -> do
              L.logIO L.Error $ printf "Failed with a read error (%d)" tag
            CE.TestSuccess tr -> do
              let Just (inst, expectedFinal) = M.lookup (CE.resultNonce tr) caseMap
              if (expectedFinal /= CE.resultContext tr)
                 then L.logIO L.Error $ printf "ERROR: Context mismatch for instruction %s: diff: %s"
                          (show inst) (show $ stateDiff proxy expectedFinal (CE.resultContext tr))
                 else L.logIO L.Info "SUCCESS"

stateDiff :: (A.Architecture arch)
          => proxy arch
          -> V.ConcreteState arch
          -> V.ConcreteState arch
          -> [(Some (A.Location arch), (Maybe (Some V.Value), Maybe (Some V.Value)))]
stateDiff _ a b =
    let aKeys = S.fromList $ MapF.keys a
        bKeys = S.fromList $ MapF.keys b
        allKeys = S.union aKeys bKeys
        pairs = [ (k,) <$>
                  case k of
                    Some v ->
                      let aVal = MapF.lookup v a
                          bVal = MapF.lookup v b
                      in if aVal == bVal
                         then Nothing
                         else Just (Some <$> aVal, Some <$> bVal)
                | k <- S.toList allKeys
                ]
    in catMaybes pairs

data Batch =
    Batch { batchFuzzerHost :: String
          , batchFuzzerUser :: String
          , batchTestingHost :: String
          , batchArch :: String
          , batchEntries :: [BatchEntry]
          }

instance AE.ToJSON Batch where
    toJSON b =
        AE.object [ "fuzzer-host" AE..= batchFuzzerHost b
                  , "fuzzer-user" AE..= batchFuzzerUser b
                  , "testing-host" AE..= batchTestingHost b
                  , "arch" AE..= batchArch b
                  , "entries" AE..= batchEntries b
                  ]

data BatchEntry = Success TestSuccess
                | Failure TestFailure

instance AE.ToJSON BatchEntry where
    toJSON (Success s) = AE.toJSON s
    toJSON (Failure f) = AE.toJSON f

data TestSuccess =
    TestSuccess { testSuccessOpcode :: String
                , testSuccessCount :: Int
                }

instance AE.ToJSON TestSuccess where
    toJSON s =
        AE.object [ "type" AE..= ("success"::T.Text)
                  , "opcode" AE..= testSuccessOpcode s
                  , "count" AE..= testSuccessCount s
                  ]

data TestFailure =
    TestFailure { testFailureOpcode :: String
                , testFailureOperands :: String
                , testFailureStates :: [TestFailureState]
                }

instance AE.ToJSON TestFailure where
    toJSON s =
        AE.object [ "type" AE..= ("failure"::T.Text)
                  , "opcode" AE..= testFailureOpcode s
                  , "operands" AE..= testFailureOperands s
                  , "state" AE..= testFailureStates s
                  ]

data TestFailureState =
    TestFailureState { testFailureLocation :: String
                     , testFailureExpected :: String
                     , testFailureActual :: String
                     }

instance AE.ToJSON TestFailureState where
    toJSON s =
        AE.object [ "location" AE..= testFailureLocation s
                  , "expected" AE..= testFailureExpected s
                  , "actual" AE..= testFailureActual s
                  ]
