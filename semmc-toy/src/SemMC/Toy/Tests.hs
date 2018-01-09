{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
module SemMC.Toy.Tests where

import qualified Control.Concurrent.Chan as C
import qualified Data.Foldable as F
import           Data.IORef ( newIORef )
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import           System.FilePath

import qualified GHC.Err.Located as L

import           Data.Parameterized.Classes
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import           Dismantle.Tablegen.TH.Capture ( captureDictionaries )
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import           SemMC.Formula
import qualified SemMC.Concrete.Execution as CE
import           SemMC.Stochastic.Strata
import qualified SemMC.Stochastic.CandidateProgram as CP
import qualified SemMC.Stochastic.IORelation.Types as I
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Stochastic.Synthesize as S
import           SemMC.Synthesis
import           SemMC.Synthesis.Template
import           SemMC.Toy as T
import           SemMC.Util as U

import           Debug.Trace

allOperands :: [Some (T.Opcode T.Operand)]
allOperands = $(captureDictionaries (const True) ''T.Opcode)

readBinOpc :: SimpleBackend t
           -> Opcode Operand sh
           -> IO (Either String (ParameterizedFormula (SimpleBackend t) Toy sh))
readBinOpc sym opc = readFormulaFromFile sym env (HR.typeRepr opc) ("toy-semantics" </> show opc <.> "sem")
  where
    env = FormulaEnv Map.empty undefined

doThing :: IO ()
doThing = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOpc sym AddRr
  Right sub <- readBinOpc sym SubRr
  Right movi <- readBinOpc sym MovRi
  let opcodes = MapF.insert AddRr add
              $ MapF.insert SubRr sub
              $ MapF.insert MovRi movi
              $ MapF.empty
  -- print =<< instantiateFormula sym pf (R32 Reg1 :> R32 Reg3 :> Nil)
  let templated = templatedInstructions (toBaseSet opcodes)
  mapM_ print templated
  -- (f1 : f2 : _) <- templatizeFormula' sym pf
  -- print =<< sequenceFormulas sym (tfFormula f1) (tfFormula f2)

-- Formula for moving 2*r1 into r2. The assembly I have in mind:
--
-- > SubRr r2, r2
-- > AddRr r2, r1
-- > AddRr r2, r1
--
fooFormula :: (ShowF (S.SymExpr sym)) => (S.IsSymInterface sym, S.IsExprBuilder sym) => sym -> IO (Formula sym Toy)
fooFormula sym = do
  reg1 <- S.freshBoundVar sym (makeSymbol (show Reg1)) (A.locationType (RegLoc Reg1))
  twoLit <- S.bvLit sym (knownNat :: NatRepr 32) 2
  reg1TimesTwo <- S.bvMul sym twoLit (S.varExpr sym reg1)
  reg2Def <- S.bvAdd sym reg1TimesTwo twoLit
  return $ Formula { formParamVars = MapF.insert (RegLoc Reg1) reg1
                                   $ MapF.empty
                   , formDefs = MapF.insert (RegLoc Reg2) reg2Def
                              $ MapF.empty
                   }

independentFormula :: (ShowF (S.SymExpr sym)) => (S.IsSymInterface sym, S.IsExprBuilder sym) => sym -> IO (Formula sym Toy)
independentFormula sym = do
  reg1 <- S.freshBoundVar sym (makeSymbol (show Reg1)) (A.locationType (RegLoc Reg1))
  reg2 <- S.freshBoundVar sym (makeSymbol (show Reg2)) (A.locationType (RegLoc Reg2))
  twoLit <- S.bvLit sym (knownNat :: NatRepr 32) 2
  -- reg1TimesTwo <- S.bvMul sym twoLit (S.varExpr sym reg1)
  reg1Def <- S.bvMul sym (S.varExpr sym reg1) twoLit
  -- reg2TimesTwo <- S.bvMul sym twoLit (S.varExpr sym reg2)
  reg2Def <- S.bvMul sym (S.varExpr sym reg2) twoLit
  return $ Formula { formParamVars = MapF.insert (RegLoc Reg1) reg1
                                   $ MapF.insert (RegLoc Reg2) reg2
                                   $ MapF.empty
                   , formDefs = MapF.insert (RegLoc Reg2) reg2Def
                              $ MapF.insert (RegLoc Reg1) reg1Def
                              $ MapF.empty
                   }

dependentFormula :: (ShowF (S.SymExpr sym)) => (S.IsSymInterface sym, S.IsExprBuilder sym) => sym -> IO (Formula sym Toy)
dependentFormula sym = do
  reg1 <- S.freshBoundVar sym (makeSymbol (show Reg1)) (A.locationType (RegLoc Reg1))
  -- reg2 <- S.freshBoundVar sym (makeSymbol (show Reg2)) (A.locationType (RegLoc Reg2))
  twoLit <- S.bvLit sym (knownNat :: NatRepr 32) 2
  reg1TimesTwo <- S.bvMul sym twoLit (S.varExpr sym reg1)
  reg1Def <- S.bvAdd sym reg1TimesTwo twoLit
  reg1TimesTwo <- S.bvMul sym twoLit (S.varExpr sym reg1)
  reg2Def <- S.bvAdd sym reg1TimesTwo twoLit
  return $ Formula { formParamVars = MapF.insert (RegLoc Reg1) reg1
                                   $ MapF.empty
                   , formDefs = MapF.insert (RegLoc Reg2) reg2Def
                              $ MapF.insert (RegLoc Reg1) reg1Def
                              $ MapF.empty
                   }

doThing2 :: IO ()
doThing2 = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOpc sym AddRr
  Right sub <- readBinOpc sym SubRr
  Right movi <- readBinOpc sym MovRi
  let baseset = MapF.insert AddRr add
              $ MapF.insert SubRr sub
              $ MapF.insert MovRi movi
              $ MapF.empty
  -- target <- fooFormula sym
  target <- independentFormula sym

  let env = setupEnvironment sym (toBaseSet baseset)
  print =<< mcSynth env target
  print $ extractUsedLocs (formParamVars target) (fromJust $ MapF.lookup (RegLoc Reg2) $ formDefs target)

doThing3 :: IO ()
doThing3 = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOpc sym AddRr
  putStrLn $ T.unpack $ printParameterizedFormula (HR.typeRepr AddRr) add

doThing4 :: IO ()
doThing4 = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOpc sym AddRr
  print add
  Right sub <- readBinOpc sym SubRr
  Right movi <- readBinOpc sym MovRi
  let baseset = MapF.insert AddRr add
              $ MapF.insert SubRr sub
              $ MapF.insert MovRi movi
              $ MapF.empty

  ind <- independentFormula sym
  let env = setupEnvironment sym (toBaseSet baseset)
  print =<< mcSynth env ind

----------------------------------------------------------------
-- * Stratefied synthesis

-- Goal: run 'synthesize' directly and see if we learn a program that
-- passes the tests. Remaining work
--
-- - initialize LocalSynEnv, including nested SynEnv
--
--   - [ ] initial test cases
--     - [X] random
--     - [ ] heuristic
--
--   - [X] base set with semantics
--
--   - [X] test runner with backend thread running tests

{-
-- | The initial tests, including random tests and "heuristically
-- interesting" tests. STRATA Section 3.3.
--
-- During synthesis these tests get augmented by new tests discovered
-- by the SMT solver that distinguish candidates that are equal on all
-- tests so far.
--
-- XXX: right now the 'loadInitialState' takes a list of heuristically
-- interesting tests, a test generator, and number of tests to
-- generate as arguments (some in the config).
generateTestCases :: (C.ConcreteArchitecture arch)
                  => proxy arch -> A.Gen -> IO [Test arch]
generateTestCases p gen = do
  let numRandomTests = 1024
  randomTests <- replicateM numRandomTests (C.randomState p gen)
  heuristicTests <- return [] -- TODO
  return $ randomTests ++ heuristicTests
-}

-- | Test runner backend for Toy arch.
toyTestRunnerBackend :: arch ~ Toy => Integer -> I.TestRunner arch
toyTestRunnerBackend !i tChan rChan = do
  maybeTests <- C.readChan tChan
  case maybeTests of
    Nothing -> return ()
    Just tests -> do
      i' <- F.foldlM evaluateTest i tests
      toyTestRunnerBackend i' tChan rChan
  where
    _debug i msg = traceIO $ "toyTestRunnerBackend: "++show i++": "++msg
    evaluateTest ix test = do
      let resultContext = evalProg (CE.testContext test) (CE.testProgram test)
      let result = CE.TestResult
            { CE.resultNonce = CE.testNonce test
            , CE.resultContext = resultContext
            }
      C.writeChan rChan (CE.TestSuccess result)
      return (ix + 1)

{-
  logCfg <- L.mkLogCfg
  loggerThread <- C.async $ logEventConsumer logCfg
  C.link loggerThread
-}

-- | Configuration for 'runSynToy'.
--
-- E.g. limit the 'opcodes' to speed up the search.
data RunSynToyCfg =
  RunSynToyCfg
  { rstOpcodes :: [ Some (Opcode Operand) ]
    -- ^ Known opcodes. Use 'Toy.allOpcodes' if you want them all.
  , rstPseudoOpcodes :: [ Some (P.Pseudo Toy Operand) ]
    -- ^ Psuedo opcodes. Use 'Toy.allPseudoOpcodes' if you want them all.
  , rstTargetOpcodes :: [ Some (Opcode Operand) ]
    -- ^ Opcodes to learn semantics for.
  }
-- | Default value for 'RunSyntoyCfg'.
defaultRunSynToyCfg :: RunSynToyCfg
defaultRunSynToyCfg = RunSynToyCfg
  { rstOpcodes = Set.toList T.allOpcodes
  , rstPseudoOpcodes = T.allPseudoOpcodes
  , rstTargetOpcodes = []
  }

-- | Initialize a 'LocalSynEnv' for the toy arch and run a toy 'Syn'
-- action in it.
--
-- WARNING: this initializer doesn't consider the possibility that the
-- action run in this env (with 'runSyn') has any on-disk side
-- effects, e.g. writing a file. If we want on disk side effects, then
-- we should change this to set up the on-disk test env in a tmp dir.
runSynToy :: (U.HasLogCfg)
          => RunSynToyCfg
          -> FilePath -- ^ Root dir for test data, e.g. semantics.
          -> (forall t. Syn t Toy a)
          -> IO a
runSynToy rstCfg dataRoot action = do
  stThread <- newStatisticsThread (dataRoot </> "stats.sqlite")
  let cfg :: Config Toy
      cfg = Config
        { baseSetDir = dataRoot </> "base"
        , pseudoSetDir = dataRoot </> "pseudo"
        , learnedSetDir = dataRoot </> "learned"
        , programCountThreshold = L.error "programCountThreshold"
        , randomTestCount = 1024
        , remoteRunnerTimeoutSeconds = 20
        , opcodeTimeoutSeconds = 600
        , parallelOpcodes = 1
        , parallelSynth = 1
        , testRunner = toyTestRunnerBackend 0 :: I.TestRunner Toy
        , logConfig = getLogCfg
        , statsThread = stThread
        }

  {-
  testCases <- generateTestCases p gen
  seTestCases <- C.newTVarIO testCases
  -- seFormulas <- C.newTVarIO $ MapF.fromList []
  let synEnv :: SynEnv t Toy -- What's @t@?
      synEnv = SynEnv
        { seFormulas = L.error "seFormulas"
        , sePseudoFormulas = L.error "sePseudoFormulas"
        , seKnownCongruentOps = L.error "seKnownCongruentOps"
        , seWorklist = L.error "seWorklist"
        , seTestCases = seTestCases
        , seIORelations = ioRelations
        , seSymBackend = L.error "seSymBackend"
        , seStatsThread = L.error "seStatsThread"
        , seConfig = cfg
        }
  -}

  -- The way the opcode list and semantic files on disk interact is a
  -- little weird: the opcode list bounds the possible semantic files
  -- we look for. I.e., the instructions with known semantics are the
  -- intersection of the opcode lists here and the semantic files on
  -- disk.
  let allOpcodes = rstOpcodes rstCfg
  let pseudoOpcodes = rstPseudoOpcodes rstCfg
  let targetOpcodes = rstTargetOpcodes rstCfg

  withInitialState cfg allOpcodes pseudoOpcodes targetOpcodes ioRelations $ \synEnv -> do
  gen <- A.createGen
  nref <- newIORef 0
  tChan <- C.newChan :: IO (C.Chan (Maybe [I.TestCase Toy]))
  rChan <- C.newChan
  U.withAsyncLinked (testRunner cfg tChan rChan) $ const $ do
  let localSynEnv = LocalSynEnv
        { seGlobalEnv = synEnv
        , seRandomGen = gen
        , seTestChan = tChan
        , seResChan = rChan
        , seNonceSource = nref
        }
  res <- runSyn localSynEnv action
  terminateStatisticsThread stThread
  return res

-- | Synthesize a single candidate program that agrees with the target
-- program on the tests.
--
-- This is the inner loop of stratified synthesis, and candidates
-- generated this way are then proven equivalent to build confidence
-- that they implement the target on all possible inputs.
--
-- The test is randomized, and the time it takes to succeed varies
-- significantly from run to run. Interpreted it usually finishes in
-- under a minute, and compiled in under 5 seconds.
test_synthesizeCandidate :: (U.HasLogCfg)
                         => IO (Maybe [P.SynthInstruction Toy])
test_synthesizeCandidate = do
  let ops = (R32 Reg1 SL.:< R32 Reg2 SL.:< SL.Nil)
  let instruction = AC.RI { AC.riInstruction = D.Instruction AddRr ops
                         , AC.riOpcode = AddRr
                         , AC.riOperands = ops
                         , AC.riLiteralLocs = MapF.empty
                         }
  let rstCfg = defaultRunSynToyCfg
        { rstOpcodes = [ Some SubRr
                       , Some NegR ]
        , rstPseudoOpcodes = []
        , rstTargetOpcodes = [ Some AddRr ] }
  runSynToy rstCfg "tests/data/test_synthesizeCandidate" $ do
    (res, _) <- withTimeout (S.synthesize instruction)
    return (fmap CP.cpInstructions res)

-- | Weigh a candidate that produces the right value in the wrong
-- place.
--
-- The weight should be penalty * number of test cases.
--
-- Returns @(<expected weight>, <actual weight>)@.
test_rightValueWrongPlace :: (U.HasLogCfg) => IO (Double, Double)
test_rightValueWrongPlace = do
  runSynToy defaultRunSynToyCfg "tests/data/test_rightValueWrongPlace" $ do
    tests <- askTestCases
    (targetTests, targetResults) <- S.computeTargetResults target tests
    weight <- S.weighCandidate target targetTests targetResults candidate
    let expectedWeight =
          S.wrongPlacePenalty * fromIntegral (length tests)
    return (expectedWeight, weight)
  where
    -- Add r1 and r2 and store the result in *r3*, and then set r1 to
    -- a value as different as possible from r3, i.e. the complement.
    candidate = S.fromList $ map (Just . P.actualInsnToSynth) $
      -- Add r1 and r2 and store in r3.
      [ D.Instruction SubRr (R32 Reg3 SL.:< R32 Reg3 SL.:< SL.Nil)
      , D.Instruction AddRr (R32 Reg3 SL.:< R32 Reg1 SL.:< SL.Nil)
      , D.Instruction AddRr (R32 Reg3 SL.:< R32 Reg2 SL.:< SL.Nil)

      -- Set r1 to xthe complement of r3!
      , D.Instruction MovRi (R32 Reg1 SL.:< I32 0 SL.:< SL.Nil)
      , D.Instruction AddRr (R32 Reg1 SL.:< R32 Reg3 SL.:< SL.Nil)
      , D.Instruction NotR  (R32 Reg1 SL.:< SL.Nil) ]

    -- Add r1 and r2 and store the result in *r1*.
    ops = (R32 Reg1 SL.:< R32 Reg2 SL.:< SL.Nil)
    target = AC.RI { AC.riInstruction = D.Instruction AddRr ops
                   , AC.riOpcode = AddRr
                   , AC.riOperands = ops
                   , AC.riLiteralLocs = MapF.empty
                   }
