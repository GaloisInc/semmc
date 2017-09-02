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
module SemMC.ToyExample.Tests where

import qualified Control.Concurrent.Async as C
import qualified Control.Concurrent.Chan as C
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as T

import qualified GHC.Err.Located as L

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.ShapedList ( ShapedList(..) )
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Unfold as U
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import           Dismantle.Tablegen.TH ( captureDictionaries )
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend

import           SemMC.Architecture
import           SemMC.Formula
import qualified SemMC.Concrete.State as C
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Stochastic.IORelation.Types as I
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Stochastic.Strata as S
import qualified SemMC.Stochastic.Synthesize as S
import           SemMC.Synthesis
import           SemMC.Synthesis.Template
import           SemMC.ToyExample as T
import           SemMC.Util

import Debug.Trace

allOperands :: [Some (Witness U.UnfoldShape (T.Opcode T.Operand))]
allOperands = $(captureDictionaries (const True) ''T.Opcode)

readBinOp :: forall t. SimpleBackend t -> FilePath -> IO (Either String (ParameterizedFormula (SimpleBackend t) (TemplatedArch Toy) '["R32", "R32"]))
readBinOp sym fp = readFormulaFromFile sym (FormulaEnv Map.empty undefined) ("data/toy/base/" <> fp)

readBinOp' :: forall t. SimpleBackend t -> FilePath -> IO (Either String (ParameterizedFormula (SimpleBackend t) (TemplatedArch Toy) '["R32", "I32"]))
readBinOp' sym fp = readFormulaFromFile sym (FormulaEnv Map.empty undefined) ("data/toy/base/" <> fp)

doThing :: IO ()
doThing = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOp sym "AddRr.sem"
  Right sub <- readBinOp sym "SubRr.sem"
  Right movi <- readBinOp' sym "MovRi.sem"
  let opcodes = MapF.insert (Witness AddRr) add
              $ MapF.insert (Witness SubRr) sub
              $ MapF.insert (Witness MovRi) movi
              $ MapF.empty
  -- print =<< instantiateFormula sym pf (R32 Reg1 :> R32 Reg3 :> Nil)
  let templated = templatedInstructions opcodes
  print $ templated !! 50
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
  reg1 <- S.freshBoundVar sym (makeSymbol (show Reg1)) (locationType (RegLoc Reg1))
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
  reg1 <- S.freshBoundVar sym (makeSymbol (show Reg1)) (locationType (RegLoc Reg1))
  reg2 <- S.freshBoundVar sym (makeSymbol (show Reg2)) (locationType (RegLoc Reg2))
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
  reg1 <- S.freshBoundVar sym (makeSymbol (show Reg1)) (locationType (RegLoc Reg1))
  -- reg2 <- S.freshBoundVar sym (makeSymbol (show Reg2)) (locationType (RegLoc Reg2))
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
  Right add <- readBinOp sym "AddRr.sem"
  Right sub <- readBinOp sym "SubRr.sem"
  Right movi <- readBinOp' sym "MovRi.sem"
  let baseset = MapF.insert (Witness AddRr) add
              $ MapF.insert (Witness SubRr) sub
              $ MapF.insert (Witness MovRi) movi
              $ MapF.empty
  -- target <- fooFormula sym
  target <- independentFormula sym

  let env = setupEnvironment sym baseset
  print =<< mcSynth env target
  print $ extractUsedLocs (formParamVars target) (fromJust $ MapF.lookup (RegLoc Reg2) $ formDefs target)

doThing3 :: IO ()
doThing3 = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOp sym "AddRr.sem"
  putStrLn $ T.unpack $ printFormula add

doThing4 :: IO ()
doThing4 = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOp sym "AddRr.sem"
  print add
  Right sub <- readBinOp sym "SubRr.sem"
  Right movi <- readBinOp' sym "MovRi.sem"
  let baseset = MapF.insert (Witness AddRr) add
              $ MapF.insert (Witness SubRr) sub
              $ MapF.insert (Witness MovRi) movi
              $ MapF.empty

  ind <- independentFormula sym
  let env = setupEnvironment sym baseset
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
toyTestRunnerBackend !i tChan rChan _logChan = do
  maybeTest <- C.readChan tChan
  case maybeTest of
    Nothing -> return Nothing
    Just test -> do
      let resultContext = evalProg (CE.testContext test) (CE.testProgram test)
      let result = CE.TestResult
            { CE.resultNonce = CE.testNonce test
            , CE.resultContext = resultContext
            }
      C.writeChan rChan (CE.TestSuccess result)
      toyTestRunnerBackend (i+1) tChan rChan _logChan
  where
    _debug i msg = traceIO $ "toyTestRunnerBackend: "++show i++": "++msg

-- | Initialize a 'LocalSynEnv' for the toy arch and run a toy 'Syn'
-- action in it.
--
-- The initializer doesn't consider the possibility that the action
-- run in this env (with 'runSyn') has any on-disk side effects,
-- e.g. writing a file. If we want on disk side effects, then we
-- should change this to set up the on-disk test env in a tmp dir.
runSynToy :: (forall t. Syn t Toy a) -> IO a
runSynToy action = do
  let cfg :: Config Toy
      cfg = Config
        { baseSetDir = "test-toy/test1/base"
        , pseudoSetDir = "test-toy/test1/pseudo"
        , learnedSetDir = "test-toy/test1/learned"
        , statisticsFile = "test-toy/test1/stats.txt"
        , programCountThreshold = L.error "programCountThreshold"
        , randomTestCount = 1024
        , threadCount = L.error "threadCount"
        , testRunner = toyTestRunnerBackend 0 :: I.TestRunner Toy
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
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r

  gen <- A.createGen
  let p = Proxy :: Proxy Toy
  let genTest = C.randomState p gen

  -- TODO: heuristic tests.
  let interestingTests = []
  -- The way the opcode list and semantic files on disk interact is a
  -- little weird: the opcode list bounds the possible semantic files
  -- we look for. I.e., the instructions with known semantics are the
  -- intersection of the opcode lists here and the semantic files on
  -- disk.
  --
  -- let allOpcodes = opcodesWitnessingBuildOperandList
  let allOpcodes = [ {- Some (Witness AddRr)
                   , -} Some (Witness NegR)
                   , Some (Witness SubRr) ]
  let pseudoOpcodes = pseudoOpcodesWitnessingBuildOperandList
  let targetOpcodes = L.error "targetOpcodes"
  synEnv <- loadInitialState cfg sym genTest interestingTests allOpcodes pseudoOpcodes targetOpcodes ioRelations

  tChan <- C.newChan :: IO (C.Chan (Maybe (I.TestCase Toy)))
  rChan <- C.newChan
  logChan <- C.newChan
  _testRunnerThread <- C.async $
    testRunner (seConfig synEnv) tChan rChan logChan
  C.link _testRunnerThread
  let runTest = S.naiveRunTest @Toy tChan rChan
  let localSynEnv = LocalSynEnv
        { seGlobalEnv = synEnv
        , seRandomGen = gen
        , seRunTest = runTest
        }
  runSyn localSynEnv action

-- | Synthesize a single candidate program that agrees with the target
-- program on the tests.
--
-- This is the inner loop of stratified synthesis, and candidates
-- generated this way are then proven equivalent to build confidence
-- that they implement the target on all possible inputs.
synthesizeCandidate :: IO (Maybe [P.SynthInstruction Toy])
synthesizeCandidate = do
  let ops = (R32 Reg1 :> R32 Reg2 :> Nil)
  let instruction = C.RI { C.riInstruction = D.Instruction AddRr ops
                         , C.riOpcode = AddRr
                         , C.riOperands = ops
                         , C.riLiteralLocs = MapF.empty
                         }
  -- let instruction = D.Instruction SubRr (R32 Reg1 :> R32 Reg2 :> Nil)
  runSynToy (S.synthesize instruction)

-- | Weigh a candidate that produces the right value in the wrong
-- place.
--
-- The weight should be 3 * number of test cases.
rightValueWrongPlace :: IO (Double, Double)
rightValueWrongPlace = return (0, 0) -- TODO
