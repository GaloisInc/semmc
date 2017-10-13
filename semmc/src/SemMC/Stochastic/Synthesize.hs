{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Description: Synthesize a program that implements a target instruction.
--
-- Stochastic synthesis as described in the STOKE and STRATA papers.
--
-- STOKE: Stochastic superoptimization:
-- https://cs.stanford.edu/people/eschkufz/docs/asplos_13.pdf
--
-- STRATA: Stratified Synthesis:
-- https://cs.stanford.edu/people/eschkufz/docs/pldi_16.pdf
module SemMC.Stochastic.Synthesize
  ( -- * API
    synthesize
    -- * Exports for testing
  , compareTargetToCandidate
  , computeCandidateResults
  , computeTargetResults
  , wrongLocationPenalty
  ) where

import           GHC.Stack ( HasCallStack )

import qualified Control.Concurrent.Async as A
import qualified Control.Exception as C
import           Control.Monad ( join, replicateM )
import qualified Control.Monad.Catch as MC
import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Maybe ( catMaybes )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as S
import qualified Data.Set as Set
import           Data.Typeable ( Typeable )
import qualified GHC.Err.Located as L
import           Text.Printf

import qualified Data.Set.NonEmpty as NES
import           Data.Parameterized.HasRepr ( HasRepr )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.ShapedList ( lengthFC, traverseFCIndexed, indexAsInt, indexShapedList, ShapeRepr )
import           Dismantle.Arbitrary as D
import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.Instruction as D

import           SemMC.Architecture ( Instruction, Opcode, Operand )
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Stochastic.CandidateProgram as CP
import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.Pseudo
                 ( Pseudo
                 , SynthOpcode
                 , SynthInstruction(..)
                 , synthArbitraryOperands
                 , synthInsnToActual
                 )
import qualified SemMC.Stochastic.IORelation as I
import qualified SemMC.Util as U

-- | Attempt to stochastically find a program in terms of the base set that has
-- the same semantics as the given instruction.
--
-- This function can loop forever, and should be called under a timeout
synthesize :: (SynC arch, U.HasCallStack)
           => AC.RegisterizedInstruction arch
           -> Syn t arch (CP.CandidateProgram t arch)
synthesize target = do
  case target of
    AC.RI { AC.riInstruction = i0 } ->
      U.logM U.Info $ printf "Attempting to synthesize a program for %s" (show i0)
  (numRounds, candidate) <- parallelSynthOne target -- mcmcSynthesizeOne target
  U.logM U.Info $ printf "found candidate after %i rounds" numRounds
  let candidateWithoutNops = catMaybes . F.toList $ candidate
  U.logM U.Debug $ printf "candidate:\n%s" (unlines . map show $ candidateWithoutNops)
  tests <- askTestCases
  U.logM U.Debug $ printf "number of test cases = %i\n" (length tests)
  withSymBackend $ \sym -> do
    f <- CP.programFormula sym candidateWithoutNops
    return CP.CandidateProgram { CP.cpInstructions = candidateWithoutNops
                               , CP.cpFormula = f
                               }

data SynthesisException arch = AllSynthesisThreadsFailed (Proxy arch) (Instruction arch)

deriving instance (SynC arch) => Show (SynthesisException arch)
instance (Typeable arch, SynC arch) => C.Exception (SynthesisException arch)

parallelSynthOne :: forall arch t
                  . (SynC arch, HasCallStack)
                 => AC.RegisterizedInstruction arch
                 -> Syn t arch (Integer, Candidate arch)
parallelSynthOne target = do
  nThreads <- askParallelSynth
  asyncs <- replicateM nThreads (asyncWithIsolatedEnv (mcmcSynthesizeOne target))
  waitForCompletion asyncs
  where
    waitForCompletion asyncs =
      case asyncs of
        [] -> do
          U.logM U.Warn "All synthesis threads failed with exceptions"
          MC.throwM (AllSynthesisThreadsFailed (Proxy @arch) (AC.riInstruction target))
        _ -> do
          (firstAsync, res) <- liftIO (A.waitAnyCatch asyncs)
          case res of
            Left exn -> do
              U.logM U.Info $ printf "Async synthesis thread failed with error '%s'" (show exn)
              waitForCompletion (filter (/= firstAsync) asyncs)
            Right success -> do
              let rest = filter (/= firstAsync) asyncs
              mapM_ (liftIO . A.cancel) rest
              return success

-- | Get concrete execution results for the tests on a target program.
--
-- Note that this function also returns the tests with alterations required by
-- the registerization process.  The tests are paired with a unique ID that can
-- be used to correlate the tests with runs on candidate programs.
computeTargetResults :: (SynC arch)
                     => AC.RegisterizedInstruction arch
                     -> [V.ConcreteState arch]
                     -> Syn t arch ([CE.TestCase (V.ConcreteState arch) (Instruction arch)],
                                    (CE.ResultIndex (V.ConcreteState arch)))
computeTargetResults target tests = do
  let registerizedInsns = map (AC.registerizeInstruction target) tests
  tcs <- mapM (\(insn, t) -> mkTestCase t [insn]) registerizedInsns
  resultIndex <- runConcreteTests tcs
  return (tcs, resultIndex)

-- | Get concrete execution results for the tests on a candidate program.
--
-- Note that the tests should be the tests returned by 'computeTargetResults',
-- which modifies test programs as necessary due to registerization.
--
-- In addition to the result index (containing the concrete execution results),
-- we return a list of pairs of test cases.  The first element of each pair is
-- the corresponding test case from the target.  The second element of each pair
-- is the corresponding test case for the candidate.  This gives both nonces,
-- which we can use to look up results in the relevant result indexes.
computeCandidateResults :: (SynC arch)
                        => Candidate arch
                        -> [CE.TestCase (V.ConcreteState arch) (Instruction arch)]
                        -> Syn t arch ([(CE.TestCase (V.ConcreteState arch) (Instruction arch), CE.TestCase (V.ConcreteState arch) (Instruction arch))],
                                       (CE.ResultIndex (V.ConcreteState arch)))
computeCandidateResults candidate tests = do
  indexedTests <- mapM (\tc -> (tc,) <$> mkTestCase (CE.testContext tc) (candidateInstructions candidate)) tests
  resultIndex <- runConcreteTests (map snd indexedTests)
  return (indexedTests, resultIndex)

mcmcSynthesizeOne :: forall arch t
                   . (SynC arch, U.HasCallStack)
                  => AC.RegisterizedInstruction arch
                  -> Syn t arch (Integer, Candidate arch)
mcmcSynthesizeOne target = do
  -- Max length of candidate programs. Can make it a parameter if
  -- needed.
  let progLen = 8 -- STOKE Figure 10.
  candidate <- emptyCandidate progLen
  -- let candidate = fromList [Just $ actualInsnToSynth @arch target]

  tests <- askTestCases
  (targetTests, targetResults) <- computeTargetResults target tests
  (testPairs, candidateResults) <- computeCandidateResults candidate targetTests
  cost <- sum <$> mapM (compareTargetToCandidate target targetResults candidateResults) testPairs
  evolve 0 cost targetTests targetResults candidate
  where
    -- | Evolve the candidate until it agrees with the target on the
    -- tests.
    evolve k  0    _ _ candidate = return (k, candidate)
    evolve !k cost targetTests targetResults candidate = do
      candidate' <- perturb candidate
      if candidate == candidate'
      then evolve (k+1) cost targetTests targetResults candidate
      else do
        U.logM U.Debug $ "candidate:\n"++prettyCandidate candidate
        U.logM U.Debug $ "candidate':\n"++prettyCandidate candidate'
        U.logM U.Debug $ "cost = " ++ show cost
        -- liftIO $ showDiff candidate candidate'
        (cost'', candidate'') <- chooseNextCandidate @arch target targetTests targetResults candidate cost candidate'
        evolve (k+1) cost'' targetTests targetResults candidate''
{-
import qualified Data.List as L
import Control.Monad

    showDiff c c' = do
      U.logM U.Debug "===================================================="
      forM_ zipped $ \xs -> do
        U.logM U.Debug $ case xs of
          [Nothing] -> ""
          [Just i] -> "=== "++show i++"\n"
          [x,x'] -> "!!! "++show x++"\n!!! "++show x'++"\n"
          _ -> L.error "the sky is falling"
      where
        zipped = S.zipWith (\x x' -> L.nub [x,x']) c c'
-}

prettyCandidate :: Show (SynthInstruction arch)
                => Candidate arch -> String
prettyCandidate = unlines . map (("    "++) . show) . catMaybes . F.toList

-- | Choose the new candidate if it's a better match, and with the
-- Metropolis probability otherwise.
--
-- This includes an optimization from the paper where we compute
-- the cost of the new candidate incrementally and stop as soon as
-- we know it's too expensive [STOKE Section 4.5].
chooseNextCandidate :: (SynC arch, U.HasCallStack)
                    => AC.RegisterizedInstruction arch
                    -> [CE.TestCase (V.ConcreteState arch) (Instruction arch)]
                    -> CE.ResultIndex (V.ConcreteState arch)
                    -> Candidate arch
                    -> Double
                    -> Candidate arch
                    -> Syn t arch (Double, Candidate arch)
chooseNextCandidate target targetTests targetResults candidate cost candidate' = do
  gen <- askGen
  threshold <- liftIO $ D.uniformR (0::Double, 1) gen
  U.logM U.Debug $ printf "threshold = %f" threshold
  (testPairs, candidateResults) <- computeCandidateResults candidate' targetTests
  go threshold 0 candidateResults testPairs
  where
    go threshold cost' candidateResults testPairs
        -- STOKE Equation 14. Note that the min expression there
        -- is a typo, as in Equation 6, and should be a *difference*
        -- of costs in the exponent, not a *ratio* of costs.
      | cost' >= cost - log threshold/beta = do
          U.logM U.Debug "reject"
          return (cost, candidate)
      | [] <- testPairs = do
          U.logM U.Debug "accept"
          return (cost', candidate')
      | (testPair:testPairs') <- testPairs = do
          -- U.logM U.Debug $ printf "%f %f" cost threshold
          dcost <- compareTargetToCandidate target targetResults candidateResults testPair
          -- U.logM U.Debug (show dcost)
          go threshold (cost' + dcost) candidateResults testPairs'

    beta = 0.1 -- STOKE Figure 10.

----------------------------------------------------------------

-- | Compute the cost, in terms of mismatch, of the candidate compared
-- to the target. STOKE Section 4.6.
--
-- If the candidate causes a crash, we give it an infinite weight to cause it to
-- be rejected.
compareTargetToCandidate :: forall arch t.
                            SynC arch
                         => AC.RegisterizedInstruction arch
                         -> CE.ResultIndex (V.ConcreteState arch)
                         -> CE.ResultIndex (V.ConcreteState arch)
                         -> (CE.TestCase (V.ConcreteState arch) (Instruction arch), CE.TestCase (V.ConcreteState arch) (Instruction arch))
                         -> Syn t arch Double
compareTargetToCandidate target targetResultIndex candidateResultIndex (targetTest, candidateTest) = do
  let (target', _test') = AC.registerizeInstruction target (CE.testContext targetTest)
  !liveOut     <- getOutMasks target'
  let targetRes = M.lookup (CE.testNonce targetTest) (CE.riSuccesses targetResultIndex)
      candidateRes = M.lookup (CE.testNonce candidateTest) (CE.riSuccesses candidateResultIndex)
  eitherWeight <- liftIO (doComparison liveOut targetRes candidateRes `C.catches` handlers)
  case eitherWeight of
    Left e -> do
      U.logM U.Debug $ printf "error = %s" (show e)
      U.logM U.Debug $ printf "target = %s" (show targetRes)
      U.logM U.Debug $ printf "candidate = %s" (show candidateRes)
      return (1.0 / 0.0)
    Right weight -> return weight
  where
    handlers = [ C.Handler arithHandler
               , C.Handler runnerHandler
               , C.Handler comparisonHandler
               ]
    arithHandler :: C.ArithException -> IO (Either C.SomeException a)
    arithHandler e = return (Left (C.SomeException e))
    runnerHandler :: CE.RunnerResultError -> IO (Either C.SomeException a)
    runnerHandler e = return (Left (C.SomeException e))
    comparisonHandler :: ComparisonError -> IO (Either C.SomeException a)
    comparisonHandler e = return (Left (C.SomeException e))
    doComparison liveOut mTargetRes mCandidateRes = do
      case mTargetRes of
        Just CE.TestResult { CE.resultContext = targetSt } ->
          case mCandidateRes of
            Just CE.TestResult { CE.resultContext = candidateSt } ->
              Right <$> C.evaluate (compareTargetOutToCandidateOut liveOut targetSt candidateSt)
            Nothing -> C.throwIO NoCandidateResult
        Nothing -> C.throwIO NoTargetResult

data ComparisonError = NoCandidateResult
                     | NoTargetResult
                     deriving (Show)

instance C.Exception ComparisonError

-- | The masks for locations that are live out for the target instruction.
--
-- We could cache this since the target instruction is fixed.
getOutMasks :: forall arch t. SynC arch
            => Instruction arch -> Syn t arch [V.SemanticView arch]
getOutMasks (D.Instruction opcode operands) = do
  Just ioRel <- opcodeIORelation opcode
  let outputs = Set.toList $ I.outputs ioRel
  -- Ignore implicit operands for now.
  -- TODO: handle implicits.
  let outputs' = [ s | I.OperandRef s <- outputs ]
  let outputs'' = [ Some (indexShapedList operands i) | Some i <- outputs' ]
  let semViews = map operandToSemView outputs''
  return semViews
  where
    operandToSemView :: Some (Operand arch) -> V.SemanticView arch
    operandToSemView (Some operand) = desc
      where Just desc = AC.operandToSemanticView (Proxy :: Proxy arch) operand

-- Sum the weights of all test outputs.
compareTargetOutToCandidateOut :: forall arch.
                                  SynC arch
                               => [V.SemanticView arch]
                               -> V.ConcreteState arch
                               -> V.ConcreteState arch
                               -> Double
compareTargetOutToCandidateOut descs targetSt candidateSt =
  sum [ NR.withKnownNat (V.viewTypeRepr view) $ weighBestMatch desc targetSt candidateSt
      | desc@(V.SemanticView { V.semvView = view }) <- descs
      ]

-- Find the number-of-bits error in the best match, penalizing matches
-- that are in the wrong location.
weighBestMatch :: forall arch.
                  (SynC arch)
               => V.SemanticView arch
               -> V.ConcreteState arch
               -> V.ConcreteState arch
               -> Double
weighBestMatch (V.SemanticView view@(V.View _ _) congruentViews diff) targetSt candidateSt =
  minimum $ [ weigh (V.peekMS candidateSt view) ] ++
            [ weigh (V.peekMS candidateSt view') + wrongLocationPenalty
            | view' <- congruentViews ]
  where
    targetVal = V.peekMS targetSt view
    weigh candidateVal = fromIntegral $ diff targetVal candidateVal

-- | The penalty used in 'weighBestMatch' when looking for a candidate
-- value in the wrong location.
wrongLocationPenalty :: Double
wrongLocationPenalty = 3 -- STOKE Figure 10.

----------------------------------------------------------------

-- | A candidate program.
--
-- We use 'Nothing' to represent no-ops, which we need because the
-- candidate program has fixed length during its evolution.
type Candidate arch = S.Seq (Maybe (SynthInstruction arch))

candidateInstructions :: (SynC arch) => Candidate arch -> [Instruction arch]
candidateInstructions = concatMap synthInsnToActual . catMaybes . F.toList

-- | The empty program is a sequence of no-ops.
emptyCandidate :: Int -> Syn t arch (Candidate arch)
emptyCandidate len = return $ S.replicate len Nothing

----------------------------------------------------------------

-- | Randomly perturb a candidate. STOKE Section 4.3.
perturb :: SynC arch => Candidate arch -> Syn t arch (Candidate arch)
perturb candidate = do
  gen <- askGen
  strategy <- liftIO $ D.categoricalChoose
    [ (p_c, perturbOpcode)
    , (p_o, perturbOperand)
    , (p_s, swapInstructions)
    , (p_i, perturbInstruction) ]
    gen
  strategy candidate
  where
    -- Constants from STOKE Figure 10.
    p_c = 1/6
    p_o = 1/2
    p_s = 1/6
    p_i = 1/6

-- | Replace the opcode of the given instruction with a random one of the same
-- shape.
randomizeOpcode :: (HasRepr (Opcode arch (Operand arch)) ShapeRepr,
                    HasRepr (Pseudo arch (Operand arch)) ShapeRepr)
                => SynthInstruction arch
                -> Syn t arch (SynthInstruction arch)
randomizeOpcode (SynthInstruction oldOpcode operands) = do
  gen <- askGen
  congruent <- CP.lookupCongruentOpcodes oldOpcode
  case S.length congruent of
    0 -> L.error "bug! The opcode being replaced should always be in the congruent set!"
    len -> do
      ix <- liftIO $ D.uniformR (0, len - 1) gen
      let !newOpcode = congruent `S.index` ix
      return (SynthInstruction newOpcode operands)

-- | Randomly replace one operand of an instruction.
--
-- FIXME: This is ripped straight out of dismantle. We should probably make the
-- one there more generic.
randomizeOperand :: (D.ArbitraryOperand (Operand arch))
                 => D.Gen
                 -> SynthInstruction arch
                 -> IO (SynthInstruction arch)
randomizeOperand gen (SynthInstruction op os) = do
  updateAt <- D.uniformR (0, (lengthFC os - 1)) gen
  os' <- traverseFCIndexed (f' updateAt gen) os
  return (SynthInstruction op os')
  where
    f' target g ix o
      | indexAsInt ix == target = D.arbitraryOperand g o
      | otherwise = return o

-- | Generate a random instruction
randomInstruction :: (D.ArbitraryOperands (Opcode arch) (Operand arch),
                      D.ArbitraryOperands (Pseudo arch) (Operand arch))
                  => D.Gen
                  -> NES.Set (Some (SynthOpcode arch))
                  -> IO (SynthInstruction arch)
randomInstruction gen baseSet = do
  Some opcode <- D.choose baseSet gen
  SynthInstruction opcode <$> synthArbitraryOperands gen opcode

-- | Randomly replace an opcode with another compatible opcode, while
-- keeping the operands fixed.
perturbOpcode :: SynC arch => Candidate arch -> Syn t arch (Candidate arch)
perturbOpcode candidate = do
  gen <- askGen
  index <- liftIO $ D.uniformR (0, S.length candidate - 1) gen
  let oldInstruction = candidate `S.index` index
  newInstruction <- randomizeOpcode `mapM` oldInstruction
  return $ S.update index newInstruction candidate

-- | Randomly replace the operands, while keeping the opcode fixed.
perturbOperand :: SynC arch => Candidate arch -> Syn t arch (Candidate arch)
perturbOperand candidate = do
  gen <- askGen
  index <- liftIO $ D.uniformR (0, S.length candidate - 1) gen
  let oldInstruction = candidate `S.index` index
  newInstruction <- liftIO $ randomizeOperand gen `mapM` oldInstruction
  return $ S.update index newInstruction candidate

-- | Swap two instructions in a program.
swapInstructions :: SynC arch => Candidate arch -> Syn t arch (Candidate arch)
swapInstructions candidate = do
  gen <- askGen
  index1 <- liftIO $ D.uniformR (0, S.length candidate - 1) gen
  index2 <- liftIO $ do
    -- Avoid @index1 == index2@.
    i2 <- D.uniformR (0, S.length candidate - 2) gen
    return $ if i2 < index1 then i2 else i2+1
  let [instr1, instr2] = map (candidate `S.index`) [index1, index2]
  return $ S.update index1 instr2 $ S.update index2 instr1 candidate

-- | Replace an instruction with an unrelated instruction.
perturbInstruction :: SynC arch => Candidate arch -> Syn t arch (Candidate arch)
perturbInstruction candidate = do
  gen <- askGen
  index <- liftIO $ D.uniformR (0, S.length candidate - 1) gen
  baseSet <- askBaseSet
  newInstruction <- liftIO $ join $ D.categoricalChoose
    [ (p_u, return Nothing)
    , (1 - p_u, Just <$> randomInstruction gen baseSet) ]
    gen
  return $ S.update index newInstruction candidate
  where
    -- STOKE Figure 10.
    p_u = 1/6
