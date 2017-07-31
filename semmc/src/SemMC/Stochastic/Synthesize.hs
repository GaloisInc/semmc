{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Description: Synthesize a program that implements a target instruction.
--
-- Stochastic synthesis as described in the STOKE and STRATA papers.
--
-- STOKE: Stochastic superoptimization:
-- https://cs.stanford.edu/people/eschkufz/docs/asplos_13.pdf
--
-- STRATA: Stratified Synthesis:
-- https://cs.stanford.edu/people/eschkufz/docs/pldi_16.pdf
module SemMC.Stochastic.Synthesize ( synthesize ) where

import           Control.Monad ( join )
import           Control.Monad.Trans ( liftIO )
import           Data.Maybe ( catMaybes )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as S
import qualified Data.Set as Set
import           GHC.Exts ( IsList(..) )

import           Data.Parameterized.Some ( Some(..) )
import           Dismantle.Arbitrary as D
import           Dismantle.Instruction.Random as D
import qualified Dismantle.Instruction as D

import           SemMC.Architecture ( Instruction, Operand )
import qualified SemMC.ConcreteState as C
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.IORelation as I

-- | Attempt to stochastically find a program in terms of the base set that has
-- the same semantics as the given instruction.
--
-- Can fail due to timeouts.
synthesize :: SynC arch
           => Instruction arch
           -> Syn t arch (Maybe [Instruction arch])
synthesize target = do
  initialTests <- generateInitialTests target
  mapM_ addTestCase initialTests
  -- TODO: fork and kill on timeout here, or do that in 'strataOne'.
  candidate <- mcmcSynthesizeOne target
  let candidateWithoutNops = catMaybes . toList $ candidate
  return $ Just candidateWithoutNops

mcmcSynthesizeOne :: SynC arch => Instruction arch -> Syn t arch (Candidate arch)
mcmcSynthesizeOne target = do
  -- Fixed start and make it a parameter if needed. STOKE Figure 10.
  let progLen = 50
  candidate <- emptyCandidate progLen

  tests <- askTestCases
  cost <- sum <$> mapM (compareTargetToCandidate target candidate) tests
  evolve cost candidate
  where
    -- | Evolve the candidate until it agrees with the target on the
    -- tests.
    evolve 0    candidate = return candidate
    evolve cost candidate = do
      candidate' <- perturb candidate
      (cost'', candidate'') <- chooseNextCandidate target candidate cost candidate'
      evolve cost'' candidate''

-- | Choose the new candidate if it's a better match, and with the
-- Metropolis probability otherwise.
--
-- This includes an optimization from the paper where we compute
-- the cost of the new candidate incrementally and stop as soon as
-- we know it's too expensive [STOKE Section 4.5].
chooseNextCandidate :: SynC arch
                    => Instruction arch
                    -> Candidate arch
                    -> Double
                    -> Candidate arch
                    -> Syn t arch (Double, Candidate arch)
chooseNextCandidate target candidate cost candidate' = do
  gen <- askGen
  threshold <- liftIO $ D.uniformR (0::Double, 1) gen
  tests <- askTestCases
  go threshold 0 tests
  where
    go threshold cost' tests
        -- STOKE Equation 14. Note that the min expression there
        -- is a typo, as in Equation 6, and should be a *difference*
        -- of costs in the exponent, not a *ratio* of costs.
      | cost' >= cost - log threshold/beta = return (cost, candidate)
      | [] <- tests = return (cost', candidate')
      | (test:tests') <- tests = do
          dcost <- compareTargetToCandidate target candidate test
          go threshold (cost + dcost) tests'

    beta = 0.1 -- STOKE Figure 10.

----------------------------------------------------------------

-- | Compute the cost, in terms of mismatch, of the candidate compared
-- to the target. STOKE Section 4.6.
--
-- TODO: We'd like to only compare the parts of the state that matter
-- (are live) for the target.
compareTargetToCandidate :: forall arch t.
                            SynC arch
                         => Instruction arch
                         -> Candidate arch
                         -> Test arch
                         -> Syn t arch Double
compareTargetToCandidate target candidate test = do
  let candidateProg = catMaybes . toList $ candidate
  let targetProg = [target]
  -- TODO: cache result of running target on test, since it never
  -- changes. An easy way to do this is to change the definition of
  -- test to be a pair of a start state and the end state for the
  -- start state when running the target.
  candidateSt <- runTest test candidateProg
  targetSt    <- runTest test targetProg
  liveOut     <- getOutMasks target
  let p = Proxy :: Proxy arch
  let weight = compareTargetOutToCandidateOut p liveOut targetSt candidateSt
  return weight
  where
    runTest :: Test arch -> [Instruction arch] -> Syn t arch (C.ConcreteState arch)
    runTest = undefined

-- | The masks for locations that are live out for the target instruction.
--
-- We could cache this since the target instruction is fixed.
getOutMasks :: forall arch t. SynC arch
            => Instruction arch -> Syn t arch (C.OutMasks arch)
getOutMasks (D.Instruction opcode operands) = do
  Just ioRel <- opcodeIORelation opcode
  let outputs = Set.toList $ I.outputs ioRel
  -- Ignore implicit operands for now.
  -- TODO: handle implicits.
  let outputs' = [ s | I.OperandRef s <- outputs ]
  let outputs'' = [ Some (D.indexOpList operands i) | Some i <- outputs' ]
  let masks = [ operandToOutMask operand | operand <- outputs'' ]
  return masks
  where
    -- TODO: we just assume all operands are integers, but this isn't
    -- sound. Rather, the interpretation (int or float) of operand
    -- should be determined by the opcode, perhaps by another method in
    -- @Architecture@.
    operandToOutMask :: Some (Operand arch) -> Some (C.OutMask arch)
    operandToOutMask (Some operand) = case someView of
      Some view -> Some $ C.OutMask view C.diffInt
      where
        Just someView = C.operandToView (Proxy :: Proxy arch) operand

-- Sum the weights of all test outputs.
compareTargetOutToCandidateOut :: forall arch.
                                  SynC arch
                               => Proxy arch
                               -> C.OutMasks arch
                               -> C.ConcreteState arch
                               -> C.ConcreteState arch
                               -> Double
compareTargetOutToCandidateOut arch masks targetSt candidateSt =
  sum [ C.withKnownNat (C.viewTypeRepr (C.outMaskView mask)) $ weighBestMatch arch mask targetSt candidateSt
      | Some mask <- masks
      ]

-- Find the number-of-bits error in the best match, penalizing matches
-- that are in the wrong location.
weighBestMatch :: forall arch n.
                  (C.KnownNat n, SynC arch)
               => Proxy arch
               -> C.OutMask arch n
               -> C.ConcreteState arch
               -> C.ConcreteState arch
               -> Double
weighBestMatch arch (C.OutMask view diff) targetSt candidateSt =
  minimum $ [ weigh (C.peekMS candidateSt view) ] ++
            [ weigh (C.peekMS candidateSt view') + penalty
            | view' <- C.congruentViews arch view ]
  where
    targetVal = C.peekMS targetSt view
    penalty = 3 -- STOKE Figure 10.
    weigh candidateVal = fromIntegral $ diff targetVal candidateVal

----------------------------------------------------------------

-- | The initial tests, including random tests and "heuristically
-- interesting" tests.
--
-- During synthesis these tests get augmented by new tests discovered
-- by the SMT solver that distinguish candidates that are equal on all
-- tests so far.
generateInitialTests :: Instruction arch -> Syn t arch [Test arch]
generateInitialTests target = undefined

-- | A candidate program.
--
-- We use 'Nothing' to represent no-ops, which we need because the
-- candidate program has fixed length during its evolution.
type Candidate arch = S.Seq (Maybe (Instruction arch))

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

-- | Randomly replace an opcode with another compatible opcode, while
-- keeping the operands fixed.
perturbOpcode :: SynC arch => Candidate arch -> Syn t arch (Candidate arch)
perturbOpcode candidate = do
  gen <- askGen
  index <- liftIO $ D.uniformR (0, S.length candidate - 1) gen
  baseSet <- askBaseSet
  let oldInstruction = candidate `S.index` index
  newInstruction <- liftIO $
    D.randomizeOpcode gen baseSet `mapM` oldInstruction
  return $ S.update index newInstruction candidate

-- | Randomly replace the operands, while keeping the opcode fixed.
perturbOperand :: SynC arch => Candidate arch -> Syn t arch (Candidate arch)
perturbOperand candidate = do
  gen <- askGen
  index <- liftIO $ D.uniformR (0, S.length candidate - 1) gen
  let oldInstruction = candidate `S.index` index
  newInstruction <- liftIO $ D.randomizeOperand gen `mapM` oldInstruction
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
    , (1 - p_u, Just <$> D.randomInstruction gen baseSet) ]
    gen
  return $ S.update index newInstruction candidate
  where
    -- STOKE Figure 10.
    p_u = 1/6
