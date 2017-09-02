{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
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

import qualified Control.Exception as C
import           Control.Monad ( join )
import           Control.Monad.Trans ( liftIO )
import           Data.Maybe ( catMaybes )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as S
import qualified Data.Set as Set
import           GHC.Exts ( IsList(..) )
import qualified GHC.Err.Located as L

import qualified Data.Set.NonEmpty as NES
import           Data.Parameterized.HasRepr ( HasRepr )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.ShapedList ( lengthFC, traverseFCIndexed, indexAsInt, indexShapedList, ShapeRepr )
import           Dismantle.Arbitrary as D
import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.Instruction as D

import           SemMC.Architecture ( Instruction, Opcode, Operand )
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Concrete.State as C
import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.Pseudo
                 ( Pseudo
                 , SynthOpcode
                 , SynthInstruction(..)
                 , synthArbitraryOperands
                 , synthInsnToActual
                 , actualInsnToSynth
                 )
import qualified SemMC.Stochastic.IORelation as I

import Debug.Trace
import Text.Printf
import qualified Data.List as L
import Control.Monad

-- | Attempt to stochastically find a program in terms of the base set that has
-- the same semantics as the given instruction.
--
-- Can fail due to timeouts.
synthesize :: SynC arch
           => C.RegisterizedInstruction arch
           -> Syn t arch (Maybe [SynthInstruction arch])
synthesize target = do
  -- TODO: fork and kill on timeout here, or do that in 'strataOne'.
  (numRounds, candidate) <- mcmcSynthesizeOne target
  debug $ printf "found candidate after %i rounds" numRounds
  let candidateWithoutNops = catMaybes . toList $ candidate
  debug $ printf "candidate:\n%s" (unlines . map show $ candidateWithoutNops)
  tests <- askTestCases
  debug $ printf "# test cases = %i\n" (length tests)
  return $ Just candidateWithoutNops
  where
    debug msg = liftIO $ traceIO msg

mcmcSynthesizeOne :: forall arch t. SynC arch => C.RegisterizedInstruction arch -> Syn t arch (Integer, Candidate arch)
mcmcSynthesizeOne target = do
  -- Max length of candidate programs. Can make it a parameter if
  -- needed.
  let progLen = 50 -- STOKE Figure 10.
  candidate <- emptyCandidate progLen
  -- let candidate = fromList [Just $ actualInsnToSynth @arch target]

  tests <- askTestCases
  cost <- sum <$> mapM (compareTargetToCandidate target candidate) tests
  liftIO $ print cost
  evolve 0 cost candidate
  where
    -- | Evolve the candidate until it agrees with the target on the
    -- tests.
    evolve k  0    candidate = return (k, candidate)
    evolve !k cost candidate = do
      candidate' <- perturb candidate
      if candidate == candidate'
      then evolve (k+1) cost candidate
      else do
        debug $ "candidate:\n"++prettyCandidate candidate
        debug $ "candidate':\n"++prettyCandidate candidate'
        debug $ "cost = " ++ show cost
        -- liftIO $ showDiff candidate candidate'
        (cost'', candidate'') <- chooseNextCandidate @arch target candidate cost candidate'
        evolve (k+1) cost'' candidate''

    debug msg = liftIO $ traceIO $ "mcmcSynthesizeOne: "++msg

    showDiff c c' = do
      debug "===================================================="
      forM_ zipped $ \xs -> do
        debug $ case xs of
          [Nothing] -> ""
          [Just i] -> "=== "++show i++"\n"
          [x,x'] -> "!!! "++show x++"\n!!! "++show x'++"\n"
          _ -> L.error "the sky is falling"
      where
        zipped = S.zipWith (\x x' -> L.nub [x,x']) c c'

prettyCandidate :: Show (SynthInstruction arch)
                => Candidate arch -> String
prettyCandidate = unlines . map show . catMaybes . toList

-- | Choose the new candidate if it's a better match, and with the
-- Metropolis probability otherwise.
--
-- This includes an optimization from the paper where we compute
-- the cost of the new candidate incrementally and stop as soon as
-- we know it's too expensive [STOKE Section 4.5].
chooseNextCandidate :: SynC arch
                    => C.RegisterizedInstruction arch
                    -> Candidate arch
                    -> Double
                    -> Candidate arch
                    -> Syn t arch (Double, Candidate arch)
chooseNextCandidate target candidate cost candidate' = do
  gen <- askGen
  threshold <- liftIO $ D.uniformR (0::Double, 1) gen
  debug $ printf "threshold = %f" threshold
  tests <- askTestCases
  go threshold 0 tests
  where
    go threshold cost' tests
        -- STOKE Equation 14. Note that the min expression there
        -- is a typo, as in Equation 6, and should be a *difference*
        -- of costs in the exponent, not a *ratio* of costs.
      | cost' >= cost - log threshold/beta = do
          debug "reject"
          return (cost, candidate)
      | [] <- tests = do
          debug "accept"
          return (cost', candidate')
      | (test:tests') <- tests = do
          -- debug $ printf "%f %f" cost threshold
          dcost <- compareTargetToCandidate target candidate' test
          -- debug (show dcost)
          go threshold (cost' + dcost) tests'

    beta = 0.1 -- STOKE Figure 10.

    debug (msg :: String) = liftIO $ traceIO $ "chooseNextCandidate: "++msg

----------------------------------------------------------------

-- | Compute the cost, in terms of mismatch, of the candidate compared
-- to the target. STOKE Section 4.6.
compareTargetToCandidate :: forall arch t.
                            SynC arch
                         => C.RegisterizedInstruction arch
                         -> Candidate arch
                         -> C.ConcreteState arch
                         -> Syn t arch Double
compareTargetToCandidate target candidate test = do
  let candidateProg =
        concatMap synthInsnToActual . catMaybes . toList $ candidate
  let (target', test') = C.registerizeInstruction target test
  let targetProg = [target']
  -- TODO: cache result of running target on test, since it never
  -- changes. An easy way to do this is to change the definition of
  -- test to be a pair of a start state and the end state for the
  -- start state when running the target.
  CE.TestSuccess (CE.TestResult { CE.resultContext = candidateSt })
     <- runConcreteTest =<< mkTestCase test' candidateProg

  CE.TestSuccess (CE.TestResult { CE.resultContext = targetSt })
     <- runConcreteTest =<< mkTestCase test' targetProg
  !liveOut     <- getOutMasks target'
  !eitherWeight <- liftIO $ C.tryJust pred $ do
    let !weight = compareTargetOutToCandidateOut liveOut targetSt candidateSt
    return weight
  case eitherWeight of
    Left e -> do
      debug (show e)
      debug $ printf "target = %s" (show targetSt)
      debug $ printf "candidate = %s" (show candidateSt)
      liftIO (C.throwIO e)
    Right weight -> return weight
  where
    pred = \(e :: C.ArithException) -> Just e
    debug (msg :: String) = liftIO $ traceIO $ "compareTargetToCandidate: "++msg

-- | The masks for locations that are live out for the target instruction.
--
-- We could cache this since the target instruction is fixed.
getOutMasks :: forall arch t. SynC arch
            => Instruction arch -> Syn t arch [C.SemanticView arch]
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
    operandToSemView :: Some (Operand arch) -> C.SemanticView arch
    operandToSemView (Some operand) = desc
      where Just desc = C.operandToSemanticView (Proxy :: Proxy arch) operand

-- Sum the weights of all test outputs.
compareTargetOutToCandidateOut :: forall arch.
                                  SynC arch
                               => [C.SemanticView arch]
                               -> C.ConcreteState arch
                               -> C.ConcreteState arch
                               -> Double
compareTargetOutToCandidateOut descs targetSt candidateSt =
  sum [ NR.withKnownNat (C.viewTypeRepr view) $ weighBestMatch desc targetSt candidateSt
      | desc@(C.SemanticView { C.semvView = view }) <- descs
      ]

-- Find the number-of-bits error in the best match, penalizing matches
-- that are in the wrong location.
weighBestMatch :: forall arch.
                  (SynC arch)
               => C.SemanticView arch
               -> C.ConcreteState arch
               -> C.ConcreteState arch
               -> Double
weighBestMatch (C.SemanticView view@(C.View (C.Slice _ _ _ _) _) congruentViews diff) targetSt candidateSt =
  minimum $ [ weigh (C.peekMS candidateSt view) ] ++
            [ weigh (C.peekMS candidateSt view') + penalty
            | view' <- congruentViews ]
  where
    targetVal = C.peekMS targetSt view
    penalty = 3 -- STOKE Figure 10.
    weigh candidateVal = fromIntegral $ diff targetVal candidateVal

----------------------------------------------------------------

-- | A candidate program.
--
-- We use 'Nothing' to represent no-ops, which we need because the
-- candidate program has fixed length during its evolution.
type Candidate arch = S.Seq (Maybe (SynthInstruction arch))

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
  congruent <- lookupCongruent oldOpcode
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
