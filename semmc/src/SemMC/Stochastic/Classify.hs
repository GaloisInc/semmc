{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.Classify (
  classify,
  chooseClass,
  chooseProgram,
  EquivalenceClasses,
  equivalenceClasses,
  countPrograms
  ) where

import qualified GHC.Err.Located as L

import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe ( mapMaybe )
import qualified Data.Set as S
import           Data.Word ( Word64 )
import           Text.Printf ( printf )

import qualified SemMC.Architecture as A
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Equivalence as F
import qualified SemMC.Formula.Instantiate as F
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Concrete.State as CS
import           SemMC.Symbolic ( Sym )
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.Pseudo as P

-- | A set of equivalence classes of programs
data EquivalenceClasses arch = EquivalenceClasses { unClasses :: S.Set (S.Set [P.SynthInstruction arch]) }

data EquivalenceClass arch = EquivalenceClass { unClass :: S.Set [P.SynthInstruction arch] }

-- | Construct an initial set of equivalence classes with a single program
equivalenceClasses :: [P.SynthInstruction arch] -> EquivalenceClasses arch
equivalenceClasses p = EquivalenceClasses (S.singleton (S.singleton p))

-- | Count the total number of programs in the set of equivalence classes
countPrograms :: EquivalenceClasses arch -> Int
countPrograms s = sum (map S.size (S.toList (unClasses s)))

-- | Given a set of initial equivalence classes, assign the new program to one
-- of them (or create a new equivalence class if the new program doesn't match
-- any).
--
-- This function returns 'Nothing' if the new program generates a counterexample
-- that invalidates all of the previous programs.
classify :: forall arch t
          . (SynC arch)
         => CS.RegisterizedInstruction arch
         -- ^ The instruction whose semantics we are trying to learn
         -> [P.SynthInstruction arch]
         -- ^ The program we just learned and need to add to an equivalence class
         -> EquivalenceClasses arch
         -- ^ The current equivalence classes
         -> Syn t arch (Maybe (EquivalenceClasses arch))
classify target p eqclasses = do
  mclasses <- classifyByClass target p S.empty (S.toList (unClasses eqclasses))
  case mclasses of
    Nothing -> return Nothing
    Just classes
      | S.null classes -> do
          -- Add a new equivalence class for this program, since it isn't
          -- equivalent to any existing class
          return (Just (EquivalenceClasses (S.singleton (S.singleton p) `S.union` unClasses eqclasses)))
      | otherwise -> do
          let eqclasses' = S.insert p (mergeClasses classes)
          return (Just (EquivalenceClasses (S.singleton eqclasses')))

-- | For each class in the current equivalence classes, see if the given program
-- matches any.  This function returns the list of all matching equivalence
-- classes.
--
-- If it matches more than one, equivalence class, we merge the matches in
-- 'classify'.
--
-- This function also produces counterexamples if the new program isn't
-- equivalent to the old programs.
classifyByClass :: (SynC arch)
                => CS.RegisterizedInstruction arch
                -- ^ The target instruction
                -> [P.SynthInstruction arch]
                -- ^ The program to classify
                -> S.Set (S.Set [P.SynthInstruction arch])
                -- ^ The set of classes matching the input program
                -> [S.Set [P.SynthInstruction arch]]
                -- ^ The existing equivalence classes
                -> Syn t arch (Maybe (S.Set (S.Set [P.SynthInstruction arch])))
classifyByClass target p eqs klasses =
  case klasses of
    [] -> return (Just eqs)
    (klass:rest) -> do
      representative <- chooseProgram (EquivalenceClass klass)
      eqv <- testEquivalence p representative
      case eqv of
        F.Equivalent -> classifyByClass target p (S.insert klass eqs) rest
        F.DifferentBehavior cx -> do
          -- See Note [Registerization and Counterexamples]
          let (target', cx') = CS.registerizeInstruction target cx
          addTestCase cx'
          eqclasses' <- removeInvalidPrograms target' cx' undefined
          case countPrograms eqclasses' of
            0 -> return Nothing
            -- FIXME: We are modifying eqclasses while we iterate over them.
            -- What are the semantics there?  The paper isn't precise.
            _ -> classifyByClass target p eqs rest

-- | Remove the programs in the equivalence classes that do not have the same
-- output on the counterexample as the target instruction.
--
-- We have a new counterexample and need to do concrete executions of *each*
-- existing candidate programs and reject those that do not provide the correct
-- output.
removeInvalidPrograms :: (SynC arch)
                      => A.Instruction arch
                      -> CS.ConcreteState arch
                      -> EquivalenceClasses arch
                      -> Syn t arch (EquivalenceClasses arch)
removeInvalidPrograms target cx (EquivalenceClasses klasses) = do
  targetTC <- mkTestCase cx [target]
  CE.TestSuccess (CE.TestResult { CE.resultContext = targetSt })
    <- runConcreteTest targetTC
  -- Convert the equivalence classes to nested lists, then tag each program with
  -- a number.  Keep the proper nesting structure, though, so that we don't
  -- destroy the classes.
  let testLists = numberNestedListItems [ F.toList klass | klass <- F.toList klasses ]
  (testCases, testIndex) <- F.foldrM (makeAndIndexTest cx) ([], M.empty) (concat testLists)
  testResults <- runConcreteTests testCases
  let testLists' = [ S.fromList (mapMaybe (consistentWithTarget testIndex testResults targetSt) testList)
                   | testList <- testLists
                   ]
  return $ EquivalenceClasses (S.fromList testLists')

-- | Reject candidate programs that have a different behavior on the given counterexample
consistentWithTarget :: (SynC arch)
                     => M.Map Int Word64
                     -- ^ The mapping of candidate program numbers to test
                     -- nonces (so that we can find the results for a given
                     -- candidate program)
                     -> CE.ResultIndex (CS.ConcreteState arch)
                     -- ^ All of the results that came back from the remote runner
                     -> CS.ConcreteState arch
                     -- ^ The output of the target instruction on the test vector
                     -> (Int, [P.SynthInstruction arch])
                     -- ^ The current program (and its index into the @testIndex@ map)
                     -> Maybe [P.SynthInstruction arch]
consistentWithTarget testIndex testResults cx (testNum, tc) =
  case M.lookup nonce (CE.riExitedWithSignal testResults) of
    Just _ -> Nothing
    Nothing ->
      case M.lookup nonce (CE.riSuccesses testResults) of
        Nothing -> L.error (printf "Missing a test result for test number %d with nonce %d" testNum nonce)
        Just res
          | CE.resultContext res == cx -> Just tc
          | otherwise -> Nothing
  where
    Just nonce = M.lookup testNum testIndex

-- | Create a test case out of a test vector and the given candidate program.
--
-- We have to expand the 'SynthInstruction's to real instructions before we
-- construct the test case.  We accumulate the test cases and maintain a mapping
-- between test numbers and concrete test nonces.
makeAndIndexTest :: (P.ArchitectureWithPseudo arch)
                 => CS.ConcreteState arch
                 -> (Int, [P.SynthInstruction arch])
                 -> ([CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)], M.Map Int Word64)
                 -> Syn t arch ([CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)], M.Map Int Word64)
makeAndIndexTest cx (pix, insns) (cases, idx) = do
  tc <- mkTestCase cx program
  return (tc : cases, M.insert pix (CE.testNonce tc) idx)
  where
    program = concatMap P.synthInsnToActual insns

-- | Assign a unique number to each list item (in the inner lists)
numberNestedListItems :: [[a]] -> [[(Int, a)]]
numberNestedListItems = snd . L.mapAccumR number1 0
  where
    number1 n = L.mapAccumR number2 n
    number2 n itm = (n + 1, (n, itm))

-- | Heuristically-choose the best equivalence class
--
-- It prefers classes with more examples and fewer uninterpreted functions
chooseClass :: EquivalenceClasses arch -> Syn t arch (EquivalenceClass arch)
chooseClass = undefined

-- | Choose the best program out of an equivalence class as the basis for a formula
--
-- We want to minimize the number of uninterpreted functions and non-linear
-- operations (as well as formula size).
chooseProgram :: EquivalenceClass arch -> Syn t arch [P.SynthInstruction arch]
chooseProgram = undefined

-- | Flatten a set of equivalence classes into one equivalence class
mergeClasses :: (Ord pgm)
             => S.Set (S.Set pgm)
             -> S.Set pgm
mergeClasses s = S.unions (S.toList s)

-- | Convert an instruction into a 'F.Formula'
instructionFormula :: (P.ArchitectureWithPseudo arch)
                   => Sym t
                   -> P.SynthInstruction arch
                   -> Syn t arch (F.Formula (Sym t) arch)
instructionFormula sym i = do
  case i of
    P.SynthInstruction op operands -> do
      Just pf <- lookupFormula op
      (_, f) <- liftIO $ F.instantiateFormula sym pf operands
      return f

-- | Convert a program into a formula
programFormula :: (P.ArchitectureWithPseudo arch)
               => Sym t
               -> [P.SynthInstruction arch]
               -> Syn t arch (F.Formula (Sym t) arch)
programFormula sym insns = do
  fs <- mapM (instructionFormula sym) insns
  liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula fs

-- | Use an SMT solver to check if two programs are equivalent.
--
-- If they are not, return an input that demonstrates the difference.
testEquivalence :: (P.ArchitectureWithPseudo arch)
                => [P.SynthInstruction arch]
                -> [P.SynthInstruction arch]
                -> Syn t arch (F.EquivalenceResult arch CS.Value)
testEquivalence p representative = do
  withSymBackend $ \sym -> do
    pf <- programFormula sym p
    repFormula <- programFormula sym representative
    liftIO $ F.formulasEquivConcrete sym pf repFormula


{- Note [Registerization and Counterexamples]

  FIXME: If cx /= cx', we don't know if this is still a counterexample.  That
  would be kind of strange, but we need to be able to handle that case.

  Maybe we tweak the counterexample at a higher level (when we generate it)
  and add that as a test case.  If it is still a counterexample, throwing
  away candidate programs is still sound.


-}
