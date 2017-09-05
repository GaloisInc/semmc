{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import qualified Control.Monad.State.Strict as St
import           Control.Monad.Trans ( liftIO, lift )
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe ( mapMaybe )
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
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
data EquivalenceClasses arch = EquivalenceClasses { unClasses :: Seq.Seq (Seq.Seq [P.SynthInstruction arch]) }

data EquivalenceClass arch = EquivalenceClass (Seq.Seq [P.SynthInstruction arch])

-- | Construct an initial set of equivalence classes with a single program
equivalenceClasses :: [P.SynthInstruction arch] -> EquivalenceClasses arch
equivalenceClasses p = EquivalenceClasses (Seq.singleton (Seq.singleton p))

-- | Count the total number of programs in the set of equivalence classes
countPrograms :: EquivalenceClasses arch -> Int
countPrograms s = sum (map Seq.length (F.toList (unClasses s)))

data ClassifyState arch =
  ClassifyState { eqClassesSeq :: Seq.Seq (Seq.Seq (Int, [P.SynthInstruction arch]))
                , mergableClasses :: [Int]
                -- ^ Indexes of the equivalence classes to merge
                }

classAtIndex :: Int -> ClassifyM t arch (Maybe (Seq.Seq [P.SynthInstruction arch]))
classAtIndex ix = do
  eqClasses <- St.gets eqClassesSeq
  return $ fmap (fmap snd) (Seq.lookup ix eqClasses)

-- | Merge all of the classes marked in `mergableClasses` (if any)
extractMergableClasses :: ClassifyM t arch (Seq.Seq (Seq.Seq [P.SynthInstruction arch]))
extractMergableClasses = do
  s <- St.get
  return (Seq.fromList (fmap (fmap snd) (mapMaybe (flip Seq.lookup (eqClassesSeq s)) (mergableClasses s))))

-- | Count the number of non-empty classes
countRemainingClasses :: ClassifyM t arch Int
countRemainingClasses = do
  klasses <- St.gets eqClassesSeq
  return $ sum [ if Seq.null s then 0 else 1 | s <- F.toList klasses ]

-- | Add a class (by index) as a class marked to be merged (because it is
-- equivalent to the newly-discovered candidate program).
addMergableClass :: Int -> ClassifyM t arch ()
addMergableClass ix = St.modify' $ \s -> s { mergableClasses = ix : mergableClasses s }

-- | A version of 'lift' that actually works for our type.  The type signature
-- of 'lift' fixes the second type parameter as a 'Monad' of kind @* -> *@, but
-- our second parameter is @arch@.
liftC :: Syn t arch a -> ClassifyM t arch a
liftC a = ClassifyM (lift a)

-- | A Monad for tracking equivalence classes for reduction and merging.
--
-- This lets us easily mutate equivalence classes (removing candidates that are
-- invalidated by a counterexample)
newtype ClassifyM t arch a = ClassifyM { unClassify :: St.StateT (ClassifyState arch) (Syn t arch) a }
                           deriving (Functor,
                                     Applicative,
                                     Monad,
                                     St.MonadState (ClassifyState arch))

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
  let classesSeq = unClasses eqclasses
      s0 = ClassifyState { eqClassesSeq = numberNestedItems classesSeq
                         , mergableClasses = []
                         }
      act = classifyByClass target p 0
  mclasses <- St.evalStateT (unClassify act) s0
  case mclasses of
    Nothing -> return Nothing
    Just classes
      | Seq.null classes -> do
          -- Add a new equivalence class for this program, since it isn't
          -- equivalent to any existing class
          return (Just (EquivalenceClasses (Seq.singleton p Seq.<| unClasses eqclasses)))
      | otherwise -> do
          let eqclasses' = p Seq.<| mergeClasses classes
          return (Just (EquivalenceClasses (Seq.singleton eqclasses')))

-- | For each class in the current equivalence classes, see if the given program
-- matches any.  This function returns the list of all matching equivalence
-- classes.
--
-- If it matches more than one, equivalence class, we merge the matches in
-- 'classify'.
--
-- This function also produces counterexamples if the new program isn't
-- equivalent to the old programs.
--
-- Note that this function never adds equivalence classes, so iterating by index
-- is safe.
classifyByClass :: (SynC arch)
                => CS.RegisterizedInstruction arch
                -- ^ The target instruction
                -> [P.SynthInstruction arch]
                -- ^ The program to classify
                -> Int
                -- ^ The index (into the sequence of equivalence classes) of the
                -- equivalence class that we are currently analyzing
                -> ClassifyM t arch (Maybe (Seq.Seq (Seq.Seq [P.SynthInstruction arch])))
classifyByClass target p ix = do
  mklass <- classAtIndex ix
  case mklass of
    Nothing -> Just <$> extractMergableClasses
    Just klass -> do
      representative <- liftC $ chooseProgram (EquivalenceClass klass)
      eqv <- liftC $ testEquivalence p representative
      case eqv of
        F.Equivalent -> do
          addMergableClass ix
          classifyByClass target p (ix + 1)
        F.DifferentBehavior cx -> do
          -- See Note [Registerization and Counterexamples]
          let (target', cx') = CS.registerizeInstruction target cx
          liftC $ addTestCase cx'
          removeInvalidPrograms target' cx'
          nClasses <- countRemainingClasses
          case nClasses of
            0 -> return Nothing
            -- In this loop, we never add a new equivalence class (we could make
            -- one empty, though), so iterating is fine.  Since we don't add or
            -- remove classes, the iteration is stable and we never need to
            -- restart.
            _ -> classifyByClass target p (ix + 1)

-- | Remove the programs in the equivalence classes that do not have the same
-- output on the counterexample as the target instruction.
--
-- We have a new counterexample and need to do concrete executions of *each*
-- existing candidate programs and reject those that do not provide the correct
-- output.
removeInvalidPrograms :: (SynC arch)
                      => A.Instruction arch
                      -> CS.ConcreteState arch
                      -> ClassifyM t arch ()
removeInvalidPrograms target cx = do
  targetTC <- liftC $ mkTestCase cx [target]
  CE.TestSuccess (CE.TestResult { CE.resultContext = targetSt })
    <- liftC $ runConcreteTest targetTC
  klasses <- St.gets eqClassesSeq
  let allCandidates = foldr (Seq.><) Seq.empty klasses
  (testCases, testIndex) <- F.foldrM (makeAndIndexTest cx) ([], M.empty) allCandidates
  testResults <- liftC $ runConcreteTests testCases
  let testLists' = [ mapMaybeSeq (consistentWithTarget testIndex testResults targetSt) testList
                   | testList <- F.toList klasses
                   ]
  St.modify' $ \s -> s { eqClassesSeq = Seq.fromList testLists' }

mapMaybeSeq :: (a -> Maybe b) -> Seq.Seq a -> Seq.Seq b
mapMaybeSeq f = Seq.fromList . mapMaybe f . F.toList

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
                     -> Maybe (Int, [P.SynthInstruction arch])
consistentWithTarget testIndex testResults cx (testNum, tc) =
  case M.lookup nonce (CE.riExitedWithSignal testResults) of
    Just _ -> Nothing
    Nothing ->
      case M.lookup nonce (CE.riSuccesses testResults) of
        Nothing -> L.error (printf "Missing a test result for test number %d with nonce %d" testNum nonce)
        Just res
          | CE.resultContext res == cx -> Just (testNum, tc)
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
                 -> ClassifyM t arch ([CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)], M.Map Int Word64)
makeAndIndexTest cx (pix, insns) (cases, idx) = do
  tc <- liftC $ mkTestCase cx program
  return (tc : cases, M.insert pix (CE.testNonce tc) idx)
  where
    program = concatMap P.synthInsnToActual insns

-- | Assign a unique number to each list item (in the inner lists)
numberNestedItems :: (T.Traversable t) => t (t a) -> t (t (Int, a))
numberNestedItems = snd . L.mapAccumR number1 0
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
mergeClasses :: Seq.Seq (Seq.Seq pgm)
             -> Seq.Seq pgm
mergeClasses = F.foldr mappend Seq.empty

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
