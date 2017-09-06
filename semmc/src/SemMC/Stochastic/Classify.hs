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
  EquivalenceClass,
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

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.BaseTypes as S

import qualified SemMC.Architecture as A
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Equivalence as F
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Concrete.State as CS
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.Pseudo as P
import           SemMC.Symbolic ( Sym )

-- | A set of equivalence classes of programs
data EquivalenceClasses t arch = EquivalenceClasses { unClasses :: Seq.Seq (Seq.Seq (CandidateProgram t arch)) }

data EquivalenceClass t arch = EquivalenceClass (Seq.Seq (CandidateProgram t arch))

-- | Construct an initial set of equivalence classes with a single program
equivalenceClasses :: CandidateProgram t arch -> EquivalenceClasses t arch
equivalenceClasses p = EquivalenceClasses (Seq.singleton (Seq.singleton p))

-- | Count the total number of programs in the set of equivalence classes
countPrograms :: EquivalenceClasses t arch -> Int
countPrograms s = sum (map Seq.length (F.toList (unClasses s)))

data ClassifyState t arch =
  ClassifyState { eqClassesSeq :: Seq.Seq (Seq.Seq (Int, CandidateProgram t arch))
                , mergableClasses :: [Int]
                -- ^ Indexes of the equivalence classes to merge
                }

classAtIndex :: Int -> ClassifyM t arch (Maybe (Seq.Seq (CandidateProgram t arch)))
classAtIndex ix = do
  eqClasses <- St.gets eqClassesSeq
  return $ fmap (fmap snd) (Seq.lookup ix eqClasses)

-- | Merge all of the classes marked in `mergableClasses` (if any)
extractMergableClasses :: ClassifyM t arch (Seq.Seq (Seq.Seq (CandidateProgram t arch)))
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
newtype ClassifyM t arch a = ClassifyM { unClassify :: St.StateT (ClassifyState t arch) (Syn t arch) a }
                           deriving (Functor,
                                     Applicative,
                                     Monad,
                                     St.MonadState (ClassifyState t arch))

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
         -> CandidateProgram t arch
         -- ^ The program we just learned and need to add to an equivalence class
         -> EquivalenceClasses t arch
         -- ^ The current equivalence classes
         -> Syn t arch (Maybe (EquivalenceClasses t arch))
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
                -> CandidateProgram t arch
                -- ^ The program to classify
                -> Int
                -- ^ The index (into the sequence of equivalence classes) of the
                -- equivalence class that we are currently analyzing
                -> ClassifyM t arch (Maybe (Seq.Seq (Seq.Seq (CandidateProgram t arch))))
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
                     -> (Int, CandidateProgram t arch)
                     -- ^ The current program (and its index into the @testIndex@ map)
                     -> Maybe (Int, CandidateProgram t arch)
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
                 -> (Int, CandidateProgram t arch)
                 -> ([CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)], M.Map Int Word64)
                 -> ClassifyM t arch ([CE.TestCase (CS.ConcreteState arch) (A.Instruction arch)], M.Map Int Word64)
makeAndIndexTest cx (pix, cp) (cases, idx) = do
  tc <- liftC $ mkTestCase cx program
  return (tc : cases, M.insert pix (CE.testNonce tc) idx)
  where
    program = concatMap P.synthInsnToActual (cpInstructions cp)

-- | Assign a unique number to each list item (in the inner lists)
numberNestedItems :: (T.Traversable t) => t (t a) -> t (t (Int, a))
numberNestedItems = snd . L.mapAccumR number1 0
  where
    number1 n = L.mapAccumR number2 n
    number2 n itm = (n + 1, (n, itm))

-- | Heuristically-choose the best equivalence class
--
-- It prefers classes with more examples and fewer uninterpreted functions
chooseClass :: EquivalenceClasses t arch -> Syn t arch (EquivalenceClass t arch)
chooseClass (EquivalenceClasses klasses) =
  case Seq.viewl klasses of
    Seq.EmptyL -> L.error "Empty equivalence class set"
    k1 Seq.:< rest -> EquivalenceClass <$> F.foldlM bestClass k1 rest

bestClass :: Seq.Seq (CandidateProgram t arch)
          -> Seq.Seq (CandidateProgram t arch)
          -> Syn t arch (Seq.Seq (CandidateProgram t arch))
bestClass k1 k2 = do
  best1 <- chooseProgram (EquivalenceClass k1)
  best2 <- chooseProgram (EquivalenceClass k2)
  case compareCandidate best1 best2 of
    GT -> return k1
    EQ -> return k1
    LT -> return k2

-- | Choose the best program out of an equivalence class as the basis for a formula
--
-- We want to minimize the number of uninterpreted functions and non-linear
-- operations (as well as formula size).
chooseProgram :: EquivalenceClass t arch -> Syn t arch (CandidateProgram t arch)
chooseProgram (EquivalenceClass progs) =
  case Seq.viewl progs of
    Seq.EmptyL -> L.error "BUG: Empty equivalence classes are not allowed"
    p1 Seq.:< rest -> return $ F.foldl' betterCandidateProgram p1 rest

-- | Return the better of two candidate programs
--
-- Note that this is definitely a heuristic.  We probably want to choose the
-- formula with the fewest uninterpreted functions and non-linear functions, but
-- it is not obvious which of those to prefer.  If there is a tie, then use
-- formula size.
--
-- FIXME: Add a weight for formulas that mix arithmetic and bitwise BV
-- operations; mixed mode is more expensive than formulas that don't mix.
-- Mixing includes comparisons.
betterCandidateProgram :: CandidateProgram t arch
                       -> CandidateProgram t arch
                       -> CandidateProgram t arch
betterCandidateProgram p1 p2 =
  case compareCandidate p1 p2 of
    GT -> p1
    EQ -> p1
    LT -> p2

-- | Compare two programs: @p1@ is GT @p2@ if it is "better" according to the
-- heuristic
compareCandidate :: CandidateProgram t arch -> CandidateProgram t arch -> Ordering
compareCandidate p1 p2
  | uf1 < uf2 = GT
  | uf2 < uf1 = LT
  | nonLinear1 < nonLinear2 = GT
  | nonLinear2 < nonLinear1 = LT
  | size1 < size2 = GT
  | otherwise = LT
  where
    (uf1, nonLinear1, size1) = summarizeFormula (cpFormula p1)
    (uf2, nonLinear2, size2) = summarizeFormula (cpFormula p2)


summarizeFormula :: forall t arch . F.Formula (Sym t) arch -> (Int, Int, Int)
summarizeFormula f = F.foldl' (summarizeExpr) (0, 0, 0) someExprs
  where
    someExprs = MapF.elems (F.formDefs f)

summarizeExpr :: (Int, Int, Int) -> Some (S.Elt t) -> (Int, Int, Int)
summarizeExpr acc@(uf, nl, sz) (Some se) =
  case se of
    S.SemiRingLiteral {} -> acc
    S.BVElt {} -> acc
    S.BoundVarElt {} -> acc
    S.NonceAppElt ne ->
      case S.nonceEltApp ne of
        S.FnApp {} -> (uf + 1, nl, sz + 1)
        _ -> (uf, nl, sz + 1)
    S.AppElt ae -> FC.foldlFC' summarizeElt acc (S.appEltApp ae)

summarizeElt :: (Int, Int, Int) -> S.Elt t tp -> (Int, Int, Int)
summarizeElt acc@(uf, nl, sz) elt =
  case elt of
    S.SemiRingLiteral {} -> acc
    S.BVElt {} -> acc
    S.BoundVarElt {} -> acc
    S.NonceAppElt ne ->
      case S.nonceEltApp ne of
        S.FnApp {} -> (uf + 1, nl, sz + 1)
        _ -> (uf, nl, sz + 1)
    S.AppElt ae ->
      case S.appEltApp ae of
        -- According to crucible, any occurrence of this constructor is non-linear
        S.SemiRingMul {} -> (uf, nl + 1, sz + 1)
        S.BVMul _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVUdiv _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVUrem _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVSdiv _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVSrem _ lhs rhs -> addIfNonlinear lhs rhs acc
        _ -> (uf, nl, sz + 1)

-- | If the operation is nonlinear (based on operands), increment the nonlinear
-- op count.
--
-- The operation is non-linear if one of the operands is /not/ a constant.
addIfNonlinear :: S.Elt t (S.BaseBVType w)
               -> S.Elt t (S.BaseBVType w)
               -> (Int, Int, Int)
               -> (Int, Int, Int)
addIfNonlinear lhs rhs (uf, nl, sz)
  | isBVConstant lhs || isBVConstant rhs = (uf, nl, sz + 1)
  | otherwise = (uf, nl + 1, sz + 1)

isBVConstant :: S.Elt t (S.BaseBVType w) -> Bool
isBVConstant e =
  case e of
    S.BVElt {} -> True
    _ -> False

-- | Flatten a set of equivalence classes into one equivalence class
mergeClasses :: Seq.Seq (Seq.Seq pgm)
             -> Seq.Seq pgm
mergeClasses = F.foldr mappend Seq.empty

-- | Use an SMT solver to check if two programs are equivalent.
--
-- If they are not, return an input that demonstrates the difference.
testEquivalence :: (P.ArchitectureWithPseudo arch)
                => CandidateProgram t arch
                -> CandidateProgram t arch
                -> Syn t arch (F.EquivalenceResult arch CS.Value)
testEquivalence p representative = do
  withSymBackend $ \sym -> do
    liftIO $ F.formulasEquivConcrete sym (cpFormula p) (cpFormula representative)


{- Note [Registerization and Counterexamples]

  FIXME: If cx /= cx', we don't know if this is still a counterexample.  That
  would be kind of strange, but we need to be able to handle that case.

  Maybe we tweak the counterexample at a higher level (when we generate it)
  and add that as a test case.  If it is still a counterexample, throwing
  away candidate programs is still sound.


-}
