{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module SemMC.Stochastic.Classify (
  classify,
  chooseClass,
  chooseProgram,
  EquivalenceClasses,
  emptyEquivalenceClasses,
  EquivalenceClass,
  countPrograms
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.RWS.Strict as RWS
import           Control.Monad.Trans ( liftIO, lift )
import qualified Data.Foldable as F
import           Data.Function ( on )
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe ( catMaybes, mapMaybe )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Traversable as T
import           Data.Word ( Word64 )
import           Text.Printf ( printf )

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.Some ( Some(..), viewSome )
import qualified Data.Parameterized.TraversableFC as FC
import qualified What4.Interface as S
import qualified What4.Expr.Builder as S
import qualified What4.BaseTypes as S
import qualified What4.Protocol.Online as WPO

import qualified Dismantle.Instruction as D

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.View as V
import qualified SemMC.Formula as F
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Stochastic.IORelation as IOR
import qualified SemMC.Log as L
import qualified SemMC.Stochastic.CandidateProgram as CP
import           SemMC.Stochastic.Monad
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Stochastic.Statistics as S
import           SemMC.Symbolic ( Sym )
import           SemMC.Util ( allBoundVars, filterMapF )

-- | A set of equivalence classes of programs
data EquivalenceClasses a =
  EquivalenceClasses { unClasses :: Seq.Seq (EquivalenceClass a) }
  deriving (Show, Functor, F.Foldable, T.Traversable)

data EquivalenceClass a =
  EquivalenceClass { ecPrograms :: Seq.Seq a
                   , ecRepresentative :: Int
                   }
  deriving (Show, Functor, F.Foldable, T.Traversable)

equivalenceClass :: CP.CandidateProgram t solver arch -> EquivalenceClass (CP.CandidateProgram t solver arch)
equivalenceClass p = EquivalenceClass { ecPrograms = Seq.singleton p
                                      , ecRepresentative = 0
                                      }

-- | Construct a new equivalence class from a list, choosing the best
-- representative
equivalenceClassFromList :: (a -> a -> Ordering) -> a -> Seq.Seq a -> EquivalenceClass a
equivalenceClassFromList cmp e1 es =
  EquivalenceClass { ecPrograms = e1 Seq.<| es
                   , ecRepresentative = fst $ F.foldl' takeBest (0, e1) (Seq.zip (Seq.fromList [1..Seq.length es]) es)
                   }
  where
    takeBest acc@(_ixr, r) elt@(_ixe, e) =
      case cmp r e of
        GT -> acc
        EQ -> acc
        LT -> elt

equivalenceClassFromListMay :: (a -> a -> Ordering) -> Seq.Seq a -> Maybe (EquivalenceClass a)
equivalenceClassFromListMay cmp s =
  case Seq.viewl s of
    Seq.EmptyL -> Nothing
    p1 Seq.:< rest -> Just (equivalenceClassFromList cmp p1 rest)

-- | Construct an initial empty set of equivalence classes with no programs.
emptyEquivalenceClasses :: EquivalenceClasses (CP.CandidateProgram t solver arch)
emptyEquivalenceClasses = EquivalenceClasses Seq.empty

-- | Count the total number of programs in the set of equivalence classes
countPrograms :: EquivalenceClasses (CP.CandidateProgram t solver arch) -> Int
countPrograms s = sum (map (Seq.length . ecPrograms) (F.toList (unClasses s)))

classAtIndex :: Int -> ClassifyM t solver sh arch (Maybe (EquivalenceClass (CP.CandidateProgram t solver arch)))
classAtIndex ix = do
  EquivalenceClasses eqClasses <- RWS.gets eqClassesSeq
  return $ fmap (fmap snd) (Seq.lookup ix eqClasses)

-- | Merge all of the classes marked in `mergableClasses` (if any)
extractMergableClasses :: ClassifyM t solver sh arch (Seq.Seq (Seq.Seq (CP.CandidateProgram t solver arch)))
extractMergableClasses = do
  s <- RWS.get
  let classesToKeep = mapMaybe (flip Seq.lookup (unClasses (eqClassesSeq s))) (mergableClasses s)
      justProgramGroups = fmap ((fmap snd) . ecPrograms) classesToKeep
  return (Seq.fromList justProgramGroups)

-- | Count the number of non-empty classes
countRemainingClasses :: ClassifyM t solver sh arch Int
countRemainingClasses = do
  klasses <- RWS.gets eqClassesSeq
  return $ sum [ if Seq.null (ecPrograms klass) then 0 else 1
               | klass <- F.toList (unClasses klasses)
               ]

-- | Add a class (by index) as a class marked to be merged (because it is
-- equivalent to the newly-discovered candidate program).
addMergableClass :: Int -> ClassifyM t solver sh arch ()
addMergableClass ix = RWS.modify' $ \s -> s { mergableClasses = ix : mergableClasses s }

-- | Given a set of initial equivalence classes, assign the new program to one
-- of them (or create a new equivalence class if the new program doesn't match
-- any).
--
-- This function returns 'Nothing' if the new program generates a counterexample
-- that invalidates all of the previous programs.
classify :: forall arch t solver
          . (SynC arch, WPO.OnlineSolver t solver)
         => AC.RegisterizedInstruction arch
         -- ^ The instruction whose semantics we are trying to learn
         -> CP.CandidateProgram t solver arch
         -- ^ The program we just learned and need to add to an equivalence class
         -> EquivalenceClasses (CP.CandidateProgram t solver arch)
         -- ^ The current equivalence classes
         -> Syn t solver arch (Maybe (EquivalenceClasses (CP.CandidateProgram t solver arch)))
classify target@AC.RI{ AC.riOperands = oplist, AC.riOpcode = opc } p eqclasses = do
  let s0 = ClassifyState { eqClassesSeq = snd (L.mapAccumR numberItem 0 eqclasses)
                         , mergableClasses = []
                         }
      act = classifyByClass target p 0
  Just iorel <- opcodeIORelation opc
  let e0 = ClassifyEnv { operandList = oplist
                       , iorelation = iorel
                       }
  (mclasses, _) <- RWS.evalRWST (unClassify act) e0 s0
  case mclasses of
    Nothing -> return Nothing
    Just classes
      | Seq.null classes -> do
          -- Add a new equivalence class for this program, since it isn't
          -- equivalent to any existing class
          return (Just (EquivalenceClasses (equivalenceClass p Seq.<| unClasses eqclasses)))
      | otherwise -> do
          return (Just (EquivalenceClasses (Seq.singleton (equivalenceClassFromList compareCandidate p (mergeClasses classes)))))

numberItem :: Int -> a -> (Int, (Int, a))
numberItem n itm = (n + 1, (n, itm))

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
classifyByClass :: forall arch t solver sh
                 . (SynC arch, WPO.OnlineSolver t solver)
                => AC.RegisterizedInstruction arch
                -- ^ The target instruction
                -> CP.CandidateProgram t solver arch
                -- ^ The program to classify
                -> Int
                -- ^ The index (into the sequence of equivalence classes) of the
                -- equivalence class that we are currently analyzing
                -> ClassifyM t solver sh arch (Maybe (Seq.Seq (Seq.Seq (CP.CandidateProgram t solver arch))))
classifyByClass target p ix = do
  mklass <- classAtIndex ix
  case mklass of
    Nothing -> Just <$> extractMergableClasses
    Just klass -> do
      env <- RWS.ask
      representative <- liftC $ chooseProgram klass
      (eqv, equivTime) <- liftC $ timeSyn $ testEquivalence env p representative
      case eqv of
        F.Equivalent -> do
          case target of
            AC.RI { AC.riOpcode = oc } -> do
              liftC $ withStats $ S.recordSolverInvocation (Some oc) (S.Completed equivTime)
              liftC $ L.logM L.Info $ printf "Equivalent candidate program for %s" (showF oc)
          addMergableClass ix
          classifyByClass target p (ix + 1)
        F.DifferentBehavior (promoteCounterexample (Proxy @arch) -> cx) -> do
          -- See Note [Registerization and Counterexamples]
          case target of
            AC.RI { AC.riOpcode = oc } -> do
              liftC $ withStats $ S.recordCounterexample (Some oc)
              liftC $ withStats $ S.recordSolverInvocation (Some oc) (S.Completed equivTime)
              liftC $ L.logM L.Info $ printf "Found a counterexample while classifying a candidate program for %s (CX=%s)" (showF oc) (show cx)
          let (target', cx') = AC.registerizeInstruction target cx
          liftC $ addTestCase cx'
          ix' <- removeInvalidPrograms ix target' cx'
          nClasses <- countRemainingClasses
          case nClasses of
            0 -> return Nothing
            -- In this loop, we never add a new equivalence class (we could make
            -- one empty, though), so iterating is fine.  Since we don't add or
            -- remove classes, the iteration is stable and we never need to
            -- restart.
            _ -> classifyByClass target p ix'
        F.Timeout -> do
          -- If we time out, just assume that we can't put this candidate
          -- program in the current equivalence class.  If it times out on all
          -- of them, it will get its own (probably not very useful) equivalence
          -- class.
          case target of
            AC.RI { AC.riOpcode = oc } -> do
              liftC $ withStats $ S.recordSolverInvocation (Some oc) (S.Timeout equivTime)
              liftC $ L.logM L.Info $ printf "Solver timeout while classifying a candidate program for %s" (showF oc)
          classifyByClass target p (ix + 1)

-- | The counterexamples we get from the theorem prover
--
-- FIXME: It would be nice to make 'ConcreteState' into a newtype so that this is safer
promoteCounterexample :: (AC.ConcreteArchitecture arch) => proxy arch -> L.ArchState arch V.Value -> V.ConcreteState arch
promoteCounterexample proxy cx = F.foldl' addMissingKey cx (MapF.toList (AC.zeroState proxy))
  where
    addMissingKey m (MapF.Pair k v) =
      case MapF.lookup k m of
        Just _ -> m
        Nothing -> MapF.insert k v m


-- | Remove the programs in the equivalence classes that do not have the same
-- output on the counterexample as the target instruction.
--
-- We have a new counterexample and need to do concrete executions of *each*
-- existing candidate programs and reject those that do not provide the correct
-- output.
--
-- Returns an updated index (the next index to pass to a recursive call).  If no
-- equivalence classes are removed, this is @ix + 1@.
removeInvalidPrograms :: forall t solver arch sh
                       . (SynC arch)
                      => Int
                      -- ^ The current index into the equivalence classes for
                      -- our iteration; this will be modified on return if we
                      -- had to eliminate an empty equivalence class
                      -> A.Instruction arch
                      -- ^ The target instruction
                      -> V.ConcreteState arch
                      -- ^ A learned counterexample
                      -> ClassifyM t solver sh arch Int
removeInvalidPrograms ix target cx = do
  targetTC <- liftC $ mkTestCase cx [target]
  CE.TestSuccess (CE.TestResult { CE.resultContext = targetSt })
    <- liftC $ runConcreteTest targetTC
  klasses <- RWS.gets eqClassesSeq
  let allCandidates = foldr (\k a -> ecPrograms k Seq.>< a) Seq.empty (unClasses klasses)
  (testCases, testIndex) <- F.foldrM (makeAndIndexTest cx) ([], M.empty) allCandidates
  testResults <- liftC $ runConcreteTests testCases
  -- Since we are removing entries here, we have to rebuild each equivalence
  -- class to select a new representative.  This is done by
  -- 'equivalenceClassFromListMay'.
  let testListsMay' :: [Maybe (EquivalenceClass (Int, CP.CandidateProgram t solver arch))]
      testListsMay' = [ equivalenceClassFromListMay (compareCandidate `on` snd) maybes
                      | ec <- F.toList (unClasses klasses)
                      , let maybes = mapMaybeSeq (consistentWithTarget testIndex testResults targetSt) (ecPrograms ec)
                      ]
  mergable <- RWS.gets mergableClasses
  let (ix', testLists', mergable') = computeNewIndexes ix testListsMay' mergable
      klasses' = EquivalenceClasses (Seq.fromList testLists')
  RWS.modify' $ \s -> s { eqClassesSeq = klasses'
                       , mergableClasses = mergable'
                       }

  let nRemoved = countPrograms (fmap snd klasses) - countPrograms (fmap snd klasses')
  case target of
    D.Instruction oc _
      | nRemoved > 0 -> do
          liftC $ withStats $ S.recordRemovedCandidatePrograms (Some oc) nRemoved
          liftC $ L.logM L.Info $ printf "Removed %d candidate programs invalidated by a counterexample for %s" nRemoved (showF oc)
      | otherwise -> return ()
  return ix'

-- | Correct indexes (from the next iteration index and the mergable classes
-- list) if we have to remove any empty equivalence classes.
--
-- Note: if an index into the list is to be removed, we have to remove it from
-- the index list.
computeNewIndexes :: Int
                  -- ^ The index of the current iteration (will be the basis for the next index)
                  --
                  -- The index is into the second argument, and will need to be
                  -- corrected if earlier entries are eliminated
                  -> [Maybe a]
                  -- ^ The sequence where some elements may be filtered (replaced by a Nothing)
                  -> [Int]
                  -- ^ Mergable entries (indexes into the above list, which may
                  -- need to be corrected for Nothings)
                  -> (Int, [a], [Int])
computeNewIndexes curIdx elts indexes =
  (nextIdx, catMaybes elts, updatedIndexes)
  where
    -- First, compute all of the indexes we are removing.  We can compute the
    -- index corrections based entirely on that.  Then the new list is just a
    -- catMaybes away.
    removedIndexes = mapMaybe keepNothingIndex (zip [0..] elts)
    keepNothingIndex (idx, melt) = maybe (Just idx) (const Nothing) melt

    correctIndex idx = idx - length (filter (<= curIdx) removedIndexes)

    -- The next index is naturally @nextIdx + 1@; however, we have to make a
    -- correction (by one) for each index less than or equal to the current
    -- index.  Count the number of indexes @<=@ curIdx in removedIndexes, and
    -- subtract that from curIdx (then add one to advance).
    --
    -- > curIdx = 5, removedIndexes = [0].  (5 - 1) + 1 => nextIndex = 5
    --
    -- > curIdx = 5, removedIndexes = [5]. (5 - 1) + 1 => nextIndex = 5
    --
    -- > curIdx = 5, removedIndexes = [3, 5].  (5 - 2) + 1 => nextIndex  = 4
    nextIdx = correctIndex curIdx + 1
    updatedIndexes = mapMaybe correctOrRemoveIndex indexes
    correctOrRemoveIndex idx
      | idx `elem` removedIndexes = Nothing
      | otherwise = Just (correctIndex idx)

mapMaybeSeq :: (a -> Maybe b) -> Seq.Seq a -> Seq.Seq b
mapMaybeSeq f = Seq.fromList . mapMaybe f . F.toList

-- | Reject candidate programs that have a different behavior on the given counterexample
consistentWithTarget :: (SynC arch)
                     => M.Map Int Word64
                     -- ^ The mapping of candidate program numbers to test
                     -- nonces (so that we can find the results for a given
                     -- candidate program)
                     -> CE.ResultIndex (V.ConcreteState arch)
                     -- ^ All of the results that came back from the remote runner
                     -> V.ConcreteState arch
                     -- ^ The output of the target instruction on the test vector
                     -> (Int, CP.CandidateProgram t solver arch)
                     -- ^ The current program (and its index into the @testIndex@ map)
                     -> Maybe (Int, CP.CandidateProgram t solver arch)
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
                 => V.ConcreteState arch
                 -> (Int, CP.CandidateProgram t solver arch)
                 -> ([CE.TestCase (V.ConcreteState arch) (A.Instruction arch)], M.Map Int Word64)
                 -> ClassifyM t solver sh arch ([CE.TestCase (V.ConcreteState arch) (A.Instruction arch)], M.Map Int Word64)
makeAndIndexTest cx (pix, cp) (cases, idx) = do
  tc <- liftC $ mkTestCase cx program
  return (tc : cases, M.insert pix (CE.testNonce tc) idx)
  where
    program = concatMap P.synthInsnToActual (CP.cpInstructions cp)

-- | Heuristically-choose the best equivalence class
--
-- It prefers classes with more examples and fewer uninterpreted
-- functions. Returns 'Nothing' if all classes are empty.
chooseClass :: EquivalenceClasses (CP.CandidateProgram t solver arch)
            -> Syn t solver arch (Maybe (EquivalenceClass (CP.CandidateProgram t solver arch)))
chooseClass (EquivalenceClasses klasses) =
  case Seq.viewl klasses of
    Seq.EmptyL -> return Nothing
    k1 Seq.:< rest -> Just <$> F.foldlM bestClass k1 rest

bestClass :: EquivalenceClass (CP.CandidateProgram t solver arch)
          -> EquivalenceClass (CP.CandidateProgram t solver arch)
          -> Syn t solver arch (EquivalenceClass (CP.CandidateProgram t solver arch))
bestClass k1 k2 = do
  best1 <- chooseProgram k1
  best2 <- chooseProgram k2
  case compareCandidate best1 best2 of
    GT -> return k1
    EQ -> return k1
    LT -> return k2

-- | Choose the best program out of an equivalence class as the basis for a formula
--
-- We want to minimize the number of uninterpreted functions and non-linear
-- operations (as well as formula size).
chooseProgram :: EquivalenceClass (CP.CandidateProgram t solver arch) -> Syn t solver arch (CP.CandidateProgram t solver arch)
chooseProgram ec =
  case Seq.lookup (ecRepresentative ec) (ecPrograms ec) of
    Nothing -> L.error "BUG: Invalid representative index in equivalence class"
    Just repr -> return repr

-- | Compare two programs: @p1@ is GT @p2@ if it is "better" according to the
-- heuristic
--
-- FIXME: Add a weight for formulas that mix arithmetic and bitwise BV
-- operations; mixed mode is more expensive than formulas that don't mix.
-- Mixing includes comparisons.
compareCandidate :: CP.CandidateProgram t solver arch -> CP.CandidateProgram t solver arch -> Ordering
compareCandidate p1 p2
  | uf1 < uf2 = GT
  | uf2 < uf1 = LT
  | nonLinear1 < nonLinear2 = GT
  | nonLinear2 < nonLinear1 = LT
  | size1 < size2 = GT
  | otherwise = LT
  where
    (uf1, nonLinear1, size1) = summarizeFormula (CP.cpFormula p1)
    (uf2, nonLinear2, size2) = summarizeFormula (CP.cpFormula p2)


summarizeFormula :: forall t solver arch . F.Formula (Sym t solver) arch -> (Int, Int, Int)
summarizeFormula f = F.foldl' (summarizeExpr) (0, 0, 0) someExprs
  where
    someExprs = MapF.elems (F.formDefs f)

summarizeExpr :: (Int, Int, Int) -> Some (S.Expr t) -> (Int, Int, Int)
summarizeExpr acc@(uf, nl, sz) (Some se) =
  case se of
    S.SemiRingLiteral {} -> acc
    S.BVExpr {} -> acc
    S.BoundVarExpr {} -> acc
    S.NonceAppExpr ne ->
      case S.nonceExprApp ne of
        S.FnApp {} -> (uf + 1, nl, sz + 1)
        _ -> (uf, nl, sz + 1)
    S.AppExpr ae -> FC.foldlFC' summarizeElt acc (S.appExprApp ae)
    S.StringExpr {} -> acc

summarizeElt :: (Int, Int, Int) -> S.Expr t tp -> (Int, Int, Int)
summarizeElt acc@(uf, nl, sz) elt =
  case elt of
    S.SemiRingLiteral {} -> acc
    S.BVExpr {} -> acc
    S.BoundVarExpr {} -> acc
    S.NonceAppExpr ne ->
      case S.nonceExprApp ne of
        S.FnApp {} -> (uf + 1, nl, sz + 1)
        _ -> (uf, nl, sz + 1)
    S.AppExpr ae ->
      case S.appExprApp ae of
        -- According to crucible, any occurrence of this constructor is non-linear
        S.SemiRingMul {} -> (uf, nl + 1, sz + 1)
        S.BVMul _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVUdiv _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVUrem _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVSdiv _ lhs rhs -> addIfNonlinear lhs rhs acc
        S.BVSrem _ lhs rhs -> addIfNonlinear lhs rhs acc
        _ -> (uf, nl, sz + 1)
    S.StringExpr {} -> acc

-- | If the operation is nonlinear (based on operands), increment the nonlinear
-- op count.
--
-- The operation is non-linear if one of the operands is /not/ a constant.
addIfNonlinear :: S.Expr t (S.BaseBVType w)
               -> S.Expr t (S.BaseBVType w)
               -> (Int, Int, Int)
               -> (Int, Int, Int)
addIfNonlinear lhs rhs (uf, nl, sz)
  | isBVConstant lhs || isBVConstant rhs = (uf, nl, sz + 1)
  | otherwise = (uf, nl + 1, sz + 1)

isBVConstant :: S.Expr t (S.BaseBVType w) -> Bool
isBVConstant e =
  case e of
    S.BVExpr {} -> True
    _ -> False

-- | Flatten a set of equivalence classes into one equivalence class
mergeClasses :: Seq.Seq (Seq.Seq pgm)
             -> Seq.Seq pgm
mergeClasses = F.foldr mappend Seq.empty

-- | Use an SMT solver to check if two programs are equivalent.
--
-- If they are not, return an input that demonstrates the difference.
--
-- We pass in the environment so that we can access the IORelation for the
-- current target instruction
testEquivalence :: (P.ArchitectureWithPseudo arch, AC.ConcreteArchitecture arch, WPO.OnlineSolver t solver)
                => ClassifyEnv arch sh
                -> CP.CandidateProgram t solver arch
                -> CP.CandidateProgram t solver arch
                -> Syn t solver arch (F.EquivalenceResult arch V.Value)
testEquivalence env p representative = do
  L.logM L.Info "Testing equivalence of:"
  L.logM L.Info (show newFormula)
  L.logM L.Info (show repFormula)
  withSymBackend $ \sym -> do
    liftIO $ F.formulasEquivConcrete sym newFormula repFormula
  where
    newFormula = projectRelevantLocations env (CP.cpFormula p)
    repFormula = projectRelevantLocations env (CP.cpFormula representative)

-- | Using the operand list and IORelation from the environment, project out all
-- of the relevant locations defined by the formula.
projectRelevantLocations :: forall t solver arch sh
                          . (AC.ConcreteArchitecture arch)
                         => ClassifyEnv arch sh
                         -> F.Formula (Sym t solver) arch
                         -> F.Formula (Sym t solver) arch
projectRelevantLocations env f0 =
  F.Formula { F.formDefs = projectedDefs
            , F.formParamVars = paramVars
            }
  where
    usedRefs = IOR.outputs (iorelation env)
    usedLocs = S.fromList $ mapMaybe refToLoc (F.toList usedRefs)
    boundVars = mconcat (map (viewSome allBoundVars) (MapF.elems projectedDefs))
    projectedDefs = filterMapF keepUsedDef (F.formDefs f0)
    paramVars = filterMapF keepUsedVar (F.formParamVars f0)

    keepUsedDef :: forall tp v . A.Location arch tp -> v tp -> Bool
    keepUsedDef k _ = S.member (Some k) usedLocs

    keepUsedVar :: forall tp . A.Location arch tp -> S.BoundVar (Sym t solver) tp -> Bool
    keepUsedVar _ bv = S.member (Some bv) boundVars

    refToLoc oref =
      case oref of
        IOR.ImplicitOperand (Some (V.View _ loc)) -> Just (Some loc)
        IOR.OperandRef (Some ix) -> do
          sv <- AC.operandToSemanticView (Proxy @arch) (operandList env SL.!! ix)
          case sv of
            V.SemanticView { V.semvView = V.View _ loc } -> return (Some loc)

-- Monad definition

-- | The state for 'ClassifyM' that lets us maintain a mutable set of
-- equivalence classes.
data ClassifyState t solver arch =
  ClassifyState { eqClassesSeq :: EquivalenceClasses (Int, CP.CandidateProgram t solver arch)
                -- ^ The current set of equivalence classes
                , mergableClasses :: [Int]
                -- ^ Indexes of the equivalence classes to merge
                }

-- | The environment contains the IORelation and operand list for the target
-- instruction.  We need these for the equivalence test, as we only want to test
-- equality on the locations that are actually relevant for our target
-- instruction (the search is free to use all other locations as scratch space).
data ClassifyEnv arch sh =
  ClassifyEnv { operandList :: SL.List (A.Operand arch) sh
              , iorelation :: IOR.IORelation arch sh
              }

-- | A version of 'lift' that actually works for our type.  The type signature
-- of 'lift' fixes the second type parameter as a 'Monad' of kind @* -> *@, but
-- our second parameter is @arch@.
liftC :: Syn t solver arch a -> ClassifyM t solver sh arch a
liftC a = ClassifyM (lift a)

-- | A Monad for tracking equivalence classes for reduction and merging.
--
-- This lets us easily mutate equivalence classes (removing candidates that are
-- invalidated by a counterexample)
newtype ClassifyM t solver sh arch a = ClassifyM { unClassify :: RWS.RWST (ClassifyEnv arch sh) () (ClassifyState t solver arch) (Syn t solver arch) a }
                           deriving (Functor,
                                     Applicative,
                                     Monad,
                                     RWS.MonadReader (ClassifyEnv arch sh),
                                     RWS.MonadState (ClassifyState t solver arch))

{- Note [Registerization and Counterexamples]

  FIXME: If cx /= cx', we don't know if this is still a counterexample.  That
  would be kind of strange, but we need to be able to handle that case.

  Maybe we tweak the counterexample at a higher level (when we generate it)
  and add that as a test case.  If it is still a counterexample, throwing
  away candidate programs is still sound.


-}
