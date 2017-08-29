{-# LANGUAGE FlexibleContexts #-}
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

import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Set as S

import           Data.Parameterized.Classes ( OrdF )
import           SemMC.Architecture ( Operand )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Equivalence as F
import qualified SemMC.Formula.Instantiate as F
import           SemMC.ConcreteState ( ConcreteState, Value )
import           SemMC.Symbolic ( Sym )
import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.Pseudo ( ArchitectureWithPseudo, SynthInstruction(..) )

-- | A set of equivalence classes of programs
data EquivalenceClasses arch = EquivalenceClasses { unClasses :: S.Set (S.Set [SynthInstruction arch]) }

data EquivalenceClass arch = EquivalenceClass { unClass :: S.Set [SynthInstruction arch] }

-- | Construct an initial set of equivalence classes with a single program
equivalenceClasses :: [SynthInstruction arch] -> EquivalenceClasses arch
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
          . (ArchitectureWithPseudo arch, OrdF (Operand arch))
         => [SynthInstruction arch]
         -> EquivalenceClasses arch
         -> Syn t arch (Maybe (EquivalenceClasses arch))
classify p eqclasses = do
  mclasses <- classifyByClass p S.empty (S.toList (unClasses eqclasses))
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
classifyByClass :: (ArchitectureWithPseudo arch, Ord (SynthInstruction arch))
                => [SynthInstruction arch]
                -- ^ The program to classify
                -> S.Set (S.Set [SynthInstruction arch])
                -- ^ The set of classes matching the input program
                -> [S.Set [SynthInstruction arch]]
                -- ^ The existing equivalence classes
                -> Syn t arch (Maybe (S.Set (S.Set [SynthInstruction arch])))
classifyByClass p eqs klasses =
  case klasses of
    [] -> return (Just eqs)
    (klass:rest) -> do
      representative <- chooseProgram (EquivalenceClass klass)
      eqv <- testEquivalence p representative
      case eqv of
        F.Equivalent -> classifyByClass p (S.insert klass eqs) rest
        F.DifferentBehavior cx -> do
          addTestCase cx
          eqclasses' <- removeInvalidPrograms cx undefined
          case countPrograms eqclasses' of
            0 -> return Nothing
            -- FIXME: We are modifying eqclasses while we iterate over them.
            -- What are the semantics there?  The paper isn't precise.
            _ -> classifyByClass p eqs rest

-- | Remove the programs in the equivalence classes that do not have the same
-- output on the counterexample as the target instruction
removeInvalidPrograms :: ConcreteState arch -> EquivalenceClasses arch -> Syn t arch (EquivalenceClasses arch)
removeInvalidPrograms = undefined

-- | Heuristically-choose the best equivalence class
--
-- It prefers classes with more examples and fewer uninterpreted functions
chooseClass :: EquivalenceClasses arch -> Syn t arch (EquivalenceClass arch)
chooseClass = undefined

-- | Choose the best program out of an equivalence class as the basis for a formula
--
-- We want to minimize the number of uninterpreted functions and non-linear
-- operations (as well as formula size).
chooseProgram :: EquivalenceClass arch -> Syn t arch [SynthInstruction arch]
chooseProgram = undefined

-- | Flatten a set of equivalence classes into one equivalence class
mergeClasses :: (Ord pgm)
             => S.Set (S.Set pgm)
             -> S.Set pgm
mergeClasses s = S.unions (S.toList s)

-- | Convert an instruction into a 'F.Formula'
instructionFormula :: (ArchitectureWithPseudo arch)
                   => Sym t
                   -> SynthInstruction arch
                   -> Syn t arch (F.Formula (Sym t) arch)
instructionFormula sym i = do
  case i of
    SynthInstruction op operands -> do
      Just pf <- lookupFormula op
      (_, f) <- liftIO $ F.instantiateFormula sym pf operands
      return f

-- | Convert a program into a formula
programFormula :: (ArchitectureWithPseudo arch)
               => Sym t
               -> [SynthInstruction arch]
               -> Syn t arch (F.Formula (Sym t) arch)
programFormula sym insns = do
  fs <- mapM (instructionFormula sym) insns
  liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula fs

-- | Use an SMT solver to check if two programs are equivalent.
--
-- If they are not, return an input that demonstrates the difference.
testEquivalence :: (ArchitectureWithPseudo arch)
                => [SynthInstruction arch]
                -> [SynthInstruction arch]
                -> Syn t arch (F.EquivalenceResult arch Value)
testEquivalence p representative = do
  withSymBackend $ \sym -> do
    pf <- programFormula sym p
    repFormula <- programFormula sym representative
    liftIO $ F.formulasEquivConcrete sym pf repFormula

