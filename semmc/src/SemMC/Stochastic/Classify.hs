module SemMC.Stochastic.Classify (
  classify,
  chooseClass,
  chooseProgram,
  EquivalenceClasses,
  equivalenceClasses,
  countPrograms
  ) where

import qualified Data.Set as S

import SemMC.Architecture ( Instruction )
import SemMC.Stochastic.Monad

data EquivalenceClasses arch = EquivalenceClasses { unClasses :: S.Set (EquivalenceClass arch) }

data EquivalenceClass arch = EquivalenceClass { unClass :: S.Set [Instruction arch] }

equivalenceClasses :: [Instruction arch] -> EquivalenceClasses arch
equivalenceClasses p = EquivalenceClasses (S.singleton (EquivalenceClass (S.singleton p)))

countPrograms :: EquivalenceClasses arch -> Int
countPrograms s = sum (map (S.size . unClass) (S.toList (unClasses s)))

classify :: [Instruction arch] -> EquivalenceClasses arch -> Syn sym arch (Maybe (EquivalenceClasses arch))
classify = undefined

chooseClass :: EquivalenceClasses arch -> Syn sym arch (EquivalenceClass arch)
chooseClass = undefined

chooseProgram :: EquivalenceClass arch -> Syn sym arch [Instruction arch]
chooseProgram = undefined
