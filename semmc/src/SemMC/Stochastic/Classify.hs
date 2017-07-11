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

import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S

import SemMC.Architecture ( Instruction )
import SemMC.Stochastic.Monad

data EquivalenceClasses arch = EquivalenceClasses { unClasses :: S.Set (S.Set [Instruction arch]) }

data EquivalenceClass arch = EquivalenceClass { unClass :: S.Set [Instruction arch] }

equivalenceClasses :: [Instruction arch] -> EquivalenceClasses arch
equivalenceClasses p = EquivalenceClasses (S.singleton (S.singleton p))

countPrograms :: EquivalenceClasses arch -> Int
countPrograms s = sum (map S.size (S.toList (unClasses s)))

classify :: forall arch sym
          . (Ord (Instruction arch))
         => [Instruction arch]
         -> EquivalenceClasses arch
         -> Syn sym arch (Maybe (EquivalenceClasses arch))
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
          let proxy = Proxy :: Proxy arch
          let eqclasses' = S.insert p (mergeClasses proxy classes)
          return (Just (EquivalenceClasses (S.singleton eqclasses')))

classifyByClass :: [Instruction arch]
                -- ^ The program to classify
                -> S.Set (S.Set [Instruction arch])
                -- ^ The set of classes matching the input program
                -> [S.Set [Instruction arch]]
                -- ^ The existing equivalence classes
                -> Syn sym arch (Maybe (S.Set (S.Set [Instruction arch])))
classifyByClass = undefined

chooseClass :: EquivalenceClasses arch -> Syn sym arch (EquivalenceClass arch)
chooseClass = undefined

chooseProgram :: EquivalenceClass arch -> Syn sym arch [Instruction arch]
chooseProgram = undefined

-- | Flatten a set of equivalence classes into one equivalence class
--
-- Note that the proxy argument is annoying, but required to control instance
-- selection (because 'Instruction' is a type function, and type functions are
-- not injective).  It would be really nice if that wasn't required.
mergeClasses :: (Ord (Instruction arch))
             => proxy arch
             -> S.Set (S.Set [Instruction arch])
             -> S.Set [Instruction arch]
mergeClasses _ s = S.unions (S.toList s)
