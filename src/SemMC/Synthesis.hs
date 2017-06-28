{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
module SemMC.Synthesis
  ( synthesizeFormula
  ) where

import           Control.Monad.State
import qualified Data.Dequeue as Dequeue
import           Data.Foldable
import qualified Data.Parameterized.Map as MapF
import qualified Data.Set as Set
import           Data.Typeable

import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.SimpleBackend as S

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Formula.Instantiate
import           SemMC.Synthesis.Template
import           SemMC.Synthesis.Cegis
import           SemMC.Util

-- NOTE: This initial implementation is not meant to be at all fast; it is just
-- a proof of concept.

condenseFormula :: forall t st arch.
                   (Architecture arch,
                    Architecture (TemplatedArch arch))
                => S.SimpleBuilder t st
                -> [TemplatedInstructionFormula (S.SimpleBuilder t st) arch]
                -> IO (Formula (S.SimpleBuilder t st) arch)
condenseFormula sym = fmap (coerceFormula :: Formula sym (TemplatedArch arch) -> Formula sym arch)
                    . foldrM (sequenceFormulas sym) emptyFormula
                    . map (\(TemplatedInstructionFormula _ tf) -> tfFormula tf)

-- Algorithm Synthesize, from [Srinivasan and Reps 2015]:

data SynthesisState sym arch =
  SynthesisState { synthTests :: [(ArchState sym arch, ArchState sym arch)]
                 , synthPrefixes :: Dequeue.BankersDequeue [TemplatedInstructionFormula sym arch]
                 }

footprintFilter :: (Architecture arch)
                => Formula (S.SimpleBuilder t st) arch
                -> Formula (S.SimpleBuilder t st) arch
                -> Bool
footprintFilter target candidate =
     formUses candidate `Set.isSubsetOf` formUses target
  && Set.fromList (mapFKeys (formDefs candidate)) `Set.isSubsetOf` Set.fromList (mapFKeys (formDefs target))

instantiate :: (MonadState (SynthesisState (S.SimpleBackend t) arch) m,
                MonadIO m,
                Architecture arch,
                Architecture (TemplatedArch arch))
            => S.SimpleBackend t
            -> MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula (S.SimpleBackend t) (TemplatedArch arch))
            -> Formula (S.SimpleBackend t) arch
            -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
            -> m (Maybe [Instruction arch])
instantiate sym m target trial = do
  trialFormula <- liftIO $ condenseFormula sym trial
  -- TODO: footprintFilter can probably be run faster before condensing the formula
  case footprintFilter target trialFormula of
    True -> do
      st <- get
      liftIO (cegis sym m target (synthTests st) trial trialFormula)
        >>= \case
               Right insns -> return (Just insns)
               Left newTests -> do
                let oldPrefixes = synthPrefixes st
                put (st { synthTests = newTests
                        , synthPrefixes = Dequeue.pushBack oldPrefixes trial
                        })
                return Nothing
    False -> return Nothing

-- This short-circuits on long (or infinite) lists.
sequenceMaybes :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
sequenceMaybes [] = return Nothing
sequenceMaybes (x : xs) = x >>= maybe (sequenceMaybes xs) (return . Just)

synthesizeFormula' :: (MonadState (SynthesisState (S.SimpleBackend t) arch) m,
                       MonadIO m,
                       Architecture arch,
                       Architecture (TemplatedArch arch))
                   => S.SimpleBackend t
                   -> MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula (S.SimpleBackend t) (TemplatedArch arch))
                   -> Formula (S.SimpleBackend t) arch
                   -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
                   -> m (Maybe [Instruction arch])
synthesizeFormula' sym m target possibleInsns = do
  -- I'm still conflicted whether to use MonadState here or not...
  st <- get
  let front = Dequeue.popFront (synthPrefixes st)
  case front of
    Just (prefix, newPrefixes) -> do
      put $ st { synthPrefixes = newPrefixes }
      -- N.B.: 'instantiate' here will add back to the prefixes if it's worth
      -- saving. I'm not a big fan of MonadState here, but it was the nicest
      -- solution I could come up with.
      result <- sequenceMaybes $
        map (\insn -> instantiate sym m target (prefix ++ [insn])) possibleInsns
      case result of
        Just insns -> return (Just insns)
        Nothing -> synthesizeFormula' sym m target possibleInsns
    Nothing -> return Nothing

synthesizeFormula :: forall t arch.
                     (Architecture arch,
                      Architecture (TemplatedArch arch),
                      Typeable arch
                      )
                  => S.SimpleBackend t
                  -> MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula (S.SimpleBackend t) (TemplatedArch arch))
                  -> Formula (S.SimpleBackend t) arch
                  -> [(ArchState (S.SimpleBackend t) arch, ArchState (S.SimpleBackend t) arch)]
                  -> IO (Maybe [Instruction arch])
synthesizeFormula sym m target tests = do
  insns <- templatedInstructions sym m
  evalStateT (synthesizeFormula' sym m target insns)
    $ SynthesisState { synthTests = tests
                     , synthPrefixes = Dequeue.fromList [[]]
                     }
