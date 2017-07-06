{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Synthesis.Core
  ( synthesizeFormula
  , SynthesisParams(..)
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Sequence as Seq
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

data SynthesisParams sym arch =
  SynthesisParams { synthSym :: sym
                  , synthBaseSet :: MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula sym (TemplatedArch arch))
                  , synthMaxLength :: Int
                  }

data SynthesisState sym arch =
  SynthesisState { synthTests :: [(ArchState sym arch, ArchState sym arch)]
                 , synthPrefixes :: Seq.Seq [TemplatedInstructionFormula sym arch]
                 }

footprintFilter :: (Architecture arch)
                => Formula (S.SimpleBuilder t st) arch
                -> Formula (S.SimpleBuilder t st) arch
                -> Bool
footprintFilter target candidate =
     formUses candidate `Set.isSubsetOf` formUses target
  && Set.fromList (mapFKeys (formDefs candidate)) `Set.isSubsetOf` Set.fromList (mapFKeys (formDefs target))

instantiate :: (MonadReader (SynthesisParams (S.SimpleBackend t) arch) m,
                MonadState (SynthesisState (S.SimpleBackend t) arch) m,
                MonadIO m,
                Architecture arch,
                Architecture (TemplatedArch arch))
            => Formula (S.SimpleBackend t) arch
            -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
            -> m (Maybe [Instruction arch])
instantiate target trial = do
  SynthesisParams { synthSym = sym
                  , synthBaseSet = baseSet
                  } <- ask
  trialFormula <- liftIO $ condenseFormula sym trial
  -- TODO: footprintFilter can probably be run faster before condensing the formula
  case footprintFilter target trialFormula of
    True -> do
      st <- get
      liftIO (cegis sym baseSet target (synthTests st) trial trialFormula)
        >>= \case
               Right insns -> return (Just insns)
               Left newTests -> do
                let oldPrefixes = synthPrefixes st
                put (st { synthTests = newTests
                        , synthPrefixes = oldPrefixes Seq.|> trial
                        })
                return Nothing
    False -> return Nothing

synthesizeFormula' :: (MonadReader (SynthesisParams (S.SimpleBackend t) arch) m,
                       MonadState (SynthesisState (S.SimpleBackend t) arch) m,
                       MonadIO m,
                       Architecture arch,
                       Architecture (TemplatedArch arch))
                   => Formula (S.SimpleBackend t) arch
                   -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
                   -> m (Maybe [Instruction arch])
synthesizeFormula' target possibleInsns = do
  -- I'm still conflicted whether to use MonadState here or not...
  st <- get
  case Seq.viewl (synthPrefixes st) of
    prefix Seq.:< prefixesTail -> do
      maxLen <- reader synthMaxLength
      if 1 + length prefix > maxLen then return Nothing else do
      put $ st { synthPrefixes = prefixesTail }
      -- N.B.: 'instantiate' here will add back to the prefixes if it's worth
      -- saving. I'm not a big fan of MonadState here, but it was the nicest
      -- solution I could come up with.
      result <- sequenceMaybes $
        map (\insn -> instantiate target (prefix ++ [insn])) possibleInsns
      case result of
        Just insns -> return (Just insns)
        Nothing -> synthesizeFormula' target possibleInsns
    Seq.EmptyL -> return Nothing

synthesizeFormula :: forall t arch.
                     (Architecture arch,
                      Architecture (TemplatedArch arch),
                      Typeable arch)
                  => SynthesisParams (S.SimpleBackend t) arch
                  -> Formula (S.SimpleBackend t) arch
                  -> IO (Maybe [Instruction arch])
synthesizeFormula params target = do
  insns <- templatedInstructions (synthSym params) (synthBaseSet params)
  flip runReaderT params
    $ evalStateT (synthesizeFormula' target insns)
    $ SynthesisState { synthTests = []
                     , synthPrefixes = Seq.singleton []
                     }
