{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
module SemMC.Synthesis
  ( synthesizeFormula
  ) where

import           Control.Monad.State
import           Data.Foldable
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           Data.Typeable

import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.SimpleBackend as S

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Formula.Instantiate
import           SemMC.Synthesis.Template
import           SemMC.Synthesis.Cegis

-- NOTE: This initial implementation is not meant to be at all fast; it is just
-- a proof of concept.

condenseFormula :: forall t st arch.
                   (Architecture arch,
                    Architecture (TemplatedArch arch))
                => S.SimpleBuilder t st
                -> [InstructionWTFormula (S.SimpleBuilder t st) arch]
                -> IO (Formula (S.SimpleBuilder t st) arch)
condenseFormula sym = fmap (coerceFormula :: Formula sym (TemplatedArch arch) -> Formula sym arch)
                    . foldrM (sequenceFormulas sym) emptyFormula
                    . map (\(InstructionWTFormula _ tf) -> tfFormula tf)

mapFKeys :: MapF.MapF k v -> [Some k]
mapFKeys = MapF.foldrWithKey (\k _ l -> Some k : l) []

footprintFilter :: (Architecture arch)
                => Formula (S.SimpleBuilder t st) arch
                -> Formula (S.SimpleBuilder t st) arch
                -> Bool
footprintFilter target candidate =
     formUses candidate `Set.isSubsetOf` formUses target
  && Set.fromList (mapFKeys (formDefs candidate)) `Set.isSubsetOf` Set.fromList (mapFKeys (formDefs target))

data SynthesisState sym arch =
  SynthesisState { synthTests :: [(ArchState sym arch, ArchState sym arch)]
                 , synthUselessPrefixes :: [[InstructionWTFormula sym arch]]
                 }

instantiate :: (MonadState (SynthesisState (S.SimpleBackend t) arch) m,
                MonadIO m,
                Architecture arch,
                Architecture (TemplatedArch arch),
                ShowF ((Opcode arch) (Operand arch)),
                ShowF (Operand arch))
            => S.SimpleBackend t
            -> Formula (S.SimpleBackend t) arch
            -> [InstructionWTFormula (S.SimpleBackend t) arch]
            -> m (Maybe [Instruction arch])
            -- -> m (Maybe [InstructionWTFormula (S.SimpleBackend t) arch])
instantiate sym target trial = do
  trialFormula <- liftIO $ condenseFormula sym trial
  -- liftIO $ print trialFormula
  case footprintFilter target trialFormula of
  -- case True of
    True -> do
      -- liftIO $ print trial
      tests <- synthTests <$> get
      liftIO $ cegis sym target tests trial trialFormula
    False -> return Nothing

-- This works correctly on infinite lists.
sequenceMaybes :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
sequenceMaybes [] = return Nothing
sequenceMaybes (x : xs) = x >>= maybe (sequenceMaybes xs) (return . Just)

synthesizeFormula :: forall t st arch.
                     (Architecture arch,
                      -- OrdF (Opcode arch (Operand (TemplatedArch arch))),
                      -- SF.ShowF (Opcode arch (Operand (TemplatedArch arch))),
                      -- EnumF (Opcode arch (Operand (TemplatedArch arch))),
                      Architecture (TemplatedArch arch),
                      Typeable arch,
                      ShowF ((Opcode arch) (Operand arch)),
                      ShowF (Operand arch)
                      )
                  => S.SimpleBackend t
                  -> MapF.MapF (OpcodeGoodShape (Opcode arch) (Operand arch) arch) (ParameterizedFormula (S.SimpleBackend t) (TemplatedArch arch))
                  -> Formula (S.SimpleBackend t) arch
                  -> [(ArchState (S.SimpleBackend t) arch, ArchState (S.SimpleBackend t) arch)]
                  -> IO (Maybe [Instruction arch])
                  -- -> IO (Maybe [InstructionWTFormula (S.SimpleBackend t) arch])
synthesizeFormula sym m target tests = do
  insns <- templatedInstructions sym m
  -- 'insns' is an infinite list, so we have to be careful with what we do with it.
  evalStateT (sequenceMaybes $ map (instantiate sym target) insns)
    $ SynthesisState { synthTests = tests
                     , synthUselessPrefixes = []
                     }
