{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module SemMC.Synthesis.Core
  ( synthesizeFormula
  , SynthesisEnvironment(..)
  , SynthesisParams(..)
  ) where

import           Control.Arrow ( (&&&) )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Parameterized.Some ( Some, viewSome )
import qualified Data.Sequence as Seq
import           Data.Foldable
import qualified Data.Set as Set
import           Data.Typeable

import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.SimpleBackend as S

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Synthesis.Template
import           SemMC.Synthesis.Cegis
import           SemMC.Util

-- NOTE: This initial implementation is not meant to be at all fast; it is just
-- a proof of concept.


-- Algorithm Synthesize, from [Srinivasan and Reps 2015]:

data SynthesisEnvironment sym arch =
  SynthesisEnvironment { synthSym :: sym
                       , synthBaseSet :: BaseSet sym arch
                       , synthInsns :: [Some (TemplatedInstruction sym arch)]
                       }

data SynthesisParams sym arch =
  SynthesisParams { synthEnv :: SynthesisEnvironment sym arch
                  , synthMaxLength :: Int
                  }

data SynthesisState sym arch =
  SynthesisState { synthTests :: [(ArchState arch (S.SymExpr sym), ArchState arch (S.SymExpr sym))]
                 , synthPrefixes :: Seq.Seq [Some (TemplatedInstruction sym arch)]
                 }

askSym :: (MonadReader (SynthesisParams sym arch) m) => m sym
askSym = reader (synthSym . synthEnv)

askBaseSet :: (MonadReader (SynthesisParams sym arch) m) => m (BaseSet sym arch)
askBaseSet = reader (synthBaseSet . synthEnv)

askInsns :: (MonadReader (SynthesisParams sym arch) m) => m [Some (TemplatedInstruction sym arch)]
askInsns = reader (synthInsns . synthEnv)

askMaxLength :: (MonadReader (SynthesisParams sym arch) m) => m Int
askMaxLength = reader synthMaxLength

calcFootprint :: (Architecture arch)
              => [Some (TemplatedInstruction sym arch)]
              -> (Set.Set (Some (Location arch)), Set.Set (Some (Location arch)))
calcFootprint = foldl' addPrint (Set.empty, Set.empty)
  where addPrint (curInputs, curOutputs) insn =
          let (newInputs, newOutputs) = viewSome (templatedInputs &&& templatedOutputs) insn
          in (curInputs `Set.union` (newInputs Set.\\ curOutputs),
              curOutputs `Set.union` newOutputs)

footprintFilter :: (Architecture arch)
                => Formula sym arch
                -> [Some (TemplatedInstruction sym arch)]
                -> Bool
footprintFilter target candidate =
  let (candInputs, candOutputs) = calcFootprint candidate
      targetInputs = formInputs target
      targetOutputs = formOutputs target
  in candInputs `Set.isSubsetOf` targetInputs &&
     candOutputs `Set.isSubsetOf` targetOutputs

instantiate :: (MonadReader (SynthesisParams (S.SimpleBackend t) arch) m,
                MonadState (SynthesisState (S.SimpleBackend t) arch) m,
                MonadIO m,
                TemplateConstraints arch)
            => Formula (S.SimpleBackend t) arch
            -> [Some (TemplatedInstruction (S.SimpleBackend t) arch)]
            -> m (Maybe [Instruction arch])
instantiate target trial
  | footprintFilter target trial = do
      sym <- askSym
      baseSet <- askBaseSet
      tifs <- liftIO $ traverse (viewSome (genTemplatedFormula sym)) trial
      st <- get
      liftIO (cegis sym baseSet target (synthTests st) tifs)
        >>= \case
               Right insns -> return (Just insns)
               Left newTests -> do
                let oldPrefixes = synthPrefixes st
                put (st { synthTests = newTests
                        , synthPrefixes = oldPrefixes Seq.|> trial
                        })
                return Nothing
  | otherwise = return Nothing

synthesizeFormula' :: (MonadReader (SynthesisParams (S.SimpleBackend t) arch) m,
                       MonadState (SynthesisState (S.SimpleBackend t) arch) m,
                       MonadIO m,
                       Architecture arch,
                       Architecture (TemplatedArch arch))
                   => Formula (S.SimpleBackend t) arch
                   -> m (Maybe [Instruction arch])
synthesizeFormula' target = do
  -- I'm still conflicted whether to use MonadState here or not...
  st <- get
  case Seq.viewl (synthPrefixes st) of
    prefix Seq.:< prefixesTail -> do
      maxLen <- askMaxLength
      if 1 + length prefix > maxLen then return Nothing else do
      put $ st { synthPrefixes = prefixesTail }
      possibleInsns <- askInsns
      -- N.B.: 'instantiate' here will add back to the prefixes if it's worth
      -- saving. I'm not a big fan of MonadState here, but it was the nicest
      -- solution I could come up with.
      result <- sequenceMaybes $
        map (\insn -> instantiate target (prefix ++ [insn])) possibleInsns
      case result of
        Just insns -> return (Just insns)
        Nothing -> synthesizeFormula' target
    Seq.EmptyL -> return Nothing

synthesizeFormula :: forall t arch.
                     (Architecture arch,
                      Architecture (TemplatedArch arch),
                      Typeable arch)
                  => SynthesisParams (S.SimpleBackend t) arch
                  -> Formula (S.SimpleBackend t) arch
                  -> IO (Maybe [Instruction arch])
synthesizeFormula params target = do
  flip runReaderT params
    $ evalStateT (synthesizeFormula' target)
    $ SynthesisState { synthTests = []
                     , synthPrefixes = Seq.singleton []
                     }
