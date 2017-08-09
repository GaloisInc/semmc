{-# LANGUAGE FlexibleContexts #-}
module SemMC.Synthesis
  ( setupEnvironment
  , mcSynth
  ) where

import           Data.Typeable
import qualified Data.Parameterized.Map as MapF

import qualified Lang.Crucible.Solver.SimpleBackend as S

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Synthesis.Core
import           SemMC.Synthesis.DivideAndConquer
import           SemMC.Synthesis.Template

setupEnvironment :: (Architecture arch,
                     Architecture (TemplatedArch arch),
                     Typeable arch)
                 => S.SimpleBackend t
                 -> BaseSet (S.SimpleBackend t) arch
                 -> IO (SynthesisEnvironment (S.SimpleBackend t) arch)
setupEnvironment sym baseSet = do
  insns <- templatedInstructions sym baseSet
  return $! SynthesisEnvironment { synthSym = sym
                                 , synthBaseSet = baseSet
                                 , synthInsns = insns
                                 }

mcSynth :: (Architecture arch,
            Architecture (TemplatedArch arch),
            Typeable arch)
        => SynthesisEnvironment (S.SimpleBackend t) arch
        -> Formula (S.SimpleBackend t) arch
        -> IO (Maybe [Instruction arch])
mcSynth env target = do
  let params = SynthesisParams { synthEnv = env
                               , synthMaxLength = 0
                               }
  ret1 <- divideAndConquer (params { synthMaxLength = 1 }) target
  case ret1 of
    Just _ -> return ret1
    Nothing -> do
      ret2 <- divideAndConquer (params { synthMaxLength = 2 }) target
      case ret2 of
        Just _ -> return ret2
        Nothing -> synthesizeFormula (params { synthMaxLength = 1000 }) target
