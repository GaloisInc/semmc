{-# LANGUAGE FlexibleContexts #-}
module SemMC.Synthesis
  ( mcSynth
  ) where

import           Data.Typeable
import qualified Data.Parameterized.Map as MapF

import qualified Lang.Crucible.Solver.SimpleBackend as S

import           SemMC.Architecture
import           SemMC.Formula.Formula
import           SemMC.Synthesis.Core
import           SemMC.Synthesis.DivideAndConquer
import           SemMC.Synthesis.Template

mcSynth :: (Architecture arch,
            Architecture (TemplatedArch arch),
            Typeable arch)
        => S.SimpleBackend t
        -> MapF.MapF (TemplatableOpcode arch) (ParameterizedFormula (S.SimpleBackend t) (TemplatedArch arch))
        -> Formula (S.SimpleBackend t) arch
        -> IO (Maybe [Instruction arch])
mcSynth sym baseSet target = do
  let params = SynthesisParams { synthSym = sym
                               , synthBaseSet = baseSet
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
