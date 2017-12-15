{-# LANGUAGE FlexibleContexts #-}
module SemMC.Synthesis
  ( setupEnvironment
  , mcSynth
  ) where

import           Data.Typeable

import qualified Lang.Crucible.Solver.SimpleBackend as S

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Synthesis.Core
import           SemMC.Synthesis.DivideAndConquer
import           SemMC.Synthesis.Template

-- The synthesis implemented in this module is based on
--
-- Venkatesh Srinivasan and Thomas Reps. 2015. Synthesis of machine code from
-- semantics. In Proceedings of the 36th ACM SIGPLAN Conference on Programming
-- Language Design and Implementation (PLDI '15). ACM, New York, NY, USA,
-- 596-607. DOI: http://dx.doi.org/10.1145/2737924.2737960
--
-- Further useful optimizations (none of which have been implemented here yet)
-- can be found in
--
-- Venkatesh Srinivasan, Tushar Sharma, and Thomas Reps. 2016. Speeding up
-- machine-code synthesis. In Proceedings of the 2016 ACM SIGPLAN International
-- Conference on Object-Oriented Programming, Systems, Languages, and
-- Applications (OOPSLA 2016). ACM, New York, NY, USA, 165-180. DOI:
-- https://doi.org/10.1145/2983990.2984006

setupEnvironment :: (Architecture arch,
                     Architecture (TemplatedArch arch),
                     TemplatableOperand arch,
                     Typeable arch)
                 => S.SimpleBackend t
                 -> BaseSet (S.SimpleBackend t) arch
                 -> SynthesisEnvironment (S.SimpleBackend t) arch
setupEnvironment sym baseSet =
  let insns = templatedInstructions baseSet
  in SynthesisEnvironment { synthSym = sym
                          , synthBaseSet = baseSet
                          , synthInsns = insns
                          }

mcSynth :: (Architecture arch,
            Architecture (TemplatedArch arch),
            TemplatableOperand arch,
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
