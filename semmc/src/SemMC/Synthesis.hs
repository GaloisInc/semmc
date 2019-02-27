{-# LANGUAGE FlexibleContexts #-}
module SemMC.Synthesis
  ( setupEnvironment
  , SynthesisEnvironment
  , synthSym
  , mcSynth
  , mcSynthTimeout
  , TemplatedArch
  , TemplatedOperand
  , BaseSet
  , unTemplate
  -- * Constraints
  , TemplatableOperand
  ) where

import           Data.Typeable
import           Control.Monad (join)
import           System.Timeout (timeout)
import           Control.Exception.Base (finally)

import qualified What4.Protocol.Online as WPO
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO

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
                     ArchRepr arch,
                     TemplatableOperand arch,
                     Typeable arch)
                 => CBO.OnlineBackend t solver fs
                 -> BaseSet (CBO.OnlineBackend t solver fs) arch
                 -> SynthesisEnvironment (CBO.OnlineBackend t solver fs) arch
setupEnvironment sym baseSet =
  let insns = templatedInstructions baseSet
  in SynthesisEnvironment { synthSym = sym
                          , synthBaseSet = baseSet
                          , synthInsns = insns
                          }

-- | Synthesizes a list of instructions from a formula.
--
-- TODO: Restore divide and conquer passes that are currently commented out
mcSynth :: (TemplateConstraints arch,
            ArchRepr arch,
            WPO.OnlineSolver t solver,
            CB.IsSymInterface (CBO.OnlineBackend t solver fs)
           )
        => SynthesisEnvironment (CBO.OnlineBackend t solver fs) arch
        -> Formula (CBO.OnlineBackend t solver fs) arch
        -> IO (Maybe [Instruction arch])
mcSynth env target = do

  -- Synthesize the target without considering the behavior of the instruction pointer
  let target' = formStripIP target

  putStrLn $ "Calling mcSynth on target " ++ show target'
  let params = SynthesisParams { synthEnv = env
                               , synthMaxLength = 0
                               }
  safeAssumptionFrame (synthSym env) $ do
   --  synthesizeFormula (params {synthMaxLength = 1}) target'
    ret1 <- divideAndConquer (params { synthMaxLength = 1 }) target'
    case ret1 of
      Just _ -> return ret1
      Nothing -> do
      -- Instead of repeating each call, perhaps we can only recurse if the result
      -- ran out of space, which could be tracked in 'synthesizeFormula'
        ret2 <- divideAndConquer (params { synthMaxLength = 2 }) target'
        case ret2 of
          Just _ -> return ret2
          Nothing -> synthesizeFormula (params { synthMaxLength = 1000 }) target'
  where
    -- Isolate the solver environment using pushFrame and popFrame
    safeAssumptionFrame sym op = do
      frame <- CB.pushAssumptionFrame sym
      finally op (CB.popAssumptionFrame sym frame)


-- | Synthesizes a list of instructions from a formula, within a particular
-- timeout limit.
mcSynthTimeout :: (TemplateConstraints arch,
                   ArchRepr arch,
                   WPO.OnlineSolver t solver,
                   CB.IsSymInterface (CBO.OnlineBackend t solver fs)
                  )
               => Int 
               -> SynthesisEnvironment (CBO.OnlineBackend t solver fs) arch
               -> Formula (CBO.OnlineBackend t solver fs) arch
               -> IO (Maybe [Instruction arch])
mcSynthTimeout t env f = join <$> (timeout t $ mcSynth env f)
