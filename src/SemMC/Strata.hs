module SemMC.Strata (
  Config(..),
  strata
  ) where

import qualified Control.Concurrent as C
import Control.Monad ( replicateM )
import Data.Monoid
import qualified Data.Set as S

import Prelude

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Backend ( Backend(..) )
import qualified SemMC.Statistics as Stats
import qualified SemMC.Worklist as WL

-- Have this process the worklist in a single-threaded way, but for each work
-- item spawn off an Async thread to process it.  Wait once there are enough
-- outstanding threads.

data Config = Config { numThreads :: Int
                     , baseSetDir :: FilePath
                     , learnedSetDir :: FilePath
                     }

strata :: (D.ArbitraryOperands opcode operand) => Config -> Backend opcode operand -> IO [(instr, semantics)]
strata cfg backend = do
  stats <- Stats.createStatisticsThread
  baseSet <- loadFormulas (baseSetDir cfg)
  learnedSet <- loadFormulas (learnedSetDir cfg)
  let initialFormulas = baseSet <> learnedSet
  worklist <- buildWorklist (allOpcodes backend) initialFormulas
  threads <- replicateM (numThreads cfg) (C.forkIO (processWorklist stats backend worklist))
  Stats.terminate stats
  return undefined

processWorklist :: (D.ArbitraryOperands opcode operand)
                => Stats.StatisticsThread
                -> Backend opcode operand
                -> WL.Worklist (D.SomeOpcode opcode operand)
                -> IO ()
processWorklist _stats _backend wl = do
  mwork <- WL.takeWork wl
  case mwork of
    Nothing -> return ()
    Just someOp -> do
      -- FIXME: Put this into a state
      gen <- A.createGen
      Just target <- D.randomInstruction gen (S.singleton someOp)
      return ()

data FormulaSet = FormulaSet

instance Monoid FormulaSet where
  mempty = FormulaSet
  mappend = undefined

loadFormulas :: FilePath -> IO FormulaSet
loadFormulas = undefined

buildWorklist :: [D.SomeOpcode opcode operand] -> FormulaSet -> IO (WL.Worklist (D.SomeOpcode opcode operand))
buildWorklist allOps knownFormulas = do
  let workitems :: [D.SomeOpcode opcode operand]
      workitems = undefined allOps knownFormulas
  WL.newWorklist workitems

