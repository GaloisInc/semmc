module SemMC.Strata (
  Config(..),
  strata
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import Control.Monad ( replicateM )
import Data.Monoid

import Prelude

import qualified Dismantle.Instruction as D

import SemMC.Backend ( Backend(..) )
import qualified SemMC.Statistics as Stats

-- Have this process the worklist in a single-threaded way, but for each work
-- item spawn off an Async thread to process it.  Wait once there are enough
-- outstanding threads.

data Config = Config { numThreads :: Int
                     , baseSetDir :: FilePath
                     , learnedSetDir :: FilePath
                     }

strata :: Config -> Backend opcode operand -> IO [(instr, semantics)]
strata cfg backend = do
  stats <- Stats.createStatisticsThread
  baseSet <- loadFormulas (baseSetDir cfg)
  learnedSet <- loadFormulas (learnedSetDir cfg)
  let initialFormulas = baseSet <> learnedSet
  worklist <- buildWorklist (allOpcodes backend) initialFormulas
  threads <- replicateM (numThreads cfg) (C.forkIO (processWorklist stats backend worklist))
  Stats.logStatistics stats Stats.Terminate
  return undefined

processWorklist :: Stats.StatisticsThread
                -> Backend opcode operand
                -> STM.TChan (D.SomeOpcode opcode operand)
                -> IO ()
processWorklist = undefined

data FormulaSet = FormulaSet

instance Monoid FormulaSet where
  mempty = FormulaSet
  mappend = undefined

loadFormulas :: FilePath -> IO FormulaSet
loadFormulas = undefined

buildWorklist :: [D.SomeOpcode opcode operand] -> FormulaSet -> IO (STM.TChan (D.SomeOpcode opcode operand))
buildWorklist allOps knownFormulas = do
  worklist <- STM.newTChanIO
  let workitems :: [D.SomeOpcode opcode operand]
      workitems = undefined allOps knownFormulas
  STM.atomically $ mapM_ (STM.writeTChan worklist) workitems
  return worklist
