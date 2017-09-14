{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- | A module defining some helpers to manage run-time statistics
--
-- It is structured as its own thread so that access to the on-disk stats files
-- is synchronized.
module SemMC.Stochastic.Statistics (
  -- * Statistics thread management
  StatisticsThread,
  newStatisticsThread,
  terminateStatisticsThread,
  -- * Gatherable statistics
  SolverTime(..),
  recordSolverInvocation,
  recordStrataTimeout,
  recordStrataSuccess,
  recordSynthesizeSuccess,
  recordRemovedCandidatePrograms,
  recordCounterexample
  ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as M
import qualified Data.Time.Clock as TM
import           Data.Word ( Word64 )

import           Data.Parameterized.Classes ( OrdF )
import           Data.Parameterized.Some ( Some(..) )

import           SemMC.Architecture ( Opcode, Operand )

data Statistics arch =
  Statistics { sSolverInvocations :: Word64
             -- ^ The overall number of solver invocations
             , sSolverTime :: Word64
             -- ^ The cumulative number of milliseconds of solver time
             , sSolverTimeouts :: Word64
             -- ^ The number of timeouts
             , sOpcodeTime :: M.Map (Some (Opcode arch (Operand arch))) Word64
             -- ^ The time required to learn the semantics for each operand
             , sOpcodeTimeouts :: M.Map (Some (Opcode arch (Operand arch))) Word64
             -- ^ The number of timeouts for each opcode
             , sConcreteExecutions :: Word64
             -- ^ The number of concrete program executions performed (of target
             -- programs and candidate programs)
             }

emptyStats :: Statistics arch
emptyStats = Statistics { sSolverInvocations = 0
                        , sSolverTime = 0
                        , sSolverTimeouts = 0
                        , sOpcodeTime = M.empty
                        , sOpcodeTimeouts = M.empty
                        , sConcreteExecutions = 0
                        }

data StatisticsRecord arch = Terminate
                           -- ^ Tell the thread to terminate
                           | SolverInvocation (Some (Opcode arch (Operand arch))) SolverTime
                           -- ^ An invocation while trying to learn the given opcode
                           -- that ran for the given number of milliseconds
                           | CounterexampleFound (Some (Opcode arch (Operand arch)))
                           | StrataTimeout (Some (Opcode arch (Operand arch)))
                           | StrataSuccess (Some (Opcode arch (Operand arch))) TM.NominalDiffTime
                           | SynthesizeSuccess (Some (Opcode arch (Operand arch))) TM.NominalDiffTime
                           | RemovedCandidatePrograms (Some (Opcode arch (Operand arch))) Int

data StatisticsThread arch =
  StatisticsThread { stMsgs :: STM.TChan (StatisticsRecord arch)
                   , stTerm :: STM.TChan ()
                   , stStats :: STM.TVar (Statistics arch)
                   , stFilePath :: FilePath
                   }

-- | Start a new statistics thread
--
-- The 'FilePath' describes the location on disk to persist statistics after
-- updates.
newStatisticsThread :: (OrdF (Opcode arch (Operand arch)))
                    => FilePath
                    -> IO (StatisticsThread arch)
newStatisticsThread statsFile = do
  chan <- STM.newTChanIO
  term <- STM.newTChanIO
  stats <- STM.newTVarIO emptyStats
  -- FIXME: Load up an initial stats file
  let st = StatisticsThread { stMsgs = chan
                            , stTerm = term
                            , stStats = stats
                            , stFilePath = statsFile
                            }
  _a <- A.async (loop st)
  return st

loop :: (OrdF (Opcode arch (Operand arch))) => StatisticsThread arch -> IO ()
loop st = do
  msg <- STM.atomically $ STM.readTChan (stMsgs st)
  case msg of
    Terminate -> STM.atomically $ STM.writeTChan (stTerm st) ()
    SolverInvocation op tag@(asMilliseconds -> millis) -> do
      stats <- STM.atomically $ do
        stats <- STM.readTVar (stStats st)
        let stats' = stats { sSolverInvocations = sSolverInvocations stats + 1
                           , sSolverTime = sSolverTime stats + millis
                           , sOpcodeTime = M.insertWith (+) op millis (sOpcodeTime stats)
                           }
            stats'' = case tag of
                        Completed {} -> stats'
                        Timeout _ -> stats' { sSolverTimeouts = sSolverTimeouts stats' + 1
                                            , sOpcodeTimeouts = M.insertWith (+) op 1 (sOpcodeTimeouts stats')
                                            }
        STM.writeTVar (stStats st) stats''
        return stats''
      writeStatsFile (stFilePath st) stats
      loop st

-- | Send a message to terminate the statistics thread and wait for a response
terminateStatisticsThread :: StatisticsThread arch -> IO ()
terminateStatisticsThread st = do
  STM.atomically $ STM.writeTChan (stMsgs st) Terminate
  () <- STM.atomically $ STM.readTChan (stTerm st)
  return ()

-- | A record of the time taken for a solver invocation, with the payload being
-- the number of milliseconds
data SolverTime = Timeout TM.NominalDiffTime
                | Completed TM.NominalDiffTime

asMilliseconds :: SolverTime -> Word64
asMilliseconds st =
  case st of
    Timeout w -> round (w * 1000)
    Completed w -> round (w * 1000)

-- | Record a single invocation of the solver (on behalf of some instruction)
-- and the time taken for the invocation
recordSolverInvocation :: Some (Opcode arch (Operand arch)) -> SolverTime -> StatisticsThread arch -> IO ()
recordSolverInvocation op millis st = STM.atomically $ STM.writeTChan (stMsgs st) msg
  where
    msg = SolverInvocation op millis

recordCounterexample :: Some (Opcode arch (Operand arch)) -> StatisticsThread arch -> IO ()
recordCounterexample op st =
  STM.atomically $ STM.writeTChan (stMsgs st) (CounterexampleFound op)

recordStrataTimeout :: Some (Opcode arch (Operand arch)) -> StatisticsThread arch -> IO ()
recordStrataTimeout op st =
  STM.atomically $ STM.writeTChan (stMsgs st) (StrataTimeout op)

recordStrataSuccess :: Some (Opcode arch (Operand arch)) -> TM.NominalDiffTime -> StatisticsThread arch -> IO ()
recordStrataSuccess op diff st =
  STM.atomically $ STM.writeTChan (stMsgs st) (StrataSuccess op diff)

recordSynthesizeSuccess :: Some (Opcode arch (Operand arch)) -> TM.NominalDiffTime -> StatisticsThread arch -> IO ()
recordSynthesizeSuccess op diff st =
  STM.atomically $ STM.writeTChan (stMsgs st) (SynthesizeSuccess op diff)

recordRemovedCandidatePrograms :: Some (Opcode arch (Operand arch)) -> Int -> StatisticsThread arch -> IO ()
recordRemovedCandidatePrograms op nRemoved st =
  STM.atomically $ STM.writeTChan (stMsgs st) (RemovedCandidatePrograms op nRemoved)

writeStatsFile :: FilePath -> Statistics arch -> IO ()
writeStatsFile = undefined
