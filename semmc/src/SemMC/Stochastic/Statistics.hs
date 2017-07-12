{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- | A module defining some helpers to manage run-time statistics
--
-- It is structured as its own thread so that access to the on-disk stats files
-- is synchronized.
module SemMC.Stochastic.Statistics (
  StatisticsThread,
  newStatisticsThread,
  SolverTime(..),
  recordSolverInvocation,
  terminateStatisticsThread
  ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as M
import Data.Word ( Word64 )

import Data.Parameterized.Classes ( OrdF )
import Data.Parameterized.Some ( Some(..) )

import SemMC.Architecture ( Opcode, Operand )

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
             }

emptyStats :: Statistics arch
emptyStats = Statistics { sSolverInvocations = 0
                        , sSolverTime = 0
                        , sSolverTimeouts = 0
                        , sOpcodeTime = M.empty
                        , sOpcodeTimeouts = M.empty
                        }

data StatisticsRecord arch = Terminate
                           -- ^ Tell the thread to terminate
                           | SolverInvocation (Some (Opcode arch (Operand arch))) SolverTime
                           -- ^ An invocation while trying to learn the given opcode
                           -- that ran for the given number of milliseconds

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
data SolverTime = Timeout Word64
                | Completed Word64

asMilliseconds :: SolverTime -> Word64
asMilliseconds st =
  case st of
    Timeout w -> w
    Completed w -> w

-- | Record a single invocation of the solver (on behalf of some instruction)
-- and the time taken for the invocation
recordSolverInvocation :: StatisticsThread arch -> Some (Opcode arch (Operand arch)) -> SolverTime -> IO ()
recordSolverInvocation st op millis = STM.atomically $ STM.writeTChan (stMsgs st) msg
  where
    msg = SolverInvocation op millis

writeStatsFile :: FilePath -> Statistics arch -> IO ()
writeStatsFile = undefined
