-- | A module defining some helpers to manage run-time statistics
module SemMC.Statistics (
  StatisticsThread,
  createStatisticsThread,
  StatisticsRecord(..),
  logStatistics
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM

data StatisticsThread =
  StatisticsThread { stChan :: STM.TChan StatisticsRecord
                   , stThread :: C.ThreadId
                   }

createStatisticsThread :: IO StatisticsThread
createStatisticsThread = do
  chan <- STM.newTChanIO
  tid <- C.forkIO (statisticsLoop chan)
  return StatisticsThread { stChan = chan
                          , stThread = tid
                          }

statisticsLoop :: STM.TChan StatisticsRecord -> IO ()
statisticsLoop chan = do
  msg <- STM.atomically $ STM.readTChan chan

  case msg of
    Terminate -> return ()
    _ -> statisticsLoop chan

data StatisticsRecord = Terminate
                      | SolverInvocation Int
  deriving (Eq, Ord, Show)

logStatistics :: StatisticsThread -> StatisticsRecord -> IO ()
logStatistics t r = STM.atomically (STM.writeTChan (stChan t) r)
