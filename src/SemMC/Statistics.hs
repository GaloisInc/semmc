-- | A module defining some helpers to manage run-time statistics
module SemMC.Statistics (
  StatisticsThread,
  createStatisticsThread,
  terminate
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM

data StatisticsThread =
  StatisticsThread { stMsgChan :: STM.TChan StatisticsRecord
                   , stTermChan :: STM.TChan ()
                   , stThread :: C.ThreadId
                   }

createStatisticsThread :: IO StatisticsThread
createStatisticsThread = do
  msgChan <- STM.newTChanIO
  termChan <- STM.newTChanIO
  tid <- C.forkIO (statisticsLoop termChan msgChan)
  return StatisticsThread { stMsgChan = msgChan
                          , stTermChan = termChan
                          , stThread = tid
                          }

statisticsLoop :: STM.TChan () -> STM.TChan StatisticsRecord -> IO ()
statisticsLoop termChan msgChan = do
  msg <- STM.atomically $ STM.readTChan msgChan

  case msg of
    Terminate -> STM.atomically $ STM.writeTChan termChan ()

    _ -> statisticsLoop termChan msgChan

data StatisticsRecord = Terminate
                      | SolverInvocation Int
  deriving (Eq, Ord, Show)

terminate :: StatisticsThread -> IO ()
terminate st = do
  STM.atomically $ STM.writeTChan (stMsgChan st) Terminate
  () <- STM.atomically $ STM.readTChan (stTermChan st)
  return ()
