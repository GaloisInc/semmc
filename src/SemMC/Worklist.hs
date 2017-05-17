-- | The worklist managing the list of work items to process
module SemMC.Worklist (
  Worklist,
  newWorklist,
  takeWork,
  putWork
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Sequence as Seq

newtype Worklist a =
  Worklist { unWorklist :: STM.TMVar (WorkState a) }

data WorkState a =
  WorkState { wsQueue :: Seq.Seq a
            }

newWorklist :: [a] -> IO (Worklist a)
newWorklist work = do
  let ws0 = WorkState { wsQueue = Seq.fromList work }
  Worklist <$> STM.newTMVarIO ws0

-- | Take an item off of the front of the worklist
takeWork :: Worklist a -> IO (Maybe a)
takeWork wl = do
  STM.atomically $ do
    ws0 <- STM.takeTMVar (unWorklist wl)
    case Seq.viewl (wsQueue ws0) of
      Seq.EmptyL -> do
        STM.putTMVar (unWorklist wl) ws0
        return Nothing
      someOp Seq.:< rest -> do
        let ws1 = ws0 { wsQueue = rest }
        STM.putTMVar (unWorklist wl) ws1
        return (Just someOp)

-- | Put an item back onto the end of the worklist
putWork :: Worklist a -> a -> IO ()
putWork wl op = do
  STM.atomically $ do
    ws0 <- STM.takeTMVar (unWorklist wl)
    let ws1 = ws0 { wsQueue = wsQueue ws0 Seq.|> op }
    STM.putTMVar (unWorklist wl) ws1
