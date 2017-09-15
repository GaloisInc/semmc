{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import           Control.Monad ( forever )
import qualified Control.Monad.Except as ME
import           Control.Monad.Trans ( liftIO )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Time.Clock as TM
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL

import           Data.Parameterized.Classes ( OrdF, ShowF(..) )
import           Data.Parameterized.Some ( Some(..) )

import           SemMC.Architecture ( Opcode, Operand )
import Debug.Trace
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
                   , stConn :: SQL.Connection
                   }

initializeSchema :: SQL.Connection -> IO ()
initializeSchema conn = mapM_ (SQL.execute_ conn) schema

-- | Look up the ID for an opcode (adding it to the database if required)
opcodeId :: (ShowF (Opcode arch (Operand arch)))
         => proxy arch
         -> SQL.Connection
         -> Some (Opcode arch (Operand arch))
         -> IO OpcodeId
opcodeId _ conn (Some opc) = do
  res <- SQL.query conn "SELECT opid FROM opcodes WHERE name = ?" (SQL.Only (showF opc))
  case res of
    [SQL.Only r] -> return r
    [] -> do
      SQL.execute conn "INSERT INTO opcodes VALUES (?);" (SQL.Only (showF opc))
      (OpcodeId . fromIntegral) <$> SQL.lastInsertRowId conn
    _ -> error ("Impossible, multiple entries in the statistics database for opcode " ++ showF opc)

-- | Start a new statistics thread
--
-- The 'FilePath' describes the location on disk to persist statistics after
-- updates.
newStatisticsThread :: (OrdF (Opcode arch (Operand arch)), ShowF (Opcode arch (Operand arch)))
                    => FilePath
                    -> IO (StatisticsThread arch)
newStatisticsThread statsFile = do
  chan <- STM.newTChanIO
  term <- STM.newTChanIO
  conn <- SQL.open statsFile
  initializeSchema conn
  traceM "Schema initialized"
  let st = StatisticsThread { stMsgs = chan
                            , stTerm = term
                            , stConn = conn
                            }
  _a <- C.forkIO (loop (processStatistic st) `E.finally` (putStrLn "Cleaning up stats thread" >> SQL.close conn))
  return st

processStatistic :: forall arch
                  . (OrdF (Opcode arch (Operand arch)), ShowF (Opcode arch (Operand arch)))
                 => StatisticsThread arch
                 -> Break () IO ()
processStatistic st = do
  liftIO $ putStrLn "processing a statistic"
  msg <- liftIO $ STM.atomically $ STM.readTChan (stMsgs st)
  return ()
  {-
  case msg of
    Terminate -> do
      liftIO $ STM.atomically $ STM.writeTChan (stTerm st) ()
      breakWith ()
    StrataTimeout sop -> withTransaction $ do
      oid <- opcodeId (Proxy @arch) conn sop
      insertStrataTimeout conn oid
    SolverInvocation sop tm -> withTransaction $ do
      oid <- opcodeId (Proxy @arch) conn sop
      case tm of
        Completed dt -> insertSolverSuccess conn oid dt
        Timeout _ -> insertSolverTimeout conn oid
    RemovedCandidatePrograms sop nRemoved -> withTransaction $ do
      oid <- opcodeId (Proxy @arch) conn sop
      insertRemovedCandidates conn oid nRemoved
    CounterexampleFound sop -> withTransaction $ do
      oid <- opcodeId (Proxy @arch) conn sop
      insertCounterexample conn oid
    StrataSuccess sop tm -> withTransaction $ do
      oid <- opcodeId (Proxy @arch) conn sop
      insertStrataSuccess conn oid tm
    SynthesizeSuccess sop tm -> withTransaction $ do
      oid <- opcodeId (Proxy @arch) conn sop
      insertSynthesizeSuccess conn oid tm
  where
    conn = stConn st
    withTransaction k = liftIO (SQL.withTransaction conn k)
-}
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

-- | Record a single invocation of the solver (on behalf of some instruction)
-- and the time taken for the invocation
recordSolverInvocation :: Some (Opcode arch (Operand arch)) -> SolverTime -> StatisticsThread arch -> IO ()
recordSolverInvocation op tm st = STM.atomically $ STM.writeTChan (stMsgs st) msg
  where
    msg = SolverInvocation op tm

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

-- SQL


newtype OpcodeId = OpcodeId { unOpcodeId :: Int }
  deriving (Show)

insertSynthesizeSuccess :: SQL.Connection -> OpcodeId -> TM.NominalDiffTime -> IO ()
insertSynthesizeSuccess conn oid tm =
  SQL.execute conn "INSERT INTO synthesize_success VALUES(?,?);" (oid, realToFrac tm :: Double)

insertStrataSuccess :: SQL.Connection -> OpcodeId -> TM.NominalDiffTime -> IO ()
insertStrataSuccess conn oid tm =
  SQL.execute conn "INSERT INTO strata_success VALUES(?,?);" (oid, realToFrac tm :: Double)

insertCounterexample :: SQL.Connection -> OpcodeId -> IO ()
insertCounterexample conn oid =
  SQL.execute conn "INSERT INTO counterexample_found VALUES(?);" (SQL.Only oid)

insertStrataTimeout :: SQL.Connection -> OpcodeId -> IO ()
insertStrataTimeout conn oid =
  SQL.execute conn "INSERT INTO strata_timeouts VALUES(?);" (SQL.Only oid)

insertSolverSuccess :: SQL.Connection -> OpcodeId -> TM.NominalDiffTime -> IO ()
insertSolverSuccess conn oid tm =
  SQL.execute conn "INSERT INTO solver_invocation_success VALUES(?,?);" (oid, realToFrac tm :: Double)

insertSolverTimeout :: SQL.Connection -> OpcodeId -> IO ()
insertSolverTimeout conn oid =
  SQL.execute conn "INSERT INTO solver_invocation_timeout VALUES(?);" (SQL.Only oid)

insertRemovedCandidates :: SQL.Connection -> OpcodeId -> Int -> IO ()
insertRemovedCandidates conn oid nRemoved =
  SQL.execute conn "INSERT INTO removed_candidate_programs VALUES(?,?);" (oid, nRemoved)

schema :: [SQL.Query]
schema = [ "CREATE TABLE IF NOT EXISTS opcodes(opid INTEGER PRIMARY KEY,\
                                              \name TEXT NOT NULL UNIQUE);"
         , "CREATE TABLE IF NOT EXISTS strata_timeouts(strata_timeout_opid INTEGER NOT NULL,\
                                                      \FOREIGN KEY(strata_timeout_opid) REFERENCES opcodes(opid));"
         , "CREATE TABLE IF NOT EXISTS solver_invocation_success(solver_invocation_opid INTEGER NOT NULL,\
                                                                \success_seconds REAL NOT NULL,\
                                                                \FOREIGN KEY(solver_invocation_opid) REFERENCES opcodes(opid));"
         , "CREATE TABLE IF NOT EXISTS solver_invocation_timeout(solver_invocation_opid INTEGER NOT NULL,\
                                                                \FOREIGN KEY(solver_invocation_opid) REFERENCES opcodes(opid));"
         , "CREATE TABLE IF NOT EXISTS removed_candidate_programs(remove_candidate_programs_opid INTEGER NOT NULL,\
                                                                 \num_removed INTEGER NOT NULL,\
                                                                 \FOREIGN KEY(remove_candidate_programs_opid) REFERENCES opcodes(opid));"
         , "CREATE TABLE IF NOT EXISTS counterexample_found(counterexample_found_opid INTEGER NOT NULL,\
                                                           \FOREIGN KEY(counterexample_found_opid) REFERENCES opcodes(opid));"
         , "CREATE TABLE IF NOT EXISTS strata_success(strata_success_opid INTEGER NOT NULL,\
                                                     \strata_seconds REAL NOT NULL,\
                                                     \FOREIGN KEY(strata_success_opid) REFERENCES opcodes(opid));"
         , "CREATE TABLE IF NOT EXISTS synthesize_success(synthesize_success_opid INTEGER NOT NULL,\
                                                         \synthesize_seconds REAL NOT NULL,\
                                                         \FOREIGN KEY(synthesize_success_opid) REFERENCES opcodes(opid));"
         ]

-- Control helpers

-- | A helper monad to let us easily write an infinite loop that we can break
-- out of (with 'breakWith')
type Break r m a = ME.ExceptT r m a

-- | Loop forever with the option to break using 'breakWith'
loop :: (Monad m) => Break r m () -> m r
loop b = do
  res <- ME.runExceptT (forever b)
  case res of
    Left r -> return r
    Right r -> return r

-- | Break out of the 'Break' monad with a value
breakWith :: (Monad m) => r -> Break r m a
breakWith = ME.throwError

-- Boring instances

instance SQL.ToField OpcodeId where
  toField = SQL.toField . unOpcodeId

instance SQL.FromField OpcodeId where
  fromField x = OpcodeId <$> SQL.fromField x

