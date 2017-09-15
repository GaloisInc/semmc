{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Description: Log msgs via a synchronized channel
--
-- Log msgs via a synchronized channel.
--
-- With inspiration from the @monad-logger@ package.
--
-- See examples in 'SemMC.Log.Tests'.
--
-- WARNING: loggers that automatically infer the call stack (via
-- `Ghc.HasCallStack`) are not composable, in that they infer a call
-- stack at their call site. So, if you use one to build up another
-- log function, then that derived log function will infer bogus call
-- sites! Of course, it's pretty easy to write
--
--     writeLogEvent logCfg level msg
--
-- when defining a new logger, so not a big deal, just something to
-- watch out for.
module SemMC.Log (
  -- * Misc
  LogLevel(..),
  LogMsg,
  Ghc.HasCallStack,
  -- * Implicit param logger interface
  HasLogCfg,
  logIO,
  logTrace,
  withLogCfg,
  getLogCfg,
  -- * Explicit parameter logger interface
  logIOWith,
  logEndWith,
  writeLogEvent,
  -- * Monadic logger interface
  MonadHasLogCfg(..),
  logM,
  -- * Configuration
  LogCfg,
  mkLogCfg,
  -- * Log consumers
  stdErrLogEventConsumer,
  fileLogEventConsumer,
  tmpFileLogEventConsumer,
  -- * Named threads
  named,
  namedM,
  asyncNamed,
  asyncNamedM
  ) where

import qualified GHC.Err.Located as Ghc
import qualified GHC.Stack as Ghc

import qualified Control.Concurrent as Cc
import           Control.Concurrent.Async ( Async )
import qualified Control.Concurrent.Async as Cc
import qualified Control.Exception as Cc

import qualified UnliftIO as U

import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Cm
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           System.Directory ( createDirectoryIfMissing )
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO
import           Text.Printf ( printf )

----------------------------------------------------------------
-- * API

-- | Log levels, in increasing severity/precedence order.
data LogLevel = Debug -- ^ Fine details
              | Info  -- ^ Tracking progress
              | Warn  -- ^ Something notable or suspicious
              | Error -- ^ Something bad
              deriving (Show, Eq, Ord)

type LogMsg = String

-- | Constraints for functions that call the logger.
--
-- WARNING: Due to GHC bug, this can't be used:
-- https://ghc.haskell.org/trac/ghc/ticket/14218
--
-- Prefer this to plain 'HasLogCfg', since this also adds a call stack
-- frame which allows the logger to print the name of the function
-- calling the logger.
{-
type LogC = (Ghc.HasCallStack, HasLogCfg)
-}

----------------------------------------------------------------
-- ** Implicit param logger interface

-- | Access to the log config.
--
-- Users should prefer 'withLogCfg' to binding the implicit param. The
-- implicit param is an implementation detail, and we could change the
-- implementation later, e.g. to use the @reflection@ package.
--
-- We use an implicit param to avoid having to change all code in 'IO'
-- that wants to log to be in 'MonadHasLogCfg' and 'MonadIO' classes.
--
-- An even more convenient but more "unsafe" implementation would
-- store the 'LogCfg' in a global, 'unsafePerformIO'd 'IORef'
-- (cf. @uniqueSource@ in 'Data.Unique').
type HasLogCfg = (?logCfg :: LogCfg)

-- | Satisfy a 'HasLogCfg' constraint.
--
-- Users can call this function instead of using @ImplicitParams@
-- themselves.
withLogCfg :: LogCfg -> (HasLogCfg => a) -> a
withLogCfg logCfg x = let ?logCfg = logCfg in x

-- | Recover the log config.
--
-- Useful for going between implicit and monadic interfaces. E.g.
--
-- > flip runReaderT getLogCfg ...
getLogCfg :: HasLogCfg => LogCfg
getLogCfg = ?logCfg

-- | Log in a 'MonadIO'.
--
-- If you want the name of function that called 'log' to be included
-- in the output, then you need to add a 'Ghc.HasCallStack' constraint
-- to it as well (see 'LogC'). Otherwise, one of two things will happen:
--
-- - if no enclosing function has a 'Ghc.HasCallStack' constraint,
--   then '???' will be used for the enclosing function name.
--
-- - if at least one enclosing function has a 'Ghc.HasCallStack'
--   constraint, then the name of the *closest* enclosing function
--   with that constraint will be used for the enclosing function
--   name. So, for example, if you define @outer@ by
--
--   > outer :: (MonadHasLogCfg m, Ghc.HasCallStack) => m Int
--   > outer = inner
--   >   where
--   >     inner = do
--   >       log Debug "Inside 'inner' ..."
--   >       return 42
--
--   then the call to 'log' in @inner@ will have "outer" as the
--   enclosing function name.
logIO :: (HasLogCfg, Ghc.HasCallStack, MonadIO m)
      => LogLevel -> LogMsg -> m ()
logIO level msg = do
  liftIO $ writeLogEvent ?logCfg Ghc.callStack level msg

-- | 'logIO' with an explicit config
logIOWith :: (Ghc.HasCallStack, MonadIO m) => LogCfg -> LogLevel -> LogMsg -> m ()
logIOWith cfg level msg =
  liftIO $ writeLogEvent cfg Ghc.callStack level msg

-- | Log in pure code using 'unsafePerformIO', like 'Debug.Trace'.
--
-- See 'logIO'.
logTrace :: (HasLogCfg, Ghc.HasCallStack) => LogLevel -> LogMsg -> a -> a
logTrace level msg x = x {- IO.unsafePerformIO $ do
  writeLogEvent ?logCfg Ghc.callStack level msg
  return x -}
{-# NOINLINE logTrace #-}

----------------------------------------------------------------
-- ** Monadic logger interface

-- | Monads with logger configuration.
class MonadHasLogCfg m where
  getLogCfgM :: m LogCfg

-- | Log in a 'MonadHasLogCfg'.
--
-- See 'logIO'.
logM :: (MonadHasLogCfg m, Ghc.HasCallStack, MonadIO m)
     => LogLevel -> LogMsg -> m ()
logM level msg = do
  logCfg <- getLogCfgM
  liftIO $ writeLogEvent logCfg Ghc.callStack level msg

-- | Signal to the log consumer that there are no more log messages and
-- terminate the log consumer.  This is useful for cases where the logger is
-- running in a separate thread and the parent thread wants to wait until the
-- logger has finished logging and has successfully flushed all log messages
-- before terminating it.
logEndWith :: LogCfg -> IO ()
logEndWith cfg = Stm.atomically $ Stm.writeTChan (lcChan cfg) Nothing

----------------------------------------------------------------
-- ** Initialization

-- | Initialize a 'LogCfg'.
--
-- The first argument is the human friendly name to assign to the
-- current thread. Since logging should be configured as soon as
-- possible on startup, "main" is probably the right name.
--
-- See 'asyncNamed' for naming other threads.
--
-- Need to start a log event consumer in another thread,
-- e.g. 'stdErrLogEventConsumer', if you want anything to happen with
-- the log events.
mkLogCfg :: String -> IO LogCfg
mkLogCfg threadName = do
  lcChan <- Stm.newTChanIO
  threadMap <- do
    tid <- show <$> Cc.myThreadId
    return $ Map.fromList [ (tid, threadName) ]
  lcThreadMap <- Stm.newTVarIO threadMap
  return $ LogCfg { lcChan = lcChan
                  , lcThreadMap = lcThreadMap }

----------------------------------------------------------------
-- ** Log event consumers

consumeUntilEnd :: (LogEvent -> IO ()) -> LogCfg -> IO ()
consumeUntilEnd k cfg = do
  mevent <- Stm.atomically $ Stm.readTChan (lcChan cfg)
  case mevent of
    Nothing -> return ()
    Just event -> k event >> consumeUntilEnd k cfg

-- | A log event consumer that prints formatted log events to stderr.
stdErrLogEventConsumer :: LogCfg -> IO ()
stdErrLogEventConsumer = consumeUntilEnd (IO.hPutStrLn IO.stderr . prettyLogEvent)

-- | A logger that writes to a user-specified file
--
-- Note that logs are opened in the 'w' mode (i.e., overwrite).  Callers should
-- preserve old log files if they really want.
fileLogEventConsumer :: FilePath -> LogCfg -> IO ()
fileLogEventConsumer fp cfg = IO.withFile fp IO.WriteMode $ \h -> do
  consumeUntilEnd (\e -> IO.hPutStrLn h (prettyLogEvent e) >> IO.hFlush h) cfg

-- | A log event consumer that writes formatted log events to a tmp
-- file.
tmpFileLogEventConsumer :: LogCfg -> IO ()
tmpFileLogEventConsumer cfg = do
  createDirectoryIfMissing True "/tmp/brittle"
  (tmpFilePath, tmpFile) <- IO.openTempFile "/tmp/brittle" "log.txt"
  printf "\n\nWriting logs to %s\n\n" tmpFilePath
  consumeUntilEnd (\e -> IO.hPutStrLn tmpFile (prettyLogEvent e) >> IO.hFlush tmpFile) cfg

----------------------------------------------------------------
-- ** Named threads

-- | Run an IO action with a human friendly thread name.
--
-- Any existing thread name will be restored when the action finishes.
named :: (U.MonadUnliftIO m, MonadIO m) => LogCfg -> String -> m a -> m a
named cfg threadName action = do
  actionIO <- U.toIO action
  liftIO $ do
    tid <- show <$> Cc.myThreadId
    mOldName <- Map.lookup tid <$> Stm.readTVarIO (lcThreadMap cfg)
    Cc.bracket_ (insert tid) (remove tid mOldName) actionIO
  where
    modify = Stm.atomically . Stm.modifyTVar' (lcThreadMap cfg)

    insert tid = modify $ Map.insert tid threadName

    remove tid Nothing        = modify $ Map.delete tid
    remove tid (Just oldName) = modify $ Map.insert tid oldName

-- | Version of 'named' for 'MonadHasLogCfg' monads.
namedM :: (MonadHasLogCfg m, U.MonadUnliftIO m, MonadIO m)
       => String -> m a -> m a
namedM threadName action = do
  cfg <- getLogCfgM
  named cfg threadName action

-- | Fork a thread, giving it a human friendly name for use in log
-- messages.
asyncNamed :: (U.MonadUnliftIO m, MonadIO m)
           => LogCfg -> String -> m a -> m (Async a)
asyncNamed cfg threadName = U.async . named cfg threadName

-- | Version of 'asyncNamed' for 'MonadHasLogCfg' monads.
asyncNamedM :: (MonadHasLogCfg m, U.MonadUnliftIO m, MonadIO m) => String -> m a -> m (Async a)
asyncNamedM threadName action = do
  cfg <- getLogCfgM
  asyncNamed cfg threadName action

----------------------------------------------------------------
-- * Internals

-- | Stored as 'String' because 'Control.Concurrent.ThreadId' docs say
-- a thread can't be GC'd as long as someone maintains a reference to
-- its 'ThreadId'!!!
type ThreadId = String

-- | A log event.
--
-- Can be converted to a string later, or thrown away.
data LogEvent = LogEvent
  { leCallSite :: (Maybe String, Ghc.SrcLoc)
    -- ^ The @Maybe String@ is the name of the enclosing function in
    -- which the logging function was called. Not always available,
    -- since it depends on the enclosing function having a
    -- 'Ghc.HasCallStack' constraint.
  , leLevel    :: LogLevel
  , leMsg      :: LogMsg
  , leThreadId :: ThreadId
    -- ^ ID of thread that generated the event.
  }

-- | Logging configuration.
data LogCfg = LogCfg
  { lcChan :: Stm.TChan (Maybe LogEvent)
  , lcThreadMap :: Stm.TVar (Map ThreadId String)
    -- ^ User friendly names for threads. See 'asyncNamed'.

  -- Idea: add a predicate on log events that is used to discard log
  -- events that e.g. aren't of a high enough precedence
  -- level. E.g. only keep events of level 'Warn' or above:
  --
  -- > lcPred le = leLevel le >= Warn
  --
  -- , lcPred :: LogEvent -> Bool
  }

-- | Format a log event.
prettyLogEvent :: LogEvent -> String
prettyLogEvent le =
  printf "[%s][%s][%s]\n%s"
    (show $ leLevel le) location (leThreadId le) (leMsg le)
  where
    location :: String
    location = printf "%s:%s"
      (prettyFun maybeFun) (Ghc.prettySrcLoc srcLoc)
    (maybeFun, srcLoc) = leCallSite le
    prettyFun Nothing = "???"
    prettyFun (Just fun) = fun

prettyThreadId :: LogCfg -> ThreadId -> IO ThreadId
prettyThreadId cfg tid = do
  mThreadName <- Map.lookup tid <$> Stm.readTVarIO (lcThreadMap cfg)
  return $ printf "%s (%s)" (maybe "???" id mThreadName) tid

-- | Write a 'LogEvent' to the underlying channel.
--
-- This is a low-level function. See 'logIO', 'logM', and 'logTrace'
-- for a high-level interface that supplies the 'LogCfg' and
-- 'Ghc.CallStack' parameters automatically.
--
-- However, those functions can't be used to build up custom loggers,
-- since they infer call stack information automatically. If you want
-- to define a custom logger (even something simple like
--
-- > debug msg = logM Debug msg
--
-- ) then use 'writeLogEvent'.
writeLogEvent :: LogCfg -> Ghc.CallStack -> LogLevel -> LogMsg -> IO ()
writeLogEvent cfg cs level msg = do
  -- tid <- show <$> Cc.myThreadId
  -- ptid <- prettyThreadId cfg tid
  Stm.atomically $ Stm.writeTChan (lcChan cfg) (Just (event ""))
  where
    event tid = LogEvent
      { leCallSite = callSite
      , leLevel = level
      , leMsg = msg
      , leThreadId = tid
      }
    -- | The call stack has the most recent call first. Assuming
    -- 'writeLogEvent' is always called in a logging function with a
    -- 'Ghc.HasCallStack' constraint, the call stack will be non-empty
    -- -- i.e. @topSrcLoc@ will be defined -- but there may not be a
    -- lower frame corresponding to the context in which the logging
    -- function was called. To get a lower frame, some enclosing
    -- function needs a 'Ghc.HasCallStack' constraint itself.
    --
    -- And only functions with 'Ghc.HasCallStack' will get frames. See
    -- discussion at 'log'.
    callSite = case Ghc.getCallStack cs of
                 (_,topSrcLoc):rest -> case rest of
                   []                 -> (Nothing,           topSrcLoc)
                   (enclosingFun,_):_ -> (Just enclosingFun, topSrcLoc)
                 [] -> Ghc.error "Do we ever not have a call site?"
