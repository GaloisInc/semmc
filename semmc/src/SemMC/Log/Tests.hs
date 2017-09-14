{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Log.Tests where

import           Prelude hiding ( log )

import           SemMC.Log

import           Control.Concurrent
import           Control.Concurrent.Async ( wait )
import           Control.Exception ( evaluate )
import           Control.Monad
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans.Reader
import           Text.Printf ( printf )

----------------------------------------------------------------
--- * Example logger monad

-- | A 'MonadLogger' monad.
newtype M a = M { unM :: ReaderT LogCfg IO a }
  deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadHasLogCfg M where
  getLogCfgM = M ask

runM :: LogCfg -> M a -> IO a
runM cfg = flip runReaderT cfg . unM

-----------------------------------------------------------------
-- * Tests of pure and monadic logging

test_monadicLoop :: forall m. (MonadHasLogCfg m, MonadIO m, HasCallStack) => Int -> m ()
test_monadicLoop n = do
  logM Info "About to loop ..."
  go n
  where
    go :: (HasCallStack) => Int -> m ()
    go 0 = return ()
    go n = do
      logM Debug (printf "Looping with n = %i" n)
      go (n-1)

test_IOLoop :: (HasLogCfg, HasCallStack) => Int -> IO ()
test_IOLoop n = do
  logIO Info "About to loop ..."
  go n
  where
    go :: (HasLogCfg, HasCallStack) => Int -> IO ()
    go 0 = return ()
    go n = do
      logIO Debug (printf "Looping with n = %i" n)
      go (n-1)

test_pureLoop :: (HasCallStack, HasLogCfg) => Int -> Int
test_pureLoop 0 = 0
test_pureLoop n = logTrace Debug (printf "Adding n = %i" n) $
  n + test_pureLoop (n-1)

test_asyncNamed :: (HasLogCfg) => IO ()
test_asyncNamed = void . wait =<< asyncNamed getLogCfg "test_asyncNamed" (test_IOLoop 3)

----------------------------------------------------------------

-- To make this a machine checked test, we could define a log event
-- consumer (i.e. replace 'stdErrLogEventConsumer') that checked that
-- we got the log events we expect, in the order we expect.
main :: IO ()
main = do
  cfg <- mkLogCfg "main"
  _ <- forkIO $ stdErrLogEventConsumer cfg
  
  withLogCfg cfg $ do
    runM getLogCfg $ test_monadicLoop 3
    test_IOLoop 3
    void . evaluate $ test_pureLoop 3
    test_asyncNamed
