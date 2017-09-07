{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SemMC.Log.Tests where

import           Prelude hiding ( log )

import           SemMC.Log

import qualified GHC.Stack as Ghc

import           Control.Concurrent
import           Control.Exception ( evaluate )
import           Control.Monad
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans.Reader
import           Text.Printf ( printf )

----------------------------------------------------------------
-- * Example logger monad

-- | A 'MonadLogger' monad.
newtype M a = M { unM :: ReaderT LogCfg IO a }
  deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadLogger M where
  getLogCfg = M ask

runM :: LogCfg -> M a -> IO a
runM cfg = flip runReaderT cfg . unM

----------------------------------------------------------------
-- * Tests of pure and monadic logging

test_monadicLoop :: Ghc.HasCallStack => Int -> M ()
test_monadicLoop n = do
  log Info "About to loop ..."
  go n
  where
    -- go :: Ghc.HasCallStack => Int -> M ()
    go 0 = return ()
    go n = do
      log Debug (printf "Looping with n = %i" n)
      go (n-1)

test_pureLoop :: Ghc.HasCallStack => LogCfg -> Int -> Int
test_pureLoop _   0 = 0
test_pureLoop cfg n = logTrace cfg Debug (printf "Adding n = %i" n) $
  n + test_pureLoop cfg (n-1)

----------------------------------------------------------------

-- To make this a machine checked test, we could define a log event
-- consumer (i.e. replace 'stdErrLogEventConsumer') that checked that
-- we got the log events we expect, in the order we expect.
main :: IO ()
main = do
  cfg <- mkLogCfg
  _ <- forkIO $ stdErrLogEventConsumer cfg
  runM cfg $ do
    test_monadicLoop 3
    liftIO $ void . evaluate $ test_pureLoop cfg 3
