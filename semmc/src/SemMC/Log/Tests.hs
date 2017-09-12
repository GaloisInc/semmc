{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SemMC.Log.Tests where

import           Prelude hiding ( log )

import           SemMC.Log

import           Control.Concurrent
import           Control.Exception ( evaluate )
import           Control.Monad
import           Text.Printf ( printf )

----------------------------------------------------------------
-- * Tests of pure and monadic logging

test_monadicLoop :: (HasLogCfg, HasCallStack) => Int -> IO ()
test_monadicLoop n = do
  -- log Warn $ show Ghc.callStack
  log Info "About to loop ..."
  go n
  where
    go :: (HasLogCfg, HasCallStack) => Int -> IO ()
    -- go :: LogC => Int -> IO ()
    go 0 = return ()
    go n = do
      log Debug (printf "Looping with n = %i" n)
      go (n-1)

test_pureLoop :: (HasCallStack, HasLogCfg) => Int -> Int
test_pureLoop 0 = 0
test_pureLoop n = logTrace Debug (printf "Adding n = %i" n) $
  n + test_pureLoop (n-1)

----------------------------------------------------------------

-- To make this a machine checked test, we could define a log event
-- consumer (i.e. replace 'stdErrLogEventConsumer') that checked that
-- we got the log events we expect, in the order we expect.
main :: IO ()
main = do
  logCfg <- mkLogCfg
  let ?logCfg = logCfg
  _ <- forkIO $ stdErrLogEventConsumer logCfg

  test_monadicLoop 3
  void . evaluate $ test_pureLoop 3
