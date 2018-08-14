{-# LANGUAGE BangPatterns #-}
-- | Description: Tests for SemMC.Util
--
-- Tests for 'SemMC. These tests return 'Nothing' on success and 'Just
-- <failure description>' on failure.
module SemMC.Util.Tests where

import qualified Control.Exception as CE
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Async as CC

import qualified SemMC.Util as U

tryAny :: IO a -> IO (Either CE.SomeException a)
tryAny = CE.try

-- | Test that canceling an 'asyncLinked' async doesn't raise an
-- exception.
test_asyncLinked_cancelExceptionsNotPropagated :: IO (Maybe String)
test_asyncLinked_cancelExceptionsNotPropagated = do
  result <- tryAny $ do
    a <- U.asyncLinked $ CC.threadDelay 1000000
    CC.cancel a
  return $ case result of
    Left e -> Just $ "Unexpected exception: "++show e
    Right _ -> Nothing

-- | Test that non-'CC.cancel' exceptions are propagated by
-- 'asyncLinked'.
test_asyncLinked_nonCancelExceptionsPropagated :: IO (Maybe String)
test_asyncLinked_nonCancelExceptionsPropagated = do
  let e = userError "Oh no!"
  result <- tryAny $ do
    _ <- U.asyncLinked $ ioError e
    -- If we return immediately there is a race between the exception
    -- firing and the async being returned, which allows the exception
    -- to escape the 'tryAny'.
    CC.threadDelay 1000000
  return $ case result of
             Left se | Just (CC.ExceptionInLinkedThread _ e'') <- CE.fromException se
                     , Just e' <- CE.fromException e''
                     , e' == e -> Nothing
             Left se | Just e' <- CE.fromException se
                     , e' == e -> Nothing
             Left e' -> Just $ "Unexpected exception: "++show e'
             Right _ -> Just $ "No exception! Expecting: "++show e

-- | Test that canceling an 'withAsyncLinked' async doesn't raise an
-- exception.
test_withAsyncLinked_cancelExceptionsNotPropagated :: IO (Maybe String)
test_withAsyncLinked_cancelExceptionsNotPropagated = do
  result <- tryAny $ do
    U.withAsyncLinked (CC.threadDelay 1000000) CC.cancel
  return $ case result of
    Left e -> Just $ "Unexpected exception: "++show e
    Right _ -> Nothing

-- | Test that non-'CC.cancel' exceptions are propagated by
-- 'withAsyncLinked'.
test_withAsyncLinked_nonCancelExceptionsPropagated :: IO (Maybe String)
test_withAsyncLinked_nonCancelExceptionsPropagated = do
  let e = userError "Oh no!"
  result <- tryAny $ do
    U.withAsyncLinked (ioError e) (const $ CC.threadDelay 1000000)
  return $ case result of
             Left se | Just (CC.ExceptionInLinkedThread _ e'') <- CE.fromException se
                     , Just e' <- CE.fromException e''
                     , e' == e -> Nothing
             Left se | Just e' <- CE.fromException se
                     , e' == e -> Nothing
             Left e' -> Just $ "Unexpected exception: "++show e'
             Right _ -> Just $ "No exception! Expecting: "++show e
