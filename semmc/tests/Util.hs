{-# LANGUAGE TypeApplications #-}
module Util ( tests ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified SemMC.Util.Tests as U

tests :: T.TestTree
tests = T.testGroup "Util"
  [ mkTest "asyncLinked_cancelExceptionsNotPropagated"
    U.test_asyncLinked_cancelExceptionsNotPropagated
  , mkTest "asyncLinked_nonCancelExceptionsPropagated"
    U.test_asyncLinked_nonCancelExceptionsPropagated
  , mkTest "withAsyncLinked_cancelExceptionsNotPropagated"
    U.test_withAsyncLinked_cancelExceptionsNotPropagated
  , mkTest "withAsyncLinked_nonCancelExceptionsPropagated"
    U.test_withAsyncLinked_nonCancelExceptionsPropagated ]

mkTest :: T.TestName -> IO (Maybe String) -> T.TestTree
mkTest name test = T.testCase name $ do
  result <- test
  -- If the test fails it returns @Just <failure description>@, so no
  -- reason for a more descriptive string here.
  T.assertEqual "" Nothing result
