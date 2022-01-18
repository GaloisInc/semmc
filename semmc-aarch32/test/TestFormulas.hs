{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Concurrent
import           Control.Exception
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some

import           SemMC.Architecture.ARM.Opcodes ( loadSemantics
                                                , ASLSemanticsOpts(..))
import qualified SemMC.Util as U
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified What4.Expr.Builder as WB

main :: IO ()
main = do
  defaultMain tests


withTestLogging :: (U.HasLogCfg => IO a) -> IO a

withTestLogging op = do
  logOut <- newMVar []
  U.withLogging "testmain" (logVarEventConsumer logOut (const True)) $
   catch op $
       \(e :: SomeException) ->
           do threadDelay 10000
              -- the delay allows async log thread to make updates.  A
              -- delay is a kludgy hack, but this will only occur when
              -- the test has failed anyhow, so some extra punishment
              -- is not uncalled for.
              takeMVar logOut >>= (hPutStrLn stderr . concatMap U.prettyLogEvent)
              throwIO e


-- | A log event consumer that prints formatted log events to stderr.
logVarEventConsumer :: MVar [U.LogEvent] -> (U.LogEvent -> Bool) -> U.LogCfg -> IO ()
logVarEventConsumer logOut logPred =
  U.consumeUntilEnd logPred $ \e -> do
    modifyMVar logOut $ \l -> return (l ++ [e], ())


tests :: TestTree
tests = do testGroup "Read Formulas" [ testAll ]

testAll :: TestTree
testAll = testCase "testAll" $ withTestLogging $ do
  Some ng <- PN.newIONonceGenerator
  sym <- WB.newExprBuilder WB.FloatIEEERepr SemMCAArch32Data ng
  WB.startCaching sym
  _sem <- loadSemantics sym (ASLSemanticsOpts { aslOptTrimRegs = True })
  return ()

data SemMCAArch32Data t = SemMCAArch32Data
