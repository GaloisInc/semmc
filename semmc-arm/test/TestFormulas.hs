{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString as BS
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import           Data.Semigroup
import qualified Lang.Crucible.Backend as CRUB
import qualified Lang.Crucible.Backend.Simple as S
import qualified SemMC.Architecture.AArch32 as ARM
import           SemMC.Architecture.ARM.Combined
import           SemMC.Architecture.ARM.Opcodes ( allA32Semantics, allT32Semantics )
import qualified SemMC.Formula.Formula as F
import qualified SemMC.Formula.Load as FL
import qualified SemMC.Util as U
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified What4.Interface as CRU


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
tests = testGroup "Read Formulas"
        [ testCase "warmup test" $ 1 + 1 @?= (2::Int)
        , testA32Formulas
        , testT32Formulas
        ]

testA32Formulas :: TestTree
testA32Formulas = testGroup "A32 Formulas" $
                  fmap testFormula allA32Semantics

testT32Formulas :: TestTree
testT32Formulas = testGroup "T32 Formulas" $
                  fmap testFormula allT32Semantics

testFormula :: (Some (ARMOpcode ARMOperand), BS.ByteString) -> TestTree
testFormula a@(some'op, _sexp) = testCase ("formula for " <> (opname some'op)) $
  do Some ng <- PN.newIONonceGenerator
     sym <- S.newSimpleBackend ng
     fm <- withTestLogging $ loadFormula sym a
     -- The Main test is loadFormula doesn't generate an exception.
     -- The result should be a MapF with a valid entry in it.
     MapF.size fm @?= 1
    where opname (Some op) = showF op

loadFormula :: ( CRUB.IsSymInterface sym
               , ShowF (CRU.SymExpr sym)
               , U.HasLogCfg) =>
               sym
            -> (Some (ARMOpcode ARMOperand), BS.ByteString)
            -> IO (MapF.MapF (ARMOpcode ARMOperand) (F.ParameterizedFormula sym ARM.AArch32))
loadFormula sym a = FL.loadFormulas sym [a]
