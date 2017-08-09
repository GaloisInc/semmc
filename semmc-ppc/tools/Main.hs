{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Concurrent as C
import Control.Monad ( replicateM_, when )
import Data.Proxy ( Proxy(..) )
import qualified Data.Time.Format as T
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import Text.Printf ( printf )

import Lang.Crucible.BaseTypes ( BaseBVType )
import qualified Data.Word.Indexed as W
import qualified Dismantle.PPC as PPC
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Stochastic.Remote as R
import SemMC.Architecture.PPC

type PPCState = CS.ConcreteState PPC

main :: IO ()
main = do
  [hostname] <- E.getArgs
  logChan <- C.newChan
  caseChan <- C.newChan
  resChan <- C.newChan
  _ <- C.forkIO (printLogMessages logChan)
  _ <- C.forkIO (testRunner caseChan resChan)
  merr <- R.runRemote hostname testSerializer caseChan resChan logChan
  case merr of
    Just err -> do
      IO.hPutStrLn IO.stderr $ printf "SSH Error: %s" (show err)
      IO.exitFailure
    Nothing -> return ()

testRunner :: C.Chan (Maybe (R.TestCase PPCState PPC.Instruction))
           -> C.Chan (R.ResultOrError PPCState)
           -> IO ()
testRunner caseChan resChan = do
  [r1, r2] <- mapM (doTest Nothing) [testVector1, testVector2]
  replicateM_ 10000 (doTest (Just r1) testVector1)
  replicateM_ 10000 (doTest (Just r2) testVector2)
  C.writeChan caseChan Nothing
  where
    doTest mr vec = do
      C.writeChan caseChan (Just vec)
      res <- C.readChan resChan
      case res of
        R.InvalidTag t -> do
          IO.hPutStrLn IO.stderr $ printf "Invalid tag: %d" t
          IO.exitFailure
        R.TestContextParseFailure -> do
          IO.hPutStrLn IO.stderr "Test context parse failure"
          IO.exitFailure
        R.TestSignalError nonce sig -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with unexpected signal (%d) on test case %d" sig nonce
          IO.exitFailure
        R.TestReadError tag -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with a read error (%d)" tag
          IO.exitFailure
        R.TestSuccess tr -> do
          -- printf "Received test result with nonce %d\n" (R.resultNonce tr)
          case mr of
            Just oldRes -> do
              when (oldRes /= R.resultContext tr) $ do
                IO.hPutStrLn IO.stderr "ERROR: Context mismatch"
            Nothing -> return ()
          -- print (R.resultContext tr)
          return (R.resultContext tr)

testVector1 :: R.TestCase PPCState PPC.Instruction
testVector1 =
  R.TestCase { R.testNonce = 11
             , R.testProgram = [i]
             , R.testContext = sbase
             }
  where
    -- ctx0 = CS.zeroState (Proxy @PPC)
    -- ctx1 = CS.pokeMS ctx0 v14 (CS.ValueBV (W.W 4161512095))
    -- ctx2 = CS.pokeMS ctx1 v16 (CS.ValueBV (W.W 1648667738))
    -- ctx3 = CS.pokeMS ctx2 v13 (CS.ValueBV (W.W 2624061017)) -- (W.W 2457480688))
    -- ctx4 = CS.pokeMS ctx3 v9 (CS.ValueBV (W.W 3022194918))
    i = PPC.Instruction PPC.ADD4 (PPC.Gprc r28 PPC.:> PPC.Gprc r17 PPC.:> PPC.Gprc r25 PPC.:> PPC.Nil)

sbase = toState [ 1228099099
                , 1418706367
                , 1088784811
                , 1086493020
                , 993879871
                , 1842131776
                , 3208511580
                , 3445859057
                , 2715791173
                , 705601903
                , 2507931803
                , 2791613726
                , 308375196
                , 1965798804
                , 3843768939
                , 521939701
                , 2045111831
                , 1950251963
                , 1544847281 -- r18 base
                , 2705051657
                , 1842052213
                , 1083024008
                , 1943252099
                , 2130324546
                , 3894957546
                , 1593304881
                , 186677478
                , 4031322344
                , 81925986 --
                , 4221655630
                , 3278629892
                , 1933312003
                ]

smod = toState [ 1228099099
               , 1418706367
               , 1088784811
               , 1086493020
               , 993879871
               , 1842131776
               , 3208511580
               , 3445859057
               , 2715791173
               , 705601903
               , 2507931803
               , 2791613726
               , 308375196
               , 1965798804
               , 3843768939
               , 521939701
               , 2045111831
               , 1950251963
               , 3767770070 -- r18 (modified)
               , 2705051657
               , 1842052213
               , 1083024008
               , 1943252099
               , 2130324546
               , 3894957546
               , 1593304881
               , 186677478
               , 4031322344
               , 81925986 --
               , 4221655630
               , 3278629892
               , 1933312003
               ]

{-

Initial base: { r0 -> ValueBV (1228099099 :: W 32), r1 -> ValueBV (1418706367 ::
W 32), r2 -> ValueBV (1088784811 :: W 32), r3 -> ValueBV (1086493020 :: W 32),
r4 -> ValueBV (993879871 :: W 32), r5 -> ValueBV (1842131776 :: W 32), r6 ->
ValueBV (3208511580 :: W 32), r7 -> ValueBV (3445859057 :: W 32), r8 -> ValueBV
(2715791173 :: W 32), r9 -> ValueBV (705601903 :: W 32), r10 -> ValueBV
(2507931803 :: W 32), r11 -> ValueBV (2791613726 :: W 32), r12 -> ValueBV
(308375196 :: W 32), r13 -> ValueBV (1965798804 :: W 32), r14 -> ValueBV
(3843768939 :: W 32), r15 -> ValueBV (521939701 :: W 32), r16 -> ValueBV
(2045111831 :: W 32), r17 -> ValueBV (1950251963 :: W 32), r18 -> ValueBV
(1544847281 :: W 32), r19 -> ValueBV (2705051657 :: W 32), r20 -> ValueBV
(1842052213 :: W 32), r21 -> ValueBV (1083024008 :: W 32), r22 -> ValueBV
(1943252099 :: W 32), r23 -> ValueBV (2130324546 :: W 32), r24 -> ValueBV
(3894957546 :: W 32), r25 -> ValueBV (1593304881 :: W 32), r26 -> ValueBV
(186677478 :: W 32), r27 -> ValueBV (4031322344 :: W 32), r28 -> ValueBV
(81925986 :: W 32), r29 -> ValueBV (4221655630 :: W 32), r30 -> ValueBV
(3278629892 :: W 32), r31 -> ValueBV (1933312003 :: W 32)
}



Initial Tweaked: { r0 -> ValueBV (1228099099 :: W 32), r1 -> ValueBV (1418706367
:: W 32), r2 -> ValueBV (1088784811 :: W 32), r3 -> ValueBV (1086493020 :: W
32), r4 -> ValueBV (993879871 :: W 32), r5 -> ValueBV (1842131776 :: W 32), r6
-> ValueBV (3208511580 :: W 32), r7 -> ValueBV (3445859057 :: W 32), r8 ->
ValueBV (2715791173 :: W 32), r9 -> ValueBV (705601903 :: W 32), r10 -> ValueBV
(2507931803 :: W 32), r11 -> ValueBV (2791613726 :: W 32), r12 -> ValueBV
(308375196 :: W 32), r13 -> ValueBV (1965798804 :: W 32), r14 -> ValueBV
(3843768939 :: W 32), r15 -> ValueBV (521939701 :: W 32), r16 -> ValueBV
(2045111831 :: W 32), r17 -> ValueBV (1950251963 :: W 32), r18 -> ValueBV
(3767770070 :: W 32), r19 -> ValueBV (2705051657 :: W 32), r20 -> ValueBV
(1842052213 :: W 32), r21 -> ValueBV (1083024008 :: W 32), r22 -> ValueBV
(1943252099 :: W 32), r23 -> ValueBV (2130324546 :: W 32), r24 -> ValueBV
(3894957546 :: W 32), r25 -> ValueBV (1593304881 :: W 32), r26 -> ValueBV
(186677478 :: W 32), r27 -> ValueBV (4031322344 :: W 32), r28 -> ValueBV
(81925986 :: W 32), r29 -> ValueBV (4221655630 :: W 32), r30 -> ValueBV
(3278629892 :: W 32), r31 -> ValueBV (1933312003 :: W 32), 
 

-}

toState vals = foldr poke (CS.zeroState (Proxy @PPC)) (zip gprviews vals)
  where
    poke (v, val) ctx = CS.pokeMS ctx v (CS.ValueBV (W.W val))

gprs :: [Location (BaseBVType 32)]
gprs = fmap (LocGPR . PPC.GPR) [0..31]

gprviews = fmap (CS.trivialView (Proxy @PPC)) gprs

r1 :: PPC.GPR
r1 = PPC.GPR 1
r2 :: PPC.GPR
r2 = PPC.GPR 2
r3 :: PPC.GPR
r3 = PPC.GPR 3

r9 = PPC.GPR 9
v9 = CS.trivialView (Proxy @PPC) (LocGPR r9)
r12 = PPC.GPR 12
v12 = CS.trivialView (Proxy @PPC) (LocGPR r12)
r13 = PPC.GPR 13
v13 = CS.trivialView (Proxy @PPC) (LocGPR r13)
r14 = PPC.GPR 14
v14 = CS.trivialView (Proxy @PPC) (LocGPR r14)
r16 = PPC.GPR 16
v16 = CS.trivialView (Proxy @PPC) (LocGPR r16)
r23 = PPC.GPR 23
v23 = CS.trivialView (Proxy @PPC) (LocGPR r23)
-- 28, 25, 17
r17 = PPC.GPR 17
v17 = CS.trivialView (Proxy @PPC) (LocGPR r16)
r18 = PPC.GPR 18
v18 = CS.trivialView (Proxy @PPC) (LocGPR r18)
r25 = PPC.GPR 25
v25 = CS.trivialView (Proxy @PPC) (LocGPR r25)
r28 = PPC.GPR 28
v28 = CS.trivialView (Proxy @PPC) (LocGPR r28)

v2 :: CS.View PPC 32
v2 = CS.trivialView (Proxy @PPC) (LocGPR r2)

v3 :: CS.View PPC 32
v3 = CS.trivialView (Proxy @PPC) (LocGPR r3)

testVector2 :: R.TestCase PPCState PPC.Instruction
testVector2 = testVector1 { R.testNonce = 22
                          , R.testContext = smod -- ctx2
                          , R.testProgram = [i]
                          }
  where
    -- ctx0 = CS.zeroState (Proxy @PPC)
    -- ctx1 = CS.pokeMS ctx0 v2 (CS.ValueBV (W.W 1))
    -- ctx2 = CS.pokeMS ctx1 v3 (CS.ValueBV (W.W 5))
    i = PPC.Instruction PPC.ADD4 (PPC.Gprc r28 PPC.:> PPC.Gprc r17 PPC.:> PPC.Gprc r25 PPC.:> PPC.Nil)

printLogMessages :: C.Chan R.LogMessage -> IO ()
printLogMessages c = do
  msg <- C.readChan c
  let fmtTime = T.formatTime T.defaultTimeLocale "%T" (R.lmTime msg)
  -- IO.hPutStrLn IO.stderr $ printf "%s[%s]: %s" fmtTime (R.lmHost msg) (R.lmMessage msg)
  printLogMessages c
