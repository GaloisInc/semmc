{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Concurrent as C
import           Control.Monad ( replicateM_, when )
import           Data.Proxy ( Proxy(..) )
import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import           Text.Printf ( printf )

import qualified Data.Parameterized.Map as MapF
import qualified Data.Word.Indexed as W
import qualified Dismantle.PPC as PPC
import qualified SemMC.Architecture.Concrete as AC
import           SemMC.Architecture.PPC32
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Log as L
import           What4.BaseTypes ( BaseBVType )

type PPCState = V.ConcreteState PPC

main :: IO ()
main = do
  [hostname] <- E.getArgs
  caseChan <- C.newChan
  resChan <- C.newChan
  _ <- C.forkIO (testRunner caseChan resChan)
  logCfg <- L.mkLogCfg "main"
  L.withLogCfg logCfg $
    CE.runRemote (Just "remote-runner.ppc32") hostname testSerializer caseChan resChan

testRunner :: C.Chan (Maybe [CE.TestCase PPCState PPC.Instruction])
           -> C.Chan (CE.ResultOrError PPCState)
           -> IO ()
testRunner caseChan resChan = do
  [res1, res2] <- mapM (doTest Nothing) [testVector1, testVector2]
  replicateM_ 10000 (doTest (Just res1) testVector1)
  replicateM_ 10000 (doTest (Just res2) testVector2)
  C.writeChan caseChan Nothing
  where
    doTest mr vec = do
      C.writeChan caseChan (Just [vec])
      res <- C.readChan resChan
      case res of
        CE.InvalidTag t -> do
          IO.hPutStrLn IO.stderr $ printf "Invalid tag: %d" t
          IO.exitFailure
        CE.TestContextParseFailure -> do
          IO.hPutStrLn IO.stderr "Test context parse failure"
          IO.exitFailure
        CE.TestSignalError nonce sig -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with unexpected signal (%d) on test case %d" sig nonce
          IO.exitFailure
        CE.TestReadError tag -> do
          IO.hPutStrLn IO.stderr $ printf "Failed with a read error (%d)" tag
          IO.exitFailure
        CE.TestSuccess tr -> do
          -- printf "Received test result with nonce %d\n" (R.resultNonce tr)
          case mr of
            Just oldRes -> do
              when (oldRes /= CE.resultContext tr) $ do
                IO.hPutStrLn IO.stderr "ERROR: Context mismatch"
            Nothing -> return ()
          -- print (R.resultContext tr)
          return (CE.resultContext tr)

testVector1 :: CE.TestCase PPCState PPC.Instruction
testVector1 =
  CE.TestCase { CE.testNonce = 11
              , CE.testProgram = [i]
              , CE.testContext = sbase
             }
  where
    i = PPC.Instruction PPC.ADD4 (PPC.Gprc r28 PPC.:< PPC.Gprc r17 PPC.:< PPC.Gprc r25 PPC.:< PPC.Nil)

sbase :: MapF.MapF (Location PPC) V.Value
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

smod :: MapF.MapF (Location PPC) V.Value
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

toState :: [Integer] -> MapF.MapF (Location PPC) V.Value
toState vals = foldr poke (AC.zeroState (Proxy @PPC)) (zip gprviews vals)
  where
    poke (v, val) ctx = V.pokeMS ctx v (V.ValueBV (W.w val))

gprs :: [Location PPC (BaseBVType 32)]
gprs = fmap (LocGPR . PPC.GPR) [0..31]

gprviews :: [V.View PPC 32]
gprviews = fmap (V.trivialView (Proxy @PPC)) gprs


r17 :: PPC.GPR
r17 = PPC.GPR 17
r25 :: PPC.GPR
r25 = PPC.GPR 25
r28 :: PPC.GPR
r28 = PPC.GPR 28

testVector2 :: CE.TestCase PPCState PPC.Instruction
testVector2 = testVector1 { CE.testNonce = 22
                          , CE.testContext = smod -- ctx2
                          , CE.testProgram = [i]
                          }
  where
    -- ctx0 = V.zeroState (Proxy @PPC)
    -- ctx1 = V.pokeMS ctx0 v2 (V.ValueBV (W.W 1))
    -- ctx2 = V.pokeMS ctx1 v3 (V.ValueBV (W.W 5))
    i = PPC.Instruction PPC.ADD4 (PPC.Gprc r28 PPC.:< PPC.Gprc r17 PPC.:< PPC.Gprc r25 PPC.:< PPC.Nil)
