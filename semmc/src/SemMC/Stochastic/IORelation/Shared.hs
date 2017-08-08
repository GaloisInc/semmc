{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.IORelation.Shared (
  makeTestCase,
  wrapTestBundle,
  withTestResults,
  indexResults,
  withGeneratedValueForLocation,
  instructionRegisterOperands,
  testCaseLocations,
  IndexedSemanticView(..)
  ) where

import qualified Control.Concurrent as C
import Control.Monad ( replicateM )
import qualified Control.Monad.Catch as E
import Control.Monad.Trans ( liftIO )
import qualified Data.Map.Strict as M
import Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.NatRepr ( withKnownNat )
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.ShapedList ( foldrFCIndexed, Index, ShapedList )
import qualified Lang.Crucible.BaseTypes as S

import qualified Dismantle.Arbitrary as A

import SemMC.Architecture
import qualified SemMC.ConcreteState as CS

import qualified SemMC.Stochastic.Remote as R
import SemMC.Stochastic.IORelation.Types

withTestResults :: forall a f arch sh
                 . (Architecture arch)
                => Opcode arch (Operand arch) sh
                -> [TestBundle (TestCase arch) f]
                -> ([R.ResultOrError (CS.ConcreteState arch)] -> Learning arch a)
                -> Learning arch a
withTestResults op tests k = do
  tchan <- askTestChan
  rchan <- askResultChan
  let remoteTestCases = map tbTestBase tests ++ concatMap tbTestCases tests
  liftIO $ mapM_ (C.writeChan tchan . Just) remoteTestCases
  mresults <- timeout $ replicateM (length remoteTestCases) (C.readChan rchan)
  case mresults of
    Nothing -> liftIO $ E.throwM (LearningTimeout (Proxy :: Proxy arch) (Some op))
    Just results -> k results

-- | Given a bundle of tests, wrap all of the contained raw test cases with nonces.
wrapTestBundle :: (Architecture arch)
               => Instruction arch
               -> TestBundle (CS.ConcreteState arch) f
               -> Learning arch (TestBundle (TestCase arch) f)
wrapTestBundle i tb = do
  cases <- mapM (makeTestCase i) (tbTestCases tb)
  base <- makeTestCase i (tbTestBase tb)
  return TestBundle { tbTestCases = cases
                    , tbTestBase = base
                    , tbResult = tbResult tb
                    }

-- | Take a test bundle of raw tests ('ConcreteState (Sym t) arch') and convert the
-- raw tests to 'R.TestCase' by allocating a nonce
makeTestCase :: (Architecture arch)
             => Instruction arch
             -> CS.ConcreteState arch
             -> Learning arch (TestCase arch)
makeTestCase i c = do
  tid <- nextNonce
  return R.TestCase { R.testNonce = tid
                    , R.testContext = c
                    , R.testProgram = [i]
                    }

indexResults :: ResultIndex a -> R.ResultOrError a -> ResultIndex a
indexResults ri res =
  case res of
    R.TestReadError {} -> ri
    R.TestSignalError trNonce trSignum ->
      ri { riExitedWithSignal = M.insert trNonce trSignum (riExitedWithSignal ri) }
    R.TestContextParseFailure -> ri
    R.InvalidTag {} -> ri
    R.TestSuccess tr ->
      ri { riSuccesses = M.insert (R.resultNonce tr) tr (riSuccesses ri) }

-- | A view of a location, indexed by its position in the operand list
data IndexedSemanticView arch sh where
  IndexedSemanticView :: Index sh tp -> CS.SemanticView arch -> IndexedSemanticView arch sh

instructionRegisterOperands :: forall arch sh proxy
                             . (CS.ConcreteArchitecture arch)
                            => proxy arch
                            -> ShapedList (Operand arch) sh
                            -> [IndexedSemanticView arch sh]
instructionRegisterOperands proxy operands =
  foldrFCIndexed collectLocations [] operands
  where
    collectLocations :: forall tp . Index sh tp
                     -> Operand arch tp
                     -> [IndexedSemanticView arch sh]
                     -> [IndexedSemanticView arch sh]
    collectLocations ix operand acc =
      case CS.operandToSemanticView proxy operand of
        Just v -> IndexedSemanticView ix v : acc
        Nothing -> acc

-- | Return all of the locations referenced in the architecture state
testCaseLocations :: forall proxy arch
                   . (CS.ConcreteArchitecture arch)
                  => proxy arch
                  -> CS.ConcreteState arch
                  -> [Some (CS.View arch)]
testCaseLocations proxy = MapF.foldrWithKey getKeys []
  where
    getKeys :: forall a s . Location arch s -> a s -> [Some (CS.View arch)] -> [Some (CS.View arch)]
    getKeys k _ acc = CS.someTrivialView proxy (Some k) : acc

withGeneratedValueForLocation :: forall arch n a
                               . (Architecture arch)
                              => CS.View arch n
                              -> (CS.Value (S.BaseBVType n) -> a)
                              -> Learning arch a
withGeneratedValueForLocation loc k = do
  g <- askGen
  withKnownNat (CS.viewTypeRepr loc) $ do
    randomBV <- liftIO (A.arbitrary g)
    return (k randomBV)
