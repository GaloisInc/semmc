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
  PairF(..)
  ) where

import qualified Control.Concurrent as C
import Control.Monad ( replicateM )
import qualified Control.Monad.Catch as E
import Control.Monad.Trans ( liftIO )
import qualified Data.Map.Strict as M
import Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.NatRepr ( NatRepr, withKnownNat )
import Data.Parameterized.Some ( Some(..) )
import qualified Lang.Crucible.BaseTypes as S

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D

import SemMC.Architecture
import SemMC.ConcreteState ( ConcreteState, Value(..) )

import qualified SemMC.Stochastic.Remote as R
import SemMC.Stochastic.IORelation.Types

withTestResults :: forall a f arch sh
                 . (Architecture arch)
                => Opcode arch (Operand arch) sh
                -> [TestBundle (R.TestCase (ConcreteState arch)) f]
                -> ([R.ResultOrError (ConcreteState arch)] -> Learning arch a)
                -> Learning arch a
withTestResults op tests k = do
  tchan <- askTestChan
  rchan <- askResultChan
  let remoteTestCases = concatMap tbTestCases tests
  liftIO $ mapM_ (C.writeChan tchan . Just) remoteTestCases
  mresults <- timeout $ replicateM (length remoteTestCases) (C.readChan rchan)
  case mresults of
    Nothing -> liftIO $ E.throwM (LearningTimeout (Proxy :: Proxy arch) (Some op))
    Just results -> k results

-- | Given a bundle of tests, wrap all of the contained raw test cases with nonces.
wrapTestBundle :: (Architecture arch)
               => Instruction arch
               -> TestBundle (ConcreteState arch) f
               -> Learning arch (TestBundle (R.TestCase (ConcreteState arch)) f)
wrapTestBundle i tb = do
  cases <- mapM (makeTestCase i) (tbTestCases tb)
  return TestBundle { tbTestCases = cases
                    , tbResult = tbResult tb
                    }

-- | Take a test bundle of raw tests ('ConcreteState (Sym t) arch') and convert the
-- raw tests to 'R.TestCase' by allocating a nonce
makeTestCase :: (Architecture arch)
             => Instruction arch
             -> ConcreteState arch
             -> Learning arch (R.TestCase (ConcreteState arch))
makeTestCase i c = do
  tid <- nextNonce
  asm <- askAssembler
  return R.TestCase { R.testNonce = tid
                    , R.testContext = c
                    , R.testProgram = asm i
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

data PairF a b tp = PairF (a tp) (b tp)

instructionRegisterOperands :: forall arch sh proxy
                             . (Architecture arch)
                            => proxy arch
                            -> D.OperandList (Operand arch) sh
                            -> [Some (PairF (D.Index sh) (TypedLocation arch))]
instructionRegisterOperands proxy operands =
  D.foldrOperandList collectLocations [] operands
  where
    collectLocations :: forall tp . D.Index sh tp
                     -> Operand arch tp
                     -> [Some (PairF (D.Index sh) (TypedLocation arch))]
                     -> [Some (PairF (D.Index sh) (TypedLocation arch))]
    collectLocations ix operand acc =
      case operandToLocation proxy operand of
        Just loc -> Some (PairF ix (TL loc)) : acc
        Nothing -> acc

-- | Return all of the locations referenced in the architecture state
testCaseLocations :: (Architecture arch)
                  => proxy arch
                  -> ConcreteState arch
                  -> [Some (Location arch)]
testCaseLocations _ = MapF.foldrWithKey getKeys []
  where
    getKeys k _ acc = Some k : acc

withGeneratedValueForLocation :: forall arch tp a
                               . (Architecture arch)
                              => Location arch tp
                              -> (Value tp -> a)
                              -> Learning arch a
withGeneratedValueForLocation loc k = do
  g <- askGen
  case locationType loc of
    S.BaseBVRepr (wVal :: NatRepr w) -> withKnownNat wVal $ do
      randomBV :: Value (S.BaseBVType w)
                <- liftIO (A.arbitrary g)
      return (k randomBV)
    repr -> error ("Unsupported base type repr in withGeneratedValueForLocation: " ++ show repr)
