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
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( liftIO )
import qualified Data.Map.Strict as M
import Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )
import qualified Lang.Crucible.BaseTypes as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D

import SemMC.Architecture
import SemMC.Stochastic.Monad ( Sym )
import qualified SemMC.Stochastic.Remote as R
import SemMC.Stochastic.IORelation.Types

withTestResults :: forall a f t arch sh
                 . (Architecture arch)
                => Opcode arch (Operand arch) sh
                -> [TestBundle (R.TestCase (ArchState (Sym t) arch)) f]
                -> ([R.ResultOrError (ArchState (Sym t) arch)] -> M t arch a)
                -> M t arch a
withTestResults op tests k = do
  tchan <- St.gets testChan
  rchan <- St.gets resChan
  let remoteTestCases = concatMap tbTestCases tests
  liftIO $ mapM_ (C.writeChan tchan . Just) remoteTestCases
  mresults <- timeout $ replicateM (length remoteTestCases) (C.readChan rchan)
  case mresults of
    Nothing -> liftIO $ E.throwM (LearningTimeout (Proxy :: Proxy arch) (Some op))
    Just results -> k results

-- | Given a bundle of tests, wrap all of the contained raw test cases with nonces.
wrapTestBundle :: (Architecture arch, R.MachineState (ArchState (Sym t) arch))
               => Instruction arch
               -> TestBundle (ArchState (Sym t) arch) f
               -> M t arch (TestBundle (R.TestCase (ArchState (Sym t) arch)) f)
wrapTestBundle i tb = do
  cases <- mapM (makeTestCase i) (tbTestCases tb)
  return TestBundle { tbTestCases = cases
                    , tbResult = tbResult tb
                    }

-- | Take a test bundle of raw tests ('ArchState (Sym t) arch') and convert the
-- raw tests to 'R.TestCase' by allocating a nonce
makeTestCase :: (Architecture arch, R.MachineState (ArchState (Sym t) arch))
             => Instruction arch
             -> ArchState (Sym t) arch
             -> M t arch (R.TestCase (ArchState (Sym t) arch))
makeTestCase i c = do
  tid <- St.gets nonce
  asm <- St.gets assemble
  St.modify' $ \s -> s { nonce = nonce s + 1 }
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
                  -> ArchState (Sym t) arch
                  -> [Some (Location arch)]
testCaseLocations _ = MapF.foldrWithKey getKeys []
  where
    getKeys k _ acc = Some k : acc

withGeneratedValueForLocation :: forall arch tp a t
                               . (Architecture arch)
                              => Location arch tp
                              -> (S.SymExpr (Sym t) tp -> a)
                              -> M t arch a
withGeneratedValueForLocation loc k = do
  sym <- St.gets backend
  g <- St.gets gen
  case locationType loc of
    S.BaseBVRepr w -> do
      randomInt :: Int
                <- liftIO (A.uniform g)
      bv <- liftIO $ S.bvLit sym w (fromIntegral randomInt)
      return (k bv)
    repr -> error ("Unsupported base type repr in withGeneratedValueForLocation: " ++ show repr)
