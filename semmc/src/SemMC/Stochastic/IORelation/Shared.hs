{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SemMC.Stochastic.IORelation.Shared (
  makeTestCase,
  wrapTestBundle,
  withTestResults,
  withGeneratedValueForLocation,
  instructionRegisterOperands,
  testCaseLocations,
  IndexedSemanticView(..)
  ) where

import qualified Control.Monad.Catch as E
import           Control.Monad.Trans ( liftIO )
import           Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr ( withKnownNat )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.ShapedList as SL
import qualified Lang.Crucible.BaseTypes as S

import qualified Dismantle.Arbitrary as DA

import qualified SemMC.Architecture as A
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Concrete.Execution as CE
import           SemMC.Stochastic.IORelation.Types

withTestResults :: forall a f arch sh
                 . (A.Architecture arch)
                => A.Opcode arch (A.Operand arch) sh
                -> [TestBundle (TestCase arch) f]
                -> (CE.ResultIndex (CS.ConcreteState arch) -> Learning arch a)
                -> Learning arch a
withTestResults op tests k = do
  tchan <- askTestChan
  rchan <- askResultChan
  let remoteTestCases = map tbTestBase tests ++ concatMap tbTestCases tests
  mresults <- timeout $ CE.withTestResults tchan rchan remoteTestCases return
  case mresults of
    Nothing -> liftIO $ E.throwM (LearningTimeout (Proxy @arch) (Some op))
    Just results -> k (CE.indexResults results)

-- | Given a bundle of tests, wrap all of the contained raw test cases with nonces.
wrapTestBundle :: (A.Architecture arch)
               => A.Instruction arch
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
-- raw tests to 'CE.TestCase' by allocating a nonce
makeTestCase :: (A.Architecture arch)
             => A.Instruction arch
             -> CS.ConcreteState arch
             -> Learning arch (TestCase arch)
makeTestCase i c = do
  tid <- nextNonce
  return CE.TestCase { CE.testNonce = tid
                     , CE.testContext = c
                     , CE.testProgram = [i]
                     }

-- | A view of a location, indexed by its position in the operand list
data IndexedSemanticView arch sh where
  IndexedSemanticView :: SL.Index sh tp -> CS.SemanticView arch -> IndexedSemanticView arch sh

instructionRegisterOperands :: forall arch sh proxy
                             . (CS.ConcreteArchitecture arch)
                            => proxy arch
                            -> SL.ShapedList (A.Operand arch) sh
                            -> [IndexedSemanticView arch sh]
instructionRegisterOperands proxy operands =
  SL.foldrFCIndexed collectLocations [] operands
  where
    collectLocations :: forall tp
                      . SL.Index sh tp
                     -> A.Operand arch tp
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
    getKeys :: forall a s . A.Location arch s -> a s -> [Some (CS.View arch)] -> [Some (CS.View arch)]
    getKeys k _ acc = CS.someTrivialView proxy (Some k) : acc

withGeneratedValueForLocation :: forall arch n a
                               . (A.Architecture arch)
                              => CS.View arch n
                              -> (CS.Value (S.BaseBVType n) -> a)
                              -> Learning arch a
withGeneratedValueForLocation loc k = do
  g <- askGen
  withKnownNat (CS.viewTypeRepr loc) $ do
    randomBV <- liftIO (DA.arbitrary g)
    return (k randomBV)
