{-# LANGUAGE TypeOperators #-}
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
import           GHC.TypeLits (type (<=))

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr ( withKnownNat )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.List as SL
import qualified Lang.Crucible.BaseTypes as S

import qualified Dismantle.Arbitrary as DA

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE
import           SemMC.Stochastic.IORelation.Types

withTestResults :: forall f arch sh
                 . (A.Architecture arch)
                => A.Opcode arch (A.Operand arch) sh
                -> [TestBundle (TestCase arch) f]
                -> Learning arch (CE.ResultIndex (V.ConcreteState arch))
withTestResults op tests = do
  tchan <- askTestChan
  rchan <- askResultChan
  let remoteTestCases = map tbTestBase tests ++ concatMap tbTestCases tests
  mresults <- timeout $ CE.withTestResults tchan rchan remoteTestCases
  case mresults of
    Nothing -> liftIO $ E.throwM (LearningTimeout (Proxy @arch) (Some op))
    Just results -> return $ CE.indexResults results

-- | Given a bundle of tests, wrap all of the contained raw test cases with nonces.
wrapTestBundle :: (A.Architecture arch)
               => A.Instruction arch
               -> TestBundle (V.ConcreteState arch) f
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
             -> V.ConcreteState arch
             -> Learning arch (TestCase arch)
makeTestCase i c = do
  tid <- nextNonce
  return CE.TestCase { CE.testNonce = tid
                     , CE.testContext = c
                     , CE.testProgram = [i]
                     }

-- | A view of a location, indexed by its position in the operand list
data IndexedSemanticView arch sh where
  IndexedSemanticView :: SL.Index sh tp -> V.SemanticView arch -> IndexedSemanticView arch sh

instructionRegisterOperands :: forall arch sh proxy
                             . (AC.ConcreteArchitecture arch)
                            => proxy arch
                            -> SL.List (A.Operand arch) sh
                            -> [IndexedSemanticView arch sh]
instructionRegisterOperands proxy operands =
  SL.ifoldr collectLocations [] operands
  where
    collectLocations :: forall tp
                      . SL.Index sh tp
                     -> A.Operand arch tp
                     -> [IndexedSemanticView arch sh]
                     -> [IndexedSemanticView arch sh]
    collectLocations ix operand acc =
      case AC.operandToSemanticView proxy operand of
        Just v -> IndexedSemanticView ix v : acc
        Nothing -> acc

-- | Return all of the locations referenced in the architecture state
testCaseLocations :: forall proxy arch
                   . (AC.ConcreteArchitecture arch)
                  => proxy arch
                  -> V.ConcreteState arch
                  -> [Some (V.View arch)]
testCaseLocations proxy = MapF.foldrWithKey getKeys []
  where
    getKeys :: forall a s . A.Location arch s -> a s -> [Some (V.View arch)] -> [Some (V.View arch)]
    getKeys k _ acc = V.someTrivialView proxy (Some k) : acc

withGeneratedValueForLocation :: forall arch n a .
                                 (1 <= n, A.Architecture arch)
                              => V.View arch n
                              -> (V.Value (S.BaseBVType n) -> a)
                              -> Learning arch a
withGeneratedValueForLocation loc k = do
  g <- askGen
  withKnownNat (V.viewTypeRepr loc) $ do
    randomBV <- liftIO (DA.arbitrary g)
    return (k randomBV)
