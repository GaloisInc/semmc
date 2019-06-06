{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ParamFormulaTests where

import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import qualified Data.Parameterized.List as SL
import           Test.Tasty
import           Test.Tasty.Hedgehog
import qualified SemMC.Formula.Formula as F
import           TestArchPropGen
import           What4.BaseTypes
import           Hedgehog

import           Prelude


parameterizedFormulaTests :: [TestTree]
parameterizedFormulaTests = [
  testGroup "Parameterized Formulas" $

    [ testProperty "parameter type" $
      property $ do p <- forAll (genParameterizedFormula @'["NatArg:Foo"] TestSymbolicBackend)
                    assert (all isValidParamType (F.pfUses p))
    , testProperty "operand type" $
      property $ do p <- forAll (genParameterizedFormula @'["NatArg:Foo"] TestSymbolicBackend)
                    assert $ isNatArgFoo ((F.pfOperandVars p) SL.!! SL.index0)
      -- TBD: needs other tests
    ]
  ]
  where
    isNatArgFoo :: TestOperand "NatArg:Foo" -> Bool
    isNatArgFoo _ = True
    -- isValidParamType :: forall arch (m :: * -> *) (sh :: [ghc-prim-0.5.3:GHC.Types.Symbol]).
    --                     (L.IsLocation (L.Location arch), MonadTest m) =>
    --                     Some (F.Parameter arch sh) -> m ()
    isValidParamType (Some param) =
      case testEquality (F.paramType param) BaseNatRepr of
        Just Refl -> True
        Nothing ->
          case testEquality (F.paramType param) BaseIntegerRepr of
            Just Refl -> True
            Nothing -> False
