{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ParamFormulaTests where

import           Control.Monad.IO.Class ( liftIO )
import           Data.Either
import qualified Data.Foldable as F
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Nonce
import qualified SemMC.Log as Log
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           Hedgehog
import           Hedgehog.Internal.Property ( forAllT )
import           Lang.Crucible.Backend.Simple ( newSimpleBackend )
import qualified SemMC.BoundVar as BV
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Formula.Parser as FI
import qualified SemMC.Formula.Printer as FO
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           TestArch
import           TestArchPropGen
import           What4.BaseTypes

import           Prelude


parameterizedFormulaTests :: [TestTree]
parameterizedFormulaTests = [
  testGroup "Parameterized Formulas" $

    [ testProperty "parameter type" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend r
                    p <- forAllT (genParameterizedFormula @'["Foo"] sym)
                    assert (all isValidParamType (SF.pfUses p))
    , testProperty "operand type" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend r
                    p <- forAllT (genParameterizedFormula @'["Foo"] sym)
                    assert $ isNatArgFoo ((SF.pfOperandVars p) SL.!! SL.index0)
    , testProperty "literal vars" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend r
                    p <- forAllT (genParameterizedFormula @'["Foo"] sym)
                    success -- TBD: something (manything?) to test literal vars here
      -- TBD: needs other tests
    , testProperty "defs keys in uses" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend r
                    p <- forAllT (genParameterizedFormula @'["Foo"] sym)
                    assert (all (flip Set.member (SF.pfUses p)) (MapF.keys $ SF.pfDefs p))

    , testProperty "serialized formula" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend r
                    p <- forAllT (genParameterizedFormula @'["Foo"] sym)
                    liftIO $ putStrLn $ "parameterizedFormula: " <> show p
                    liftIO $ putStrLn $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
                    liftIO $ putStrLn $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
                    let printedFormula = FO.printParameterizedFormula opWaveShape p
                    liftIO $ putStrLn $ "printedFormula: " <> show printedFormula
                    let fenv = undefined
                    lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
                    reForm <- liftIO $
                              Log.withLogCfg lcfg $
                              FI.readFormula sym fenv opWaveShape printedFormula
                    liftIO $ putStrLn $ "re-Formulized: " <> show reForm
                    case reForm of
                      Left e -> ("read got error for: " <> show printedFormula) === e
                      Right f -> do SF.pfUses p === SF.pfUses f
                                    -- SF.pfOperandVars p === SF.pfOperandVars f
                                    -- SF.pfLiteralVars p === SF.pfLiteralVars f
                                    -- SF.pfDefs p === SF.pfDefs f

    ]
  ]
  where
    isNatArgFoo :: BV.BoundVar sym TestGenArch "Foo" -> Bool
    isNatArgFoo _ = True
    -- isValidParamType :: forall arch (m :: * -> *) (sh :: [ghc-prim-0.5.3:GHC.Types.Symbol]).
    --                     (L.IsLocation (L.Location arch), MonadTest m) =>
    --                     Some (SF.Parameter arch sh) -> m ()
    isValidParamType (Some param) =
      case testEquality (SF.paramType param) BaseNatRepr of
        Just Refl -> True
        Nothing ->
          case testEquality (SF.paramType param) BaseIntegerRepr of
            Just Refl -> True
            Nothing -> False
