{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ParamFormulaTests where

import qualified Control.Monad.Catch as E
import           Control.Monad.IO.Class ( liftIO )
import           Data.Maybe
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           Hedgehog
import           Hedgehog.Internal.Property ( forAllT )
import           HedgehogUtil ( )
import qualified Lang.Crucible.Backend.Online as CBO
import           Lang.Crucible.Backend.Simple ( newSimpleBackend )
import qualified SemMC.BoundVar as BV
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Formula.Parser as FI
import qualified SemMC.Formula.Printer as FO
import qualified SemMC.Log as Log
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           TestArch
import           TestArchPropGen
import           TestUtils
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
    , testProperty "parameter type multiple" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend r
                    p <- forAllT (genParameterizedFormula @'["Foo", "Bar"] sym)
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

    , testProperty "serialized formula round trip, simple backend" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend r
                    p <- forAllT (genParameterizedFormula @'["Bar"] sym)
                    debugPrint $ "parameterizedFormula: " <> show p
                    debugPrint $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
                    debugPrint $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
                    let printedFormula = FO.printParameterizedFormula opWaveShape p
                    debugPrint $ "printedFormula: " <> show printedFormula
                    let fenv = undefined
                    lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
                    reForm <- liftIO $
                              Log.withLogCfg lcfg $
                              FI.readFormula sym fenv opWaveShape printedFormula
                    debugPrint $ "re-Formulized: " <> show reForm
                    f <- evalEither reForm
                    compareParameterizedFormulasSimply sym 1 p f

    , testProperty "serialized formula round trip, online backend" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend @(CBO.Flags CBO.FloatReal) r CBO.NoUnsatFeatures $ \sym -> do
          -- generate a formula
          p <- forAllT (genParameterizedFormula @'["Bar"] sym)
          -- ensure that formula compares as equivalent to itself
          compareParameterizedFormulasSymbolically sym opWaveShape 1 p p
          -- now print the formula to a text string
          debugPrint $ "parameterizedFormula: " <> show p
          debugPrint $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
          debugPrint $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
          let printedFormula = FO.printParameterizedFormula opWaveShape p
          debugPrint $ "printedFormula: " <> show printedFormula
          -- convert the printed text string back into a formula
          let fenv = undefined
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv opWaveShape printedFormula
          debugPrint $ "re-Formulized: " <> show reForm
          f <- evalEither reForm
          -- verify the recreated formula matches the original
          compareParameterizedFormulasSymbolically sym opWaveShape 1 p f

    , testProperty "serialized formula double round trip" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend @(CBO.Flags CBO.FloatReal) r CBO.NoUnsatFeatures $ \sym -> do
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"

          p <- forAllT (genParameterizedFormula @'["Bar"] sym)

          -- first round trip:
          let printedFormula = FO.printParameterizedFormula opWaveShape p  -- KWQ: opWaveShape?!
          let fenv = undefined
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv opWaveShape printedFormula
          f <- evalEither reForm

          -- second round trip:
          let printedFormula' = FO.printParameterizedFormula opWaveShape f
          reForm' <- liftIO $
                     Log.withLogCfg lcfg $
                     FI.readFormula sym fenv opWaveShape printedFormula'
          f' <- evalEither reForm'

          -- verification of results
          -- KWQ: is variable renaming OK as long as the renaming is consistent and non-overlapping?
          compareParameterizedFormulasSymbolically sym opWaveShape 1 p f
          compareParameterizedFormulasSymbolically sym opWaveShape 1 f f'
          compareParameterizedFormulasSymbolically sym opWaveShape 2 p f'


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
            Nothing ->
              let aBV32 = BaseBVRepr knownNat :: BaseTypeRepr (BaseBVType 32) in
              case testEquality (SF.paramType param) aBV32 of
                Just Refl -> True
                Nothing -> False
