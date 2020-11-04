{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ParamFormulaTests where

import           Control.Monad ( join, void )
import qualified Control.Monad.Catch as E
import           Control.Monad.IO.Class ( liftIO )
import           Data.Maybe
import           Data.Parameterized.Classes
import qualified Data.Parameterized.HasRepr as HR
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some
import qualified Data.Set as Set
import qualified Data.Text as T
import           Hedgehog
import           Hedgehog.Internal.Property ( forAllT )
import           HedgehogUtil ( )
import qualified Lang.Crucible.Backend.Online as CBO
import           Lang.Crucible.Backend.Simple ( newSimpleBackend, FloatModeRepr(..) )
import qualified SemMC.BoundVar as BV
import           SemMC.DSL ( defineOpcode, comment, input, defLoc, param, ite, uf
                           , bvadd, bvmul, bvshl, bvnot
                           , runSem, printDefinition
                           , ExprTypeRepr(..), Literal(..), Expr(..)
                           , Location(..), Package(..) )
import qualified SemMC.Formula.Formula as SF
import qualified SemMC.Formula.Parser as FI
import qualified SemMC.Formula.Printer as FO
import qualified SemMC.Log as Log
import           Test.Tasty
import           Test.Tasty.HUnit ( assertEqual, testCase, (@?=) )
import           Test.Tasty.Hedgehog
import           TestArch
import           TestArchPropGen
import           TestUtils
import           What4.BaseTypes
import           What4.Config
import qualified What4.Interface as WI -- ( getConfiguration )
import qualified What4.ProblemFeatures as WPF
import qualified What4.Serialize.Parser as W4P


import           Prelude


parameterizedFormulaTests :: [TestTree]
parameterizedFormulaTests = [
  testGroup "Parameterized Formulas" $
    testBasicParameters
    <> testRoundTripPrintParse
  ]


testBasicParameters :: [TestTree]
testBasicParameters =
    [ testProperty "parameter type" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    (p, _operands, _trace) <- forAllT (genParameterizedFormula sym OpSurf)
                    assert (all isValidParamType (SF.pfUses p))
    , testProperty "parameter type multiple" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    (p, _operands, _trace) <- forAllT (genParameterizedFormula sym OpPack)
                    assert (all isValidParamType (SF.pfUses p))
    , testProperty "operand type" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    (p, _operands, _trace) <- forAllT (genParameterizedFormula sym OpSurf)
                    assert $ isNatArgFoo ((SF.pfOperandVars p) SL.!! SL.index0)
    , testProperty "literal vars" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    _ <- forAllT (genParameterizedFormula sym OpSurf)
                    success -- TBD: something (manything?) to test literal vars here
      -- TBD: needs other tests
    , testProperty "defs keys in uses" $
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    (p, _operands, _trace) <- forAllT (genParameterizedFormula sym OpSurf)
                    assert (all (flip Set.member (SF.pfUses p)) (MapF.keys $ SF.pfDefs p))
    ]
  where
    isNatArgFoo :: BV.BoundVar sym TestGenArch "Foo" -> Bool
    isNatArgFoo _ = True
    isValidParamType (Some parameter) =
      case testEquality (SF.paramType parameter) BaseNatRepr of
        Just Refl -> True
        Nothing ->
          case testEquality (SF.paramType parameter) BaseIntegerRepr of
            Just Refl -> True
            Nothing ->
              let aBV32 = BaseBVRepr knownNat :: BaseTypeRepr (BaseBVType 32) in
              case testEquality (SF.paramType parameter) aBV32 of
                Just Refl -> True
                Nothing -> False


testRoundTripPrintParse :: [TestTree]
testRoundTripPrintParse =
  [
    testProperty "ser/des round trip, simple backend, OpPack" $
      withTests 500 $  -- default is 100 tests, but formulas have lots of options, so get more
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    let opcode = OpPack
                    (p, _operands, trace) <- forAllT (genParameterizedFormula sym opcode)
                    debugOut "TEST: ser/des round trip, simple backend, OpPack"
                    debugOut $ "trace: " <> show trace
                    debugOut $ "parameterizedFormula: " <> show p
                    debugOut $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
                    debugOut $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
                    let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
                    debugOut $ "printedFormula:\n" <> (T.unpack printedFormula)
                    fenv <- testFormulaEnv sym
                    lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
                    reForm <- liftIO $
                              Log.withLogCfg lcfg $
                              FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
                    debugOut $ "re-Formulized: " <> show reForm
                    f <- evalEither reForm
                    compareParameterizedFormulasSimply sym 1 p f

    , testProperty "ser/des round trip, simple backend, OpWave" $
      withTests 500 $  -- default is 100 tests, but formulas have lots of options, so get more
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    let opcode = OpWave
                    (p, _operands, trace) <- forAllT (genParameterizedFormula sym opcode)
                    debugOut "TEST: ser/des round trip, simple backend, OpWave"
                    debugOut $ "trace: " <> show trace
                    debugOut $ "parameterizedFormula: " <> show p
                    debugOut $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
                    debugOut $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
                    let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
                    debugOut $ "printedFormula:\n" <> (T.unpack printedFormula)
                    fenv <- testFormulaEnv sym
                    lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
                    reForm <- liftIO $
                              Log.withLogCfg lcfg $
                              FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
                    debugOut $ "re-Formulized: " <> show reForm
                    f <- evalEither reForm
                    compareParameterizedFormulasSimply sym 1 p f

    , testProperty "ser/des round trip, simple backend, OpSolo" $
      withTests 500 $  -- default is 100 tests, but formulas have lots of options, so get more
      property $ do Some r <- liftIO newIONonceGenerator
                    sym <- liftIO $ newSimpleBackend FloatRealRepr r
                    let opcode = OpSolo
                    (p, _operands, trace) <- forAllT (genParameterizedFormula sym opcode)
                    debugOut "TEST: ser/des round trip, simple backend, OpSolo"
                    debugOut $ "trace: " <> show trace
                    debugOut $ "parameterizedFormula: " <> show p
                    debugOut $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
                    debugOut $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
                    let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
                    debugOut $ "printedFormula:\n" <> (T.unpack printedFormula)
                    fenv <- testFormulaEnv sym
                    lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
                    reForm <- liftIO $
                              Log.withLogCfg lcfg $
                              FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
                    debugOut $ "re-Formulized: " <> show reForm
                    f <- evalEither reForm
                    compareParameterizedFormulasSimply sym 1 p f

    , testProperty "ser/des round trip, online backend, OpWave" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend CBO.FloatRealRepr r CBO.NoUnsatFeatures WPF.noFeatures $ \sym -> do
          void $ liftIO $ join (setOpt
                                <$> getOptionSetting enable_mcsat (WI.getConfiguration sym)
                                <*> pure False)
          -- generate a formula
          let opcode = OpWave
          (p, operands, trace) <- forAllT (genParameterizedFormula sym opcode)
          -- ensure that formula compares as equivalent to itself
          compareParameterizedFormulasSymbolically sym operands 1 p p
          -- now print the formula to a text string
          debugOut "TEST: ser/des round trip, online backend, OpWave"
          debugOut $ "trace: " <> show trace
          debugOut $ "parameterizedFormula: " <> show p
          debugOut $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
          debugOut $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
          let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
          debugOut $ "printedFormula:\n" <> (T.unpack printedFormula)
          -- convert the printed text string back into a formula
          fenv <- testFormulaEnv sym
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
          debugOut $ "re-Formulized: " <> show reForm
          f <- evalEither reForm
          -- verify the recreated formula matches the original
          compareParameterizedFormulasSymbolically sym operands 1 p f

    , testProperty "ser/des round trip, online backend, OpPack" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend CBO.FloatRealRepr r CBO.NoUnsatFeatures WPF.noFeatures $ \sym -> do
          void $ liftIO $ join (setOpt
                                <$> getOptionSetting enable_mcsat (WI.getConfiguration sym)
                                <*> pure False)
          -- generate a formula
          let opcode = OpPack
          (p, operands, trace) <- forAllT (genParameterizedFormula sym opcode)
          -- ensure that formula compares as equivalent to itself
          compareParameterizedFormulasSymbolically sym operands 1 p p
          -- now print the formula to a text string
          debugOut "TEST: ser/des round trip, online backend, OpPack"
          debugOut $ "trace: " <> show trace
          debugOut $ "parameterizedFormula: " <> show p
          debugOut $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
          debugOut $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
          let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
          debugOut $ "printedFormula:\n" <> (T.unpack printedFormula)
          -- convert the printed text string back into a formula
          fenv <- testFormulaEnv sym
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
          debugOut $ "re-Formulized: " <> show reForm
          f <- evalEither reForm
          -- verify the recreated formula matches the original
          compareParameterizedFormulasSymbolically sym operands 1 p f

    , testProperty "ser/des round trip, online backend, OpSolo" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend CBO.FloatRealRepr r CBO.NoUnsatFeatures WPF.noFeatures $ \sym -> do
          void $ liftIO $ join (setOpt
                                <$> getOptionSetting enable_mcsat (WI.getConfiguration sym)
                                <*> pure False)
          -- generate a formula
          let opcode = OpSolo
          (p, operands, trace) <- forAllT (genParameterizedFormula sym opcode)

          -- ensure that formula compares as equivalent to itself
          compareParameterizedFormulasSymbolically sym operands 1 p p
          -- now print the formula to a text string
          debugOut "TEST: ser/des round trip, online backend, OpSolo"
          debugOut $ "trace: " <> show trace
          debugOut $ "parameterizedFormula: " <> show p
          debugOut $ "# literalVars: " <> show (MapF.size $ SF.pfLiteralVars p)
          debugOut $ "# defs: " <> show (MapF.size $ SF.pfDefs p)
          let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
          debugOut $ "printedFormula:\n" <> (T.unpack printedFormula)
          -- convert the printed text string back into a formula
          fenv <- testFormulaEnv sym
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
          debugOut $ "re-Formulized: " <> show reForm
          f <- evalEither reForm
          -- verify the recreated formula matches the original
          compareParameterizedFormulasSymbolically sym operands 1 p f

    , testProperty "ser/des double round trip, OpWave" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend CBO.FloatRealRepr r CBO.NoUnsatFeatures WPF.noFeatures $ \sym -> do
          void $ liftIO $ join (setOpt
                                <$> getOptionSetting enable_mcsat (WI.getConfiguration sym)
                                <*> pure False)
          debugOut "TEST: ser/des double round trip, OpWave"
          -- generate a formula
          let opcode = OpWave
          (p, operands, _trace) <- forAllT (genParameterizedFormula sym opcode)

          -- first round trip:
          let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
          debugOut $ "printedFormula round 1:\n" <> (T.unpack printedFormula)
          fenv <- testFormulaEnv sym
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
          f <- evalEither reForm

          -- second round trip:
          let printedFormula' = FO.printParameterizedFormula (HR.typeRepr opcode) f
          debugOut $ "printedFormula round 2:\n" <> (T.unpack printedFormula)
          reForm' <- liftIO $
                     Log.withLogCfg lcfg $
                     FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula'
          f' <- evalEither reForm'

          -- verification of results
          compareParameterizedFormulasSymbolically sym operands 1 p f
          compareParameterizedFormulasSymbolically sym operands 1 f f'
          -- KWQ: is variable renaming OK as long as the renaming is consistent and non-overlapping?
          compareParameterizedFormulasSymbolically sym operands 2 p f'

    , testProperty "ser/des double round trip, OpPack" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend CBO.FloatRealRepr r CBO.NoUnsatFeatures WPF.noFeatures $ \sym -> do
          void $ liftIO $ join (setOpt
                                <$> getOptionSetting enable_mcsat (WI.getConfiguration sym)
                                <*> pure False)
          debugOut "TEST: ser/des double round trip, OpPack"
          let opcode = OpPack
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"

          (p, operands, _trace) <- forAllT (genParameterizedFormula sym opcode)

          -- first round trip:
          let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
          debugOut $ "printedFormula round 1:\n" <> (T.unpack printedFormula)
          fenv <- testFormulaEnv sym
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
          f <- evalEither reForm

          -- second round trip:
          let printedFormula' = FO.printParameterizedFormula (HR.typeRepr opcode) f
          debugOut $ "printedFormula round 2:\n" <> (T.unpack printedFormula)
          reForm' <- liftIO $
                     Log.withLogCfg lcfg $
                     FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula'
          f' <- evalEither reForm'

          -- verification of results
          compareParameterizedFormulasSymbolically sym operands 1 p f
          compareParameterizedFormulasSymbolically sym operands 1 f f'
          compareParameterizedFormulasSymbolically sym operands 2 p f'

    , testProperty "ser/des double round trip, OpSolo" $
      property $
      E.handleAll (\e -> annotate (show e) >> failure) $ do
        Some r <- liftIO newIONonceGenerator
        CBO.withYicesOnlineBackend CBO.FloatRealRepr r CBO.NoUnsatFeatures WPF.noFeatures $ \sym -> do
          void $ liftIO $ join (setOpt
                                <$> getOptionSetting enable_mcsat (WI.getConfiguration sym)
                                <*> pure False)
          debugOut "TEST: ser/des double round trip, OpSolo"
          let opcode = OpSolo
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"

          (p, operands, _trace) <- forAllT (genParameterizedFormula sym opcode)

          -- first round trip:
          let printedFormula = FO.printParameterizedFormula (HR.typeRepr opcode) p
          debugOut $ "printedFormula round 1:\n" <> (T.unpack printedFormula)
          fenv <- testFormulaEnv sym
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula
          f <- evalEither reForm

          -- second round trip:
          let printedFormula' = FO.printParameterizedFormula (HR.typeRepr opcode) f
          debugOut $ "printedFormula round 2:\n" <> (T.unpack printedFormula)
          reForm' <- liftIO $
                     Log.withLogCfg lcfg $
                     FI.readFormula sym fenv (HR.typeRepr opcode) printedFormula'
          f' <- evalEither reForm'

          -- verification of results
          compareParameterizedFormulasSymbolically sym operands 1 p f
          compareParameterizedFormulasSymbolically sym operands 1 f f'
          compareParameterizedFormulasSymbolically sym operands 2 p f'

    , testGroup "DSL specified"
      [
        testCase "Trivial formula, OpSolo" $ do
          debugOut "TEST: DSL specified Trivial formula, OpSolo"
          let opcode = OpSolo
          -- Use the SemMC DSL to specify a Formula
          let opdef = defineOpcode (show opcode) $
                      do comment $ (show opcode) <> " (no arguments)"
              pkg = runSem opdef
          -- Verify the S-expression stored form of the Formula is as expected
          length (pkgFunctions pkg) @?= 0
          length (pkgFormulas pkg) @?= 1
          let (pkgN, pkgD) = head $ pkgFormulas pkg
          pkgN @?= show opcode
          let sexprTxt = printDefinition pkgD
          debugOut $ "printDefinition pkgD: " <> (T.unpack sexprTxt)
          sexprTxt @?= (T.strip $ T.pack $ unlines
                        [ ";; " <> show opcode <> " (no arguments)"
                        , "((operands ())"
                        , " (in ())"
                        , " (defs ()))"
                        ])
          -- verify that the expression can be parsed back into a Formula
          Some r <- liftIO newIONonceGenerator
          sym <- liftIO $ newSimpleBackend FloatRealRepr r
          fenv <- testFormulaEnv sym
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula @_ @TestGenArch sym fenv (HR.typeRepr opcode) sexprTxt
          debugOut $ "re-Formulized: " <> show reForm
          -- n.b. no actual validation of the proper semantics here,
          -- just that it had enough valid syntax to be parsed.
          case reForm of
            Right _ -> return ()
            Left e -> assertEqual (T.unpack $ "valid parse of " <> sexprTxt) "error" e

      , testCase "Small formula, OpSolo" $ do
          debugOut "TEST: DSL specified Small formula, OpSolo"
          let opcode = OpSolo
              foo = LiteralLoc Literal { lName = "Box_0", lExprType = EBV 32 }
              bar = LiteralLoc Literal { lName = "Bar",   lExprType = EBV 32 }
          -- Use the SemMC DSL to specify a Formula
          let opdef = defineOpcode (show opcode) $
                      do comment $ (show opcode) <> " (no operands, two locations)"
                         input foo
                         input bar
                         defLoc foo (bvadd (Loc bar) (LitBV 32 0x4a4a))
                         defLoc bar $ LitBV 32 0
              pkg = runSem opdef
          -- Verify the S-expression stored form of the Formula is as expected
          length (pkgFunctions pkg) @?= 0
          length (pkgFormulas pkg) @?= 1
          let (pkgN, pkgD) = head $ pkgFormulas pkg
          pkgN @?= show opcode
          let sexprTxt = printDefinition pkgD
          debugOut $ "printDefinition pkgD: " <> (T.unpack sexprTxt)
          let actual = either
                       (const "actual failed to parse")
                       (W4P.printSExpr mempty)
                       (W4P.parseSExpr sexprTxt)
              expected = either
                         (const "expected failed to parse")
                         (W4P.printSExpr mempty)
                         (W4P.parseSExpr
                          (T.strip $ T.pack $ unlines
                           [ ";; " <> show opcode <> " (no operands, two locations)"
                           , "((operands ())"
                           , " (in (loc.Bar loc.Box_0))"
                           , " (defs"
                           , "  ((loc.Bar (with () #x00000000))"
                           , "   (loc.Box_0"
                           , "    (with ()"
                           , "      (bvadd loc.Bar #x00004a4a))))))"
                           ]))
          actual @?= expected
          -- verify that the expression can be parsed back into a Formula
          Some r <- liftIO newIONonceGenerator
          sym <- liftIO $ newSimpleBackend FloatRealRepr r
          fenv <- testFormulaEnv sym
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula @_ @TestGenArch sym fenv (HR.typeRepr opcode) sexprTxt
          debugOut $ "re-Formulized: " <> show reForm
          -- n.b. no actual validation of the proper semantics here,
          -- just that it had enough valid syntax to be parsed.
          case reForm of
            Right _ -> return ()
            Left e -> assertEqual (T.unpack $ "valid parse of " <> sexprTxt) "error" e

      , testCase "Medium formula, OpPack" $ do
          debugOut "TEST: DSL specified Medium formula, OpSolo"
          let opcode = OpPack
              foo = LiteralLoc Literal { lName = "Box_0", lExprType = EBV 32 }
              bar = LiteralLoc Literal { lName = "Bar",   lExprType = EBV 32 }
          -- Use the SemMC DSL to specify a Formula
          let opdef = defineOpcode (show opcode) $
                      do comment $ (show opcode) <> " four operands, two locations"
                         box0 <- param "box__0" "Box" (EBV 32)
                         bar1 <- param "bar__1" "Bar" (EBV 32)
                         box2 <- param "box__2" "Box" (EBV 32)
                         box3 <- param "box__3" "Box" (EBV 32)
                         input foo
                         input bar
                         input box2
                         input bar1
                         let zero = LitBV 32 0
                             two  = LitBV 32 2
                             nineteen = LitBV 32 19
                             nine = LitBV 32 9
                             isBox3 = uf EBool "tst.isBox3" . ((:[]) . Some) . Loc
                         defLoc foo (bvadd (ite (isBox3 bar)
                                            (Loc bar)
                                            (LitBV 32 0xa4))
                                      (LitBV 32 0x4a4a))
                         defLoc bar zero
                         defLoc box0 (bvmul
                                      (bvadd (bvshl two (Loc box3)) nineteen)
                                      (bvnot nine))
              pkg = runSem opdef
          -- Verify the S-expression stored form of the Formula is as expected
          length (pkgFunctions pkg) @?= 0
          length (pkgFormulas pkg) @?= 1
          let (pkgN, pkgD) = head $ pkgFormulas pkg
          pkgN @?= show opcode
          let sexprTxt = printDefinition pkgD
          debugOut $ "printDefinition pkgD: " <> (T.unpack sexprTxt)
          let actual = either
                       (const "actual failed to parse")
                       (W4P.printSExpr mempty)
                       (W4P.parseSExpr sexprTxt)
              expected = either
                         (const "expected failed to parse")
                         (W4P.printSExpr mempty)
                         (W4P.parseSExpr
                          (T.strip $ T.pack $ unlines
                           [ ";; " <> show opcode <> " four operands, two locations"
                           , "((operands"
                           , "  ((box__0 Box)"
                           , "   (bar__1 Bar)"
                           , "   (box__2 Box)"
                           , "   (box__3 Box)))"
                           , " (in"
                           , "  (op.bar__1"
                           , "   op.box__2"
                           , "   loc.Bar"
                           , "   loc.Box_0))"
                           , " (defs"
                           , "  ((op.box__0"
                           , "    (with ()"
                           , "     (bvmul"
                           , "      (bvadd"
                           , "       (bvshl #x00000002 op.box__3)"
                           , "       #x00000013)"
                           , "      (bvnot #x00000009))))"
                           , "   (loc.Bar (with ()#x00000000))"
                           , "   (loc.Box_0"
                           , "    (with ()"
                           , "     (bvadd"
                           , "      (ite (call uf.tst.isBox3 loc.Bar)"
                           , "       loc.Bar"
                           , "       #x000000a4)"
                           , "      #x00004a4a))))))"
                           ]))
          actual @?= expected
          -- verify that the expression can be parsed back into a Formula
          Some r <- liftIO newIONonceGenerator
          sym <- liftIO $ newSimpleBackend FloatRealRepr r
          fenv <- testFormulaEnv sym
          lcfg <- liftIO $ Log.mkLogCfg "rndtrip"
          reForm <- liftIO $
                    Log.withLogCfg lcfg $
                    FI.readFormula @_ @TestGenArch sym fenv (HR.typeRepr opcode) sexprTxt
          debugOut $ "re-Formulized: " <> show reForm
          -- n.b. no actual validation of the proper semantics here,
          -- just that it had enough valid syntax to be parsed.
          case reForm of
            Right _ -> return ()
            Left e -> assertEqual (T.unpack $ "valid parse of " <> sexprTxt) "error" e

      ]
    ]


enable_mcsat :: ConfigOption BaseBoolType
enable_mcsat = configOption knownRepr "yices_enable-mcsat"
