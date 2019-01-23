{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
-- | Definitions common to PPC32 and PPC64
module SemMC.Architecture.PPC.Shared (
  -- * Type reprs
  repr32,
  repr64,
  repr128,
  extendBV,
  withTruncI16Val,
  withTruncIVal,
  withTruncWVal,
  -- * Serialization
  serializeVec,
  serializeSymVal,
  HighBits(..),
  getWord128be,
  getValue,
  -- * Parsing
  Parser,
  tryOne,
  -- * Arch. agnostic
  parsePrefixedRegister,
  locationFuncInterpretation,
  concreteTemplatedOperand,
  symbolicTemplatedOperand,
  -- * 'OperandComponent' helper functions
  fromMaybeGPRLoc,
  fromMaybeGPRBase
  ) where

import           Data.Bits ( shiftR, shiftL, (.|.), (.&.) )
import qualified Data.ByteString.Builder as B
import           Data.Int ( Int16 )
import qualified Data.Int.Indexed as I
import qualified Data.List.NonEmpty as NEL
import           Data.Monoid ( (<>) )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy
import qualified Data.Serialize.Get as G
import qualified Data.Set as S
import           Data.Word ( Word16 )
import qualified Data.Word.Indexed as W
import qualified Dismantle.PPC as PPC
import           GHC.TypeLits
import           Numeric.Natural ( Natural )

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.PPC.Eval as E
import           SemMC.Architecture.PPC.Location
import           SemMC.Architecture.PPC.OperandComponents ( OperandComponents(..) )
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Eval as E
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import           What4.BaseTypes ( BaseBVType, NatRepr, knownNat, BaseTypeRepr(..) )
import qualified What4.Expr as S
import qualified What4.Interface as S

repr32 :: NatRepr 32
repr32 = knownNat

repr64 :: NatRepr 64
repr64 = knownNat

repr128 :: NatRepr 128
repr128 = knownNat

extendBV :: V.Value (BaseBVType 64) -> V.Value (BaseBVType 128)
extendBV (V.ValueBV (W.unW -> n)) = V.ValueBV (W.w n)

withTruncI16Val :: (KnownNat n)
                => V.Value (BaseBVType n)
                -> Word16
                -> (Int16 -> PPC.Operand s)
                -> (V.Value (BaseBVType n), PPC.Operand s)
withTruncI16Val (V.ValueBV w) mask con =
  let w' = W.unW w .&. fromIntegral mask
  in (V.ValueBV (W.w w'), con (fromIntegral w'))

withTruncIVal :: (KnownNat n)
             => V.Value (BaseBVType n)
             -> W.W n
             -> (I.I n' -> PPC.Operand s)
             -> (V.Value (BaseBVType n), PPC.Operand s)
withTruncIVal (V.ValueBV w) mask con =
  let w' = w .&. mask
  in (V.ValueBV w', con (I.I (fromIntegral (W.unW w'))))

withTruncWVal :: (KnownNat n, KnownNat n')
              => V.Value (BaseBVType n)
              -> W.W n
              -> (W.W n' -> PPC.Operand s)
              -> (V.Value (BaseBVType n), PPC.Operand s)
withTruncWVal (V.ValueBV w) mask con =
  let w' = w .&. mask
  in (V.ValueBV w', con (W.w (fromIntegral (W.unW w'))))

-- Serialization

-- | Serialize a 128 bit value into a bytestring
serializeVec :: Integer -> B.Builder
serializeVec i = B.word64BE w1 <> B.word64BE w2
  where
    w1 = fromInteger i
    w2 = fromInteger (i `shiftR` 64)

serializeSymVal :: (KnownNat n) => (Integer -> B.Builder) -> V.Value (BaseBVType n) -> B.Builder
serializeSymVal toBuilder sv =
  case sv of
    V.ValueBV (W.unW -> w) -> toBuilder (toInteger w)

data HighBits = IgnoreHighBits
              | KeepHighBits

getWord128be :: HighBits -> G.Get Natural
getWord128be hb = do
  w1 <- G.getWord64be
  w2 <- G.getWord64be
  case hb of
    IgnoreHighBits -> return (fromIntegral w1)
    KeepHighBits -> return ((fromIntegral w2 `shiftL` 64) .|. fromIntegral w1)

getValue :: (1 <= n, Integral w, KnownNat n)
         => G.Get w
         -> NatRepr n
         -> G.Get (V.Value (BaseBVType n))
getValue g _ = (V.ValueBV . W.w . fromIntegral) <$> g

type Parser = P.Parsec String String

tryOne :: [Parser a] -> Parser a
tryOne = P.choice . map P.try

parsePrefixedRegister :: (Integral a, Show a) => (a -> b) -> Char -> Parser b
parsePrefixedRegister f c = do
  _ <- P.char c
  n <- P.decimal
  case n >= 0 && n <= 31 of
    True -> return (f n)
    False -> P.failure (Just $ P.Tokens $ NEL.fromList $ show n)
                      (S.fromList $ [ P.Label $ NEL.fromList "Register number 0-31" ])

concreteTemplatedOperand :: forall arch s a.
                            (A.Architecture arch)
                         => (a -> A.Operand arch s)
                         -> (a -> A.Location arch (A.OperandType arch s))
                         -> a
                         -> T.TemplatedOperand arch s
concreteTemplatedOperand op loc x =
  T.TemplatedOperand { T.templOpLocation = Just (loc x)
                     , T.templUsedLocations = S.singleton (Some (loc x))
                     , T.templOpFn = mkTemplate'
                     }
  where mkTemplate' :: T.TemplatedOperandFn arch s
        mkTemplate' sym locLookup = do
          ao <- A.taggedOperand <$> A.allocateSymExprsForOperand (Proxy @arch) sym locLookup (op x)
          return (ao, T.RecoverOperandFn $ const (return (op x)))

symbolicTemplatedOperand :: forall arch s (bits :: Nat)
                          . (A.OperandType arch s ~ BaseBVType bits,
                             KnownNat bits,
                             1 <= bits)
                         => Proxy bits
                         -> Bool
                         -> String
                         -> (Integer -> A.Operand arch s)
                         -> T.TemplatedOperand arch s
symbolicTemplatedOperand Proxy _signed name constr =
  T.TemplatedOperand { T.templOpLocation = Nothing
                     , T.templUsedLocations = S.empty
                     , T.templOpFn = mkTemplate'
                     }
  where mkTemplate' :: T.TemplatedOperandFn arch s
        mkTemplate' sym _ = do
          v <- S.freshConstant sym (U.makeSymbol name) (knownRepr :: BaseTypeRepr (BaseBVType bits))
          let recover evalFn = constr <$> evalFn v
          return (A.ValueOperand v, T.RecoverOperandFn recover)

isR0 :: forall t st fs sh u tp arch sym
      . ( A.Location arch ~ Location arch
        , A.Operand arch ~ PPC.Operand
        , sym ~ S.ExprBuilder t st fs
        )
     => sym
     -> F.ParameterizedFormula sym arch sh
     -> PL.List (A.AllocatedOperand arch sym) sh
     -> Ctx.Assignment (S.Expr t) u
     -> (forall tp' . Location arch tp' -> IO (S.SymExpr sym tp'))
     -> BaseTypeRepr tp
     -> IO (S.Expr t tp)
isR0 sym pf ops actuals _locToExpr repr = do
  let typedReturn :: forall tp' . S.Expr t tp' -> IO (S.Expr t tp)
      typedReturn e =
        case testEquality (S.exprType e) repr of
          Nothing -> error ("The caller of isR0 expected type " ++ show repr ++ " but it returned a value of type " ++ show (S.exprType e))
          Just Refl -> return e
  case actuals of
    Ctx.Empty Ctx.:> S.BoundVarExpr ufArg ->
      case ufArg `E.lookupVarInFormulaOperandList` pf of
        Nothing ->
          case MapF.lookup r0 (F.pfLiteralVars pf) of
            Nothing -> typedReturn (S.falsePred sym)
            Just r0Var ->
              case testEquality r0Var ufArg of
                Nothing -> typedReturn (S.falsePred sym)
                Just Refl -> typedReturn (S.truePred sym)
        Just (Some idx) ->
          case ops PL.!! idx of
            A.LocationOperand loc _
              | Just Refl <- testEquality loc r0 -> typedReturn (S.truePred sym)
            _ -> typedReturn (S.falsePred sym)
    _ -> error "Unexpected argument list to isR0"
  where
    r0 :: Location arch (BaseBVType (A.RegWidth arch))
    r0 = LocGPR (PPC.GPR 0x0)

locationFuncInterpretation
  :: ( A.IsLocation (Location ppc)
     , A.Location ppc ~ Location ppc
     , A.Operand ppc ~ PPC.Operand
     , sym ~ S.ExprBuilder t st fs
     , A.OperandComponents ppc sym ~ OperandComponents ppc sym
     )
  => [(String, A.FunctionInterpretation t st fs ppc)]
locationFuncInterpretation =
  [ ("ppc.memri_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemriReg
                                               , A.exprInterpName = 'E.interpMemriRegExtractor
                                               , A.exprInterp = E.evalRegExtractor "memri_reg" $ \ao ->
                                                   case ao of
                                                     A.CompoundOperand (OCMemri l _ _) -> Just (Some l)
                                                     _ -> Nothing
                                               })
  , ("ppc.memrix_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrixReg
                                                , A.exprInterpName = 'E.interpMemrixRegExtractor
                                                , A.exprInterp = E.evalRegExtractor "memrix_reg" $ \ao ->
                                                    case ao of
                                                      A.CompoundOperand (OCMemrix l _ _) -> Just (Some l)
                                                      _ -> Nothing
                                                })
  , ("ppc.memrr_base", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrrBase
                                                , A.exprInterpName = 'E.interpMemrrBaseExtractor
                                                , A.exprInterp = E.evalRegExtractor "memrr_base" $ \ao ->
                                                    case ao of
                                                      A.CompoundOperand (OCMemrr b _ _ _) -> Just (Some b)
                                                      _ -> Nothing
                                                })
  , ("ppc.memrr_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrrOffset
                                                  , A.exprInterpName = 'E.interpMemrrOffsetExtractor
                                                  , A.exprInterp = E.evalRegExtractor "memrr_offset" $ \ao ->
                                                      case ao of
                                                        A.CompoundOperand (OCMemrr _ _ offset _) -> Just (Some offset)
                                                        _ -> Nothing
                                                  })
  , ("ppc.memrix_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrixOffset
                                                   , A.exprInterpName = 'E.interpMemrixOffsetExtractor
                                                   , A.exprInterp = E.evalBitvectorExtractor "memrix_offset" (knownNat @14) $ \ao nr' ->
                                                       case ao of
                                                         OCMemrix _ _ e
                                                           | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
                                                         _ -> Nothing
                                                   })
  , ("ppc.memri_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemriOffset
                                                  , A.exprInterpName = 'E.interpMemriOffsetExtractor
                                                  , A.exprInterp = E.evalBitvectorExtractor "memri_offset" (knownNat @16) $ \ao nr' ->
                                                       case ao of
                                                         OCMemri _ _ e
                                                           | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
                                                         _ -> Nothing
                                                  })
  , ("ppc.is_r0", A.FunctionInterpretation { A.exprInterpName = 'E.interpIsR0
                                           , A.exprInterp = A.Evaluator isR0
                                           , A.locationInterp = F.LocationFuncInterp (\_ _ _ -> Nothing)
                                           })
  ]

-- | Helper function to convert from Dismantle 'Maybe GPR' values to total
-- locations, as needed by 'OperandComponents'
fromMaybeGPRLoc :: forall arch.
                   Maybe PPC.GPR -> Location arch (BaseBVType (A.RegWidth arch))
fromMaybeGPRLoc (Just gpr) = LocGPR gpr
fromMaybeGPRLoc Nothing    = LocGPR (PPC.GPR 0)

-- | Helper function to conver Dismantle 'Maybe GPR' values to symbolic
-- expressions representing their values, as needed by 'OperandComponents'
fromMaybeGPRBase :: forall arch sym.
                    ( A.Architecture arch
                    , S.IsExprBuilder sym )
                 => sym 
                 -> Maybe PPC.GPR
                 -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
                 -> IO (S.SymBV sym (A.RegWidth arch))
fromMaybeGPRBase _sym (Just gpr) locLookup  = locLookup (LocGPR gpr)
fromMaybeGPRBase sym  Nothing    _locLookup = S.bvLit sym knownNat 0
