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
  boundVarElemIndex,
  concreteTemplatedOperand,
  symbolicTemplatedOperand,
  evalMemReg,
  isR0
  ) where

import           Data.Bits                          ( shiftR, shiftL, (.|.), (.&.) )
import qualified Data.ByteString.Builder            as B
import           Data.Int                           ( Int16 )
import qualified Data.Int.Indexed                   as I
import           Data.List
import qualified Data.List.NonEmpty                 as NEL
import           Data.Monoid                        ( (<>) )
import           Data.Parameterized.Classes
import           Data.Parameterized.Context
import qualified Data.Parameterized.Map             as M
import qualified Data.Parameterized.Map             as MapF
import           Data.Parameterized.Some            ( Some(..) )
import           Data.Parameterized.TraversableFC
import           Data.Proxy
import qualified Data.Serialize.Get                 as G
import qualified Data.Set                           as S
import           Data.Type.Equality                 ((:~:)(Refl), testEquality)
import           Data.Word                          ( Word16 )
import qualified Data.Word.Indexed                  as W
import qualified Dismantle.PPC                      as PPC
import           GHC.TypeLits
import           Lang.Crucible.BaseTypes            ( BaseBVType, NatRepr, knownNat, BaseTypeRepr )
import qualified Lang.Crucible.Solver.Interface     as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import           Numeric.Natural                    ( Natural )
import qualified SemMC.Architecture                 as A
import qualified SemMC.Architecture.PPC.Eval        as E
import           SemMC.Architecture.PPC.Location
import qualified SemMC.Architecture.Value           as V
import qualified SemMC.BoundVar                     as BoundVar
import qualified SemMC.Formula                      as F
import qualified SemMC.Formula.Eval                 as E
import qualified SemMC.Synthesis.Template           as T
import qualified SemMC.Util                         as U
import qualified Text.Megaparsec                    as P
import qualified Text.Megaparsec.Char               as P
import qualified Text.Megaparsec.Char.Lexer         as P

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
          expr <- A.unTagged <$> A.operandValue (Proxy @arch) sym locLookup (op x)
          return (expr, T.WrappedRecoverOperandFn $ const (return (op x)))

symbolicTemplatedOperand :: forall arch s (bits :: Nat) extended
                          . (A.OperandType arch s ~ BaseBVType extended,
                             KnownNat bits,
                             KnownNat extended,
                             1 <= bits,
                             bits <= extended)
                         => Proxy bits
                         -> Bool
                         -> String
                         -> (Integer -> A.Operand arch s)
                         -> T.TemplatedOperand arch s
symbolicTemplatedOperand Proxy signed name constr =
  T.TemplatedOperand { T.templOpLocation = Nothing
                     , T.templUsedLocations = S.empty
                     , T.templOpFn = mkTemplate'
                     }
  where mkTemplate' :: T.TemplatedOperandFn arch s
        mkTemplate' sym _ = do
          v <- S.freshConstant sym (U.makeSymbol name) (knownRepr :: BaseTypeRepr (BaseBVType bits))
          let bitsRepr = knownNat @bits
              extendedRepr = knownNat @extended
          extended <- case S.testNatCases bitsRepr extendedRepr of
            S.NatCaseLT S.LeqProof ->
              if signed
              then S.bvSext sym knownNat v
              else S.bvZext sym knownNat v
            S.NatCaseEQ -> return v
            S.NatCaseGT S.LeqProof -> error "impossible"
          let recover evalFn = constr <$> evalFn v
          return (extended, T.WrappedRecoverOperandFn recover)

data MemoryAccess
  = Base
  | Offset
  deriving (Show, Eq)

evalMemReg :: ( A.Operand arch ~ PPC.Operand
              , A.Location arch ~ Location arch
              , A.IsLocation (Location arch)
              )
           => MemoryAccess
           -> E.Evaluator arch t
evalMemReg access = E.Evaluator (rewrite access)

isR0
  :: forall t st sh u tp arch . (A.Location arch ~ Location arch, A.Operand arch ~ PPC.Operand) =>
  S.SimpleBuilder t st
  -> F.ParameterizedFormula (S.SimpleBuilder t st) arch sh
  -> PPC.List (A.Operand arch) sh
  -> Assignment (S.Elt t) u
  -> BaseTypeRepr tp
  -> IO (S.Elt t tp, Literals arch (S.SimpleBuilder t st))
isR0 sym pf operands assignment typeRepr =
  case assignment of
    Empty :> S.BoundVarElt b ->
      case Some b `boundVarElemIndex` (toListFC Some (F.pfOperandVars pf)) of
        Nothing ->
         returnElt $
          case MapF.lookup (LocGPR (PPC.GPR 0x0)) (F.pfLiteralVars pf) of
            Nothing -> S.falsePred sym
            Just b' ->
              case testEquality b b' of
                Just Refl -> S.truePred sym
                Nothing -> S.falsePred sym
        Just index ->
          case findOperandByIndex index operands of
            Just (Some g@(PPC.Gprc _)) -> returnElt $
              case testEquality g (PPC.Gprc (PPC.GPR 0x0)) of
                Just Refl -> S.truePred sym
                Nothing -> S.falsePred sym
            Just (Some g@(PPC.Gprc_nor0 _)) -> returnElt $
              case testEquality g (PPC.Gprc_nor0 (PPC.GPR 0x0)) of
                Just Refl -> S.truePred sym
                Nothing -> S.falsePred sym
            Just x -> error $ "Expected a GPRC but got: " ++ show x
            Nothing -> error "Index out of range in operandVars"
   where
     returnElt
       :: S.Pred (S.SimpleBuilder t st)
       -> IO (S.Elt t tp, Literals arch (S.SimpleBuilder t st))
     returnElt b' =
       case testEquality (S.exprType b') typeRepr of
         Just Refl -> do
           pure (b', M.empty)
         Nothing -> error "Failed type equality in isR0"

type Literals arch sym = M.MapF (A.Location arch) (S.BoundVar sym)

rewrite
  :: forall t st sh u tp arch . ( A.Location arch ~ Location arch
                                , A.Operand arch ~ PPC.Operand
                                , A.IsLocation (Location arch)
                                ) =>
     MemoryAccess
  -> S.SimpleBuilder t st
  -> F.ParameterizedFormula (S.SimpleBuilder t st) arch sh
  -> PPC.List (A.Operand arch) sh
  -> Assignment (S.Elt t) u
  -> BaseTypeRepr tp
  -> IO (S.Elt t tp, Literals arch (S.SimpleBuilder t st))
rewrite access sym pf operands assignment baseTypeRepr =
  case assignment of
    Empty :> S.BoundVarElt b ->
      case Some b `boundVarElemIndex` (toListFC Some (F.pfOperandVars pf)) of
        Nothing -> error "BoundVar not present in ParameterizedFormula"
        Just index ->
          case findOperandByIndex index operands of
            Just (Some op) ->
              case op of
                PPC.Memri (PPC.MemRI maybeBase offset) ->
                  case access of
                    Offset -> handleOffset (fromIntegral offset) (knownNat :: NatRepr 16)
                    Base -> handleBase maybeBase
                PPC.Memrix (PPC.MemRIX maybeBase (I.I offset)) ->
                  case access of
                    Offset -> handleOffset (fromIntegral offset) (knownNat :: NatRepr 14)
                    Base -> handleBase maybeBase
                PPC.Memrr (PPC.MemRR maybeBase offset) ->
                  case access of
                    Offset -> handleBase (Just offset)
                    Base -> handleBase maybeBase
                _ -> error $ "Unexpected operand type: " ++ show op
            Nothing ->
              error "Index not present in operands list"

         where
           handleOffset
             :: ( KnownNat n
                , 1 <= n
                ) => Integer
                  -> NatRepr n
                  -> IO (S.Elt t tp, Literals arch (S.SimpleBuilder t st))
           handleOffset offset natRepr = do
             s <- S.bvLit sym natRepr offset
             case S.exprType s `testEquality` baseTypeRepr of
               Just Refl -> pure (s, M.empty)
               Nothing -> error "Couldn't unify offset types"
           handleBase maybeBase =
             case maybeBase of
               Nothing ->
                 findAtRegister (PPC.GPR 0x0) (U.makeSymbol "r0")
               Just (PPC.GPR base) ->
                 findAtRegister (PPC.GPR base) $ U.makeSymbol $ "r" ++ show base
           findAtRegister
             :: A.IsLocation (Location arch)
             => PPC.GPR
             -> S.SolverSymbol
             -> IO (S.Elt t tp, Literals arch (S.SimpleBuilder t st))
           findAtRegister reg symbol =
             case MapF.lookup (LocGPR reg) (F.pfLiteralVars pf) of
               Nothing ->
                 case findVariableForReg reg operands pf of
                   Just (Some (BoundVar.BoundVar k)) ->
                     case testEquality baseTypeRepr (S.bvarType k) of
                       Just Refl ->
                         pure (S.varExpr sym k, M.empty)
                       _ -> error "Type equality failure"
                   Nothing -> do
                     let loc :: Location arch (BaseBVType (ArchRegWidth arch)) = LocGPR reg
                     s <- S.freshBoundVar sym symbol baseTypeRepr
                     case testEquality (A.locationType loc) (S.bvarType s) of
                       Just Refl -> do
                         let map' = M.singleton loc s
                         pure (S.BoundVarElt s, map')
                       Nothing -> error "Type equality failure"
               Just k ->
                 pure $ case testEquality baseTypeRepr (S.bvarType k) of
                   Just Refl -> (S.varExpr sym k, M.empty)
                   _ -> error "Type equality failure"

findOperandByIndex
  :: Int
  -> PPC.List PPC.Operand sh
  -> Maybe (Some PPC.Operand)
findOperandByIndex index list =
  lookup index $ zip [0..] (toListFC Some list)

findVariableForReg
  :: (FoldableFC t, S.TestEquality (S.BoundVar sym), A.Operand arch ~ PPC.Operand)
  => PPC.GPR
  -> t PPC.Operand c2
  -> F.ParameterizedFormula sym arch c2
  -> Maybe (Some (BoundVar.BoundVar sym arch))
findVariableForReg b list pf = do
  index <- getIndex
  lookup index $ zip [0..] $
    toListFC Some (F.pfOperandVars pf)
      where
        getIndex = go (toListFC Some list) 0
          where
            go :: [Some PPC.Operand] -> Int -> Maybe Int
            go [] _ = Nothing
            go (Some x : xs) n =
              case x of
                PPC.Gprc base | base == b -> Just n
                _ -> go xs (n+1)

boundVarElemIndex
  :: S.TestEquality (S.BoundVar sym)
  => Some (S.BoundVar sym)
  -> [Some (BoundVar.BoundVar sym arch)]
  -> Maybe Int
boundVarElemIndex x xs = do
  let xs' = [ Some y | Some (BoundVar.BoundVar y) <- xs ]
  x `elemIndex` xs'

locationFuncInterpretation
  :: (A.IsLocation (Location ppc), A.Location ppc ~ Location ppc, A.Operand ppc ~ PPC.Operand)
  => [(String, A.FunctionInterpretation t ppc)]
locationFuncInterpretation =
  [ ("ppc.memri_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemriReg
                                               , A.exprInterpName = 'E.interpMemriRegExtractor
                                               , A.exprInterp = evalMemReg Base
                                               })
  , ("ppc.memrix_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrixReg
                                                , A.exprInterpName = 'E.interpMemrixRegExtractor
                                                , A.exprInterp = evalMemReg Base
                                                })
  , ("ppc.memrr_base", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrrBase
                                                , A.exprInterpName = 'E.interpMemrrBaseExtractor
                                                , A.exprInterp = evalMemReg Base
                                                })
  , ("ppc.memrr_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrrOffset
                                                  , A.exprInterpName = 'E.interpMemrrOffsetExtractor
                                                  , A.exprInterp = evalMemReg Offset
                                                  })
  , ("ppc.memrix_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemrixOffset
                                                   , A.exprInterpName = 'E.interpMemrixOffsetExtractor
                                                   , A.exprInterp = evalMemReg Offset
                                                   })
  , ("ppc.memri_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp E.interpMemriOffset
                                                  , A.exprInterpName = 'E.interpMemriOffsetExtractor
                                                  , A.exprInterp = evalMemReg Offset
                                                  })
  , ("ppc.is_r0", A.FunctionInterpretation { A.exprInterpName = 'E.interpIsR0
                                           , A.exprInterp = E.Evaluator isR0
                                           , A.locationInterp = F.LocationFuncInterp (\_ _ _ -> Nothing)
                                           })
  ]
