{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Formula.Printer
  ( printParameterizedFormula
  , printFormula
  , printFunctionFormula
  ) where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Pair
import           Data.Parameterized.Some ( Some(..), viewSome )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           SemMC.Util ( fromJust' )

import qualified Data.SCargot.Repr.Rich as SE

import           What4.BaseTypes
import qualified What4.Expr as S
import qualified What4.Expr.BoolMap as BooM
import qualified What4.Expr.Builder as S
import qualified What4.Expr.WeightedSum as WSum
import qualified What4.Interface as S
import qualified What4.Symbol as S

import qualified SemMC.Architecture as A
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Formula
import           SemMC.Formula.SETokens ( FAtom(..), printTokens'
                                        , ident', quoted', int', string', bitvec'
                                        )

-- This file is organized top-down, i.e., from high-level to low-level.

-- | Serialize a 'ParameterizedFormula' into its textual s-expression form.
printParameterizedFormula :: (A.Architecture arch)
                          => A.ShapeRepr arch sh
                          -> ParameterizedFormula (S.ExprBuilder t st fs) arch sh
                          -> T.Text
printParameterizedFormula rep =
  printTokens' mempty . sexprConvertParameterized rep

printFormula :: (ShowF (A.Location arch))
             => Formula (S.ExprBuilder t st fs) arch
             -> T.Text
printFormula = printTokens' mempty . sexprConvert

printFunctionFormula :: FunctionFormula (S.ExprBuilder t st fs) '(tps, tp)
                     -> T.Text
printFunctionFormula = printTokens' mempty . sexprConvertFunction

sexprConvert :: forall t st fs arch
              . (ShowF (A.Location arch))
             => Formula (S.ExprBuilder t st fs) arch
             -> SE.RichSExpr FAtom
sexprConvert f =
  SE.L $ (ident' "defs") : map (convertSimpleDef (Proxy @arch) (formParamVars f)) (MapF.toList (formDefs f))

convertSimpleDef :: forall arch proxy t
                  . (ShowF (A.Location arch))
                 => proxy arch
                 -> MapF.MapF (A.Location arch) (S.ExprBoundVar t)
                 -> MapF.Pair (A.Location arch) (S.Expr t)
                 -> SE.RichSExpr FAtom
convertSimpleDef _ paramVars (MapF.Pair loc elt) =
  SE.L [ convertLocation loc, convertElt paramLookup elt ]
  where
    tbl = Map.fromList [ (Some bv, convertLocation l) | MapF.Pair l bv <- MapF.toList paramVars ]
    paramLookup :: ParamLookup t
    paramLookup bv = Map.lookup (Some bv) tbl

-- | Intermediate serialization.
sexprConvertParameterized :: (A.Architecture arch)
                          => A.ShapeRepr arch sh
                          -> ParameterizedFormula (S.ExprBuilder t st fs) arch sh
                          -> SE.RichSExpr FAtom
sexprConvertParameterized rep (ParameterizedFormula { pfUses = uses
                                                    , pfOperandVars = opVars
                                                    , pfLiteralVars = litVars
                                                    , pfDefs = defs
                                                    }) =
  SE.L [ SE.L [SE.A (AIdent "operands"), convertOperandVars rep opVars]
       , SE.L [SE.A (AIdent "in"),       convertUses opVars uses]
       , SE.L [SE.A (AIdent "defs"),     convertDefs opVars litVars defs]
       ]

sexprConvertFunction :: FunctionFormula (S.ExprBuilder t st fs) '(tps, tp)
                     -> SE.RichSExpr FAtom
sexprConvertFunction (FunctionFormula { ffName = name
                                      , ffArgTypes = argTypes
                                      , ffArgVars = argVars
                                      , ffRetType = retType
                                      , ffDef = def
                                      }) =
  SE.L [ SE.L [ SE.A (AIdent "function"), SE.A (AIdent name)]
       , SE.L [ SE.A (AIdent "arguments"), convertArgumentVars argTypes argVars ]
       , SE.L [ SE.A (AIdent "ret"), printBaseType retType ]
       , SE.L [ SE.A (AIdent "body"), convertFnBody undefined ]
       ]

convertFnBody :: S.ExprSymFn t args ret
              -> SE.RichSExpr FAtom
convertFnBody = undefined

-- convertFnApp :: ParamLookup t
--              -> S.ExprSymFn t args ret
--              -> Ctx.Assignment (S.Expr t) args
--              -> SE.RichSExpr FAtom

convertUses :: (ShowF (A.Location arch))
            => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
            -> Set.Set (Some (Parameter arch sh))
            -> SE.RichSExpr FAtom
convertUses oplist = SE.L . fmap (viewSome (convertParameter oplist)) . Set.toList

convertParameter :: (ShowF (A.Location arch))
                 => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
                 -> Parameter arch sh tp
                 -> SE.RichSExpr FAtom
convertParameter opVars (OperandParameter _ idx) = ident' name
  where name = varName (opVars SL.!! idx)
convertParameter _ (LiteralParameter loc) = quoted' (showF loc)
convertParameter opVars (FunctionParameter fnName (WrappedOperand orep oix) _) =
  SE.L [uf, args]
  where
    uf = SE.L [ ident' "_", ident' "call", string' fnName ]
    args = SE.L [convertParameter opVars (OperandParameter orep oix) ]

-- | Used for substituting in the result expression when a variable is
-- encountered in a definition.
type ParamLookup t = forall tp. S.ExprBoundVar t tp -> Maybe (SE.RichSExpr FAtom)

convertDefs :: forall t st fs arch sh.
               (ShowF (A.Location arch))
            => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
            -> MapF.MapF (A.Location arch) (S.ExprBoundVar t)
            -> MapF.MapF (Parameter arch sh) (S.Expr t)
            -> SE.RichSExpr FAtom
convertDefs opVars locVars = SE.L . fmap (convertDef opVars paramLookup) . MapF.toList
  where paramLookup :: ParamLookup t
        paramLookup = flip Map.lookup paramMapping . Some
        paramMapping = MapF.foldrWithKey insertLoc opMapping locVars
        insertLoc loc var = Map.insert (Some var) (convertLocation loc)
        opMapping = buildOpMapping opVars

convertLocation :: (ShowF loc) => loc tp -> SE.RichSExpr FAtom
convertLocation = quoted' . showF

-- | For use in the parameter lookup function.
buildOpMapping :: SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
               -> Map.Map (Some (S.ExprBoundVar t)) (SE.RichSExpr FAtom)
buildOpMapping SL.Nil = Map.empty
buildOpMapping (var SL.:< rest) =
  Map.insert (Some (BV.unBoundVar var)) (ident' name) $ buildOpMapping rest
  where name = varName var

convertDef :: (ShowF (A.Location arch))
           => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
           -> ParamLookup t
           -> Pair (Parameter arch sh) (S.Expr t)
           -> SE.RichSExpr FAtom
convertDef opVars paramLookup (Pair param expr) =
  SE.L [ convertParameter opVars param, convertElt paramLookup expr ]

-- NOTE: There's probably some fancy caching we can do because of the nonces in
-- all the expressions. If the current implementation is at all slow, we can
-- implement that. However, I'm skipping it for now, since I imagine this won't
-- be a bottleneck.

convertElt :: ParamLookup t -> S.Expr t tp -> SE.RichSExpr FAtom
convertElt _ (S.SemiRingLiteral S.SemiRingNatRepr _ _) = error "NatElt not supported"
convertElt _ (S.SemiRingLiteral S.SemiRingIntegerRepr _ _) = error "IntElt not supported"
convertElt _ (S.SemiRingLiteral S.SemiRingRealRepr _ _) = error "RatElt not supported"
convertElt _ (S.SemiRingLiteral (S.SemiRingBVRepr _ sz) val _) = SE.A (ABV (widthVal sz) val)
convertElt _ (S.StringExpr {}) = error "StringExpr is not supported"
convertElt _ (S.BoolExpr b _) = ident' $ if b then "true" else "false"
convertElt paramLookup (S.AppExpr appElt) = convertAppElt paramLookup appElt
convertElt paramLookup (S.NonceAppExpr nae) =
  case S.nonceExprApp nae of
    S.FnApp fn args -> convertFnApp paramLookup fn args
    S.Forall {} -> error "Forall NonceAppExpr not supported"
    S.Exists {} -> error "Exists NonceAppExpr not supported"
    S.ArrayFromFn {} -> error "ArrayFromFn NonceAppExpr not supported"
    S.MapOverArrays {} -> error "MapOverArrays NonceAppExpr not supported"
    S.ArrayTrueOnEntries {} -> error "ArrayTrueOnEntries NonceAppExpr not supported"
convertElt paramLookup (S.BoundVarExpr var) = fromJust' "SemMC.Formula.Printer paramLookup boundvar" $ paramLookup var

convertFnApp :: ParamLookup t
             -> S.ExprSymFn t args ret
             -> Ctx.Assignment (S.Expr t) args
             -> SE.RichSExpr FAtom
convertFnApp paramLookup fn args
  | name == "undefined"
  , BaseBVRepr nr <- S.fnReturnType fn =
      let call = SE.L [ ident' "_", ident' "call", quoted' "uf.undefined" ]
      in SE.L [ call, int' (NR.intValue nr) ]
  | otherwise =
    let call = SE.L [ ident' "_", ident' "call", quoted' (prefix ++ T.unpack name) ]
    in SE.L (call : FC.toListFC (convertElt paramLookup) args)
  where
    name = S.solverSymbolAsText (S.symFnName fn)
    prefix = case S.symFnInfo fn of
      S.UninterpFnInfo _ _ -> "uf."
      S.DefinedFnInfo _ _ _ -> "df."
      _ -> error ("Unsupported function: " ++ T.unpack name)

convertAppElt :: ParamLookup t -> S.AppExpr t tp -> SE.RichSExpr FAtom
convertAppElt paramLookup = convertApp paramLookup . S.appExprApp

convertApp :: forall t tp. ParamLookup t -> S.App (S.Expr t) tp -> SE.RichSExpr FAtom
convertApp paramLookup = convertApp'
  where convert :: forall tp'. S.Expr t tp' -> SE.RichSExpr FAtom
        convert = convertElt paramLookup

        convertApp' :: S.App (S.Expr t) tp -> SE.RichSExpr FAtom

        convertApp' (S.BaseIte _bt _ c xe ye) = SE.L [ident' "ite", convert c, convert xe, convert ye]
        convertApp' (S.BaseEq _bt v1 v2) = SE.L [ident' "=", convert v1, convert v2]
        convertApp' (S.NotPred x) = SE.L [ident' "notp", convert x]
        convertApp' (S.ConjPred bm) = convertBoolMap "andp" True bm
        convertApp' (S.DisjPred bm) = convertBoolMap "orp" False bm

        convertApp' (S.BVSlt bv1 bv2) = SE.L [ident' "bvslt", convert bv1, convert bv2]
        convertApp' (S.BVUlt bv1 bv2) = SE.L [ident' "bvult", convert bv1, convert bv2]
        convertApp' (S.BVConcat _ bv1 bv2) = SE.L [ident' "concat", convert bv1, convert bv2]
        convertApp' (S.BVSelect idx n bv) = extract i j bv
          -- See SemMC.Formula.Parser.readExtract for the explanation behind
          -- these values.
          where i = intValue n + j - 1
                j = intValue idx

        -- Note that because the SemiRing has an identity element that
        -- always gets applied, resulting in lots of additional,
        -- unnecessary elements like: "(bvand #xffffffff TERM)".
        -- These will get manifested in the stored form (but generally
        -- _not_ via DSL-generated versions since they don't output
        -- via Printer) and result in longer stored forms.  They could
        -- be eliminated by checking for the identity (e.g. "if mul ==
        -- SR.one (WSum.sumRepr sm)") but the re-loaded representation
        -- will still use the SemiRing, so it's probably not worth the
        -- effort to reduce these.
        convertApp' (S.SemiRingSum sm) =
          case WSum.sumRepr sm of
            S.SemiRingBVRepr S.BVArithRepr w ->
              let smul mul e = SE.L [ ident' "bvmul", bitvec' (natValue w) mul, convert e ]
                  sval v = bitvec' (natValue w) v
                  add x y = SE.L [ ident' "bvadd", x, y ]
              in WSum.eval add smul sval sm
            S.SemiRingBVRepr S.BVBitsRepr w ->
              let smul mul e = SE.L [ ident' "bvand", bitvec' (natValue w) mul, convert e ]
                  sval v = bitvec' (natValue w) v
                  add x y = let op = ident' "bvxor" in SE.L [ op, x, y ]
              in WSum.eval add smul sval sm
            S.SemiRingNatRepr     -> error "SemiRingSum NatRepr not supported"
            S.SemiRingIntegerRepr -> error "SemiRingSum IntRepr not supported"
            S.SemiRingRealRepr    -> error "SemiRingSum RealRepr not supported"

        convertApp' (S.SemiRingProd pd) =
          case WSum.prodRepr pd of
            S.SemiRingBVRepr S.BVArithRepr w ->
              let pmul x y = SE.L [ ident' "bvmul", x, y ]
                  unit = bitvec' (natValue w) 1
              in maybe unit id $ WSum.prodEval pmul convert pd
            S.SemiRingBVRepr S.BVBitsRepr w ->
              let pmul x y = SE.L [ ident' "bvand", x, y ]
                  unit = bitvec' (natValue w) $ maxUnsigned w
              in maybe unit id $ WSum.prodEval pmul convert pd
            S.SemiRingNatRepr     -> error "convertApp' S.SemiRingProd Nat unsupported"
            S.SemiRingIntegerRepr -> error "convertApp' S.SemiRingProd Integer unsupported"
            S.SemiRingRealRepr    -> error "convertApp' S.SemiRingProd Real unsupported"

        convertApp' (S.BVOrBits pd) =
          case WSum.prodRepr pd of
            S.SemiRingBVRepr _ w ->
              let pmul x y = SE.L [ident' "bvor", x, y ]
                  unit = bitvec' (natValue w) 0
              in maybe unit id $ WSum.prodEval pmul convert pd

        convertApp' (S.BVUdiv _ bv1 bv2) = SE.L [ident' "bvudiv", convert bv1, convert bv2]
        convertApp' (S.BVUrem _ bv1 bv2) = SE.L [ident' "bvurem", convert bv1, convert bv2]
        convertApp' (S.BVSdiv _ bv1 bv2) = SE.L [ident' "bvsdiv", convert bv1, convert bv2]
        convertApp' (S.BVSrem _ bv1 bv2) = SE.L [ident' "bvsrem", convert bv1, convert bv2]
        convertApp' (S.BVShl _ bv1 bv2) = SE.L [ident' "bvshl", convert bv1, convert bv2]
        convertApp' (S.BVLshr _ bv1 bv2) = SE.L [ident' "bvlshr", convert bv1, convert bv2]
        convertApp' (S.BVAshr _ bv1 bv2) = SE.L [ident' "bvashr", convert bv1, convert bv2]
        convertApp' (S.BVZext r bv) = extend "zero" (intValue r) bv
        convertApp' (S.BVSext r bv) = extend "sign" (intValue r) bv

        convertApp' app = error $ "unhandled App: " ++ show app

        convertBoolMap op base bm =
          let strBase b = if b
                          then SE.L [ident' "=", bitvec' 1 0, bitvec' 1 0]  -- true
                          else SE.L [ident' "=", bitvec' 1 0, bitvec' 1 1]  -- false
              strNotBase = strBase . not
          in case BooM.viewBoolMap bm of
               BooM.BoolMapUnit -> strBase base
               BooM.BoolMapDualUnit -> strNotBase base
               BooM.BoolMapTerms ts ->
                 let onEach e r = SE.L [ident' op, arg e, r]
                     arg (t, BooM.Positive) = convert t
                     arg (t, BooM.Negative) = SE.L [ident' "notp", convert t]
                 in F.foldr onEach (strBase base) ts

        extract :: forall tp'. Integer -> Integer -> S.Expr t tp' -> SE.RichSExpr FAtom
        extract i j bv = SE.L [ SE.L [ ident' "_", ident' "extract", int' i, int' j ]
                              , convert bv
                              ]

        extend :: forall w. String -> Integer -> S.Expr t (BaseBVType w) -> SE.RichSExpr FAtom
        extend op r bv = SE.L [ SE.L [ ident' "_", ident' $ op <> "_extend", int' extension ]
                              , convert bv
                              ]
          where extension = r - w
                w = case S.exprType bv of BaseBVRepr len -> intValue len

-- | Extract the name, as a String, of a wrapped bound variable.
varName :: BV.BoundVar (S.ExprBuilder t st fs) arch op -> String
varName (BV.BoundVar var) = show (S.bvarName var)

printBaseType :: BaseTypeRepr tp
              -> SE.RichSExpr FAtom
printBaseType tp = case tp of
  S.BaseBoolRepr -> SE.A (AQuoted "bool")
  S.BaseNatRepr -> SE.A (AQuoted "nat")
  S.BaseIntegerRepr -> SE.A (AQuoted "int")
  S.BaseRealRepr -> SE.A (AQuoted "real")
  S.BaseStringRepr -> SE.A (AQuoted "string")
  S.BaseComplexRepr -> SE.A (AQuoted "complex")
  S.BaseBVRepr wRepr -> SE.L [SE.A (AQuoted "bv"), SE.A (AInt (NR.intValue wRepr)) ]
  S.BaseStructRepr tps -> SE.L [SE.A (AQuoted "struct"), printBaseTypes tps]
  S.BaseArrayRepr ixs tp -> SE.L [SE.A (AQuoted "array"), SE.L [printBaseTypes ixs, printBaseType tp]]
  _ -> undefined

printBaseTypes :: Ctx.Assignment BaseTypeRepr tps
               -> SE.RichSExpr FAtom
printBaseTypes tps = case tps of
  Ctx.Empty -> SE.Nil
  tps' Ctx.:> tp -> SE.cons (printBaseType tp) (printBaseTypes tps')

convertOperandVars :: forall arch sh t st fs
                    . (A.Architecture arch)
                   => A.ShapeRepr arch sh
                   -> SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
                   -> SE.RichSExpr FAtom
convertOperandVars rep l =
  case (rep, l) of
    (SL.Nil, SL.Nil) -> SE.Nil
    (r SL.:< rep', var SL.:< rest) ->
      let nameExpr = ident' (varName var)
          typeExpr = AQuoted (A.operandTypeReprSymbol (Proxy @arch) r)
      in SE.cons (SE.DL [nameExpr] typeExpr) (convertOperandVars rep' rest)

convertArgumentVars :: forall sh t st fs
                     . SL.List BaseTypeRepr sh
                    -> SL.List (S.BoundVar (S.ExprBuilder t st fs)) sh
                    -> SE.RichSExpr FAtom
convertArgumentVars rep l =
  case (rep, l) of
    (SL.Nil, SL.Nil) -> SE.Nil
    (r SL.:< rep', var SL.:< rest) ->
      let nameExpr = ident' (T.unpack (S.solverSymbolAsText (S.bvarName var)))
          typeExpr = printBaseType r
      in SE.cons (SE.L [ nameExpr, typeExpr ]) (convertArgumentVars rep' rest)
