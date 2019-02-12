{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
  ) where

import qualified Data.Map as Map
import           Data.Maybe ( fromJust )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Pair
import           Data.Parameterized.Some ( Some(..), viewSome )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Parameterized.List as SL
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Data.SCargot.Repr as SC

import           What4.BaseTypes
import qualified What4.Interface as S
import qualified What4.Expr.Builder as S
import qualified What4.Symbol as S

import qualified SemMC.Architecture as A
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Formula
import           SemMC.Formula.SETokens ( FAtom(..), printTokens
                                        , fromFoldable, fromFoldable'
                                        , ident, quoted, int )

-- This file is organized top-down, i.e., from high-level to low-level.

-- | Serialize a 'ParameterizedFormula' into its textual s-expression form.
printParameterizedFormula :: (A.Architecture arch)
                          => A.ShapeRepr arch sh
                          -> ParameterizedFormula (S.ExprBuilder t st fs) arch sh
                          -> T.Text
printParameterizedFormula rep =
  printTokens mempty . sexprConvertParameterized rep

printFormula :: (ShowF (A.Location arch))
             => Formula (S.ExprBuilder t st fs) arch
             -> T.Text
printFormula = printTokens mempty . sexprConvert

sexprConvert :: forall t st fs arch
              . (ShowF (A.Location arch))
             => Formula (S.ExprBuilder t st fs) arch
             -> SC.SExpr FAtom
sexprConvert f =
  fromFoldable' (ident "defs" : map (convertSimpleDef (Proxy @arch) (formParamVars f)) (MapF.toList (formDefs f)))

convertSimpleDef :: forall arch proxy t
                  . (ShowF (A.Location arch))
                 => proxy arch
                 -> MapF.MapF (A.Location arch) (S.ExprBoundVar t)
                 -> MapF.Pair (A.Location arch) (S.Expr t)
                 -> SC.SExpr FAtom
convertSimpleDef _ paramVars (MapF.Pair loc elt) =
  SC.SCons (convertLocation loc) (convertElt paramLookup elt)
  where
    tbl = Map.fromList [ (Some bv, convertLocation l) | MapF.Pair l bv <- MapF.toList paramVars ]
    paramLookup :: forall tp . S.ExprBoundVar t tp -> Maybe (SC.SExpr FAtom)
    paramLookup bv = Map.lookup (Some bv) tbl

-- | Intermediate serialization.
sexprConvertParameterized :: (A.Architecture arch)
                          => A.ShapeRepr arch sh
                          -> ParameterizedFormula (S.ExprBuilder t st fs) arch sh
                          -> SC.SExpr FAtom
sexprConvertParameterized rep (ParameterizedFormula { pfUses = uses
                                       , pfOperandVars = opVars
                                       , pfLiteralVars = litVars
                                       , pfDefs = defs
                                       }) =
  fromFoldable' [ SC.SCons (SC.SAtom (AIdent "operands")) (SC.SCons (convertOperandVars rep opVars) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "in")) (SC.SCons (convertUses opVars uses) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "defs")) (SC.SCons (convertDefs opVars litVars defs) SC.SNil)
                ]

convertUses :: (ShowF (A.Location arch))
            => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
            -> Set.Set (Some (Parameter arch sh))
            -> SC.SExpr FAtom
convertUses oplist = fromFoldable (viewSome (convertParameter oplist))

convertParameter :: (ShowF (A.Location arch))
                 => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
                 -> Parameter arch sh tp
                 -> SC.SExpr FAtom
convertParameter opVars (OperandParameter _ idx) = ident name
  where name = varName (opVars SL.!! idx)
convertParameter _ (LiteralParameter loc) = quoted (showF loc)
convertParameter opVars (FunctionParameter fnName (WrappedOperand orep oix) _) =
  SC.SCons uf (SC.SCons args SC.SNil)
  where
    uf = SC.SCons (SC.SAtom (AIdent "_"))
                  (SC.SCons (SC.SAtom (AIdent "call"))
                            (SC.SCons (SC.SAtom (AString fnName))
                                                SC.SNil))
    args = SC.SCons (convertParameter opVars (OperandParameter orep oix)) SC.SNil

-- | Used for substituting in the result expression when a variable is
-- encountered in a definition.
type ParamLookup t = forall tp. S.ExprBoundVar t tp -> Maybe (SC.SExpr FAtom)

convertDefs :: forall t st fs arch sh.
               (ShowF (A.Location arch))
            => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
            -> MapF.MapF (A.Location arch) (S.ExprBoundVar t)
            -> MapF.MapF (Parameter arch sh) (S.Expr t)
            -> SC.SExpr FAtom
convertDefs opVars locVars = fromFoldable (convertDef opVars paramLookup) . MapF.toList
  where paramLookup :: ParamLookup t
        paramLookup = flip Map.lookup paramMapping . Some
        paramMapping = MapF.foldrWithKey insertLoc opMapping locVars
        insertLoc loc var = Map.insert (Some var) (convertLocation loc)
        opMapping = buildOpMapping opVars

convertLocation :: (ShowF loc) => loc tp -> SC.SExpr FAtom
convertLocation = SC.SAtom . AQuoted . showF

-- | For use in the parameter lookup function.
buildOpMapping :: SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
               -> Map.Map (Some (S.ExprBoundVar t)) (SC.SExpr FAtom)
buildOpMapping SL.Nil = Map.empty
buildOpMapping (var SL.:< rest) =
  Map.insert (Some (BV.unBoundVar var)) (ident name) $ buildOpMapping rest
  where name = varName var

convertDef :: (ShowF (A.Location arch))
           => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
           -> ParamLookup t
           -> Pair (Parameter arch sh) (S.Expr t)
           -> SC.SExpr FAtom
convertDef opVars paramLookup (Pair param expr) =
  SC.SCons (convertParameter opVars param) (SC.SCons (convertElt paramLookup expr) SC.SNil)

-- NOTE: There's probably some fancy caching we can do because of the nonces in
-- all the expressions. If the current implementation is at all slow, we can
-- implement that. However, I'm skipping it for now, since I imagine this won't
-- be a bottleneck.

convertElt :: ParamLookup t -> S.Expr t tp -> SC.SExpr FAtom
convertElt _ (S.SemiRingLiteral S.SemiRingNat _ _) = error "NatElt not supported"
convertElt _ (S.SemiRingLiteral S.SemiRingInt _ _) = error "IntElt not supported"
convertElt _ (S.SemiRingLiteral S.SemiRingReal _ _) = error "RatElt not supported"
convertElt _ (S.StringExpr {}) = error "StringExpr is not supported"
convertElt _ (S.BVExpr sz val _) = SC.SAtom (ABV (widthVal sz) val)
convertElt paramLookup (S.AppExpr appElt) = convertAppElt paramLookup appElt
convertElt paramLookup (S.NonceAppExpr nae) =
  case S.nonceExprApp nae of
    S.FnApp fn args -> convertFnApp paramLookup fn args
    S.Forall {} -> error "Forall NonceAppExpr not supported"
    S.Exists {} -> error "Exists NonceAppExpr not supported"
    S.ArrayFromFn {} -> error "ArrayFromFn NonceAppExpr not supported"
    S.MapOverArrays {} -> error "MapOverArrays NonceAppExpr not supported"
    S.ArrayTrueOnEntries {} -> error "ArrayTrueOnEntries NonceAppExpr not supported"
convertElt paramLookup (S.BoundVarExpr var) = fromJust $ paramLookup var

convertFnApp :: ParamLookup t -> S.ExprSymFn t args ret -> Ctx.Assignment (S.Expr t) args -> SC.SExpr FAtom
convertFnApp paramLookup fn args
  | name == "undefined"
  , BaseBVRepr nr <- S.fnReturnType fn =
      let call = fromFoldable' [ ident "_", ident "call", quoted "uf.undefined" ]
      in fromFoldable' [ call, int (NR.intValue nr) ]
  | otherwise =
    let call = fromFoldable' [ ident "_", ident "call", quoted (prefix ++ T.unpack name) ]
    in fromFoldable' (call : FC.toListFC (convertElt paramLookup) args)
  where
    name = S.solverSymbolAsText (S.symFnName fn)
    prefix = case S.symFnInfo fn of
      S.UninterpFnInfo _ _ -> "uf."
      S.DefinedFnInfo _ _ _ -> "df."
      _ -> error ("Unsupported function: " ++ T.unpack name)

convertAppElt :: ParamLookup t -> S.AppExpr t tp -> SC.SExpr FAtom
convertAppElt paramLookup = convertApp paramLookup . S.appExprApp

convertApp :: forall t tp. ParamLookup t -> S.App (S.Expr t) tp -> SC.SExpr FAtom
convertApp paramLookup = fromFoldable' . convertApp'
  where convert :: forall tp'. S.Expr t tp' -> SC.SExpr FAtom
        convert = convertElt paramLookup

        convertApp' :: S.App (S.Expr t) tp -> [SC.SExpr FAtom]
        convertApp' S.TrueBool = [ident "true"]
        convertApp' S.FalseBool = [ident "false"]
        convertApp' (S.NotBool b) = [ident "not", convert b]
        convertApp' (S.AndBool b1 b2) = [ident "and", convert b1, convert b2]
        convertApp' (S.XorBool b1 b2) = [ident "xor", convert b1, convert b2]
        convertApp' (S.IteBool bp bt be) = [ident "ite", convert bp, convert bt, convert be]
        convertApp' (S.BVIte _ _ bp bvt bve) = [ident "ite", convert bp, convert bvt, convert bve]
        convertApp' (S.BVEq bv1 bv2) = [ident "=", convert bv1, convert bv2]
        convertApp' (S.BVSlt bv1 bv2) = [ident "bvslt", convert bv1, convert bv2]
        convertApp' (S.BVUlt bv1 bv2) = [ident "bvult", convert bv1, convert bv2]
        convertApp' (S.BVConcat _ bv1 bv2) = [ident "concat", convert bv1, convert bv2]
        convertApp' (S.BVSelect idx n bv) = extract i j bv
          -- See SemMC.Formula.Parser.readExtract for the explanation behind
          -- these values.
          where i = intValue n + j - 1
                j = intValue idx
        convertApp' (S.BVNeg _ bv) = [ident "bvneg", convert bv]
        convertApp' (S.BVAdd _ bv1 bv2) = [ident "bvadd", convert bv1, convert bv2]
        convertApp' (S.BVMul _ bv1 bv2) = [ident "bvmul", convert bv1, convert bv2]
        convertApp' (S.BVUdiv _ bv1 bv2) = [ident "bvudiv", convert bv1, convert bv2]
        convertApp' (S.BVUrem _ bv1 bv2) = [ident "bvurem", convert bv1, convert bv2]
        convertApp' (S.BVSdiv _ bv1 bv2) = [ident "bvsdiv", convert bv1, convert bv2]
        convertApp' (S.BVSrem _ bv1 bv2) = [ident "bvsrem", convert bv1, convert bv2]
        convertApp' (S.BVShl _ bv1 bv2) = [ident "bvshl", convert bv1, convert bv2]
        convertApp' (S.BVLshr _ bv1 bv2) = [ident "bvlshr", convert bv1, convert bv2]
        convertApp' (S.BVAshr _ bv1 bv2) = [ident "bvashr", convert bv1, convert bv2]
        convertApp' (S.BVZext r bv) = extend "zero" (intValue r) bv
        convertApp' (S.BVSext r bv) = extend "sign" (intValue r) bv
        convertApp' (S.BVBitNot _ bv) = [ident "bvnot", convert bv]
        convertApp' (S.BVBitAnd _ bv1 bv2) = [ident "bvand", convert bv1, convert bv2]
        convertApp' (S.BVBitOr _ bv1 bv2) = [ident "bvor", convert bv1, convert bv2]
        convertApp' (S.BVBitXor _ bv1 bv2) = [ident "bvxor", convert bv1, convert bv2]
        convertApp' app = error $ "unhandled App: " ++ show app

        extract :: forall tp'. Integer -> Integer -> S.Expr t tp' -> [SC.SExpr FAtom]
        extract i j bv = [fromFoldable' [ident "_", ident "extract", int i, int j],
                          convert bv]

        extend :: forall w. String -> Integer -> S.Expr t (BaseBVType w) -> [SC.SExpr FAtom]
        extend op r bv = [fromFoldable' [ident "_", ident (op ++ "_extend"), int extension],
                          convert bv]
          where extension = r - w
                w = case S.exprType bv of BaseBVRepr len -> intValue len

-- | Extract the name, as a String, of a wrapped bound variable.
varName :: BV.BoundVar (S.ExprBuilder t st fs) arch op -> String
varName (BV.BoundVar var) = show (S.bvarName var)


convertOperandVars :: forall arch sh t st fs
                    . (A.Architecture arch)
                   => A.ShapeRepr arch sh
                   -> SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
                   -> SC.SExpr FAtom
convertOperandVars rep l =
  case (rep, l) of
    (SL.Nil, SL.Nil) -> SC.SNil
    (r SL.:< rep', var SL.:< rest) ->
      let nameExpr = ident (varName var)
          typeExpr = quoted (A.operandTypeReprSymbol (Proxy @arch) r)
      in SC.SCons (SC.SCons nameExpr typeExpr) (convertOperandVars rep' rest)
