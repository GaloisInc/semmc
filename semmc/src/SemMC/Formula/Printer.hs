{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Formula.Printer
  ( printFormula
  , ConvertShape
  ) where

import qualified Data.Map as Map
import           Data.Maybe ( fromJust )
import           Data.Monoid ( (<>) )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair
import           Data.Parameterized.Some ( Some(..), viewSome )
import           Data.Parameterized.ShapedList ( indexShapedList, ShapedList(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import           Text.Printf ( printf )

import qualified Data.SCargot as SC
import qualified Data.SCargot.Repr as SC

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S

import qualified SemMC.Architecture as A
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Formula
import           SemMC.Formula.Parser ( Atom(..) )

-- This file is organized top-down, i.e., from high-level to low-level.

-- | Serialize a 'ParameterizedFormula' into its textual s-expression form.
printFormula :: (ShowF (A.Location arch),
                 ConvertShape sh)
             => ParameterizedFormula (S.SimpleBuilder t st) arch sh
             -> T.Text
printFormula = SC.encodeOne (SC.basicPrint printAtom) . sexprConvert

printAtom :: Atom -> T.Text
printAtom (AIdent s) = T.pack s
printAtom (AQuoted s) = T.pack ('\'' : s)
printAtom (AString s) = "\"" <> T.pack s <> "\""
printAtom (AInt i) = T.pack (show i)
printAtom (ABV sz val) = T.pack (prefix ++ printf fmt val)
  where (prefix, fmt)
          | sz `rem` 4 == 0 = ("#x", "%0" ++ show (sz `div` 4) ++ "x")
          | otherwise = ("#b", "%0" ++ show sz ++ "b")

-- | Intermediate serialization.
sexprConvert :: (ShowF (A.Location arch),
                 ConvertShape sh)
             => ParameterizedFormula (S.SimpleBuilder t st) arch sh
             -> SC.SExpr Atom
sexprConvert (ParameterizedFormula { pfUses = uses
                                   , pfOperandVars = opVars
                                   , pfLiteralVars = litVars
                                   , pfDefs = defs
                                   }) =
  fromFoldable' [ SC.SCons (SC.SAtom (AIdent "operands")) (SC.SCons (convertOperandVars opVars) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "in")) (SC.SCons (convertUses opVars uses) SC.SNil)
                , SC.SCons (SC.SAtom (AIdent "defs")) (SC.SCons (convertDefs opVars litVars defs) SC.SNil)
                ]

convertUses :: (ShowF (A.Location arch))
            => ShapedList (BV.BoundVar (S.SimpleBuilder t st) arch) sh
            -> Set.Set (Some (Parameter arch sh))
            -> SC.SExpr Atom
convertUses oplist = fromFoldable (viewSome (convertParameter oplist))

convertParameter :: (ShowF (A.Location arch))
                 => ShapedList (BV.BoundVar (S.SimpleBuilder t st) arch) sh
                 -> Parameter arch sh tp
                 -> SC.SExpr Atom
convertParameter opVars (OperandParameter _ idx) = ident name
  where name = varName (indexShapedList opVars idx)
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
type ParamLookup t = forall tp. S.SimpleBoundVar t tp -> Maybe (SC.SExpr Atom)

convertDefs :: forall t st arch sh.
               (ShowF (A.Location arch))
            => ShapedList (BV.BoundVar (S.SimpleBuilder t st) arch) sh
            -> MapF.MapF (A.Location arch) (S.SimpleBoundVar t)
            -> MapF.MapF (Parameter arch sh) (S.Elt t)
            -> SC.SExpr Atom
convertDefs opVars locVars = fromFoldable (convertDef opVars paramLookup) . MapF.toList
  where paramLookup :: ParamLookup t
        paramLookup = flip Map.lookup paramMapping . Some
        paramMapping = MapF.foldrWithKey insertLoc opMapping locVars
        insertLoc loc var = Map.insert (Some var) (convertLocation loc)
        opMapping = buildOpMapping opVars

convertLocation :: (ShowF loc) => loc tp -> SC.SExpr Atom
convertLocation = SC.SAtom . AQuoted . showF

-- | For use in the parameter lookup function.
buildOpMapping :: ShapedList (BV.BoundVar (S.SimpleBuilder t st) arch) sh
               -> Map.Map (Some (S.SimpleBoundVar t)) (SC.SExpr Atom)
buildOpMapping Nil = Map.empty
buildOpMapping (var :> rest) =
  Map.insert (Some (BV.unBoundVar var)) (ident name) $ buildOpMapping rest
  where name = varName var

convertDef :: (ShowF (A.Location arch))
           => ShapedList (BV.BoundVar (S.SimpleBuilder t st) arch) sh
           -> ParamLookup t
           -> Pair (Parameter arch sh) (S.Elt t)
           -> SC.SExpr Atom
convertDef opVars paramLookup (Pair param expr) =
  SC.SCons (convertParameter opVars param) (SC.SCons (convertElt paramLookup expr) SC.SNil)

-- NOTE: There's probably some fancy caching we can do because of the nonces in
-- all the expressions. If the current implementation is at all slow, we can
-- implement that. However, I'm skipping it for now, since I imagine this won't
-- be a bottleneck.

convertElt :: ParamLookup t -> S.Elt t tp -> SC.SExpr Atom
convertElt _ (S.SemiRingLiteral S.SemiRingNat _ _) = error "NatElt not supported"
convertElt _ (S.SemiRingLiteral S.SemiRingInt _ _) = error "IntElt not supported"
convertElt _ (S.SemiRingLiteral S.SemiRingReal _ _) = error "RatElt not supported"
convertElt _ (S.BVElt sz val _) = SC.SAtom (ABV (widthVal sz) val)
convertElt paramLookup (S.AppElt appElt) = convertAppElt paramLookup appElt
convertElt _ (S.NonceAppElt _) = error "NonceAppElt not supported"
convertElt paramLookup (S.BoundVarElt var) = fromJust $ paramLookup var

convertAppElt :: ParamLookup t -> S.AppElt t tp -> SC.SExpr Atom
convertAppElt paramLookup = convertApp paramLookup . S.appEltApp

convertApp :: forall t tp. ParamLookup t -> S.App (S.Elt t) tp -> SC.SExpr Atom
convertApp paramLookup = fromFoldable' . convertApp'
  where convert :: forall tp'. S.Elt t tp' -> SC.SExpr Atom
        convert = convertElt paramLookup

        convertApp' :: S.App (S.Elt t) tp -> [SC.SExpr Atom]
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
          where i = natValue n + j + 1
                j = natValue idx
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
        convertApp' (S.BVZext r bv) = extend "zero" (natValue r) bv
        convertApp' (S.BVSext r bv) = extend "sign" (natValue r) bv
        convertApp' (S.BVTrunc r bv) = extract (natValue r - 1) 0 bv
        convertApp' (S.BVBitNot _ bv) = [ident "bvnot", convert bv]
        convertApp' (S.BVBitAnd _ bv1 bv2) = [ident "bvand", convert bv1, convert bv2]
        convertApp' (S.BVBitOr _ bv1 bv2) = [ident "bvor", convert bv1, convert bv2]
        convertApp' (S.BVBitXor _ bv1 bv2) = [ident "bvxor", convert bv1, convert bv2]
        convertApp' app = error $ "unhandled App: " ++ show app

        extract :: forall tp'. Integer -> Integer -> S.Elt t tp' -> [SC.SExpr Atom]
        extract i j bv = [fromFoldable' [ident "_", ident "extract", int i, int j],
                          convert bv]

        extend :: forall w. String -> Integer -> S.Elt t (BaseBVType w) -> [SC.SExpr Atom]
        extend op r bv = [fromFoldable' [ident "_", ident (op ++ "_extend"), int extension],
                          convert bv]
          where extension = r - w
                w = case S.exprType bv of BaseBVRepr len -> natValue len

-- | Extract the name, as a String, of a wrapped bound variable.
varName :: BV.BoundVar (S.SimpleBuilder t st) arch op -> String
varName (BV.BoundVar var) = show (S.bvarName var)

-- | Turn any 'Foldable' into an s-expression by transforming each element with
-- the given function, then assembling as you would expect.
fromFoldable :: (Foldable f) => (a -> SC.SExpr atom) -> f a -> SC.SExpr atom
fromFoldable f = foldr (SC.SCons . f) SC.SNil

-- | @fromFoldable id@
fromFoldable' :: (Foldable f) => f (SC.SExpr atom) -> SC.SExpr atom
fromFoldable' = fromFoldable id

-- | Lift an unquoted identifier.
ident :: String -> SC.SExpr Atom
ident = SC.SAtom . AIdent

-- | Lift a quoted identifier.
quoted :: String -> SC.SExpr Atom
quoted = SC.SAtom . AQuoted

-- | Lift an integer.
int :: Integer -> SC.SExpr Atom
int = SC.SAtom . AInt

-- | Class for shape-dependent operations.
--
-- This is at the bottom of the file so as to not screw up code formatting for
-- the rest (due to haskell-mode not behaving nicely with TypeOperators).
class ConvertShape (sh :: [Symbol]) where
  -- | Convert the 'ShapedList' of variables into the serialized s-expression
  -- form, e.g. @((ra . 'Gprc) (imm . 'I32))@.
  convertOperandVars :: ShapedList (BV.BoundVar (S.SimpleBuilder t st) arch) sh -> SC.SExpr Atom

instance ConvertShape '[] where
  convertOperandVars Nil = SC.SNil

instance (KnownSymbol s, ConvertShape sh) => ConvertShape (s ': sh) where
  convertOperandVars (var :> rest) =
    SC.SCons (SC.SCons nameExpr typeExpr) (convertOperandVars rest)
    where nameExpr = ident (varName var)
          typeExpr = quoted (symbolVal (Proxy :: Proxy s))
