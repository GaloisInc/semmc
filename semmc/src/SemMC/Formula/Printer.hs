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
{-# LANGUAGE PatternSynonyms #-}

{--

This module writes out SemMC Formula and
ParameterizedFormula from as s-expressions which are later
parsed by the code in SemMC.Formula.Parser.

The bodies of the Formula and ParameterizedFormula are
serialized using a _generic interface_ for serializing What4
expressions and are then wrapped in an s-expression containing
enough metadata to rebuild the respective SemMC Formula or
ParameterizedFormula when parsed back in (by the code in
SemMC.Formula.Parser).

E.g., a ParameterizedFormula is roughly serialized out as follows:

((operands
 ((OP_NAME OP_TYPE) ...))
 (in (PARAM_1 ... ))
 (defs
  ((NAME
    (with
     ((LOCAL_NAME SEMMC_NAME) ...)
     SERIALIZED_BODY_EXPRESSION))
   ...)))

The top-level forms/info comes directly from the contents of
a ParameterizedFormula. Each `SERIALIZED_BODY_EXPRESSION`
within the right-hand side of the bindings pairs in the
`defs` section is serialized using the _generic_ What4
expression printer. To be sane/sound, that What4 serialization
uses _fresh_ names for encountered What4 BoundVars and SymFns
(since their human-readable names are not guaranteed to be unique).
We record those fresh names along with their corresponding
BoundVar/SymFn in the surrounding `with` binding clauses;
this way when we parse back in the
`SERIALIZED_BODY_EXPRESSION` we can parameterize the What4
parser to insert the correct values when encountering those
fresh BoundVar/SymFn names.


There are a few misc. important things to note:

1. SemMC.DSL _also_ has code to generate s-expressions that
are intended to be compatible with the s-expressions
generated here. So... be careful to keep this printer and
the s-expression generating code in SemMC.DSL in sync!

2. There are s-expression artifacts checked into this git
repo (search for `*.sem` and `*.fun`). Changes to
printing/parsing need to take note of this and regenerate
those artifacts when necessary. Some are generated by this
printer (or the DSL printer) and there should be a
corresponding `semmc-BLAH-genbase` executable described in
the appropriate cabal file, others are small and written by
hand for the sake of simple testing (e.g., the semmc-toy
tests).

--}

module SemMC.Formula.Printer
  ( printParameterizedFormula
  , printFormula
  , printFunctionFormula
  , printSExpr
  ) where

import qualified Data.Map as Map
import qualified Data.Map.Ordered as OMap
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair
import           Data.Parameterized.Some ( Some(..), viewSome )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           Data.Text ( Text )
import qualified Data.Text as T

import qualified What4.Expr as S
import qualified What4.Expr.Builder as S
import qualified What4.Symbol as S
import           What4.Serialize.Printer ( serializeExprWithConfig
                                         , serializeBaseType
                                         , Config(..)
                                         , Result(..)
                                         , SomeExprSymFn(..)
                                         , printSExpr
                                         , defaultConfig
                                         , SExpr
                                         , ident
                                         , pattern L
                                         )

import qualified What4.BaseTypes as WT
import qualified What4.Interface as WI


import qualified SemMC.Architecture as A
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Formula

-- This file is organized top-down, i.e., from high-level to low-level.

-- | Serialize a 'ParameterizedFormula' into its textual s-expression form.
printParameterizedFormula :: (A.Architecture arch)
                          => A.ShapeRepr arch sh
                          -> ParameterizedFormula (S.ExprBuilder t st fs) arch sh
                          -> Text
printParameterizedFormula rep =
  printSExpr mempty . sexprConvertParameterized rep

printFormula :: (ShowF (A.Location arch))
             => Formula (S.ExprBuilder t st fs) arch
             -> Text
printFormula = printSExpr mempty . sexprConvert
  where sexprConvert :: forall t st fs arch
                        . (ShowF (A.Location arch))
                     => Formula (S.ExprBuilder t st fs) arch
                     -> SExpr
        sexprConvert f = let bodies = (map
                                       (convertSimpleDef
                                        (Proxy @arch)
                                        (formParamVars f))
                                       (MapF.toList (formDefs f)))
                         in L $ (ident "defs") : bodies

printFunctionFormula :: FunctionFormula (S.ExprBuilder t st fs) tps
                     -> T.Text
printFunctionFormula = printSExpr mempty . sexprConvertFunction

convertSimpleDef :: forall arch proxy t
                  . (ShowF (A.Location arch))
                 => proxy arch
                 -> MapF.MapF (A.Location arch) (S.ExprBoundVar t)
                 -> MapF.Pair (A.Location arch) (S.Expr t)
                 -> SExpr
convertSimpleDef _ paramVars (MapF.Pair loc expr) =
  L [ convertLocation loc
    , L [ident "with", L (exprs++symFns), sexpr]]
  where
    Result { resSExpr = sexpr
           , resFreeVarEnv = freeVarNameTable
           , resSymFnEnv = symFnNameTable
           } = serializeExprWithConfig
               defaultConfig { cfgAllowFreeVars = True
                             , cfgAllowFreeSymFns = True
                             }
               expr
    bvLocTable = Map.fromList [ (Some bv, convertLocation l)
                              | MapF.Pair l bv <- MapF.toList paramVars
                              ]
    symFns :: [SExpr]
    symFns = [ L [ ident nm, convertSomeSymFn sfn ]
             | (sfn, nm) <- OMap.toAscList symFnNameTable
             ]
    exprs :: [SExpr]
    exprs = [ L [ident nm, lsexp]
            | (bv, nm) <- OMap.toAscList freeVarNameTable
            , let lsexp = case Map.lookup bv bvLocTable of
                            Just l -> l
                            Nothing ->
                              error $ ("ERROR! While converting simple semmc def,"
                                        ++" we encountered a bound variable `"
                                        ++(showSomeExprBoundVar bv)
                                        ++"` of type "
                                        ++(showSomeExprBoundVarType bv)
                                        ++" which did not correspond to a known"
                                        ++" architecture location.")
            ]

showSomeExprBoundVar :: Some (S.ExprBoundVar t) -> String
showSomeExprBoundVar (Some bv) = T.unpack $ S.solverSymbolAsText $ S.bvarName bv

showSomeExprBoundVarType :: Some (S.ExprBoundVar t) -> String
showSomeExprBoundVarType (Some bv) = (show $ S.bvarType bv)


-- | Intermediate serialization.
sexprConvertParameterized :: (A.Architecture arch)
                          => A.ShapeRepr arch sh
                          -> ParameterizedFormula (S.ExprBuilder t st fs) arch sh
                          -> SExpr
sexprConvertParameterized rep (ParameterizedFormula { pfUses = uses
                                                    , pfOperandVars = opVars
                                                    , pfLiteralVars = litVars
                                                    , pfDefs = defs
                                                    }) =
  L [ L [ident "operands", convertOperandVars rep opVars]
       , L [ident "in", convertUses opVars uses]
       , L [ident "defs", convertDefs opVars litVars defs]
       ]

sexprConvertFunction :: forall t st fs tps
                      . FunctionFormula (S.ExprBuilder t st fs) tps
                     -> SExpr
sexprConvertFunction (FunctionFormula { ffName = name
                                      , ffArgTypes = argTypes
                                      , ffArgVars = argVars
                                      , ffRetType = retType
                                      , ffDef = def
                                      }) =
  let
    expr = case S.symFnInfo def of
      S.DefinedFnInfo _ expr' _ -> expr'
      _ -> error "sexprConvertFunction: expected defined function"
    Result body freeVarNameTable symFnNameTable =
      serializeExprWithConfig
        defaultConfig { cfgAllowFreeVars = True
                      , cfgAllowFreeSymFns = True}
        expr
    exprs :: [SExpr]
    exprs = [ L [ident nm, ident (argumentName bv) ]
            | (Some bv, nm) <- OMap.toAscList freeVarNameTable
            ]
    symFns :: [SExpr]
    symFns = [ L [ ident nm, convertSomeSymFn sfn ]
             | (sfn, nm) <- OMap.toAscList symFnNameTable
             ]
  in L [ L [ ident "function", ident (T.pack name)]
       , L [ ident "arguments", convertArgumentVars argTypes argVars ]
       , L [ ident "return", serializeBaseType retType ]
       , L [ ident "body", L [ ident "with", L (exprs++symFns), body ] ]
       ]

argumentName :: WI.BoundVar (S.ExprBuilder t st fs) tp -> T.Text
argumentName bv = "op." <> S.solverSymbolAsText (S.bvarName bv)

convertSomeSymFn :: SomeExprSymFn sym -> SExpr
convertSomeSymFn (SomeExprSymFn fn) =
  let
    rawnm = S.solverSymbolAsText $ S.symFnName fn
    prefix = case S.symFnInfo fn of
      S.DefinedFnInfo{} -> "df."
      S.UninterpFnInfo{} -> "uf."
      _ -> error "convertSomeSymFn: unexpected function kind"
  in ident (prefix <> rawnm)

convertUses :: (ShowF (A.Location arch))
            => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
            -> Set.Set (Some (Parameter arch sh))
            -> SExpr
convertUses oplist = L . fmap (viewSome (convertParameter oplist)) . Set.toList

convertParameter :: (ShowF (A.Location arch))
                 => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
                 -> Parameter arch sh tp
                 -> SExpr
convertParameter opVars (OperandParameter _ idx) = convertOperandIdx opVars idx
convertParameter _ (LiteralParameter loc) = convertLocation loc
convertParameter opVars (FunctionParameter fnName (WrappedOperand orep oix) _) =
  L [ ident "call"
    , ident $ T.pack fnName
    , convertParameter opVars (OperandParameter orep oix)
    ]


convertDefs :: forall t st fs arch sh.
               (ShowF (A.Location arch))
            => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
            -> MapF.MapF (A.Location arch) (S.ExprBoundVar t)
            -> MapF.MapF (Parameter arch sh) (S.Expr t)
            -> SExpr
convertDefs opVars locVars = L . fmap (convertDef opVars paramMapping) . MapF.toList
  where
    paramMapping :: Map.Map (Some (S.ExprBoundVar t)) SExpr
    paramMapping = MapF.foldrWithKey insertLoc (buildOpMapping opVars) locVars
    insertLoc ::
      A.Location arch tp
      -> S.ExprBoundVar t tp
      -> Map.Map (Some (S.ExprBoundVar t)) SExpr
      -> Map.Map (Some (S.ExprBoundVar t)) SExpr
    insertLoc loc var = Map.insert (Some var) (convertLocation loc)

convertLocation :: (ShowF loc) => loc tp -> SExpr
convertLocation l = ident $ T.pack $ "loc."++(showF l)

convertOperandIdx ::
  SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
  -> SL.Index sh n
  -> SExpr
convertOperandIdx opVars idx  = convertOperandName name
  where name = varName (opVars SL.!! idx)

convertOperandName :: Text -> SExpr
convertOperandName name = ident $ T.concat ["op.", name]

-- | For use in the parameter lookup function.
buildOpMapping :: SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
               -> Map.Map (Some (S.ExprBoundVar t)) SExpr
buildOpMapping SL.Nil = Map.empty
buildOpMapping (var SL.:< rest) =
  Map.insert (Some (BV.unBoundVar var)) (convertOperandName name) $ buildOpMapping rest
  where name = varName var

convertDef :: (ShowF (A.Location arch))
           => SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
           -> Map.Map (Some (S.ExprBoundVar t)) SExpr
           -> Pair (Parameter arch sh) (S.Expr t)
           -> SExpr
convertDef opVars opMapping (Pair param expr) =
  L [ convertParameter opVars param
    , L [ident "with", L (exprs++symFns), sexpr]
    ]
  where
    Result { resSExpr = sexpr
           , resFreeVarEnv = freeVarNameTable
           , resSymFnEnv = symFnNameTable
           } = serializeExprWithConfig
               defaultConfig { cfgAllowFreeVars = True
                             , cfgAllowFreeSymFns = True
                             }
               expr
    symFns :: [SExpr]
    symFns = [ L [ ident nm, convertSomeSymFn sfn ]
             | (sfn, nm) <- OMap.toAscList symFnNameTable
             ]
    exprs :: [SExpr]
    exprs = [ L [ident nm, lsexp]
            | (bv, nm) <- OMap.toAscList freeVarNameTable
            , let lsexp = case Map.lookup bv opMapping of
                            Just l -> l
                            Nothing ->
                              error $ ("[convertDef] SERIALIZATION ERROR!"
                                        ++" While converting semmc def,"
                                        ++" we encountered a bound variable `"
                                        ++(showSomeExprBoundVar bv)
                                        ++"` of type "
                                        ++(showSomeExprBoundVarType bv)
                                        ++" which did not correspond to a known operator.")
            ]

-- | Extract the name, as a String, of a wrapped bound variable.
varName :: BV.BoundVar (S.ExprBuilder t st fs) arch op -> Text
varName (BV.BoundVar var) = S.solverSymbolAsText (S.bvarName var)


convertOperandVars :: forall arch sh t st fs
                    . (A.Architecture arch)
                   => A.ShapeRepr arch sh
                   -> SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh
                   -> SExpr
convertOperandVars x y = L $  go x y
  where go ::
          A.ShapeRepr arch sh'
          -> SL.List (BV.BoundVar (S.ExprBuilder t st fs) arch) sh'
          -> [SExpr]
        go rep l =
          case (rep, l) of
            (SL.Nil, SL.Nil) -> []
            (r SL.:< rep', var SL.:< rest) ->
              let nameExpr = ident (varName var)
                  typeExpr = ident $ T.pack (A.operandTypeReprSymbol (Proxy @arch) r)
              in (L [nameExpr, typeExpr]):(go rep' rest)

convertArgumentVars :: forall sh t st fs
                     . SL.List WT.BaseTypeRepr sh
                    -> SL.List (WI.BoundVar (S.ExprBuilder t st fs)) sh
                    -> SExpr
convertArgumentVars rep0 l0 = L (go rep0 l0)
  where
    go :: forall sh'
        . SL.List WT.BaseTypeRepr sh'
       -> SL.List (WI.BoundVar (S.ExprBuilder t st fs)) sh'
       -> [SExpr]
    go rep l =
      case (rep, l) of
        (SL.Nil, SL.Nil) -> []
        (r SL.:< rep', var SL.:< rest) ->
          let nameExpr = ident (S.solverSymbolAsText (S.bvarName var))
              typeExpr = serializeBaseType r
          in (L [ nameExpr, typeExpr ]) : go rep' rest
