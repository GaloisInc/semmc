{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SemMC.ASL.SyntaxTraverse
  ( mkSyntaxOverrides
  , applySyntaxOverridesInstrs
  , applySyntaxOverridesDefs
  , SyntaxCollectors(..)
  , noCollectors
  , SyntaxMaps(..)
  , noMaps
  , mapsToCollector
  , traverseExpr
  , traverseStmt
  , SyntaxWrites(..)
  , noWrites
  , writeExpr
  , writeStmt
  , mapSyntax
  , mkFunctionName
  , mapInnerName
  )
where

import           Control.Applicative
import qualified Control.Monad.Writer.Lazy as W
import qualified Language.ASL.Syntax as AS
import qualified Data.Text as T
import           Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.State as MSS
import           Data.Maybe (maybeToList, catMaybes, fromMaybe, listToMaybe, isJust, mapMaybe)
import           SemMC.ASL.Types
import           SemMC.ASL.StaticExpr

-- | Syntactic-level expansions that should happen aggressively before
-- any interpretation.

applySyntaxOverridesDefs :: SyntaxMaps -> [AS.Definition] -> [AS.Definition]
applySyntaxOverridesDefs maps defs =
  let
    f :: forall e. MapSyntax e => e -> e
    f = mapSyntax maps


    -- TODO: For sanity we delete setter definitions which require
    -- pass-by-reference since we don't have a sane semantics for this

    argName (AS.SetterArg name False) = Just name
    argName _ = Nothing

    mapDecl (i, t) = (i, f t)

    mapIxType ix = case ix of
      AS.IxTypeRange e e' -> AS.IxTypeRange (f e) (f e')
      _ -> ix

    mapDefs d = case d of
      AS.DefCallable qName args rets stmts ->
        [AS.DefCallable qName (mapDecl <$> args) (f <$> rets) (f <$> stmts)]
      AS.DefGetter qName args rets stmts ->
        [AS.DefCallable (mkGetterName (isJust args) qName)
         (mapDecl <$> (concat $ maybeToList args)) (f <$> rets) (f <$> stmts)]
      AS.DefSetter qName args rhs stmts -> maybeToList $ do
        argNames <- sequence (argName <$> (concat $ maybeToList args))
        Just $ AS.DefCallable { callableName = mkSetterName (isJust args) qName
                       , callableArgs = mapDecl <$> (rhs : argNames)
                       , callableRets = []
                       , callableStmts = f <$> stmts
                       }
      AS.DefConst i t e -> [AS.DefConst i (f t) (f e)]
      AS.DefTypeStruct i ds -> [AS.DefTypeStruct i (mapDecl <$> ds)]
      AS.DefArray i t ixt -> [AS.DefArray i (f t) (mapIxType ixt)]
      AS.DefVariable i t -> [AS.DefVariable i (f t)]
      _ -> [d]

  in concat $ mapDefs <$> defs

applySyntaxOverridesInstrs :: SyntaxMaps -> [AS.Instruction] -> [AS.Instruction]
applySyntaxOverridesInstrs maps instrs =
  let
    f :: forall e. MapSyntax e => e -> e
    f = mapSyntax maps

    mapInstr (AS.Instruction instName instEncodings instPostDecode instExecute conditional) =
      AS.Instruction instName (mapEnc <$> instEncodings) (f <$> instPostDecode) (f <$> instExecute) conditional

    mapEnc (AS.InstructionEncoding a b c d encGuard encUnpredictable encDecode) =
      AS.InstructionEncoding a b c d (f <$> encGuard) encUnpredictable (f <$> encDecode)

  in mapInstr <$> instrs


prepASL :: ([AS.Instruction], [AS.Definition]) -> ([AS.Instruction], [AS.Definition])
prepASL (instrs, defs) =
  let ovrs = mkSyntaxOverrides defs
  in (applySyntaxOverridesInstrs ovrs instrs, applySyntaxOverridesDefs ovrs defs)


data InternalOverride = InternalOverride
  { iovGetters :: Set.Set T.Text
  , iovSetters :: Set.Set T.Text
  , iovInlineGetters :: Map.Map (T.Text, Int) ([AS.Expr] -> AS.Expr)
  , iovInlineSetters :: Map.Map (T.Text, Int) ([AS.Expr] -> AS.Stmt)
  }

emptyInternalOverride :: InternalOverride
emptyInternalOverride = InternalOverride Set.empty Set.empty Map.empty Map.empty

mkIdentOverride :: (AS.QualifiedIdentifier -> AS.QualifiedIdentifier) -> SyntaxMaps
mkIdentOverride f =
  let
    exprOverride e = case e of
      AS.ExprVarRef qident -> AS.ExprVarRef (f qident)
      _ -> e
    lvalOverride lv = case lv of
      AS.LValVarRef qident -> AS.LValVarRef (f qident)
      _ -> lv
  in
    SyntaxMaps exprOverride lvalOverride id id

exprToLVal :: AS.Expr -> AS.LValExpr
exprToLVal e = case e of
  AS.ExprVarRef qident -> AS.LValVarRef qident
  AS.ExprIndex e slices -> AS.LValArrayIndex (exprToLVal e) slices
  AS.ExprSlice e slices -> AS.LValSliceOf (exprToLVal e) slices
  AS.ExprMembers e [mem] -> AS.LValMember (exprToLVal e) mem
  AS.ExprTuple es -> AS.LValTuple (map exprToLVal es)
  _ -> error $ "Invalid inline for expr:" <> show e

-- Some gross hackery to avoid variable name clashes
mkVarMap :: [AS.SymbolDecl] -> [AS.Expr] -> (AS.LValExpr -> AS.LValExpr, AS.Expr -> AS.Expr)
mkVarMap syms args = if length syms == length args then
  let
    swap = zip (map fst syms) args

    typeVars = mapMaybe bvSize syms

    bvSize (v, AS.TypeFun "bits" (AS.ExprVarRef (AS.QualifiedIdentifier _ nm))) =
      Just (nm, v)
    bvSize _ = Nothing

    externVars (AS.QualifiedIdentifier q nm) = AS.QualifiedIdentifier q ("EXTERN_" <> nm)

    unexternVars qid@(AS.QualifiedIdentifier q nm) = case T.stripPrefix "EXTERN_" nm of
      Just nm' -> (AS.QualifiedIdentifier q nm')
      _ -> qid

    exprOverride e = case e of
      AS.ExprVarRef (AS.QualifiedIdentifier _ nm)
        | Just e' <- nm `lookup` swap ->
          mapSyntax (mkIdentOverride externVars) e'
      AS.ExprVarRef (AS.QualifiedIdentifier _ nm)
        | Just t' <- nm `lookup` typeVars ->
          case t' `lookup` swap of
            Just e' ->
              mapSyntax (mkIdentOverride externVars)
                (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "sizeOf") [e'])
      AS.ExprVarRef (AS.QualifiedIdentifier _ nm)
        | Just t' <- nm `lookup` typeVars
        , Nothing <- t' `lookup` swap ->
          error $  "Missing argument for inlined type variable: "++ show t'
      _ -> e

    lvalOverride e = case e of
      AS.LValVarRef (AS.QualifiedIdentifier _ nm)
        | Just e' <- nm `lookup` swap ->
          let lv' = exprToLVal e'
          in mapSyntax (mkIdentOverride externVars) lv'
      _ -> e

    override = SyntaxMaps exprOverride lvalOverride id id

  in (mapSyntax (mkIdentOverride unexternVars) . mapSyntax override,
      mapSyntax (mkIdentOverride unexternVars) . mapSyntax override)
  else error $ "Mismatch in inlined function application: " <> show syms <> " " <> show args

simpleReturn :: [AS.Stmt] -> Maybe AS.Expr
simpleReturn stmts =
  getSimple stmts
  where
    getSimple (AS.StmtAssert _ : stmts') = getSimple stmts'
    getSimple [AS.StmtReturn ret] = ret
    getSimple _ = Nothing

simpleAssign :: [AS.Stmt] -> Maybe AS.LValExpr
simpleAssign stmts =
  getSimple stmts
  where
    getSimple (AS.StmtAssert _ : stmts') = getSimple stmts'
    getSimple [AS.StmtAssign lv _, AS.StmtReturn Nothing] = Just lv
    getSimple _ = Nothing


mkSyntaxOverrides :: [AS.Definition] -> SyntaxMaps
mkSyntaxOverrides defs =
  let
      inlineGetterNames = []
        --["Elem"]
      inlineSetterNames = ["Elem"]
        --["Elem", "Qin", "Din", "Q"]
      addInternalOverride d iovrs = case d of

        AS.DefGetter (AS.QualifiedIdentifier _ nm) (Just args) _ stmts
          | nm `elem` inlineGetterNames
          , Just ret <- simpleReturn stmts ->
          let inline es =
                let (_, f) = mkVarMap args es in f ret
          in
          iovrs { iovInlineGetters = Map.insert (nm, length args) inline (iovInlineGetters iovrs) }

        AS.DefSetter (AS.QualifiedIdentifier _ nm) (Just args) val stmts
          | nm `elem` inlineSetterNames
          , Just lv <- simpleAssign stmts ->

          let
            args' = val : map (\(AS.SetterArg arg _) -> arg) args
            inline es@(e : _) =
              let (f, _) = mkVarMap args' es
                in AS.StmtAssign (f lv) e
          in
            iovrs { iovInlineSetters = Map.insert (nm, length args) inline (iovInlineSetters iovrs) }

        AS.DefGetter qName (Just args) _ _ ->
          iovrs { iovGetters = Set.insert (mkFunctionName (mkGetterName True qName) (length args)) (iovGetters iovrs) }
        AS.DefGetter qName Nothing _ _ ->
          iovrs { iovGetters = Set.insert (mkFunctionName (mkGetterName False qName) 0) (iovGetters iovrs) }
        AS.DefSetter qName (Just args) _ _ ->
           iovrs { iovSetters = Set.insert (mkFunctionName (mkSetterName True qName) (length args + 1)) (iovSetters iovrs) }
        AS.DefSetter qName Nothing _ _ ->
           iovrs { iovSetters = Set.insert (mkFunctionName (mkSetterName False qName) 1) (iovSetters iovrs) }
        _ -> iovrs

      InternalOverride getters setters inlineGetters inlineSetters =
        foldr addInternalOverride emptyInternalOverride defs

      getSliceExpr slice = case slice of
        AS.SliceSingle e -> e
        _ -> error "Unexpected slice argument."

      assignOverrides lv = case lv of
        AS.LValArrayIndex (AS.LValVarRef (AS.QualifiedIdentifier _ nm)) slices
          | Just f <- Map.lookup (nm, length slices) inlineSetters ->
            Just $ (\rhs -> stmtOverrides $ f (rhs : (map getSliceExpr slices)))
        AS.LValArrayIndex (AS.LValVarRef qName) slices
          | Set.member (mkFunctionName (mkSetterName True qName) (length slices + 1)) setters ->
            Just $ (\rhs -> AS.StmtCall (mkSetterName True qName) (rhs : map getSliceExpr slices))
        AS.LValTuple lvs
          | lvs' <- map assignOverrides lvs
          , not $ null (catMaybes lvs') -> Just $ \rhs -> do
            let vars = take (length lvs') $
                  map (\i -> "__tupleResult" <> T.pack (show i)) ([0..] :: [Integer])
            let mkVar nm = AS.QualifiedIdentifier AS.ArchQualAny nm
            let getlv (i, (mlv', lv)) = case mlv' of
                  Just lv' -> AS.LValVarRef (mkVar $ vars !! i)
                  _ -> lv
            let tuple = map getlv (zip [0..] (zip lvs' lvs))
            let asnResult (i, mlv') = case mlv' of
                  Just lv' -> Just $ lv' (AS.ExprVarRef $ mkVar $ vars !! i)
                  Nothing -> Nothing
            let stmts' =
                  [ AS.StmtAssign (AS.LValTuple tuple) rhs ]
                  ++ catMaybes (map asnResult (zip [0..] lvs'))
            letInStmt vars stmts'

        AS.LValSliceOf (AS.LValArrayIndex (AS.LValVarRef qName) slices) outerSlices
          | Set.member (mkFunctionName (mkSetterName True qName) (length slices + 1)) setters ->
            Just $ \rhs -> do
              let getter = mkGetterName True qName
              let setter = mkSetterName True qName

              let mkIdent nm = AS.QualifiedIdentifier AS.ArchQualAny nm
              let mkVar nm = AS.ExprVarRef (mkIdent nm)
              let args = map getSliceExpr slices
              let old = "__oldGetterValue"
              let width = AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "sizeOf") [mkVar old]
              let mask = "__maskedGetterValue"
              let stmts =
                    [ AS.StmtAssign (AS.LValVarRef $ mkIdent old)
                       (AS.ExprCall getter args)
                    ,  AS.StmtAssign (AS.LValVarRef $ mkIdent mask)
                       (AS.ExprCall (mkIdent "Ones") [width])
                    , AS.StmtAssign (AS.LValSliceOf (AS.LValVarRef $ mkIdent mask) outerSlices)
                       rhs
                    , AS.StmtCall setter (AS.ExprBinOp AS.BinOpBitwiseAnd (mkVar mask) (mkVar old) : args)
                    ]
              letInStmt [old, mask] stmts
        AS.LValVarRef qName
          | Set.member (mkFunctionName (mkSetterName False qName) 1) setters ->
            Just $ \rhs -> AS.StmtCall (mkSetterName False qName) [rhs]
        _ -> Nothing

      stmtOverrides stmt = case stmt of
        AS.StmtAssign lv rhs
          | Just f <- assignOverrides lv ->
            f rhs
        AS.StmtUndefined -> mkCallStmt "ASLSetUndefined"
        AS.StmtUnpredictable -> mkCallStmt "ASLSetUnpredictable"
        AS.StmtSeeExpr {} -> mkCallStmt "ASLSetUndefined"
        AS.StmtSeeString {} -> mkCallStmt "ASLSetUndefined"
        _ -> stmt

      mkCallStmt nm = AS.StmtCall (AS.QualifiedIdentifier AS.ArchQualAny nm) []

      exprOverrides' expr = case expr of
        -- Limited support for alternate slice syntax
        AS.ExprIndex (AS.ExprVarRef (AS.QualifiedIdentifier _ nm)) slices
          | Just f <- Map.lookup (nm, length slices) inlineGetters ->
            exprOverrides' $ f (map getSliceExpr slices)
        AS.ExprIndex e slices@[AS.SliceOffset _ _] ->
          AS.ExprSlice e slices
        AS.ExprIndex e slices@[AS.SliceRange _ _] ->
          AS.ExprSlice e slices
        AS.ExprIndex (AS.ExprVarRef qName) slices
          | Set.member (mkFunctionName (mkGetterName True qName) (length slices)) getters ->
            AS.ExprCall (mkGetterName True qName) (map getSliceExpr slices)
        AS.ExprVarRef qName
          | Set.member (mkFunctionName (mkGetterName False qName) 0) getters ->
            AS.ExprCall (mkGetterName False qName) []
        _ -> expr

      lvalOverrides lval = lval

      -- FIXME: This is a simple toplevel rewrite that assumes
      -- aliases and consts are never shadowed

      typeSynonyms = catMaybes $ typeSyn <$> defs
      typeSyn d = case d of
        AS.DefTypeAlias nm t -> Just (nm, t)
        _ -> Nothing

      typeSynMap = Map.fromList (typeSynonyms ++
                                 [(T.pack "signal", (AS.TypeFun "bits" (AS.ExprLitInt 1)))])

      typeOverrides t = case t of
        AS.TypeFun "__RAM" (AS.ExprLitInt 52) -> AS.TypeFun "bits" (AS.ExprLitInt 52)
        AS.TypeRef (AS.QualifiedIdentifier _ nm) ->
          case Map.lookup nm typeSynMap of
          Just t' -> t'
          Nothing -> t
        _ -> t


      varSynonyms = catMaybes $ varSyn <$> defs
      varSyn d = case d of
        AS.DefConst id _ e -> Just (id, e)
        _ -> Nothing

      varSynMap = Map.fromList varSynonyms

      exprOverrides e = case e of
        AS.ExprVarRef (AS.QualifiedIdentifier _ nm) -> case Map.lookup nm varSynMap of
          Just e' -> e'
          Nothing -> exprOverrides' e
        _ -> exprOverrides' e

  in SyntaxMaps exprOverrides lvalOverrides stmtOverrides typeOverrides

data SyntaxCollectors t = SyntaxCollectors
  { exprCollect :: AS.Expr -> t AS.Expr
  , lvalCollect :: AS.LValExpr -> t AS.LValExpr
  , stmtCollect :: AS.Stmt -> t AS.Stmt
  , typeCollect :: AS.Type -> t AS.Type
  }

data SyntaxWrites t w = SyntaxWrites
  { exprWrite :: AS.Expr -> t w
  , lvalWrite :: AS.LValExpr -> t w
  , stmtWrite :: AS.Stmt -> t w
  , typeWrite :: AS.Type -> t w
  }


data SyntaxMaps = SyntaxMaps
  { exprMap :: AS.Expr -> AS.Expr
  , lvalMap :: AS.LValExpr -> AS.LValExpr
  , stmtMap :: AS.Stmt -> AS.Stmt
  , typeMap :: AS.Type -> AS.Type
  }


noCollectors :: Monad t => SyntaxCollectors t
noCollectors = SyntaxCollectors return return return return

noWrites :: Monoid w => Monad t => SyntaxWrites t w
noWrites = SyntaxWrites
  (\_ -> return mempty)
  (\_ -> return mempty)
  (\_ -> return mempty)
  (\_ -> return mempty)


noMaps :: SyntaxMaps
noMaps = SyntaxMaps id id id id

writeToCollector :: Monoid w => Monad t => SyntaxWrites t w -> SyntaxCollectors (W.WriterT w t)
writeToCollector SyntaxWrites{..} =
  SyntaxCollectors
    { exprCollect = liftWrite exprWrite
    , lvalCollect = liftWrite lvalWrite
    , stmtCollect = liftWrite stmtWrite
    , typeCollect = liftWrite typeWrite
    }
  where
    liftWrite f e =  W.lift (f e) >>= (\w -> W.tell w >> return e)


writeExpr :: Monoid w => Monad t => SyntaxWrites t w -> AS.Expr -> t w
writeExpr writes e = W.execWriterT (traverseExpr (writeToCollector writes) e)

writeStmt :: Monoid w => Monad t => SyntaxWrites t w -> AS.Stmt -> t w
writeStmt writes e = W.execWriterT (traverseStmt (writeToCollector writes) e)

class MapSyntax a where
  mapSyntax :: SyntaxMaps -> a -> a

instance MapSyntax AS.Expr where
  mapSyntax maps e = snd $ traverseExpr (mapsToCollector maps) e

instance MapSyntax AS.Stmt where
  mapSyntax maps e = snd $ traverseStmt (mapsToCollector maps) e

instance MapSyntax AS.Type where
  mapSyntax maps e = snd $ traverseType (mapsToCollector maps) e

instance MapSyntax AS.LValExpr where
  mapSyntax maps e = snd $ traverseLVal (mapsToCollector maps) e

mapsToCollector :: SyntaxMaps -> SyntaxCollectors ((,) ())
mapsToCollector SyntaxMaps{..} =
  SyntaxCollectors
    { exprCollect = (\e -> ((), exprMap e))
    , lvalCollect = (\lv -> ((), lvalMap lv))
    , stmtCollect = (\stmt -> ((), stmtMap stmt))
    , typeCollect = (\t -> ((), typeMap t))
    }

traverseSlice :: Monad t => SyntaxCollectors t -> AS.Slice -> t AS.Slice
traverseSlice cols slice =
  let
    f = traverseExpr cols

  in case slice of
     AS.SliceSingle e -> AS.SliceSingle <$> (f e)
     AS.SliceOffset e e' -> liftA2 AS.SliceOffset (f e) (f e')
     AS.SliceRange e e' -> liftA2 AS.SliceRange (f e) (f e')

-- | Fold over the nested expressions of a given expression
traverseExpr :: forall t. Monad t => SyntaxCollectors t -> AS.Expr -> t AS.Expr
traverseExpr cols expr =
  let
    f = traverseExpr cols -- inner expressions are recursively folded
    k = traverseType cols

    foldSetElems slice = case slice of
      AS.SetEltSingle e -> AS.SetEltSingle <$> f e
      AS.SetEltRange e e' -> liftA2 AS.SetEltRange (f e) (f e')

  in exprCollect cols expr >>= \expr' -> case expr' of
    AS.ExprSlice e slices ->
      liftA2 AS.ExprSlice (f e) (traverse (traverseSlice cols) slices)

    AS.ExprIndex e slices ->
      liftA2 AS.ExprIndex (f e)
        (traverse (traverseSlice cols) slices)
    AS.ExprUnOp uop e -> (AS.ExprUnOp uop) <$> f e
    AS.ExprBinOp bop e e' -> liftA2 (AS.ExprBinOp bop) (f e) (f e')
    AS.ExprMembers e mems -> (\e' -> AS.ExprMembers e' mems) <$> f e
    AS.ExprInMask e mask -> (\e' -> AS.ExprInMask e' mask) <$> f e
    AS.ExprMemberBits e bits -> (\e' -> AS.ExprMemberBits e' bits) <$> f e
    AS.ExprCall ident es -> (\es' -> AS.ExprCall ident es') <$> traverse f es
    AS.ExprInSet e se -> liftA2 AS.ExprInSet (f e) $ traverse foldSetElems se
    AS.ExprTuple es -> AS.ExprTuple <$> traverse f es
    AS.ExprIf pes e -> liftA2 AS.ExprIf (traverse (\(x,y) -> liftA2 (,) (f x) (f y)) pes) (f e)
    AS.ExprMember e mem -> (\e' -> AS.ExprMember e' mem) <$> f e
    AS.ExprUnknown t -> (\t' -> AS.ExprUnknown t') <$> k t
    _ -> return expr'

traverseLVal :: Monad t => SyntaxCollectors t -> AS.LValExpr -> t AS.LValExpr
traverseLVal cols lval =
  let
    h = traverseLVal cols

  in lvalCollect cols lval >>= \lval' -> case lval' of
    AS.LValMember lv mem -> (\lv' -> AS.LValMember lv' mem) <$> h lv
    AS.LValMemberArray lv idx -> (\lv' -> AS.LValMemberArray lv' idx) <$> h lv
    AS.LValArrayIndex lv slices -> liftA2 AS.LValArrayIndex (h lv) $ traverse (traverseSlice cols) slices
    AS.LValSliceOf lv slices -> liftA2 AS.LValSliceOf (h lv) $ traverse (traverseSlice cols) slices
    AS.LValArray lvs -> AS.LValArray <$> traverse h lvs
    AS.LValTuple lvs -> AS.LValTuple <$> traverse h lvs
    AS.LValMemberBits lv bits -> (\lv' -> AS.LValMemberBits lv' bits) <$> h lv
    AS.LValSlice lvs -> AS.LValSlice <$> traverse h lvs
    _ -> return lval'

traverseType :: forall t. Monad t => SyntaxCollectors t -> AS.Type -> t AS.Type
traverseType cols t =
  let
    f = traverseExpr cols
    k = traverseType cols

    foldField field = case field of
      AS.RegField i slices -> (\slices' -> AS.RegField i slices') <$>
        traverse (traverseSlice cols) slices

    foldIxType ix = case ix of
      AS.IxTypeRange e e' -> liftA2 AS.IxTypeRange (f e) (f e')
      _ -> return ix

  in typeCollect cols t >>= \t' -> case t' of
    AS.TypeFun i e -> (\e' -> AS.TypeFun i e') <$> f e
    AS.TypeOf e -> AS.TypeOf <$> f e
    AS.TypeReg i fs -> (\fs' -> AS.TypeReg i fs') <$> traverse foldField fs
    AS.TypeArray t ixt -> liftA2 AS.TypeArray (k t) (foldIxType ixt)
    _ -> return t'

traverseStmt :: forall t. Monad t => SyntaxCollectors t -> AS.Stmt -> t AS.Stmt
traverseStmt cols stmt =
  let
    g = traverseStmt cols -- inner statments are recursively folded
    f = traverseExpr cols
    h = traverseLVal cols
    k = traverseType cols

    h' (ident, ty) = (\ty' -> (ident, ty')) <$> (k ty)

    foldCases cases = case cases of
      AS.CaseWhen pats me stmts ->
        liftA2 (\me' stmts' -> AS.CaseWhen pats me' stmts')
          (traverse f me)
          (traverse g stmts)
      AS.CaseOtherwise stmts -> AS.CaseOtherwise <$> traverse g stmts

    foldCatches catches = case catches of
      AS.CatchWhen e stmts ->
        liftA2 AS.CatchWhen (f e) $ traverse g stmts
      AS.CatchOtherwise stmts -> AS.CatchOtherwise <$> traverse g stmts

  in stmtCollect cols stmt >>= \stmt' -> case stmt' of
    AS.StmtVarsDecl ty idents -> (\ty' -> AS.StmtVarsDecl ty' idents) <$> k ty
    AS.StmtVarDeclInit decl e -> liftA2 AS.StmtVarDeclInit (h' decl) (f e)
    AS.StmtConstDecl decl e -> liftA2 AS.StmtConstDecl (h' decl) (f e)
    AS.StmtAssign lv e ->  liftA2 AS.StmtAssign (h lv) (f e)
    AS.StmtCall ident es -> (\es' -> AS.StmtCall ident es') <$> traverse f es
    AS.StmtReturn me -> (\me' -> AS.StmtReturn me') <$> traverse f me
    AS.StmtAssert e -> AS.StmtAssert <$> f e
    AS.StmtIf tests melse ->
      liftA2 AS.StmtIf
        (traverse (\(e, stmts) -> liftA2 (,) (f e) (traverse g stmts)) tests)
        (traverse (\stmt'' -> traverse g stmt'') melse)
    AS.StmtCase e alts -> liftA2 AS.StmtCase (f e) (traverse foldCases alts)
    AS.StmtFor ident rng stmts -> liftA2 (\rng' stmts' -> AS.StmtFor ident rng' stmts')
      (liftA2 (,) (f $ fst rng) (f $ snd rng))
      (traverse g stmts)
    AS.StmtWhile e stmts -> liftA2 AS.StmtWhile (f e) (traverse g stmts)
    AS.StmtRepeat stmts e -> liftA2 AS.StmtRepeat (traverse g stmts) (f e)
    AS.StmtSeeExpr e -> AS.StmtSeeExpr <$> f e
    AS.StmtTry stmts ident alts -> liftA2 (\stmts' alts' -> AS.StmtTry stmts' ident alts')
      (traverse g stmts)
      (traverse foldCatches alts)
    _ -> return stmt'

-- foldDef :: (T.Text -> AS.Expr -> b -> b) ->
--            (T.Text -> AS.LValExpr -> b -> b) ->
--            (T.Text -> AS.Stmt -> b -> b) ->
--            AS.Definition -> b -> b
-- foldDef f' h' g' d b = case d of
--   AS.DefCallable (AS.QualifiedIdentifier _ ident) _ _ stmts ->
--     let
--       f = f' ident
--       h = h' ident
--       g = g' ident
--       cols = SyntaxCollectors f h g (\_ -> id)
--     in foldr (foldStmt cols) b stmts
--   _ -> b

-- foldInstruction :: (T.Text -> AS.Expr -> b -> b) ->
--                    (T.Text -> AS.LValExpr -> b -> b) ->
--                    (T.Text -> AS.Stmt -> b -> b) ->
--                    AS.Instruction -> b -> b
-- foldInstruction f' h' g' (AS.Instruction ident instEncodings instPostDecode instExecute _) b =
--   let
--     f = f' ident
--     h = h' ident
--     g = g' ident
--     cols = SyntaxCollectors f h g (\_ -> id)
--     foldEncoding (AS.InstructionEncoding {encDecode=stmts}) b' =
--       foldr (foldStmt cols) b' stmts
--   in foldr (foldStmt cols) (foldr foldEncoding b instEncodings) (instPostDecode ++ instExecute)

-- foldASL :: (T.Text -> AS.Expr -> b -> b) ->
--            (T.Text -> AS.LValExpr -> b -> b) ->
--            (T.Text -> AS.Stmt -> b -> b) ->
--   [AS.Definition] -> [AS.Instruction] -> b -> b
-- foldASL f h g defs instrs b = foldr (foldInstruction f h g) (foldr (foldDef f h g) b defs) instrs


getterText :: Bool -> T.Text
getterText withArgs = if withArgs then "GETTER_" else "BAREGETTER_"

mkGetterName :: Bool -> AS.QualifiedIdentifier -> AS.QualifiedIdentifier
mkGetterName withArgs = do
  mapInnerName (\s -> getterText withArgs <> s)

setterText :: Bool -> T.Text
setterText withArgs = if withArgs then "SETTER_" else "BARESETTER_"

mkSetterName :: Bool -> AS.QualifiedIdentifier -> AS.QualifiedIdentifier
mkSetterName withArgs = mapInnerName (\s -> setterText withArgs <> s)

-- | Make a function name given its ASL name and arity.
mkFunctionName :: AS.QualifiedIdentifier -> Int -> T.Text
mkFunctionName name numArgs = collapseQualID name <> T.pack "_" <> T.pack (show numArgs)


collapseQualID :: AS.QualifiedIdentifier -> T.Text
collapseQualID (AS.QualifiedIdentifier AS.ArchQualAArch64 name) = "AArch64_" <> name
collapseQualID (AS.QualifiedIdentifier AS.ArchQualAArch32 name) = "AArch32_" <> name
collapseQualID (AS.QualifiedIdentifier _ name) = name

mapInnerName :: (T.Text -> T.Text) -> AS.QualifiedIdentifier -> AS.QualifiedIdentifier
mapInnerName f (AS.QualifiedIdentifier q name) = AS.QualifiedIdentifier q (f name)
