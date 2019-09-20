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

module SemMC.ASL.SyntaxTraverse
  ( mkSyntaxOverrides
  , applySyntaxOverridesInstrs
  , applySyntaxOverridesDefs
  , SyntaxOverrides
  , foldASL
  , foldExpr
  , mkFunctionName
  , mapInnerName
  )
where

import qualified Language.ASL.Syntax as AS
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe (maybeToList, catMaybes, fromMaybe, listToMaybe, isJust)

-- | Syntactic-level expansions that should happen aggressively before
-- any interpretation.

-- FIXME: Lots of rewriting to handle globals which should go away when
-- we are properly handling the register definitions file
data SyntaxOverrides = SyntaxOverrides { stmtOverrides :: AS.Stmt -> AS.Stmt
                                       , exprOverrides :: AS.Expr -> AS.Expr
                                       , typeOverrides :: AS.Type -> AS.Type
                                       , lvalOverrides :: AS.LValExpr -> AS.LValExpr
                                       }

applySyntaxOverridesDefs :: SyntaxOverrides -> [AS.Definition] -> [AS.Definition]
applySyntaxOverridesDefs ovrs defs =
  let
    g = applyStmtSyntaxOverride ovrs
    f = applyExprSyntaxOverride ovrs
    h = applyTypeSyntaxOverride ovrs


    -- TODO: For sanity we delete setter definitions which require
    -- pass-by-reference since we don't have a sane semantics for this

    argName (AS.SetterArg name False) = Just name
    argName _ = Nothing

    mapDecl (i, t) = (i, h t)

    mapIxType ix = case ix of
      AS.IxTypeRange e e' -> AS.IxTypeRange (f e) (f e')
      _ -> ix

    mapDefs d = case d of
      AS.DefCallable qName args rets stmts ->
        [AS.DefCallable qName (mapDecl <$> args) (h <$> rets) (g <$> stmts)]
      AS.DefGetter qName args rets stmts ->
        [AS.DefCallable (mkGetterName (isJust args) qName)
         (mapDecl <$> (concat $ maybeToList args)) (h <$> rets) (g <$> stmts)]
      AS.DefSetter qName args rhs stmts -> maybeToList $ do
        argNames <- sequence (argName <$> (concat $ maybeToList args))
        Just $ AS.DefCallable { callableName = mkSetterName (isJust args) qName
                       , callableArgs = mapDecl <$> (rhs : argNames)
                       , callableRets = []
                       , callableStmts = g <$> stmts
                       }
      AS.DefConst i t e -> [AS.DefConst i (h t) (f e)]
      AS.DefTypeStruct i ds -> [AS.DefTypeStruct i (mapDecl <$> ds)]
      AS.DefArray i t ixt -> [AS.DefArray i (h t) (mapIxType ixt)]
      AS.DefVariable i t -> [AS.DefVariable i (h t)]
      _ -> [d]

  in concat $ mapDefs <$> defs

applySyntaxOverridesInstrs :: SyntaxOverrides -> [AS.Instruction] -> [AS.Instruction]
applySyntaxOverridesInstrs ovrs instrs =
  let
    g = applyStmtSyntaxOverride ovrs
    f = applyExprSyntaxOverride ovrs

    mapInstr (AS.Instruction instName instEncodings instPostDecode instExecute conditional) =
      AS.Instruction instName (mapEnc <$> instEncodings) (g <$> instPostDecode) (g <$> instExecute) conditional

    mapEnc (AS.InstructionEncoding a b c d encGuard encUnpredictable encDecode) =
      AS.InstructionEncoding a b c d (f <$> encGuard) encUnpredictable (g <$> encDecode)

  in mapInstr <$> instrs


prepASL :: ([AS.Instruction], [AS.Definition]) -> ([AS.Instruction], [AS.Definition])
prepASL (instrs, defs) =
  let ovrs = mkSyntaxOverrides defs
  in (applySyntaxOverridesInstrs ovrs instrs, applySyntaxOverridesDefs ovrs defs)



mkSyntaxOverrides :: [AS.Definition] -> SyntaxOverrides
mkSyntaxOverrides defs =
  let getters = Set.fromList $ catMaybes $ getterName <$> defs
      setters = Set.fromList $ catMaybes $ setterName <$> defs

      getterName d = case d of
        AS.DefGetter qName (Just args) _ _ ->
          Just $ mkFunctionName (mkGetterName True qName) (length args)
        AS.DefGetter qName Nothing _ _ ->
          Just $ mkFunctionName (mkGetterName False qName) 0
        _ -> Nothing

      setterName d = case d of
        AS.DefSetter qName (Just args) _ _ ->
           Just $ mkFunctionName (mkSetterName True qName) (length args + 1)
        AS.DefSetter qName Nothing _ _ ->
           Just $ mkFunctionName (mkSetterName False qName) 1
        _ -> Nothing

      getSliceExpr slice = case slice of
        AS.SliceSingle e -> e
        _ -> error "Unexpected slice argument."

      --FIXME: There are a few cases of tuple assignments with setters that need to
      --be specially handled

      stmtOverrides stmt = case stmt of
        AS.StmtAssign (AS.LValArrayIndex (AS.LValVarRef qName) slices) rhs ->
          if Set.member (mkFunctionName (mkSetterName True qName) (length slices + 1)) setters then
            AS.StmtCall (mkSetterName True qName) (rhs : map getSliceExpr slices)
          else stmt
        AS.StmtAssign (AS.LValTuple (AS.LValArrayIndex (AS.LValVarRef qName) slices : rest)) rhs ->
          if Set.member (mkFunctionName (mkSetterName True qName) (length slices + 1)) setters then
            AS.StmtAssign (AS.LValTuple rest)
             (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "SETTERTUPLE")
              ([AS.ExprVarRef (mkSetterName True qName)]
               ++ (rhs : map getSliceExpr slices)))
          else stmt
        AS.StmtAssign (AS.LValSliceOf (AS.LValArrayIndex (AS.LValVarRef qName) slices) outerSlices) rhs ->
          if Set.member (mkFunctionName (mkSetterName True qName) (length slices + 1)) setters then
            AS.StmtCall (AS.QualifiedIdentifier AS.ArchQualAny "GETTERSETTER")
              ([AS.ExprSlice (AS.ExprVarRef (mkGetterName True qName)) outerSlices,
               AS.ExprVarRef (mkSetterName True qName)]
               ++ (rhs : map getSliceExpr slices))
          else stmt
        AS.StmtAssign (AS.LValVarRef qName) rhs ->
          if Set.member (mkFunctionName (mkSetterName False qName) 1) setters then
            AS.StmtCall (mkSetterName False qName) [rhs]
          else stmt
        _ -> stmt

      exprOverrides' expr = case expr of
        -- Limited support for alternate slice syntax
        AS.ExprIndex e slices@[AS.SliceOffset _ _] ->
          AS.ExprSlice e slices
        AS.ExprIndex e slices@[AS.SliceRange _ _] ->
          AS.ExprSlice e slices
        AS.ExprIndex (AS.ExprVarRef qName) slices ->
          if Set.member (mkFunctionName (mkGetterName True qName) (length slices)) getters then
            AS.ExprCall (mkGetterName True qName) (map getSliceExpr slices)
          else expr
        AS.ExprVarRef qName ->
          if Set.member (mkFunctionName (mkGetterName False qName) 0) getters then
            AS.ExprCall (mkGetterName False qName) []
          else expr
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

  in SyntaxOverrides stmtOverrides exprOverrides typeOverrides lvalOverrides

applyTypeSyntaxOverride :: SyntaxOverrides -> AS.Type -> AS.Type
applyTypeSyntaxOverride ovrs t =
  let
    f = applyExprSyntaxOverride ovrs
    h = applyTypeSyntaxOverride ovrs

    mapSlice slice = case slice of
      AS.SliceSingle e -> AS.SliceSingle (f e)
      AS.SliceOffset e e' -> AS.SliceOffset (f e) (f e')
      AS.SliceRange e e' -> AS.SliceRange (f e) (f e')

    mapField field = case field of
      AS.RegField i slices -> AS.RegField i (mapSlice <$> slices)

    mapIxType ix = case ix of
      AS.IxTypeRange e e' -> AS.IxTypeRange (f e) (f e')
      _ -> ix

    t' = typeOverrides ovrs t
  in case t' of
    AS.TypeFun i e -> AS.TypeFun i (f e)
    AS.TypeOf e -> AS.TypeOf (f e)
    AS.TypeReg i fs -> AS.TypeReg i (mapField <$> fs)
    AS.TypeArray t ixt -> AS.TypeArray (h t) (mapIxType ixt)
    _ -> t'

applyLValSyntaxOverride :: SyntaxOverrides -> AS.LValExpr -> AS.LValExpr
applyLValSyntaxOverride ovrs lval =
  let
    k = applyLValSyntaxOverride ovrs
    lval' = lvalOverrides ovrs lval
   in case lval' of
    AS.LValMember lv i -> AS.LValMember (k lv) i
    AS.LValMemberArray lv is -> AS.LValMemberArray (k lv) is
    AS.LValArrayIndex lv sl -> AS.LValArrayIndex (k lv) sl
    AS.LValSliceOf lv sl -> AS.LValSliceOf (k lv) sl
    AS.LValArray lvs -> AS.LValArray (k <$> lvs)
    AS.LValTuple lvs -> AS.LValTuple (k <$> lvs)
    AS.LValMemberBits lv is -> AS.LValMemberBits (k lv) is
    AS.LValSlice lvs -> AS.LValSlice (k <$> lvs)
    _ -> lval'


applyExprSyntaxOverride :: SyntaxOverrides -> AS.Expr -> AS.Expr
applyExprSyntaxOverride ovrs expr =
  let
    f = applyExprSyntaxOverride ovrs
    h = applyTypeSyntaxOverride ovrs
    mapSlice slice = case slice of
      AS.SliceSingle e -> AS.SliceSingle (f e)
      AS.SliceOffset e e' -> AS.SliceOffset (f e) (f e')
      AS.SliceRange e e' -> AS.SliceRange (f e) (f e')

    mapSetElement selem = case selem of
      AS.SetEltSingle e -> AS.SetEltSingle (f e)
      AS.SetEltRange e e' -> AS.SetEltRange (f e) (f e')

    expr' = exprOverrides ovrs expr
  in case expr' of
    AS.ExprSlice e slices -> AS.ExprSlice (f e) (mapSlice <$> slices)
    AS.ExprIndex e slices -> AS.ExprIndex (f e) (mapSlice <$> slices)
    AS.ExprUnOp o e -> AS.ExprUnOp o (f e)
    AS.ExprBinOp o e e' -> AS.ExprBinOp o (f e) (f e')
    AS.ExprMembers e is -> AS.ExprMembers (f e) is
    AS.ExprInMask e m -> AS.ExprInMask (f e) m
    AS.ExprMemberBits e is -> AS.ExprMemberBits (f e) is
    AS.ExprCall i es -> AS.ExprCall i (f <$> es)
    AS.ExprInSet e se -> AS.ExprInSet (f e) (mapSetElement <$> se)
    AS.ExprTuple es -> AS.ExprTuple (f <$> es)
    AS.ExprIf pes e -> AS.ExprIf ((\(x,y) -> (f x, f y)) <$> pes) (f e)
    AS.ExprMember e i -> AS.ExprMember (f e) i
    AS.ExprUnknown t -> AS.ExprUnknown (h t)
    _ -> expr'

applyStmtSyntaxOverride :: SyntaxOverrides -> AS.Stmt -> AS.Stmt
applyStmtSyntaxOverride ovrs stmt =
  let
    g = applyStmtSyntaxOverride ovrs
    f = applyExprSyntaxOverride ovrs
    h = applyTypeSyntaxOverride ovrs

    mapCases cases = case cases of
      AS.CaseWhen pat me stmts -> AS.CaseWhen pat (f <$> me) (g <$> stmts)
      AS.CaseOtherwise stmts -> AS.CaseOtherwise (g <$> stmts)
    mapCatches catches = case catches of
      AS.CatchWhen e stmts -> AS.CatchWhen (f e) (g <$> stmts)
      AS.CatchOtherwise stmts -> AS.CatchOtherwise (g <$> stmts)
    stmt' = stmtOverrides ovrs stmt
  in case stmt' of
    AS.StmtVarsDecl ty i -> AS.StmtVarsDecl (h ty) i
    AS.StmtVarDeclInit decl e -> AS.StmtVarDeclInit (h <$> decl) (f e)
    AS.StmtConstDecl decl e -> AS.StmtConstDecl decl (f e)
    AS.StmtAssign lv e -> AS.StmtAssign lv (f e)
    AS.StmtCall qi es -> AS.StmtCall qi (f <$> es)
    AS.StmtReturn me -> AS.StmtReturn (f <$> me)
    AS.StmtAssert e -> AS.StmtAssert (f e)
    AS.StmtIf tests body -> AS.StmtIf ((\(e, stmts) -> (f e, g <$> stmts)) <$> tests) ((fmap g) <$> body)
    AS.StmtCase e alts -> AS.StmtCase (f e) (mapCases <$> alts)
    AS.StmtFor ident (e,e') stmts -> AS.StmtFor ident (f e, f e') (g <$> stmts)
    AS.StmtWhile e stmts -> AS.StmtWhile (f e) (g <$> stmts)
    AS.StmtRepeat stmts e -> AS.StmtRepeat (g <$> stmts) (f e)
    AS.StmtSeeExpr e -> AS.StmtSeeExpr (f e)
    AS.StmtTry stmts ident alts -> AS.StmtTry (g <$> stmts) ident (mapCatches <$> alts)
    _ -> stmt'

-- | Fold over the nested expressions of a given expression
foldExpr :: (AS.Expr -> b -> b) -> AS.Expr -> b -> b
foldExpr f' expr b' =
  let
    b = f' expr b' -- resolve top expression first
    f = foldExpr f' -- inner expressions are recursively folded

    foldSlice slice = case slice of
      AS.SliceSingle e -> f e
      AS.SliceOffset e e' -> f e' . f e
      AS.SliceRange e e' -> f e' . f e

    foldSetElems slice = case slice of
      AS.SetEltSingle e -> f e
      AS.SetEltRange e e' -> f e' . f e

  in case expr of
    AS.ExprSlice e slices -> f e $ foldr foldSlice b slices
    AS.ExprIndex e slices -> f e $ foldr foldSlice b slices
    AS.ExprUnOp _ e -> f e b
    AS.ExprBinOp _ e e' -> f e' $ f e b
    AS.ExprMembers e _ -> f e b
    AS.ExprInMask e _ -> f e b
    AS.ExprMemberBits e _ -> f e b
    AS.ExprCall _ es -> foldr f b es
    AS.ExprInSet e se -> foldr foldSetElems (f e b) se
    AS.ExprTuple es -> foldr f b es
    AS.ExprIf pes e -> f e $ foldr (\(x,y) -> f y . f x) b pes
    AS.ExprMember e _ -> f e b
    _ -> b

foldLVal :: (AS.LValExpr -> b -> b) -> AS.LValExpr -> b -> b
foldLVal h' lval b' =
  let
    b = h' lval b'
    h = foldLVal h'
  in case lval of
    AS.LValMember lv _ -> h lv b
    AS.LValMemberArray lv _ -> h lv b
    AS.LValArrayIndex lv _ -> h lv b
    AS.LValSliceOf lv _ -> h lv b
    AS.LValArray lvs -> foldr h b lvs
    AS.LValTuple lvs -> foldr h b lvs
    AS.LValMemberBits lv _ -> h lv b
    AS.LValSlice lvs -> foldr h b lvs
    _ -> b

-- | Fold over nested statements and their *top-level* expressions
foldStmt' :: (AS.Expr -> b -> b) ->
             (AS.LValExpr -> b -> b) ->
             (AS.Stmt -> b -> b) ->
             AS.Stmt -> b -> b
foldStmt' f h g' stmt b' =
  let
    b = g' stmt b' -- resolve top statement first
    g = foldStmt' f h g' -- inner statments are recursively folded

    foldCases cases b'' = case cases of
      AS.CaseWhen _ me stmts -> foldr g (foldr f b'' me) stmts
      AS.CaseOtherwise stmts -> foldr g b'' stmts
    foldCatches catches b'' = case catches of
      AS.CatchWhen e stmts -> foldr g (f e b'') stmts
      AS.CatchOtherwise stmts -> foldr g b'' stmts
  in case stmt of
    AS.StmtVarDeclInit _ e -> f e b
    AS.StmtConstDecl _ e -> f e b
    AS.StmtAssign lv e -> f e (h lv b)
    AS.StmtCall _ es -> foldr f b es
    AS.StmtReturn me -> foldr f b me
    AS.StmtAssert e -> f e b
    AS.StmtIf tests body ->
      let testsb = foldr (\(e, stmts) -> \b'' -> foldr g (f e b'') stmts) b tests in
        foldr (\stmts -> \b'' -> foldr g b'' stmts) testsb body
    AS.StmtCase e alts -> foldr foldCases (f e b) alts
    AS.StmtFor _ (e, e') stmts -> foldr g (f e' $ f e b) stmts
    AS.StmtWhile e stmts -> foldr g (f e b) stmts
    AS.StmtRepeat stmts e -> f e $ foldr g b stmts
    AS.StmtSeeExpr e -> f e b
    AS.StmtTry stmts _ alts -> foldr foldCatches (foldr g b stmts) alts
    _ -> b

-- | Fold over nested statements and nested expressions
foldStmt :: (AS.Expr -> b -> b) ->
            (AS.LValExpr -> b -> b) ->
            (AS.Stmt -> b -> b) ->
            AS.Stmt -> b -> b
foldStmt f h = foldStmt' (foldExpr f) (foldLVal h)


foldDef :: (T.Text -> AS.Expr -> b -> b) ->
           (T.Text -> AS.LValExpr -> b -> b) ->
           (T.Text -> AS.Stmt -> b -> b) ->
           AS.Definition -> b -> b
foldDef f' h' g' d b = case d of
  AS.DefCallable (AS.QualifiedIdentifier _ ident) _ _ stmts ->
    let
      f = f' ident
      h = h' ident
      g = g' ident
    in foldr (foldStmt f h g) b stmts
  _ -> b

foldInstruction :: (T.Text -> AS.Expr -> b -> b) ->
                   (T.Text -> AS.LValExpr -> b -> b) ->
                   (T.Text -> AS.Stmt -> b -> b) ->
                   AS.Instruction -> b -> b
foldInstruction f' h' g' (AS.Instruction ident instEncodings instPostDecode instExecute _) b =
  let
    f = f' ident
    h = h' ident
    g = g' ident

    foldEncoding (AS.InstructionEncoding {encDecode=stmts}) b' =
      foldr (foldStmt f h g) b' stmts
  in foldr (foldStmt f h g) (foldr foldEncoding b instEncodings) (instPostDecode ++ instExecute)

foldASL :: (T.Text -> AS.Expr -> b -> b) ->
           (T.Text -> AS.LValExpr -> b -> b) ->
           (T.Text -> AS.Stmt -> b -> b) ->
  [AS.Definition] -> [AS.Instruction] -> b -> b
foldASL f h g defs instrs b = foldr (foldInstruction f h g) (foldr (foldDef f h g) b defs) instrs

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
