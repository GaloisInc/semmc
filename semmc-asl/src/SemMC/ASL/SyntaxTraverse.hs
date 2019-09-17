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
  ( prepASL
  , foldASL
  , foldExpr
  , mkFunctionName
  )
where

import qualified Language.ASL.Syntax as AS
import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Maybe (maybeToList, catMaybes, fromMaybe)

-- | Syntactic-level expansions that should happen aggressively before
-- any interpretation.

-- FIXME: Lots of rewriting to handle globals which should go away when
-- we are properly handling the register definitions file
data SyntaxOverrides = SyntaxOverrides { stmtOverrides :: AS.Stmt -> AS.Stmt
                                       , exprOverrides :: AS.Expr -> AS.Expr
                                       , globalStructTypes :: [(T.Text, [(T.Text, AS.Type)])]
                                       , globalTypeSynonyms :: [(T.Text, AS.Type)]
                                       , typedGlobalStructs :: [(T.Text, T.Text)]}

type GlobalInfo = ([(T.Text, [(T.Text, AS.Type)])], [(T.Text, AS.Type)], [(T.Text, T.Text)])

applySyntaxOverridesDefs :: SyntaxOverrides -> [AS.Definition] -> [AS.Definition]
applySyntaxOverridesDefs ovrs defs =
  let
    g = applyStmtSyntaxOverride ovrs
    f = applyExprSyntaxOverride ovrs


    -- TODO: For sanity we delete setter definitions which require
    -- pass-by-reference since we don't have a sane semantics for this

    argName (AS.SetterArg name False) = Just name
    argName _ = Nothing

    updDecls d = if | (name, AS.TypeRef (AS.QualifiedIdentifier _ tname)) <- d
                    , Just syn <- lookup tname (globalTypeSynonyms ovrs)
                      -> (name, syn)
                    | otherwise -> d

    mapDefs d = case d of
      AS.DefCallable qName args rets stmts ->
        [AS.DefCallable qName args rets (g <$> stmts)]
      c@AS.DefGetter {} -> callablesFromGetter ovrs g c
      AS.DefSetter qName args rhs stmts -> maybeToList $ do
        argNames <- sequence (argName <$> args)
        Just $ AS.DefCallable { callableName = mkSetterName qName
                       , callableArgs = updDecls <$> (rhs : argNames)
                       , callableRets = []
                       , callableStmts = g <$> stmts
                       }
      AS.DefConst i t e -> [AS.DefConst i t (f e)]
      _ -> [d]

  in concat $ mapDefs <$> defs

applySyntaxOverridesInstrs :: SyntaxOverrides -> [AS.Instruction] -> [AS.Instruction]
applySyntaxOverridesInstrs ovrs instrs =
  let
    g = applyStmtSyntaxOverride ovrs

    mapInstr (AS.Instruction instName instEncodings instPostDecode instExecute conditional) =
      AS.Instruction instName (mapEnc <$> instEncodings) (g <$> instPostDecode) (g <$> instExecute) conditional

    mapEnc (AS.InstructionEncoding a b c d e f encDecode) =
      AS.InstructionEncoding a b c d e f (g <$> encDecode)

  in mapInstr <$> instrs


prepASL :: ([AS.Instruction], [AS.Definition])
        -> GlobalInfo
        -> ([AS.Instruction], [AS.Definition])
prepASL (instrs, defs) gInfo =
  let ovrs = mkSyntaxOverrides defs gInfo
  in (applySyntaxOverridesInstrs ovrs instrs, applySyntaxOverridesDefs ovrs defs)



mkSyntaxOverrides :: [AS.Definition] -> GlobalInfo -> SyntaxOverrides
mkSyntaxOverrides defs (globalStructTypes, globalTypeSynonyms, typedGlobalStructs) =
  let getters = Set.fromList $ catMaybes $ getterName <$> defs
      setters = Set.fromList $ catMaybes $ setterName <$> defs

      getterName d = case d of
        AS.DefGetter qName args [_] _ ->
          Just $ mkFunctionName (mkGetterName qName) (length args)
        AS.DefGetter (AS.QualifiedIdentifier _ name) _ _ _ ->
          error $ "Unexpected getter for: " <> T.unpack name
        _ -> Nothing

      setterName d = case d of
        AS.DefSetter qName args _ _ ->
           Just $ mkFunctionName (mkSetterName qName) (length args + 1)
        _ -> Nothing

      getSliceExpr slice = case slice of
        AS.SliceSingle e -> e
        _ -> error "Unexpected slice argument."

      --FIXME: There are a few cases of tuple assignments with setters that need to
      --be specially handled

      stmtOverrides stmt = case stmt of
        AS.StmtAssign (AS.LValArrayIndex (AS.LValVarRef qName) slices) rhs ->
          if Set.member (mkFunctionName (mkSetterName qName) (length slices + 1)) setters then
            AS.StmtCall (mkSetterName qName) (rhs : map getSliceExpr slices)
          else stmt
        AS.StmtAssign (AS.LValVarRef qName) rhs ->
          if Set.member (mkFunctionName (mkSetterName qName) 1) setters then
            AS.StmtCall (mkSetterName qName) [rhs]
          else stmt
        _ -> stmt

      exprOverrides' expr mmem = case expr of
        AS.ExprIndex (AS.ExprVarRef qName) slices ->
          if Set.member (mkFunctionName (mkGetterName qName) (length slices)) getters then
            Just $ AS.ExprCall (mkGetterNameField qName mmem) (map getSliceExpr slices)
          else Nothing
        AS.ExprVarRef qName ->
          if Set.member (mkFunctionName (mkGetterName qName) 0) getters then
            Just $ AS.ExprCall (mkGetterNameField qName mmem) []
          else Nothing
        _ -> Nothing

      exprOverrides expr = case expr of
        AS.ExprMember e mem | Just e' <- exprOverrides' e (Just mem) -> e'
        _ | Just e' <- exprOverrides' expr Nothing -> e'
        _ -> expr
  in SyntaxOverrides stmtOverrides exprOverrides globalStructTypes globalTypeSynonyms typedGlobalStructs


-- | Transform a function that returns a struct type
-- into one that instead returns a specific member @mem@ of the resulting struct
collapseReturn :: SyntaxOverrides -> T.Text -> [AS.Stmt] -> [AS.Stmt]
collapseReturn ovrs mem stmts =
  let assignLast finident stmts' =
        reverse $ case reverse stmts' of
          AS.StmtAssign (AS.LValVarRef lvident) eident
            : rest | lvident == finident
                   , checkValidField ovrs eident mem ->
                AS.StmtReturn (Just $ AS.ExprMember eident mem) : rest
          end@(AS.StmtCall (AS.QualifiedIdentifier q "Unreachable") [] : rest) ->
            end
          AS.StmtReturn (Just eident) : rest
            | checkValidField ovrs eident mem ->
              AS.StmtReturn (Just $ AS.ExprMember eident mem) : rest
          _ -> error $ "Failed to collapse getter structure: " <> show stmts <> " " <> show mem
      mapCases finident cases = case cases of
        AS.CaseWhen pat me stmts' -> AS.CaseWhen pat me (assignLast finident stmts')
        AS.CaseOtherwise stmts' -> AS.CaseOtherwise (assignLast finident stmts')

  in reverse $ case reverse stmts of
    AS.StmtReturn (Just (AS.ExprVarRef finident)) : cond : rest ->
      case cond of
        AS.StmtIf tests body ->
          AS.StmtIf ((\(e, stmts') -> (e, assignLast finident stmts')) <$> tests)
                    (assignLast finident <$> body) : rest
        AS.StmtCase e alts ->
          AS.StmtCase e (mapCases finident <$> alts) : rest
        _ -> error $ "Unexpected statement structure: " <> show stmts
    AS.StmtReturn (Just (AS.ExprCall qName args)) : rest ->
      case getterSuffixOf qName of
        Just qName' ->
          AS.StmtReturn (Just (AS.ExprCall (mkGetterNameField qName' (Just mem)) args)) : rest
        Nothing -> error $ "Unexpected statement structure: " <> show stmts
    _ -> error $ "Unexpected statement structure: " <> show stmts


callablesFromGetter :: SyntaxOverrides -> (AS.Stmt -> AS.Stmt) -> AS.Definition -> [AS.Definition]
callablesFromGetter ovrs g
  (AS.DefGetter qName args [AS.TypeRef (AS.QualifiedIdentifier _ tname)] stmts) =
  case lookup tname (globalStructTypes ovrs) of
    Just fields -> (\(fnm, ftype) -> mkCallable (Just fnm) ftype) <$> fields
    _ -> case lookup tname (globalTypeSynonyms ovrs) of
      Just syn -> [mkCallable Nothing syn]
      _ -> error $
             "Missing struct definition for getter:" <> show qName <>
             " and type " <> T.unpack tname
  where
    getStatements (Just fnm) = collapseReturn ovrs fnm (g <$> stmts)
    getStatements Nothing = g <$> stmts
    mkCallable mfnm ftype =
      AS.DefCallable { callableName = mkGetterNameField qName mfnm
                     , callableArgs = args
                     , callableRets = [ftype]
                     , callableStmts = getStatements mfnm
                     }
callablesFromGetter _ g (AS.DefGetter qName args rets stmts) =
  [AS.DefCallable (mkGetterName qName) args rets (g <$> stmts)]
callablesFromGetter _ _ _ = error "Unexpected Definition."

checkValidField :: SyntaxOverrides -> AS.Expr -> T.Text -> Bool
checkValidField ovrs (AS.ExprVarRef (AS.QualifiedIdentifier _ struct)) mem =
  fromMaybe (False) $ do
    tp <- lookup struct (typedGlobalStructs ovrs)
    mems <- lookup tp (globalStructTypes ovrs)
    _ <- lookup mem mems
    return True
checkValidField _ _ _ = False

applyExprSyntaxOverride :: SyntaxOverrides -> AS.Expr -> AS.Expr
applyExprSyntaxOverride ovrs expr =
  let
    f = applyExprSyntaxOverride ovrs
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
    _ -> expr'

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



applyStmtSyntaxOverride :: SyntaxOverrides -> AS.Stmt -> AS.Stmt
applyStmtSyntaxOverride ovrs stmt =
  let
    g = applyStmtSyntaxOverride ovrs
    f = applyExprSyntaxOverride ovrs
    mapCases cases = case cases of
      AS.CaseWhen pat me stmts -> AS.CaseWhen pat (f <$> me) (g <$> stmts)
      AS.CaseOtherwise stmts -> AS.CaseOtherwise (g <$> stmts)
    mapCatches catches = case catches of
      AS.CatchWhen e stmts -> AS.CatchWhen (f e) (g <$> stmts)
      AS.CatchOtherwise stmts -> AS.CatchOtherwise (g <$> stmts)
    stmt' = stmtOverrides ovrs stmt
  in case stmt' of
    AS.StmtVarDeclInit decl e -> AS.StmtVarDeclInit decl (f e)
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

getterText :: T.Text
getterText = "GETTER_"

mkGetterName :: AS.QualifiedIdentifier -> AS.QualifiedIdentifier
mkGetterName = do
  mapInnerName (\s -> getterText <> s)

mkGetterNameField :: AS.QualifiedIdentifier -> Maybe T.Text -> AS.QualifiedIdentifier
mkGetterNameField name (Just field) =
  mapInnerName (\s -> s <> "_" <> field) $ mkGetterName name
mkGetterNameField name Nothing = mkGetterName name

getterSuffixOf :: AS.QualifiedIdentifier -> Maybe (AS.QualifiedIdentifier)
getterSuffixOf (AS.QualifiedIdentifier q nm) = case T.stripPrefix getterText nm of
  Just nm' -> Just (AS.QualifiedIdentifier q nm')
  Nothing -> Nothing

mkSetterName :: AS.QualifiedIdentifier -> AS.QualifiedIdentifier
mkSetterName = mapInnerName (\s -> "SETTER_" <> s)

-- | Make a function name given its ASL name and arity.
mkFunctionName :: AS.QualifiedIdentifier -> Int -> T.Text
mkFunctionName name numArgs = collapseQualID name <> T.pack "_" <> T.pack (show numArgs)


collapseQualID :: AS.QualifiedIdentifier -> T.Text
collapseQualID (AS.QualifiedIdentifier AS.ArchQualAArch64 name) = "AArch64_" <> name
collapseQualID (AS.QualifiedIdentifier AS.ArchQualAArch32 name) = "AArch32_" <> name
collapseQualID (AS.QualifiedIdentifier _ name) = name

mapInnerName :: (T.Text -> T.Text) -> AS.QualifiedIdentifier -> AS.QualifiedIdentifier
mapInnerName f (AS.QualifiedIdentifier q name) = AS.QualifiedIdentifier q (f name)