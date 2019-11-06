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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module SemMC.ASL.SyntaxTraverse
  ( mkSyntaxOverrides
  , applySyntaxOverridesInstrs
  , applySyntaxOverridesDefs
  , varsOfExpr
  , SyntaxRepr(..)
  , SyntaxTrace(..)
  , SyntaxTraceE(..)
  , SyntaxTraceT
  , SyntaxCollector(..)
  , SyntaxTraceError(..)
  , SyntaxTraceStack(..)
  , KnownSyntaxRepr(..)
  , withKnownSyntaxRepr
  , syntaxTraceUpdate
  , SyntaxExt(..)
  , runSyntaxTraceT
  , noCollectors
  , SyntaxMap(..)
  , noMaps
  , mapsToCollector
  , traverseExpr
  , traverseStmt
  , SyntaxWriter(..)
  , noWrites
  , writeSyntax
  , mapSyntax
  , mkFunctionName
  , mapInnerName
  , pattern VarName
  )
where

import           Data.Typeable
import qualified Control.Exception as X
import           Control.Applicative
import qualified Control.Monad.Writer.Lazy as W
import           Control.Monad.Identity
import qualified Control.Monad.Except as E
import qualified Control.Monad.Trans as MT
import qualified Control.Monad.Reader as R
import qualified Control.Monad.RWS as RWS
import qualified Language.ASL.Syntax as AS
import qualified Data.Text as T
import qualified Data.List as List
import           Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.State as MSS
import           Data.Maybe (maybeToList, catMaybes, fromMaybe, listToMaybe, isJust, mapMaybe)
import           SemMC.ASL.Types
import           Data.Parameterized.Classes

pattern VarName :: T.Text -> AS.QualifiedIdentifier
pattern VarName nm <- AS.QualifiedIdentifier _ nm

varsOfExpr :: AS.Expr -> [T.Text]
varsOfExpr e = runIdentity $ writeSyntax (SyntaxWriter getVar) e
  where
    getVar :: forall t. KnownSyntaxRepr t => t -> Identity [T.Text]
    getVar = withKnownSyntaxRepr $ \case
      SyntaxExprRepr -> \case
        (AS.ExprVarRef (VarName ident)) -> return $ [ident]
        _ -> return $ []
      _ -> \_ -> return $ []

-- | Syntactic-level expansions that should happen aggressively before
-- any interpretation.

applySyntaxOverridesDefs :: [SyntaxMap] -> [AS.Definition] -> [AS.Definition]
applySyntaxOverridesDefs maps defs =
  let
    f :: forall e. KnownSyntaxRepr e => e -> e
    f e = foldr mapSyntax e maps


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

applySyntaxOverridesInstrs :: [SyntaxMap] -> [AS.Instruction] -> [AS.Instruction]
applySyntaxOverridesInstrs maps instrs =
  let
    f :: forall e. KnownSyntaxRepr e => e -> e
    f e = foldr mapSyntax e maps

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
  }

emptyInternalOverride :: InternalOverride
emptyInternalOverride = InternalOverride Set.empty Set.empty

exprToLVal :: AS.Expr -> AS.LValExpr
exprToLVal e = case e of
  AS.ExprVarRef qident -> AS.LValVarRef qident
  AS.ExprIndex e slices -> AS.LValArrayIndex (exprToLVal e) slices
  AS.ExprSlice e slices -> AS.LValSliceOf (exprToLVal e) slices
  AS.ExprMembers e [mem] -> AS.LValMember (exprToLVal e) mem
  AS.ExprTuple es -> AS.LValTuple (map exprToLVal es)
  _ -> error $ "Invalid inline for expr:" <> show e

mkSyntaxOverrides :: [AS.Definition] -> [SyntaxMap]
mkSyntaxOverrides defs =
  let
      addInternalOverride d iovrs = case d of
        AS.DefGetter qName (Just args) _ _ ->
          iovrs { iovGetters = Set.insert (mkFunctionName (mkGetterName True qName) (length args)) (iovGetters iovrs) }
        AS.DefGetter qName Nothing _ _ ->
          iovrs { iovGetters = Set.insert (mkFunctionName (mkGetterName False qName) 0) (iovGetters iovrs) }
        AS.DefSetter qName (Just args) _ _ ->
           iovrs { iovSetters = Set.insert (mkFunctionName (mkSetterName True qName) (length args + 1)) (iovSetters iovrs) }
        AS.DefSetter qName Nothing _ _ ->
           iovrs { iovSetters = Set.insert (mkFunctionName (mkSetterName False qName) 1) (iovSetters iovrs) }
        _ -> iovrs

      InternalOverride getters setters =
        foldr addInternalOverride emptyInternalOverride defs

      getSliceExpr slice = case slice of
        AS.SliceSingle e -> e
        _ -> error "Unexpected slice argument."

      assignOverrides lv = case lv of
        AS.LValArrayIndex (AS.LValVarRef (AS.QualifiedIdentifier _ "Elem"))
          [AS.SliceSingle vector, AS.SliceSingle e, AS.SliceSingle size] -> Just $ \rhs -> stmtOverrides $
          AS.StmtAssign
          (AS.LValSliceOf (exprToLVal vector)
           [(AS.SliceRange
            (AS.ExprBinOp AS.BinOpSub
             (AS.ExprBinOp AS.BinOpMul
              (AS.ExprBinOp AS.BinOpAdd e (AS.ExprLitInt 1))
              size)
             (AS.ExprLitInt 1))
            (AS.ExprBinOp AS.BinOpMul e size))]) rhs
        AS.LValArrayIndex (AS.LValVarRef (AS.QualifiedIdentifier q "Elem")) [vector, e] -> Just $ \rhs ->
          case assignOverrides (AS.LValArrayIndex (AS.LValVarRef (AS.QualifiedIdentifier q "Elem"))
                                [vector, e, AS.SliceSingle $ AS.ExprCall (AS.QualifiedIdentifier q "sizeOf") [rhs]]) of
            Just f -> f rhs
            Nothing -> error "Bad overrides"
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
        AS.ExprIndex e slices@[AS.SliceOffset _ _] ->
          AS.ExprSlice e slices
        AS.ExprIndex e slices@[AS.SliceRange _ _] ->
          AS.ExprSlice e slices
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

      -- Given a bottom-up traversal, getter expansion needs to happen
      -- after all other rewrites, since apparent getters can actually
      -- be syntactic sugar for a getter/setter
      expandGetters expr = case expr of
        AS.ExprIndex (AS.ExprVarRef qName) slices
          | Set.member (mkFunctionName (mkGetterName True qName) (length slices)) getters ->
            AS.ExprCall (mkGetterName True qName) (map getSliceExpr slices)
        AS.ExprVarRef qName
          | Set.member (mkFunctionName (mkGetterName False qName) 0) getters ->
            AS.ExprCall (mkGetterName False qName) []
        _ -> expr

      firstMap :: forall t. KnownSyntaxRepr t => t -> t
      firstMap = withKnownSyntaxRepr $ \case
        SyntaxExprRepr -> exprOverrides
        SyntaxLValRepr -> lvalOverrides
        SyntaxStmtRepr -> stmtOverrides
        SyntaxTypeRepr -> typeOverrides
        _ -> id

      secondMap :: forall t. KnownSyntaxRepr t => t -> t
      secondMap = withKnownSyntaxRepr $ \case
        SyntaxExprRepr -> expandGetters
        _ -> id

  in [SyntaxMap secondMap, SyntaxMap firstMap]

data SyntaxRepr t where
  SyntaxStmtRepr :: SyntaxRepr AS.Stmt
  SyntaxExprRepr :: SyntaxRepr AS.Expr
  SyntaxTypeRepr :: SyntaxRepr AS.Type
  SyntaxLValRepr :: SyntaxRepr AS.LValExpr
  SyntaxCallRepr :: SyntaxRepr (AS.QualifiedIdentifier, [AS.Expr])

instance TestEquality SyntaxRepr where
  testEquality repr repr' = case (repr, repr') of
    (SyntaxStmtRepr, SyntaxStmtRepr) -> Just Refl
    (SyntaxExprRepr, SyntaxExprRepr) -> Just Refl
    (SyntaxTypeRepr, SyntaxTypeRepr) -> Just Refl
    (SyntaxLValRepr, SyntaxLValRepr) -> Just Refl
    (SyntaxCallRepr, SyntaxCallRepr) -> Just Refl
    _ -> Nothing

deriving instance Eq (SyntaxRepr t)
deriving instance Ord (SyntaxRepr t)

class KnownSyntaxRepr t where
  knownSyntaxRepr :: SyntaxRepr t

withKnownSyntaxRepr :: KnownSyntaxRepr t => (SyntaxRepr t -> t -> a) -> t -> a
withKnownSyntaxRepr f = f knownSyntaxRepr

instance KnownSyntaxRepr AS.Stmt where
  knownSyntaxRepr = SyntaxStmtRepr

instance KnownSyntaxRepr AS.Expr where
  knownSyntaxRepr = SyntaxExprRepr

instance KnownSyntaxRepr AS.LValExpr where
  knownSyntaxRepr = SyntaxLValRepr

instance KnownSyntaxRepr AS.Type where
  knownSyntaxRepr = SyntaxTypeRepr

instance KnownSyntaxRepr (AS.QualifiedIdentifier, [AS.Expr]) where
  knownSyntaxRepr = SyntaxCallRepr


class Monad m => SyntaxTrace m where
  traceSyntax :: forall t a. KnownSyntaxRepr t => t -> m a -> m a

class Monad m => SyntaxExt (ext :: * -> *) m where
  syntaxExt :: forall t. KnownSyntaxRepr t => t -> m (ext t)

instance Monad m => SyntaxExt (Const ()) m where
  syntaxExt _ = return (Const ())

data SyntaxTraceStack (ext :: * -> *) where
  SyntaxTraceStack :: (forall t. SyntaxRepr t -> [(t, ext t)]) -> SyntaxTraceStack ext

syntaxTraceUpdate' :: forall ext t
                   . SyntaxRepr t
                  -> ([(t, ext t)] -> [(t, ext t)])
                  -> SyntaxTraceStack ext
                  -> SyntaxTraceStack ext
syntaxTraceUpdate' repr f sts@(SyntaxTraceStack stacks) = SyntaxTraceStack $ \repr' ->
  case testEquality repr repr' of
    Just Refl -> f (stacks repr')
    _ -> stacks repr'

syntaxTraceUpdate :: forall ext t
                   . KnownSyntaxRepr t
                  => ([(t, ext t)] -> [(t, ext t)])
                  -> SyntaxTraceStack ext
                  -> SyntaxTraceStack ext
syntaxTraceUpdate f sts = syntaxTraceUpdate' knownSyntaxRepr f sts

data SyntaxTraceError e ext = SyntaxTraceError e (SyntaxTraceStack ext)

type ShowableExt ext = (Show (ext AS.Stmt), Show (ext AS.Expr), Show (ext (AS.QualifiedIdentifier, [AS.Expr])))

instance (ShowableExt ext, Show e) => Show (SyntaxTraceError e ext) where
  show (SyntaxTraceError e stack) =
    "Traced error stacks:\n" ++ show stack ++ "\nError: " ++ show e

instance (ShowableExt ext, X.Exception e, Typeable ext) => X.Exception (SyntaxTraceError e ext)

newtype SyntaxTraceT e ext (m :: * -> *) a =
  SyntaxTraceT { unSyntaxTraceT :: E.ExceptT (SyntaxTraceError e ext) (R.ReaderT (SyntaxTraceStack ext) m) a }
  deriving (
    Functor,
    Applicative,
    Monad,
    E.MonadError (SyntaxTraceError e ext))

instance SyntaxExt ext m => SyntaxTrace (SyntaxTraceT e ext m) where
  traceSyntax syn (SyntaxTraceT f) = SyntaxTraceT $ do
    ext <- MT.lift $ MT.lift $ syntaxExt syn
    R.local (syntaxTraceUpdate $ \syns -> ((syn, ext) : syns)) f

class Monad m => SyntaxTraceE e m where
  throwTrace :: forall a. e -> m a

instance Monad m => SyntaxTraceE e (SyntaxTraceT e ext m) where
  throwTrace e = do
    tr <- SyntaxTraceT $ R.ask
    E.throwError $ SyntaxTraceError e tr

runSyntaxTraceT :: Monad m => SyntaxTraceT e ext m a -> m (Either (SyntaxTraceError e ext) a)
runSyntaxTraceT (SyntaxTraceT f) = R.runReaderT (E.runExceptT f) (SyntaxTraceStack (\_ -> []))

instance MT.MonadTrans (SyntaxTraceT e ext) where
  lift f = SyntaxTraceT $ (MT.lift $ MT.lift $ f)

instance SyntaxTrace Identity where
  traceSyntax _ f = f

instance (Monoid w, Monad m, SyntaxTrace m) => SyntaxTrace (W.WriterT w m) where
  traceSyntax syn (W.WriterT f) = W.WriterT $ traceSyntax syn f

instance R.MonadReader env m => R.MonadReader env (SyntaxTraceT e ext m) where
  ask = MT.lift $ R.ask
  local f (SyntaxTraceT m) = SyntaxTraceT $ E.mapExceptT (R.mapReaderT (R.local f)) m

instance RWS.MonadState s m => RWS.MonadState s (SyntaxTraceT e ext m) where
  state f = MT.lift $ RWS.state f


data SyntaxCollector m where
  SyntaxCollector :: (forall t. KnownSyntaxRepr t => t -> m t) -> SyntaxCollector m

data SyntaxWriter w m where
  SyntaxWriter :: (forall t. KnownSyntaxRepr t => t -> m w) -> SyntaxWriter w m

data SyntaxMap where
  SyntaxMap :: (forall t. KnownSyntaxRepr t => t -> t) -> SyntaxMap



noCollectors :: SyntaxTrace m => SyntaxCollector m
noCollectors = SyntaxCollector return

noWrites :: Monoid w => SyntaxTrace m => SyntaxWriter w m
noWrites = SyntaxWriter
  (\_ -> return mempty)

noMaps :: SyntaxMap
noMaps = SyntaxMap id

writeToCollector :: forall w m.
  Monoid w => SyntaxTrace m => SyntaxWriter w m -> SyntaxCollector (W.WriterT w m)
writeToCollector (SyntaxWriter syntaxWrite) =
  SyntaxCollector (liftWrite syntaxWrite)
  where
    liftWrite :: forall t. KnownSyntaxRepr t => (t -> m w) -> t -> W.WriterT w m t
    liftWrite f e = MT.lift (f e) >>= (\w -> W.tell w >> return e)


writeSyntax :: KnownSyntaxRepr t => Monoid w => SyntaxTrace m => SyntaxWriter w m -> t -> m w
writeSyntax writes e = W.execWriterT (traverseSyntax (writeToCollector writes) e)

mapSyntax :: KnownSyntaxRepr t => SyntaxMap -> t -> t
mapSyntax smap t = runIdentity $ traverseSyntax (mapsToCollector smap) t

mapsToCollector :: SyntaxMap -> SyntaxCollector Identity
mapsToCollector (SyntaxMap syntaxMap) =
  SyntaxCollector (\t -> return $ syntaxMap t)

-- | Recursive traversal
traverseSyntax :: SyntaxTrace m => KnownSyntaxRepr t => SyntaxCollector m -> t -> m t
traverseSyntax col = withKnownSyntaxRepr $ \case
  SyntaxStmtRepr -> traverseStmt col
  SyntaxTypeRepr -> traverseType col
  SyntaxLValRepr -> traverseLVal col
  SyntaxExprRepr -> traverseExpr col
  SyntaxCallRepr -> traverseCall col


-- | Shallow traversal
collectSyntax :: KnownSyntaxRepr t => SyntaxTrace m => SyntaxCollector m -> t -> m t
collectSyntax (SyntaxCollector col) r = col r

traverseCall :: SyntaxTrace m
             => SyntaxCollector m
             -> (AS.QualifiedIdentifier, [AS.Expr])
             -> m (AS.QualifiedIdentifier, [AS.Expr])
traverseCall cols call = traceSyntax call (collectSyntax cols call)

traverseSlice :: SyntaxTrace m => SyntaxCollector m -> AS.Slice -> m AS.Slice
traverseSlice cols slice =
  let
    f = traverseExpr cols

  in case slice of
     AS.SliceSingle e -> AS.SliceSingle <$> (f e)
     AS.SliceOffset e e' -> liftA2 AS.SliceOffset (f e) (f e')
     AS.SliceRange e e' -> liftA2 AS.SliceRange (f e) (f e')

-- | Fold over the nested expressions of a given expression
traverseExpr :: forall m. SyntaxTrace m => SyntaxCollector m -> AS.Expr -> m AS.Expr
traverseExpr cols expr =
  let
    f = traverseExpr cols -- inner expressions are recursively folded
    k = traverseType cols

    collectExpr = case expr of
      AS.ExprCall qIdent argEs -> do
        (qIdent', argEs') <- traceSyntax (qIdent, argEs) (collectSyntax cols (qIdent, argEs))
        let expr' = AS.ExprCall qIdent' argEs'
        traceSyntax expr' (collectSyntax cols expr')
      _ -> traceSyntax expr (collectSyntax cols expr)

    foldSetElems slice = case slice of
      AS.SetEltSingle e -> AS.SetEltSingle <$> f e
      AS.SetEltRange e e' -> liftA2 AS.SetEltRange (f e) (f e')

  in collectExpr >>= \expr' -> case expr' of
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

traverseLVal :: SyntaxTrace m => SyntaxCollector m -> AS.LValExpr -> m AS.LValExpr
traverseLVal cols lval =
  let
    h = traverseLVal cols
    collectLVal = traceSyntax lval (collectSyntax cols lval)

  in collectLVal >>= \lval' -> case lval' of
    AS.LValMember lv mem -> (\lv' -> AS.LValMember lv' mem) <$> h lv
    AS.LValMemberArray lv idx -> (\lv' -> AS.LValMemberArray lv' idx) <$> h lv
    AS.LValArrayIndex lv slices -> liftA2 AS.LValArrayIndex (h lv) $ traverse (traverseSlice cols) slices
    AS.LValSliceOf lv slices -> liftA2 AS.LValSliceOf (h lv) $ traverse (traverseSlice cols) slices
    AS.LValArray lvs -> AS.LValArray <$> traverse h lvs
    AS.LValTuple lvs -> AS.LValTuple <$> traverse h lvs
    AS.LValMemberBits lv bits -> (\lv' -> AS.LValMemberBits lv' bits) <$> h lv
    AS.LValSlice lvs -> AS.LValSlice <$> traverse h lvs
    _ -> return lval'

traverseType :: forall m. SyntaxTrace m => SyntaxCollector m -> AS.Type -> m AS.Type
traverseType cols t =
  let
    f = traverseExpr cols
    k = traverseType cols
    collectType ty = traceSyntax ty (collectSyntax cols ty)

    foldField field = case field of
      AS.RegField i slices -> (\slices' -> AS.RegField i slices') <$>
        traverse (traverseSlice cols) slices

    foldIxType ix = case ix of
      AS.IxTypeRange e e' -> liftA2 AS.IxTypeRange (f e) (f e')
      _ -> return ix

  in collectType t >>= \t' -> case t' of
    AS.TypeFun i e -> (\e' -> AS.TypeFun i e') <$> f e
    AS.TypeOf e -> AS.TypeOf <$> f e
    AS.TypeReg i fs -> (\fs' -> AS.TypeReg i fs') <$> traverse foldField fs
    AS.TypeArray t ixt -> liftA2 AS.TypeArray (k t) (foldIxType ixt)
    _ -> return t'

traverseStmt :: forall m. SyntaxTrace m => SyntaxCollector m -> AS.Stmt -> m AS.Stmt
traverseStmt cols stmt =
  let
    g = traverseStmt cols -- inner statments are recursively folded
    f = traverseExpr cols
    h = traverseLVal cols
    k = traverseType cols

    h' (ident, ty) = (\ty' -> (ident, ty')) <$> (k ty)

    collectStmt = case stmt of
      AS.StmtCall qIdent argEs -> do
        (qIdent', argEs') <- traceSyntax (qIdent, argEs) (collectSyntax cols (qIdent, argEs))
        let stmt' = AS.StmtCall qIdent' argEs'
        traceSyntax stmt' (collectSyntax cols stmt')
      _ -> traceSyntax stmt (collectSyntax cols stmt)

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

  in collectStmt >>= \stmt' -> case stmt' of
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

instance ShowableExt ext => Show (SyntaxTraceStack ext) where
  show (SyntaxTraceStack stacks) = let
    stmts = stacks SyntaxStmtRepr
    exprs = stacks SyntaxExprRepr
    calls = stacks SyntaxCallRepr
    in "Statement call stack:\n"
      ++ unlines (map prettyToplevelStmt (List.reverse stmts))
      ++ "Expression call stack:\n"
      ++ unlines (map prettyToplevelExpr (List.reverse exprs))
      ++ "Function call stack:\n"
      ++ unlines (map prettyToplevelCall (List.reverse calls))

withStar :: String -> String
withStar (' ' : rest) = '*' : rest
withStar s = s

atDepth :: Int -> String -> String
atDepth depth s = concat (replicate depth " ") ++ s

withLines :: [String] -> String
withLines strs = List.intercalate "\n" strs

showExt :: Show t => t -> String
showExt ext = case show ext of
  "" -> ""
  s -> ">>>" ++ s ++ "\n"

prettyToplevelStmt :: Show t => (AS.Stmt, t) -> String
prettyToplevelStmt (stmt, ext) = showExt ext ++ withStar (prettyStmt 3 stmt)

prettyToplevelExpr :: Show t => (AS.Expr, t) -> String
prettyToplevelExpr (expr, ext) = showExt ext ++ "*  " ++ prettyExpr expr

prettyToplevelCall :: Show t => ((AS.QualifiedIdentifier, [AS.Expr]), t) -> String
prettyToplevelCall (call, ext) = showExt ext ++ "*  " ++ prettyCall call

prettyCall :: (AS.QualifiedIdentifier, [AS.Expr]) -> String
prettyCall call = show call

prettyStmt :: Int -> AS.Stmt -> String
prettyStmt depth stmt = case stmt of
  AS.StmtIf tests melse ->
    atDepth depth "StmtIf: " ++
    unlines (map (\(test, stmts) ->
           prettyExpr test ++ "\n"
           ++ withLines (map (prettyStmt $ depth + 1) stmts)) tests)
    ++
    case melse of
      Just stmts -> (atDepth depth "Else\n") ++ withLines (map (prettyStmt $ depth + 1) stmts)
      Nothing -> ""
  AS.StmtFor var range stmts ->
    atDepth depth "StmtFor: " ++ show var ++ show range ++ "\n"
      ++ withLines (map (prettyStmt $ depth + 1) stmts)
  AS.StmtRepeat stmts test ->
    atDepth depth "StmtRepeat: " ++ prettyExpr test ++ "\n"
    ++ withLines (map (prettyStmt $ depth + 1) stmts)
  _ -> atDepth depth $ show stmt

prettyExpr :: AS.Expr -> String
prettyExpr expr = show expr

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
