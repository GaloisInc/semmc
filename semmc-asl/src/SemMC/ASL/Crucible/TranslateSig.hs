{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
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

module SemMC.ASL.Crucible.TranslateSig (
    computeSignatures
  , SignatureMap
  , SigException(..)
  , SigState(..)
  ) where

import qualified Control.Monad.Except as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.RWS as RWS
import           Data.Foldable (forM_)
import           Data.List (nub)
import           Data.Maybe (maybeToList, catMaybes, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Traversable (forM)
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Signature
import           SemMC.ASL.Translation ( UserType(..), userTypeRepr )

type SignatureMap = Map.Map T.Text SomeSignature

computeSignatures :: [AS.Definition] -> Either (SigException, SigState) SignatureMap
computeSignatures defs =
  let env = buildEnv defs
  in execSigM env $ do
    forM_ (Map.elems (callables env)) $ \def -> do
      computeCallableSignature def
    finalState <- RWS.get
    return $ callableSignatureMap finalState

buildEnv :: [AS.Definition] -> SigEnv
buildEnv defs =
  let callables = Map.fromList ((\c -> (getCallableName c, c)) <$> (catMaybes (asCallable <$> defs)))
      globalVars = Map.fromList ((\v -> (getVariableName v, v)) <$> (catMaybes (asDefVariable <$> defs)))
      types = Map.fromList ((\t -> (getTypeName t, t)) <$> (catMaybes (asDefType <$> defs)))
      -- | TODO: Populate builtin types
      builtinTypes = Map.empty
      getCallableName c = let AS.QualifiedIdentifier _ name = callableName c
                          in name
      getVariableName v = let DefVariable name _ = v
                          in name
      getTypeName t = case t of
        DefTypeBuiltin name -> name
        DefTypeAbstract name -> name
        DefTypeAlias name _ -> name
        DefTypeStruct (AS.QualifiedIdentifier _ name) _ -> name
        DefTypeEnum name _ -> name
  in SigEnv {..}

-- FIXME: We currently do not capture 'DefArray', 'DefGetter', and 'DefSetter'
-- constructors; that needs to happen.

data Callable = Callable { callableName :: AS.QualifiedIdentifier
                         , callableArgs :: [AS.SymbolDecl]
                         , callableRets :: [AS.Type]
                         , callableStmts :: [AS.Stmt]
                         }

asCallable :: AS.Definition -> Maybe Callable
asCallable def =
  case def of
    AS.DefCallable { AS.callableName = name
                   , AS.callableArgs = args
                   , AS.callableRets = rets
                   , AS.callableStmts = stmts
                   } ->
      Just Callable { callableName = name
                    , callableArgs = args
                    , callableRets = rets
                    , callableStmts = stmts
                    }
    _ -> Nothing

data DefType = DefTypeBuiltin AS.Identifier
             | DefTypeAbstract AS.Identifier
             | DefTypeAlias AS.Identifier AS.Type
             | DefTypeStruct AS.QualifiedIdentifier [AS.SymbolDecl]
             | DefTypeEnum AS.Identifier [AS.Identifier]

asDefType :: AS.Definition -> Maybe DefType
asDefType def =
  case def of
    AS.DefTypeBuiltin ident -> Just $ DefTypeBuiltin ident
    AS.DefTypeAbstract ident -> Just $ DefTypeAbstract ident
    AS.DefTypeAlias ident tp -> Just $ DefTypeAlias ident tp
    AS.DefTypeStruct ident decls -> Just $ DefTypeStruct ident decls
    AS.DefTypeEnum ident idents -> Just $ DefTypeEnum ident idents
    _ -> Nothing


data DefVariable = DefVariable AS.Identifier AS.Type

-- | We also capture 'DefConst's here to simplify things; they are also
-- straightforward mappings from names to types, just like variables.
asDefVariable :: AS.Definition -> Maybe DefVariable
asDefVariable def = case def of
  AS.DefVariable (AS.QualifiedIdentifier _ ident) tp -> Just (DefVariable ident tp)
  AS.DefConst ident tp _ -> Just (DefVariable ident tp)
  _ -> Nothing

-- | Monad for computing ASL signatures of 'AS.Definition's.
--
-- The environment provided is a list of ASL definitions -- callables, type
-- declarations, and global variable declarations.
--
-- The state is a mapping from names to signatures. The idea is that we start with an
-- empty state, then traverse the definitions, running 'computeDefinitionSignature'
-- on each one and storing its definition in the lookup table.

newtype SigM a = SigM { getSigM :: E.ExceptT SigException (RWS.RWS SigEnv () SigState) a }
  deriving ( Functor
           , Applicative
           , Monad
           , RWS.MonadReader SigEnv
           , RWS.MonadState SigState
           , E.MonadError SigException
           )

execSigM :: SigEnv -> SigM a -> Either (SigException, SigState) a
execSigM env action =
  let rws = E.runExceptT $ getSigM action
      (e, finalState, _) = RWS.runRWS rws env initState
  in case e of
    Left err -> Left (err, finalState)
    Right a -> Right a
  where initState = SigState Map.empty Map.empty Map.empty Seq.empty

data SigEnv = SigEnv { callables :: Map.Map T.Text Callable
                     , globalVars :: Map.Map T.Text DefVariable
                     , types :: Map.Map T.Text DefType
                     , builtinTypes :: Map.Map T.Text (Some UserType)
                     }

data SigState = SigState { userTypes :: Map.Map T.Text (Some UserType)
                           -- ^ user-defined types
                         , callableGlobalsMap :: Map.Map T.Text [(T.Text, Some WT.BaseTypeRepr)]
                           -- ^ map from function/procedure name to list of globals
                         , callableSignatureMap :: Map.Map T.Text SomeSignature
                           -- ^ map of all signatures found thus far
                         , unfoundCallables :: Seq.Seq T.Text
                           -- ^ list of callables we encountered that were not in the
                           -- pre-loaded environment
                         }

data SigException = TypeNotFound T.Text
                  | BuiltinTypeNotFound T.Text
                  | CallableNotFound T.Text
  deriving (Eq, Show)

storeType :: T.Text -> UserType tp -> SigM ()
storeType tpName tp = do
  st <- RWS.get
  RWS.put $ st { userTypes = Map.insert tpName (Some tp) (userTypes st) }

lookupCallable :: T.Text -> SigM Callable
lookupCallable fnName = do
  env <- RWS.ask
  case Map.lookup fnName (callables env) of
    Just callable -> return callable
    Nothing -> E.throwError $ CallableNotFound fnName

lookupBuiltinType :: T.Text -> SigM (Some UserType)
lookupBuiltinType tpName = do
  env <- RWS.ask
  case Map.lookup tpName (builtinTypes env) of
    Just tp -> return tp
    Nothing -> E.throwError $ BuiltinTypeNotFound tpName

lookupDefType :: T.Text -> SigM DefType
lookupDefType tpName = do
  env <- RWS.ask
  case Map.lookup tpName (types env) of
    Just defType -> return defType
    Nothing -> E.throwError $ TypeNotFound tpName

-- | If the variable is present, return its definition. Otherwise, return 'Nothing'.
lookupGlobalVar :: T.Text -> SigM (Maybe DefVariable)
lookupGlobalVar varName = do
  env <- RWS.ask
  return $ Map.lookup varName (globalVars env)

lookupCallableGlobals :: T.Text -> SigM (Maybe [(T.Text, Some WT.BaseTypeRepr)])
lookupCallableGlobals callableName = do
  globalsMap <- callableGlobalsMap <$> RWS.get
  return $ Map.lookup callableName globalsMap

storeCallableGlobals :: T.Text -> [(T.Text, Some WT.BaseTypeRepr)] -> SigM ()
storeCallableGlobals callableName globals = do
  st <- RWS.get
  RWS.put $ st { callableGlobalsMap = Map.insert callableName globals (callableGlobalsMap st) }

lookupCallableSignature :: T.Text -> SigM (Maybe SomeSignature)
lookupCallableSignature callableName = do
  signatureMap <- callableSignatureMap <$> RWS.get
  return $ Map.lookup callableName signatureMap

storeCallableSignature :: T.Text -> SomeSignature -> SigM ()
storeCallableSignature callableName sig = do
  st <- RWS.get
  RWS.put $ st { callableSignatureMap = Map.insert callableName sig (callableSignatureMap st) }

addUnfoundCallable :: T.Text -> SigM ()
addUnfoundCallable callableName = do
  st <- RWS.get
  RWS.put $ st { unfoundCallables = callableName Seq.:<| unfoundCallables st }

-- | Compute the What4 representation of a user-defined ASL type, from the name of
-- the type as a 'T.Text'. Store it in 'typeSigs' (if it isn't already there).
computeUserType :: T.Text -> SigM (Some UserType)
computeUserType tpName = do
  -- If the type has already been computed, it will be in the 'userTypes' map.
  mTp <- Map.lookup tpName <$> userTypes <$> RWS.get
  case mTp of
    Just tp -> return tp
    Nothing -> do
      -- If it has not already been computed, then compute, store and return it.
      defType <- lookupDefType tpName
      Some tp <- case defType of
        DefTypeBuiltin builtinTpName -> lookupBuiltinType builtinTpName
        DefTypeEnum _ enumVals -> do
          -- Enumeration types are represented as integers.
          -- FIXME: somehow store the 'enumVals' in the 'SigM' monad so that we
          -- can resolve their type when we encounter them
          return $ Some $ UserEnum (fromIntegral (length enumVals))
        DefTypeStruct _ structVars -> do
          varTps <- forM structVars $ \(varName, varType) -> do
            Some tp <- computeType varType
            return $ Some $ LabeledValue varName tp
          Some varTpAssignment <- return $ someAssignment varTps
          return $ Some $ UserStruct varTpAssignment
        DefTypeAbstract _ -> error "computeUserType: abstract type"
        _ -> error $ "computeUserType: unsupported type " ++ T.unpack tpName
      storeType tpName tp
      return $ Some tp

-- | Compute the What4 representation of an ASL 'AS.Type'.
computeType :: AS.Type -> SigM (Some WT.BaseTypeRepr)
computeType tp = case tp of
  AS.TypeRef (AS.QualifiedIdentifier _ tpName) -> do
    Some userType <- computeUserType tpName
    return $ Some $ userTypeRepr userType
  AS.TypeFun "bits" e ->
    case e of
      AS.ExprLitInt w
        | Just (Some wRepr) <- NR.someNat w
        , Just NR.LeqProof <- NR.isPosNat wRepr -> return $ Some (WT.BaseBVRepr wRepr)
      _ -> error "computeType, TypeFun"
  AS.TypeOf _ -> error "computeType, TypeOf"
  AS.TypeReg _ _ -> error "computeType, TypeReg"
  AS.TypeArray _ _ -> error "computeType, TypeArray"
  _ -> error "computeType"

-- | If the identifier is a global variable, return its type. Otherwise, return 'Nothing'.
computeGlobalVarType :: T.Text -> SigM (Maybe (Some WT.BaseTypeRepr))
computeGlobalVarType varName = do
  mVar <- lookupGlobalVar varName
  case mVar of
    Nothing -> return Nothing
    Just (DefVariable _ asType) -> do
      tp <- computeType asType
      return $ Just tp

-- | Given a variable name, determine whether it is a global variable or not. If so,
-- return a pair containing the variable and its type; if not, return 'Nothing'.
varGlobal :: T.Text -> SigM (Maybe (T.Text, Some WT.BaseTypeRepr))
varGlobal varName = do
  mVarType <- computeGlobalVarType varName
  return $ (varName,) <$> mVarType

sliceGlobalVars :: AS.Slice -> SigM [(T.Text, Some WT.BaseTypeRepr)]
sliceGlobalVars slice = case slice of
  AS.SliceSingle e -> exprGlobalVars e
  AS.SliceOffset e1 e2 -> (++) <$> exprGlobalVars e1 <*> exprGlobalVars e2
  AS.SliceRange e1 e2 -> (++) <$> exprGlobalVars e1 <*> exprGlobalVars e2

setEltGlobalVars :: AS.SetElement -> SigM [(T.Text, Some WT.BaseTypeRepr)]
setEltGlobalVars setElt = case setElt of
  AS.SetEltSingle e -> exprGlobalVars e
  AS.SetEltRange e1 e2 -> (++) <$> exprGlobalVars e1 <*> exprGlobalVars e2

lValExprGlobalVars :: AS.LValExpr -> SigM [(T.Text, Some WT.BaseTypeRepr)]
lValExprGlobalVars lValExpr = case lValExpr of
  AS.LValVarRef (AS.QualifiedIdentifier _ varName) -> maybeToList <$> varGlobal varName
  AS.LValMember le varName -> do
    leGlobals <- lValExprGlobalVars le
    varGlobals <- maybeToList <$> varGlobal varName
    return $ leGlobals ++ varGlobals
  AS.LValMemberArray le vars -> do
    leGlobals <- lValExprGlobalVars le
    varGlobals <- catMaybes <$> traverse varGlobal vars
    return $ leGlobals ++ varGlobals
  AS.LValArrayIndex le slices -> do
    leGlobals <- lValExprGlobalVars le
    sliceGlobals <- concat <$> traverse sliceGlobalVars slices
    return $ leGlobals ++ sliceGlobals
  AS.LValSliceOf le slices -> do
    leGlobals <- lValExprGlobalVars le
    sliceGlobals <- concat <$> traverse sliceGlobalVars slices
    return $ leGlobals ++ sliceGlobals
  AS.LValArray les ->
    concat <$> traverse lValExprGlobalVars les
  AS.LValTuple les ->
    concat <$> traverse lValExprGlobalVars les
  AS.LValMemberBits le vars -> do
    leGlobals <- lValExprGlobalVars le
    varGlobals <- catMaybes <$> traverse varGlobal vars
    return $ leGlobals ++ varGlobals
  AS.LValSlice les ->
    concat <$> traverse lValExprGlobalVars les
  _ -> return []

casePatternGlobalVars :: AS.CasePattern -> SigM [(T.Text, Some WT.BaseTypeRepr)]
casePatternGlobalVars pat = case pat of
  AS.CasePatternIdentifier varName -> maybeToList <$> varGlobal varName
  AS.CasePatternTuple pats -> concat <$> traverse casePatternGlobalVars pats
  _ -> return []

caseAlternativeGlobalVars :: AS.CaseAlternative -> SigM [(T.Text, Some WT.BaseTypeRepr)]
caseAlternativeGlobalVars alt = case alt of
  AS.CaseWhen pats mExpr stmts -> do
    patGlobals <- concat <$> traverse casePatternGlobalVars pats
    eGlobals <- fromMaybe [] <$> traverse exprGlobalVars mExpr
    stmtGlobals <- concat <$> traverse stmtGlobalVars stmts
    return $ patGlobals ++ eGlobals ++ stmtGlobals
  AS.CaseOtherwise stmts -> concat <$> traverse stmtGlobalVars stmts

-- | Collect all global variables from a single 'AS.Expr'.
exprGlobalVars :: AS.Expr -> SigM [(T.Text, Some WT.BaseTypeRepr)]
exprGlobalVars expr = case expr of
  AS.ExprVarRef (AS.QualifiedIdentifier _ varName) ->
    maybeToList <$> varGlobal varName
  AS.ExprSlice e slices -> do
    eGlobals <- exprGlobalVars e
    sliceGlobals <- concat <$> traverse sliceGlobalVars slices
    return $ eGlobals ++ sliceGlobals
  AS.ExprIndex e slices -> do
    eGlobals <- exprGlobalVars e
    sliceGlobals <- concat <$> traverse sliceGlobalVars slices
    return $ eGlobals ++ sliceGlobals
  AS.ExprUnOp _ e -> exprGlobalVars e
  AS.ExprBinOp _ e1 e2 -> do
    e1Globals <- exprGlobalVars e1
    e2Globals <- exprGlobalVars e2
    return $ e1Globals ++ e2Globals
  AS.ExprMembers e vars -> do
    eGlobals <- exprGlobalVars e
    varGlobals <- catMaybes <$> traverse varGlobal vars
    return $ eGlobals ++ varGlobals
  AS.ExprInMask e _ -> exprGlobalVars e
  AS.ExprMemberBits e vars -> do
    eGlobals <- exprGlobalVars e
    varGlobals <- catMaybes <$> traverse varGlobal vars
    return $ eGlobals ++ varGlobals
  AS.ExprCall (AS.QualifiedIdentifier _ callableName) argEs -> do
    callableGlobals <- E.catchError
      (do callable <- lookupCallable callableName
          callableGlobalVars callable )
      $ \e -> case e of
                CallableNotFound _ -> do
                  addUnfoundCallable callableName
                  return []
                _ -> E.throwError e
    -- callableGlobals <- callableGlobalVars callable
    argGlobals <- concat <$> traverse exprGlobalVars argEs
    return $ callableGlobals ++ argGlobals
  AS.ExprInSet e setElts -> do
    eGlobals <- exprGlobalVars e
    setEltGlobals <- concat <$> traverse setEltGlobalVars setElts
    return $ eGlobals ++ setEltGlobals
  AS.ExprTuple es ->
    concat <$> traverse exprGlobalVars es
  AS.ExprIf branches def -> do
    branchGlobals <- forM branches $ \(testExpr, resExpr) -> do
      testExprGlobals <- exprGlobalVars testExpr
      resExprGlobals <- exprGlobalVars resExpr
      return $ testExprGlobals ++ resExprGlobals
    defaultGlobals <- exprGlobalVars def
    return $ concat branchGlobals ++ defaultGlobals
  AS.ExprMember e var -> do
    eGlobals <- exprGlobalVars e
    varGlobals <- maybeToList <$> varGlobal var
    return $ eGlobals ++ varGlobals
  _ -> return []

-- | Collect all global variables from a single 'AS.Stmt'.
stmtGlobalVars :: AS.Stmt -> SigM [(T.Text, Some WT.BaseTypeRepr)]
stmtGlobalVars stmt = case stmt of
  AS.StmtVarDeclInit _ e -> exprGlobalVars e
  AS.StmtAssign le e -> (++) <$> lValExprGlobalVars le <*> exprGlobalVars e
  AS.StmtCall (AS.QualifiedIdentifier _ callableName) argEs -> do
    callable <- lookupCallable callableName
    callableGlobals <- callableGlobalVars callable
    argGlobals <- concat <$> traverse exprGlobalVars argEs
    return $ callableGlobals ++ argGlobals
  AS.StmtReturn (Just e) -> exprGlobalVars e
  AS.StmtAssert e -> exprGlobalVars e
  AS.StmtIf branches mDefault -> do
    branchGlobals <- forM branches $ \(testExpr, stmts) -> do
      testExprGlobals <- exprGlobalVars testExpr
      stmtGlobals <- concat <$> traverse stmtGlobalVars stmts
      return $ testExprGlobals ++ stmtGlobals
    defaultGlobals <- case mDefault of
      Nothing -> return []
      Just stmts -> concat <$> traverse stmtGlobalVars stmts
    return $ concat branchGlobals ++ defaultGlobals
  AS.StmtCase e alts -> do
    eGlobals <- exprGlobalVars e
    altGlobals <- concat <$> traverse caseAlternativeGlobalVars alts
    return $ eGlobals ++ altGlobals
  AS.StmtFor _ (initialize, term) stmts -> do
    initGlobals <- exprGlobalVars initialize
    termGlobals <- exprGlobalVars term
    stmtGlobals <- concat <$> traverse stmtGlobalVars stmts
    return $ initGlobals ++ termGlobals ++ stmtGlobals
  AS.StmtWhile term stmts -> do
    termGlobals <- exprGlobalVars term
    stmtGlobals <- concat <$> traverse stmtGlobalVars stmts
    return $ termGlobals ++ stmtGlobals
  AS.StmtRepeat stmts term -> do
    termGlobals <- exprGlobalVars term
    stmtGlobals <- concat <$> traverse stmtGlobalVars stmts
    return $ termGlobals ++ stmtGlobals
  _ -> return []

-- | Compute the list of global variables in a 'Callable' and store it in the
-- state. If it has already been computed, simply return it.
callableGlobalVars :: Callable -> SigM [(T.Text, Some WT.BaseTypeRepr)]
callableGlobalVars Callable{..} = do
  let AS.QualifiedIdentifier _ name = callableName
  mGlobals <- lookupCallableGlobals name
  case mGlobals of
    Just globals -> return globals
    Nothing -> do
      globals <- concat <$> traverse stmtGlobalVars callableStmts
      storeCallableGlobals name globals
      return globals

-- | Compute the signature of a callable (function/procedure). Currently, we assume
-- that if the return list is empty, it is a procedure, and if it is nonempty, then
-- it is a function.
computeCallableSignature :: Callable -> SigM SomeSignature
computeCallableSignature callable@Callable{..} = do
  let AS.QualifiedIdentifier _ name = callableName
  mSig <- lookupCallableSignature name

  case mSig of
    Just sig -> return sig
    Nothing -> do
      globalVars <- callableGlobalVars callable
      labeledVals <- forM (nub globalVars) $ \(varName, Some varTp) -> do
        return $ Some (LabeledValue varName varTp)
      labeledArgs <- forM callableArgs $ \(argName, asType) -> do
        Some tp <- computeType asType
        let ctp = CT.baseToType tp-- traverse computeType (snd <$> callableArgs)
        return (Some (LabeledValue argName ctp))

      Some globalReprs <- return $ someAssignment labeledVals
      Some argReprs <- return $ someAssignment labeledArgs
      sig <- case callableRets of
        [] -> do -- procedure
          return $ SomeProcedureSignature $ ProcedureSignature
            { procName = name
            , procArgReprs = argReprs
            , procGlobalReprs = globalReprs
            }
        _ -> do -- function
          Some sigRepr <- case callableRets of
            [asType] -> computeType asType
            asTypes -> do
              someTypes <- traverse computeType asTypes
              Some assignment <- return $ someAssignment someTypes
              return $ Some (WT.BaseStructRepr assignment)
          return $ SomeFunctionSignature $ FunctionSignature
            { funcName = name
            , funcSigRepr = sigRepr
            , funcArgReprs = argReprs
            , funcGlobalReprs = globalReprs
            }
      storeCallableSignature name sig
      return sig

someAssignment :: [Some f] -> Some (Ctx.Assignment f)
someAssignment [] = Some Ctx.empty
someAssignment (Some f : rst) = I.runIdentity $ do
  Some assignRst <- return $ someAssignment rst
  return $ Some (Ctx.extend assignRst f)
