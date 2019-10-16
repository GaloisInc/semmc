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

module SemMC.ASL.Translation.Preprocess
  ( -- * Top-level interface
    getDefinitions
  , computeInstructionSignature'
  , prepASL
  , ASLSpec(..)
  , SigState
  , SigEnv
  , SigM
  , runSigM
  , buildSigState
  , SigException(..)
  , InnerSigException(..)
  , Callable(..)
  , Definitions(..)
  , bitsToInteger
  , mkFunctionName
  , applyStaticEnv
  , mkSignature
  , registerTypeSynonyms
  , localTypeHints
  , mkExtendedTypeData'
  , mkBaseStructRepr
  , mkFinalFunctionName
  ) where

import Debug.Trace (traceM)

import           Control.Applicative ( (<|>) )
import qualified Control.Exception as X
import           Control.Monad (void)
import qualified Control.Monad.Except as E
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State.Class as MS
import qualified Control.Monad.State as MSS
import           Control.Monad.Trans.Maybe as MaybeT
import qualified Data.BitVector.Sized as BVS
import           Data.Foldable (find)
import           Data.List (nub)
import           Data.Maybe (maybeToList, catMaybes, fromMaybe, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..), viewSome, mapSome )

import qualified Data.Text as T
import           Data.Traversable (forM)
import qualified Data.Parameterized.TraversableFC as FC
import qualified Lang.Crucible.CFG.Expr as CCE
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Extension
import           SemMC.ASL.Signature
import           SemMC.ASL.Types
import           SemMC.ASL.StaticExpr
import qualified SemMC.ASL.SyntaxTraverse as ASLT
import           SemMC.ASL.SyntaxTraverse (mkFunctionName)
import           SemMC.ASL.Exceptions (TranslationException (CannotStaticallyEvaluateType))

import System.IO.Unsafe -- FIXME: For debugging

----------------
-- Notes
--
-- * Structs - capture each member of a struct as an individual global
-- variable.
--
-- * Arrays - capture 'DefArray' as a bunch of individual global variables, one for
-- each index value.
--
-- * Investigate SCR and make sure we are doing the right thing for that. I don't
-- actually think we are.
--
-- Questions
--
-- * Should we handle getters and setters as functions & procedures? It might
-- actually be relatively straightforward to do so. At the moment, these are silently
-- skipped.
--
-- * How do we deal with dependently-typed functions? Do we actually need to?

-- | Compute the signature of a single callable, given its name and arity.
computeSignature :: AS.QualifiedIdentifier -> Int -> SigM ext f ()
computeSignature qName arity = do
  mCallable <- lookupCallable qName arity
  case mCallable of
    Nothing -> throwError $ CallableNotFound (mkFunctionName qName arity)
    Just c -> void $ computeCallableSignature c

computeSignature' :: T.Text -> SigM ext f ()
computeSignature' name = do
  mCallable <- lookupCallable' name
  case mCallable of
    Nothing -> throwError $ CallableNotFound name
    Just c -> void $ computeCallableSignature c


data Definitions arch =
  Definitions { defSignatures :: Map.Map T.Text (SomeSimpleSignature, [AS.Stmt])
              , defDepSignatures :: Map.Map T.Text (SomeDFS, [AS.Stmt])
              , defTypes :: Map.Map T.Text (Some UserType)
              , defEnums :: Map.Map T.Text Integer
              , defConsts :: Map.Map T.Text (Some ConstVal)
              , defExtendedTypes :: Map.Map T.Text ExtendedTypeData
              }

-- | Collect the definitions representing the current state

getDefinitions :: SigM ext f (Definitions arch)
getDefinitions = do
  st <- RWS.get
  env <- RWS.ask
  return $ Definitions
    { defSignatures = (\(sig, c) -> (sig, callableStmts c)) <$> callableSignatureMap st
    , defDepSignatures = Map.empty
    , defTypes = userTypes st
    , defEnums = enums env
    , defConsts = consts env
    , defExtendedTypes = extendedTypeData st
    }

builtinGlobals :: [(T.Text, Some WT.BaseTypeRepr)]
builtinGlobals =
  [ ("UNDEFINED", Some WT.BaseBoolRepr)
  , ("UNPREDICTABLE", Some WT.BaseBoolRepr)
  , ("EXCEPTIONTAKEN", Some WT.BaseBoolRepr)
  , ("ASSERTIONFAILURE", Some WT.BaseBoolRepr)
  , ("CurrentInstrSet", Some WT.BaseIntegerRepr)
  ]


-- NOTE: This is clagged from types.asl, in
-- theory we could read this in instead.
registerTypeSynonyms :: [(T.Text, T.Text)]
registerTypeSynonyms =
  [ ("CPACRType", "CPACR_EL1")
  , ("CNTKCTLType", "CNTKCTL_EL1")
  , ("ESRType", "ESR_EL1")
  , ("FPCRType", "FPCR")
  , ("MAIRType", "MAIR_EL1")
  , ("SCRType", "SCR")
  , ("SCTLRType", "SCTLR_EL1")
  ]



builtinConsts :: [(T.Text, Some ConstVal)]
builtinConsts =
  [ ("TRUE", Some $ ConstVal WT.BaseBoolRepr True)
  , ("FALSE", Some $ ConstVal WT.BaseBoolRepr False)
  , ("HIGH", Some $ ConstVal (WT.BaseBVRepr (WT.knownNat @1)) (BVS.bitVector (1 :: Integer)))
  ]

-- FIXME: We currently do not capture 'DefArray', 'DefGetter', and 'DefSetter'
-- constructors; that needs to happen.

data Callable = Callable { callableName :: AS.QualifiedIdentifier
                         , callableArgs :: [AS.SymbolDecl]
                         , callableRets :: [AS.Type]
                         , callableStmts :: [AS.Stmt]
                         }
  deriving (Eq, Show)

asCallable :: AS.Definition -> Maybe Callable
asCallable def =
  case def of
    AS.DefCallable { AS.callableName = name
                   , AS.callableArgs = args
                   , AS.callableRets = rets
                   , AS.callableStmts = stmts
                   }
      | not $ null stmts ->
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
  deriving Show

mkCallableName :: Callable -> T.Text
mkCallableName c =
  let numArgs = length (callableArgs c)
  in mkFunctionName (callableName c) numArgs

asDefType :: AS.Definition -> Maybe DefType
asDefType def =
  case def of
    AS.DefTypeBuiltin ident -> Just $ DefTypeBuiltin ident
    AS.DefTypeAbstract ident -> Just $ DefTypeAbstract ident
    AS.DefTypeAlias ident tp -> Just $ DefTypeAlias ident tp
    AS.DefTypeStruct ident decls -> Just $ DefTypeStruct ident decls
    AS.DefTypeEnum ident idents -> Just $ DefTypeEnum ident idents
    _ -> Nothing


-- | Monad for computing ASL signatures of 'AS.Definition's.
newtype SigM ext f a = SigM { getSigM :: E.ExceptT SigException (RWS.RWS SigEnv () SigState) a }
  deriving ( Functor
           , Applicative
           , Monad
           , RWS.MonadReader SigEnv
           , RWS.MonadState SigState
           , E.MonadError SigException
           )

data ASLSpec = ASLSpec
  { aslInstructions :: [AS.Instruction]
  , aslDefinitions :: [AS.Definition]
  , aslSupportDefinitions :: [AS.Definition]
  , aslExtraDefinitions :: [AS.Definition]
  , aslRegisterDefinitions :: [AS.RegisterDefinition]
  }

-- FIXME: One hardcoded register missing from the ASL
extraRegisters :: [AS.RegisterDefinition]
extraRegisters = [
  AS.RegisterDefSingle (AS.Register "TTBCR_S" 32
                       [ mkField "EAE" 31 31
                       , mkField "PD1" 5 5
                       , mkField "PD0" 4 4
                       , mkField "N" 0 2
                       , mkField "SH1" 28 29
                       , mkField "ORGN1" 26 27
                       , mkField "IRGN1" 24 25
                       , mkField "EPD1" 23 23
                       , mkField "T1SZ" 16 18
                       , mkField "SH0" 12 13
                       , mkField "ORGN0" 10 11
                       , mkField "IRGN0" 8 9
                       , mkField "EPD0" 6 6
                       , mkField "T2E" 6 6
                       , mkField "T0SZ" 0 2
                       ])
  ]
  where
    mkField nm lo hi = AS.RegisterField (Just nm) lo hi


prepASL :: ASLSpec -> ASLSpec
prepASL (ASLSpec instrs defs sdefs edefs rdefs) =
  let
    ovrs = ASLT.mkSyntaxOverrides (defs ++ sdefs ++ edefs)
    f = ASLT.applySyntaxOverridesDefs ovrs
    g = ASLT.applySyntaxOverridesInstrs ovrs
  in
    ASLSpec (g instrs) (f defs) (f sdefs) (f edefs) (rdefs ++ extraRegisters)


getRegisterType :: AS.Register -> Some WT.BaseTypeRepr
getRegisterType r =
  if | Just (Some wRepr) <- NR.someNat (AS.regLength r)
     , Just NR.LeqProof <- NR.isPosNat wRepr
     -> Some (WT.BaseBVRepr wRepr)

getRegisterArrayType :: AS.RegisterArray -> Some WT.BaseTypeRepr
getRegisterArrayType ra =
  case getRegisterType (AS.regDef ra) of
    Some t -> Some (WT.BaseArrayRepr (Ctx.empty Ctx.:> WT.BaseIntegerRepr) t)

getRegisterDefType :: AS.RegisterDefinition -> (T.Text, Some WT.BaseTypeRepr)
getRegisterDefType rd = case rd of
  AS.RegisterDefSingle r -> (AS.regName r, getRegisterType r)
  AS.RegisterDefArray ra -> (AS.regName (AS.regDef ra), getRegisterArrayType ra)

getRegisterFieldSlice :: AS.RegisterField -> Maybe (T.Text, (Integer, Integer))
getRegisterFieldSlice rf = case AS.regFieldName rf of
  Just nm -> Just (nm, (AS.regFieldLo rf, AS.regFieldHi rf))
  _ -> Nothing

getRegisterSig :: AS.Register -> RegisterSig
getRegisterSig r =
  Map.fromList $ catMaybes $ map getRegisterFieldSlice (AS.regFields r)

getRegisterDefSig :: AS.RegisterDefinition -> (T.Text, ExtendedTypeData)
getRegisterDefSig rd = case rd of
  AS.RegisterDefSingle r -> (AS.regName r, TypeRegister $ getRegisterSig r)
  AS.RegisterDefArray ra -> (AS.regName (AS.regDef ra), TypeArray $ TypeRegister $ getRegisterSig (AS.regDef ra))

mkCallableOverrideVariant :: Callable -> Callable
mkCallableOverrideVariant Callable{..} =
  Callable nm' callableArgs callableRets callableStmts
  where
    getTypeStr t = case t of
      AS.TypeRef (AS.QualifiedIdentifier _ tpName) -> tpName
      AS.TypeFun "bits" (AS.ExprVarRef (AS.QualifiedIdentifier _ n)) -> "bits" <> n
      _ -> error $ "Bad type for override variant" ++ show t
    nm' = ASLT.mapInnerName (\nm -> T.concat $ nm : map (\(nm, t) -> getTypeStr t) callableArgs) callableName

-- FIXME: This is a gross hack for the fact that the combined support.asl
-- ends up with multiple versions of the same functions
preferLongerCallable :: Callable -> Maybe Callable -> Maybe Callable
preferLongerCallable c1 (Just c2) =
  if (length $ callableStmts c1) > (length $ callableStmts c2)
  then Just c1
  else Just c2
preferLongerCallable c1 Nothing = Just c1

uniqueCallables :: Callable -> Maybe Callable -> Maybe Callable
uniqueCallables c1 (Just c2) = error $ "Duplicate function declarations for: " <> (show $ callableName c1)
uniqueCallables c1 Nothing = Just c1



buildCallableMap :: (Callable -> Maybe Callable -> Maybe Callable)
                 -> [(T.Text, Callable)] -> Map.Map T.Text Callable
buildCallableMap merge cs =
  let foo = Map.fromListWith (++) (map (\(nm,c) -> (nm, [c])) cs)
      (overrides, foo') = Map.mapAccumWithKey getOverrides [] foo
      foo'' = Map.mapMaybe id foo' in
  foldr (\c -> Map.alter (merge c) (mkCallableName c))  foo'' overrides

  where
    getOverrides a nm [c] = (a, Just c)
    getOverrides a nm cs = case nub cs of
           [c] -> (a, Just c)
           cs -> (map mkCallableOverrideVariant cs ++ a, Nothing)




mkExtendedTypeData' :: Monad m
                   => (T.Text -> m (Maybe (Some UserType)))
                   -> (T.Text -> m ExtendedTypeData)
                   -> AS.Type
                   -> m ExtendedTypeData
mkExtendedTypeData' getUT getET ty = do
  case ty of
    AS.TypeRef qi@(AS.QualifiedIdentifier _ tident) -> do
      uts <- getUT tident
      case uts of
        Just s -> return $ fromUT s
        Nothing -> do
          case lookup tident registerTypeSynonyms of
            Just nm -> getET nm
            Nothing -> return TypeBasic
    _ -> return TypeBasic
  where
    fromUT :: Some UserType -> ExtendedTypeData
    fromUT ut = case ut of
      Some (UserStruct s) -> do
        let (_, asn) = MSS.runState (Ctx.traverseAndCollect
                                     (collectAssignment (FC.fmapFC projectValue s)) s) Map.empty
        TypeStruct asn
      _ -> TypeBasic
    collectAssignment :: Ctx.Assignment WT.BaseTypeRepr tps
                      -> Ctx.Index tps tp
                      -> LabeledValue (T.Text, Maybe (Some UserType)) WT.BaseTypeRepr tp
                      -> MSS.State (Map.Map T.Text StructAccessor) ()
    collectAssignment repr idx lblv = do
      let (nm, mUT) = projectLabel lblv
      ext <- case mUT of
        Just sUT -> return $ fromUT sUT
        Nothing -> return $ TypeBasic
      MS.modify' (Map.insert nm (StructAccessor repr idx ext))

mkExtendedTypeData :: AS.Type -> SigM ext f (ExtendedTypeData)
mkExtendedTypeData = mkExtendedTypeData' getUT getET
  where
    getUT tpName = Map.lookup tpName <$> userTypes <$> RWS.get
    getET tpName = do
      etd <- RWS.gets extendedTypeData
      return $ fromMaybe TypeBasic (Map.lookup tpName etd)

allDefs :: ASLSpec -> [AS.Definition]
allDefs ASLSpec{..} = aslDefinitions ++ aslSupportDefinitions ++ aslExtraDefinitions

-- | Given the top-level list of definitions, build a 'SigEnv' for preprocessing the
-- signatures.
buildEnv :: ASLSpec -> SigEnv
buildEnv (spec@ASLSpec{..}) =
  let defs = allDefs spec
      getCallables merge ds = buildCallableMap merge
        ((\c -> (mkCallableName c, c)) <$> (catMaybes (asCallable <$> ds)))

      baseCallables = getCallables uniqueCallables aslDefinitions
      extraCallables = getCallables preferLongerCallable (aslSupportDefinitions ++ aslExtraDefinitions)
      envCallables = Map.union extraCallables baseCallables -- extras override base definitions

      -- globalVars = Map.fromList $
      --   ((\v -> (getVariableName v, v)) <$> (catMaybes (asDefVariable <$> defs)))
        -- ((\v -> (getVariableName v, v)) <$> concatMap getEnumVariables defs)
      types = Map.fromList ((\t -> (getTypeName t, t)) <$> (catMaybes (asDefType <$> defs)))
      -- | TODO: Populate enums
      enums = Map.fromList (concatMap getEnumValues defs)
      consts = Map.fromList (builtinConsts ++ catMaybes (getConst <$> defs))
      -- | TODO: Populate builtin types
      builtinTypes = Map.empty
      -- getVariableName v = let DefVariable name _ = v
      --                     in name

      -- Map each enum type to a name->integer map.
      getEnumValues d = case d of
        AS.DefTypeEnum _ names -> zip names [0..]
        _ -> []
      getConst d = case d of
        AS.DefConst name asType e -> case (asType, e) of
          (AS.TypeRef (AS.QualifiedIdentifier _ "integer"), (AS.ExprLitInt i)) ->
            Just (name, Some $ ConstVal WT.BaseIntegerRepr i)
          (AS.TypeFun "bits" (AS.ExprLitInt n), AS.ExprLitBin bv) -> case NR.someNat n of
            Just (Some wRepr) -> case NR.testLeq (NR.knownNat @1) wRepr of
              Just NR.LeqProof ->
                Just (name, Some $ ConstVal (WT.BaseBVRepr wRepr) (BVS.bitVector' wRepr (bitsToInteger bv)))
              Nothing -> error $ "bv width 0"
            Nothing -> error $ "negative natural " ++ show n
          _ -> Nothing
        _ -> Nothing
      getTypeName t = case t of
        DefTypeBuiltin name -> name
        DefTypeAbstract name -> name
        DefTypeAlias name _ -> name
        DefTypeStruct (AS.QualifiedIdentifier _ name) _ -> name
        DefTypeEnum name _ -> name
  in SigEnv {..}

buildInitState :: ASLSpec -> SigState
buildInitState ASLSpec{..} =
  let globalVars = Map.fromList (builtinGlobals ++ map getRegisterDefType aslRegisterDefinitions)
      extendedTypeData = Map.fromList (map getRegisterDefSig aslRegisterDefinitions)
      userTypes = Map.empty
      callableGlobalsMap  = Map.empty
      callableSignatureMap = Map.empty
      callableOpenSearches = []
  in SigState{..}

insertUnique :: Ord k => Show k => k -> a -> Map.Map k a -> Map.Map k a
insertUnique k v =
  Map.alter f k
  where
    f x = case x of
      Just _ -> error $ "Unexpected existing member in map:" ++ show k
      Nothing -> Just v

initializeSigM :: ASLSpec -> SigM ext f ()
initializeSigM spec = do
  mapM_ initDefGlobal (allDefs spec)
  where
    initDefGlobal (AS.DefVariable (AS.QualifiedIdentifier _ nm) ty) = do
      tp <- computeType ty
      ext <- mkExtendedTypeData ty
      st <- RWS.get
      RWS.put $ st { globalVars = insertUnique nm tp (globalVars st),
                     extendedTypeData = insertUnique nm ext (extendedTypeData st) }
    initDefGlobal (AS.DefArray nm ty idxty) = do
      Some tp <- computeType ty
      ext <- mkExtendedTypeData ty
      st <- RWS.get
      let atp = Some $ WT.BaseArrayRepr (Ctx.empty Ctx.:> WT.BaseIntegerRepr) tp
      let aext = TypeArray ext
      RWS.put $ st { globalVars = insertUnique nm atp (globalVars st),
                     extendedTypeData = insertUnique nm aext (extendedTypeData st) }
    initDefGlobal _ = return ()

buildSigState :: ASLSpec -> (SigEnv, SigState)
buildSigState spec =
  let env = (buildEnv spec) in
  case runSigM env (buildInitState spec) (initializeSigM spec) of
    (Left err, _) -> error $ "Unexpected exception when initializing SigState: " ++ show err
    (Right _, state) -> (env, state)

runSigM :: SigEnv -> SigState -> SigM ext f a -> (Either SigException a, SigState)
runSigM env state action =
  let rws = E.runExceptT $ getSigM action
      (e, s, _) = RWS.runRWS rws env state
  in case e of
    Left err -> (Left err, s)
    Right a -> (Right a, s)


data SigEnv = SigEnv { envCallables :: Map.Map T.Text Callable
                           -- , globalVars :: Map.Map T.Text DefVariable
                           , enums :: Map.Map T.Text Integer
                           , consts :: Map.Map T.Text (Some ConstVal)
                           , types :: Map.Map T.Text DefType
                           , builtinTypes :: Map.Map T.Text (Some UserType)
                           }

-- deriving instance Show (SigEnv ext f)

data SigState = SigState { userTypes :: Map.Map T.Text (Some UserType)
                           -- ^ user-defined types
                         , callableGlobalsMap :: Map.Map T.Text [(T.Text, Some WT.BaseTypeRepr)]
                           -- ^ map from function/procedure name to list of globals
                         , callableSignatureMap :: Map.Map T.Text (SomeSimpleSignature, Callable)
                           -- ^ map of all signatures found thus far
                         , callableOpenSearches :: [T.Text]
                           -- ^ all callables encountered on the current search path
                         , globalVars :: Map.Map T.Text (Some WT.BaseTypeRepr)
                         , extendedTypeData :: Map.Map T.Text ExtendedTypeData
                         }

throwError :: InnerSigException -> SigM ext f a
throwError e = do
  stack <- RWS.gets callableOpenSearches
  E.throwError (SigException stack e)

data InnerSigException = TypeNotFound T.Text
                       | BuiltinTypeNotFound T.Text
                       | CallableNotFound T.Text
                       | VariableNotFound T.Text
                       | WrongType T.Text T.Text
                       | StructMissingField T.Text T.Text
                       | UnsupportedSigExpr AS.Expr
  deriving (Eq, Show)

data SigException = SigException
  { exCallStack :: [T.Text]
  , exInner :: InnerSigException }
  deriving (Eq, Show)

storeType :: T.Text -> UserType tp -> SigM ext f ()
storeType tpName tp = do
  st <- RWS.get
  RWS.put $ st { userTypes = Map.insert tpName (Some tp) (userTypes st) }

pushCallableSearch :: AS.QualifiedIdentifier -> Int -> SigM ext f Bool
pushCallableSearch name' arity = do
  st <- RWS.get
  let name = mkFunctionName name' arity
  if elem name (callableOpenSearches st) then
    return True
  else do
    RWS.put $ st { callableOpenSearches = name : (callableOpenSearches st) }
    return False

popCallableSearch :: AS.QualifiedIdentifier -> Int -> SigM ext f ()
popCallableSearch name' arity = do
  st <- RWS.get
  let name = mkFunctionName name' arity
  case callableOpenSearches st of
    x : xs | True <- x == name ->
      RWS.put $ st { callableOpenSearches = xs }
    _ -> error $ "Mismatched callable pops and pushes:" <> show name


lookupCallable :: AS.QualifiedIdentifier -> Int -> SigM ext f (Maybe Callable)
lookupCallable name' arity = do
  env <- RWS.ask
  let name = mkFunctionName name' arity
  return $ Map.lookup name (envCallables env)

lookupCallable' :: T.Text -> SigM ext f (Maybe Callable)
lookupCallable' name = do
  env <- RWS.ask
  return $ Map.lookup name (envCallables env)

lookupBuiltinType :: T.Text -> SigM ext f (Some UserType)
lookupBuiltinType tpName = do
  env <- RWS.ask
  case Map.lookup tpName (builtinTypes env) of
    Just tp -> return tp
    Nothing -> throwError $ BuiltinTypeNotFound tpName

lookupDefType :: T.Text -> SigM ext f DefType
lookupDefType tpName = do
  env <- RWS.ask
  case Map.lookup tpName (types env) of
    Just defType -> return defType
    Nothing -> throwError $ TypeNotFound tpName

-- | If the variable is present, return its definition. Otherwise, return 'Nothing'.
--lookupGlobalVar :: T.Text -> SigM ext f (Maybe DefVariable)
lookupGlobalVar :: T.Text -> SigM ext f (Maybe (Some WT.BaseTypeRepr))
lookupGlobalVar varName = do
  env <- RWS.get
  return $ Map.lookup varName (globalVars env)

lookupCallableGlobals :: Callable -> SigM ext f (Maybe [(T.Text, Some WT.BaseTypeRepr)])
lookupCallableGlobals c = do
  globalsMap <- callableGlobalsMap <$> RWS.get
  let name = mkCallableName c
  return $ Map.lookup name globalsMap

storeCallableGlobals :: Callable -> [(T.Text, Some WT.BaseTypeRepr)] -> SigM ext f ()
storeCallableGlobals c globals = do
  st <- RWS.get
  let name = mkCallableName c
  RWS.put $ st { callableGlobalsMap = Map.insert name globals (callableGlobalsMap st) }

lookupCallableSignature :: Callable -> SigM ext f (Maybe SomeSimpleSignature)
lookupCallableSignature c = do
  signatureMap <- callableSignatureMap <$> RWS.get
  let name = mkCallableName c
  return $ (fst <$> Map.lookup name signatureMap)

storeCallableSignature :: Callable -> SomeSimpleSignature -> SigM ext f()
storeCallableSignature c sig = do
  st <- RWS.get
  let name = mkCallableName c
  RWS.put $ st { callableSignatureMap = Map.insert name (sig, c) (callableSignatureMap st) }


-- | If the given type is user-defined, compute its signature and store it
storeUserType :: AS.Type -> SigM ext f ()
storeUserType tp = case applyTypeSynonyms tp of
  AS.TypeRef (AS.QualifiedIdentifier _ tpName) -> do
   case tpName of
     "integer" -> return ()
     "boolean" -> return ()
     "bit" -> return ()
     _ -> do
       _ <- computeUserType tpName
       return ()
  _ -> return ()

-- | Compute the What4 representation of a user-defined ASL type, from the name of
-- the type as a 'T.Text'. Store it in 'typeSigs' (if it isn't already there).
computeUserType :: T.Text -> SigM ext f (Some UserType)
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
            case computeType' varType of
              Left (Some tp) -> do
                return $ Some $ LabeledValue (varName, Nothing) tp
              Right nm -> do
                Some ut <- computeUserType nm
                return $ Some $ LabeledValue (varName, Just (Some ut)) (userTypeRepr ut)
          Some varTpAssignment <- return $ Ctx.fromList varTps
          return $ Some $ UserStruct varTpAssignment
        DefTypeAbstract _ -> error $ "computeUserType: abstract type " ++ show tpName
        _ -> error $ "computeUserType: unsupported type " ++ T.unpack tpName
      storeType tpName tp
      return $ Some tp


-- | Either compute the What4 representation of an ASL 'AS.Type' or
-- return a name representing a user-defined type.
computeType' :: AS.Type -> Either (Some WT.BaseTypeRepr) T.Text
computeType' tp = case applyTypeSynonyms tp of
  AS.TypeRef (AS.QualifiedIdentifier _ tpName) -> do
    case tpName of
      "integer" -> Left (Some WT.BaseIntegerRepr)
      "boolean" -> Left (Some WT.BaseBoolRepr)
      "bit" -> Left (Some (WT.BaseBVRepr (NR.knownNat @1)))
      _ -> Right tpName
  AS.TypeFun "bits" e ->
    case e of
      AS.ExprLitInt w
        | Just (Some wRepr) <- NR.someNat w
        , Just NR.LeqProof <- NR.isPosNat wRepr -> Left $ Some (WT.BaseBVRepr wRepr)
      e' -> error $ "computeType, TypeFun" <> show e'
  AS.TypeOf _ -> error "computeType, TypeOf"
  AS.TypeReg _ _ -> error "computeType, TypeReg"
  AS.TypeArray _ _ -> error "computeType, TypeArray"
  _ -> error $ "computeType" ++ show tp

-- | Compute the What4 representation of an ASL 'AS.Type'.
computeType :: AS.Type -> SigM ext f (Some WT.BaseTypeRepr)
computeType tp = case computeType' tp of
  Left tp -> return tp
  Right tpName -> do
    Some userType <- computeUserType tpName
    return $ Some $ userTypeRepr userType

-- | If the identifier is a global variable, return its type. Otherwise, return
-- 'Nothing', indicating the variable is not global.
computeGlobalVarType :: T.Text -> SigM ext f (Maybe (Some WT.BaseTypeRepr))
computeGlobalVarType varName = do
  lookupGlobalVar varName

-- | Given a variable name, determine whether it is a global variable or not. If so,
-- return a pair containing the variable and its type; if not, return 'Nothing'.
varGlobal :: T.Text -> SigM ext f (Maybe (T.Text, Some WT.BaseTypeRepr))
varGlobal varName = do
  mVarType <- computeGlobalVarType varName
  case mVarType of
    Nothing -> return Nothing
    Just varType -> return $ Just (varName, varType)

theVarGlobal :: T.Text -> SigM ext f (T.Text, Some WT.BaseTypeRepr)
theVarGlobal varName = do
  mg <- varGlobal varName
  case mg of
    Just g -> return g
    Nothing -> error $ "Unknown global variable: " <> show varName

sliceGlobalVars :: AS.Slice -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
sliceGlobalVars slice = case slice of
  AS.SliceSingle e -> exprGlobalVars e
  AS.SliceOffset e1 e2 -> (++) <$> exprGlobalVars e1 <*> exprGlobalVars e2
  AS.SliceRange e1 e2 -> (++) <$> exprGlobalVars e1 <*> exprGlobalVars e2

setEltGlobalVars :: AS.SetElement -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
setEltGlobalVars setElt = case setElt of
  AS.SetEltSingle e -> exprGlobalVars e
  AS.SetEltRange e1 e2 -> (++) <$> exprGlobalVars e1 <*> exprGlobalVars e2

lValExprGlobalVars :: AS.LValExpr -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
lValExprGlobalVars lValExpr = case lValExpr of
  -- If the variable isn't in the list of globals, we assume it is locally bound and
  -- simply return the empty list.
  AS.LValVarRef (AS.QualifiedIdentifier _ varName) -> maybeToList <$> varGlobal varName
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
  AS.LValMember lv _ -> lValExprGlobalVars lv
  AS.LValMemberBits lv _ -> lValExprGlobalVars lv
  AS.LValSlice les ->
    concat <$> traverse lValExprGlobalVars les
  _ -> return []

casePatternGlobalVars :: AS.CasePattern -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
casePatternGlobalVars pat = case pat of
  AS.CasePatternIdentifier varName -> maybeToList <$> varGlobal varName
  AS.CasePatternTuple pats -> concat <$> traverse casePatternGlobalVars pats
  _ -> return []

caseAlternativeGlobalVars :: AS.CaseAlternative -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
caseAlternativeGlobalVars alt = case alt of
  AS.CaseWhen pats mExpr stmts -> do
    patGlobals <- concat <$> traverse casePatternGlobalVars pats
    eGlobals <- fromMaybe [] <$> traverse exprGlobalVars mExpr
    stmtGlobals <- concat <$> traverse stmtGlobalVars stmts
    return $ patGlobals ++ eGlobals ++ stmtGlobals
  AS.CaseOtherwise stmts -> concat <$> traverse stmtGlobalVars stmts

-- | Collect all global variables from a single 'AS.Expr'.
exprGlobalVars :: AS.Expr -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
exprGlobalVars expr = case overrideExpr overrides expr of
  Just exprs -> concat <$> traverse exprGlobalVars exprs
  Nothing ->
    case expr of
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
      AS.ExprCall qName argEs -> do
        argGlobals <- concat <$> traverse exprGlobalVars argEs
        mCallable <- lookupCallable qName (length argEs)
        case mCallable of
          Just callable -> do
            -- Compute the signature of the callable
            recursed <- pushCallableSearch qName (length argEs)
            if recursed then
              return argGlobals
            else do
              void $ computeCallableSignature callable
              callableGlobals <- callableGlobalVars callable
              popCallableSearch qName (length argEs)
              return $ callableGlobals ++ argGlobals
          Nothing -> return argGlobals
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
      AS.ExprMember e _ -> exprGlobalVars e
      AS.ExprMemberBits e _ -> exprGlobalVars e
      _ -> return []

-- | Collect all global variables from a single 'AS.Stmt'.
stmtGlobalVars :: AS.Stmt -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
stmtGlobalVars stmt =
  case overrideStmt overrides stmt of
    Just stmts -> concat <$> traverse stmtGlobalVars stmts
    Nothing ->
      case stmt of
          AS.StmtVarsDecl ty i -> do
            storeUserType ty
            return []
          AS.StmtVarDeclInit (_, ty) e -> do
            storeUserType ty
            exprGlobalVars e
          AS.StmtAssign le e -> (++) <$> lValExprGlobalVars le <*> exprGlobalVars e
          AS.StmtCall qName argEs -> do
            argGlobals <- concat <$> traverse exprGlobalVars argEs
            mCallable <- lookupCallable qName (length argEs)
            case mCallable of
              Just callable -> do
                -- Compute the signature of the callable
                recursed <- pushCallableSearch qName (length argEs)
                if recursed then
                  return argGlobals
                else do
                  void $ computeCallableSignature callable
                  callableGlobals <- callableGlobalVars callable
                  popCallableSearch qName (length argEs)
                  return $ callableGlobals ++ argGlobals
              Nothing -> return argGlobals
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
          AS.StmtUnpredictable -> return []
          AS.StmtUndefined -> return []
          _ -> return []

-- | Compute the list of global variables in a 'Callable' and store it in the
-- state. If it has already been computed, simply return it.
callableGlobalVars :: Callable -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
callableGlobalVars c@Callable{..} = do
  mGlobals <- lookupCallableGlobals c
  case mGlobals of
    Just globals -> return globals
    Nothing -> do
      globals <- concat <$> traverse stmtGlobalVars callableStmts
      storeCallableGlobals c globals
      return globals

-- | Compute the signature of a callable (function/procedure). Currently, we assume
-- that if the return list is empty, it is a procedure, and if it is nonempty, then
-- it is a function.
computeCallableSignature :: Callable -> SigM ext f (SomeSimpleSignature)
computeCallableSignature c@Callable{..} = do
  let name = mkCallableName c
  mSig <- lookupCallableSignature c

  case mSig of
    Just sig -> return sig
    Nothing -> do
      mapM_ (\(_,t) -> storeUserType t) callableArgs
      mapM_ storeUserType callableRets
      
      globalVars' <- callableGlobalVars c
      let globalVars = globalVars' ++ builtinGlobals
      labeledVals <- forM (nub globalVars) $ \(varName, Some varTp) -> do
        return $ Some (LabeledValue varName varTp)

      Some globalReprs <- return $ Ctx.fromList labeledVals
      sig <- case callableRets of
        [] -> -- procedure
          return $ SomeSimpleProcedureSignature $ SimpleProcedureSignature
            { sprocName = name
            , sprocArgs = callableArgs
            , sprocGlobalReprs = globalReprs
            }
        _ -> do -- function
          return $ SomeSimpleFunctionSignature $ SimpleFunctionSignature
            { sfuncName = name
            , sfuncRet = callableRets
            , sfuncArgs = callableArgs
            , sfuncGlobalReprs = globalReprs
            }
      storeCallableSignature c sig
      return sig

computeType'' :: Definitions arch -> AS.Type -> Some WT.BaseTypeRepr
computeType'' defs t = case computeType' t of
  Left tp -> tp
  Right tpName -> case Map.lookup tpName (defTypes defs) of
    Just (Some ut) -> Some $ userTypeRepr ut
    Nothing -> error $ "Missing user type definition for: " <> (show tpName)

mkBaseStructRepr :: [Some WT.BaseTypeRepr] -> Some WT.BaseTypeRepr
mkBaseStructRepr ts = case ts of
  [t] -> t
  ts | Some assignment <- Ctx.fromList ts -> Some (WT.BaseStructRepr assignment)

applyStaticEnv :: StaticEnvMap
               -> AS.Type
               -> AS.Type
applyStaticEnv env t =
  fromMaybe (X.throw $ CannotStaticallyEvaluateType t (staticEnvMapVals env)) $
   applyStaticEnv' env t


mkSignature :: Definitions arch -> StaticValues -> SomeSimpleSignature -> Some (SomeSignature)
mkSignature defs env sig =
  case sig of
    SomeSimpleFunctionSignature fsig |
        Some retT <- mkBaseStructRepr $ map mkType (sfuncRet fsig)
      , Some args <- Ctx.fromList $ map mkLabel (sfuncArgs fsig) -> 
       
      Some $ SomeFunctionSignature $ FunctionSignature
        { funcName = mkFinalFunctionName env $ sfuncName fsig
        , funcSigRepr = retT
        , funcArgReprs = args
        , funcGlobalReprs = sfuncGlobalReprs fsig
        , funcStaticVals = staticEnvMapVals $ addStandardStaticEnv $ simpleStaticEnvMap env
        , funcArgs = sfuncArgs fsig
        }
    SomeSimpleProcedureSignature fsig |
      Some args <-  Ctx.fromList $ map mkLabel (sprocArgs fsig) ->

      Some $ SomeProcedureSignature $ ProcedureSignature
        { procName = mkFinalFunctionName env $ sprocName fsig
        , procArgReprs = args
        , procGlobalReprs = sprocGlobalReprs fsig
        , procStaticVals = staticEnvMapVals $ addStandardStaticEnv $ simpleStaticEnvMap env
        , procArgs = sprocArgs fsig
        }
  where
    mkType t = computeType'' defs (applyStaticEnv (simpleStaticEnvMap env) t)
    mkLabel (nm, t) =
      if | Some tp <- mkType t ->
           Some (LabeledValue nm (CT.baseToType tp))
        

mkInstructionName :: T.Text -- ^ name of instruction
                  -> T.Text -- ^ name of encoding
                  -> T.Text
mkInstructionName instName encName = instName <> "_" <> encName

computeFieldType :: AS.InstructionField -> SigM ext f (Some WT.BaseTypeRepr)
computeFieldType AS.InstructionField{..} = do
  case WT.someNat instFieldOffset of
    Nothing -> error $ "Bad field width: " ++ show instFieldName ++ ", " ++ show instFieldOffset
    Just (Some repr) -> case (WT.knownNat @1) `WT.testLeq` repr of
      Nothing -> error $ "Bad field width: " ++ show instFieldName ++ ", " ++ show instFieldOffset
      Just WT.LeqProof -> return $ Some (WT.BaseBVRepr repr)

computeInstructionSignature' :: AS.Instruction
                             -> T.Text -- ^ name of encoding
                             -> AS.InstructionSet
                             -> SigM ext f (Some SomeSignature, [AS.Stmt])
computeInstructionSignature' AS.Instruction{..} encName iset = do
  let name = mkInstructionName instName encName

  let mEnc = find (\e -> AS.encName e == encName && AS.encInstrSet e == iset) instEncodings
  case mEnc of
    Nothing -> error $ "Invalid encoding " ++ show encName ++ " for instruction " ++ show instName
    Just enc -> do
      let initUnusedFields = initializeUnusedFields (AS.encFields enc) (map AS.encFields instEncodings)
      let initStmts = AS.encDecode enc ++ initUnusedFields ++ initializeGlobals enc
      let possibleEnvs = getPossibleEnvs (AS.encFields enc) (AS.encDecode enc)
      let instExecute' = liftOverEnvs instName possibleEnvs instExecute
      let instStmts = initStmts ++ instPostDecode ++ instExecute'
      let instGlobalVars = concat <$> traverse stmtGlobalVars instStmts
      let staticEnv = addInitializedVariables initStmts (addStandardStaticEnv emptyStaticEnvMap)
      globalVars' <- instGlobalVars
      let globalVars = globalVars' ++ builtinGlobals
      labeledVals <- forM (nub globalVars) $ \(varName, Some varTp) -> do
        return $ Some (LabeledValue varName varTp)
      labeledArgs <- forM (AS.encFields enc) $ \field -> do
        Some tp <- computeFieldType field
        let ctp = CT.baseToType tp
        return (Some (LabeledValue (AS.instFieldName field) ctp))
      Some globalReprs <- return $ Ctx.fromList labeledVals
      Some argReprs <- return $ Ctx.fromList labeledArgs
      let pSig = ProcedureSignature { procName = name
                                    , procArgReprs = argReprs
                                    , procGlobalReprs = globalReprs
                                    , procStaticVals = staticEnvMapVals staticEnv
                                    , procArgs = []
                                    }
      return (Some (SomeProcedureSignature pSig), instStmts)

-- The virtual "StaticEnvironment" function is interpreted by the translator as
-- a hint to inject the case statement into the current static environment in
-- order to resolve the type dependency.

liftOverEnvs :: T.Text -> [StaticEnvP] -> [AS.Stmt] -> [AS.Stmt]
liftOverEnvs instName envs stmts = case Map.lookup instName dependentVariableHints of
  Just vars ->
    let
      varToStatic asns var = case Map.lookup var asns of
        Just sv -> sv
        _ -> error $ "Missing variable in possible assignments: " ++ show var
      cases = do
        asns <- getPossibleValuesFor vars envs
        return $ (map (varToStatic asns) vars, stmts)

    in [staticEnvironmentStmt vars cases]
  _ -> stmts

getPossibleEnvs :: [AS.InstructionField] -> [AS.Stmt] -> [StaticEnvP]
getPossibleEnvs fields decodes =
  let
    addField (AS.InstructionField instFieldName _ instFieldOffset) =
      insertStaticEnvP instFieldName (EnvPInfeasable (StaticBVType instFieldOffset) Set.empty)
    initEnv = foldr addField emptyStaticEnvP fields
  in
    getPossibleStaticsStmts initEnv decodes

-- | In general execution bodies may refer to fields that have not been set by
-- this particular encoding. To account for this, we initialize all fields from
-- other encodings to undefined values, under the assumption that they should not be read.
-- Ambiguous fields (with multiple sizes) are not initialized.
initializeUnusedFields :: [AS.InstructionField] -> [[AS.InstructionField]] -> [AS.Stmt]
initializeUnusedFields encFields allFields =
  let
    getFieldEntry (AS.InstructionField instFieldName _ instFieldOffset) =
      (instFieldName, [instFieldOffset])
    encFieldsSet = Set.fromList (map (\(AS.InstructionField nm _ _) -> nm) encFields)
    otherFieldsMap = Map.withoutKeys
      (Map.fromListWith (++) (map getFieldEntry $ concat allFields))
      encFieldsSet

    getDecl instFieldName [instFieldOffset] =
        Just $ AS.StmtVarsDecl (AS.TypeFun "bits" (AS.ExprLitInt instFieldOffset)) [instFieldName]
    getDecl _ _ = Nothing
  in
    Map.elems $ Map.mapMaybeWithKey getDecl otherFieldsMap

initializeGlobals :: AS.InstructionEncoding -> [AS.Stmt]
initializeGlobals AS.InstructionEncoding{..} =
  [AS.StmtVarDeclInit
     ("CurrentInstrSet", AS.TypeRef (AS.QualifiedIdentifier AS.ArchQualAny "integer"))
      (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny (instrSetToVar encInstrSet)))]

instrSetToInt :: AS.InstructionSet -> Integer
instrSetToInt is = case is of
  AS.A32 -> 0
  AS.T32 -> 1
  AS.T16 -> 2
  AS.A64 -> 3

allInstrSets :: [AS.InstructionSet]
allInstrSets = [AS.A32, AS.T32, AS.T16, AS.A64]

instrSetToVar :: AS.InstructionSet -> T.Text
instrSetToVar is = case is of
  AS.A32 -> "InstrSet_A32"
  AS.T32 -> "InstrSet_T32"
  AS.T16 -> "InstrSet_T16"
  AS.A64 -> "InstrSet_A64"

addStandardStaticEnv :: StaticEnvMap -> StaticEnvMap
addStandardStaticEnv env =
   foldl addInstructionSet env allInstrSets
 where
   addInstructionSet env' is = insertStaticEnv (instrSetToVar is) (StaticInt $ instrSetToInt is) env'

-- | Evaluate any known static values for this specific encoding
addInitializedVariables :: [AS.Stmt] -> StaticEnvMap -> StaticEnvMap
addInitializedVariables stmts env =
  foldl addStaticAssignment env stmts
  where
    addStaticAssignment env' stmt = case stmt of
      AS.StmtAssign (AS.LValVarRef (AS.QualifiedIdentifier _ ident)) e
        | Just v <- exprToStatic env' e ->
          insertStaticEnv ident v env'
      AS.StmtVarDeclInit (nm, _) e
        | Just v <- exprToStatic env' e ->
          insertStaticEnv nm v env'
      _ -> env'

-- | Create the full list of statements in an instruction given the main execute
-- block and the encoding-specific operations.
createInstStmts :: [AS.Stmt]
                -- ^ Encoding-specific operations
                -> [AS.Stmt]
                -- ^ Execute block
                -> [AS.Stmt]
createInstStmts encodingSpecificOperations stmts =
  encodingSpecificOperations ++ stmts

-- Overrides only for the purposes of collecting global variables
data Overrides arch =
  Overrides { overrideExpr :: AS.Expr -> Maybe [AS.Expr]
            , overrideStmt :: AS.Stmt -> Maybe [AS.Stmt]
            }
overrides :: forall arch . Overrides arch
overrides = Overrides {..}
  where overrideStmt ::  AS.Stmt -> Maybe [AS.Stmt]
        overrideStmt stmt = case stmt of
          _ -> Nothing

        overrideExpr :: AS.Expr -> Maybe [AS.Expr]
        overrideExpr e = case e of
          AS.ExprCall (AS.QualifiedIdentifier q f) args
           | f `elem` ["Min", "Max"] ->
             Just $ [AS.ExprCall (AS.QualifiedIdentifier q (f <> "integerinteger")) args]
          AS.ExprCall (AS.QualifiedIdentifier q "Align") args ->
            Just $ [ AS.ExprCall (AS.QualifiedIdentifier q "Alignintegerinteger") args
                   , AS.ExprCall (AS.QualifiedIdentifier q "AlignbitsNinteger") args
                   ]
          _ -> mkFaultOv e "IsExternalAbort" <|>
               mkFaultOv e "IsAsyncAbort" <|>
               mkFaultOv e "IsSErrorInterrupt" <|>
               mkFaultOv e "IsExternalSyncAbort"

        mkFaultOv e nm = case e of
          AS.ExprCall (AS.QualifiedIdentifier q nm') args | nm == nm' ->
            Just $ [ AS.ExprCall (AS.QualifiedIdentifier q (nm <> "FaultRecord")) args
                   , AS.ExprCall (AS.QualifiedIdentifier q (nm <> "Fault")) args
                   ]
          _ -> Nothing

mkFinalFunctionName :: StaticValues -> T.Text ->  T.Text
mkFinalFunctionName dargs nm = T.concat $ [nm] ++ map (\(nm,i) -> nm <> "_" <> T.pack (show i)) (Map.assocs dargs)

-- Extra typing hints for local variables, where
-- type inference would otherwise require lookahead
localTypeHints :: Map.Map (T.Text, T.Text) TypeConstraint
localTypeHints = Map.fromList
  [(("AArch32_TranslationTableWalkLD_7", "baseaddress"), ConstraintSingle (CT.BVRepr $ WT.knownNat @40))
  ,(("AArch32_TranslationTableWalkLD_7", "outputaddress"),  ConstraintSingle (CT.BVRepr $ WT.knownNat @40))
  ,(("AArch64_TranslationTableWalk_8", "baseaddress"), ConstraintSingle (CT.BVRepr $ WT.knownNat @52))
  ,(("AArch64_TranslationTableWalk_8", "outputaddress"),  ConstraintSingle (CT.BVRepr $ WT.knownNat @52))
  ]

-- Hints for instructions for variables that need to be concretized
-- to successfully translate
dependentVariableHints :: Map.Map T.Text [T.Text]
dependentVariableHints = Map.fromList
  [("aarch32_VQSUB_A", ["esize"])
  ,("aarch32_VMOVL_A", ["esize"])
  ,("aarch32_VQDMULH_A", ["esize", "elements"])
  ,("aarch32_VABA_A", ["esize", "elements"])
  ,("aarch32_VMAX_i_A", ["esize", "elements"])
  ,("aarch32_VRSHR_A", ["esize", "elements"])
  ,("aarch32_VST4_m_A", ["ebytes", "elements"])
  ,("aarch32_VPMAX_i_A", ["esize", "elements"])
  ,("aarch32_USAT16_A", ["saturate_to"])
  ,("aarch32_SSAT16_A", ["saturate_to"])
  ,("aarch32_SSAT_A", ["saturate_to"])
  ,("aarch32_VLD1_m_A", ["ebytes", "elements"])
  ,("aarch32_VLD2_m_A", ["ebytes", "elements"])
  ,("aarch32_VLD3_m_A", ["ebytes", "elements"])
  ,("aarch32_VREV16_A", ["esize", "elements"])
  ,("aarch32_VABD_i_A", ["esize", "elements"])
  ,("aarch32_VADDL_A", ["esize", "elements"])
  ,("aarch32_VMLA_i_A", ["esize", "elements"])
  ,("aarch32_VQRDMLAH_A", ["esize", "elements"])
  ,("aarch32_VMOVN_A", ["esize", "elements"])
  ,("aarch32_VQDMLAL_A", ["esize", "elements"])
  ,("aarch32_VDUP_r_A", ["esize", "elements"])
  ,("aarch32_CRC32_A", ["size"])
  ,("aarch32_VQSHL_i_A", ["esize", "elements"])
  ,("aarch32_VSUB_i_A", ["esize", "elements"])
  ,("aarch32_VSUBHN_A", ["esize", "elements"])
  ,("aarch32_VQRDMULH_A", ["esize", "elements"])
  ,("aarch32_VQADD_A", ["esize", "elements"])
  ,("aarch32_VDUP_s_A", ["esize", "elements"])
  ,("aarch32_VQDMULL_A", ["esize", "elements"])
  ,("aarch32_VQSHRN_A", ["esize", "elements"])
  ,("aarch32_VLD4_m_A", ["ebytes", "elements"])
  ,("aarch32_VSUBL_A", ["esize", "elements"])
  ,("aarch32_VST2_m_A", ["ebytes", "elements"])
  ,("aarch32_VSRA_A", ["esize", "elements"])
  ,("aarch32_VABS_A2_A", ["esize", "elements"])
  ,("aarch32_VABS_A", ["esize", "elements"])
  ,("aarch32_VSHL_i_A", ["esize", "elements"])
  ,("aarch32_VCLZ_A", ["esize", "elements"])
  ,("aarch32_VMOV_sr_A", ["esize", "elements"])
  ,("aarch32_USAT_A", ["esize", "elements"])
  ,("aarch32_VCLS_A", ["esize", "elements"])
  ,("aarch32_VHADD_A", ["esize", "elements"])
  ,("aarch32_VLD1_a_A", ["ebytes", "elements"])
  ,("aarch32_VLD2_a_A", ["ebytes", "elements"])
  ,("aarch32_VLD3_a_A", ["ebytes", "elements"])
  ,("aarch32_VLD4_a_A", ["esize", "elements"])
  ,("aarch32_VMUL_i_A", ["esize", "elements"])
  ,("aarch32_VNEG_A", ["esize", "elements"])
  ,("aarch32_VPADAL_A", ["esize", "elements"])
  ,("aarch32_VQMOVN_A", ["esize", "elements"])
  ,("aarch32_VQNEG_A", ["esize", "elements"])
  ,("aarch32_VQSHL_r_A", ["esize", "elements"])
  ,("aarch32_VRADDHN_A", ["esize", "elements"])
  ,("aarch32_VRSHL_A", ["esize", "elements"])
  ,("aarch32_VRSRA_A", ["esize", "elements"])
  ,("aarch32_VRSUBHN_A", ["esize", "elements"])
  ,("aarch32_VSHLL_A", ["esize", "elements"])
  ,("aarch32_VSHL_r_A", ["esize", "elements"])
  ,("aarch32_VSHRN_A", ["esize", "elements"])
  ,("aarch32_VSHR_A", ["esize", "elements"])
  ,("aarch32_VSLI_A", ["esize", "elements"])
  ,("aarch32_VSRI_A", ["esize", "elements"])
  ,("aarch32_VTRN_A", ["esize", "elements"])
  ,("aarch32_VUZP_A", ["esize", "elements"])
  ,("aarch32_VQRSHL_A", ["esize", "elements"])
  ,("aarch32_VPADDL_A", ["esize", "elements"])
  ,("aarch32_VZIP_A", ["esize"])
  ,("aarch32_VST3_m_A", ["ebytes", "elements"])
  ,("aarch32_VADD_i_A", ["esize", "elements"])
  ,("aarch32_VQRSHRN_A", ["esize", "elements"])
  ,("aarch32_VADDHN_A", ["esize", "elements"])
  ,("aarch32_VRHADD_A", ["esize", "elements"])
  ,("aarch32_VTST_A", ["esize", "elements"])
  ,("aarch32_VRSHRN_A", ["esize", "elements"])
  ,("aarch32_VPADD_i_A", ["esize", "elmeents"])
  ,("aarch32_VQRDMLSH_A", ["esize", "elements"])
  ,("aarch32_VQABS_A", ["esize", "elements"])
  ,("aarch32_VST1_m_A", ["ebytes", "elements"])
  ]
