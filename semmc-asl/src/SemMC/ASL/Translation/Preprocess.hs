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
  , applyTypeEnvir
  , exprToInt
  , exprToBool
  , mkSignature
  , registerTypeSynonyms
  , mkExtendedTypeData'
  ) where

import Debug.Trace (traceM)

import qualified Control.Exception as X
import           Control.Monad (void)
import qualified Control.Monad.Except as E
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State.Class as MS
import qualified Control.Monad.State as MSS
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


-- TODO: Type synonyms are currently a global property,
-- ideally type normalization should happen with respect to
-- some typing environment so this can be generated from the above.

-- We can't treat these as normal aliases, since we need to retain the
-- register field information.
globalTypeSynonyms :: [(T.Text, AS.Type)]
globalTypeSynonyms =
  [ ("CPACRType", bits 32)
  , ("CNTKCTLType", bits 32)
  , ("ESRType", bits 32)
  , ("FPCRType", bits 32)
  , ("MAIRType", bits 64)
  , ("SCRType", bits 32)
  , ("SCTLRType", bits 64)
  ]
  where bits n = AS.TypeFun "bits" (AS.ExprLitInt  n)

builtinConsts :: [(T.Text, Some ConstVal)]
builtinConsts =
  [ ("TRUE", Some $ ConstVal WT.BaseBoolRepr True)
  , ("FALSE", Some $ ConstVal WT.BaseBoolRepr False)
  , ("HIGH", Some $ ConstVal (WT.BaseBVRepr (WT.knownNat @1)) (BVS.bitVector (1 :: Integer)))
  ]

bitsToInteger :: [Bool] -> Integer
bitsToInteger [x] = fromIntegral (fromEnum x)
bitsToInteger (x:xs) = fromIntegral (fromEnum x) * 2 + bitsToInteger xs
bitsToInteger _ = error $ "bitsToInteger empty list"

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

buildCallableMap :: [(T.Text, Callable)] -> Map.Map T.Text Callable
buildCallableMap cs =
  let foo = Map.fromListWith (++) (map (\(nm,c) -> (nm, [c])) cs)
      (overrides, foo') = Map.mapAccumWithKey getOverrides [] foo
      foo'' = Map.mapMaybe id foo' in
  foldr (\c -> insertUnique (mkCallableName c) c) foo'' overrides

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
      getCallables ds = buildCallableMap ((\c -> (mkCallableName c, c)) <$> (catMaybes (asCallable <$> ds)))

      baseCallables = getCallables aslDefinitions
      extraCallables = getCallables (aslSupportDefinitions ++ aslExtraDefinitions)
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

applyTypeSynonyms :: AS.Type -> AS.Type
applyTypeSynonyms t = case t of
  AS.TypeRef (AS.QualifiedIdentifier _ tpName) ->
    case lookup tpName globalTypeSynonyms of
      Just syn -> syn
      Nothing -> t
  _ -> t


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
    AS.ExprCall qName argEs -> case (overrideExprCall overrides) qName of
      Just nms -> concat <$> traverse exprGlobalVars (map (\nm -> AS.ExprCall nm argEs) nms)
      Nothing -> do
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
  -- FIXME: If the stmt has an override, then we should provide a custom set of
  -- globals as well.
  --seq (unsafePerformIO $ putStrLn $ show stmt) $
  case stmt of
      AS.StmtVarsDecl ty i -> do
        storeUserType ty
        return []
      AS.StmtVarDeclInit (_, ty) e -> do
        storeUserType ty
        exprGlobalVars e
      AS.StmtAssign le e -> (++) <$> lValExprGlobalVars le <*> exprGlobalVars e
      AS.StmtCall qName argEs -> case (overrideStmtCall overrides) qName of
        Just nms -> concat <$> traverse stmtGlobalVars (map (\nm -> AS.StmtCall nm argEs) nms)
        Nothing -> do
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
      AS.StmtUnpredictable -> do
        gb <- theVarGlobal "UNPREDICTABLE"
        return [gb]
      AS.StmtUndefined -> do
        gb <- theVarGlobal "UNDEFINED"
        return [gb]
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
      
      globalVars <- callableGlobalVars c
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

mkReturnType :: [Some WT.BaseTypeRepr] -> Some WT.BaseTypeRepr
mkReturnType ts = case ts of
  [t] -> t
  ts | Some assignment <- Ctx.fromList ts -> Some (WT.BaseStructRepr assignment)


applyTypeEnvir :: TypeEnvir -> AS.Type -> AS.Type
applyTypeEnvir env t = case applyTypeSynonyms t of
  AS.TypeFun "bits" e -> case exprToInt env e of
    Just i -> AS.TypeFun "bits" (AS.ExprLitInt i)
    Nothing -> X.throw $ CannotStaticallyEvaluateType t env
  _ -> t


exprToInt :: TypeEnvir -> AS.Expr -> Maybe Integer
exprToInt env e = case e of
      AS.ExprLitInt i -> Just i
      AS.ExprVarRef (AS.QualifiedIdentifier q id) -> do
        i <- lookupTypeEnvir id env
        return i
      AS.ExprBinOp AS.BinOpSub e' e'' -> do
        i <- exprToInt env e'
        i' <- exprToInt env e''
        return $ i - i'
      AS.ExprBinOp AS.BinOpMul e' e'' -> do
        i <- exprToInt env e'
        i' <- exprToInt env e''
        return $ i * i'
      AS.ExprBinOp AS.BinOpAdd e' e'' -> do
        i <- exprToInt env e'
        i' <- exprToInt env e''
        return $ i + i'
      _ -> Nothing

exprToBool :: TypeEnvir -> AS.Expr -> Maybe Bool
exprToBool env e = case e of
  AS.ExprBinOp bop e' e'' -> do
    case (bop, exprToBool env e', exprToBool env e'') of
      (AS.BinOpLogicalAnd, Just True, Just True) -> Just True
      (AS.BinOpLogicalAnd, Just False, _) -> Just False
      (AS.BinOpLogicalAnd, _, Just False) -> Just False
      (AS.BinOpLogicalOr, Just True, Just True) -> Just True
      (AS.BinOpLogicalOr, Just False, b) -> b
      (AS.BinOpLogicalOr, b, Just False) -> b
      _ -> case (exprToInt env e', exprToInt env e'') of
             (Just i, Just i') -> bopToTest bop i i'
             _ -> Nothing
  _ -> Nothing
  where
    bopToTest bop i i' = case bop of
      AS.BinOpEQ -> Just $ i == i'
      AS.BinOpNEQ -> Just $ i /= i'
      AS.BinOpGT -> Just $ i > i'
      AS.BinOpLT -> Just $ i < i'
      AS.BinOpGTEQ -> Just $ i >= i'
      AS.BinOpLTEQ -> Just $ i <= i'
      _ -> Nothing

mkSignature :: Definitions arch -> TypeEnvir -> SomeSimpleSignature -> Some (SomeSignature)
mkSignature defs env sig =
  case sig of
    SomeSimpleFunctionSignature fsig |
        Some retT <- mkReturnType $ map mkType (sfuncRet fsig)
      , Some args <- Ctx.fromList $ map mkLabel (sfuncArgs fsig) -> 
       
      Some $ SomeFunctionSignature $ FunctionSignature
        { funcName = mkFinalFunctionName env $ sfuncName fsig
        , funcSigRepr = retT
        , funcArgReprs = args
        , funcGlobalReprs = sfuncGlobalReprs fsig
        , funcTypeEnvir = env
        , funcArgs = sfuncArgs fsig
        }
    SomeSimpleProcedureSignature fsig |
      Some args <-  Ctx.fromList $ map mkLabel (sprocArgs fsig) ->

      Some $ SomeProcedureSignature $ ProcedureSignature
        { procName = mkFinalFunctionName env $ sprocName fsig
        , procArgReprs = args
        , procGlobalReprs = sprocGlobalReprs fsig
        , procTypeEnvir = env
        , procArgs = sprocArgs fsig
        }
  where
    mkType t = computeType'' defs (applyTypeEnvir env t)
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
      let instStmts = createInstStmts (AS.encDecode enc) instExecute
      let instGlobalVars = concat <$> traverse stmtGlobalVars instStmts
      globalVars <- instGlobalVars
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
                                    , procTypeEnvir = emptyTypeEnvir
                                    , procArgs = []
                                    }
      return (Some (SomeProcedureSignature pSig), instStmts)

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
  Overrides { overrideExprCall :: AS.QualifiedIdentifier -> Maybe [AS.QualifiedIdentifier]
            , overrideStmtCall :: AS.QualifiedIdentifier -> Maybe [AS.QualifiedIdentifier]
            }
overrides :: forall arch . Overrides arch
overrides = Overrides {..}
  where overrideStmtCall ::  AS.QualifiedIdentifier -> Maybe [AS.QualifiedIdentifier]
        overrideStmtCall ident = Nothing

        mkIdent nm = (AS.QualifiedIdentifier AS.ArchQualAny nm)

        overrideExprCall :: AS.QualifiedIdentifier -> Maybe [AS.QualifiedIdentifier]
        overrideExprCall ident = case ident of
          AS.QualifiedIdentifier _ nm ->
            lookup nm [("Min",[(mkIdent "Minintegerinteger")])]
