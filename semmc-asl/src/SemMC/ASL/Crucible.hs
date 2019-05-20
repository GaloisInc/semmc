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
-- | Convert fragments of ASL code into Crucible CFGs
module SemMC.ASL.Crucible (
    functionToCrucible
  , Function(..)
  , procedureToCrucible
  , Procedure(..)
  , FunctionSignature
  , funcSigRepr
  , funcArgReprs
  , funcGlobalReprs
  , ProcedureSignature
  , procArgReprs
  , SomeSignature(..)
  , Callable(..) , asCallable
  , DefType(..), asDefType
  , Const(..), asConst
  , LabeledValue(..)
  , BaseGlobalVar(..)
  , Overrides(..)
  -- * Preprocessing
  , SigM(..)
  , SigState(..)
  , computeType
  -- , computeDefinitionSignature
  -- , computeInstructionSignature
  -- , collectUserTypes
  , UserType
  , Definitions(..)
  -- * Syntax extension
  , ASLExt
  , ASLApp(..)
  , ASLStmt
  , aslExtImpl
  -- * Exceptions
  , TranslationException(..)
  ) where

import qualified Control.Exception as X
import qualified Control.Monad.Except as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.RWS as RWS
import           Control.Monad.ST ( stToIO, RealWorld )
import           Data.Maybe (maybeToList, catMaybes)
import qualified Data.Map as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import           Data.Traversable (forM)
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.CFG.SSAConversion as CCS
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT
import qualified What4.FunctionName as WFN
import qualified What4.ProgramLoc as WP

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Extension ( ASLExt, ASLApp(..), ASLStmt(..), aslExtImpl )
import           SemMC.ASL.Exceptions ( TranslationException(..) )
import           SemMC.ASL.Signature
import           SemMC.ASL.Translation ( UserType(..), userTypeRepr, TranslationState(..), Overrides(..), translateStatement )

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

data Const = DefConst AS.Identifier AS.Type AS.Expr

asConst :: AS.Definition -> Maybe Const
asConst def =
  case def of
    AS.DefConst ident tp e -> Just $ DefConst ident tp e
    _ -> Nothing

data DefVariable = DefVariable AS.QualifiedIdentifier AS.Type

asDefVariable :: AS.Definition -> Maybe DefVariable
asDefVariable def = case def of
  AS.DefVariable ident tp -> Just (DefVariable ident tp)
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

data SigEnv = SigEnv { callables :: Map.Map T.Text Callable
                     , globalVars :: Map.Map T.Text DefVariable
                     , types :: Map.Map T.Text DefType
                     , builtinTypes :: Map.Map T.Text (Some UserType)
                     }

data SigState = SigState { userTypes :: Map.Map T.Text (Some UserType)
                           -- ^ user-defined types
                         -- , globalVarTypes :: Map.Map T.Text (Some WT.BaseTypeRepr)
                         --   -- ^ types for global variables
                         , callableSigs :: Map.Map T.Text SomeSignature
                           -- ^ signatures for callables
                         -- , consts :: Map.Map T.Text (Some WT.BaseTypeRepr)
                         }

data SigException = TypeNotFound T.Text
                  | BuiltinTypeNotFound T.Text
                  | CallableNotFound T.Text

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
  _ -> return []

caseAlternativeGlobalVars :: AS.CaseAlternative -> SigM [(T.Text, Some WT.BaseTypeRepr)]
caseAlternativeGlobalVars _ = undefined

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
    callable <- lookupCallable callableName
    callableGlobals <- callableGlobalVars callable
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
  AS.StmtFor _ (init, term) stmts -> do
    initGlobals <- exprGlobalVars init
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

callableGlobalVars :: Callable -> SigM [(T.Text, Some WT.BaseTypeRepr)]
callableGlobalVars _ = undefined

computeCallableSignature :: Callable -> SigM SomeSignature
computeCallableSignature Callable{..} = case callableRets of
  [] -> undefined -- procedure
  retTypes -> undefined -- function

someAssignment :: [Some f] -> Some (Ctx.Assignment f)
someAssignment [] = Some Ctx.empty
someAssignment (Some f : rst) = I.runIdentity $ do
  Some assignRst <- return $ someAssignment rst
  return $ Some (Ctx.extend assignRst f)

-- computeInstructionSignature :: [(T.Text, SomeSignature)] -> [AS.Stmt] -> IO SomeSignature
-- computeInstructionSignature = undefined

data Definitions arch =
  Definitions { defSignatures :: Map.Map T.Text SomeSignature
              , defTypes :: Map.Map T.Text (Some UserType)
              , defOverrides :: Overrides arch
              }

-- | Convert an ASL function (signature + list of statements) into a Crucible CFG
--
-- We currently assume that functions take arguments and return a single value,
-- while not accessing /any/ global state.
--
-- Note that there are a bunch of intermediate functions to set up the
-- 'CCG.Generator' monad; the real work is done in 'defineFunction'.
functionToCrucible :: (ret ~ CT.BaseToType tp)
                   => Definitions arch
                   -> FunctionSignature globals init tp
                   -> CFH.HandleAllocator RealWorld
                   -> [AS.Stmt]
                   -> IO (Function arch globals init tp)
functionToCrucible defs sig hdlAlloc stmts = do
  let argReprs = FC.fmapFC projectValue (funcArgReprs sig)
  let retRepr = CT.baseToType (funcSigRepr sig)
  hdl <- stToIO (CFH.mkHandle' hdlAlloc (WFN.functionNameFromText (funcName sig)) argReprs retRepr)
  globals <- FC.traverseFC allocateGlobal (funcGlobalReprs sig)
  let pos = WP.InternalPos
  (CCG.SomeCFG cfg0, _) <- stToIO $ CCG.defineFunction pos hdl (funcDef defs sig globals stmts)
  return Function { funcSig = sig
                  , funcCFG = CCS.toSSA cfg0
                  , funcGlobals = globals
                  }
  where
    allocateGlobal :: forall tp . LabeledValue T.Text WT.BaseTypeRepr tp -> IO (BaseGlobalVar tp)
    allocateGlobal (LabeledValue name rep) =
      stToIO (BaseGlobalVar <$> CCG.freshGlobalVar hdlAlloc name (CT.baseToType rep))

-- | A wrapper around translated functions to keep signatures with CFGs
data Function arch globals init tp =
  Function { funcSig :: FunctionSignature globals init tp
           , funcCFG :: CCC.SomeCFG (ASLExt arch) init (CT.BaseToType tp)
           , funcGlobals :: Ctx.Assignment BaseGlobalVar globals
           }

funcDef :: (ret ~ CT.BaseToType tp)
        => Definitions arch
        -> FunctionSignature globals init tp
        -> Ctx.Assignment BaseGlobalVar globals
        -> [AS.Stmt]
        -> Ctx.Assignment (CCG.Atom s) init
        -> (TranslationState s, CCG.Generator (ASLExt arch) h s TranslationState ret (CCG.Expr (ASLExt arch) s ret))
funcDef defs sig globals stmts args = (funcInitialState defs sig globals args, defineFunction (defOverrides defs) sig stmts args)

funcInitialState :: forall init tp s globals arch
                  . Definitions arch
                 -> FunctionSignature globals init tp
                 -> Ctx.Assignment BaseGlobalVar globals
                 -> Ctx.Assignment (CCG.Atom s) init
                 -> TranslationState s
funcInitialState defs sig globals args =
  TranslationState { tsArgAtoms = Ctx.forIndex (Ctx.size args) addArgumentAtom Map.empty
                   , tsVarRefs = Map.empty
                   , tsGlobals = FC.foldrFC addGlobal Map.empty globals
                   , tsFunctionSigs = defSignatures defs
                   , tsUserTypes = defTypes defs
                   }
  where
    addArgumentAtom :: forall tp0
                     . Map.Map T.Text (Some (CCG.Atom s))
                    -> Ctx.Index init tp0
                    -> Map.Map T.Text (Some (CCG.Atom s))
    addArgumentAtom m idx =
      let atom = args Ctx.! idx
          LabeledValue argName _ = funcArgReprs sig Ctx.! idx
      in Map.insert argName (Some atom) m
    addGlobal (BaseGlobalVar gv) m =
      Map.insert (CCG.globalName gv) (Some gv) m


defineFunction :: forall ret tp init h s arch globals
                . (ret ~ CT.BaseToType tp)
               => Overrides arch
               -> FunctionSignature globals init tp
               -> [AS.Stmt]
               -> Ctx.Assignment (CCG.Atom s) init
               -> CCG.Generator (ASLExt arch) h s TranslationState ret (CCG.Expr (ASLExt arch) s ret)
defineFunction ov sig stmts _args = do
  -- FIXME: Put args into the environment as locals (that can be read from)
  --
  -- We have the assignment of atoms available, but the arguments will be
  -- referenced by /name/ by ASL statements.
  mapM_ (translateStatement ov (CT.baseToType (funcSigRepr sig))) stmts
  -- Note: we shouldn't actually get here, as we should have called returnFromFunction while
  -- translating.
  X.throw (NoReturnInFunction (SomeFunctionSignature sig))

data Procedure arch globals init =
  Procedure { procSig :: ProcedureSignature globals init
            , procCFG :: CCC.SomeCFG (ASLExt arch) init (CT.SymbolicStructType globals)
            , procGlobals :: Ctx.Assignment BaseGlobalVar globals
            }

-- | This type alias is a constraint relating the 'globals' (base types) to the
-- actual return type in terms of Crucible types
--
-- The constraint is simple but a bit annoying to write
type ReturnsGlobals ret globals = (ret ~ CT.SymbolicStructType globals)

-- | Translate an ASL procedure (signature plus statements) into a Crucible procedure
--
-- We bundle up the signature, CFG, and allocated globals.  We need to keep the
-- globals around for re-use during simulation.
--
-- The overall strategy is to allocate a Crucible global variable for each part
-- of the CPU state (i.e., machine register) that could be read or written by
-- the procedure.  We'll use symbolic simulation to determine the effect of the
-- procedure on each register.
--
-- Every procedure takes its natural argument list plus one extra argument: the
-- register file (a struct of all of the register values).  When the procedure
-- starts, we'll copy all of the values from the register struct into the globals.
--
-- We assume that all procedures have void type in ASL.  We translate all
-- procedures to return a single argument: a struct with the updated register
-- values.
--
-- NOTE: The signature computation MUST account for the UNPREDICTABLE and
-- UNDEFINED globals.  They may be accessed during the translation and must be
-- available in the 'TranslationState'
procedureToCrucible :: forall arch init globals ret
                     . (ReturnsGlobals ret globals)
                    => Definitions arch
                    -> ProcedureSignature globals init
                    -> CFH.HandleAllocator RealWorld
                    -> [AS.Stmt]
                    -> IO (Procedure arch globals init)
procedureToCrucible defs sig hdlAlloc stmts = do
  let argReprs = FC.fmapFC projectValue (procArgReprs sig)
  let retRepr = procSigRepr sig
  hdl <- stToIO (CFH.mkHandle' hdlAlloc (WFN.functionNameFromText (procName sig)) argReprs retRepr)
  globals <- FC.traverseFC allocateGlobal (procGlobalReprs sig)
  let pos = WP.InternalPos
  (CCG.SomeCFG cfg0, _) <- stToIO $ CCG.defineFunction pos hdl (procDef defs sig globals stmts)
  return Procedure { procSig = sig
                   , procCFG = CCS.toSSA cfg0
                   , procGlobals = globals
                   }
  where
    allocateGlobal :: forall tp . LabeledValue T.Text WT.BaseTypeRepr tp -> IO (BaseGlobalVar tp)
    allocateGlobal (LabeledValue name rep) =
      stToIO (BaseGlobalVar <$> CCG.freshGlobalVar hdlAlloc name (CT.baseToType rep))

procDef :: (ReturnsGlobals ret globals)
        => Definitions arch
        -> ProcedureSignature globals init
        -> Ctx.Assignment BaseGlobalVar globals
        -> [AS.Stmt]
        -> Ctx.Assignment (CCG.Atom s) init
        -> (TranslationState s, CCG.Generator (ASLExt arch) h s TranslationState ret (CCG.Expr (ASLExt arch) s ret))
procDef defs sig globals stmts args =
  (procInitialState defs sig globals args, defineProcedure (defOverrides defs) sig globals stmts args)

procInitialState :: forall init globals s arch
                  . Definitions arch
                 -> ProcedureSignature globals init
                 -> Ctx.Assignment BaseGlobalVar globals
                 -> Ctx.Assignment (CCG.Atom s) init
                 -> TranslationState s
procInitialState defs sig globals args =
  TranslationState { tsArgAtoms = Ctx.forIndex (Ctx.size args) addArgument Map.empty
                   , tsVarRefs = Map.empty
                   , tsGlobals = FC.foldrFC addGlobal Map.empty globals
                   , tsFunctionSigs = defSignatures defs
                   , tsUserTypes = defTypes defs
                   }
  where
    addArgument :: forall tp
                 . Map.Map T.Text (Some (CCG.Atom s))
                -> Ctx.Index init tp
                -> Map.Map T.Text (Some (CCG.Atom s))
    addArgument m idx =
      Map.insert (projectLabel (procArgReprs sig Ctx.! idx)) (Some (args Ctx.! idx)) m
    addGlobal (BaseGlobalVar gv) m =
      Map.insert (CCG.globalName gv) (Some gv) m

defineProcedure :: (ReturnsGlobals ret globals)
                => Overrides arch
                -> ProcedureSignature globals init
                -> Ctx.Assignment BaseGlobalVar globals
                -> [AS.Stmt]
                -> Ctx.Assignment (CCG.Atom s) init
                -> CCG.Generator (ASLExt arch) h s TranslationState ret (CCG.Expr (ASLExt arch) s ret)
defineProcedure ov sig baseGlobals stmts _args = do
  mapM_ (translateStatement ov (procSigRepr sig)) stmts
  retExpr <- CCG.extensionStmt (GetRegState (FC.fmapFC projectValue (procGlobalReprs sig)) baseGlobals)
  if | Just Refl <- testEquality (CCG.exprType retExpr) (procSigRepr sig) ->
       return retExpr
     | otherwise -> X.throw (UnexpectedProcedureReturn (procSigRepr sig) (CCG.exprType retExpr))

{- Note [Call Translation]

There are two types of calls in ASL: functions and procedures.

Functions are simple and are already purely functional, and thus need no additional support.

Procedures are more complicated, as they return no values but instead update processor state through
side effects.  Our challenge in this code is to turn these imperative procedures into pure
functions.  The strategy will be to arrange it so that, in addition to its natural set of
parameters, each procedure takes an entire machine state as a BaseStruct.  It will also return an
entire BaseStruct register state.

At procedure initialization time, the procedure will copy all of its input machine state into a set
of locals (Crucible or globals).  Before calling a procedure, the caller takes a snapshot of the current
machine state (from the refs) to construct the BaseStruct to pass to the callee.  After a procedure
call returns, the caller will assign the contents of the register state back to its locals (refs).

Question: do we need any additional components to the return value of procedures?  Anything that
isn't a global is local, and local modifications can't be reflected to callers.

Note that we have an additional unusual constraint: we need to represent calls
in any context as uninterpreted functions, since we don't want to eagerly expand
definitions of functions.  Doing so produces an enormous code explosion that we
can't handle.  Crucible can support uninterpreted functions via what4; however,
they aren't exactly first class.  Uninterpreted functions can only take as
arguments and return base types.  Crucible doesn't have great support for
working with base types.

Beyond the normal machine registers, we introduce two extra state variables:
- Undefined
- Unpredictable

Each is a boolean that starts as False and is switched to True if an instruction
has undefined or unpredictable behavior, respectively.

-}
