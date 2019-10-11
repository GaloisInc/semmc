{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module SemMC.ASL.Translation (
    TranslationState(..)
  , translateExpr
  , translateStatement
  , translateStatements
  , addExtendedTypeData
  , Overrides(..)
  , overrides
  , UserType(..)
  , Definitions(..)
  , userTypeRepr
  , ToBaseType
  , ToBaseTypes
  ) where

import           Control.Applicative ( (<|>) )
import qualified Control.Exception as X
import           Control.Monad ( forM_, when, void, foldM, foldM_, (>=>), (<=<) )
import qualified Control.Monad.State.Class as MS
import           Control.Monad.Trans ( lift)
import qualified Control.Monad.State as MSS
import           Control.Monad.Trans.Maybe as MaybeT
import qualified Data.BitVector.Sized as BVS
import           Data.Maybe ( fromMaybe, catMaybes )
import qualified Data.Bimap as BM
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.List as List
import           Data.List.Index (imap, imapM, imapM_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Core as CC
import qualified Lang.Crucible.CFG.Expr as CCE
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.Types as CT
import           Numeric.Natural ( Natural )
import qualified What4.BaseTypes as WT
import qualified What4.FunctionName as WF
import qualified What4.ProgramLoc as WP
import qualified What4.Symbol as WS

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Extension ( ASLExt, ASLApp(..), ASLStmt(..) )
import           SemMC.ASL.Exceptions ( TranslationException(..), TracedTranslationException(..) )
import           SemMC.ASL.Signature
import           SemMC.ASL.Types
import           SemMC.ASL.Translation.Preprocess
import qualified SemMC.ASL.SyntaxTraverse as ASLT

import qualified Lang.Crucible.FunctionHandle as FH
import qualified Lang.Crucible.CFG.Reg as CCR
import qualified What4.Utils.MonadST as MST
import qualified Data.STRef as STRef

import System.IO.Unsafe


-- | This wrapper is used as a uniform return type in 'lookupVarRef', as each of
-- the lookup types (arguments, locals, or globals) technically return different
-- values, but they are values that are pretty easy to handle uniformly.
--
-- We could probably get rid of this wrapper if we made a function like
-- @withVarValue@ that took a continuation instead.
data ExprConstructor arch regs h s ret where
  ExprConstructor :: a tp
                  -> (a tp -> Generator h s arch ret (CCG.Expr (ASLExt arch) s tp))
                  -> ExprConstructor (ASLExt arch) regs h s ret

-- | Inside of the translator, look up the current definition of a name
--
-- We currently assume that arguments are never assigned to (i.e., there is no
-- name shadowing).
lookupVarRef' :: forall arch h s ret
              . T.Text
             -> Generator h s arch ret (Maybe (Some (CCG.Expr (ASLExt arch) s)))
lookupVarRef' name = do
  ts <- MS.get
  case (lookupLocalConst ts <|>
        lookupArg ts <|>
        lookupRef ts <|>
        lookupGlobal ts <|>
        lookupEnum ts <|>
        lookupConst ts) of
    Just (ExprConstructor e con) -> Just <$> Some <$> con e
    Nothing -> return Nothing
  where
    lookupLocalConst ts = do
      sv <- lookupStaticEnv name (tsStaticEnv ts)
      case sv of
        StaticInt i -> return (ExprConstructor (CCG.App (CCE.IntLit i)) return)
        StaticBool b -> return (ExprConstructor (CCG.App (CCE.BoolLit b)) return)
        StaticBV bv -> case bitsToBVExpr bv of
          Some bve -> return (ExprConstructor bve return)

    lookupArg ts = do
      Some e <- Map.lookup name (tsArgAtoms ts)
      return (ExprConstructor (CCG.AtomExpr e) return)
    lookupRef ts = do
      Some r <- Map.lookup name (tsVarRefs ts)
      return (ExprConstructor r CCG.readReg)
    lookupGlobal ts = do
      Some g <- Map.lookup name (tsGlobals ts)
      return (ExprConstructor g CCG.readGlobal)
    lookupEnum ts = do
      e <- Map.lookup name (tsEnums ts)
      return (ExprConstructor (CCG.App (CCE.IntLit e)) return)
    lookupConst ts = do
      Some (ConstVal repr e) <- Map.lookup name (tsConsts ts)
      case repr of
        WT.BaseBoolRepr -> return (ExprConstructor (CCG.App (CCE.BoolLit e)) return)
        WT.BaseIntegerRepr -> return (ExprConstructor (CCG.App (CCE.IntLit e)) return)
        WT.BaseBVRepr wRepr ->
          return (ExprConstructor (CCG.App (CCE.BVLit wRepr (BVS.bvIntegerU e))) return)
        _ -> error "bad const type"

lookupVarRef :: forall arch h s ret
             . T.Text
            -> Generator h s arch ret (Some (CCG.Expr (ASLExt arch) s))
lookupVarRef name = do
  mref <- lookupVarRef' name
  case mref of
    Just ref -> return ref
    Nothing -> throwTrace $ UnboundName name

-- | Inside of the translator, look up the current definition of a name
--
-- We currently assume that arguments are never assigned to (i.e., there is no
-- name shadowing).
lookupVarType :: forall arch h s ret
              . T.Text
             -> Generator h s arch ret (Maybe (Some (CT.TypeRepr)))
lookupVarType name = do
  ts <- MS.get
  return (lookupLocalConst ts <|>
          lookupArg ts <|>
          lookupRef ts <|>
          lookupGlobal ts <|>
          lookupEnum ts <|>
          lookupConst ts)
  where
    lookupLocalConst ts = do
      sv <- lookupStaticEnv name (tsStaticEnv ts)
      case sv of
        StaticInt _ -> return $ Some CT.IntegerRepr
        StaticBool _ -> return $ Some CT.BoolRepr
        StaticBV bv -> case bitsToBVRepr bv of
          Some (BVRepr nr) -> return $ Some $ CT.BVRepr nr
    lookupArg ts = do
      Some e <- Map.lookup name (tsArgAtoms ts)
      return $ Some $ CCG.typeOfAtom e
    lookupRef ts = do
      Some r <- Map.lookup name (tsVarRefs ts)
      return $ Some $ CCG.typeOfReg r
    lookupGlobal ts = do
      Some g <- Map.lookup name (tsGlobals ts)
      return $ Some $ CCG.globalType g
    lookupEnum ts = do
      e <- Map.lookup name (tsEnums ts)
      return $ Some $ CT.IntegerRepr
    lookupConst ts = do
      Some (ConstVal repr e) <- Map.lookup name (tsConsts ts)
      return $ Some $ CT.baseToType repr

-- | Overrides for syntactic forms
--
-- Each of the frontends can match on different bits of syntax and handle their
-- translation specially.  This should be useful for replacing some trivial
-- accessors with simpler forms in Crucible.
data Overrides arch =
  Overrides { overrideStmt :: forall h s ret . AS.Stmt -> Maybe (Generator h s arch ret ())
            , overrideExpr :: forall h s ret . AS.Expr -> TypeConstraint -> Maybe (Generator h s arch ret (Some (CCG.Atom s), ExtendedTypeData))
            }

type Generator h s arch ret = CCG.Generator (ASLExt arch) h s (TranslationState h ret) ret

-- Tracks state necessary for the translation of ASL into Crucible
--
-- This is primarily storing variable bindings and the set of signatures
-- available for other callees.
data TranslationState h ret s =
  TranslationState { tsArgAtoms :: Map.Map T.Text (Some (CCG.Atom s))
                   -- ^ Atoms corresponding to function/procedure inputs.  We assume that these are
                   -- immutable and allocated before we start executing.
                   , tsVarRefs :: Map.Map T.Text (Some (CCG.Reg s))
                   -- ^ Local registers containing values; these are created on first use
                   , tsExtendedTypes :: Map.Map T.Text ExtendedTypeData
                   -- ^ Additional type information for local variables
                   , tsGlobals :: Map.Map T.Text (Some CCG.GlobalVar)
                   -- ^ Global variables corresponding to machine state (e.g., machine registers).
                   -- These are allocated before we start executing based on the list of
                   -- transitively-referenced globals in the signature.
                   , tsEnums :: Map.Map T.Text Integer
                   -- ^ Map from enumeration constant names to their integer values.
                   , tsConsts :: Map.Map T.Text (Some ConstVal)
                   -- ^ Map from constants to their types and values.
                   , tsUserTypes :: Map.Map T.Text (Some UserType)
                   -- ^ The base types assigned to user-defined types (defined in the ASL script)
                   -- , tsEnumBounds :: Map.Map T.Text Natural
                   -- ^ The number of constructors in an enumerated type.  These
                   -- bounds are used in assertions checking the completeness of
                   -- case statements.
                   -- ,
                   , tsFunctionSigs :: Map.Map T.Text SomeSimpleSignature
                   -- ^ A collection of all of the signatures of defined functions (both functions
                   -- and procedures)
                   , tsHandle :: STRef.STRef h (Map.Map T.Text StaticEnv)
                   -- ^ Used to name functions encountered during translation
                   , tsStaticEnv :: StaticEnv
                   -- ^ Environment to give concrete instantiations to polymorphic variables
                   , tsPossibleStaticEnvs :: [StaticEnv]
                   -- ^ Environments which specify all possible concrete values for a subset
                   -- of the variables used in this function.
                   , tsSig :: SomeSignature ret
                   -- ^ Signature of the function/procedure we are translating
                   , tsStmtStack :: [AS.Stmt]
                   -- ^ Stack of statements on this traversal
                   , tsExprStack :: [(AS.Expr, TypeConstraint)]
                   }



-- | The distinguished name of the global variable that represents the bit of
-- information indicating that the processor is in the UNPREDICTABLE state
--
-- We simulate the UNPREDICATABLE and UNDEFINED ASL statements with virtual
-- processor state.
unpredictableVarName :: T.Text
unpredictableVarName = T.pack "UNPREDICTABLE"

-- | The distinguished name of the global variable that represents the bit of
-- state indicating that the processor is in the UNDEFINED state.
undefinedVarName :: T.Text
undefinedVarName = T.pack "UNDEFINED"

-- | The distinguished name of the global variable that represents the bit of
-- state indicating that an assertion has been tripped.
assertionfailureVarName :: T.Text
assertionfailureVarName = T.pack "ASSERTIONFAILURE"

-- | Obtain the global variables touched by the given 'ProcedureSignature'
--
-- This is a subset of all of the global state (and a subset of the current
-- global state).
withProcGlobals :: (m ~ Generator h s arch ret)
                => Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
                -> (Ctx.Assignment WT.BaseTypeRepr globals -> Ctx.Assignment BaseGlobalVar globals -> m r)
                -> m r
withProcGlobals reprs k = do
  globMap <- MS.gets tsGlobals
  let globReprs = FC.fmapFC projectValue reprs
  k globReprs (FC.fmapFC (fetchGlobal globMap) reprs)
  where
    fetchGlobal :: forall tp . Map.Map T.Text (Some CCG.GlobalVar)
                -> LabeledValue T.Text WT.BaseTypeRepr tp
                -> BaseGlobalVar tp
    fetchGlobal globMap (LabeledValue globName rep)
      | Just (Some gv) <- Map.lookup globName globMap
      , Just Refl <- testEquality (CT.baseToType rep) (CCG.globalType gv) =
          BaseGlobalVar gv
      | otherwise = error ("Missing global (or wrong type): " ++ show globName)

throwTrace :: TranslationException -> Generator h s arch ret a
throwTrace e = do
  stmts <- MS.gets tsStmtStack
  exprs <- MS.gets tsExprStack
  sig <- MS.gets tsSig
  env <- MS.gets tsStaticEnv
  X.throw $ TracedTranslationException (someSigName sig) env stmts exprs e

-- | Translate a list of statements, reporting an error if the execution
-- runs off the end.
translateStatements :: forall ret tp h s arch
                . (ret ~ CT.BaseToType tp)
               => Overrides arch
               -> [AS.Stmt]
               -> Generator h s arch ret ()
translateStatements ov stmts = do
  sig <- MS.gets tsSig
  mapM_ (\(nm,t) -> addExtendedTypeData nm t) (someSigArgs sig)
  mapM_ (translateStatement ov) stmts
  let errmsg = "Function " <> someSigName sig <> " does not return."
  errStr <- CCG.mkAtom (CCG.App (CCE.TextLit errmsg))
  CCG.reportError (CCG.AtomExpr errStr)

translateStatement :: forall arch ret h s
                    . Overrides arch
                   -> AS.Stmt
                   -> Generator h s arch ret ()
translateStatement ov stmt = do
  MS.modify' $ \s -> s { tsStmtStack = stmt : (tsStmtStack s) }
  translateStatement' ov stmt
  MS.modify' $ \s -> s { tsStmtStack = List.tail $ tsStmtStack s }
  return ()

assertExpr :: Overrides arch
           -> AS.Expr
           -> T.Text
           -> Generator h s arch ret ()
assertExpr ov e msg = do
  (Some res) <- translateExpr overrides e
  Refl <- assertAtomType e CT.BoolRepr res
  assertAtom res (Just e) msg

assertAtom :: CCG.Atom s CT.BoolType
           -> Maybe AS.Expr
           -> T.Text
           -> Generator h s arch ret ()
assertAtom res mexpr msg = do
  gs <- MS.gets tsGlobals
  case Map.lookup assertionfailureVarName gs of
    Just (Some gv)
      | Just Refl <- testEquality (CCG.globalType gv) CT.BoolRepr -> do
          CCG.writeGlobal gv $ CCG.AtomExpr res
          case mexpr of
            Just (AS.ExprVarRef (AS.QualifiedIdentifier _ "FALSE")) ->
              return ()
            Just expr ->
              CCG.assertExpr (CCG.AtomExpr res) (CCG.App (CCE.TextLit $ msg <> (T.pack $ "Expression: " <> show expr)))
            _ -> CCG.assertExpr (CCG.AtomExpr res) (CCG.App (CCE.TextLit msg))
      | otherwise -> X.throw (UnexpectedGlobalType assertionfailureVarName (CCG.globalType gv))
    _ -> X.throw $ MissingGlobal assertionfailureVarName

-- | Translate a single ASL statement into Crucible
translateStatement' :: forall arch ret h s
                    . Overrides arch
                   -> AS.Stmt
                   -- ^ Statement we are translating
                   -> Generator h s arch ret ()
translateStatement' ov stmt
  | Just so <- overrideStmt ov stmt = so
  | otherwise = case stmt of
      AS.StmtReturn Nothing -> do
        sig <- MS.gets tsSig
        let rep = someSigRepr sig
        case sig of
          SomeProcedureSignature pSig
            | globalBaseTypes <- FC.fmapFC projectValue (procGlobalReprs pSig)
            , pSigRepr <- procSigRepr pSig
            , Just Refl <- testEquality pSigRepr (CT.SymbolicStructRepr globalBaseTypes) ->
              withProcGlobals (procGlobalReprs pSig) $ \globalBaseTypes globals -> do
                Refl <- return $ baseCrucProof globalBaseTypes
                globalsSnapshot <- CCG.extensionStmt (GetRegState globalBaseTypes globals)
                CCG.returnFromFunction globalsSnapshot
          _ -> X.throw (InvalidReturnType rep)
      AS.StmtReturn (Just expr) -> do
        sig <- MS.gets tsSig
        let retTy = someSigRepr sig
        (Some a, _) <- translateExpr' ov expr (ConstraintSingle retTy)
        Refl <- assertAtomType expr retTy a
        CCG.returnFromFunction (CCG.AtomExpr a)
      AS.StmtIf clauses melse -> translateIf ov clauses melse
      AS.StmtCase e alts -> translateCase ov e alts
      AS.StmtAssert e -> assertExpr ov e "ASL Assertion"
      AS.StmtVarsDecl ty idents -> mapM_ (declareUndefinedVar ty) idents
      AS.StmtVarDeclInit (ident, ty) expr -> translateDefinedVar ov ty ident expr
      AS.StmtConstDecl (ident, ty) expr -> do
        -- NOTE: We use the same translation for constants.  We don't do any verification that the
        -- ASL doesn't attempt to modify a constant.
        env <- MS.gets tsStaticEnv
        case exprToStatic env expr of
          Just sv -> mapStaticEnv (insertStaticEnv ident sv)
          _ -> return ()
        translateDefinedVar ov ty ident expr

      AS.StmtAssign lval expr -> translateAssignment ov lval expr
      AS.StmtWhile test body -> do
        let testG = do
              Some testA <- translateExpr ov test
              Refl <- assertAtomType test CT.BoolRepr testA
              return (CCG.AtomExpr testA)
        let bodyG = mapM_ (translateStatement ov) body
        CCG.while (WP.InternalPos, testG) (WP.InternalPos, bodyG)
      AS.StmtRepeat body test -> translateRepeat ov body test
      AS.StmtFor var (lo, hi) body -> translateFor ov var lo hi body
      AS.StmtUndefined -> do
        gs <- MS.gets tsGlobals
        case Map.lookup undefinedVarName gs of
          Just (Some gv)
            | Just Refl <- testEquality (CCG.globalType gv) CT.BoolRepr -> do
                CCG.writeGlobal gv (CCG.App (CCE.BoolLit True))
            | otherwise -> X.throw (UnexpectedGlobalType undefinedVarName (CCG.globalType gv))
          _ -> X.throw $ MissingGlobal undefinedVarName
      AS.StmtUnpredictable -> do
        gs <- MS.gets tsGlobals
        case Map.lookup unpredictableVarName gs of
          Just (Some gv)
            | Just Refl <- testEquality (CCG.globalType gv) CT.BoolRepr -> do
                CCG.writeGlobal gv (CCG.App (CCE.BoolLit True))
            | otherwise -> X.throw (UnexpectedGlobalType unpredictableVarName (CCG.globalType gv))
          _ ->  X.throw $ MissingGlobal unpredictableVarName
      -- NOTE: Ensure that this is safe.  Most SEE statements seem to not be
      -- particularly actionable, but many may need to be manually overridden.
      AS.StmtSeeExpr {} -> return ()
      AS.StmtSeeString {} -> return ()
      AS.StmtCall qIdent args -> do
        sigMap <- MS.gets tsFunctionSigs
        let ident = mkFunctionName qIdent (length args)
        case Map.lookup ident sigMap of
          Nothing -> throwTrace $ MissingFunctionDefinition ident
          Just (SomeSimpleFunctionSignature _) -> X.throw (ExpectedProcedureSignature ident)
          Just (SomeSimpleProcedureSignature pSig) -> do
            (finalIdent, argAtoms, _) <- unifyArgs ov ident (zip (sprocArgs pSig) args) [] ConstraintNone
            case Ctx.fromList argAtoms of
              Some argAssign -> do               
                let atomTypes = FC.fmapFC CCG.typeOfAtom argAssign
                -- FIXME: The problem is that we might need to snapshot a
                -- subset of globals for each call.  Each subset might be
                -- different.
                --
                -- How do we select a subset with the right types?
                --
                -- We could key everything on name and do dynamic type
                -- checks to assert that globals with the right name have
                -- the right type.
                withProcGlobals (sprocGlobalReprs pSig) $ \globalReprs globals -> do
                  let globalsType = CT.baseToType (WT.BaseStructRepr globalReprs)
                  globalsSnapshot <- CCG.extensionStmt (GetRegState globalReprs globals)
                  let vals = FC.fmapFC CCG.AtomExpr argAssign
                  let ufRep = WT.BaseStructRepr (FC.fmapFC projectValue (sprocGlobalReprs pSig))
                  let uf = UF finalIdent ufRep (atomTypes Ctx.:> globalsType) (vals Ctx.:> globalsSnapshot)
                  atom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp uf))
                  _ <- CCG.extensionStmt (SetRegState globals (CCG.AtomExpr atom))
                  return ()

      _ -> throwTrace $ UnsupportedStmt stmt

-- | Translate a for statement into Crucible
--
-- The translation is from
--
-- > for i = X to Y
-- >    body
--
-- to
--
-- > i = X
-- > while(i <= Y)
-- >   body
-- >   i = i + 1
--
-- NOTE: The translation is inclusive of the upper bound - is that right?
--
-- NOTE: We are assuming that the variable assignment is actually a declaration of integer type
translateFor :: Overrides arch
             -> AS.Identifier
             -> AS.Expr
             -> AS.Expr
             -> [AS.Stmt]
             -> Generator h s arch ret ()
translateFor ov var lo hi body = do
  env <- MS.gets tsStaticEnv
  case (exprToStatic env lo, exprToStatic env hi) of
    (Just (StaticInt lo'), Just (StaticInt hi')) -> unrollFor ov var lo' hi' body
    _ -> do
      vars <- MS.gets tsVarRefs
      case Map.lookup var vars of
        Just (Some lreg) -> do
          Some atom <- translateExpr ov lo
          Refl <- assertAtomType' (CCG.typeOfReg lreg) atom
          CCG.assignReg lreg (CCG.AtomExpr atom)
        _ -> do
          let ty = AS.TypeRef (AS.QualifiedIdentifier AS.ArchQualAny (T.pack "integer"))
          translateDefinedVar ov ty var lo
      let ident = AS.QualifiedIdentifier AS.ArchQualAny var
      let testG = do
            let testE = AS.ExprBinOp AS.BinOpLTEQ (AS.ExprVarRef ident) hi
            Some testA <- translateExpr ov testE
            Refl <- assertAtomType testE CT.BoolRepr testA
            return (CCG.AtomExpr testA)
      let increment = do
            AS.StmtAssign (AS.LValVarRef ident)
              (AS.ExprBinOp AS.BinOpAdd (AS.ExprVarRef ident) (AS.ExprLitInt 1))
            
      let bodyG = mapM_ (translateStatement ov) (body ++ [increment])
      CCG.while (WP.InternalPos, testG) (WP.InternalPos, bodyG)


unrollFor :: Overrides arch
          -> AS.Identifier
          -> Integer
          -> Integer
          -> [AS.Stmt]
          -> Generator h s arch ret ()
unrollFor ov var lo hi body = do
  mapM_ translateFor [lo .. hi]
  where
    translateFor i = do
      mapStaticEnv (insertStaticEnv var (StaticInt i) . pushFreshStaticEnv)
      translateStatement ov (letInStmt [] body)
      mapStaticEnv popStaticEnv
       


translateRepeat :: Overrides arch
                -> [AS.Stmt]
                -> AS.Expr
                -> Generator h s arch ret ()
translateRepeat ov body test = do
  cond_lbl <- CCG.newLabel
  loop_lbl <- CCG.newLabel
  exit_lbl <- CCG.newLabel

  CCG.defineBlock loop_lbl $ do
    mapM_ (translateStatement ov) body
    CCG.jump cond_lbl

  CCG.defineBlock cond_lbl $ do
    Some testA <- translateExpr ov test
    Refl <- assertAtomType test CT.BoolRepr testA
    CCG.branch (CCG.AtomExpr testA) loop_lbl exit_lbl

  CCG.continue exit_lbl (CCG.jump loop_lbl)

translateDefinedVar :: Overrides arch
                    -> AS.Type
                    -> AS.Identifier
                    -> AS.Expr
                    -> Generator h s arch ret ()
translateDefinedVar ov ty ident expr = do
  Some expected <- translateType ty
  (Some atom, ext) <- translateExpr' ov expr (ConstraintSingle expected)
  Refl <- assertAtomType expr expected atom
  locals <- MS.gets tsVarRefs
  when (Map.member ident locals) $ do
    X.throw (LocalAlreadyDefined ident)
  putExtendedTypeData ident ext
  reg <- CCG.newReg (CCG.AtomExpr atom)
  MS.modify' $ \s -> s { tsVarRefs = Map.insert ident (Some reg) locals }


-- | Convert an lVal to its equivalent expression.
lValToExpr :: AS.LValExpr -> Maybe AS.Expr
lValToExpr lval = case lval of
  AS.LValVarRef qName -> return $ AS.ExprVarRef qName
  AS.LValMember lv memberName -> do
    lve <- lValToExpr lv
    return $ AS.ExprMember lve memberName
  AS.LValArrayIndex lv slices -> do
    lve <- lValToExpr lv
    return $ AS.ExprIndex lve slices
  AS.LValSliceOf lv slices -> do
    lve <- lValToExpr lv
    return $ AS.ExprSlice lve slices
  _ -> Nothing


constraintOfLVal :: Overrides arch
           -> AS.LValExpr
           -> Generator h s arch ret TypeConstraint
constraintOfLVal ov lval = case lval of
  AS.LValIgnore -> return $ ConstraintNone
  AS.LValVarRef (AS.QualifiedIdentifier _ ident) -> do
    mTy <- lookupVarType ident
    case mTy of
      Just (Some ty) -> return $ ConstraintSingle ty
      Nothing -> do
        sig <- MS.gets tsSig
        case Map.lookup (someSigName sig, ident) localTypeHints of
          Just tc -> return $ tc
          _ -> return $ ConstraintNone
  AS.LValTuple lvs -> do
    lvTs <- mapM (constraintOfLVal ov) lvs
    return $ ConstraintTuple lvTs
  AS.LValMemberBits _ bits
    | Just (Some nr) <- WT.someNat (length bits)
    , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` nr ->
      return $ ConstraintSingle $ CT.BVRepr nr
  AS.LValSlice lvs -> do
    mTy <- runMaybeT $ do
      lengths <- mapM (bvLengthM <=< lift . constraintOfLVal ov) lvs
      case WT.someNat (sum lengths) of
        Just (Some repr)
          | Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` repr ->
            return $ Some $ CT.BVRepr repr
        Nothing -> fail ""
    return $ mConstraint mTy
  AS.LValSliceOf e [slice] -> do
    mLen <- getStaticSliceLength slice
    case mLen of
      Just (Some (BVRepr len)) ->
        return $ ConstraintSingle $ CT.BVRepr len
      Nothing -> do
        innerConstraint <- constraintOfLVal ov e
        return $ relaxConstraint innerConstraint

  _ -> case lValToExpr lval of
         Just lve -> do
           Some lveAtom <- translateExpr ov lve
           return $ ConstraintSingle $ (CCG.typeOfAtom lveAtom)
         Nothing -> return ConstraintNone
  where
    bvLengthM t = MaybeT (return (bvLength t))

    bvLength :: TypeConstraint -> Maybe Integer
    bvLength tp = case tp of
      ConstraintSingle (CT.BVRepr nr) -> Just (WT.intValue nr)
      _ -> Nothing

    mConstraint :: Maybe (Some (CT.TypeRepr)) -> TypeConstraint
    mConstraint mTy = case mTy of
      Just (Some ty) -> ConstraintSingle ty
      Nothing -> ConstraintNone



-- | Translate general assignment statements into Crucible
--
-- This case is interesting, as assignments can be to locals or globals.
--
-- NOTE: We are assuming that there cannot be assignments to arguments.
translateAssignment :: Overrides arch
                    -> AS.LValExpr
                    -> AS.Expr
                    -> Generator h s arch ret ()
translateAssignment ov lval e = do
  -- If possible, determine the type of the left hand side first in order
  -- to inform the translation of the given expression
  constraint <- constraintOfLVal ov lval
  (Some atom, ext) <- translateExpr' ov e constraint
  translateAssignment'' ov lval atom constraint ext (Just e)

translateAssignment' :: forall arch s tp h ret . Overrides arch
                     -> AS.LValExpr
                     -> CCG.Atom s tp
                     -> ExtendedTypeData
                     -> Maybe AS.Expr
                     -> Generator h s arch ret ()
translateAssignment' ov lval atom atomext mE =
  translateAssignment'' ov lval atom (ConstraintSingle (CCG.typeOfAtom atom)) atomext mE

mkSliceRange :: (Integer, Integer) -> AS.Slice
mkSliceRange (lo, hi) = AS.SliceRange (AS.ExprLitInt hi) (AS.ExprLitInt lo)


assertExprType :: ext ~ ASLExt arch =>
                   CT.TypeRepr tp1
                -- ^ Expected type
                -> CT.TypeRepr tp2
                -- ^ Expr type repr
                -> CCG.Expr ext s tp2
                -- ^ Translation (which contains the actual type)
                -> Generator h s arch ret (tp1 :~: tp2)
assertExprType expectedRepr repr expr =
  case testEquality expectedRepr repr of
    Nothing -> throwTrace (UnexpectedExprType Nothing repr expectedRepr)
    Just Refl -> return Refl


translateAssignment'' :: forall arch s tp h ret . Overrides arch
                     -> AS.LValExpr
                     -> CCG.Atom s tp
                     -> TypeConstraint
                     -> ExtendedTypeData
                     -> Maybe AS.Expr
                     -> Generator h s arch ret ()
translateAssignment'' ov lval atom constraint atomext mE = do
  case lval of
    AS.LValIgnore -> return () -- Totally ignore - this probably shouldn't happen (except inside of a tuple)
    AS.LValVarRef (AS.QualifiedIdentifier _ ident) -> do
      locals <- MS.gets tsVarRefs
      putExtendedTypeData ident atomext

      case Map.lookup ident locals of
        Just (Some lreg) -> do
          Refl <- assertAtomType' (CCG.typeOfReg lreg) atom
          CCG.assignReg lreg (CCG.AtomExpr atom)
        Nothing -> do
          globals <- MS.gets tsGlobals
          case Map.lookup ident globals of
            Just (Some gv) -> do
              Refl <- assertAtomType' (CCG.globalType gv) atom
              CCG.writeGlobal gv (CCG.AtomExpr atom)
            Nothing -> do
              let atomType = CCG.typeOfAtom atom
              reg <- CCG.newReg (CCG.AtomExpr atom)
              MS.modify' $ \s -> s { tsVarRefs = Map.insert ident (Some reg) locals }
    AS.LValMember struct memberName -> do
      Just lve <- return $ lValToExpr struct
      (Some structAtom, ext) <- translateExpr' ov lve ConstraintNone
      case ext of
        TypeRegister sig ->
          case Map.lookup memberName sig of
            Just slice -> do
              translatelValSlice ov struct (mkSliceRange slice) atom constraint
            _ -> X.throw $ MissingRegisterField lve memberName
        TypeStruct acc ->
          -- Some e <- lookupVarRef structName
          -- satom <- CCG.mkAtom e
          -- case (CCG.typeOfAtom satom, Map.lookup memberName acc) of
          --   (CT.SymbolicStructRepr tps, Just (StructAccessor repr idx _)) |
          --     Just Refl <- testEquality tps repr -> do
          --       let getStruct = GetBaseStruct (CT.SymbolicStructRepr tps) idx (CCG.AtomExpr satom)
          --       getAtom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp getStruct))
                return () -- FIXME: Construct modified struct here
           -- _ -> error $ "Mismatch in struct fields" <> show struct <> "." <> show memberName
        _ -> X.throw $ UnexpectedExtendedType lve ext

    AS.LValTuple lvals ->
      case atomext of
        TypeTuple exts | length exts == length lvals ->
          case CCG.typeOfAtom atom of
            CT.SymbolicStructRepr tps -> void $ Ctx.traverseAndCollect (assignTupleElt lvals exts tps atom) tps
            tp -> X.throw $ ExpectedStructType mE tp
        _ -> error $ "Unexpected extended type information:" <> show lvals <> " " <> show atomext

    AS.LValSliceOf lv [slice] -> translatelValSlice ov lv slice atom constraint

    AS.LValSliceOf lv [fstSlice@(AS.SliceSingle _), slice] -> do
      case CCG.typeOfAtom atom of
        CT.BVRepr wRepr -> do
          let topIndex = WT.intValue wRepr - 1
          Some topBit <- translateSlice' ov atom (AS.SliceSingle (AS.ExprLitInt topIndex)) ConstraintNone
          translatelValSlice ov lv fstSlice topBit ConstraintNone
          Some rest <- translateSlice' ov atom (AS.SliceRange (AS.ExprLitInt (topIndex - 1))
                                                (AS.ExprLitInt 0)) ConstraintNone
          translatelValSlice ov lv slice rest ConstraintNone
        tp -> throwTrace $ ExpectedBVType' mE tp

    AS.LValArrayIndex ref@(AS.LValVarRef (AS.QualifiedIdentifier _ arrName)) [AS.SliceSingle slice] -> do
        Some e <- lookupVarRef arrName
        arrAtom <- CCG.mkAtom e
        Some idxAtom <- translateExpr ov slice
        if | CT.AsBaseType bt <- CT.asBaseType (CCG.typeOfAtom idxAtom)
           , CT.SymbolicArrayRepr (Ctx.Empty Ctx.:> bt') retTy <- CCG.typeOfAtom arrAtom
           , Just Refl <- testEquality bt bt' -- index types match
           , CT.AsBaseType btAsn <- CT.asBaseType (CCG.typeOfAtom atom)
           , Just Refl <- testEquality btAsn retTy -- array element types match
           -> do
               let asn = Ctx.singleton (CCE.BaseTerm bt (CCG.AtomExpr idxAtom))
               --FIXME: We mask array updates with an uninterpreted function to
               -- avoid interpreting their updates
               -- let name = "ARRAY_" <> arrName <> "_UPDATE"
               -- let uf = UF name (CT.BaseArrayRepr (Ctx.Empty Ctx.:> bt') retTy)
               --            (Ctx.singleton $ CCG.typeOfAtom idxAtom)
               --            (Ctx.singleton $ CCG.AtomExpr idxAtom)
               -- newArr <- CCG.mkAtom (CCG.App (CCE.ExtensionApp uf))
               -- translateAssignment' ov ref newArr TypeBasic Nothing
               let arr = CCG.App $ CCE.SymArrayUpdate retTy (CCG.AtomExpr arrAtom) asn (CCG.AtomExpr atom)
               newArr <- CCG.mkAtom arr
               translateAssignment' ov ref newArr TypeBasic Nothing
           | otherwise -> error $ "Invalid array assignment: " ++ show lval

    AS.LValArrayIndex _ (_ : _ : _) -> do
      error $
        "Unexpected multi-argument array assignment. Is this actually a setter?" ++ show lval

    AS.LValMemberBits ref bits -> do
      let ibits = imap (\i -> \e -> (i, e)) bits
      mapM_ (\(i, e) -> do
        Some aslice <- translateSlice' ov atom (AS.SliceSingle (AS.ExprLitInt (toInteger i))) ConstraintNone
        let lv' = AS.LValMember ref e in
          translateAssignment' ov lv' aslice TypeBasic Nothing) ibits

    AS.LValSlice lvs ->
      case CCG.typeOfAtom atom of
        CT.BVRepr repr -> foldM_ (translateImplicitSlice ov repr atom) 0 lvs
        tp -> throwTrace $ ExpectedBVType' mE tp

    _ -> X.throw $ UnsupportedLVal lval
    where assignTupleElt :: [AS.LValExpr]
                         -> [ExtendedTypeData]
                         -> Ctx.Assignment WT.BaseTypeRepr ctx
                         -> CCG.Atom s (CT.SymbolicStructType ctx)
                         -> Ctx.Index ctx tp'
                         -> WT.BaseTypeRepr tp'
                         -> Generator h s arch ret ()
          assignTupleElt lvals exts tps struct ix _ = do
            let getStruct = GetBaseStruct (CT.SymbolicStructRepr tps) ix (CCG.AtomExpr struct)
            getAtom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp getStruct))
            let ixv = Ctx.indexVal ix
            translateAssignment' ov (lvals !! ixv) getAtom (exts !! ixv) Nothing

translateImplicitSlice :: Overrides arch
                       -> WT.NatRepr w
                       -> CCG.Atom s (CT.BVType w)
                       -> Integer
                       -> AS.LValExpr
                       -> Generator h s arch ret (Integer)
translateImplicitSlice ov rhsRepr rhs offset lv  = do
  lvT <- constraintOfLVal ov lv
  case lvT of
    ConstraintSingle (CT.BVRepr lvRepr) -> do
      let lvLength = WT.intValue lvRepr
      let rhsLength = WT.intValue rhsRepr
      let hi = rhsLength - offset - 1
      let lo = rhsLength - offset - lvLength
      let slice = AS.SliceRange (AS.ExprLitInt hi) (AS.ExprLitInt lo)
      Some slicedRhs <- translateSlice' ov rhs slice ConstraintNone
      translateAssignment' ov lv slicedRhs TypeBasic Nothing
      return (offset + lvLength)
    _ -> X.throw $ UnsupportedLVal lv

-- These functions push all the bitvector type matching into runtime checks.

maybeBVSelect :: WT.NatRepr idx
               -> WT.NatRepr len
               -> WT.NatRepr w
               -> Maybe (CCG.Expr ext s (CT.BVType w))
               -> Maybe ((CCG.Expr ext s) (CT.BaseToType (WT.BaseBVType len)))
maybeBVSelect lo len w (Just expr) =
  if | Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` len
     , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` w
     , Just WT.LeqProof <- (lo `WT.addNat` len) `WT.testLeq` w
     -> Just $ CCG.App (CCE.BVSelect lo len w expr)
     | otherwise -> Nothing
maybeBVSelect _ _ _ Nothing = Nothing


maybeBVConcat :: WT.NatRepr u
               -> WT.NatRepr v
               -> Maybe (CCG.Expr ext s (CT.BVType u))
               -> Maybe (CCG.Expr ext s (CT.BVType v))
               -> Maybe ((CCG.Expr ext s) (CT.BaseToType (WT.BaseBVType (u WT.+ v))))
maybeBVConcat lo len (Just pre) (Just atom) =
  if | Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` len
     , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` lo
     , Just WT.LeqProof <- (WT.knownNat @1)  `WT.testLeq` (lo `WT.addNat` len)
     -> Just $ CCG.App (CCE.BVConcat lo len pre atom)
     | otherwise -> Nothing
maybeBVConcat lo _ Nothing (Just atom) =
  if | Just Refl <- testEquality (WT.knownNat @0) lo
     -> Just atom
     | otherwise -> error "BVConcat type mismatch"
maybeBVConcat _ len (Just pre) Nothing =
  if | Just Refl <- testEquality (WT.knownNat @0) len
     -> Just pre
     | otherwise -> error "BVConcat type mismatch"
maybeBVConcat _ _ _ _ = error "BVConcat type mismatch"


translatelValSlice :: Overrides arch
               -> AS.LValExpr
               -> AS.Slice
               -> CCG.Atom s tp
               -> TypeConstraint
               -> Generator h s arch ret ()
translatelValSlice ov lv slice asnAtom' constraint = do
  let Just lve = lValToExpr lv
  Some atom' <- translateExpr ov lve
  SomeSlice range sliceOf@(SliceOf lenRepr wRepr atom) <- getSomeSlice ov slice atom' constraint

  withValidSlice' slice range sliceOf $ do
    case range of
      SliceStatic loRepr hiRepr -> do
        let asnAtom = asnAtom'
        Refl <- assertAtomType' (CT.BVRepr lenRepr) asnAtom
        let lenUpper = wRepr `WT.subNat` (hiRepr `WT.addNat` (WT.knownNat @1))
        -- [x0 .. x_loRepr .. x_hiRepr .. x_wRepr-1 ] -- original bitvector in lv
        -- [y_loRepr .. y_hiRepr] -- asnAtom value

        let lvBV = Just (CCG.AtomExpr atom)
        let asnBV = Just (CCG.AtomExpr asnAtom)
        let pre = maybeBVSelect (WT.knownNat @0) loRepr wRepr lvBV
          -- pre = [x0 .. x_loRepr-1]
        let first = maybeBVConcat loRepr lenRepr pre asnBV
          -- first = pre ++ [y_loRepr .. y_hiRepr]
        let post = maybeBVSelect (hiRepr `WT.addNat` (WT.knownNat @1)) lenUpper wRepr lvBV
          -- post = [x_hiRepr+1 .. x_wRepr-1]
        let fin = maybeBVConcat (loRepr `WT.addNat` lenRepr) lenUpper first post
          -- Final bv has the correct length:
          -- (lo + len) + lenUpper = lo + 1 + (hi - lo) + (w - (1 + hi))
          --                       = lo + 1 + hi - lo + w - 1 - hi
          --                       = (lo - lo) + (hi - hi) + (1 - 1) + w
          --                       = w
          -- Shape of final bv:
          -- fin = first ++ post
          --     = [x0 .. x_loRepr-1] ++ [y_loRepr .. y_hiRepr] ++ [x_hiRepr+1 .. x_wRepr-1]
        case fin of
          Just r -> do
            result <- CCG.mkAtom r
            translateAssignment' ov lv result TypeBasic Nothing
          Nothing ->
            X.throw $ InvalidSliceRange (WT.intValue loRepr) (WT.intValue hiRepr)
      SliceSymbolic loAtom _ -> do
        asnAtom <- zextBVAtom lenRepr asnAtom'
        let loBV = CCG.App $ CCE.IntegerToBV wRepr (CCG.AtomExpr loAtom)
        -- [0 0 .. x_hiAtom .. x_0](lenRepr) -- asnAtom
        negAsn <- CCG.mkAtom $ CCG.App $ CCE.BVNot lenRepr (CCG.AtomExpr asnAtom)
        -- [0 0 .. -x_hiAtom .. -x_0](lenRepr)
        zextnegAsn <- zextBVAtom wRepr negAsn
        -- [0 0 0 .. -x_hiAtom .. -x_0](wRepr)
        let zextnegShiftAsn = CCG.App $ CCE.BVShl wRepr (CCG.AtomExpr zextnegAsn) loBV
        -- [0 .. -x_(hiAtom + loAtom) .. -x_loAtom .. 0](wRepr)
        let oextShiftAsn = CCG.App $ CCE.BVNot wRepr zextnegShiftAsn
        -- [1 .. x_(hiAtom + loAtom) .. x_loAtom .. 1](wRepr)
        -- [y_(wRepr)          ..                 y_0](wRepr) -- atom
        let maskedResult = CCG.App $ CCE.BVAnd wRepr oextShiftAsn (CCG.AtomExpr atom)
        -- [y_(wRepr) .. x(hiAtom + loAtom) .. x_loAtom .. y_0](wRepr)
        result <- CCG.mkAtom maskedResult
        translateAssignment' ov lv result TypeBasic Nothing


-- | Get the "default" value for a given crucible type. We can't use unassigned
-- registers, as ASL functions occasionally return values uninitialized.
getDefaultValue :: CT.TypeRepr tp
                -> CCG.Expr (ASLExt arch) s tp
getDefaultValue repr = case repr of
  CT.BVRepr wRepr -> CCG.App $ CCE.BVUndef wRepr
  CT.SymbolicStructRepr tps -> do
    let crucAsn = toCrucTypes tps
    if | Refl <- baseCrucProof tps ->
          CCG.App $ CCE.ExtensionApp $ MkBaseStruct crucAsn (FC.fmapFC getDefaultValue crucAsn)
  CT.IntegerRepr -> mkUF "UNDEFINED_Integer" repr
  CT.NatRepr -> mkUF "UNDEFINED_Nat" repr
  CT.BoolRepr -> mkUF "UNDEFINED_Bool" repr
  _ -> error $ "Invalid undefined value: " <> show repr
  where
    mkUF :: T.Text -> CT.TypeRepr tp -> CCG.Expr (ASLExt arch) s tp
    mkUF nm repr = case CT.asBaseType repr of
      CT.AsBaseType brepr -> do
        let uf = UF nm brepr Ctx.empty Ctx.empty
        CCG.App (CCE.ExtensionApp uf)
      _ -> error $ "Illegal crucible type: " <> show repr


-- | Put a new local in scope and initialize it to an undefined value
declareUndefinedVar :: AS.Type
                    -> AS.Identifier
                    -> Generator h s arch ret ()
declareUndefinedVar ty ident = do
  locals <- MS.gets tsVarRefs
  when (Map.member ident locals) $ do
    X.throw (LocalAlreadyDefined ident)
  addExtendedTypeData ident ty
  tty <- translateType ty
  case tty of
    Some rep -> do
      e <- return $ getDefaultValue rep
      reg <- CCG.newReg e
      MS.modify' $ \s -> s { tsVarRefs = Map.insert ident (Some reg) locals }



mkExtendedTypeData :: AS.Type
                   -> Generator h s arch ret (ExtendedTypeData)
mkExtendedTypeData = mkExtendedTypeData' getUT getExtendedTypeData
  where
    getUT :: T.Text -> Generator h s arch ret (Maybe (Some UserType))
    getUT tpName = Map.lookup tpName <$> MS.gets tsUserTypes
    getET tpName = getUT tpName

addExtendedTypeData :: AS.Identifier
                    -> AS.Type                    
                    -> Generator h s arch ret ()
addExtendedTypeData ident ty = do
  ext <- mkExtendedTypeData ty
  putExtendedTypeData ident ext

putExtendedTypeData :: AS.Identifier
                    -> ExtendedTypeData
                    -> Generator h s arch ret ()
putExtendedTypeData ident ext' = do
  ext'' <- getExtendedTypeData ident
  ext <- mergeExtensions ext' ext''
  MS.modify' $ \s -> s { tsExtendedTypes = Map.insert ident ext (tsExtendedTypes s) }

getExtendedTypeData :: AS.Identifier
                    -> Generator h s arch ret (ExtendedTypeData)
getExtendedTypeData ident = do
  exts <- MS.gets tsExtendedTypes
  return $ fromMaybe TypeBasic (Map.lookup ident exts)

mergeExtensions :: ExtendedTypeData
                -> ExtendedTypeData
                -> Generator h s arch ret (ExtendedTypeData)
mergeExtensions ext1 ext2 =
  case (ext1, ext2) of
  (_, TypeBasic) -> return ext1
  (TypeBasic, _) -> return ext2
  _ -> if ext1 == ext2 then return ext1
    else return TypeBasic


-- | Translate types (including user-defined types) into Crucible type reprs
--
-- Translations of user-defined types (i.e., types defined in an ASL program)
-- are stored in the 'TranslationState' and are looked up when needed.
--
translateType :: AS.Type -> Generator h s arch ret (Some CT.TypeRepr)
translateType t = do
  env <- MS.gets tsStaticEnv
  t' <- case applyStaticEnv' env t of
    Just t' -> return $ t'
    Nothing -> throwTrace $ CannotStaticallyEvaluateType t env
  case t' of
    AS.TypeRef (AS.QualifiedIdentifier _ "integer") -> return (Some CT.IntegerRepr)
    AS.TypeRef (AS.QualifiedIdentifier _ "boolean") -> return (Some CT.BoolRepr)
    AS.TypeRef (AS.QualifiedIdentifier _ "bit") -> return (Some (CT.BVRepr (NR.knownNat @1)))
    AS.TypeRef qi@(AS.QualifiedIdentifier _ ident) -> do
      uts <- MS.gets tsUserTypes
      case Map.lookup ident uts of
        Nothing -> X.throw (UnexpectedType qi)
        Just (Some ut) -> return (Some (CT.baseToType (userTypeRepr ut)))
    AS.TypeFun "bits" e ->
      case e of
        AS.ExprLitInt nBits
          | Just (Some nr) <- NR.someNat nBits
          , Just NR.LeqProof <- NR.isPosNat nr -> return (Some (CT.BVRepr nr))
        _ -> error ("Unsupported type: " ++ show t)
    AS.TypeFun _ _ -> error ("Unsupported type: " ++ show t)
    AS.TypeArray _ty _idxTy -> error ("Unsupported type: " ++ show t)
    AS.TypeReg _i _flds -> error ("Unsupported type: " ++ show t)
    _ -> error ("Unsupported type: " ++ show t)

withStaticEnv :: StaticEnv
              -> Generator h s arch ret a
              -> Generator h s arch ret (a, StaticEnv)
withStaticEnv tenv f = do
  env <- MS.gets tsStaticEnv
  MS.modify' $ \s -> s { tsStaticEnv = tenv }
  x <- f
  env' <- MS.gets tsStaticEnv
  MS.modify' $ \s -> s { tsStaticEnv = env }
  return (x, env')

mapStaticEnv :: (StaticEnv -> StaticEnv)
             -> Generator h s arch ret ()
mapStaticEnv f = do
  env <- MS.gets tsStaticEnv
  MS.modify' $ \s -> s { tsStaticEnv = f env }



collectStaticValues :: StaticEnv
                    -> AS.SymbolDecl
                    -> AS.Expr
                    -> Generator h s arch ret ()
collectStaticValues outerEnv (nm, t) e = do
  case exprToStatic outerEnv e of
    Just i -> mapStaticEnv (insertStaticEnv nm i)
    _ -> return ()

-- Unify a syntactic ASL type against a crucible type, and update
-- the current static variable evironment with any discovered instantiations
unifyType :: AS.Type
          -> TypeConstraint
          -> Generator h s arch ret ()
unifyType aslT constraint = do
  env <- MS.gets tsStaticEnv
  case (aslT, constraint) of
    (AS.TypeFun "bits" e, ConstraintSingle (CT.BVRepr repr)) ->
      case e of
        AS.ExprLitInt i | Just (Some nr) <- NR.someNat i, Just Refl <- testEquality repr nr -> return ()
        AS.ExprVarRef (AS.QualifiedIdentifier _ id) ->
          case lookupStaticEnv id env of
            Just (StaticInt i) | Just (Some nr) <- NR.someNat i, Just Refl <- testEquality repr nr -> return ()
            Nothing -> mapStaticEnv (insertStaticEnv id (StaticInt $ toInteger (NR.natValue repr)))
            _ -> throwTrace $ TypeUnificationFailure aslT constraint env
        AS.ExprBinOp AS.BinOpMul e e' ->
          case (mInt env e, mInt env e') of
            (Left i, Left i') | Just (Some nr) <- NR.someNat (i * i'), Just Refl <- testEquality repr nr -> return ()
            (Right (AS.ExprVarRef (AS.QualifiedIdentifier _ id)), Left i')
              | reprVal <- toInteger $ WT.natValue repr
              , (innerVal, 0) <- reprVal `divMod` i' ->
                mapStaticEnv $ insertStaticEnv id (StaticInt innerVal)
            (Left i, Right (AS.ExprVarRef (AS.QualifiedIdentifier _ id)))
              | reprVal <- toInteger $ WT.natValue repr
              , (innerVal, 0) <- reprVal `divMod` i ->
               mapStaticEnv $ insertStaticEnv id (StaticInt innerVal)
            _ -> throwTrace $ TypeUnificationFailure aslT constraint env
        _ -> throwTrace $ TypeUnificationFailure aslT constraint env
    -- it's not clear if this is always safe

    -- (AS.TypeFun "bits" _ , ConstraintHint (HintMaxBVSize nr)) -> do
    --   case applyStaticEnv' env aslT of
    --     Just _ -> return ()
    --     _ -> unifyType aslT (ConstraintSingle (CT.BVRepr nr))
    (_, ConstraintHint _) -> return ()
    (_, ConstraintNone) -> return ()
    (_, ConstraintTuple _) -> throwTrace $ TypeUnificationFailure aslT constraint env
    (_, ConstraintSingle cTy) -> do
      Some atomT' <- translateType aslT
      case testEquality cTy atomT' of
        Just Refl -> return ()
        _ -> throwTrace $ TypeUnificationFailure aslT constraint env
  where
    mInt env e = case exprToStatic env e of
      Just (StaticInt i) -> Left i
      _ -> Right e

varsOfExpr :: AS.Expr -> [T.Text]
varsOfExpr e = ASLT.foldExpr getVar e []
  where
    getVar (AS.ExprVarRef (AS.QualifiedIdentifier q ident)) = (:) ident
    getVar _ = id

dependentVarsOfType :: AS.Type -> [T.Text]
dependentVarsOfType t = case t of
  AS.TypeFun "bits" e -> varsOfExpr e
  _ -> []


unifyTypes :: [AS.Type]
           -> TypeConstraint
           -> Generator h s arch ret ()
unifyTypes tps constraint = do
  case constraint of
    ConstraintSingle (CT.SymbolicStructRepr stps) |
        insts <- zip tps (FC.toListFC (ConstraintSingle . CT.baseToType) stps)
      , length insts == length tps ->
          mapM_ (\(tp, stp) -> unifyType tp stp) insts
    ConstraintTuple cts
      | length tps == length cts ->
        mapM_ (\(tp, ct) -> unifyType tp ct) (zip tps cts)
    ConstraintNone -> return ()
    _ -> X.throw $ TypesUnificationFailure tps constraint

getConcreteTypeConstraint :: AS.Type -> Generator h s arch ret TypeConstraint
getConcreteTypeConstraint t = do
  env <- MS.gets tsStaticEnv
  case applyStaticEnv' env t of
    Just t' -> do
      Some ty <- translateType t'
      return $ ConstraintSingle ty
    _ -> return $ ConstraintNone

someTypeOfAtom :: CCG.Atom s tp
               -> TypeConstraint
someTypeOfAtom atom = ConstraintSingle (CCG.typeOfAtom atom)
        
unifyArg :: Overrides arch
         -> StaticEnv
         -> AS.SymbolDecl
         -> AS.Expr
         -> Generator h s arch ret (Some (CCG.Atom s))
unifyArg ov outerEnv (nm,t) e = do
  cty <- getConcreteTypeConstraint t
  ((Some atom, _), _) <- withStaticEnv outerEnv $ translateExpr' ov e cty
  let atomT = CCG.typeOfAtom atom
  unifyType t (ConstraintSingle (CCG.typeOfAtom atom))
  return $ Some atom

asBaseType :: Some CT.TypeRepr -> Some WT.BaseTypeRepr
asBaseType (Some t) = case CT.asBaseType t of
  CT.AsBaseType bt -> Some bt
  CT.NotBaseType -> error $ "Expected base type: " <> show t

unifyArgs :: Overrides arch
          -> T.Text
          -> [(AS.SymbolDecl, AS.Expr)]
          -> [AS.Type]
          -> TypeConstraint
          -> Generator h s arch ret
               (T.Text, [Some (CCG.Atom s)], Maybe (Some WT.BaseTypeRepr))
unifyArgs ov fnname args rets constraint = do
  outerEnv <- MS.gets tsStaticEnv
  (atoms, tenv) <- withStaticEnv emptyStaticEnv $ do
      mapM_ (\(decl, e) -> collectStaticValues outerEnv decl e) args
      atoms <- mapM (\(decl, e) -> unifyArg ov outerEnv decl e) args
      unifyRet outerEnv rets constraint
      return atoms
  let dvars = concat $ map dependentVarsOfType rets ++ map (\((_,t), _) -> dependentVarsOfType t) args
  listenv <- mapM (getConcreteValue tenv) dvars
  let env = fromListStaticEnv listenv
  hdl <- MS.gets tsHandle
  MST.liftST (STRef.modifySTRef hdl (Map.insert fnname env))
  retsT <- mapM (\t -> fst <$> withStaticEnv tenv (translateType t)) rets
  retT <- case retsT of
            [] -> return Nothing
            _ -> return $ Just $ mkBaseStructRepr (map asBaseType retsT)
  
  return (mkFinalFunctionName env fnname, atoms, retT)
  where
    mInt env e = case exprToStatic env e of
      Just (StaticInt i) -> Left i
      _ -> Right e

    unifyRet :: StaticEnv
          -> [AS.Type] -- return type of function
          -> TypeConstraint -- potential concrete target type
          -> Generator h s arch ret ()
    unifyRet outerEnv [t] constraint = unifyType t constraint
    unifyRet outerEnv ts constraints = unifyTypes ts constraints

    getConcreteValue env nm = case lookupStaticEnv nm env of
      Just i -> return (nm, i)
      _ -> throwTrace $ CannotMonomorphizeFunctionCall fnname env


-- | Collect any new variables declared
getNewVars :: Generator h s arch ret a
           -> Generator h s arch ret ([Some (CCG.Reg s)], a)
getNewVars f = do
  vars <- MS.gets tsVarRefs
  r <- f
  vars' <- MS.gets tsVarRefs
  let diff = Map.difference vars' vars
  return (Map.elems diff, r)

-- | Initialize registers to their default values
initVars :: [Some (CCG.Reg s)]
         -> Generator h s arch ret ()
initVars regs = do
    mapM_ initVar regs
  where
    initVar (Some reg) =
      CCG.assignReg reg (getDefaultValue (CCG.typeOfReg reg))

-- | Statement-level if-then-else. Newly assigned variables from
-- either branch are implicitly assigned to default
-- values before branching avoid dangling registers.
ifte_ :: CCG.Expr (ASLExt arch) s CT.BoolType
      -> Generator h s arch ret () -- ^ true branch
      -> Generator h s arch ret () -- ^ false branch
      -> Generator h s arch ret ()
ifte_ e x y = do
  c_id <- CCG.newLabel
  (newvarsThen, x_id) <- getNewVars (CCG.defineBlockLabel $ x >> CCG.jump c_id)
  (newvarsElse, y_id) <- getNewVars (CCG.defineBlockLabel $ y >> CCG.jump c_id)
  initVars $ newvarsThen ++ newvarsElse
  CCG.continue c_id (CCG.branch e x_id y_id)

translateIf :: Overrides arch
            -> [(AS.Expr, [AS.Stmt])]
            -> Maybe [AS.Stmt]
            -> Generator h s arch ret ()
translateIf ov clauses melse =
  case clauses of
    [] -> mapM_ (translateStatement ov) (fromMaybe [] melse)
    (cond, body) : rest ->
      withStaticTest cond
        (mapM_ (translateStatement ov) body)
        (translateIf ov rest melse) $ do
      Some condAtom <- translateExpr ov cond
      Refl <- assertAtomType cond CT.BoolRepr condAtom
      let genThen = mapM_ (translateStatement ov) body
      let genElse = translateIf ov rest melse
      ifte_ (CCG.AtomExpr condAtom) genThen genElse

translateCase :: Overrides arch
              -> AS.Expr
              -> [AS.CaseAlternative]
              -> Generator h s arch ret ()
translateCase ov expr alts = case alts of
  [AS.CaseOtherwise els] -> mapM_ (translateStatement ov) els
  -- FIXME: We assume that the case below is equivalent to "otherwise"
  [AS.CaseWhen _ Nothing body] -> mapM_ (translateStatement ov) body
  -- FIXME: If we detect an "unreachable", translate it as if the preceding "when"
  -- were "otherwise"
  [AS.CaseWhen _ Nothing body, AS.CaseOtherwise [AS.StmtCall (AS.QualifiedIdentifier _ "Unreachable") []]] ->
    mapM_ (translateStatement ov) body
  (AS.CaseWhen pats Nothing body : rst) -> do
    let matchExpr = caseWhenExpr expr pats
    Some matchAtom <- translateExpr ov matchExpr
    Refl <- assertAtomType matchExpr CT.BoolRepr matchAtom
    let genThen = mapM_ (translateStatement ov) body
    let genRest = translateCase ov expr rst
    ifte_ (CCG.AtomExpr matchAtom) genThen genRest
  _ -> error (show alts)
  where
    caseWhenExpr :: AS.Expr -> [AS.CasePattern] -> AS.Expr
    caseWhenExpr _ [] = error "caseWhenExpr"
    caseWhenExpr expr [pat] = matchPat expr pat
    caseWhenExpr expr (pat:pats) = AS.ExprBinOp AS.BinOpLogicalOr (matchPat expr pat) (caseWhenExpr expr pats)

matchPat :: AS.Expr -> AS.CasePattern -> AS.Expr
matchPat expr (AS.CasePatternInt i) = AS.ExprBinOp AS.BinOpEQ expr (AS.ExprLitInt i)
matchPat expr (AS.CasePatternBin bv) = AS.ExprBinOp AS.BinOpEQ expr (AS.ExprLitBin bv)
matchPat expr (AS.CasePatternIdentifier ident) = AS.ExprBinOp AS.BinOpEQ expr (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny ident))
matchPat expr (AS.CasePatternMask mask) = AS.ExprBinOp AS.BinOpEQ expr (AS.ExprLitMask mask)
matchPat _ AS.CasePatternIgnore = X.throw $ UNIMPLEMENTED "ignore pattern unimplemented"
matchPat _ (AS.CasePatternTuple _) = X.throw $ UNIMPLEMENTED "tuple pattern unimplemented"

assertAtomType :: AS.Expr
               -- ^ Expression that was translated
               -> CT.TypeRepr tp1
               -- ^ Expected type
               -> CCG.Atom s tp2
               -- ^ Translation (which contains the actual type)
               -> Generator h s arch ret (tp1 :~: tp2)
assertAtomType expr expectedRepr atom =
  case testEquality expectedRepr (CCG.typeOfAtom atom) of
    Nothing -> throwTrace (UnexpectedExprType (Just expr) (CCG.typeOfAtom atom) expectedRepr)
    Just Refl -> return Refl

assertAtomType' :: CT.TypeRepr tp1
                -- ^ Expected type
                -> CCG.Atom s tp2
                -- ^ Translation (which contains the actual type)
                -> Generator h s arch ret (tp1 :~: tp2)
assertAtomType' expectedRepr atom =
  case testEquality expectedRepr (CCG.typeOfAtom atom) of
    Nothing -> throwTrace (UnexpectedExprType Nothing (CCG.typeOfAtom atom) expectedRepr)
    Just Refl -> return Refl

data BVRepr tp where
  BVRepr :: (tp ~ CT.BVType w, 1 WT.<= w) => WT.NatRepr w -> BVRepr tp

getAtomBVRepr :: CCG.Atom s tp
              -> Generator h s arch ret (BVRepr tp)
getAtomBVRepr atom =
  case CCG.typeOfAtom atom of
    CT.BVRepr wRepr -> return $ BVRepr wRepr
    tp -> throwTrace $ ExpectedBVType' Nothing tp

translateExpr' :: Overrides arch
              -> AS.Expr
              -> TypeConstraint
              -> Generator h s arch ret (Some (CCG.Atom s), ExtendedTypeData)
translateExpr' ov expr constraint = do
  MS.modify' $ \s -> s { tsExprStack = (expr, constraint) : (tsExprStack s) }
  r <- translateExpr'' ov expr constraint
  MS.modify' $ \s -> s { tsExprStack = List.tail $ tsExprStack s }
  return r

getStaticValue :: AS.Expr
               -> Generator h s arch ret (Maybe (StaticValue))
getStaticValue expr = do
  env <- MS.gets tsStaticEnv
  return $ exprToStatic env expr


-- This is not necessarily complete
constraintsOfArgs :: AS.BinOp -> TypeConstraint -> (TypeConstraint, TypeConstraint)
constraintsOfArgs bop tc = case bop of
  AS.BinOpAdd -> (tc, tc)
  AS.BinOpSub -> (tc, tc)
  _ -> (ConstraintNone, ConstraintNone)

bitsToBVRepr :: [Bool] -> Some (BVRepr)
bitsToBVRepr bits = do
  let nBits = length bits
  case NR.mkNatRepr (fromIntegral nBits) of
   Some nr
     | Just NR.LeqProof <- NR.testLeq (NR.knownNat @1) nr ->
       Some $ BVRepr nr
     | otherwise -> X.throw InvalidZeroLengthBitvector

bitsToBVExpr :: [Bool] -> Some (CCG.Expr (ASLExt arch) s)
bitsToBVExpr bits = do
  case bitsToBVRepr bits of
   Some (BVRepr nr) -> Some $ CCG.App $ CCE.BVLit nr (bitsToInteger bits)

-- | Translate an ASL expression into an Atom (which is a reference to an immutable value)
--
-- Atoms may be written to registers, which are mutable locals
translateExpr'' :: Overrides arch
              -> AS.Expr
              -> TypeConstraint
              -> Generator h s arch ret (Some (CCG.Atom s), ExtendedTypeData)
translateExpr'' ov expr ty
  | Just eo <- overrideExpr ov expr ty = eo
  | otherwise = do
      mStatic <- getStaticValue expr
      case mStatic of
        Just (StaticInt i) -> mkAtom (CCG.App (CCE.IntLit i))
        Just (StaticBool b) -> mkAtom (CCG.App (CCE.BoolLit b))
        _ -> case expr of
          AS.ExprLitInt i -> mkAtom (CCG.App (CCE.IntLit i))
          AS.ExprLitBin bits
            | Some exp <- bitsToBVExpr bits ->
              mkAtom exp

          AS.ExprVarRef (AS.QualifiedIdentifier _ ident) -> do
            Some e <- lookupVarRef ident
            atom <- CCG.mkAtom e
            ext <- getExtendedTypeData ident
            return (Some atom, ext)

          AS.ExprLitReal {} -> throwTrace $ UnsupportedExpr expr
          AS.ExprLitString {} -> throwTrace $ UnsupportedExpr expr
          AS.ExprUnOp op expr' -> basicExpr $ translateUnaryOp ov op expr'
          AS.ExprBinOp op e1 e2 -> basicExpr $ translateBinaryOp ov op e1 e2 ty
          AS.ExprTuple exprs -> do
            atomExts <- case ty of
              ConstraintSingle (CT.SymbolicStructRepr tps) -> do
                let exprTs = zip (FC.toListFC Some tps) exprs
                mapM (\(Some ty, e) -> translateExpr' ov e (ConstraintSingle (CT.baseToType ty))) exprTs
              ConstraintTuple cts -> do
                mapM (\(ct, e) -> translateExpr' ov e ct) (zip cts exprs)
              _ -> do
               mapM (\e -> translateExpr' ov e ConstraintNone) exprs
            let (atoms, exts) = unzip atomExts
            case Ctx.fromList atoms of
              Some asgn -> do
                let reprs = FC.fmapFC CCG.typeOfAtom asgn
                let atomExprs = FC.fmapFC CCG.AtomExpr asgn
                let struct = MkBaseStruct reprs atomExprs
                atom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp struct))
                return (Some atom, TypeTuple exts)

          AS.ExprInSet e elts -> do
            Some atom <- translateExpr ov e
            when (null elts) $ X.throw (EmptySetElementList expr)
            preds <- mapM (translateSetElementTest ov expr atom) elts
            mkAtom (foldr disjoin (CCG.App (CCE.BoolLit False)) preds)
          AS.ExprIf clauses elseExpr -> translateIfExpr ov expr clauses elseExpr

          AS.ExprCall qIdent args -> do
            sigMap <- MS.gets tsFunctionSigs
            -- FIXME: make this nicer?
            let ident = mkFunctionName qIdent (length args)

            case Map.lookup ident sigMap of
              Nothing -> throwTrace $ MissingFunctionDefinition ident
              Just (SomeSimpleProcedureSignature _) -> X.throw (ExpectedFunctionSignature ident)
              Just (SomeSimpleFunctionSignature sig) -> do
                (finalIdent, argAtoms, Just (Some retT)) <- unifyArgs ov ident (zip (sfuncArgs sig) args) (sfuncRet sig) ty
                case Ctx.fromList argAtoms of
                  Some argAssign -> do
                    let atomTypes = FC.fmapFC CCG.typeOfAtom argAssign
                    let vals = FC.fmapFC CCG.AtomExpr argAssign
                    let uf = UF finalIdent retT atomTypes vals
                    satom <- Some <$> CCG.mkAtom (CCG.App (CCE.ExtensionApp uf))
                    ext <- case (sfuncRet sig) of
                      [ret] -> mkExtendedTypeData ret
                      rets -> do
                        exts <- mapM mkExtendedTypeData rets
                        return $ TypeTuple exts
                    return (satom, ext)
          -- FIXME: Should this trip a global flag?
          AS.ExprImpDef _ t -> do
            Some ty <- translateType t
            mkAtom (getDefaultValue ty)

          AS.ExprMember struct memberName -> do
            (Some structAtom, ext) <- translateExpr' ov struct ConstraintNone
            case ext of
              TypeRegister sig -> do
                case Map.lookup memberName sig of
                  Just slice -> do
                    satom <- translateSlice ov struct (mkSliceRange slice) ConstraintNone
                    return (satom, TypeBasic)
                  _ -> X.throw $ MissingRegisterField struct memberName
              TypeStruct acc -> do
                case (CCG.typeOfAtom structAtom, Map.lookup memberName acc) of
                  (CT.SymbolicStructRepr tps, Just (StructAccessor repr idx fieldExt)) |
                    Just Refl <- testEquality tps repr -> do
                      let getStruct = GetBaseStruct (CT.SymbolicStructRepr tps) idx (CCG.AtomExpr structAtom)
                      atom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp getStruct))
                      return (Some atom, fieldExt)
                  _ -> X.throw $ MissingStructField struct memberName
              _ -> do
                exts <- MS.gets tsExtendedTypes
                X.throw $ UnexpectedExtendedType struct ext

          AS.ExprMemberBits var bits -> do
            let (hdvar : tlvars) = map (\member -> AS.ExprMember var member) bits
            let expr = foldl (\var -> \e -> AS.ExprBinOp AS.BinOpConcat var e) hdvar tlvars
            translateExpr' ov expr ty

          AS.ExprSlice e [slice] -> do
            satom <- translateSlice ov e slice ty
            return (satom, TypeBasic)

          AS.ExprSlice e (slice : slices) -> do
            let expr = AS.ExprBinOp AS.BinOpConcat (AS.ExprSlice e [slice]) (AS.ExprSlice e slices)
            translateExpr' ov expr ty

          AS.ExprIndex array [AS.SliceSingle slice]  -> do
            (Some atom, ext) <- translateExpr' ov array ConstraintNone
            Some idxAtom <- translateExpr ov slice
            if | CT.AsBaseType bt <- CT.asBaseType (CCG.typeOfAtom idxAtom)
               , CT.SymbolicArrayRepr (Ctx.Empty Ctx.:> bt') retTy <- CCG.typeOfAtom atom
               , Just Refl <- testEquality bt bt' -> do
                   let asn = Ctx.singleton (CCE.BaseTerm bt (CCG.AtomExpr idxAtom))
                   let arr = CCE.SymArrayLookup retTy (CCG.AtomExpr atom) asn
                   ext' <- case ext of
                     TypeArray ext' -> return ext'
                     _ -> return TypeBasic
                   atom' <- CCG.mkAtom (CCG.App arr)
                   return (Some atom', ext')
               | otherwise -> throwTrace $ UnsupportedExpr expr
          AS.ExprUnknown t -> do
            Some ty <- translateType t
            mkAtom (getDefaultValue ty)
          _ -> throwTrace $ UnsupportedExpr expr
  where
    mkAtom e = do
      atom <- CCG.mkAtom e
      return (Some atom, TypeBasic)
    basicExpr f = do
      satom <- f
      return (satom, TypeBasic)

translateExpr :: Overrides arch
              -> AS.Expr
              -> Generator h s arch ret (Some (CCG.Atom s))
translateExpr ov expr = do
  (atom, _) <- translateExpr' ov expr ConstraintNone
  return atom

getIndexOfLabel :: Ctx.Assignment WT.BaseTypeRepr tps
                -> T.Text
                -> CCG.Atom s (CT.SymbolicStructType tps)
                -> Ctx.Index tps tp
                -> LabeledValue T.Text WT.BaseTypeRepr tp
                -> MSS.StateT (Maybe (Some (CCG.Atom s))) (Generator h s arch ret) ()
getIndexOfLabel tps lbl struct ix val =
  if projectLabel val == lbl then do
    let getStruct = GetBaseStruct (CT.SymbolicStructRepr tps) ix (CCG.AtomExpr struct)
    getAtom <- MSS.lift $ CCG.mkAtom (CCG.App (CCE.ExtensionApp getStruct))
    MS.put (Just $ Some getAtom)
  else return ()

-- (1 <= w, 1 <= len, idx + len <= w)

-- data SliceContinue s a where
--   SliceContinue :: (WT.NatRepr lo
--             ->  WT.NatRepr len
--             -> WT.NatRepr w
--             -> WT.LeqProof 1 len
--             -> WT.LeqProof 1 w
--             -> WT.LeqProof (lo WT.+ len) w
--             -> Generator h s arch ret a)
--            -> SliceContinue s a

-- translateSliceAccess :: (Some (CCG.Atom s)) -> SliceContinue s (Some (CCG.Atom s))
-- translateSliceAccess (Some atom) =
--   case CCG.typeOfAtom atom of
--       CT.BVRepr wRepr' ->
--         SliceContinue (\loRepr lenRepr wRepr prf1 prf2 prf3 ->
--                                 if | Just Refl <- testEquality wRepr' wRepr
--                                    , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` lenRepr
--                                    , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` wRepr'
--                                    , WT.LeqProof <- prf3 ->
--                                        Some <$> CCG.mkAtom (CCG.App (CCE.BVSelect loRepr lenRepr wRepr (CCG.AtomExpr atom))))


normalizeSlice :: AS.Slice -> (AS.Expr, AS.Expr)
normalizeSlice slice = case slice of
  AS.SliceRange e e' -> (e', e)
  AS.SliceSingle e -> (e, e)
  AS.SliceOffset e e' ->
    let hi = AS.ExprBinOp AS.BinOpAdd e (AS.ExprBinOp AS.BinOpSub e' (AS.ExprLitInt 1))
        in (e, hi)

data SliceOf s sliceLength atomLength where
  SliceOf :: (1 WT.<= atomLength, 1 WT.<= sliceLength, sliceLength WT.<= atomLength)
          => WT.NatRepr sliceLength
          -> WT.NatRepr atomLength
          -> CCG.Atom s (CT.BVType atomLength)
          -> SliceOf s sliceLength atomLength

data SliceRange s sliceLength atomLength where
    SliceStatic :: (lo WT.<= hi,
                    sliceLength ~ (1 WT.+ (hi WT.- lo)),
                    1 WT.<= sliceLength
                   , (hi WT.+ 1) WT.<= atomLength)
                => WT.NatRepr lo
                -> WT.NatRepr hi
                -> SliceRange s sliceLength atomLength

    SliceSymbolic :: 1 WT.<= sliceLength
                  => CCG.Atom s CT.IntegerType
                  -> CCG.Atom s CT.IntegerType
                  -> SliceRange s sliceLength atomLength

data SomeSlice s where
  SomeSlice :: SliceRange s sliceLength atomLength
            -> SliceOf s sliceLength atomLength
            -> SomeSlice s

getStaticSliceRange :: AS.Slice
                    -> Generator h s arch ret (Maybe (Some WT.NatRepr, Some WT.NatRepr))
getStaticSliceRange slice = do
  let (e', e) = normalizeSlice slice
  env <- MS.gets tsStaticEnv
  case (exprToStatic env e', exprToStatic env e) of
    (Just (StaticInt lo), Just (StaticInt hi)) ->
      case (WT.someNat lo, WT.someNat hi) of
        (Just someLoRepr, Just someHiRepr) ->
          return $ Just (someLoRepr, someHiRepr)
        _ -> throwTrace $ InvalidSliceRange lo hi
    _ -> return Nothing

exprRangeToLength :: StaticEnv -> AS.Expr -> AS.Expr -> Maybe Integer
exprRangeToLength env lo hi = case (lo, hi) of
  (AS.ExprVarRef loVar, AS.ExprBinOp AS.BinOpAdd e (AS.ExprVarRef hiVar)) -> getStaticLength loVar hiVar e
  (AS.ExprVarRef loVar, AS.ExprBinOp AS.BinOpAdd (AS.ExprVarRef hiVar) e) -> getStaticLength loVar hiVar e
  (AS.ExprBinOp AS.BinOpSub (AS.ExprVarRef loVar) e, AS.ExprVarRef hiVar) -> getStaticLength loVar hiVar e
  (e, e') | e == e' -> Just 1
  _ | Just (StaticInt loInt) <- exprToStatic env lo
    , Just (StaticInt hiInt) <- exprToStatic env hi ->
      Just $ (hiInt - loInt) + 1
  _ -> Nothing

  where getStaticLength loVar hiVar e =
          if | loVar == hiVar
             , Just (StaticInt i) <- exprToStatic env e
             , i > 0 ->
               Just $ i + 1
             | otherwise -> Nothing

getStaticSliceLength :: AS.Slice
                     -> Generator h s arch ret (Maybe (Some BVRepr))
getStaticSliceLength slice = do
  let (e', e) = normalizeSlice slice
  env <- MS.gets tsStaticEnv
  if | Just length <- exprRangeToLength env e' e
     , Just (Some lenRepr) <- WT.someNat length
     , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` lenRepr ->
        return $ Just $ Some $ BVRepr lenRepr
     | otherwise -> return Nothing

getSymbolicSliceRange :: Overrides arch
                      -> AS.Slice
                      -> Generator h s arch ret (CCG.Atom s CT.IntegerType, CCG.Atom s CT.IntegerType)
getSymbolicSliceRange ov slice = do
  let (e', e) = normalizeSlice slice
  Some loAtom <- translateExpr ov e'
  Some hiAtom <- translateExpr ov e
  Refl <- assertAtomType e' CT.IntegerRepr loAtom
  Refl <- assertAtomType e CT.IntegerRepr hiAtom
  return (loAtom, hiAtom)

assertJust :: Generator h s arch ret (Maybe a)
           -> Generator h s arch ret a
assertJust f = do
  ma <- f
  case ma of
    Just a -> return a
    _ -> throwTrace $ UnexpectedNothing

getSomeSlice :: Overrides arch
             -> AS.Slice
             -> CCG.Atom s tp
             -> TypeConstraint
             -> Generator h s arch ret (SomeSlice s)
getSomeSlice ov slice slicedAtom constraint = do
  mStatic <- getStaticSliceRange slice
  case mStatic of
    Just (Some loRepr, Some hiRepr)
      | Just (loLeqHi@WT.LeqProof) <- loRepr `WT.testLeq` hiRepr
      , lenRepr <- (WT.knownNat @1) `WT.addNat` (hiRepr `WT.subNat` loRepr)
      , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` lenRepr -> do
        case CCG.typeOfAtom slicedAtom of
          CT.BVRepr wRepr
            | Just WT.LeqProof <- lenRepr `WT.testLeq` wRepr
            , Just WT.LeqProof <- (hiRepr `WT.addNat` (WT.knownNat @1)) `WT.testLeq` wRepr ->
              return $ SomeSlice (SliceStatic loRepr hiRepr) (SliceOf lenRepr wRepr slicedAtom)
          CT.IntegerRepr
            | wRepr <- hiRepr `WT.addNat` (WT.knownNat @1)
            , WT.LeqProof <- WT.leqAdd (WT.leqRefl (WT.knownNat @1)) hiRepr
            , Refl <- WT.plusComm (WT.knownNat @1) hiRepr
            , WT.LeqProof <- WT.leqAdd2 (WT.leqRefl (WT.knownNat @1)) (WT.leqSub (WT.leqRefl hiRepr) loLeqHi)
             -> do
                intAtom <- CCG.mkAtom $ CCG.App (CCE.IntegerToBV wRepr (CCG.AtomExpr slicedAtom))
                return $ SomeSlice (SliceStatic loRepr hiRepr) (SliceOf lenRepr wRepr intAtom)
          tp -> throwTrace $ InvalidSlice (WT.intValue loRepr) (WT.intValue hiRepr) tp
      | otherwise -> throwTrace $ InvalidSliceRange (WT.intValue loRepr) (WT.intValue hiRepr)
    Nothing -> do
      (loAtom, hiAtom) <- getSymbolicSliceRange ov slice
      mLength <- getStaticSliceLength slice
      (Some lenRepr :: Some WT.NatRepr) <- case mLength of
        Just (Some (BVRepr length)) -> return $ Some length
        _ -> case constraint of
          ConstraintSingle (CT.BVRepr length) -> return $ Some length
          ConstraintHint (HintMaxBVSize maxlength) ->
            case CCG.typeOfAtom slicedAtom of
              CT.BVRepr wRepr -> case wRepr `WT.testNatCases` maxlength of
                WT.NatCaseEQ -> return $ Some wRepr
                WT.NatCaseLT _ -> return $ Some wRepr
                WT.NatCaseGT WT.LeqProof -> return $ Some maxlength
              CT.IntegerRepr -> return $ Some maxlength
              _ -> throwTrace $ UnsupportedSlice slice constraint
          ConstraintHint HintAnyBVSize ->
            case CCG.typeOfAtom slicedAtom of
              CT.BVRepr wRepr -> return $ Some wRepr
              _ -> throwTrace $ UnsupportedSlice slice constraint
          _ -> throwTrace $ UnsupportedSlice slice constraint
      WT.LeqProof <- assertJust $ return $ (WT.knownNat @1) `WT.testLeq` lenRepr
      case CCG.typeOfAtom slicedAtom of
        CT.BVRepr wRepr
          | Just WT.LeqProof <- lenRepr `WT.testLeq` wRepr ->
            return $ SomeSlice (SliceSymbolic loAtom hiAtom) (SliceOf lenRepr wRepr slicedAtom)
        CT.IntegerRepr -> do
          intAtom <- CCG.mkAtom $ CCG.App (CCE.IntegerToBV lenRepr (CCG.AtomExpr slicedAtom))
          return $ SomeSlice (SliceSymbolic loAtom hiAtom) (SliceOf lenRepr lenRepr intAtom)
        _ -> throwTrace $ UnsupportedSlice slice constraint

translateSlice :: Overrides arch
               -> AS.Expr
               -> AS.Slice
               -> TypeConstraint
               -> Generator h s arch ret (Some (CCG.Atom s))
translateSlice ov e slice constraint = do
   Some atom <- translateExpr ov e
   translateSlice' ov atom slice constraint

sliceValidTests :: SomeSlice s -> Generator h s arch ret ()
sliceValidTests (SomeSlice range sliceOf@(SliceOf lenRepr wRepr atom)) = case range of
  SliceStatic _ _ -> return ()
  SliceSymbolic loAtom hiAtom -> do
    let lo = CCG.AtomExpr loAtom
    let hi = CCG.AtomExpr hiAtom
    let len = CCG.App (CCE.IntLit (CT.intValue lenRepr))
    let wlen = CCG.App (CCE.IntLit (CT.intValue wRepr))

    let tests = map CCG.App [ CCE.IntLe lo hi
                              -- lo <= hi
                            , CCE.IntLe (CCG.App (CCE.IntAdd (CCG.App $ CCE.IntSub hi lo) (CCG.App $ CCE.IntLit 1))) len
                              -- (hi - lo) + 1 <= len
                            , CCE.IntLe (CCG.App (CCE.IntAdd hi (CCG.App $ CCE.IntLit 1))) wlen
                              -- hi + 1 <= wlen
                            ]
    mapM_ mkTest tests
  where
    mkTest test = do
      atest <- CCG.mkAtom test
      assertAtom atest Nothing ("Slicing constraint failure: " <> (T.pack $ show test))


withValidSlice :: AS.Slice
               -> SliceRange s sliceLength atomLength
               -> SliceOf s sliceLength atomLength
               -> Generator h s arch ret (CCG.Atom s (CT.BVType sliceLength))
               -> Generator h s arch ret (Some (CCG.Atom s))
withValidSlice slice range sliceOf@(SliceOf lenRepr wRepr atom) f = do
    sliceValidTests (SomeSlice range sliceOf)
    --atest <- CCG.mkAtom test
    --assertAtom atest
    --CCG.assertExpr (CCG.AtomExpr atest) (CCG.App (CCE.TextLit $ "Slicing constraints: " <> (T.pack $ show slice) <> (T.pack $ show test)))
    atom <- f
    --atom2 <- CCG.mkAtom $ CCG.App (CCE.BaseIte (CT.BaseBVRepr lenRepr) (CCG.AtomExpr test) (CCG.AtomExpr atom) (getDefaultValue (CT.BVRepr lenRepr)))
    return (Some atom)

withValidSlice' :: AS.Slice
               -> SliceRange s sliceLength atomLength
               -> SliceOf s sliceLength atomLength
               -> Generator h s arch ret ()
               -> Generator h s arch ret ()
withValidSlice' slice range sliceOf f = do
    sliceValidTests (SomeSlice range sliceOf)
    --atest <- CCG.mkAtom test
    --assertAtom atest
    --CCG.assertExpr (CCG.AtomExpr atest) (CCG.App (CCE.TextLit $ "Slicing constraints" <> (T.pack $ show slice) <> (T.pack $ show test)))
    f

translateSlice' :: Overrides arch
                -> CCG.Atom s tp
                -> AS.Slice
                -> TypeConstraint
                -> Generator h s arch ret (Some (CCG.Atom s))
translateSlice' ov atom' slice constraint = do
  SomeSlice range sliceOf@(SliceOf lenRepr wRepr atom) <- getSomeSlice ov slice atom' constraint
  withValidSlice slice range sliceOf $ do
    case lenRepr `WT.testNatCases` wRepr of
      WT.NatCaseEQ ->
        -- when the slice covers the whole range we just return the whole atom
        return atom
      WT.NatCaseLT WT.LeqProof ->
        case range of
          SliceStatic loRepr hiRepr
             -- FIXME: This should be derivable
             | Just WT.LeqProof <- (loRepr `WT.addNat` lenRepr) `WT.testLeq` wRepr
             -> CCG.mkAtom (CCG.App (CCE.BVSelect loRepr lenRepr wRepr (CCG.AtomExpr atom)))
          SliceSymbolic loAtom hiAtom -> let
            loBV = CCG.App $ CCE.IntegerToBV wRepr (CCG.AtomExpr loAtom)
            hiBV = CCG.App $ CCE.IntegerToBV wRepr (CCG.AtomExpr hiAtom)
            top = CCG.App $ CCE.IntegerToBV wRepr $ CCG.App $
                   CCE.IntSub (CCG.App (CCE.IntLit (WT.intValue wRepr - 1))) (CCG.AtomExpr hiAtom)

            -- [ x_w-1 .. x_hi .. x_lo .. x_0](w) -- atom BV
            shift1 = CCG.App $ CCE.BVShl wRepr (CCG.AtomExpr atom) top
            -- [ x_hi .. x_lo .. x_0  ..  0 0](w)
            shift2 = CCG.App $ CCE.BVLshr wRepr shift1 top
            -- [ 0 0   .. x_hi .. x_lo .. x_0](w)
            shift3 = CCG.App $ CCE.BVLshr wRepr shift2 loBV
            -- [0 0 0 0 0 0 0 .. x_hi .. x_lo](w)
            in
            CCG.mkAtom $ CCG.App (CCE.BVTrunc lenRepr wRepr shift3)
            -- [x_hiRepr .. x_loAtom](len)
          _ -> throwTrace $ UnsupportedSlice slice constraint
      _ -> throwTrace $ UnsupportedSlice slice constraint

withStaticTest :: AS.Expr
               -> Generator h s arch ret a
               -> Generator h s arch ret a
               -> Generator h s arch ret a
               -> Generator h s arch ret a
withStaticTest test ifTrue ifFalse ifUnknown = do
  env <- MS.gets tsStaticEnv
  case exprToStatic env test of
    Just (StaticBool True) -> ifTrue
    Just (StaticBool False) -> ifFalse
    _ -> ifUnknown

-- | Translate the expression form of a conditional into a Crucible atom
translateIfExpr :: Overrides arch
                -> AS.Expr
                -> [(AS.Expr, AS.Expr)]
                -> AS.Expr
                -> Generator h s arch ret (Some (CCG.Atom s), ExtendedTypeData)
translateIfExpr ov orig clauses elseExpr =
  case clauses of
    [] -> X.throw (MalformedConditionalExpression orig)
    [(test, res)] ->
      withStaticTest test
        (translateExpr' ov res ConstraintNone)
        (translateExpr' ov elseExpr ConstraintNone) $ do
      Some testA <- translateExpr ov test
      (Some resA, extRes) <- translateExpr' ov res ConstraintNone
      (Some elseA, extElse) <- translateExpr' ov elseExpr (someTypeOfAtom resA)
      ext <- mergeExtensions extRes extElse
      Refl <- assertAtomType test CT.BoolRepr testA
      Refl <- assertAtomType res (CCG.typeOfAtom elseA) resA
      case CT.asBaseType (CCG.typeOfAtom elseA) of
        CT.NotBaseType -> X.throw (ExpectedBaseType orig (CCG.typeOfAtom elseA))
        CT.AsBaseType btr -> do
          atom <- CCG.mkAtom (CCG.App (CCE.BaseIte btr (CCG.AtomExpr testA) (CCG.AtomExpr resA) (CCG.AtomExpr elseA)))
          return (Some atom, ext)
    (test, res) : rest ->
      withStaticTest test
        (translateExpr' ov res ConstraintNone)
        (translateIfExpr ov orig rest elseExpr) $ do
      (Some trA, extRest) <- translateIfExpr ov orig rest elseExpr
      Some testA <- translateExpr ov test
      (Some resA, extRes) <- translateExpr' ov res (someTypeOfAtom trA)
      ext <- mergeExtensions extRes extRest
      Refl <- assertAtomType test CT.BoolRepr testA
      Refl <- assertAtomType res (CCG.typeOfAtom trA) resA
      case CT.asBaseType (CCG.typeOfAtom trA) of
        CT.NotBaseType -> X.throw (ExpectedBaseType orig (CCG.typeOfAtom trA))
        CT.AsBaseType btr -> do
          atom <- CCG.mkAtom (CCG.App (CCE.BaseIte btr (CCG.AtomExpr testA) (CCG.AtomExpr resA) (CCG.AtomExpr trA)))
          return (Some atom, ext)

maskToBV :: AS.Mask -> AS.BitVector
maskToBV mask = map maskBitToBit mask
  where
    maskBitToBit mb = case mb of
      AS.MaskBitSet -> True
      AS.MaskBitUnset -> False
      AS.MaskBitEither -> False

-- | Translate set element tests
--
-- Single element tests are translated into a simple equality test
--
-- Ranges are translated as a conjunction of inclusive tests. x IN [5..10] => 5 <= x && x <= 10
translateSetElementTest :: Overrides arch
                        -> AS.Expr
                        -> CCG.Atom s tp
                        -> AS.SetElement
                        -> Generator h s arch ret (CCG.Expr (ASLExt arch) s CT.BoolType)
translateSetElementTest ov e0 a0 elt =
  case elt of
    AS.SetEltSingle expr@(AS.ExprLitMask mask) -> do
      let maskExpr = AS.ExprLitBin (maskToBV mask)
      Some maskAtom <- translateExpr ov maskExpr
      Refl <- assertAtomType expr (CCG.typeOfAtom a0) maskAtom
      Some maskedBV <- bvBinOp CCE.BVOr (e0, a0) (maskExpr, maskAtom)
      Some testAtom <- applyBinOp eqOp (e0, a0) (AS.ExprBinOp AS.BinOpBitwiseOr e0 expr, maskedBV)
      Refl <- assertAtomType expr CT.BoolRepr testAtom
      return (CCG.AtomExpr testAtom)

    AS.SetEltSingle expr -> do
      Some atom1 <- translateExpr ov expr
      Refl <- assertAtomType expr (CCG.typeOfAtom a0) atom1
      Some atom2 <- applyBinOp eqOp (e0, a0) (expr, atom1)
      Refl <- assertAtomType expr CT.BoolRepr atom2
      return (CCG.AtomExpr atom2)
    AS.SetEltRange lo hi -> do
      Some loA <- translateExpr ov lo
      Some hiA <- translateExpr ov hi
      Refl <- assertAtomType lo (CCG.typeOfAtom a0) loA
      Refl <- assertAtomType hi (CCG.typeOfAtom a0) hiA
      Some loTest <- applyBinOp leOp (lo, loA) (e0, a0)
      Some hiTest <- applyBinOp leOp (e0, a0) (hi, hiA)
      Refl <- assertAtomType lo CT.BoolRepr loTest
      Refl <- assertAtomType hi CT.BoolRepr hiTest
      return (CCG.App (CCE.And (CCG.AtomExpr loTest) (CCG.AtomExpr hiTest)))



disjoin :: (CCE.IsSyntaxExtension ext)
        => CCG.Expr ext s CT.BoolType
        -> CCG.Expr ext s CT.BoolType
        -> CCG.Expr ext s CT.BoolType
disjoin p1 p2 = CCG.App (CCE.Or p1 p2)

translateBinaryOp :: forall h s ret arch
                   . Overrides arch
                  -> AS.BinOp
                  -> AS.Expr
                  -> AS.Expr
                  -> TypeConstraint
                  -> Generator h s arch ret (Some (CCG.Atom s))
translateBinaryOp ov op e1 e2 tc = do
  let (tc1, tc2) = constraintsOfArgs op tc
  (Some a1, _) <- translateExpr' ov e1 tc1
  (Some a2, _) <- translateExpr' ov e2 tc2
  let p1 = (e1, a1)
  let p2 = (e2, a2)
  env <- MS.gets tsStaticEnv
  case op of
    AS.BinOpPlusPlus -> X.throw (UnsupportedBinaryOperator op)
    AS.BinOpLogicalAnd -> logicalBinOp CCE.And p1 p2
    AS.BinOpLogicalOr -> logicalBinOp CCE.Or p1 p2
    AS.BinOpBitwiseOr -> bvBinOp CCE.BVOr p1 p2
    AS.BinOpBitwiseAnd -> bvBinOp CCE.BVAnd p1 p2
    AS.BinOpBitwiseXor -> bvBinOp CCE.BVXor p1 p2
    AS.BinOpEQ -> applyBinOp eqOp p1 p2
    AS.BinOpNEQ -> do
      Some atom <- applyBinOp eqOp p1 p2
      Refl <- assertAtomType (AS.ExprBinOp op e1 e2) CT.BoolRepr atom
      Some <$> CCG.mkAtom (CCG.App (CCE.Not (CCG.AtomExpr atom)))
    AS.BinOpGT -> do
      -- NOTE: We always use unsigned comparison for bitvectors - is that correct?
      Some atom <- applyBinOp leOp p1 p2
      Refl <- assertAtomType (AS.ExprBinOp op e1 e2) CT.BoolRepr atom
      Some <$> CCG.mkAtom (CCG.App (CCE.Not (CCG.AtomExpr atom)))
    AS.BinOpLTEQ -> applyBinOp leOp p1 p2
    AS.BinOpLT -> applyBinOp ltOp p1 p2
    AS.BinOpGTEQ -> do
      Some atom <- applyBinOp ltOp p1 p2
      Refl <- assertAtomType (AS.ExprBinOp op e1 e2) CT.BoolRepr atom
      Some <$> CCG.mkAtom (CCG.App (CCE.Not (CCG.AtomExpr atom)))
    AS.BinOpAdd -> applyBinOp addOp p1 p2
    AS.BinOpSub -> applyBinOp subOp p1 p2
    AS.BinOpMul -> applyBinOp mulOp p1 p2
    AS.BinOpMod -> applyBinOp modOp p1 p2
    --FIXME: REM is only used once in mapvpmw, is it just mod there?
    AS.BinOpRem -> applyBinOp modOp p1 p2
    AS.BinOpDiv -> applyBinOp divOp p1 p2
    AS.BinOpShiftLeft -> bvBinOp CCE.BVShl p1 p2
    AS.BinOpShiftRight -> bvBinOp CCE.BVLshr p1 p2
    -- FIXME: What is the difference between BinOpDiv and BinOpDivide?
    AS.BinOpConcat -> do
      BVRepr n1 <- getAtomBVRepr a1
      BVRepr n2 <- getAtomBVRepr a2
      Just n1PosProof <- return $ WT.isPosNat n1
      WT.LeqProof <- return $ WT.leqAdd n1PosProof n2
      Some <$> CCG.mkAtom (CCG.App (CCE.BVConcat n1 n2 (CCG.AtomExpr a1) (CCG.AtomExpr a2)))
    AS.BinOpPow
      | Just (StaticInt 2) <- exprToStatic env e1 -> do
        Refl <- assertAtomType e2 CT.IntegerRepr a2
        let nr = WT.knownNat @128
        let shift = CCG.App $ CCE.IntegerToBV nr (CCG.AtomExpr a2)
        let base = CCG.App $ CCE.BVLit nr 1
        let shifted = CCG.App $ (CCE.BVShl nr base shift)
        Some <$> CCG.mkAtom (CCG.App (CCE.BvToInteger nr shifted))

    _ -> X.throw (UnsupportedBinaryOperator op)

-- Linear Arithmetic operators

addOp :: BinaryOperatorBundle ext s 'SameK
addOp = BinaryOperatorBundle CCE.BVAdd CCE.NatAdd CCE.IntAdd

subOp :: BinaryOperatorBundle ext s 'SameK
subOp = BinaryOperatorBundle CCE.BVSub CCE.NatSub CCE.IntSub

-- Nonlinear Arithmetic operators

-- For now we hide these behind uninterpreted functions until we have a better story
-- for when we actually need their theories

-- mulOp :: BinaryOperatorBundle ext s 'SameK
-- mulOp = BinaryOperatorBundle CCE.BVMul CCE.NatMul CCE.IntMul

-- modOp :: BinaryOperatorBundle ext s 'SameK
-- modOp = BinaryOperatorBundle (error "BV mod not supported") CCE.NatMod CCE.IntMod

-- divOp :: BinaryOperatorBundle ext s 'SameK
-- divOp = BinaryOperatorBundle (error "BV div not supported") CCE.NatDiv CCE.IntDiv

mkBVUF :: (1 WT.<= w)
     => T.Text
     -> WT.NatRepr w
     -> CCG.Expr (ASLExt arch) s (CT.BVType w)
     -> CCG.Expr (ASLExt arch) s (CT.BVType w)
     -> CCE.App (ASLExt arch) (CCG.Expr (ASLExt arch) s) (CT.BVType w)
mkBVUF nm natRepr arg1 arg2 = do
  let brepr = CT.BaseBVRepr natRepr
  let repr = CT.BVRepr natRepr
  let uf = UF nm brepr (Ctx.empty Ctx.:> repr Ctx.:> repr) (Ctx.empty Ctx.:> arg1 Ctx.:> arg2)
  CCE.ExtensionApp uf

mkUF :: T.Text
     -> CT.TypeRepr tp
     -> CCG.Expr (ASLExt arch) s tp
     -> CCG.Expr (ASLExt arch) s tp
     -> CCE.App (ASLExt arch) (CCG.Expr (ASLExt arch) s) tp
mkUF nm repr arg1 arg2 = case CT.asBaseType repr of
    CT.AsBaseType brepr -> do
      let uf = UF nm brepr (Ctx.empty Ctx.:> repr Ctx.:> repr) (Ctx.empty Ctx.:> arg1 Ctx.:> arg2)
      CCE.ExtensionApp uf
    _ -> error $ "Illegal crucible type: " <> show repr

mulOp :: BinaryOperatorBundle (ASLExt arch) s 'SameK
mulOp = BinaryOperatorBundle (mkBVUF "BVMul") (mkUF "NatMul" CT.NatRepr) (mkUF "IntMul" CT.IntegerRepr)

modOp :: BinaryOperatorBundle (ASLExt arch) s 'SameK
modOp = BinaryOperatorBundle (error "BV mod not supported") (mkUF "NatMod" CT.NatRepr) (mkUF "IntMod" CT.IntegerRepr)

divOp :: BinaryOperatorBundle (ASLExt arch) s 'SameK
divOp = BinaryOperatorBundle (error "BV div not supported") (mkUF "NatDiv" CT.NatRepr) (mkUF "IntDiv" CT.IntegerRepr)

-- Comparison operators

eqOp :: BinaryOperatorBundle ext s 'BoolK
eqOp = BinaryOperatorBundle CCE.BVEq CCE.NatEq CCE.IntEq

leOp :: BinaryOperatorBundle ext s 'BoolK
leOp = BinaryOperatorBundle CCE.BVUle CCE.NatLe CCE.IntLe

ltOp :: BinaryOperatorBundle ext s 'BoolK
ltOp = BinaryOperatorBundle CCE.BVUlt CCE.NatLt CCE.IntLt


data ReturnK = BoolK
             -- ^ Tag used for comparison operations, which always return BoolType
             | SameK
             -- ^ Tag used for other operations, which preserve the type

type family BinaryOperatorReturn (r :: ReturnK) (tp :: CT.CrucibleType) where
  BinaryOperatorReturn 'BoolK tp = CT.BoolType
  BinaryOperatorReturn 'SameK tp = tp

data BinaryOperatorBundle ext s (rtp :: ReturnK) =
  BinaryOperatorBundle { obBV :: forall n . (1 WT.<= n) => WT.NatRepr n -> CCG.Expr ext s (CT.BVType n) -> CCG.Expr ext s (CT.BVType n) -> CCE.App ext (CCG.Expr ext s) (BinaryOperatorReturn rtp (CT.BVType n))
                       , obNat :: CCG.Expr ext s CT.NatType -> CCG.Expr ext s CT.NatType -> CCE.App ext (CCG.Expr ext s) (BinaryOperatorReturn rtp CT.NatType)
                       , obInt :: CCG.Expr ext s CT.IntegerType -> CCG.Expr ext s CT.IntegerType -> CCE.App ext (CCG.Expr ext s) (BinaryOperatorReturn rtp CT.IntegerType)
                       }



-- | Apply a binary operator to two operands, performing the necessary type checks
applyBinOp :: (ext ~ ASLExt arch)
           => BinaryOperatorBundle ext s rtp
           -> (AS.Expr, CCG.Atom s tp1)
           -> (AS.Expr, CCG.Atom s tp2)
           -> Generator h s arch ret (Some (CCG.Atom s))
applyBinOp bundle (e1, a1) (e2, a2) =
  case CCG.typeOfAtom a1 of
    CT.BVRepr nr -> do
      case CCG.typeOfAtom a2 of
        CT.IntegerRepr -> do
            let a2' = CCG.App (CCE.IntegerToBV nr (CCG.AtomExpr a2))
            Some <$> CCG.mkAtom (CCG.App (obBV bundle nr (CCG.AtomExpr a1) a2'))
        _ -> do
          Refl <- assertAtomType e2 (CT.BVRepr nr) a2
          Some <$> CCG.mkAtom (CCG.App (obBV bundle nr (CCG.AtomExpr a1) (CCG.AtomExpr a2)))
    CT.NatRepr -> do
      Refl <- assertAtomType e2 CT.NatRepr a2
      Some <$> CCG.mkAtom (CCG.App (obNat bundle (CCG.AtomExpr a1) (CCG.AtomExpr a2)))
    CT.IntegerRepr -> do
      case CCG.typeOfAtom a2 of
        CT.BVRepr nr -> do
          let a1' = CCG.App (CCE.IntegerToBV nr (CCG.AtomExpr a1))
          Some <$> CCG.mkAtom (CCG.App (obBV bundle nr a1' (CCG.AtomExpr a2)))
        _ -> do
          Refl <- assertAtomType e2 CT.IntegerRepr a2
          Some <$> CCG.mkAtom (CCG.App (obInt bundle (CCG.AtomExpr a1) (CCG.AtomExpr a2)))
    CT.BoolRepr -> do
      case CCG.typeOfAtom a2 of
        CT.BoolRepr -> do
          let nr = WT.knownNat @1
          let a1' = CCG.App $ CCE.BoolToBV nr (CCG.AtomExpr a1)
          let a2' = CCG.App $ CCE.BoolToBV nr (CCG.AtomExpr a2)
          Some <$> CCG.mkAtom (CCG.App (obBV bundle nr a1' a2'))

    _ -> X.throw (UnsupportedComparisonType e1 (CCG.typeOfAtom a1))

bvBinOp :: (ext ~ ASLExt arch)
        => (forall n . (1 WT.<= n) => WT.NatRepr n -> CCG.Expr ext s (CT.BVType n) -> CCG.Expr ext s (CT.BVType n) -> CCE.App ext (CCG.Expr ext s) (CT.BVType n))
        -> (AS.Expr, CCG.Atom s tp1)
        -> (AS.Expr, CCG.Atom s tp2)
        -> Generator h s arch ret (Some (CCG.Atom s))
bvBinOp con (e1, a1) (e2, a2) =
  case CCG.typeOfAtom a1 of
    CT.BVRepr nr -> do
      case CCG.typeOfAtom a2 of
        CT.IntegerRepr -> do
          let a2' = CCG.App (CCE.IntegerToBV nr (CCG.AtomExpr a2))
          Some <$> CCG.mkAtom (CCG.App (con nr (CCG.AtomExpr a1) a2'))
        _ -> do
          Refl <- assertAtomType e2 (CT.BVRepr nr) a2
          Some <$> CCG.mkAtom (CCG.App (con nr (CCG.AtomExpr a1) (CCG.AtomExpr a2)))
    CT.IntegerRepr -> do
      case CCG.typeOfAtom a2 of
        CT.BVRepr nr -> do
          let a1' = CCG.App (CCE.IntegerToBV nr (CCG.AtomExpr a1))
          Some <$> CCG.mkAtom (CCG.App (con nr a1' (CCG.AtomExpr a2)))
        CT.IntegerRepr -> do
          let bvrepr = WT.knownNat @64
          let a1' = CCG.App $ CCE.IntegerToBV bvrepr (CCG.AtomExpr a1)
          let a2' = CCG.App $ CCE.IntegerToBV bvrepr (CCG.AtomExpr a2)
          Some <$> CCG.mkAtom (CCG.App (CCE.BvToInteger bvrepr (CCG.App (con bvrepr a1' a2'))))
        _ -> throwTrace (ExpectedBVType e1 (CCG.typeOfAtom a2))
    _ -> throwTrace $ (ExpectedBVType e1 (CCG.typeOfAtom a1))

logicalBinOp :: (ext ~ ASLExt arch)
             => (CCG.Expr ext s CT.BoolType -> CCG.Expr ext s CT.BoolType -> CCE.App ext (CCG.Expr ext s) CT.BoolType)
             -> (AS.Expr, CCG.Atom s tp1)
             -> (AS.Expr, CCG.Atom s tp2)
             -> Generator h s arch ret (Some (CCG.Atom s))
logicalBinOp con (e1, a1) (e2, a2) = do
  Refl <- assertAtomType e1 CT.BoolRepr a1
  Refl <- assertAtomType e2 CT.BoolRepr a2
  Some <$> CCG.mkAtom (CCG.App (con (CCG.AtomExpr a1) (CCG.AtomExpr a2)))

translateUnaryOp :: Overrides arch
                 -> AS.UnOp
                 -> AS.Expr
                 -> Generator h s arch ret (Some (CCG.Atom s))
translateUnaryOp ov op expr = do
  Some atom <- translateExpr ov expr
  case op of
    AS.UnOpNot -> do
      case CCG.typeOfAtom atom of
        CT.BVRepr nr -> do
          Some <$> CCG.mkAtom (CCG.App (CCE.BVNot nr (CCG.AtomExpr atom)))
        CT.BoolRepr -> do
          Some <$> CCG.mkAtom (CCG.App (CCE.Not (CCG.AtomExpr atom)))
        _ -> throwTrace $ UnexpectedExprType (Just expr) (CCG.typeOfAtom atom) (CT.BoolRepr)
    AS.UnOpNeg ->
      case CCG.typeOfAtom atom of
        CT.IntegerRepr -> do
          Some <$> CCG.mkAtom (CCG.App (CCE.IntNeg (CCG.AtomExpr atom)))
        _ -> throwTrace $ UnexpectedExprType (Just expr) (CCG.typeOfAtom atom) (CT.BoolRepr)

translateKnownVar :: (Some CT.TypeRepr)
                    -> AS.Identifier
                    -> Some (CCG.Atom s)
                    -> Generator h s arch ret ()
translateKnownVar (Some expected) ident (Some atom) = do
  Refl <- assertAtomType' expected atom
  locals <- MS.gets tsVarRefs
  when (Map.member ident locals) $ do
    X.throw (LocalAlreadyDefined ident)
  reg <- CCG.newReg (CCG.AtomExpr atom)
  MS.modify' $ \s -> s { tsVarRefs = Map.insert ident (Some reg) locals }

data BVAtomPair s where
  BVAtomPair :: (tp ~ CT.BVType w, 1 WT.<= w)
             => WT.NatRepr w
             -> CCG.Atom s tp
             -> CCG.Atom s tp
             -> BVAtomPair s

-- zero-extend one bitvector to match the other's size
matchBVSizes :: CCG.Atom s tp
             -> CCG.Atom s tp'
             -> Generator h s arch ret (BVAtomPair s)
matchBVSizes atom1 atom2 = do
  BVRepr wRepr1 <- getAtomBVRepr atom1
  BVRepr wRepr2 <- getAtomBVRepr atom2
  case wRepr1 `WT.testNatCases` wRepr2 of
    WT.NatCaseEQ ->
      return $ BVAtomPair wRepr1 atom1 atom2
    WT.NatCaseLT WT.LeqProof -> do
      atom1' <- CCG.mkAtom (CCG.App (CCE.BVZext wRepr2 wRepr1 (CCG.AtomExpr atom1)))
      return $ BVAtomPair wRepr2 atom1' atom2
    WT.NatCaseGT WT.LeqProof -> do
      atom2' <- CCG.mkAtom (CCG.App (CCE.BVZext wRepr1 wRepr2 (CCG.AtomExpr atom2)))
      return $ BVAtomPair wRepr1 atom1 atom2'

zextBVAtom :: 1 WT.<= w
           => WT.NatRepr w
           -> CCG.Atom s tp
           -> Generator h s arch ret (CCG.Atom s (CT.BVType w))
zextBVAtom repr atom = do
  BVRepr atomRepr <- getAtomBVRepr atom
  case atomRepr `WT.testNatCases` repr of
    WT.NatCaseEQ ->
      return atom
    WT.NatCaseLT WT.LeqProof -> do
      atom' <- CCG.mkAtom (CCG.App (CCE.BVZext repr atomRepr (CCG.AtomExpr atom)))
      return atom'
    _ -> throwTrace $ UnexpectedBitvectorLength (CT.BVRepr atomRepr) (CT.BVRepr repr)

getConstraintHint :: TypeConstraint -> Maybe ConstraintHint
getConstraintHint constraint = case constraint of
  ConstraintSingle (CT.BVRepr nr) -> Just $ HintMaxBVSize nr
  ConstraintHint hint -> Just $ hint
  _ -> Nothing

relaxConstraint :: TypeConstraint -> TypeConstraint
relaxConstraint constraint = case constraint of
  ConstraintSingle (CT.BVRepr nr) -> ConstraintHint (HintMaxBVSize nr)
  _ -> constraint

-- Overrides that dispatch to ambiguous function overloads based on the argument type
overloadedDispatchOverrides :: AS.Expr -> TypeConstraint -> Maybe (Generator h s arch ret (Some (CCG.Atom s), ExtendedTypeData))
overloadedDispatchOverrides e tc = case e of
  AS.ExprCall (AS.QualifiedIdentifier q "Align") args@[e1, e2] -> Just $ do
    Some atom1 <- translateExpr overrides e1
    nm <- case CCG.typeOfAtom atom1 of
      CT.IntegerRepr ->
        return $ "Alignintegerinteger"
      CT.BVRepr _ ->
        return $ "AlignbitsNinteger"
    translateExpr' overrides (AS.ExprCall (AS.QualifiedIdentifier q nm) args) ConstraintNone
  AS.ExprCall (AS.QualifiedIdentifier q fun) args@[e1, e2]
    | fun `elem` ["Min","Max"]
    -> Just $ do
    Some atom1 <- translateExpr overrides e1
    Some atom2 <- translateExpr overrides e2
    Refl <- assertAtomType e1 CT.IntegerRepr atom1
    Refl <- assertAtomType e2 CT.IntegerRepr atom2
    translateExpr' overrides (AS.ExprCall (AS.QualifiedIdentifier q (fun <> "integerinteger")) args) tc
  _ ->  mkFaultOv "IsExternalAbort" <|>
        mkFaultOv "IsAsyncAbort" <|>
        mkFaultOv "IsSErrorInterrupt" <|>
        mkFaultOv "IsExternalSyncAbort"
  where
    mkFaultOv nm =
      case e of
        AS.ExprCall (AS.QualifiedIdentifier q nm') [arg] | nm == nm' -> Just $ do
          (_, ext) <- translateExpr' overrides arg ConstraintNone
          ov <- case ext of
            TypeStruct _ -> return $ "FaultRecord"
            _ -> return $ "Fault"
          translateExpr' overrides (AS.ExprCall (AS.QualifiedIdentifier q (nm <> ov)) [arg]) tc
        _ -> Nothing

-- Eagerly throw exceptions when it is clear that the instruction is not supported
checkSupportedExpr :: AS.Expr -> AS.Expr
checkSupportedExpr e = case e of
  _ -> e

checkSupportedStmt :: AS.Stmt -> AS.Stmt
checkSupportedStmt s = case s of
  AS.StmtIf [(AS.ExprUnOp AS.UnOpNot (AS.ExprCall (AS.QualifiedIdentifier _ "HaveSVE") []), _)] _ ->
    X.throw $ InstructionUnsupported
  _ -> s

exprToLVal :: AS.Expr -> AS.LValExpr
exprToLVal e = case e of
  AS.ExprVarRef qident -> AS.LValVarRef qident
  AS.ExprIndex e slices -> AS.LValArrayIndex (exprToLVal e) slices
  AS.ExprSlice e slices -> AS.LValSliceOf (exprToLVal e) slices
  AS.ExprMembers e [mem] -> AS.LValMember (exprToLVal e) mem
  AS.ExprTuple es -> AS.LValTuple (map exprToLVal es)
  _ -> X.throw $ TranslationError $ "Invalid inline for expr:" ++ show e

applyStaticCase :: (AS.Expr, AS.CasePattern)
                 -> Generator h s arch ret ()
applyStaticCase (AS.ExprVarRef (AS.QualifiedIdentifier _ nm), c) = do
  sv <- case c of
    AS.CasePatternInt i -> return $ StaticInt i
    AS.CasePatternBin bv -> return $ StaticBV bv
    AS.CasePatternIdentifier "FALSE" -> return $ StaticBool False
    AS.CasePatternIdentifier "TRUE" -> return $ StaticBool True
    _ -> throwTrace $ TranslationError $ "Invalid case pattern for static case: " ++ show c
  mapStaticEnv (insertStaticEnv nm sv)
applyStaticCase x = throwTrace $ TranslationError $ "Unexpected static case form" ++ show x

overrides :: forall arch . Overrides arch
overrides = Overrides {..}
  where overrideStmt :: forall h s ret . AS.Stmt -> Maybe (Generator h s arch ret ())

        overrideStmt s = case checkSupportedStmt s of
          AS.StmtCall (AS.QualifiedIdentifier _ fun@"GETTERSETTER")
            args'@(AS.ExprSlice (AS.ExprVarRef getter) slices : AS.ExprVarRef setter : value : args) -> Just $ do
              Some atom <- translateExpr overrides (AS.ExprCall getter args)
              BVRepr widthRepr <- getAtomBVRepr atom
              let width = WT.intValue widthRepr
              let old = "__oldGetterValue_" <> (T.pack $ show width)
              let mask = "__maskedGetterValue_" <> (T.pack $ show width)
              let stmts = [ AS.StmtAssign (AS.LValVarRef $ mkIdent old)
                             (AS.ExprCall getter args)
                          ,  AS.StmtAssign (AS.LValVarRef $ mkIdent mask)
                             (AS.ExprCall (mkIdent "Ones") [AS.ExprLitInt width])
                          , AS.StmtAssign (AS.LValSliceOf (AS.LValVarRef $ mkIdent mask) slices)
                             value
                          , AS.StmtCall setter (AS.ExprBinOp AS.BinOpBitwiseAnd (mkVar mask) (mkVar old) : args)
                          ]
              translateStatement overrides (letInStmt [old, mask] stmts)

          -- Special construct to mimic a local let binding environment by
          -- discarding any new variable declarations at the end of the block.

          _ | Just ([], stmts) <- unletInStmt s -> Just $ do
            vars <- MS.gets tsVarRefs
            mapM_ (translateStatement overrides) stmts
            MS.modify' $ \s -> s { tsVarRefs = vars }

          _ | Just (unvars, stmts) <- unletInStmt s -> Just $ do
            mapM_ (translateStatement overrides) stmts
            MS.modify' $ \s -> s { tsVarRefs = foldr Map.delete (tsVarRefs s) unvars }

          -- Special case construct that injects information into the static environment
          _ | Just (vars, cases) <- unstaticEnvironmentStmt s -> Just $ do

            env <- MS.gets tsStaticEnv
            forM_ cases $ \(svs, stmts) -> do
                      mapM_ (\(nm, sv) -> mapStaticEnv (insertStaticEnv nm sv)) (zip vars svs)
                      mapM_ (translateStatement overrides) stmts
                      MS.modify' $ \s -> s { tsStaticEnv = env }

          AS.StmtCall (AS.QualifiedIdentifier _ "ALUExceptionReturn") [_] -> Just $ do
            raiseException
          -- FIXME: write pc
          AS.StmtCall (AS.QualifiedIdentifier _ "ALUWritePC") [result] -> Just $ do
            return ()
          AS.StmtCall (AS.QualifiedIdentifier _ "TakeHypTrapException") [x] -> Just $ do
            return ()
          AS.StmtVarDeclInit (ident,ty) (AS.ExprCall (AS.QualifiedIdentifier _ "Zeros") []) -> Just $ do
            retT <- translateType ty
            if | Some (CT.BVRepr valWidth) <- retT
               , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` valWidth
                 -> do
                  atom <- CCG.mkAtom (CCG.App (CCE.BVLit valWidth 0))
                  translateKnownVar retT ident (Some atom)
               | otherwise -> error $ "Invalid return type for Zeros():" <> show ty
          -- FIXME: This should raise an exception
          AS.StmtCall (AS.QualifiedIdentifier _ "SEE") [x] -> Just $ do
            return ()
          AS.StmtCall (AS.QualifiedIdentifier _ "EndOfInstruction") [] -> Just $ do
            return ()
          AS.StmtCall (AS.QualifiedIdentifier _ "TraceSynchronizationBarrier") [] -> Just $ do
            return ()
          AS.StmtCall (AS.QualifiedIdentifier _ "print") [x] -> Just $ do
            return ()
          AS.StmtCall (AS.QualifiedIdentifier _ "__abort") [] -> Just $ do
            return ()
          AS.StmtCall (AS.QualifiedIdentifier _ "putchar") [x] -> Just $ do
            return ()

          -- FIXME: Memory model

          AS.StmtCall (AS.QualifiedIdentifier _ "__WriteRAM") [_, szExpr, _, addr, value] -> Just $ do
            return ()

          _ -> Nothing

        mkIdent nm = AS.QualifiedIdentifier AS.ArchQualAny nm
        mkVar nm = AS.ExprVarRef (mkIdent nm)
        nmOf (AS.ExprVarRef qnm) = qnm
        idOf (AS.ExprVarRef (AS.QualifiedIdentifier _ nm)) = nm

        mkAtom e = do
          atom <- CCG.mkAtom e
          return (Some atom, TypeBasic)

        isSlice (AS.ExprSlice _ _) = True
        isSlice _ = False


        list1ToMaybe [x] = Just (Just x)
        list1ToMaybe [] = Just Nothing
        list1ToMaybe _ = Nothing

        getLength :: TypeConstraint
                  -> Maybe AS.Expr
                  -> Generator h s arch ret (Some BVRepr)
        getLength ty mexpr = do
          env <- MS.gets tsStaticEnv
          case () of
            _ | Just e <- mexpr
              , Just (StaticInt i) <- exprToStatic env e
              , Just (Some repr) <- WT.someNat i
              , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` repr ->
                return $ Some $ BVRepr repr
            _ | ConstraintSingle (CT.BVRepr nr) <- ty ->
                return $ Some $ BVRepr $ nr
            _ | ConstraintHint (HintMaxBVSize nr) <- ty ->
                return $ Some $ BVRepr $ nr
            _ -> throwTrace $ CannotDetermineBVLength mexpr ty

        getSymbolicBVLength :: AS.Expr
                            -> Maybe (Generator h s arch ret (CCG.Atom s CT.IntegerType))
        getSymbolicBVLength e = case e of
            AS.ExprCall (AS.QualifiedIdentifier _ nm) [e]
              | nm == "Zeros" || nm == "Ones" -> Just $ do
                (Some argAtom) <- translateExpr overrides e
                Refl <- assertAtomType e CT.IntegerRepr argAtom
                CCG.mkAtom $ CCG.AtomExpr argAtom
            AS.ExprLitBin bits -> Just $ do
              CCG.mkAtom $ CCG.App $ CCE.IntLit $ fromIntegral $ length bits
            AS.ExprSlice _ [slice] -> Just $ do
              (loAtom, hiAtom) <- getSymbolicSliceRange overrides slice
              CCG.mkAtom $ CCG.App $ CCE.IntSub (CCG.AtomExpr hiAtom) (CCG.AtomExpr loAtom)
            AS.ExprVarRef (AS.QualifiedIdentifier _ ident) -> Just $ do
              mTy <- lookupVarType ident
              case mTy of
                Just (Some (CT.BVRepr wRepr)) ->
                  CCG.mkAtom $ CCG.App $ CCE.IntLit $ WT.intValue wRepr
                _ -> throwTrace $ UnboundName ident
            AS.ExprBinOp AS.BinOpConcat e1 e2
              | Just f1 <- getSymbolicBVLength e1
              , Just f2 <- getSymbolicBVLength e2 -> Just $ f1 >>= \len1 -> f2 >>= \len2 ->
                CCG.mkAtom $ CCG.App $ CCE.IntAdd (CCG.AtomExpr len1) (CCG.AtomExpr len2)
            e -> Nothing

        overrideExpr :: forall h s ret . AS.Expr -> TypeConstraint -> Maybe (Generator h s arch ret (Some (CCG.Atom s), ExtendedTypeData))
        overrideExpr e ty = case checkSupportedExpr e of
          AS.ExprBinOp AS.BinOpEQ e mask@(AS.ExprLitMask _) -> Just $ do
            translateExpr' overrides (AS.ExprInSet e [AS.SetEltSingle mask]) ty
          AS.ExprBinOp AS.BinOpNEQ e mask@(AS.ExprLitMask _) -> Just $ do
            translateExpr' overrides (AS.ExprUnOp AS.UnOpNot (AS.ExprInSet e [AS.SetEltSingle mask])) ty
          AS.ExprBinOp bop arg1@(AS.ExprSlice _ _) arg2
            | bop == AS.BinOpEQ || bop == AS.BinOpNEQ -> Just $ do
                (Some atom1', _) <- translateExpr' overrides arg1 (ConstraintHint HintAnyBVSize)
                BVRepr atom1sz <- getAtomBVRepr atom1'
                (Some atom2', _) <- translateExpr' overrides arg2 (ConstraintHint $ HintMaxBVSize atom1sz)
                BVAtomPair _ atom1 atom2 <- matchBVSizes atom1' atom2'
                (Some result') <- applyBinOp eqOp (arg1, atom1) (arg2, atom2)
                Refl <- assertAtomType' CT.BoolRepr result'
                result <- case bop of
                  AS.BinOpEQ -> return result'
                  AS.BinOpNEQ -> CCG.mkAtom (CCG.App (CCE.Not (CCG.AtomExpr result')))
                return (Some result, TypeBasic)
          AS.ExprBinOp AS.BinOpConcat expr1 expr2
            | Just hint <- getConstraintHint ty
            , (mLen1 :: Maybe (Generator h s arch ret (CCG.Atom s CT.IntegerType))) <- getSymbolicBVLength expr1
            , (mLen2 :: Maybe (Generator h s arch ret (CCG.Atom s CT.IntegerType))) <- getSymbolicBVLength expr1
            , isJust mLen1 || isJust mLen2 -> Just $ do
                (Some atom1', _) <- translateExpr' overrides expr1 (relaxConstraint ty)
                (Some atom2', _) <- translateExpr' overrides expr2 (relaxConstraint ty)
                BVAtomPair wRepr atom1 atom2 <- case hint of
                  HintMaxBVSize wRepr -> do
                    atom1 <- zextBVAtom wRepr atom1'
                    atom2 <- zextBVAtom wRepr atom2'
                    return $ BVAtomPair wRepr atom1 atom2
                  HintAnyBVSize -> do
                    matchBVSizes atom1' atom2'

                shift <- case (mLen1, mLen2) of
                  (Just f1, _) -> f1 >>= \len1 ->
                    return $ CCG.App $ CCE.IntegerToBV wRepr $ CCG.App $
                      CCE.IntSub (CCG.App (CCE.IntLit $ WT.intValue wRepr)) (CCG.AtomExpr len1)
                  (_, Just f2) -> f2 >>= \len2 ->
                    return $ CCG.App $ CCE.IntegerToBV wRepr $ (CCG.AtomExpr len2)
                let atom1Shifted = CCG.App $ CCE.BVShl wRepr (CCG.AtomExpr atom1) shift

                result <- CCG.mkAtom $ CCG.App $ CCE.BVOr wRepr atom1Shifted (CCG.AtomExpr atom2)
                return (Some result, TypeBasic)
          -- FIXME: this is should be conditional on unsigned.
          -- FIXME: we need another constraint hint for signed extensions
          AS.ExprCall (AS.QualifiedIdentifier _ "Int") [argExpr, isUnsigned] -> Just $ do
            (Some atom, _) <- translateExpr' overrides argExpr (ConstraintHint $ HintAnyBVSize)
            BVRepr nr <- getAtomBVRepr atom
            mkAtom (CCG.App (CCE.BvToInteger nr (CCG.AtomExpr atom)))
          AS.ExprCall (AS.QualifiedIdentifier _ "UInt") [argExpr] -> Just $ do
            (Some atom, _) <- translateExpr' overrides argExpr (ConstraintHint $ HintAnyBVSize)
            BVRepr nr <- getAtomBVRepr atom
            mkAtom (CCG.App (CCE.BvToInteger nr (CCG.AtomExpr atom)))
          -- FIXME: BvToInteger isn't right here, because it's unsigned. We need a
          -- signed version.
          AS.ExprCall (AS.QualifiedIdentifier _ "SInt") [argExpr] -> Just $ do
            Some atom <- translateExpr overrides argExpr
            BVRepr nr <- getAtomBVRepr atom
            mkAtom (CCG.App (CCE.BvToInteger nr (CCG.AtomExpr atom)))
          AS.ExprCall (AS.QualifiedIdentifier _ "IsZero") [argExpr] -> Just $ do
            (Some atom, _) <- translateExpr' overrides argExpr (ConstraintHint $ HintAnyBVSize)
            BVRepr nr <- getAtomBVRepr atom
            mkAtom (CCG.App (CCE.BVEq nr (CCG.AtomExpr atom) (CCG.App (CCE.BVLit nr 0))))
          AS.ExprCall (AS.QualifiedIdentifier _ "IsOnes") [argExpr] -> Just $ do
            argExpr' <- case argExpr of
              AS.ExprSlice e slices ->
                return $ AS.ExprSlice (AS.ExprUnOp AS.UnOpNot e) slices
              _ -> return $ AS.ExprUnOp AS.UnOpNot argExpr
            translateExpr' overrides
              (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "IsZero") [argExpr'])
              ConstraintNone
          AS.ExprCall (AS.QualifiedIdentifier _ fun) args@(val : rest)
            | fun == "ZeroExtend" || fun == "SignExtend"
            , Just mexpr <- list1ToMaybe rest -> Just $ do
            Some (BVRepr targetWidth) <- getLength ty mexpr
            (Some valAtom, _) <- case fun of
              -- Since slicing implicitly zero-extends to the target width, this is safe
              "ZeroExtend" -> translateExpr' overrides val (ConstraintHint (HintMaxBVSize targetWidth))
              -- FIXME: We need another constraint hint that does signed extensions
              "SignExtend" -> translateExpr' overrides val (ConstraintHint (HintMaxBVSize targetWidth))
            BVRepr valWidth <- getAtomBVRepr valAtom
            case valWidth `WT.testNatCases` targetWidth of
              WT.NatCaseEQ ->
                return $ (Some valAtom, TypeBasic)
              WT.NatCaseLT WT.LeqProof -> do
                atom <- case fun of
                  "ZeroExtend" -> CCG.mkAtom (CCG.App (CCE.BVZext targetWidth valWidth (CCG.AtomExpr valAtom)))
                  "SignExtend" -> CCG.mkAtom (CCG.App (CCE.BVSext targetWidth valWidth (CCG.AtomExpr valAtom)))
                return $ (Some atom, TypeBasic)
              _ -> throwTrace $ ExpectedBVSizeLeq valWidth targetWidth
          AS.ExprCall (AS.QualifiedIdentifier _ fun) args
            | fun == "Zeros" || fun == "Ones"
            , Just mexpr <- list1ToMaybe args -> Just $ do
            env <- MS.gets tsStaticEnv
            Some (BVRepr targetWidth) <- getLength ty mexpr
            zeros <- CCG.mkAtom (CCG.App (CCE.BVLit targetWidth 0))
            case fun of
              "Zeros" -> return (Some zeros, TypeBasic)
              "Ones" -> mkAtom (CCG.App $ CCE.BVNot targetWidth (CCG.AtomExpr zeros))

          AS.ExprCall (AS.QualifiedIdentifier _ "Replicate") args@[AS.ExprLitBin [False], repe] -> Just $ do
            translateExpr' overrides
             (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "Zeros") [repe])
             ty
          -- FIXME: Needs an actual implementation
          AS.ExprCall (AS.QualifiedIdentifier _ fun@"Replicate") args@[bve, repe] -> Just $ do
            env <- MS.gets tsStaticEnv
            Some bvatom <- translateExpr overrides bve
            case (exprToStatic env repe, CCG.typeOfAtom bvatom) of
              (Just (StaticInt width), CT.BVRepr nr) |
                  Just (Some valWidth) <- WT.someNat (width * WT.intValue nr)
                , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` valWidth -> do
                  mkAtom (CCG.App (CCE.BVLit valWidth 0))
              (Nothing, _) -> throwTrace $ RequiredConcreteValue fun repe
              _ -> throwTrace $ InvalidOverloadedFunctionCall fun args

          --FIXME: determine actual rounding here
          AS.ExprCall (AS.QualifiedIdentifier _ fun@"RoundTowardsZero") args@[e] -> Just $ do
            case e of
              (AS.ExprBinOp AS.BinOpDivide
                (AS.ExprCall (AS.QualifiedIdentifier _ "Real")
                               [e1])
                (AS.ExprCall (AS.QualifiedIdentifier _ "Real")
                               [e2]))
                  -> translateExpr' overrides (AS.ExprBinOp AS.BinOpDiv e1 e2) ty
              _ -> X.throw $ InvalidOverloadedFunctionCall fun args
          --FIXME: determine actual rounding here
          AS.ExprCall (AS.QualifiedIdentifier _ fun@"RoundUp") args@[e] -> Just $ do
            case e of
              (AS.ExprBinOp AS.BinOpDivide
                (AS.ExprCall (AS.QualifiedIdentifier _ "Real")
                               [e1])
                (AS.ExprCall (AS.QualifiedIdentifier _ "Real")
                               [e2]))
                  -> translateExpr' overrides (AS.ExprBinOp AS.BinOpDiv e1 e2) ty
              _ -> X.throw $ InvalidOverloadedFunctionCall fun args
          AS.ExprCall (AS.QualifiedIdentifier _ "NOT") [e] -> Just $ do
            translateExpr' overrides (AS.ExprUnOp AS.UnOpNot e) ty
          AS.ExprCall (AS.QualifiedIdentifier _ "Abs") [e] -> Just $ do
            Some atom <- translateExpr overrides e
            case CCG.typeOfAtom atom of
              CT.IntegerRepr -> do
                mkAtom (CCG.App (CCE.IntAbs (CCG.AtomExpr atom)))
              tp -> X.throw $ ExpectedIntegerType e tp
          -- FIXME: fix definition below; currently it just returns its args
          AS.ExprCall (AS.QualifiedIdentifier _ "ASR_C") [x, shift] -> Just $ do
            Some xAtom <- translateExpr overrides x
            Some shiftAtom <- translateExpr overrides shift
            bitAtom <- CCG.mkAtom (CCG.App (CCE.BVLit (WT.knownNat @1) 0))
            let xType = CCG.typeOfAtom xAtom
                bitType = CT.BVRepr (WT.knownNat @1)
                structType = Ctx.empty Ctx.:> xType Ctx.:> bitType
                structElts = Ctx.empty Ctx.:> CCG.AtomExpr xAtom Ctx.:> CCG.AtomExpr bitAtom
                struct = MkBaseStruct structType structElts
            structAtom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp struct))
            return $ (Some structAtom, TypeTuple [TypeBasic, TypeBasic])
          -- FIXME: fix definition below; currently it just returns its args
          AS.ExprCall (AS.QualifiedIdentifier _ "LSL_C") [x, shift] -> Just $ do
            Some xAtom <- translateExpr overrides x
            Some shiftAtom <- translateExpr overrides shift
            bitAtom <- CCG.mkAtom (CCG.App (CCE.BVLit (WT.knownNat @1) 0))
            let xType = CCG.typeOfAtom xAtom
                bitType = CT.BVRepr (WT.knownNat @1)
                structType = Ctx.empty Ctx.:> xType Ctx.:> bitType
                structElts = Ctx.empty Ctx.:> CCG.AtomExpr xAtom Ctx.:> CCG.AtomExpr bitAtom
                struct = MkBaseStruct structType structElts
            structAtom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp struct))
            return $ (Some structAtom, TypeTuple [TypeBasic, TypeBasic])
          -- FIXME: fix definition below; currently it just returns its args
          AS.ExprCall (AS.QualifiedIdentifier _ "LSL") [x, shift] -> Just $ do
            (Some xAtom, _) <- translateExpr' overrides x ty
            Some shiftAtom <- translateExpr overrides shift
            return $ (Some xAtom, TypeBasic)
          -- FIXME: fix definition below; currently it just returns its args
          AS.ExprCall (AS.QualifiedIdentifier _ "LSR_C") [x, shift] -> Just $ do
            Some xAtom <- translateExpr overrides x
            Some shiftAtom <- translateExpr overrides shift
            bitAtom <- CCG.mkAtom (CCG.App (CCE.BVLit (WT.knownNat @1) 0))
            let xType = CCG.typeOfAtom xAtom
                bitType = CT.BVRepr (WT.knownNat @1)
                structType = Ctx.empty Ctx.:> xType Ctx.:> bitType
                structElts = Ctx.empty Ctx.:> CCG.AtomExpr xAtom Ctx.:> CCG.AtomExpr bitAtom
                struct = MkBaseStruct structType structElts
            structAtom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp struct))
            return $ (Some structAtom, TypeTuple [TypeBasic, TypeBasic])
          -- FIXME: fix definition below; currently it just returns its args
          AS.ExprCall (AS.QualifiedIdentifier _ "RRX_C") [x, shift] -> Just $ do
            Some xAtom <- translateExpr overrides x
            Some shiftAtom <- translateExpr overrides shift
            bitAtom <- CCG.mkAtom (CCG.App (CCE.BVLit (WT.knownNat @1) 0))
            let xType = CCG.typeOfAtom xAtom
                bitType = CT.BVRepr (WT.knownNat @1)
                structType = Ctx.empty Ctx.:> xType Ctx.:> bitType
                structElts = Ctx.empty Ctx.:> CCG.AtomExpr xAtom Ctx.:> CCG.AtomExpr bitAtom
                struct = MkBaseStruct structType structElts
            structAtom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp struct))
            return $ (Some structAtom, TypeTuple [TypeBasic, TypeBasic])
          -- FIXME: fix definition below to actually get the "cond" local variable
          AS.ExprCall (AS.QualifiedIdentifier _ "CurrentCond") [] -> Just $ do
            mkAtom (CCG.App (CCE.BVLit (WT.knownNat @4) 0))
          -- FIXME: implement this (asl definition is recursive and dependently typed)
          AS.ExprCall (AS.QualifiedIdentifier _ "BigEndianReverse") [x] -> Just $ do
            Some xAtom <- translateExpr overrides x
            mkAtom (CCG.AtomExpr xAtom)
          AS.ExprCall (AS.QualifiedIdentifier _ "sizeOf") [x] -> Just $ do
            Some xAtom <- translateExpr overrides x
            BVRepr nr <- getAtomBVRepr xAtom
            translateExpr' overrides (AS.ExprLitInt (WT.intValue nr)) ConstraintNone
          AS.ExprCall (AS.QualifiedIdentifier _ "__ReadRAM") [_, szExpr, _, addr] -> Just $ do
            env <- MS.gets tsStaticEnv
            if | Just (StaticInt sz) <- exprToStatic env szExpr
               , Just (Some repr) <- WT.someNat (sz * 8)
               , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` repr -> do
                   mkAtom (CCG.App (CCE.BVUndef repr))
               | otherwise -> X.throw $ BadMemoryAccess szExpr
          _ -> overloadedDispatchOverrides e ty



-- FIXME: Change this to set some global flag?
raiseException :: Generator h s arch ret ()
raiseException = return ()
