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
import           Control.Monad ( when, void, foldM )
import qualified Control.Monad.State.Class as MS
import qualified Control.Monad.State as MSS
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

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Extension ( ASLExt, ASLApp(..), ASLStmt(..) )
import           SemMC.ASL.Exceptions ( TranslationException(..) )
import           SemMC.ASL.Signature
import           SemMC.ASL.Types
import           SemMC.ASL.Translation.Preprocess

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
                  -> (a tp -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (CCG.Expr (ASLExt arch) s tp))
                  -> ExprConstructor (ASLExt arch) regs h s ret

-- | Inside of the translator, look up the current definition of a name
--
-- We currently assume that arguments are never assigned to (i.e., there is no
-- name shadowing).
lookupVarRef' :: forall arch h s ret
              . T.Text
             -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Maybe (Some (CCG.Expr (ASLExt arch) s)))
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
      i <- lookupTypeEnvir name (tsTypeEnvir ts)
      return (ExprConstructor (CCG.App (CCE.IntLit i)) return)
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
            -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Expr (ASLExt arch) s))
lookupVarRef name = do
  fromMaybe (X.throw (UnboundName name)) <$> lookupVarRef' name

-- | Inside of the translator, look up the current definition of a name
--
-- We currently assume that arguments are never assigned to (i.e., there is no
-- name shadowing).
lookupVarType :: forall arch h s ret
              . T.Text
             -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Maybe (Some (CT.TypeRepr)))
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
      i <- lookupTypeEnvir name (tsTypeEnvir ts)
      return $ Some CT.IntegerRepr
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
  Overrides { overrideStmt :: forall h s ret . AS.Stmt -> Maybe (CCG.Generator (ASLExt arch) h s (TranslationState h) ret ())
            , overrideExpr :: forall h s ret . AS.Expr -> Maybe (CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s)))
            }

-- Tracks state necessary for the translation of ASL into Crucible
--
-- This is primarily storing variable bindings and the set of signatures
-- available for other callees.
data TranslationState h s =
  TranslationState { tsArgAtoms :: Map.Map T.Text (Some (CCG.Atom s))
                   -- ^ Atoms corresponding to function/procedure inputs.  We assume that these are
                   -- immutable and allocated before we start executing.
                   , tsVarRefs :: Map.Map T.Text (Some (CCG.Reg s))
                   -- ^ Local registers containing values; these are created on first use
                   , tsExtendedTypes :: Map.Map T.Text ExtendedTypeData
                   -- ^ Additional type information for local variables
                   , tsAtomToStruct :: Map.Map (Some (CCG.Atom s), Maybe Integer) (Map.Map T.Text StructAccessor)
                   -- ^ Retain struct labelling for atoms without variable names
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
                   , tsHandle :: STRef.STRef h (Map.Map T.Text TypeEnvir)
                   -- ^ Used to name functions encountered during translation
                   , tsTypeEnvir :: TypeEnvir
                   -- ^ Type environment to give concrete instantiations to polymorphic variables
                   
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

-- | Obtain the global variables touched by the given 'ProcedureSignature'
--
-- This is a subset of all of the global state (and a subset of the current
-- global state).
withProcGlobals :: (m ~ CCG.Generator (ASLExt arch) h s (TranslationState h) ret)
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

-- | Translate a list of statements, reporting an error if the execution
-- runs off the end.
translateStatements :: forall ret tp h s arch
                . (ret ~ CT.BaseToType tp)
               => Overrides arch
               -> SomeSignature ret
               -> [AS.Stmt]
               -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateStatements ov sig stmts = do
  let errmsg = "Function " ++ T.unpack (someSigName sig) ++ " does not return."
  mapM_ (\(nm,t) -> addExtendedTypeData nm t) (someSigArgs sig)
  mapM_ (translateStatement ov sig) stmts
  errStr <- CCG.mkAtom (CCG.App (CCE.TextLit (T.pack errmsg)))
  CCG.reportError (CCG.AtomExpr errStr)

-- | Translate a single ASL statement into Crucible
translateStatement :: forall arch ret h s
                    . Overrides arch
                   -> SomeSignature ret
                   -- ^ Signature of the function/procedure we are translating. We
                   -- need this to check that return statements return the correct
                   -- type.
                   -> AS.Stmt
                   -- ^ Statement we are translating
                   -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateStatement ov sig stmt
  | Just so <- overrideStmt ov stmt = so
  | otherwise =
    let rep = someSigRepr sig
    in case stmt of
      AS.StmtReturn Nothing -> case sig of
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
        Some a <- translateExpr ov expr
        Refl <- assertAtomType expr rep a
        CCG.returnFromFunction (CCG.AtomExpr a)
      AS.StmtIf clauses melse -> translateIf ov sig clauses melse
      AS.StmtCase e alts -> translateCase ov sig e alts
      AS.StmtAssert e -> do
        let msg = CCG.App (CCE.TextLit (T.pack (show e)))
        env <- MS.gets tsTypeEnvir
        case exprToBool env e of
          Just True -> return ()
          Just False -> CCG.reportError msg
          Nothing -> do
            Some atom <- translateExpr ov e
            Refl <- assertAtomType e CT.BoolRepr atom
            CCG.assertExpr (CCG.AtomExpr atom) msg
      AS.StmtVarsDecl ty idents -> mapM_ (declareUndefinedVar ty) idents
      AS.StmtVarDeclInit (ident, ty) expr -> translateDefinedVar ov ty ident expr
      AS.StmtConstDecl (ident, ty) expr ->
        -- NOTE: We use the same translation for constants.  We don't do any verification that the
        -- ASL doesn't attempt to modify a constant.
        translateDefinedVar ov ty ident expr
      AS.StmtAssign lval expr -> translateAssignment ov lval expr
      AS.StmtWhile test body -> do
        let testG = do
              Some testA <- translateExpr ov test
              Refl <- assertAtomType test CT.BoolRepr testA
              return (CCG.AtomExpr testA)
        let bodyG = mapM_ (translateStatement ov sig) body
        CCG.while (WP.InternalPos, testG) (WP.InternalPos, bodyG)
      AS.StmtRepeat body test -> translateRepeat ov sig body test
      AS.StmtFor var (lo, hi) body -> translateFor ov sig var lo hi body
      AS.StmtUndefined -> do
        gs <- MS.gets tsGlobals
        case Map.lookup undefinedVarName gs of
          Just (Some gv)
            | Just Refl <- testEquality (CCG.globalType gv) CT.BoolRepr -> do
                CCG.writeGlobal gv (CCG.App (CCE.BoolLit True))
            | otherwise -> X.throw (UnexpectedGlobalType undefinedVarName (CCG.globalType gv))
          _ -> X.throw (MissingGlobal (someSigName sig) undefinedVarName)
      AS.StmtUnpredictable -> do
        gs <- MS.gets tsGlobals
        case Map.lookup unpredictableVarName gs of
          Just (Some gv)
            | Just Refl <- testEquality (CCG.globalType gv) CT.BoolRepr -> do
                CCG.writeGlobal gv (CCG.App (CCE.BoolLit True))
            | otherwise -> X.throw (UnexpectedGlobalType unpredictableVarName (CCG.globalType gv))
          _ -> X.throw (MissingGlobal (someSigName sig) unpredictableVarName)
      -- NOTE: Ensure that this is safe.  Most SEE statements seem to not be
      -- particularly actionable, but many may need to be manually overridden.
      AS.StmtSeeExpr {} -> return ()
      AS.StmtSeeString {} -> return ()
      AS.StmtCall qIdent args -> do
        sigMap <- MS.gets tsFunctionSigs
        let ident = mkFunctionName qIdent (length args)
        case Map.lookup ident sigMap of
          Nothing -> X.throw (MissingFunctionDefinition ident)
          Just (SomeSimpleFunctionSignature _) -> X.throw (ExpectedProcedureSignature ident)
          Just (SomeSimpleProcedureSignature pSig) -> do
            (finalIdent, argAtoms, _) <- unifyArgs ov ident (zip (sprocArgs pSig) args) [] Nothing
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
      AS.StmtTry {} -> error "Try statements are not implemented"
      AS.StmtThrow err -> do
        CCG.assertExpr (CCG.App (CCE.BoolLit False)) (CCG.App (CCE.TextLit err))
      _ -> error (show stmt)

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
             -> SomeSignature ret
             -> AS.Identifier
             -> AS.Expr
             -> AS.Expr
             -> [AS.Stmt]
             -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateFor ov sig var lo hi body = do
  env <- MS.gets tsTypeEnvir
  case (exprToInt env lo, exprToInt env hi) of
    (Just lo', Just hi') -> unrollFor ov sig var lo' hi' body
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
            
      let bodyG = mapM_ (translateStatement ov sig) (body ++ [increment])
      CCG.while (WP.InternalPos, testG) (WP.InternalPos, bodyG)


unrollFor :: Overrides arch
          -> SomeSignature ret
          -> AS.Identifier
          -> Integer
          -> Integer
          -> [AS.Stmt]
          -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
unrollFor ov sig var lo hi body = do
  mapM_ translateFor [lo .. hi]
  where
    translateFor i = do
      mapTypeEnvir (insertTypeEnvir var i . pushFreshTypeEnvir)
      mapM_ (translateStatement ov sig) body
      mapTypeEnvir popTypeEnvir
       


translateRepeat :: Overrides arch
                -> SomeSignature ret
                -> [AS.Stmt]
                -> AS.Expr
                -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateRepeat ov sig body test = do
  cond_lbl <- CCG.newLabel
  loop_lbl <- CCG.newLabel
  exit_lbl <- CCG.newLabel

  CCG.defineBlock loop_lbl $ do
    mapM_ (translateStatement ov sig) body
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
                    -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateDefinedVar ov ty ident expr = do
  tty <- translateType ty
  case tty of
    Some expected -> do
      (Some atom, ext) <- translateExpr' ov expr (Just tty)
      Refl <- assertAtomType expr expected atom
      locals <- MS.gets tsVarRefs
      when (Map.member ident locals) $ do
        X.throw (LocalAlreadyDefined ident)
      putExtendedTypeData ident ext
      reg <- CCG.newReg (CCG.AtomExpr atom)
      MS.modify' $ \s -> s { tsVarRefs = Map.insert ident (Some reg) locals }


-- | Convert an lVal to its equivalent expression in order to determine
-- its value for a sliced assignement. These are the only forms seen
-- in an LValSliceOf
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

-- | Translate general assignment statements into Crucible
--
-- This case is interesting, as assignments can be to locals or globals.
--
-- NOTE: We are assuming that there cannot be assignments to arguments.
translateAssignment :: Overrides arch
                    -> AS.LValExpr
                    -> AS.Expr
                    -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateAssignment ov lval e = do
  -- If possible, determine the type of the left hand side first in order
  -- to inform the translation of the given expression

  (Some atom, ext) <- case lval of
    AS.LValVarRef (AS.QualifiedIdentifier _ ident) -> do
      mTy <- lookupVarType ident
      translateExpr' ov e mTy
    _ -> case lValToExpr lval of
      Just lve -> translateInformed lve
      _ -> translateExpr' ov e Nothing

  translateAssignment' ov lval atom ext (Just e)
  where
    translateInformed lve = do
      Some lveAtom <- translateExpr ov lve
      translateExpr' ov e (Just $ Some (CCG.typeOfAtom lveAtom))

translateAssignment' :: forall arch s tp h ret . Overrides arch
                     -> AS.LValExpr
                     -> CCG.Atom s tp
                     -> ExtendedTypeData
                     -> Maybe AS.Expr
                     -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateAssignment' ov lval atom atomext mE = do
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
              reg <- CCG.newUnassignedReg atomType
              MS.modify' $ \s -> s { tsVarRefs = Map.insert ident (Some reg) locals }
              CCG.assignReg reg (CCG.AtomExpr atom)
    AS.LValMember struct memberName -> do
      Just lve <- return $ lValToExpr struct
      lvalext <- exprExtraTypeData lve
      case lvalext of
        TypeBasic -> do
          case struct of
            AS.LValVarRef (AS.QualifiedIdentifier _ structName) -> do
              globals <- MS.gets tsGlobals
              let ident = mkStructMemberName structName memberName
              case Map.lookup ident globals of
                Just (Some gv) -> do
                  Refl <- assertAtomType' (CCG.globalType gv) atom
                  CCG.writeGlobal gv (CCG.AtomExpr atom)
                _ -> error $ "Missing global struct lval: " ++ show ident
            _ -> error $ "Bad struct lval: " ++ show lval
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

    AS.LValTuple lvals ->
      case atomext of
        TypeTuple exts | length exts == length lvals ->
          case CCG.typeOfAtom atom of
            CT.SymbolicStructRepr tps -> void $ Ctx.traverseAndCollect (assignTupleElt lvals exts tps atom) tps
            tp -> X.throw $ ExpectedStructType mE tp
        _ -> error $ "Unexpected extended type information:" <> show lvals <> " " <> show atomext

    AS.LValSliceOf lv [slice] -> translatelValSlice ov lv slice atom

    -- FIXME: This form appears only twice and could easily be broken
    -- into two assignments. The above case covers all other cases.
    AS.LValSliceOf lv [fstSlice@(AS.SliceSingle _), slice] -> do
      case CCG.typeOfAtom atom of
        CT.BVRepr wRepr -> do
          let topIndex = WT.intValue wRepr - 1
          Some topBit <- translateSlice' ov atom (AS.SliceSingle (AS.ExprLitInt topIndex))
          translatelValSlice ov lv fstSlice topBit
          Some rest <- translateSlice' ov atom (AS.SliceRange (AS.ExprLitInt (topIndex - 1))
                                                (AS.ExprLitInt 0))
          translatelValSlice ov lv slice rest
        _ -> error $ "Unsupported LVal: " ++ show lval

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
               let arr = CCE.SymArrayUpdate retTy (CCG.AtomExpr arrAtom) asn (CCG.AtomExpr atom)
               newArr <- CCG.mkAtom (CCG.App arr)
               translateAssignment' ov ref newArr TypeBasic Nothing
           | otherwise -> error $ "Invalid array assignment: " ++ show lval

    AS.LValArrayIndex _ (_ : _ : _) -> do
      error $
        "Unexpected multi-argument array assignment. Is this actually a setter?" ++ show lval

    AS.LValMemberBits ref bits -> do
      let ibits = imap (\i -> \e -> (i, e)) bits
      mapM_ (\(i, e) -> do
        Some aslice <- translateSlice' ov atom (AS.SliceSingle (AS.ExprLitInt (toInteger i)))
        let lv' = AS.LValMember ref e in
          translateAssignment' ov lv' aslice TypeBasic Nothing) ibits

    -- This never appears
    AS.LValMemberArray _ _ ->
      error $ "Unsupported LVal: " ++ show lval

    _ -> error $ "Unsupported LVal: " ++ show lval
    where assignTupleElt :: [AS.LValExpr]
                         -> [ExtendedTypeData]
                         -> Ctx.Assignment WT.BaseTypeRepr ctx
                         -> CCG.Atom s (CT.SymbolicStructType ctx)
                         -> Ctx.Index ctx tp'
                         -> WT.BaseTypeRepr tp'
                         -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
          assignTupleElt lvals exts tps struct ix _ = do
            let getStruct = GetBaseStruct (CT.SymbolicStructRepr tps) ix (CCG.AtomExpr struct)
            getAtom <- CCG.mkAtom (CCG.App (CCE.ExtensionApp getStruct))
            let ixv = Ctx.indexVal ix
            translateAssignment' ov (lvals !! ixv) getAtom (exts !! ixv) Nothing


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
                   -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translatelValSlice ov lv slice asnAtom = do
  let Just lve = lValToExpr lv
  Some atom <- translateExpr ov lve
  (Some loRepr, Some hiRepr) <- getSliceRange slice
  if | Just WT.LeqProof <- loRepr `WT.testLeq` hiRepr
     , lenRepr <- (WT.knownNat @1) `WT.addNat` (hiRepr `WT.subNat` loRepr)
     , CT.BVRepr wRepr <- CCG.typeOfAtom atom
     , Just WT.LeqProof <- (hiRepr `WT.addNat` (WT.knownNat @1)) `WT.testLeq` wRepr
     , lenUpper <- wRepr `WT.subNat` (hiRepr `WT.addNat` (WT.knownNat @1))
     , CT.BVRepr wReprAsn <- CCG.typeOfAtom asnAtom
     , Just Refl <- testEquality lenRepr wReprAsn
     --,

     -> do
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

     | otherwise ->
         X.throw $ InvalidSliceRange (WT.intValue loRepr) (WT.intValue hiRepr)


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
                    -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
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

-- Extended type data for tracking struct member identifiers. This is necessary since Crucible structs
-- are just tuples, and so extra information is required to resolve ASL struct members to their
-- corresponding Crucible struct index. 

type UserStructAcc = Map.Map T.Text StructAccessor

data StructAccessor = forall tps tp. StructAccessor
  { structRepr :: Ctx.Assignment WT.BaseTypeRepr tps
  , structIdx :: Ctx.Index tps tp
  , structFieldExt :: ExtendedTypeData}

deriving instance Show StructAccessor

instance Eq StructAccessor where
  (==) a b = case (a, b) of
    (StructAccessor ar aidx e, StructAccessor br bidx e') |
        Just Refl <- testEquality ar br
      , Just Refl <- testEquality aidx bidx -> e == e'
    _ -> False

data ExtendedTypeData =
    TypeBasic
  | TypeStruct UserStructAcc
  | TypeTuple [ExtendedTypeData]
  | TypeArray ExtendedTypeData
  deriving (Show, Eq)

mkExtendedTypeData :: AS.Type
                   -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (ExtendedTypeData)
mkExtendedTypeData ty = do
  case ty of
    AS.TypeRef qi@(AS.QualifiedIdentifier _ tident) -> do
      uts <- MS.gets tsUserTypes
      return $ fromUT (Map.lookup tident uts)
    _ -> return TypeBasic
  where
    fromUT :: Maybe (Some UserType) -> ExtendedTypeData
    fromUT mUT = case mUT of
      Just (Some (UserStruct s)) -> do
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
      let ext = fromUT mUT
      MS.modify' (Map.insert nm (StructAccessor repr idx ext))

addExtendedTypeData :: AS.Identifier
                    -> AS.Type                    
                    -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
addExtendedTypeData ident ty = do
  ext <- mkExtendedTypeData ty
  putExtendedTypeData ident ext

putExtendedTypeData :: AS.Identifier
                    -> ExtendedTypeData
                    -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
putExtendedTypeData ident ext = do
  MS.modify' $ \s -> s { tsExtendedTypes = Map.insert ident ext (tsExtendedTypes s) }

getExtendedTypeData :: AS.Identifier
                    -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (ExtendedTypeData)
getExtendedTypeData ident = do
  exts <- MS.gets tsExtendedTypes
  return $ fromMaybe TypeBasic (Map.lookup ident exts)

findExtraTypeData :: Monad m
                   => (T.Text -> m ExtendedTypeData)
                   -> AS.Expr
                   -> m ExtendedTypeData
findExtraTypeData g expr = do
  case expr of
    AS.ExprVarRef (AS.QualifiedIdentifier _ q) -> g q
    AS.ExprIndex e _ -> do
      ext <- findExtraTypeData g e
      case ext of
        TypeArray ext' -> return ext'
        _ -> return TypeBasic
    AS.ExprMember e mem -> do
      ext <- findExtraTypeData g e
      case ext of
        TypeStruct acc -> return $ doLookup acc mem
        _ -> return TypeBasic
    _ -> error $ "Unexpected qualification:" <> show expr
  where
    doLookup acc nm  = case Map.lookup nm acc of
      Just sacc -> structFieldExt sacc
      Nothing -> TypeBasic

exprExtraTypeData :: AS.Expr
                  -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (ExtendedTypeData)
exprExtraTypeData e = findExtraTypeData getExtendedTypeData e

assertEqualExtensions :: ExtendedTypeData
                      -> ExtendedTypeData
                      -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (ExtendedTypeData)
assertEqualExtensions ext1 ext2 =
  if ext1 == ext2 then return ext1
  else error $ "Mismatched extended type data:" <> show ext1 <> " " <> show ext2


-- | Translate types (including user-defined types) into Crucible type reprs
--
-- Translations of user-defined types (i.e., types defined in an ASL program)
-- are stored in the 'TranslationState' and are looked up when needed.
--
-- FIXME: Handle polymorphic types (i.e., `bits(N)`)
translateType :: AS.Type -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some CT.TypeRepr)
translateType t = do
  env <- MS.gets tsTypeEnvir
  case applyTypeEnvir env t of
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
        -- FIXME: We assume that N is always 32!!! This needs to be fixed, probably.
        --AS.ExprVarRef (AS.QualifiedIdentifier _ "N") -> return (Some (CT.BVRepr (WT.knownNat @32)))
        _ -> error ("Unsupported type: " ++ show t)
    AS.TypeFun _ _ -> error ("Unsupported type: " ++ show t)
    AS.TypeArray _ty _idxTy -> error ("Unsupported type: " ++ show t)
    AS.TypeReg _i _flds -> error ("Unsupported type: " ++ show t)
    _ -> error ("Unsupported type: " ++ show t)

withTypeEnvir :: TypeEnvir
              -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret a
              -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (a, TypeEnvir)
withTypeEnvir tenv f = do
  env <- MS.gets tsTypeEnvir
  MS.modify' $ \s -> s { tsTypeEnvir = tenv }
  x <- f
  env' <- MS.gets tsTypeEnvir
  MS.modify' $ \s -> s { tsTypeEnvir = env }
  return (x, env')


dependentVarsOfType :: AS.Type -> [T.Text]
dependentVarsOfType t = case t of
  AS.TypeFun "bits" e -> varsOfExpr e
  _ -> []

varsOfExpr :: AS.Expr -> [T.Text]
varsOfExpr e = foldExpr getVar e []
  where
    getVar (AS.ExprVarRef (AS.QualifiedIdentifier q ident)) = (:) ident
    getVar _ = id


mapTypeEnvir :: (TypeEnvir -> TypeEnvir)
             -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
mapTypeEnvir f = do
  env <- MS.gets tsTypeEnvir
  MS.modify' $ \s -> s { tsTypeEnvir = f env }



collectConstIntegers :: TypeEnvir
                     -> AS.SymbolDecl
                     -> AS.Expr
                     -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
collectConstIntegers outerEnv (nm, t) e = do
  case exprToInt outerEnv e of
    Just i -> mapTypeEnvir (insertTypeEnvir nm i)
    _ -> return ()

-- Unify a syntactic ASL type against a crucible type, and update
-- the current typing evironment with any discovered instantiations
unifyType :: AS.Type
          -> Some (CT.TypeRepr)
          -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
unifyType aslT (Some atomT) = do
  env <- MS.gets tsTypeEnvir
  case (aslT, atomT) of
    (AS.TypeFun "bits" e, CT.BVRepr repr) ->
      case e of
        AS.ExprLitInt i | Just (Some nr) <- NR.someNat i, Just Refl <- testEquality repr nr -> return ()
        AS.ExprVarRef (AS.QualifiedIdentifier _ id) ->
          case lookupTypeEnvir id env of
            Just i | Just (Some nr) <- NR.someNat i, Just Refl <- testEquality repr nr -> return ()
            Nothing -> mapTypeEnvir (insertTypeEnvir id (toInteger (NR.natValue repr)))
            _ -> X.throw $ TypeUnificationFailure aslT atomT env
        AS.ExprBinOp AS.BinOpMul e e' ->
          case (mInt env e, mInt env e') of
            (Left i, Left i') | Just (Some nr) <- NR.someNat (i * i'), Just Refl <- testEquality repr nr -> return ()
            (Right (AS.ExprVarRef (AS.QualifiedIdentifier _ id)), Left i')
              | reprVal <- toInteger $ WT.natValue repr
              , (innerVal, 0) <- reprVal `divMod` i' ->
                mapTypeEnvir $ insertTypeEnvir id innerVal
            (Left i, Right (AS.ExprVarRef (AS.QualifiedIdentifier _ id)))
              | reprVal <- toInteger $ WT.natValue repr
              , (innerVal, 0) <- reprVal `divMod` i ->
               mapTypeEnvir $ insertTypeEnvir id innerVal
            _ -> X.throw $ TypeUnificationFailure aslT atomT env
        _ -> X.throw $ TypeUnificationFailure aslT atomT env
    _ -> do
      Some atomT' <- translateType aslT
      case testEquality atomT atomT' of
        Just Refl -> return ()
        _ -> X.throw $ TypeUnificationFailure aslT atomT env
  where
    mInt env e = case exprToInt env e of
      Just i -> Left i
      Nothing -> Right e

getConcreteType :: AS.Type ->  CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Maybe (Some CT.TypeRepr))
getConcreteType t =
  case dependentVarsOfType t of
    [] -> do
      ty <- translateType t
      return $ Just ty
    _ -> return Nothing

        
unifyArg :: Overrides arch
         -> TypeEnvir
         -> AS.SymbolDecl
         -> AS.Expr
         -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s))
unifyArg ov outerEnv (nm,t) e = do
  mTy <- getConcreteType t
  ((Some atom, _), _) <- withTypeEnvir outerEnv $ translateExpr' ov e mTy
  let atomT = CCG.typeOfAtom atom
  unifyType t (Some $ CCG.typeOfAtom atom)
  return $ Some atom

asBaseType :: Some CT.TypeRepr -> Some WT.BaseTypeRepr
asBaseType (Some t) = case CT.asBaseType t of
  CT.AsBaseType bt -> Some bt
  CT.NotBaseType -> error $ "Expected base type: " <> show t



unifyArgs :: Overrides arch
          -> T.Text
          -> [(AS.SymbolDecl, AS.Expr)]
          -> [AS.Type]
          -> Maybe (Some CT.TypeRepr)
          -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret
               (T.Text, [Some (CCG.Atom s)], Maybe (Some WT.BaseTypeRepr))
unifyArgs ov fnname args rets targetRet = do
  outerEnv <- MS.gets tsTypeEnvir
  (atoms, tenv) <- withTypeEnvir emptyTypeEnvir $ do
      mapM_ (\(decl, e) -> collectConstIntegers outerEnv decl e) args
      atoms <- mapM (\(decl, e) -> unifyArg ov outerEnv decl e) args
      unifyRet outerEnv rets targetRet
      return atoms
  let dvars = concat $ map dependentVarsOfType rets ++ map (\((_,t), _) -> dependentVarsOfType t) args
  let env = fromListTypeEnvir $ map (\nm -> case lookupTypeEnvir nm tenv of {Just i -> (nm,i)}) dvars
  hdl <- MS.gets tsHandle
  MST.liftST (STRef.modifySTRef hdl (Map.insert fnname env))

  mapM_ (\nm -> case lookupTypeEnvir nm tenv of {Just _ -> return (); Nothing ->
          error $ "Could not finalize polymorphic type environment for:" <> (show fnname)}) dvars

  retsT <- mapM (\t -> fst <$> withTypeEnvir tenv (translateType t)) rets
  retT <- case map asBaseType retsT of
            [] -> return Nothing
            [retbT] -> return $ Just retbT
            retbsT -> do
              Some assignment <- return $ Ctx.fromList retbsT
              return $ Just $ Some (WT.BaseStructRepr assignment)
  
  return (mkFinalFunctionName env fnname, atoms, retT)
  where
    mInt env e = case exprToInt env e of
      Just i -> Left i
      Nothing -> Right e

    unifyRet :: TypeEnvir
          -> [AS.Type] -- return type of function
          -> Maybe (Some CT.TypeRepr) -- potential concrete target type
          -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
    unifyRet outerEnv [t] (Just targetRet) = unifyType t targetRet
    unifyRet _ _ _ = return ()


translateIf :: Overrides arch
            -> SomeSignature ret
            -> [(AS.Expr, [AS.Stmt])]
            -> Maybe [AS.Stmt]
            -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateIf ov sig clauses melse =
  case clauses of
    [] -> mapM_ (translateStatement ov sig) (fromMaybe [] melse)
    (cond, body) : rest -> do
      Some condAtom <- translateExpr ov cond
      Refl <- assertAtomType cond CT.BoolRepr condAtom
      let genThen = mapM_ (translateStatement ov sig) body
      let genElse = translateIf ov sig rest melse
      CCG.ifte_ (CCG.AtomExpr condAtom) genThen genElse

translateCase :: Overrides arch
              -> SomeSignature ret
              -> AS.Expr
              -> [AS.CaseAlternative]
              -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateCase ov sig expr alts = case alts of
  [AS.CaseOtherwise els] -> mapM_ (translateStatement ov sig) els
  -- FIXME: We assume that the case below is equivalent to "otherwise"
  [AS.CaseWhen _ Nothing body] -> mapM_ (translateStatement ov sig) body
  -- FIXME: If we detect an "unreachable", translate it as if the preceding "when"
  -- were "otherwise"
  [AS.CaseWhen _ Nothing body, AS.CaseOtherwise [AS.StmtCall (AS.QualifiedIdentifier _ "Unreachable") []]] ->
    mapM_ (translateStatement ov sig) body
  (AS.CaseWhen pats Nothing body : rst) -> do
    let matchExpr = caseWhenExpr expr pats
    Some matchAtom <- translateExpr ov matchExpr
    Refl <- assertAtomType matchExpr CT.BoolRepr matchAtom
    let genThen = mapM_ (translateStatement ov sig) body
    let genRest = translateCase ov sig expr rst
    CCG.ifte_ (CCG.AtomExpr matchAtom) genThen genRest
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
matchPat _ (AS.CasePatternMask _) = error "bitmask pattern unimplemented"
matchPat _ AS.CasePatternIgnore = error "ignore pattern unimplemented"
matchPat _ (AS.CasePatternTuple _) = error "tuple pattern unimplemented"

assertAtomType :: AS.Expr
               -- ^ Expression that was translated
               -> CT.TypeRepr tp1
               -- ^ Expected type
               -> CCG.Atom s tp2
               -- ^ Translation (which contains the actual type)
               -> CCG.Generator ext h s (TranslationState h) ret (tp1 :~: tp2)
assertAtomType expr expectedRepr atom =
  case testEquality expectedRepr (CCG.typeOfAtom atom) of
    Nothing -> X.throw (UnexpectedExprType (Just expr) (CCG.typeOfAtom atom) expectedRepr)
    Just Refl -> return Refl

assertAtomType' :: CT.TypeRepr tp1
                -- ^ Expected type
                -> CCG.Atom s tp2
                -- ^ Translation (which contains the actual type)
                -> CCG.Generator ext h s (TranslationState h) ret (tp1 :~: tp2)
assertAtomType' expectedRepr atom =
  case testEquality expectedRepr (CCG.typeOfAtom atom) of
    Nothing -> X.throw (UnexpectedExprType Nothing (CCG.typeOfAtom atom) expectedRepr)
    Just Refl -> return Refl

-- | Translate an ASL expression into an Atom (which is a reference to an immutable value)
--
-- Atoms may be written to registers, which are mutable locals
translateExpr' :: Overrides arch
              -> AS.Expr
              -> Maybe (Some CT.TypeRepr)
              -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s), ExtendedTypeData)
translateExpr' ov expr mTy
  | Just eo <- overrideExpr ov expr = do
      atom <- eo
      return (atom, TypeBasic)
  | otherwise =
    case expr of
      AS.ExprLitInt i -> mkAtom (CCG.App (CCE.IntLit i))
      AS.ExprLitBin bits -> do
        let nBits = length bits
        case NR.mkNatRepr (fromIntegral nBits) of
          Some nr
            | Just NR.LeqProof <- NR.testLeq (NR.knownNat @1) nr ->
              mkAtom (CCG.App (CCE.BVLit nr (bitsToInteger bits)))
            | otherwise -> X.throw InvalidZeroLengthBitvector
      AS.ExprVarRef (AS.QualifiedIdentifier _ ident) -> do
        Some e <- lookupVarRef ident
        atom <- CCG.mkAtom e
        ext <- getExtendedTypeData ident
        return (Some atom, ext)

      AS.ExprLitReal {} -> X.throw (UnsupportedExpr expr)
      AS.ExprLitString {} -> X.throw (UnsupportedExpr expr)
      AS.ExprUnOp op expr' -> basicExpr $ translateUnaryOp ov op expr'
      AS.ExprBinOp op e1 e2 -> basicExpr $ translateBinaryOp ov op e1 e2
      AS.ExprTuple exprs -> do
        atomExts <- case mTy of
          Just (Some (CT.SymbolicStructRepr tps)) -> do
           let exprTs = zip (FC.toListFC Some tps) exprs
           mapM (\(Some ty, e) -> translateExpr' ov e (Just (Some (CT.baseToType ty)))) exprTs
          Nothing -> do
           mapM (\e -> translateExpr' ov e Nothing) exprs
          _ -> error $ "Unexpected type target for tuple: " <> show mTy
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
          Nothing -> X.throw (MissingFunctionDefinition ident)
          Just (SomeSimpleProcedureSignature _) -> X.throw (ExpectedFunctionSignature ident)
          Just (SomeSimpleFunctionSignature sig) -> do
            (finalIdent, argAtoms, Just (Some retT)) <- unifyArgs ov ident (zip (sfuncArgs sig) args) (sfuncRet sig) mTy
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
      -- FIXME: What to do here?
      AS.ExprImpDef _ -> mkAtom (CCG.App (CCE.BoolLit True))
      -- This is just like a variable lookup, since struct members are stored as
      -- individual global variables.
      AS.ExprMember struct memberName -> do
        lvalext <- exprExtraTypeData struct
        case lvalext of
          TypeBasic -> do
            case struct of
              AS.ExprVarRef (AS.QualifiedIdentifier _ structName) -> do
                let ident = mkStructMemberName structName memberName
                Some e <- lookupVarRef ident
                mkAtom e
              _ -> X.throw (StructFieldMismatch expr)
          TypeStruct acc -> do
            Some atom <- translateExpr ov struct
            case (CCG.typeOfAtom atom, Map.lookup memberName acc) of
              (CT.SymbolicStructRepr tps, Just (StructAccessor repr idx _)) |
                Just Refl <- testEquality tps repr -> do
                  let getStruct = GetBaseStruct (CT.SymbolicStructRepr tps) idx (CCG.AtomExpr atom)
                  mkAtom (CCG.App (CCE.ExtensionApp getStruct))
              _ -> X.throw (StructFieldMismatch expr)

      AS.ExprMemberBits var bits -> do
        let (hdvar : tlvars) = map (\member -> AS.ExprMember var member) bits
        let expr = foldl (\var -> \e -> AS.ExprBinOp AS.BinOpConcat var e) hdvar tlvars
        translateExpr' ov expr mTy

      AS.ExprSlice e [slice] -> do
        satom <- translateSlice ov e slice
        return (satom, TypeBasic)

      AS.ExprSlice e [slice1, slice2] -> do
        let expr = AS.ExprBinOp AS.BinOpConcat (AS.ExprSlice e [slice1]) (AS.ExprSlice e [slice2])
        translateExpr' ov expr mTy

      -- This covers the only form of ExprIndex that appears in ASL
      AS.ExprIndex (AS.ExprVarRef (AS.QualifiedIdentifier _ arrName)) [AS.SliceSingle slice]  -> do
        Some e <- lookupVarRef arrName
        atom <- CCG.mkAtom e
        Some idxAtom <- translateExpr ov slice
        if | CT.AsBaseType bt <- CT.asBaseType (CCG.typeOfAtom idxAtom)
           , CT.SymbolicArrayRepr (Ctx.Empty Ctx.:> bt') retTy <- CCG.typeOfAtom atom
           , Just Refl <- testEquality bt bt' -> do
               let asn = Ctx.singleton (CCE.BaseTerm bt (CCG.AtomExpr idxAtom))
               let arr = CCE.SymArrayLookup retTy (CCG.AtomExpr atom) asn
               mkAtom (CCG.App arr)
           | otherwise ->  X.throw (UnsupportedExpr expr)
      AS.ExprUnknown t -> do
        Some ty <- translateType t
        mkAtom (getDefaultValue ty)

      _ -> error (show expr)
  where
    mkAtom e = do
      atom <- CCG.mkAtom e
      return (Some atom, TypeBasic)
    basicExpr f = do
      satom <- f
      return (satom, TypeBasic)

translateExpr :: Overrides arch
              -> AS.Expr
              -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s))
translateExpr ov expr = do
  (atom, _) <- translateExpr' ov expr Nothing
  return atom

getIndexOfLabel :: Ctx.Assignment WT.BaseTypeRepr tps
                -> T.Text
                -> CCG.Atom s (CT.SymbolicStructType tps)
                -> Ctx.Index tps tp
                -> LabeledValue T.Text WT.BaseTypeRepr tp
                -> MSS.StateT (Maybe (Some (CCG.Atom s))) (CCG.Generator (ASLExt arch) h s (TranslationState h) ret) ()
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
--             -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret a)
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


getSliceRange :: AS.Slice ->
                 CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some WT.NatRepr, Some WT.NatRepr)
getSliceRange slice = do
  env <- MS.gets tsTypeEnvir
  case slice of
    AS.SliceRange e e' |
        Just lo <- exprToInt env e'
      , Just hi <- exprToInt env e
      , Just someLoRepr <- WT.someNat lo
      , Just someHiRepr <- WT.someNat hi ->
        return (someLoRepr, someHiRepr)
    AS.SliceSingle e |
        Just i <- exprToInt env e
      , Just someRepr <- WT.someNat i ->
        return (someRepr, someRepr)
    _ -> X.throw $ UnsupportedSlice slice

  
translateSlice :: Overrides arch
               -> AS.Expr
               -> AS.Slice
               -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s))
translateSlice ov e slice = do
   Some atom <- translateExpr ov e
   translateSlice' ov atom slice
   

translateSlice' :: Overrides arch
                -> CCG.Atom s tp
                -> AS.Slice
                -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s))
translateSlice' _ atom slice = do
  (Some loRepr, Some hiRepr) <- getSliceRange slice
  case () of
    _ | Just WT.LeqProof <- loRepr `WT.testLeq` hiRepr
      , lenRepr <- (WT.knownNat @1) `WT.addNat` (hiRepr `WT.subNat` loRepr)
      , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` lenRepr -> do
        case CCG.typeOfAtom atom of
          CT.BVRepr wRepr | Just WT.LeqProof <- (loRepr `WT.addNat` lenRepr) `WT.testLeq` wRepr ->
                Some <$> CCG.mkAtom (CCG.App (CCE.BVSelect loRepr lenRepr wRepr (CCG.AtomExpr atom)))
          CT.IntegerRepr | wRepr <- hiRepr `WT.addNat` (WT.knownNat @1)
                         , WT.LeqProof <- WT.leqAdd (WT.leqRefl (WT.knownNat @1)) hiRepr
                         , Refl <- WT.plusComm (WT.knownNat @1) hiRepr
                         , Just WT.LeqProof <- (loRepr `WT.addNat` lenRepr) `WT.testLeq` wRepr -> do
                let bv = CCE.IntegerToBV wRepr (CCG.AtomExpr atom)
                Some <$> CCG.mkAtom (CCG.App (CCE.BVSelect loRepr lenRepr wRepr (CCG.App bv)))
          repr -> X.throw $ InvalidSlice (WT.intValue loRepr) (WT.intValue hiRepr) repr
    _ -> X.throw $ InvalidSliceRange (WT.intValue loRepr) (WT.intValue hiRepr)

-- | Translate the expression form of a conditional into a Crucible atom
translateIfExpr :: Overrides arch
                -> AS.Expr
                -> [(AS.Expr, AS.Expr)]
                -> AS.Expr
                -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s), ExtendedTypeData)
translateIfExpr ov orig clauses elseExpr =
  case clauses of
    [] -> X.throw (MalformedConditionalExpression orig)
    [(test, res)] -> do
      Some testA <- translateExpr ov test
      (Some resA, extRes) <- translateExpr' ov res Nothing
      (Some elseA, extElse) <- translateExpr' ov elseExpr Nothing
      ext <- assertEqualExtensions extRes extElse
      Refl <- assertAtomType test CT.BoolRepr testA
      Refl <- assertAtomType res (CCG.typeOfAtom elseA) resA
      case CT.asBaseType (CCG.typeOfAtom elseA) of
        CT.NotBaseType -> X.throw (ExpectedBaseType orig (CCG.typeOfAtom elseA))
        CT.AsBaseType btr -> do
          atom <- CCG.mkAtom (CCG.App (CCE.BaseIte btr (CCG.AtomExpr testA) (CCG.AtomExpr resA) (CCG.AtomExpr elseA)))
          return (Some atom, ext)
    (test, res) : rest -> do
      (Some trA, extRest) <- translateIfExpr ov orig rest elseExpr
      Some testA <- translateExpr ov test
      (Some resA, extRes) <- translateExpr' ov res Nothing
      ext <- assertEqualExtensions extRes extRest
      Refl <- assertAtomType test CT.BoolRepr testA
      Refl <- assertAtomType res (CCG.typeOfAtom trA) resA
      case CT.asBaseType (CCG.typeOfAtom trA) of
        CT.NotBaseType -> X.throw (ExpectedBaseType orig (CCG.typeOfAtom trA))
        CT.AsBaseType btr -> do
          atom <- CCG.mkAtom (CCG.App (CCE.BaseIte btr (CCG.AtomExpr testA) (CCG.AtomExpr resA) (CCG.AtomExpr trA)))
          return (Some atom, ext)

-- | Translate set element tests
--
-- Single element tests are translated into a simple equality test
--
-- Ranges are translated as a conjunction of inclusive tests. x IN [5..10] => 5 <= x && x <= 10
translateSetElementTest :: Overrides arch
                        -> AS.Expr
                        -> CCG.Atom s tp
                        -> AS.SetElement
                        -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (CCG.Expr (ASLExt arch) s CT.BoolType)
translateSetElementTest ov e0 a0 elt =
  case elt of
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
                  -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s))
translateBinaryOp ov op e1 e2 = do
  Some a1 <- translateExpr ov e1
  Some a2 <- translateExpr ov e2
  let p1 = (e1, a1)
  let p2 = (e2, a2)
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
    AS.BinOpShiftLeft -> bvBinOp CCE.BVShl p1 p2
    AS.BinOpShiftRight -> bvBinOp CCE.BVLshr p1 p2
    -- FIXME: What is the difference between BinOpDiv and BinOpDivide?
    AS.BinOpConcat -> case (CCG.typeOfAtom a1, CCG.typeOfAtom a2) of
      (CT.BVRepr n1, CT.BVRepr n2)
        | Just n1PosProof <- WT.isPosNat n1
        , WT.LeqProof <- WT.leqAdd n1PosProof n2 ->
          Some <$> CCG.mkAtom (CCG.App (CCE.BVConcat n1 n2 (CCG.AtomExpr a1) (CCG.AtomExpr a2)))
      (CT.BVRepr _, t2) -> X.throw $ ExpectedBVType e2 t2
      (t1, _) -> X.throw $ ExpectedBVType e1 t1
    _ -> X.throw (UnsupportedBinaryOperator op)

-- Arithmetic operators

addOp :: BinaryOperatorBundle ext s 'SameK
addOp = BinaryOperatorBundle CCE.BVAdd CCE.NatAdd CCE.IntAdd

subOp :: BinaryOperatorBundle ext s 'SameK
subOp = BinaryOperatorBundle CCE.BVSub CCE.NatSub CCE.IntSub

mulOp :: BinaryOperatorBundle ext s 'SameK
mulOp = BinaryOperatorBundle CCE.BVMul CCE.NatMul CCE.IntMul

modOp :: BinaryOperatorBundle ext s 'SameK
modOp = BinaryOperatorBundle (error "BV mod not supported") CCE.NatMod CCE.IntMod

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
applyBinOp :: (CCE.IsSyntaxExtension ext)
           => BinaryOperatorBundle ext s rtp
           -> (AS.Expr, CCG.Atom s tp1)
           -> (AS.Expr, CCG.Atom s tp2)
           -> CCG.Generator ext h s (TranslationState h) ret (Some (CCG.Atom s))
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
    _ -> X.throw (UnsupportedComparisonType e1 (CCG.typeOfAtom a1))

bvBinOp :: (CCE.IsSyntaxExtension ext)
        => (forall n . (1 WT.<= n) => WT.NatRepr n -> CCG.Expr ext s (CT.BVType n) -> CCG.Expr ext s (CT.BVType n) -> CCE.App ext (CCG.Expr ext s) (CT.BVType n))
        -> (AS.Expr, CCG.Atom s tp1)
        -> (AS.Expr, CCG.Atom s tp2)
        -> CCG.Generator ext h s (TranslationState h) ret (Some (CCG.Atom s))
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
        _ -> X.throw (ExpectedBVType e1 (CCG.typeOfAtom a2))
    _ -> X.throw (ExpectedBVType e1 (CCG.typeOfAtom a1))

logicalBinOp :: (CCE.IsSyntaxExtension ext)
             => (CCG.Expr ext s CT.BoolType -> CCG.Expr ext s CT.BoolType -> CCE.App ext (CCG.Expr ext s) CT.BoolType)
             -> (AS.Expr, CCG.Atom s tp1)
             -> (AS.Expr, CCG.Atom s tp2)
             -> CCG.Generator ext h s (TranslationState h) ret (Some (CCG.Atom s))
logicalBinOp con (e1, a1) (e2, a2) = do
  Refl <- assertAtomType e1 CT.BoolRepr a1
  Refl <- assertAtomType e2 CT.BoolRepr a2
  Some <$> CCG.mkAtom (CCG.App (con (CCG.AtomExpr a1) (CCG.AtomExpr a2)))

translateUnaryOp :: Overrides arch
                 -> AS.UnOp
                 -> AS.Expr
                 -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s))
translateUnaryOp ov op expr = do
  Some atom <- translateExpr ov expr
  case op of
    AS.UnOpNot -> do
      Refl <- assertAtomType expr CT.BoolRepr atom
      Some <$> CCG.mkAtom (CCG.App (CCE.Not (CCG.AtomExpr atom)))
    AS.UnOpNeg ->
      case CCG.typeOfAtom atom of
        CT.BVRepr nr -> do
          Some <$> CCG.mkAtom (CCG.App (CCE.BVNot nr (CCG.AtomExpr atom)))
        _ -> X.throw (ExpectedBVType expr (CCG.typeOfAtom atom))

translateKnownVar :: (Some CT.TypeRepr)
                    -> AS.Identifier
                    -> Some (CCG.Atom s)
                    -> CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
translateKnownVar (Some expected) ident (Some atom) = do
  Refl <- assertAtomType' expected atom
  locals <- MS.gets tsVarRefs
  when (Map.member ident locals) $ do
    X.throw (LocalAlreadyDefined ident)
  reg <- CCG.newReg (CCG.AtomExpr atom)
  MS.modify' $ \s -> s { tsVarRefs = Map.insert ident (Some reg) locals }


overrides :: forall arch . Overrides arch
overrides = Overrides {..}
  where overrideStmt :: forall h s ret . AS.Stmt -> Maybe (CCG.Generator (ASLExt arch) h s (TranslationState h) ret ())
        overrideStmt s = case s of
          AS.StmtCall (AS.QualifiedIdentifier _ "ALUExceptionReturn") [_] -> Just $ do
            raiseException
          -- FIXME: write pc
          AS.StmtCall (AS.QualifiedIdentifier _ "ALUWritePC") [result] -> Just $ do
            return ()
          AS.StmtVarDeclInit (ident,ty) (AS.ExprCall (AS.QualifiedIdentifier _ "Zeros") []) -> Just $ do
            env <- MS.gets tsTypeEnvir
            retT <- translateType ty
            if | Some (CT.BVRepr valWidth) <- retT
               , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` valWidth
                 -> do
                  atom <- CCG.mkAtom (CCG.App (CCE.BVLit valWidth 0))
                  translateKnownVar retT ident (Some atom)
               | otherwise -> error $ "Invalid return type for Zeros():" <> show ty
          -- FIXME: This is a pass-by-reference function so we need to expand it in-place
          AS.StmtCall (AS.QualifiedIdentifier _ "SETTER_Elem") [_, _,_ ] -> Just $ do
            return ()
          AS.StmtCall (AS.QualifiedIdentifier _ "SETTER_Elem") [_, _,_, _ ] -> Just $ do
            return ()

          _ -> Nothing
        overrideExpr :: forall h s ret . AS.Expr -> Maybe (CCG.Generator (ASLExt arch) h s (TranslationState h) ret (Some (CCG.Atom s)))
        overrideExpr e = case e of
          AS.ExprCall (AS.QualifiedIdentifier _ "UInt")
            [AS.ExprMember (AS.ExprVarRef (AS.QualifiedIdentifier _ "DBGDIDR")) "WRPs"] -> Just $ do
              --Some e <- lookupVarRef "DBGDIDR_WRPs_UINT"
              --Some <$> CCG.mkAtom e
              Some <$> CCG.mkAtom (CCG.App (CCE.IntLit 10))
          AS.ExprCall (AS.QualifiedIdentifier _ "UInt") [argExpr] -> Just $ do
            Some atom <- translateExpr overrides argExpr
            case CCG.typeOfAtom atom of
              CT.BVRepr nr -> do
                Some <$> CCG.mkAtom (CCG.App (CCE.BvToInteger nr (CCG.AtomExpr atom)))
              _ -> error "Called UInt on non-bitvector"
          -- FIXME: BvToInteger isn't right here, because it's unsigned. We need a
          -- signed version.
          AS.ExprCall (AS.QualifiedIdentifier _ "SInt") [argExpr] -> Just $ do
            Some atom <- translateExpr overrides argExpr
            case CCG.typeOfAtom atom of
              CT.BVRepr nr -> do
                Some <$> CCG.mkAtom (CCG.App (CCE.BvToInteger nr (CCG.AtomExpr atom)))
              _ -> error "Called SInt on non-bitvector"
          AS.ExprCall (AS.QualifiedIdentifier _ "IsZero") [argExpr] -> Just $ do
            Some atom <- translateExpr overrides argExpr
            case CCG.typeOfAtom atom of
              CT.BVRepr nr -> do
                Some <$> CCG.mkAtom (CCG.App (CCE.BVEq nr (CCG.AtomExpr atom) (CCG.App (CCE.BVLit nr 0))))
              _ -> error "Called IsZero on non-bitvector"
          AS.ExprCall (AS.QualifiedIdentifier _ "ZeroExtend") [val, e] -> Just $ do
            Some valAtom <- translateExpr overrides val
            env <- MS.gets tsTypeEnvir
            case (CCG.typeOfAtom valAtom, exprToInt env e) of
              (CT.BVRepr valWidth, Just i) |
                i == WT.intValue valWidth -> do
                  return $ Some valAtom
              (CT.BVRepr valWidth, Just i)
                | Just (Some repr) <- WT.someNat i
                , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` repr
                , Just WT.LeqProof <- (valWidth `WT.addNat` (WT.knownNat @1)) `WT.testLeq` repr -> do
                  atom <- CCG.mkAtom (CCG.App (CCE.BVZext repr valWidth (CCG.AtomExpr valAtom)))
                  return $ Some atom
              (_, Nothing) -> X.throw $ RequiredConcreteValue e
              (tp, _) -> X.throw $ ExpectedBVType val tp
          AS.ExprCall (AS.QualifiedIdentifier _ "Zeros") [e] -> Just $ do
            env <- MS.gets tsTypeEnvir
            case exprToInt env e of
              Just width |
                  Just (Some valWidth) <- WT.someNat width
                , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` valWidth -> do
                  atom <- CCG.mkAtom (CCG.App (CCE.BVLit valWidth 0))
                  return $ Some atom
              _ ->  error $ "Invalid argument type for Zeros(int):" <> show e
          AS.ExprCall (AS.QualifiedIdentifier _ "Ones") [e] -> Just $ do
            env <- MS.gets tsTypeEnvir
            case exprToInt env e of
              Just width |
                  Just (Some valWidth) <- WT.someNat width
                , Just WT.LeqProof <- (WT.knownNat @1) `WT.testLeq` valWidth -> do
                  let zeros = CCG.App (CCE.BVLit valWidth 0)
                  atom <- CCG.mkAtom (CCG.App (CCE.BVNot valWidth zeros))
                  return $ Some atom
              _ ->  error $ "Invalid argument type for Zeros(int):" <> show e
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
            return $ Some structAtom
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
            return $ Some structAtom
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
            return $ Some structAtom
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
            return $ Some structAtom
          -- FIXME: fix definition below to actually get the "cond" local variable
          AS.ExprCall (AS.QualifiedIdentifier _ "CurrentCond") [] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.BVLit (WT.knownNat @4) 0))
            return $ Some atom
          -- FIXME: implement this (asl definition is recursive and dependently typed)
          AS.ExprCall (AS.QualifiedIdentifier _ "BigEndianReverse") [x] -> Just $ do
            Some xAtom <- translateExpr overrides x
            atom <- CCG.mkAtom (CCG.AtomExpr xAtom)
            return $ Some atom
          -- FIXME: implement this (asl definition is recursive and dependently typed)
          -- There are two overloadings of this based on the type of x
          AS.ExprCall (AS.QualifiedIdentifier _ "Align") [x, y] -> Just $ do
            Some xAtom <- translateExpr overrides x
            atom <- CCG.mkAtom (CCG.AtomExpr xAtom)
            return $ Some atom
          -- FIXME: There are two overloadings of this
          AS.ExprCall (AS.QualifiedIdentifier _ "IsExternalAbort") [x] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.BoolLit False))
            return $ Some atom
          -- FIXME: There are two overloadings of this
          AS.ExprCall (AS.QualifiedIdentifier _ "IsExternalAbort") [] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.BoolLit False))
            return $ Some atom
          -- FIXME: There are two overloadings of this
          AS.ExprCall (AS.QualifiedIdentifier _ "IsAsyncAbort") [x] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.BoolLit False))
            return $ Some atom
          -- FIXME: There are two overloadings of this
          AS.ExprCall (AS.QualifiedIdentifier _ "IsExternalSyncAbort") [x] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.BoolLit False))
            return $ Some atom
          AS.ExprCall (AS.QualifiedIdentifier _ "IsSErrorInterrupt") [x] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.BoolLit False))
            return $ Some atom
          AS.ExprCall (AS.QualifiedIdentifier _ "HaveFP16Ext") [] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.BoolLit False))
            return $ Some atom
          AS.ExprCall (AS.QualifiedIdentifier _ "Unreachable") [] -> Just $ do
            atom <- CCG.assertExpr (CCG.App (CCE.BoolLit False)) (CCG.App (CCE.TextLit "Unreachable"))
            Some <$> CCG.mkAtom (CCG.App CCE.EmptyApp)
          AS.ExprCall (AS.QualifiedIdentifier _ "LSInstructionSyndrome") [] -> Just $ do
            Some <$> CCG.mkAtom (CCG.App (CCE.BVUndef (WT.knownNat @11)))
          _ -> Nothing



-- FIXME: Change this to set some global flag?
raiseException :: CCG.Generator (ASLExt arch) h s (TranslationState h) ret ()
raiseException = return ()



