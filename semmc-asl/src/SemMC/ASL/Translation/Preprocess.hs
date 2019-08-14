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
    computeDefinitions
  , computeInstructionSignature
  , prepASL
  , foldASL
  , Callable(..)
  ) where

import Debug.Trace (traceM)

import qualified Control.Exception as X
import           Control.Monad (void)
import qualified Control.Monad.Except as E
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State.Class as MS
import qualified Data.BitVector.Sized as BVS
import           Data.Foldable (find)
import           Data.List (nub)
import           Data.Maybe (maybeToList, catMaybes, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )
-- import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Traversable (forM)
import qualified Lang.Crucible.CFG.Expr as CCE
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Extension
import           SemMC.ASL.Signature
import           SemMC.ASL.Translation
import           SemMC.ASL.Crucible
  ( Definitions(..)
  , ASLExt
  )
import           SemMC.ASL.Exceptions

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
computeSignature :: T.Text -> Int -> SigM ext f ()
computeSignature name arity = do
  mCallable <- lookupCallable name arity
  case mCallable of
    Nothing -> E.throwError $ CallableNotFound name
    Just c -> void $ computeCallableSignature c

computeSignature' :: T.Text -> SigM ext f ()
computeSignature' name = do
  mCallable <- lookupCallable' name
  case mCallable of
    Nothing -> E.throwError $ CallableNotFound name
    Just c -> void $ computeCallableSignature c

-- | Compute the signature of a list of functions and procedures, given their names,
-- from the list of ASL definitions. If the callables requested call other functions
-- or procedures, we compute their signatures as well and include them in the result.
computeDefinitions :: [T.Text]
                   -- ^ The names should be qualified with "_n", where n is the
                   -- number of arguments
                   -> [AS.Definition]
                   -> Either SigException (Definitions arch)
computeDefinitions names defs = execSigM defs $ do
  mapM_ computeSignature' names
  st <- RWS.get
  env <- RWS.ask
  return $ Definitions
    { defSignatures = (\(sig, c) -> (sig, callableStmts c)) <$> callableSignatureMap st
    , defDepSignatures = Map.empty
    , defTypes = userTypes st
    , defEnums = enums env
    , defConsts = consts env
    , defOverrides = overrides
    }

computeInstructionSignature :: T.Text
                            -- ^ name of instruction
                            -> T.Text
                            -- ^ name of encoding
                            -> [AS.Instruction]
                            -- ^ list of loaded instructinos
                            -> [AS.Definition]
                            -- ^ list of loaded definitions
                            -> Either SigException (Some SomeSignature, [AS.Stmt], Map.Map T.Text (Some SomeSignature, Callable))
computeInstructionSignature instName encName insts defs = execSigM defs $
  case find (\i -> AS.instName i == instName) insts of
    Nothing -> error $ "couldn't find instruction " ++ show instName
    Just i -> do
      (sig, stmts) <- computeInstructionSignature' i encName
      sigMap <- callableSignatureMap <$> RWS.get
      return (sig, stmts, sigMap)

overrides :: forall arch . Overrides arch
overrides = Overrides {..}
  where overrideStmt :: forall h s ret . AS.Stmt -> Maybe (CCG.Generator (ASLExt arch) h s TranslationState ret ())
        overrideStmt s = case s of
          AS.StmtCall (AS.QualifiedIdentifier _ "ALUExceptionReturn") [_] -> Just $ do
            raiseException
          -- FIXME: write pc
          AS.StmtCall (AS.QualifiedIdentifier _ "ALUWritePC") [result] -> Just $ do
            return ()
          _ -> Nothing
        overrideExpr :: forall h s ret . AS.Expr -> Maybe (CCG.Generator (ASLExt arch) h s TranslationState ret (Some (CCG.Atom s)))
        overrideExpr e = case e of
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
          -- FIXME: ZeroExtend defaults to 64 for single arguments...
          AS.ExprCall (AS.QualifiedIdentifier _ "ZeroExtend") [val] -> Just $ do
            Some valAtom <- translateExpr overrides val
            case CCG.typeOfAtom valAtom of
              CT.BVRepr valWidth
                | Just WT.LeqProof <- (valWidth `WT.addNat` (WT.knownNat @1)) `WT.testLeq` (WT.knownNat @64) -> do
                    atom <- CCG.mkAtom (CCG.App (CCE.BVZext (WT.knownNat @64) valWidth (CCG.AtomExpr valAtom)))
                    return $ Some atom
          AS.ExprCall (AS.QualifiedIdentifier _ "ZeroExtend") [val, AS.ExprLitInt 32] -> Just $ do
            Some valAtom <- translateExpr overrides val
            case CCG.typeOfAtom valAtom of
              CT.BVRepr valWidth
                | Just WT.LeqProof <- (valWidth `WT.addNat` (WT.knownNat @1)) `WT.testLeq` (WT.knownNat @32) -> do
                  atom <- CCG.mkAtom (CCG.App (CCE.BVZext (WT.knownNat @32) valWidth (CCG.AtomExpr valAtom)))
                  return $ Some atom
              tp -> X.throw $ ExpectedBVType val tp
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
          AS.ExprCall (AS.QualifiedIdentifier _ "__BVTOINT32") [AS.ExprLitInt i] -> Just $ do
            atom <- CCG.mkAtom (CCG.App (CCE.IntegerToBV (WT.knownNat @32)
                                         (CCG.App (CCE.IntLit i))))
            return $ Some atom
          _ -> Nothing

-- FIXME: Change this to set some global flag?
raiseException :: CCG.Generator (ASLExt arch) h s TranslationState ret ()
raiseException = return ()

builtinGlobals :: [(T.Text, Some WT.BaseTypeRepr)]
builtinGlobals = [ ("PSTATE_N", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_Z", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_C", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_V", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_D", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_A", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_I", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_F", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_PAN", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_UAO", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_SS", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_IL", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_EL", Some (WT.BaseBVRepr (WT.knownNat @2)))
                 , ("PSTATE_nRW", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_SP", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_Q", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_GE", Some (WT.BaseBVRepr (WT.knownNat @4)))
                 , ("PSTATE_IT", Some (WT.BaseBVRepr (WT.knownNat @8)))
                 , ("PSTATE_J", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_T", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_E", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("PSTATE_M", Some (WT.BaseBVRepr (WT.knownNat @5)))
                 , ("SCR_EL3_EA", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("SCR_NS", Some (WT.BaseBVRepr (WT.knownNat @1)))
                 , ("UNDEFINED", Some WT.BaseBoolRepr)
                 , ("UNPREDICTABLE", Some WT.BaseBoolRepr)
                 , ("_PC", Some (WT.BaseBVRepr (WT.knownNat @64)))
                 , ("_R", Some (WT.BaseArrayRepr
                                (Ctx.empty Ctx.:> WT.BaseIntegerRepr)
                                (WT.BaseBVRepr (WT.knownNat @64))))
                 , ("SP_mon", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 , ("LR_mon", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 ]

builtinConsts :: [(T.Text, Some ConstVal)]
builtinConsts =
  [ ("TRUE", Some $ ConstVal WT.BaseBoolRepr True)
  , ("FALSE", Some $ ConstVal WT.BaseBoolRepr False)
  ]

-- FIXME: We currently do not capture 'DefArray', 'DefGetter', and 'DefSetter'
-- constructors; that needs to happen.

data Callable = Callable { callableName :: AS.QualifiedIdentifier
                         , callableArgs :: [AS.SymbolDecl]
                         , callableRets :: [AS.Type]
                         , callableStmts :: [AS.Stmt]
                         }
  deriving Show

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
  deriving Show

mkCallableName :: Callable -> T.Text
mkCallableName c =
  let AS.QualifiedIdentifier _ name =  callableName c
      numArgs = length (callableArgs c)
  in mkFunctionName name numArgs

mkGetterName :: T.Text -> T.Text
mkGetterName name = "GETTER_" <> name

mkSetterName :: T.Text -> T.Text
mkSetterName name = "SETTER_" <> name

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
newtype SigM ext f a = SigM { getSigM :: E.ExceptT SigException (RWS.RWS (SigEnv ext f) () SigState) a }
  deriving ( Functor
           , Applicative
           , Monad
           , RWS.MonadReader (SigEnv ext f)
           , RWS.MonadState SigState
           , E.MonadError SigException
           )


mkSyntaxOverrides :: [AS.Definition] -> SyntaxOverrides
mkSyntaxOverrides defs =
  let getters = Set.fromList $ catMaybes $ getterName <$> defs
      setters = Set.fromList $ catMaybes $ setterName <$> defs

      getterName d = case d of
        -- FIXME: This is either SCR or SCR_EL3 based on the architecture
        AS.DefGetter (AS.QualifiedIdentifier _ "SCR_GEN") _ _ _ ->
           Nothing
        AS.DefGetter (AS.QualifiedIdentifier _ name) args _ _ ->
          Just (mkFunctionName (mkGetterName name) (length args))
        _ -> Nothing

      setterName d = case d of
        AS.DefSetter (AS.QualifiedIdentifier _ name) args _ _ ->
           Just (mkFunctionName (mkSetterName name) (length args + 1))
        _ -> Nothing

      -- This is not complete, since in general we might encounter args followed by an actual slice.
      -- It may be necessary to guess what prefix of the slices corresponds to getter/setter
      -- arguments and what is an actual slice of the result.
      getSliceExpr slice = case slice of
        AS.SliceSingle e -> e
        _ -> error "Unexpected slice argument."


      --FIXME: There are a few cases of tuple assignments with setters that need to
      --be specially handled
      
      stmtOverrides stmt = case stmt of
        AS.StmtAssign (AS.LValArrayIndex (AS.LValVarRef (AS.QualifiedIdentifier q ident)) slices) rhs ->
          if Set.member (mkFunctionName (mkSetterName ident) (length slices + 1)) setters then
            AS.StmtCall (AS.QualifiedIdentifier q (mkSetterName ident)) (rhs : map getSliceExpr slices)
          else stmt
        AS.StmtAssign (AS.LValVarRef (AS.QualifiedIdentifier q ident)) rhs ->
          if Set.member (mkFunctionName (mkSetterName ident) 1) setters then
            AS.StmtCall (AS.QualifiedIdentifier q (mkSetterName ident)) [rhs]
          else stmt
        _ -> stmt
        
      exprOverrides expr = case expr of
         -- FIXME: This is either SCR or SCR_EL3 based on the architecture
        AS.ExprMember (AS.ExprIndex (AS.ExprVarRef (AS.QualifiedIdentifier q "SCR_GEN")) []) mem ->
          AS.ExprMember (AS.ExprVarRef (AS.QualifiedIdentifier q "SCR")) mem
        AS.ExprIndex (AS.ExprVarRef (AS.QualifiedIdentifier q ident)) slices ->
          if Set.member (mkFunctionName (mkGetterName ident) (length slices)) getters then
            AS.ExprCall (AS.QualifiedIdentifier q (mkGetterName ident)) (map getSliceExpr slices)
          else expr
        AS.ExprVarRef (AS.QualifiedIdentifier q ident) ->
          if Set.member (mkFunctionName (mkGetterName ident) 0) getters then
            AS.ExprCall (AS.QualifiedIdentifier q (mkGetterName ident)) []
          else expr
        _ -> expr

  in SyntaxOverrides stmtOverrides exprOverrides


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
foldInstruction f' h' g' (AS.Instruction ident instEncodings instPostDecode instExecute) b =
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
    

applySyntaxOverridesDefs :: SyntaxOverrides -> [AS.Definition] -> [AS.Definition]
applySyntaxOverridesDefs ovrs defs =
  let
    g = applyStmtSyntaxOverride ovrs
    f = applyExprSyntaxOverride ovrs


    -- TODO: For sanity we delete setter definitions which require
    -- pass-by-reference since we don't have a sane semantics for this
    
    argName (AS.SetterArg name False) = Just name
    argName _ = Nothing
    
    mapDefs d = case d of
      AS.DefCallable qName args rets stmts ->
        Just $ AS.DefCallable qName args rets (g <$> stmts)
      AS.DefGetter (AS.QualifiedIdentifier q name) args rets stmts ->
        Just AS.DefCallable { callableName = AS.QualifiedIdentifier q (mkGetterName name)
                       , callableArgs = args
                       , callableRets = rets
                       , callableStmts = g <$> stmts
                       }
      AS.DefSetter (AS.QualifiedIdentifier q name) args rhs stmts -> do
        argNames <- sequence (argName <$> args)
        Just $ AS.DefCallable { callableName = AS.QualifiedIdentifier q (mkSetterName name)
                       , callableArgs = rhs : argNames
                       , callableRets = []
                       , callableStmts = g <$> stmts
                       }
      AS.DefConst i t e -> Just $ AS.DefConst i t (f e)
      _ -> Just d

  in catMaybes $ mapDefs <$> defs

applySyntaxOverridesInstrs :: SyntaxOverrides -> [AS.Instruction] -> [AS.Instruction]
applySyntaxOverridesInstrs ovrs instrs =
  let
    g = applyStmtSyntaxOverride ovrs
    
    mapInstr (AS.Instruction instName instEncodings instPostDecode instExecute) =
      AS.Instruction instName instEncodings (g <$> instPostDecode) (g <$> instExecute)

  in mapInstr <$> instrs



-- Syntactic overrides for entire definitions.
callableOverrides :: Map.Map T.Text [AS.Stmt]
callableOverrides = Map.fromList [
  -- In the getter for R we have int literals that should actually be interpreted
  -- a 32 bit bv
  ("GETTER_R", [AS.StmtIf [(AS.ExprBinOp AS.BinOpEQ (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "n")) (AS.ExprLitInt 15),[AS.StmtAssign (AS.LValVarRef (AS.QualifiedIdentifier AS.ArchQualAny "offset")) (AS.ExprIf [(AS.ExprBinOp AS.BinOpEQ (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "CurrentInstrSet") []) (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "InstrSet_A32")),wrapLitInt32 8)] (wrapLitInt32 4)),AS.StmtReturn (Just (AS.ExprBinOp AS.BinOpAdd (AS.ExprSlice (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "_PC")) [AS.SliceRange (AS.ExprLitInt 31) (AS.ExprLitInt 0)]) (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "offset"))))])] (Just [AS.StmtReturn (Just (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "GETTER_Rmode") [AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "n"),AS.ExprMember (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "PSTATE")) "M"]))])] ),
  -- Needs to be capped with a return
  -- FIXME: The the assert seems to be tripping the simulator during globals collection
  -- AS.StmtAssert (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "FALSE")),
  ("Unreachable", [AS.StmtReturn Nothing])
  ]
  where wrapLitInt32 i = AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "__BVTOINT32") [AS.ExprLitInt i]



prepASL :: ([AS.Instruction], [AS.Definition]) -> ([AS.Instruction], [AS.Definition])
prepASL (instrs, defs) =
  let ovrs = mkSyntaxOverrides defs

      mapDefs d = case d of
        AS.DefCallable (AS.QualifiedIdentifier q name) args rets _ ->
          case Map.lookup name callableOverrides of
            Just stmts' -> AS.DefCallable (AS.QualifiedIdentifier q name) args rets stmts'
            _ -> d
        _ -> d
  in (applySyntaxOverridesInstrs ovrs instrs, mapDefs <$> applySyntaxOverridesDefs ovrs defs)
      

-- | Given the top-level list of definitions, build a 'SigEnv' for preprocessing the
-- signatures.
buildEnv :: [AS.Definition] -> SigEnv ext f
buildEnv defs =
  let envCallables = Map.fromList ((\c -> (mkCallableName c, c)) <$> (catMaybes (asCallable <$> defs)))
      globalVars = Map.fromList builtinGlobals


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

-- | Given a list of ASL 'AS.Definition's, execute a 'SigM' action and either return
-- the result or an exception coupled with the final state.
execSigM :: [AS.Definition] -> SigM ext f a -> Either SigException a
execSigM defs action =
  let rws = E.runExceptT $ getSigM action
      (e, _, _) = RWS.runRWS rws (buildEnv defs) initState
  in case e of
    Left err -> Left err
    Right a -> Right a
  where initState = SigState Map.empty Map.empty Map.empty


-- | Syntactic-level expansions that should happen aggressively before
-- any interpretation.
data SyntaxOverrides = SyntaxOverrides { stmtOverrides :: AS.Stmt -> AS.Stmt
                                       , exprOverrides :: AS.Expr -> AS.Expr }



data SigEnv ext s = SigEnv { envCallables :: Map.Map T.Text Callable
                           -- , globalVars :: Map.Map T.Text DefVariable
                           , globalVars :: Map.Map T.Text (Some WT.BaseTypeRepr)
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
                         , callableSignatureMap :: Map.Map T.Text (Some SomeSignature, Callable)
                           -- ^ map of all signatures found thus far
                         -- , unfoundCallables :: Seq.Seq T.Text
                         --   -- ^ list of callables we encountered that were not in the
                         --   -- pre-loaded environment
                         }

data SigException = TypeNotFound T.Text
                  | BuiltinTypeNotFound T.Text
                  | CallableNotFound T.Text
                  | VariableNotFound T.Text
                  | WrongType T.Text T.Text
                  | StructMissingField T.Text T.Text
  deriving (Eq, Show)

storeType :: T.Text -> UserType tp -> SigM ext f ()
storeType tpName tp = do
  st <- RWS.get
  RWS.put $ st { userTypes = Map.insert tpName (Some tp) (userTypes st) }

lookupCallable :: T.Text -> Int -> SigM ext f (Maybe Callable)
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
    Nothing -> E.throwError $ BuiltinTypeNotFound tpName

lookupDefType :: T.Text -> SigM ext f DefType
lookupDefType tpName = do
  env <- RWS.ask
  case Map.lookup tpName (types env) of
    Just defType -> return defType
    Nothing -> E.throwError $ TypeNotFound tpName

-- | If the variable is present, return its definition. Otherwise, return 'Nothing'.
--lookupGlobalVar :: T.Text -> SigM ext f (Maybe DefVariable)
lookupGlobalVar :: T.Text -> SigM ext f (Maybe (Some WT.BaseTypeRepr))
lookupGlobalVar varName = do
  env <- RWS.ask
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

lookupCallableSignature :: Callable -> SigM ext f (Maybe (Some SomeSignature))
lookupCallableSignature c = do
  signatureMap <- callableSignatureMap <$> RWS.get
  let name = mkCallableName c
  return $ (fst <$> Map.lookup name signatureMap)

storeCallableSignature :: Callable -> SomeSignature ret -> SigM ext f()
storeCallableSignature c sig = do
  st <- RWS.get
  let name = mkCallableName c
  RWS.put $ st { callableSignatureMap = Map.insert name (Some sig, c) (callableSignatureMap st) }


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
            Some tp <- computeType varType
            return $ Some $ LabeledValue varName tp
          Some varTpAssignment <- return $ Ctx.fromList varTps
          return $ Some $ UserStruct varTpAssignment
        DefTypeAbstract _ -> error $ "computeUserType: abstract type " ++ show tpName
        _ -> error $ "computeUserType: unsupported type " ++ T.unpack tpName
      storeType tpName tp
      return $ Some tp

-- | Compute the What4 representation of an ASL 'AS.Type'.
computeType :: AS.Type -> SigM ext f (Some WT.BaseTypeRepr)
computeType tp = case tp of
  AS.TypeRef (AS.QualifiedIdentifier _ tpName) -> do
    case tpName of
      "integer" -> return (Some WT.BaseIntegerRepr)
      "boolean" -> return (Some WT.BaseBoolRepr)
      "bit" -> return (Some (WT.BaseBVRepr (NR.knownNat @1)))
      _ -> do
        Some userType <- computeUserType tpName
        return $ Some $ userTypeRepr userType
  AS.TypeFun "bits" e ->
    case e of
      AS.ExprLitInt w
        | Just (Some wRepr) <- NR.someNat w
        , Just NR.LeqProof <- NR.isPosNat wRepr -> return $ Some (WT.BaseBVRepr wRepr)
      -- FIXME: For now, we interpret polymorphic bits(N) as bits(32).
      AS.ExprVarRef (AS.QualifiedIdentifier _ _) -> return $ Some (WT.BaseBVRepr (WT.knownNat @32))
      _ -> error "computeType, TypeFun"
  AS.TypeOf _ -> error "computeType, TypeOf"
  AS.TypeReg _ _ -> error "computeType, TypeReg"
  AS.TypeArray _ _ -> error "computeType, TypeArray"
  _ -> error "computeType"

-- | If the identifier is a global variable, return its type. Otherwise, return
-- 'Nothing', indicating the variable is not global.
computeGlobalVarType :: T.Text -> SigM ext f (Maybe (Some WT.BaseTypeRepr))
computeGlobalVarType varName = do
  lookupGlobalVar varName

-- | Compute the type of a struct member. If the struct is not a global variable,
-- return 'Nothing'.
computeGlobalStructMemberType :: T.Text -> T.Text -> SigM ext f (Maybe (Some WT.BaseTypeRepr))
computeGlobalStructMemberType structName memberName = do
  lookupGlobalVar (mkStructMemberName structName memberName)

-- | Given a variable name, determine whether it is a global variable or not. If so,
-- return a pair containing the variable and its type; if not, return 'Nothing'.
varGlobal :: T.Text -> SigM ext f (Maybe (T.Text, Some WT.BaseTypeRepr))
varGlobal varName = do
  mVarType <- computeGlobalVarType varName
  case mVarType of
    Nothing -> return Nothing
    Just varType -> return $ Just (varName, varType)

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
  AS.LValMember (AS.LValVarRef (AS.QualifiedIdentifier _ structName)) memberName -> do
    mVarType <- computeGlobalStructMemberType structName memberName
    case mVarType of
      Nothing -> return []
      Just varType -> return [(mkStructMemberName structName memberName, varType)]
  AS.LValMember _ _ -> return [] -- error "lValExprGlobalVars"
  AS.LValMemberBits (AS.LValVarRef (AS.QualifiedIdentifier _ structName)) memberNames -> do
    mVarTypes <- forM memberNames $ \memberName -> do
      mVarType <- computeGlobalStructMemberType structName memberName
      case mVarType of
        Nothing -> return []
        Just varType -> return [(mkStructMemberName structName memberName, varType)]
    return $ concat mVarTypes
  AS.LValMemberBits _ _ -> return [] -- error "lValExprGlobalVars"
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
  -- FIXME: Attach a list of global variables to every override
  Just _ -> return []
  Nothing -> case expr of
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
    AS.ExprCall (AS.QualifiedIdentifier _ name) argEs -> do
      argGlobals <- concat <$> traverse exprGlobalVars argEs
      mCallable <- lookupCallable name (length argEs)
      case mCallable of
        Just callable -> do
          -- Compute the signature of the callable
          void $ computeCallableSignature callable
          callableGlobals <- callableGlobalVars callable
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
    -- AS.ExprMember e var -> do
      -- eGlobals <- exprGlobalVars e
      -- varGlobals <- maybeToList <$> varGlobal var
      -- return $ eGlobals ++ varGlobals
    AS.ExprMember (AS.ExprVarRef (AS.QualifiedIdentifier _ structName)) memberName -> do
      mVarType <- computeGlobalStructMemberType structName memberName
      case mVarType of
        Nothing -> return []
        Just varType -> return [(mkStructMemberName structName memberName, varType)]
    AS.ExprMember _ _ -> X.throw $ UnsupportedExpr expr
    AS.ExprMemberBits (AS.ExprVarRef (AS.QualifiedIdentifier _ structName)) memberNames -> do
      mVarTypes <- forM memberNames $ \memberName -> do
        mVarType <- computeGlobalStructMemberType structName memberName
        case mVarType of
          Nothing -> return []
          Just varType -> return [(mkStructMemberName structName memberName, varType)]
      return $ concat mVarTypes
    AS.ExprMemberBits _ _ -> X.throw $ UnsupportedExpr expr
    _ -> return []

-- | Collect all global variables from a single 'AS.Stmt'.
stmtGlobalVars :: AS.Stmt -> SigM ext f [(T.Text, Some WT.BaseTypeRepr)]
stmtGlobalVars stmt =
  -- FIXME: If the stmt has an override, then we should provide a custom set of
  -- globals as well.
  case overrideStmt overrides stmt of
    Just _ -> return []
    Nothing -> case stmt of
      AS.StmtVarDeclInit _ e -> exprGlobalVars e
      AS.StmtAssign le e -> (++) <$> lValExprGlobalVars le <*> exprGlobalVars e
      AS.StmtCall (AS.QualifiedIdentifier _ name) argEs -> do
        argGlobals <- concat <$> traverse exprGlobalVars argEs
        mCallable <- lookupCallable name (length argEs)
        case mCallable of
          Just callable -> do
            -- Compute the signature of the callable
            void $ computeCallableSignature callable
            callableGlobals <- callableGlobalVars callable
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
        gb <- varGlobal "UNPREDICTABLE"
        case gb of
          Just g -> return [g]
          _ -> error "Missing global variable: UNPREDICTABLE"
      AS.StmtUndefined -> do
        gb <- varGlobal "UNDEFINED"
        case gb of
          Just g -> return [g]
          _ -> error "Missing global variable: UNDEFINED"
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
computeCallableSignature :: Callable -> SigM ext f (Some SomeSignature)
computeCallableSignature c@Callable{..} = do
  let name = mkCallableName c
  mSig <- lookupCallableSignature c

  case mSig of
    Just sig -> return sig
    Nothing -> do
      globalVars <- callableGlobalVars c
      labeledVals <- forM (nub globalVars) $ \(varName, Some varTp) -> do
        return $ Some (LabeledValue varName varTp)
      labeledArgs <- forM callableArgs $ \(argName, asType) -> do
        Some tp <- computeType asType
        let ctp = CT.baseToType tp
        return (Some (LabeledValue argName ctp))

      Some globalReprs <- return $ Ctx.fromList labeledVals
      Some argReprs <- return $ Ctx.fromList labeledArgs
      Some sig <- case callableRets of
        [] -> -- procedure
          return $ Some $ SomeProcedureSignature $ ProcedureSignature
            { procName = name
            , procArgReprs = argReprs
            , procGlobalReprs = globalReprs
            }
        _ -> do -- function
          Some sigRepr <- case callableRets of
            [asType] -> computeType asType
            asTypes -> do
              someTypes <- traverse computeType asTypes
              Some assignment <- return $ Ctx.fromList someTypes
              return $ Some (WT.BaseStructRepr assignment)
          return $ Some $ SomeFunctionSignature $ FunctionSignature
            { funcName = name
            , funcSigRepr = sigRepr
            , funcArgReprs = argReprs
            , funcGlobalReprs = globalReprs
            }
      storeCallableSignature c sig
      return (Some sig)

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
                             -> SigM ext f (Some SomeSignature, [AS.Stmt])
computeInstructionSignature' AS.Instruction{..} encName = do
  let name = mkInstructionName instName encName

  let mEnc = find (\e -> AS.encName e == encName) instEncodings
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
                                    }

      return (Some (SomeProcedureSignature pSig), instStmts)

-- | Create the full list of statements in an instruction given the main execute
-- block and the encoding-specific operations.
createInstStmts :: [AS.Stmt]
                -- ^ Encoding-specific operations
                -> [AS.Stmt]
                -- ^ Execute block
                -> [AS.Stmt]
createInstStmts encodingSpecificOperations stmts = case stmts of
  [AS.StmtIf
    [ ( (AS.ExprCall (AS.QualifiedIdentifier archQual "ConditionPassed") [])
      , (AS.StmtCall (AS.QualifiedIdentifier _ "EncodingSpecificOperations") [] : rst) ) ]
    Nothing] ->
    [AS.StmtIf
      [ ( (AS.ExprCall (AS.QualifiedIdentifier archQual "ConditionPassed") [])
        , encodingSpecificOperations ++ rst ) ]
      Nothing]
  _ -> error "createInstStmts"
