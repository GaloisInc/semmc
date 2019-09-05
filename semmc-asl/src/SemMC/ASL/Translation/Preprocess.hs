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
  , foldExpr
  , SigState
  , Callable(..)
  , Definitions(..)
  , bitsToInteger
  , mkFunctionName
  , mkStructMemberName
  , applyTypeEnvir
  , exprToInt
  , exprToBool
  , mkSignature
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
import           Data.Parameterized.Some ( Some(..), viewSome, mapSome )
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
import           SemMC.ASL.Types
--import           SemMC.ASL.Translation
--import           SemMC.ASL.Crucible
--  ( Definitions(..)
--  , ASLExt
--  )
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
computeSignature :: AS.QualifiedIdentifier -> Int -> SigM ext f ()
computeSignature qName arity = do
  mCallable <- lookupCallable qName arity
  case mCallable of
    Nothing -> E.throwError $ CallableNotFound (mkFunctionName qName arity)
    Just c -> void $ computeCallableSignature c

computeSignature' :: T.Text -> SigM ext f ()
computeSignature' name = do
  mCallable <- lookupCallable' name
  case mCallable of
    Nothing -> E.throwError $ CallableNotFound name
    Just c -> void $ computeCallableSignature c


data Definitions arch =
  Definitions { defSignatures :: Map.Map T.Text (SomeSimpleSignature, [AS.Stmt])
              , defDepSignatures :: Map.Map T.Text (SomeDFS, [AS.Stmt])
              , defTypes :: Map.Map T.Text (Some UserType)
              , defEnums :: Map.Map T.Text Integer
              , defConsts :: Map.Map T.Text (Some ConstVal)
              }

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
    }

computeInstructionSignature :: T.Text
                            -- ^ name of instruction
                            -> T.Text
                            -- ^ name of encoding
                            -> [AS.Instruction]
                            -- ^ list of loaded instructinos
                            -> [AS.Definition]
                            -- ^ list of loaded definitions
                            -> Either SigException (Some SomeSignature, [AS.Stmt], Map.Map T.Text (SomeSimpleSignature, Callable))
computeInstructionSignature instName encName insts defs = execSigM defs $
  case find (\i -> AS.instName i == instName) insts of
    Nothing -> error $ "couldn't find instruction " ++ show instName
    Just i -> do
      (sig, stmts) <- computeInstructionSignature' i encName
      sigMap <- callableSignatureMap <$> RWS.get
      return (sig, stmts, sigMap)


builtinGlobals :: [(T.Text, Some WT.BaseTypeRepr)]
builtinGlobals = [ ("UNDEFINED", Some WT.BaseBoolRepr)
                 , ("UNPREDICTABLE", Some WT.BaseBoolRepr)
                 , ("ThisInstrLength", Some WT.BaseIntegerRepr)
                 , ("_PC", Some (WT.BaseBVRepr (WT.knownNat @64)))
                 , ("_R", Some (WT.BaseArrayRepr
                                (Ctx.empty Ctx.:> WT.BaseIntegerRepr)
                                (WT.BaseBVRepr (WT.knownNat @64))))
                 , ("SP_mon", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 , ("LR_mon", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 , ("HVBAR", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 , ("HIFAR", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 , ("HDFAR", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 , ("HPFAR", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 , ("HPFAR_EL2", Some (WT.BaseBVRepr (WT.knownNat @64)))
                 , ("MVBAR", Some (WT.BaseBVRepr (WT.knownNat @32)))
                 ] ++
                 concat (mkGlobalStruct <$>
                         [("PSTATE", "PSTATEType")
                         ,("SCTLR_EL1", "SCTLRType")
                         ,("SCTLR_EL2", "SCTLRType")
                         ,("SCTLR_EL3", "SCTLRType")
                         ,("CNTKCTL_EL1", "CNTKCTLType")
                         ,("CNTKCTL_EL2", "CNTKCTLType")
                         ,("ESR_EL1", "ESRType")
                         ,("ESR_EL2", "ESRType")
                         ,("ESR_EL3", "ESRType")
                         ,("MAIR_EL1", "MAIRType")
                         ,("MAIR_EL2", "MAIRType")
                         ,("MAIR_EL3", "MAIRType")
                         ,("SCR", "SCRType")
                         ,("SCR_EL3", "SCRType")
                         ,("HSCTLR", "HSCTLRType")
                         ,("HCR_EL2", "HCRType")
                         ,("HCR2", "HCRType")
                         ,("HCR", "HCRType")
                         ,("HDCR", "HDCRType")
                         ,("MDCR_EL2", "MDCRType")
                         ,("DBGDSCRext", "DBGDSCRextType")
                         ,("VBAR_EL1", "VBARType")
                         ,("VBAR_EL2", "VBARType")
                         ,("VBAR_EL3", "VBARType")
                         ,("TTBCR", "TTBCRType")
                         ,("TTBCR_S", "TTBCRType")
                         ])
  where
    mkGlobalStruct (nm,tnm) =
      case lookup tnm globalStructTypes of
        Just fields -> mkField nm <$> fields
        _ -> case lookup tnm globalTypeSynonyms of
          Just syn -> [(nm, toSimpleBVType syn)]
          _  -> error $ "Missing global struct: " <> (T.unpack tnm)
    mkField nm (fnm, ftype) = (nm <> "_" <> fnm, toSimpleBVType ftype)


globalStructTypes :: [(T.Text, [(T.Text, AS.Type)])]
globalStructTypes =
  [ ("PSTATEType", bits ["N", "Z", "C", "V", "D", "A", "I", "F", "PAN", "UAO",
                         "SS", "IL", "nRW", "SP", "Q", "J", "T", "E"] ++
                   [("EL", bit 2), ("GE", bit 4), ("IT", bit 8), ("M", bit 5)])
  , ("SCRType", bits ["EL3_A", "NS", "EA", "IRQ", "FIQ", "RW"])
  , ("SCTLRType", bits ["SED","A", "TE", "EE", "SPAN", "V", "IESB"])
  , ("HSCTLRType", bits ["A", "TE", "EE"])
  , ("HCRType", bits ["TGA", "TEA", "TGE", "RW", "E2H"])
  , ("HDCRType", bits ["TDE"])
  , ("MDCRType", bits ["TDE"])
  , ("CPACRType", [("FPEN", bit 2)])
  , ("CNTKCTLType", [("EL0PTEN", bit 1)])
  , ("DBGDSCRextType", [("MOE", bit 4)])
  , ("TTBCRType", bits ["EAE"])
  ]
  where bit n = AS.TypeFun "bits" (AS.ExprLitInt n)
        bits nms = map (\nm -> (nm, bit 1)) nms

globalTypeSynonyms :: [(T.Text, AS.Type)]
globalTypeSynonyms =
  [ ("MAIRType", AS.TypeFun "bits" (AS.ExprLitInt 64))
  , ("ESRType", AS.TypeFun "bits" (AS.ExprLitInt 32))
  , ("VBARType", AS.TypeFun "bits" (AS.ExprLitInt 64))
  ]

toSimpleBVType :: AS.Type -> Some WT.BaseTypeRepr
toSimpleBVType t =
  if | AS.TypeFun "bits" e <- t
     , AS.ExprLitInt w <- e
     , Just (Some wRepr) <- NR.someNat w
     , Just NR.LeqProof <- NR.isPosNat wRepr
       -> Some (WT.BaseBVRepr wRepr)
     | otherwise -> error $ "Bad simple BV Type:" <> show t


builtinConsts :: [(T.Text, Some ConstVal)]
builtinConsts =
  [ ("TRUE", Some $ ConstVal WT.BaseBoolRepr True)
  , ("FALSE", Some $ ConstVal WT.BaseBoolRepr False)
  ]

-- | Make a function name given its ASL name and arity.
mkFunctionName :: AS.QualifiedIdentifier -> Int -> T.Text
mkFunctionName name numArgs = collapseQualID name <> T.pack "_" <> T.pack (show numArgs)


collapseQualID :: AS.QualifiedIdentifier -> T.Text
collapseQualID (AS.QualifiedIdentifier AS.ArchQualAArch64 name) = "AArch64_" <> name
collapseQualID (AS.QualifiedIdentifier AS.ArchQualAArch32 name) = "AArch32_" <> name
collapseQualID (AS.QualifiedIdentifier _ name) = name

mapInnerName :: (T.Text -> T.Text) -> AS.QualifiedIdentifier -> AS.QualifiedIdentifier
mapInnerName f (AS.QualifiedIdentifier q name) = AS.QualifiedIdentifier q (f name)

-- | Whenever we encounter a member variable of a struct, we treat it as an
-- independent global variable and use this function to construct its qualified name.
mkStructMemberName :: T.Text -> T.Text -> T.Text
mkStructMemberName s m = s <> "_" <> m


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
  let numArgs = length (callableArgs c)
  in mkFunctionName (callableName c) numArgs

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



-- | Transform a function that returns a struct type
-- into one that instead returns a specific member @mem@ of the resulting struct
collapseReturn :: T.Text -> [AS.Stmt] -> [AS.Stmt]
collapseReturn mem stmts =
  let assignLast finident stmts' =
        reverse $ case reverse stmts' of
          AS.StmtAssign (AS.LValVarRef lvident) eident
            : rest | lvident == finident ->
                AS.StmtReturn (Just $ AS.ExprMember eident mem) : rest
          end@(AS.StmtCall (AS.QualifiedIdentifier q "Unreachable") [] : rest) ->
            end
          _ -> error $ "Failed to collapse getter structure: " <> show stmts
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

callablesFromGetter :: (AS.Stmt -> AS.Stmt) -> AS.Definition -> [AS.Definition]
callablesFromGetter g
  (AS.DefGetter qName args [AS.TypeRef (AS.QualifiedIdentifier _ tname)] stmts) =
  case lookup tname globalStructTypes of
    Just fields -> (\(fnm, ftype) -> mkCallable (Just fnm) ftype) <$> fields
    _ -> case lookup tname globalTypeSynonyms of
      Just syn -> [mkCallable Nothing syn]
      _ -> error $
             "Missing struct definition for getter:" <> show qName <>
             " and type " <> T.unpack tname
  where
    getStatements (Just fnm) = collapseReturn fnm (g <$> stmts)
    getStatements Nothing = g <$> stmts
    mkCallable mfnm ftype =
      AS.DefCallable { callableName = mkGetterNameField qName mfnm
                     , callableArgs = args
                     , callableRets = [ftype]
                     , callableStmts = getStatements mfnm
                     }
callablesFromGetter g (AS.DefGetter qName args rets stmts) =
  [AS.DefCallable (mkGetterName qName) args rets (g <$> stmts)]
callablesFromGetter _ _ = error "Unexpected Definition."
    

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
                    , Just syn <- lookup tname globalTypeSynonyms
                      -> (name, syn)
                    | otherwise -> d
    
    mapDefs d = case d of
      AS.DefCallable qName args rets stmts ->
        [AS.DefCallable qName args rets (g <$> stmts)]
      c@AS.DefGetter {} -> callablesFromGetter g c
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
    
    mapInstr (AS.Instruction instName instEncodings instPostDecode instExecute) =
      AS.Instruction instName instEncodings (g <$> instPostDecode) (g <$> instExecute)

  in mapInstr <$> instrs


prepASL :: ([AS.Instruction], [AS.Definition]) -> ([AS.Instruction], [AS.Definition])
prepASL (instrs, defs') =
  let defs = defs' ++ extraDefs
      ovrs = mkSyntaxOverrides defs
  in (applySyntaxOverridesInstrs ovrs instrs, applySyntaxOverridesDefs ovrs defs)



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
  where initState = SigState Map.empty Map.empty Map.empty []


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
                         , callableSignatureMap :: Map.Map T.Text (SomeSimpleSignature, Callable)
                         , callableOpenSearches :: [T.Text]
                           -- ^ all callables encountered on the current search path
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
storeUserType tp = case tp of
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
computeType' tp = case tp of
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
  _ -> error "computeType"

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
    -- AS.ExprMember e var -> do
      -- eGlobals <- exprGlobalVars e
      -- varGlobals <- maybeToList <$> varGlobal var
      -- return $ eGlobals ++ varGlobals
    AS.ExprMember (AS.ExprVarRef (AS.QualifiedIdentifier _ structName)) memberName -> do
      mVarType <- computeGlobalStructMemberType structName memberName
      case mVarType of
        Nothing -> return []
        Just varType -> return [(mkStructMemberName structName memberName, varType)]
    AS.ExprMember _ _ -> return [] -- "Assuming no nested global structs"
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
  --seq (unsafePerformIO $ putStrLn $ show stmt) $
  case overrideStmt overrides stmt of
    Just _ -> return []
    Nothing -> case stmt of
      AS.StmtVarDeclInit _ e -> exprGlobalVars e
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
applyTypeEnvir env t = case t of
  AS.TypeFun "bits" e -> case exprToInt env e of
    Just i -> AS.TypeFun "bits" (AS.ExprLitInt i)
    Nothing -> error $ "Missing expected type variable: " <> show t <> " in environment: " <> show env
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

-- Extra definitions that give mock definitions to undefined functions
extraDefs :: [AS.Definition]
extraDefs = [
  AS.DefCallable { callableName = AS.QualifiedIdentifier AS.ArchQualAny "Zeros"
                 , callableArgs = []
                 , callableRets = [AS.TypeFun "bits" (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "N"))]
                 , callableStmts = [AS.StmtReturn (Just $
                                                   (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "Zeros")
                                                     [AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "N")]))]
                 },
  AS.DefCallable { callableName = AS.QualifiedIdentifier AS.ArchQualAny "ZeroExtend"
                 , callableArgs = [("val", AS.TypeFun "bits" (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "M")))]
                 , callableRets = [AS.TypeFun "bits" (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "N"))]
                 , callableStmts = [AS.StmtReturn (Just $
                                                   (AS.ExprCall (AS.QualifiedIdentifier AS.ArchQualAny "ZeroExtend")
                                                     [ AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "val")
                                                     , AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "N")]))]
                 },
  AS.DefCallable { callableName = AS.QualifiedIdentifier AS.ArchQualAny "ThisInstrLength"
                 , callableArgs = []
                 , callableRets = [AS.TypeRef (AS.QualifiedIdentifier AS.ArchQualAny "integer")]
                 , callableStmts = [AS.StmtReturn (Just $ AS.ExprVarRef
                                                   (AS.QualifiedIdentifier AS.ArchQualAny "ThisInstrLength"))]
                 }
  ]

-- Syntactic overrides for globals collection that should mirror the overrides
-- in SemMC.ASL.Translation
data Overrides arch =
  Overrides { overrideStmt :: AS.Stmt -> Maybe AS.Stmt
            , overrideExpr :: AS.Expr -> Maybe ()
            }
overrides :: forall arch . Overrides arch
overrides = Overrides {..}
  where overrideStmt :: AS.Stmt -> Maybe AS.Stmt
        overrideStmt s = case s of
          AS.StmtCall (AS.QualifiedIdentifier _ "ALUExceptionReturn") [_] -> Just $ AS.StmtUndefined
          AS.StmtCall (AS.QualifiedIdentifier _ "ALUWritePC") [result] -> Just $ AS.StmtUndefined
          AS.StmtVarDeclInit (nm,t) (AS.ExprCall (AS.QualifiedIdentifier _ "Zeros") []) -> Just $ AS.StmtUndefined
          _ -> Nothing
        overrideExpr :: AS.Expr -> Maybe ()
        overrideExpr e = case e of
          AS.ExprCall (AS.QualifiedIdentifier _ "UInt") [argExpr] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "SInt") [argExpr] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "IsZero") [argExpr] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "ZeroExtend") [val, _] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "Zeros") [_] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "Ones") [_] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "ASR_C") [x, shift] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "LSL_C") [x, shift] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "LSR_C") [x, shift] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "RRX_C") [x, shift] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "CurrentCond") [] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "Align") [x, y] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "IsExternalAbort") [x] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "IsExternalAbort") [] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "IsAsyncAbort") [x] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "IsExternalSyncAbort") [x] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "IsSErrorInterrupt") [x] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "Unreachable") [] -> defaultOverride
          AS.ExprCall (AS.QualifiedIdentifier _ "LSInstructionSyndrome") [] -> defaultOverride
          _ -> Nothing
        defaultOverride = Just $ ()
