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

module SemMC.ASL.StaticExpr
  (
    StaticValue(..)
  , StaticType(..)
  , StaticValues
  , StaticEnvMap(..)
  , StaticEnvP(..)
  , EnvPEntry(..)
  , StaticEnv
  , typeOfStatic
  , staticEnvValue
  , staticEnvType
  , bitsToInteger
  , applyTypeSynonyms
  , simpleStaticEnvMap
  , applyStaticEnv'
  , emptyStaticEnvMap
  , insertStaticEnv
  , insertStaticEnvP
  , emptyStaticEnvP
  , exprToStatic
  , getPossibleValuesFor
  , getPossibleStaticsStmts
  , staticBinding
  , unstaticBinding
  , staticEnvironmentStmt
  )
where

import qualified Language.ASL.Syntax as AS
import qualified Data.Text as T
import           Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import           Data.Maybe (maybeToList, catMaybes, fromMaybe, listToMaybe, isJust, mapMaybe)
import           SemMC.ASL.Types
import qualified SemMC.ASL.SyntaxTraverse as AS ( pattern VarName )
import           Data.Foldable ( traverse_ )

bitsToInteger :: [Bool] -> Integer
bitsToInteger [x] = fromIntegral (fromEnum x)
bitsToInteger (x:xs) = fromIntegral (fromEnum x) * 2 + bitsToInteger xs
bitsToInteger _ = error $ "bitsToInteger empty list"


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


data StaticValue =
    StaticInt Integer
  | StaticBool Bool
  | StaticBV AS.BitVector
  deriving (Eq, Ord)

instance Show StaticValue where
  show t = case t of
    StaticInt i -> show i
    StaticBool b -> show b
    StaticBV bv -> show bv

data StaticType =
    StaticBVType Integer
  | StaticIntType
  | StaticBoolType
  deriving (Show, Eq)

typeOfStatic :: StaticValue -> StaticType
typeOfStatic sv = case sv of
  StaticInt _ -> StaticIntType
  StaticBool _ -> StaticBoolType
  StaticBV bv -> StaticBVType (fromIntegral $ length bv)

class StaticEnv a where
  staticEnvValue :: a -> T.Text -> Maybe StaticValue
  staticEnvType :: a -> T.Text -> Maybe StaticType

type StaticValues = Map.Map T.Text StaticValue

data StaticEnvMap = StaticEnvMap
  { staticEnvMapVals :: StaticValues
  , staticEnvMapTypes :: T.Text -> Maybe StaticType
  }

instance StaticEnv StaticEnvMap where
  staticEnvValue (StaticEnvMap vars _) nm = Map.lookup nm vars
  staticEnvType (StaticEnvMap vars types) nm = case Map.lookup nm vars of
    Just sv -> Just $ typeOfStatic sv
    _ -> types nm

emptyStaticEnvMap :: StaticEnvMap
emptyStaticEnvMap = simpleStaticEnvMap Map.empty

simpleStaticEnvMap :: StaticValues -> StaticEnvMap
simpleStaticEnvMap vals = StaticEnvMap vals (\_ -> Nothing)

data EnvPEntry =
    EnvPValue StaticValue
  | EnvPInfeasable StaticType (Set.Set StaticValue)

data StaticEnvP = StaticEnvP { unStaticEnvP :: Map.Map T.Text EnvPEntry }

emptyStaticEnvP :: StaticEnvP
emptyStaticEnvP = StaticEnvP Map.empty

instance StaticEnv StaticEnvP where
  staticEnvValue (StaticEnvP entries) nm = case Map.lookup nm entries of
    Just (EnvPValue sv) -> Just sv
    _ -> Nothing
  staticEnvType (StaticEnvP entries) nm = case Map.lookup nm entries of
    Just (EnvPInfeasable st _) -> Just st
    Just (EnvPValue sv) -> Just $ typeOfStatic sv
    _ -> Nothing

insertStaticEnv :: T.Text -> StaticValue -> StaticEnvMap -> StaticEnvMap
insertStaticEnv nm v (StaticEnvMap vals types) =
  StaticEnvMap (Map.insert nm v vals) types

insertStaticEnvP :: T.Text -> EnvPEntry -> StaticEnvP -> StaticEnvP
insertStaticEnvP nm v (StaticEnvP entries) =
  StaticEnvP (Map.insert nm v entries)

exprToStaticType :: StaticEnv env
                 => env
                 -> AS.Expr
                 -> Maybe StaticType
exprToStaticType env e = case e of
  AS.ExprVarRef (AS.QualifiedIdentifier _ ident) -> staticEnvType env ident
  AS.ExprIf ((_, body) : rest) fin ->
    case exprToStaticType env body of
      Just t -> Just t
      Nothing -> exprToStaticType env (AS.ExprIf rest fin)
  AS.ExprIf [] fin -> exprToStaticType env fin
  AS.ExprLitBin bv -> Just $ StaticBVType (fromIntegral $ length bv)
  AS.ExprBinOp AS.BinOpConcat e' e''
   | Just (StaticBVType t') <- exprToStaticType env e'
   , Just (StaticBVType t'') <- exprToStaticType env e'' ->
     Just $ StaticBVType $ t' + t''
  AS.ExprSlice _ [AS.SliceRange hi lo]
   | Just (StaticInt hi') <- exprToStatic env hi
   , Just (StaticInt lo') <- exprToStatic env lo ->
     Just $ StaticBVType ((hi' - lo') + 1)
  AS.ExprSlice _ [AS.SliceSingle _] -> Just $ StaticBVType 1
  AS.ExprSlice _ [AS.SliceOffset _ offset]
   | Just (StaticInt offset') <- exprToStatic env offset ->
     Just $ StaticBVType offset'
  AS.ExprSlice e (slice : rest)
   | Just (StaticBVType rest') <- exprToStaticType env $ AS.ExprSlice e rest
   , Just (StaticBVType slice') <- exprToStaticType env $ AS.ExprSlice e [slice] ->
     Just $ StaticBVType (rest' + slice')
  _ -> Nothing

staticBinOp :: AS.BinOp
            -> Maybe StaticValue
            -> Maybe StaticValue
            -> Maybe StaticValue
staticBinOp bop msv msv' =
  case bop of
    AS.BinOpEQ
      | Just sv <- msv
      , Just sv' <- msv' ->
        Just $ StaticBool $ sv == sv'
    AS.BinOpNEQ
      | Just sv <- msv
      , Just sv' <- msv' ->
        Just $ StaticBool $ sv /= sv'

    --integer binary operation
    _ | Just (StaticInt i) <- msv
      , Just (StaticInt i') <- msv' ->
        let
          resultI primop = Just $ StaticInt $ primop i i'
          resultB primop = Just $ StaticBool $ primop i i'
          divI =
            let
              (quot, rem) = (i `divMod` i')
            in if rem == 0 then Just $ StaticInt quot
            else Nothing
        in case bop of
          AS.BinOpAdd -> resultI (+)
          AS.BinOpSub -> resultI (-)
          AS.BinOpMul -> resultI (*)
          AS.BinOpPow -> resultI (^)
          AS.BinOpDiv -> divI
          AS.BinOpShiftLeft -> resultI (\base -> \shift -> base * (2 ^ shift))
          AS.BinOpGT -> resultB (>)
          AS.BinOpLT -> resultB (<)
          AS.BinOpGTEQ -> resultB (>=)
          AS.BinOpLTEQ -> resultB (<=)
          _ -> Nothing
    -- short-circuiting for boolean logic
    _ | Just (StaticBool False) <- msv ->
        case bop of
          AS.BinOpLogicalAnd -> Just $ StaticBool False
          AS.BinOpLogicalOr -> msv'
    _ | Just (StaticBool False) <- msv' ->
        case bop of
          AS.BinOpLogicalAnd -> Just $ StaticBool False
          AS.BinOpLogicalOr -> msv
    _ | Just (StaticBool True) <- msv ->
        case bop of
          AS.BinOpLogicalAnd -> msv'
          AS.BinOpLogicalOr -> Just $ StaticBool True
    _ | Just (StaticBool True) <- msv' ->
        case bop of
          AS.BinOpLogicalAnd -> msv
          AS.BinOpLogicalOr -> Just $ StaticBool True
    _ -> Nothing



exprToStatic :: StaticEnv env
             => env
             -> AS.Expr
             -> Maybe StaticValue
exprToStatic env e = case e of
  AS.ExprIf ((test, body) : rest) fin
    | Just (StaticBool b) <- exprToStatic env test -> do
      if b then exprToStatic env body
      else exprToStatic env (AS.ExprIf rest fin)
  AS.ExprIf [] fin -> exprToStatic env fin
  AS.ExprLitInt i -> Just $ StaticInt i
  AS.ExprLitBin bv -> Just $ StaticBV bv
  AS.ExprInMask bv mask
    | Just (StaticBV bv') <- exprToStatic env bv ->
      Just $ StaticBool $ matchMask bv' mask
  AS.ExprVarRef (AS.QualifiedIdentifier _ "TRUE") -> Just $ StaticBool True
  AS.ExprVarRef (AS.QualifiedIdentifier _ "FALSE") -> Just $ StaticBool False
  AS.ExprVarRef (AS.QualifiedIdentifier q id) -> staticEnvValue env id
  AS.ExprBinOp bop e' e'' ->
    staticBinOp bop (exprToStatic env e') (exprToStatic env e'')

  AS.ExprUnOp AS.UnOpNot e'
    | Just (StaticBool b) <- exprToStatic env e' ->
      Just $ StaticBool (not b)

  AS.ExprUnOp AS.UnOpNeg e'
    | Just (StaticInt i) <- exprToStatic env e' ->
      Just $ StaticInt (-i)

  AS.ExprCall (AS.QualifiedIdentifier _ "sizeOf") [e]
    | Just (StaticBVType i) <- exprToStaticType env e ->
      Just $ StaticInt i
  _ -> Nothing

matchMask :: AS.BitVector -> AS.Mask -> Bool
matchMask bv mask =
  if | length bv == length mask ->
       List.all matchBit (zip bv mask)
     | otherwise -> error $ "Mismatched bitvector sizes."
  where
    matchBit (b, m) = case (b, m) of
      (True, AS.MaskBitSet) -> True
      (False, AS.MaskBitUnset) -> True
      (_, AS.MaskBitEither) -> True
      _ -> False


data VarRegisterStatus =
  VarMaybeRegisterIndex | VarIsRegisterIndex | VarNotRegisterIndex

instance Semigroup VarRegisterStatus where
  (<>) VarNotRegisterIndex _ = VarNotRegisterIndex
  (<>) _ VarNotRegisterIndex = VarNotRegisterIndex
  (<>) _ VarIsRegisterIndex = VarIsRegisterIndex
  (<>) VarIsRegisterIndex _ = VarIsRegisterIndex
  (<>) _ _ = VarMaybeRegisterIndex

instance Monoid VarRegisterStatus where
  mempty = VarMaybeRegisterIndex


-- Determines if the given variable is used as a register index anywhere
-- in the statement body
varRegisterStatusStmts :: T.Text -> [AS.Stmt] -> Bool
varRegisterStatusStmts var stmts = True


-- Returns either an upper bound on all possible values for the expression
-- or @Nothing@ if such a bound can't be determined.
getPossibleStaticsExpr :: StaticEnvP -> AS.Expr -> Maybe [StaticValue]
getPossibleStaticsExpr env@(StaticEnvP penv) e = case exprToStatic env e of
  Just sv -> Just [sv]
  _ -> case e of
    AS.ExprVarRef (AS.QualifiedIdentifier _ nm) ->
      case Map.lookup nm penv of
        Just (EnvPInfeasable (StaticBVType sz) inf) -> Just $ do
          bv' <- allPossibleBVs sz
          let bv = StaticBV bv'
          if Set.member bv inf then fail ""
          else return $ bv
        Just (EnvPValue sv) -> Just [sv]
        Nothing -> Nothing
    AS.ExprBinOp bop e e' ->
      case (getPossibleStaticsExpr env e, getPossibleStaticsExpr env e') of
        (Just es, Just es') -> liftMaybe $ do
          se <- es
          se' <- es'
          return $ (:[]) <$> staticBinOp bop (Just se) (Just se')
        _ -> Nothing
    AS.ExprIf tests@((test, body) : rest) fin ->
      case getPossibleStaticsExpr env test of
        Just svs -> liftMaybe $ do
            sv <- svs
            case sv of
              StaticBool True -> return $ getPossibleStaticsExpr env body
              StaticBool False -> return $ getPossibleStaticsExpr env (AS.ExprIf rest fin)
              _ -> return $ Nothing
        _ -> do
          bodies <- concat <$> mapM applyTest tests
          fin' <- getPossibleStaticsExpr env fin
          return $ bodies ++ fin'
    AS.ExprCall (AS.QualifiedIdentifier _ "UInt") [e] ->
      case getPossibleStaticsExpr env e of
        Just svs -> collapseMaybes $ do
          sv <- svs
          case sv of
            StaticBV bv -> return $ Just $ StaticInt $ bitsToInteger bv
            _ -> return $ Nothing
        _ -> Nothing
    _ -> Nothing
  where
    applyTest (test, body) = liftMaybe $ do
      env' <- assertInEnv test env
      return $ getPossibleStaticsExpr env' body

liftMaybe :: [Maybe [a]] -> Maybe [a]
liftMaybe ms = concat <$> (collapseMaybes ms)

collapseMaybes :: [Maybe a] -> Maybe [a]
collapseMaybes (Just a : rest) =
  case collapseMaybes rest of
    Just rest' -> Just $ (a : rest')
    Nothing -> Nothing
collapseMaybes (Nothing : _) = Nothing
collapseMaybes [] = Just []

matchMask :: AS.BitVector -> AS.Mask -> Bool
matchMask bv mask =
  if | length bv == length mask ->
       List.all matchBit (zip bv mask)
     | otherwise -> error $ "Mismatched bitvector sizes."
  where
    matchBit (b, m) = case (b, m) of
      (True, AS.MaskBitSet) -> True
      (False, AS.MaskBitUnset) -> True
      (_, AS.MaskBitEither) -> True
      _ -> False

allPossibleBVs :: Integer -> [[Bool]]
allPossibleBVs 0 = []
allPossibleBVs n = do
  bit <- [True, False]
  bits <- allPossibleBVs (n - 1)
  return $ bit : bits

getPossibleValuesFor :: [T.Text] -> [StaticEnvP] -> [Map.Map T.Text StaticValue]
getPossibleValuesFor vars envs =
  let
    pairs = do
      env <- envs
      pvars <- maybeToList $ collapseMaybes $ do
        var <- vars
        return $ (\val -> (var, val)) <$>
          getPossibleStaticsExpr env (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny var))
      mapM (\(var, vals) -> do
               val <- vals
               return $ (var, val)) pvars

  in nub $ map Map.fromList pairs


getPossibleStaticsStmts :: StaticEnvP -> [AS.Stmt] -> [StaticEnvP]
getPossibleStaticsStmts env stmts = case stmts of
  (stmt : rest) -> do
    let envs = getPossibleStaticsStmt env stmt
    concat $ map (\env' -> getPossibleStaticsStmts env' rest) envs
  [] -> [env]

envInfeasable :: StaticEnvP -> StaticEnvP
envInfeasable (StaticEnvP env) = StaticEnvP (Map.map makeInfeasable env)
  where
    makeInfeasable (EnvPValue sv) = EnvPInfeasable (typeOfStatic sv) (Set.singleton sv)
    makeInfeasable x = x

addStaticEnvP :: T.Text -> StaticValue -> StaticEnvP -> StaticEnvP
addStaticEnvP nm v (StaticEnvP env) = StaticEnvP (Map.insert nm (EnvPValue v) env)

getPossibleStaticsStmt :: StaticEnvP -> AS.Stmt -> [StaticEnvP]
getPossibleStaticsStmt env s = case s of
  AS.StmtSeeExpr _ -> [envInfeasable env]
  AS.StmtSeeString _ -> [envInfeasable env]

  AS.StmtAssert e | Just (StaticBool False) <- exprToStatic env e -> [envInfeasable env]
  AS.StmtAssert e -> assertInEnv e env
  AS.StmtAssign (AS.LValVarRef (AS.QualifiedIdentifier _ nm)) e ->
    case getPossibleStaticsExpr env e of
      Just svs -> map (\v -> addStaticEnvP nm v env) svs
      Nothing -> [env]
  AS.StmtIf tests@((test, body) : rest) fin -> applyTests env tests fin

  AS.StmtCase test cases ->
    let (tests, fin) = casesToTests test cases
    in applyTests env tests fin
  _ -> [env]
  where
    applyTests env tests@((test, body) : rest) fin =
      case getPossibleStaticsExpr env test of
        Just svs -> do
          sv <- svs
          case sv of
            StaticBool True -> getPossibleStaticsStmts env body
            StaticBool False -> applyTests env rest fin
            _ -> error $ "Malformed test expression:" <> show test
        _ -> do
          let testEnvs = concat $ map (applyTest env) tests
          let finEnv = getPossibleStaticsStmts env (concat $ maybeToList fin)
          testEnvs ++ finEnv

    applyTests env [] fin = getPossibleStaticsStmts env (concat $ maybeToList fin)

    applyTest env (test, body) = do
      env' <- assertInEnv test env
      getPossibleStaticsStmts env' body

    casesToTests e ((AS.CaseWhen (pat : pats) _ stmts) : rest) =
      let
        (tests, fin) = casesToTests e rest
        test = foldr (\pat' -> \e' -> AS.ExprBinOp AS.BinOpLogicalAnd e' (patToExpr e pat')) (patToExpr e pat) pats
      in
        ((test, stmts) : tests, fin)
    casesToTests e [AS.CaseOtherwise stmts] = ([], Just stmts)
    casesToTests e [] = ([], Nothing)
    casesToTests e _ = error "Malformed case statement."

    patToExpr e pat = case pat of
      AS.CasePatternInt i -> AS.ExprBinOp AS.BinOpEQ e (AS.ExprLitInt i)
      AS.CasePatternBin bv -> AS.ExprBinOp AS.BinOpEQ e (AS.ExprLitBin bv)
      AS.CasePatternMask mask -> AS.ExprInSet e [AS.SetEltSingle (AS.ExprLitMask mask)]
      AS.CasePatternIdentifier ident -> AS.ExprBinOp AS.BinOpEQ (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny ident)) e
      _ -> AS.ExprUnknown (AS.TypeRef (AS.QualifiedIdentifier AS.ArchQualAny "boolean"))




-- Where possible, produces a set of augmented environments
-- where the given expression is necessarily true
assertInEnv :: AS.Expr -> StaticEnvP -> [StaticEnvP]
assertInEnv e env@(StaticEnvP penv) = case e of
  AS.ExprBinOp AS.BinOpEQ (AS.ExprVarRef (AS.QualifiedIdentifier _ nm)) e'
    | Just svs <- getPossibleStaticsExpr env e' ->
      map (\v -> StaticEnvP (Map.insert nm (EnvPValue v) penv)) svs
  AS.ExprBinOp AS.BinOpNEQ (AS.ExprVarRef (AS.QualifiedIdentifier _ nm)) e'
    | Just svs <- getPossibleStaticsExpr env e' ->
      case Map.lookup nm penv of
        Nothing ->
          map (\v -> StaticEnvP $
                Map.insert nm (EnvPInfeasable (typeOfStatic v) (Set.singleton v)) penv) svs
        Just (EnvPInfeasable sty inf) ->
          map (\v ->
                 if typeOfStatic v /= sty
                 then error $ "Mismatch in infeasable types:" ++ show sty ++ " " ++ show (typeOfStatic v)
                 else StaticEnvP $ Map.insert nm (EnvPInfeasable sty (Set.insert v inf)) penv) svs
        Just (EnvPValue _) -> [env]
  AS.ExprBinOp AS.BinOpLogicalAnd e e' -> do
    env' <- assertInEnv e env
    assertInEnv e env'
  AS.ExprInSet e setElts -> do
    elt <- setElts
    case elt of
      AS.SetEltSingle e' -> assertInEnv (AS.ExprBinOp AS.BinOpEQ e e') env
  _ -> [env]
  where
   matchBit (b, m) = case (b, m) of
      (True, AS.MaskBitSet) -> True
      (False, AS.MaskBitUnset) -> True
      (_, AS.MaskBitEither) -> True
      _ -> False


applyTypeSynonyms :: AS.Type -> AS.Type
applyTypeSynonyms t = case t of
  AS.TypeRef (AS.QualifiedIdentifier _ tpName) ->
    case lookup tpName globalTypeSynonyms of
      Just syn -> syn
      Nothing -> t
  _ -> t

applyStaticEnv' :: StaticEnv env
                => env
                -> AS.Type
                -> Maybe AS.Type
applyStaticEnv' env t = case applyTypeSynonyms t of
  AS.TypeFun "bits" e -> case exprToStatic env e of
    Just (StaticInt i) -> Just $ AS.TypeFun "bits" (AS.ExprLitInt i)
    _ -> Nothing
  _ -> Just $ t

trueExpr :: AS.Expr
trueExpr = AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "TRUE")

falseExpr :: AS.Expr
falseExpr = AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "FALSE")

staticToExpr :: StaticValue -> AS.Expr
staticToExpr sv = case sv of
  StaticInt i -> AS.ExprLitInt i
  StaticBool True -> trueExpr
  StaticBool False -> falseExpr
  StaticBV bv -> AS.ExprLitBin bv

staticBinding :: (T.Text, StaticValue) -> AS.Stmt
staticBinding (nm, sv) =
  AS.StmtCall (AS.QualifiedIdentifier AS.ArchQualAny "StaticBind")
    [AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny nm), staticToExpr sv]

unstaticBinding :: StaticEnv env => AS.Stmt -> Maybe (env -> (T.Text, StaticValue))
unstaticBinding (AS.StmtCall (AS.QualifiedIdentifier _ "StaticBind") [AS.ExprVarRef (AS.QualifiedIdentifier _ nm), e]) = Just $ \env ->
  case exprToStatic env e of
    Just sv -> (nm, sv)
    _ -> error "Invalid StaticBind"
unstaticBinding _ = Nothing

staticEnvironmentStmt :: [T.Text] -> [([StaticValue],[AS.Stmt])] -> AS.Stmt
staticEnvironmentStmt vars bodies =
  AS.StmtIf (map mkCase bodies) (Just $ [AS.StmtAssert falseExpr])
  where
    mkCase (asns, stmts) =
      if length vars /= length asns
      then error $ "Invalid static environment:" ++ show vars ++ show asns
      else
        let
          varasns = zip vars asns
          test = foldr (\asn -> \e ->
            AS.ExprBinOp AS.BinOpLogicalAnd (staticToTest asn) e) trueExpr varasns
          bindings = map staticBinding varasns
        in
          (test, [letInStmt [] (bindings ++ stmts)])
    staticToTest (var, sv) = AS.ExprBinOp AS.BinOpEQ
      (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny var))
      (staticToExpr sv)
