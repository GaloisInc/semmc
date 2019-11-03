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
  , staticBinding
  , unstaticBinding
  , staticEnvironmentStmt
  )
where

import qualified Control.Monad.Fail as Fail
import           Control.Applicative
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
  deriving (Eq, Show)

data StaticEnvP = StaticEnvP { unStaticEnvP :: Map.Map T.Text EnvPEntry }
  deriving (Eq, Show)

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

liftStaticEnvP :: (Map.Map T.Text EnvPEntry -> Map.Map T.Text EnvPEntry) -> StaticEnvP -> StaticEnvP
liftStaticEnvP f (StaticEnvP env) = StaticEnvP (f env)

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
    AS.BinOpEQ ->
      case (msv, msv') of
        (Just sv, Just sv') -> Just $ StaticBool $ sv == sv'
        _ -> Nothing
    AS.BinOpNEQ ->
      case (msv, msv') of
        (Just sv, Just sv') -> Just $ StaticBool $ sv /= sv'
        _ -> Nothing
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
          AS.BinOpDivide -> divI
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

-- Monad for collecting possible variable assignments
newtype StaticEnvM a = StaticEnvM { getStaticPEnvs :: StaticEnvP -> [(StaticEnvP, a)] }

instance Functor StaticEnvM where
  fmap f (StaticEnvM spenvs) = StaticEnvM (map (\(env', ret) -> (env', f ret)) . spenvs)

instance Applicative StaticEnvM where
  pure x = StaticEnvM (\env -> [(env, x)])
  (<*>) x y = x >>= (\rv -> y >>= (\rv' -> return (rv rv')))

instance Monad StaticEnvM where
  (>>=) (StaticEnvM f) g =
    StaticEnvM (\env -> concat $ map (\(env', ret) -> getStaticPEnvs (g ret) env') (f env))
  fail = Fail.fail
  return x = pure x

instance Fail.MonadFail StaticEnvM where
  fail _ = StaticEnvM (\_ -> [])


runStaticEnvM ::  StaticEnvP -> StaticEnvM a -> [(StaticEnvP, a)]
runStaticEnvM env (StaticEnvM f) = f env

getEnv :: StaticEnvM StaticEnvP
getEnv = StaticEnvM (\env -> [(env, env)])

tryCatchEnv :: StaticEnvM a -> StaticEnvM a -> StaticEnvM a
tryCatchEnv (StaticEnvM f) (StaticEnvM g) = StaticEnvM (\env -> case f env of {[] -> g env; x -> x})

maybeEnv :: StaticEnvM () -> StaticEnvM ()
maybeEnv f = tryCatchEnv f (return ())

-- Nondeterministic selection
choice :: StaticEnvM a -> StaticEnvM a -> StaticEnvM a
choice (StaticEnvM f) (StaticEnvM g) = StaticEnvM (\env -> (f env) ++ (g env))

liftStaticMap :: (StaticEnvP -> StaticEnvP) -> StaticEnvM ()
liftStaticMap f = StaticEnvM (\env -> [(f env, ())])

setStaticValue :: T.Text -> StaticValue -> StaticEnvM ()
setStaticValue nm sv =
  liftStaticMap (liftStaticEnvP $ Map.insert nm (EnvPValue sv))

setInfeasableValue :: T.Text -> StaticValue -> StaticEnvM ()
setInfeasableValue nm sv =
  liftStaticMap (liftStaticEnvP $ Map.alter upd nm)
  where
    upd (Just (EnvPInfeasable ty svs)) = Just (EnvPInfeasable ty (Set.insert sv svs))
    upd Nothing = Just (EnvPInfeasable (typeOfStatic sv) (Set.singleton sv))
    upd (Just (EnvPValue sv')) = Just (EnvPValue sv')

liftList :: [a] -> StaticEnvM a
liftList xs = StaticEnvM (\env -> map (\x -> (env, x)) xs)

liftMaybe :: Maybe a -> StaticEnvM a
liftMaybe (Just a) = return a
liftMaybe _ = fail ""

possibleValuesFor :: T.Text -> StaticEnvM StaticValue
possibleValuesFor nm = do
  StaticEnvP env <- getEnv
  case Map.lookup nm env of
    Just (EnvPInfeasable (StaticBVType sz) inf) -> do
      bv' <- liftList $ allPossibleBVs sz
      let bv = StaticBV bv'
      if Set.member bv inf then fail ""
      else setStaticValue nm bv >> return bv
    Just (EnvPValue sv) -> return sv
    _ -> fail ""

staticValueOfVar :: T.Text -> StaticEnvM StaticValue
staticValueOfVar nm = do
  StaticEnvP env <- getEnv
  case Map.lookup nm env of
    Just (EnvPValue sv) -> return sv
    _ -> fail ""

assertEquality :: Bool -> AS.Expr -> AS.Expr -> StaticEnvM ()
assertEquality positive expr1 expr2 =
  case expr1 of
    AS.ExprVarRef (AS.VarName nm) -> do
      sv <- exprToStaticM expr2
      if positive then setStaticValue nm sv
      else setInfeasableValue nm sv
    -- AS.ExprBinOp AS.BinOpConcat e1 e2 -> do
    --   StaticBV bv <- exprToStaticM expr2
    --   env <- getEnv
    --   maybeEnv $ do
    --     split <- tryCatchEnv
    --       (do
    --           StaticBVType e1sz <- liftMaybe $ exprToStaticType env e1
    --           return $ fromIntegral e1sz)
    --       (do
    --           StaticBVType e2sz <- liftMaybe $ exprToStaticType env e2
    --           return (length bv - (fromIntegral e2sz)))
    --     let (bv1, bv2) = splitAt split bv
    --     maybeEnv $ assertEquality positive e1 (AS.ExprLitBin $ bv1)
    --     maybeEnv $ assertEquality positive e2 (AS.ExprLitBin $ bv2)
    _ -> fail ""

-- Where possible, produces a set of augmented environments
-- where the given expression is necessarily true
assertExpr :: Bool -> AS.Expr -> StaticEnvM ()
assertExpr positive expr = case expr of
  AS.ExprBinOp AS.BinOpEQ e e' ->
    maybeEnv $ tryCatchEnv
      (assertEquality positive e e')
      (assertEquality positive e' e)
  AS.ExprBinOp AS.BinOpNEQ e1 e2 -> assertExpr (not positive) (AS.ExprBinOp AS.BinOpEQ e1 e2)
  AS.ExprUnOp AS.UnOpNot e' -> assertExpr (not positive) e'
  AS.ExprBinOp AS.BinOpLogicalAnd e1 e2 ->
    if positive then do
      assertExpr True e1
      assertExpr True e2
    else
      choice (assertExpr False e1) (assertExpr False e2)
  AS.ExprBinOp AS.BinOpLogicalOr e1 e2 -> assertExpr positive $
    AS.ExprUnOp AS.UnOpNot (AS.ExprBinOp AS.BinOpLogicalAnd (AS.ExprUnOp AS.UnOpNot e1) (AS.ExprUnOp AS.UnOpNot e2))
  AS.ExprInSet e' setElts -> maybeEnv $ do
    elt <- liftList $ setElts
    case elt of
      AS.SetEltSingle e'' -> assertExpr positive (AS.ExprBinOp AS.BinOpEQ e' e'')
  _ -> return ()

testExpr :: AS.Expr -> StaticEnvM Bool
testExpr test = do
  StaticBool b <- tryCatchEnv (exprToStaticM test) $ choice
    (assertExpr True test >> return (StaticBool True))
    (assertExpr False test >> return (StaticBool False))
  return b

exprToStaticM :: AS.Expr -> StaticEnvM StaticValue
exprToStaticM expr = do
  env <- getEnv
  case exprToStatic env expr of
    Just sv -> return sv
    _ -> case expr of
      AS.ExprVarRef (AS.VarName nm) -> possibleValuesFor nm
      AS.ExprBinOp bop e1 e2 -> do
        sv1 <- exprToStaticM e1
        sv2 <- exprToStaticM e2
        liftMaybe $ staticBinOp bop (Just sv1) (Just sv2)
      AS.ExprIf tests@((test, body) : rest) fin -> do
        b <- testExpr test
        if b then exprToStaticM body
        else exprToStaticM (AS.ExprIf rest fin)
      AS.ExprIf [] fin -> exprToStaticM fin
      AS.ExprCall (AS.QualifiedIdentifier _ "UInt") [e] -> do
        StaticBV bv <- exprToStaticM e
        return $ StaticInt $ bitsToInteger bv
      _ -> fail ""

allPossibleBVs :: Integer -> [[Bool]]
allPossibleBVs 1 = [[True], [False]]
allPossibleBVs n = do
  bit <- [True, False]
  bits <- allPossibleBVs (n - 1)
  return $ bit : bits


-- | Get all possible variable assignments for the given variables after
-- the provided set of statements has been evaluated
getPossibleValuesFor :: StaticEnvP -- ^ The initial environment
                     -> [T.Text] -- ^ Mandatory variables
                     -> [T.Text] -- ^ Optional variables
                     -> [AS.Stmt] -- ^ Evaluated statements
                     -> [Map.Map T.Text StaticValue] -- ^ Possible binding environments
getPossibleValuesFor env vars optvars stmts =
  let results = map snd $ runStaticEnvM env $ do
        stmtsToStaticM stmts
        svs <- mapM staticValueOfVar vars
        optsvs <- mapM maybeVar optvars
        return $ (zip vars svs) ++ catMaybes optsvs
  in nub $ map Map.fromList results
  where
    maybeVar var =
      tryCatchEnv
        ((\sv -> Just (var, sv)) <$> (staticValueOfVar var))
        (return Nothing)

stmtsToStaticM :: [AS.Stmt] -> StaticEnvM ()
stmtsToStaticM stmts = traverse_ stmtToStaticM stmts

stmtToStaticM :: AS.Stmt -> StaticEnvM ()
stmtToStaticM s = case s of
  AS.StmtCall (AS.VarName "ASLSetUndefined") [] -> fail ""
  AS.StmtCall (AS.VarName "ASLSetUnpredictable") [] -> fail ""

  AS.StmtAssert e -> assertExpr True e

  AS.StmtIf tests@((test, body) : rest) fin -> applyTests tests fin

  AS.StmtAssign (AS.LValVarRef (AS.VarName nm)) e -> maybeEnv $ do
    sv <- exprToStaticM e
    setStaticValue nm sv

  AS.StmtVarDeclInit (nm, _)  e -> maybeEnv $ do
    sv <- exprToStaticM e
    setStaticValue nm sv

  AS.StmtCase test cases ->
    let (tests, fin) = casesToTests test cases
    in applyTests tests fin
  _ -> return ()
  where
    applyTests tests@((test, body) : rest) fin = do
        b <- testExpr test
        if b then stmtsToStaticM body
        else applyTests rest fin

    applyTests [] (Just fin) = stmtsToStaticM fin
    applyTests [] Nothing = return ()

    casesToTests e ((AS.CaseWhen (pat : pats) _ stmts) : rest) =
      let
        (tests, fin) = casesToTests e rest
        test = foldr (\pat' -> \e' -> AS.ExprBinOp AS.BinOpLogicalOr e' (patToExpr e pat')) (patToExpr e pat) pats
      in
        ((test, stmts) : tests, fin)
    casesToTests e [AS.CaseOtherwise stmts] = ([], Just stmts)
    casesToTests e [] = ([], Nothing)
    casesToTests e _ = error "Malformed case statement."

    patToExpr e pat = case pat of
      AS.CasePatternInt i -> AS.ExprBinOp AS.BinOpEQ e (AS.ExprLitInt i)
      AS.CasePatternBin bv -> AS.ExprBinOp AS.BinOpEQ e (AS.ExprLitBin bv)
      AS.CasePatternMask mask -> AS.ExprInMask e mask
      AS.CasePatternIdentifier ident -> AS.ExprBinOp AS.BinOpEQ (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny ident)) e
      _ -> AS.ExprUnknown (AS.TypeRef (AS.QualifiedIdentifier AS.ArchQualAny "boolean"))


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

staticEnvironmentStmt :: [([(T.Text, StaticValue)],[AS.Stmt])] -> AS.Stmt
staticEnvironmentStmt bodies =
  AS.StmtIf (map mkCase bodies) (Just $ [AS.StmtAssert falseExpr])
  where
    mkCase (varasns, stmts) =
      let
        test = foldr (\asn -> \e ->
          AS.ExprBinOp AS.BinOpLogicalAnd (staticToTest asn) e) trueExpr varasns
        bindings = map staticBinding varasns
      in
        (test, [letInStmt [] (bindings ++ stmts)])
    staticToTest (var, sv) = AS.ExprBinOp AS.BinOpEQ
      (AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny var))
      (staticToExpr sv)
