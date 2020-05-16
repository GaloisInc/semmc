{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{--

N.B., this Parser is intended to parse the SemMC specific
content surrounding serialized Formula and
ParameterizedFormula and then parameterized and call a
generic What4 s-expression parser (from the
`what4-serialize` package) to parse the definition bodies.

This parser must be compatible with the output from both
SemMC.Formula.Printer AND SemMC.DSL.

For more details on the serialization, see the comments at
the top of SemMC.Formula.Printer.

--}



-- | A parser for an s-expression representation of formulas
module SemMC.Formula.Parser
  ( operandVarPrefix
  , literalVarPrefix
  , argumentVarPrefix
  , readFormula
  , readFormulaFromFile
  , readDefinedFunction
  , readDefinedFunctionFromFile
  ) where

import qualified Control.Monad.Except as E
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad ( when )
import           Data.Foldable ( foldrM )
import           Data.Kind
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Printf ( printf )
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )
import           Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Classes
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..), mapSome, viewSome )
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.TraversableFC ( traverseFC, allFC )
import qualified Data.Parameterized.Map as MapF
import           What4.BaseTypes
import qualified Lang.Crucible.Backend as S
import qualified What4.Interface as S
import           What4.Symbol ( userSymbol )

import           What4.Serialize.Parser ( deserializeExprWithConfig
                                        , deserializeBaseType
                                        , type SExpr
                                        , Atom(..)
                                        , WellFormedSExpr(..)
                                        , SomeSymFn(..)
                                        , Config(..)
                                        , printSExpr
                                        )
import           What4.Serialize.FastSExpr ( parseSExpr )

import qualified Data.Type.List as TL -- in this package
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Env ( FormulaEnv(..), SomeSome(..) )
import           SemMC.Formula.Formula
import qualified SemMC.Util as U

import           Prelude


data OperandTypeWrapper (arch :: Type) :: TL.TyFun Symbol BaseType -> Type
type instance TL.Apply (OperandTypeWrapper arch) s = A.OperandType arch s
type OperandTypes arch sh = TL.Map (OperandTypeWrapper arch) sh

-- | A counterpart to 'SemMC.Formula.Parameter' for use in
-- the parser, where we might know only a parameter's base
-- type (such as when parsing a defined function).
data ParsedParameter arch (tps :: [BaseType]) (tp :: BaseType) where
  ParsedOperandParameter ::
    BaseTypeRepr tp
    -> SL.Index tps tp
    -> ParsedParameter arch tps tp
  ParsedLiteralParameter ::
    L.Location arch tp
    -> ParsedParameter arch tps tp

-- Translating from 'SL.Index' on 'BaseType' to 'SL.Index' on 'Symbol' is
-- tricky.  Need this view to show that when we translate some @SL.Index tps tp@
-- to an @SL.Index sh s@, the symbol @s@ maps to the base type @tp@ (assuming
-- that @tps ~ OperandTypes arch sh@).
data IndexByArchType arch sh tp where
  IndexByArchType :: A.OperandType arch s ~ tp => SL.Index sh s -> IndexByArchType arch sh tp

-- TODO This is all very silly. It's an expensive identity function.
indexByArchType :: proxy arch
                -> A.ShapeRepr arch sh
                -> SL.Index (OperandTypes arch sh) tp
                -> IndexByArchType arch sh tp
indexByArchType _ SL.Nil _ = error "impossible"
indexByArchType _ (_ SL.:< _) SL.IndexHere = IndexByArchType SL.IndexHere
indexByArchType p (_ SL.:< shapeRepr) (SL.IndexThere ix) =
  case indexByArchType p shapeRepr ix of
    IndexByArchType ix' -> IndexByArchType (SL.IndexThere ix')

toParameter :: forall arch sh tp
             . A.ShapeRepr arch sh
            -> ParsedParameter arch (OperandTypes arch sh) tp
            -> Parameter arch sh tp
toParameter shapeRepr (ParsedOperandParameter tpRepr ix) =
  case indexByArchType (Proxy @arch) shapeRepr ix of
    IndexByArchType ix' -> OperandParameter tpRepr ix'
toParameter _ (ParsedLiteralParameter loc) =
  LiteralParameter loc

-- * First pass of parsing turns the raw text into s-expressions.
--   This pass is handled by the code in SemMC.Formula.SELang

-- * Second pass of parsing: turning the s-expressions into symbolic expressions
-- and the overall templated formula

-- ** Utility functions

-- | Utility function for contextualizing errors. Prepends the given prefix
-- whenever an error is thrown.
prefixError :: (Monoid e, E.MonadError e m) => e -> m a -> m a
prefixError prefix act = E.catchError act (E.throwError . mappend prefix)

-- | Utility function for lifting a 'Maybe' into a 'MonadError'
fromMaybeError :: (E.MonadError e m) => e -> Maybe a -> m a
fromMaybeError err = maybe (E.throwError err) return

-- ** Parsing operands

-- | Data about the operands pertinent after parsing: their name and their type.
data OpData (tp :: BaseType) where
  OpData :: Text -> BaseTypeRepr tp -> OpData tp

buildOperandList' :: forall m proxy arch tps
                   . (E.MonadError String m, A.Architecture arch)
                  => proxy arch
                  -> A.ShapeRepr arch tps
                  -> [SExpr]
                  -> m (SL.List OpData (OperandTypes arch tps))
buildOperandList' proxy rep ss = 
  case (rep, ss) of
    (SL.Nil, []) -> return SL.Nil
    (SL.Nil, _) -> E.throwError $ "Expected [] but got " ++ show ss
    (_ SL.:< _, []) -> E.throwError $ "Expected an entry but got []"
    (r SL.:< rep', (s:rest)) ->
      case s of
        WFSList [ WFSAtom (AId operand)
                , WFSAtom (AId tySymbol)] -> do
          when (A.operandTypeReprSymbol proxy r /= (T.unpack tySymbol)) $
            E.throwError $ "unknown reference: " ++ show tySymbol
          rest' <- buildOperandList' proxy rep' rest
          let tyRepr = A.shapeReprToTypeRepr proxy r
          return $ (OpData operand tyRepr) SL.:< rest'
        _ -> E.throwError $ "Expected an operand entry of the form `(op type)`"

-- ** Parsing parameters
--
-- By which I mean, handling occurrences in expressions of either operands or
-- literals.



operandVarPrefix :: String
operandVarPrefix = "op_"

literalVarPrefix :: String
literalVarPrefix = "lit_"

argumentVarPrefix :: String
argumentVarPrefix = "arg_"


-- | Short-lived type that just stores an index with its
-- corresponding type representation, with the type
-- parameter ensuring they correspond to one another.
data IndexWithType (sh :: [BaseType]) (tp :: BaseType) where
  IndexWithType :: BaseTypeRepr tp -> SL.Index sh tp -> IndexWithType sh tp

-- | Look up a name in the given operand list, returning its
-- index and type if found.
findOpListIndex :: Text -> SL.List OpData sh -> Maybe (Some (IndexWithType sh))
findOpListIndex _ SL.Nil = Nothing
findOpListIndex x ((OpData name tpRepr) SL.:< rest)
  | x == name = Just $ Some (IndexWithType tpRepr SL.IndexHere)
  | otherwise = mapSome incrIndex <$> findOpListIndex x rest
      where incrIndex (IndexWithType tpRepr' idx) = IndexWithType tpRepr' (SL.IndexThere idx)

-- | Parse a single parameter, given the list of operands to
-- use as a lookup.
readParameter :: forall tps m arch proxy .
  (E.MonadError String m, A.Architecture arch)
  => proxy arch
  -> SL.List OpData tps
  -> SExpr
  -> m (Some (ParsedParameter arch tps))
readParameter _ oplist (WFSAtom (AId fullName)) =
  case splitId fullName of
    ("op", op) -> 
      maybe
      (E.throwError $ printf "couldn't find operand %s" op)
      (viewSome (\(IndexWithType tpRepr idx) -> return $ Some (ParsedOperandParameter tpRepr idx)))
      (findOpListIndex op oplist)
    ("loc", lit) ->
      maybe
      (E.throwError $ printf "%s is an invalid literal for this arch" lit)
      (return . viewSome (Some . ParsedLiteralParameter))
      (A.readLocation $ T.unpack lit)
    _ -> E.throwError $ printf "Unrecognized parameter: %s" fullName
readParameter _ _ sexpr = (E.throwError $ printf "Unrecognized parameter: %s" (printSExpr mempty sexpr))

-- | Parses the input list, e.g., @(ra rb 'ca)@
readInputs :: forall m (arch :: Type) (tps :: [BaseType])
            . (E.MonadError String m,
               A.Architecture arch)
           => SL.List OpData tps
           -> [SExpr]
           -> m [Some (ParsedParameter arch tps)]
readInputs _ [] = return []
readInputs oplist (p:rest) = do
  p' <- readParameter Proxy oplist p
  rest' <- readInputs oplist rest
  return $ p' : rest'

-- ** Parsing definitions

-- | "Global" data for parsing the definitions.
data DefsInfo sym arch tps =
  DefsInfo
  { getSym :: sym
  -- ^ SymInterface/ExprBuilder used to build up symbolic
  -- expressions while parsing the definitions.
  , getEnv :: FormulaEnv sym arch
  -- ^ Global formula environment
  , getLitVars :: MapF.MapF (A.Location arch) (S.BoundVar sym)
  -- ^ Function used to retrieve the expression
  -- corresponding to a given literal.
  , getOpVarList :: SL.List (S.BoundVar sym) tps
  -- ^ ShapedList used to retrieve the variable
  -- corresponding to a given literal.
  , getOpNameList :: SL.List OpData tps 
  -- ^ ShapedList used to look up the index given an
  -- operand's name.
  }



-- | Parse the whole definitions expression, e.g.:
--
-- > ((rt . (bvadd ra rb))
-- >  ('ca . #b1))
--
readDefs :: forall sym m arch sh
          . (S.IsSymExprBuilder sym,
             Monad m,
             E.MonadError String m,
             A.Architecture arch,
             MonadIO m,
             ShowF (S.SymExpr sym))
         => DefsInfo sym arch (OperandTypes arch sh)
          -> A.ShapeRepr arch sh
         -> [SExpr]
         -> m (MapF.MapF (Parameter arch sh) (S.SymExpr sym))
readDefs info shapeRepr rawDefs = do
  defs <- mapM (readDef info shapeRepr) rawDefs
  return $ MapF.fromList defs

readDef :: forall sym m arch sh
          . (S.IsSymExprBuilder sym,
             Monad m,
             E.MonadError String m,
             A.Architecture arch,
             MonadIO m,
             ShowF (S.SymExpr sym))
         => DefsInfo sym arch (OperandTypes arch sh)
        -> A.ShapeRepr arch sh
        -> SExpr
        -> m (MapF.Pair (Parameter arch sh) (S.SymExpr sym))
readDef info shapeRepr (WFSList [lhs, rhs]) = do
  let oplist = getOpNameList info
      sym = getSym info
  -- parse the rhs definition
  (Some def :: (Some (S.SymExpr sym))) <-
    case rhs of
      WFSList [WFSAtom (AId "with"), WFSList rawWBindings, bodyRaw] -> do
        freshNameEnv <- readWithBindings rawWBindings
        let cfg = Config { cSymFnLookup = fnLookup sym info freshNameEnv
                         , cExprLookup = exprLookup sym info freshNameEnv
                         }
        res <- liftIO $ deserializeExprWithConfig sym cfg bodyRaw
        case res of
          Left msg -> E.throwError msg
          Right val -> return val
      _ -> E.throwError $ "Invalid def body; expected a `with` bindings form, but got: " ++ (show rhs)
  -- now examine the lhs to decide what to do with the parsed expression
  case lhs of
    -- Cases where the lhs is a function application on a parameter---e.g.,
    -- `(... [(call f p) rhs] ...)`---are treated specially and stored
    -- as `FunctionParameter`s.
    (WFSList [WFSAtom (AId "call"), WFSAtom (AId fnName) , p]) -> do
      Some param <- mapSome (toParameter shapeRepr) <$> readParameter (Proxy @arch) oplist p
      case Map.lookup (T.unpack fnName) (envFunctions $ getEnv info) of
        Just (_, Some rep) -> do
          Refl <- fromMaybeError ("mismatching types of parameter and expression for " ++ showF param) $
                    testEquality rep (S.exprType def)
          case param of
            LiteralParameter {} -> E.throwError "Literals are not allowed as arguments to parameter functions"
            FunctionParameter {} -> E.throwError "Nested parameter functions are not allowed"
            OperandParameter orep oix ->
              return $ MapF.Pair (FunctionParameter (T.unpack fnName) (WrappedOperand orep oix) rep) def
        _ -> E.throwError ("Missing type repr for uninterpreted function " ++ show fnName)
    -- Other cases for the lhs just use the standard `readParameter`.
    p -> do
      Some param <- mapSome (toParameter shapeRepr) <$> readParameter (Proxy @arch) oplist p
      Refl <- fromMaybeError ("mismatching types of parameter and expression for " ++ showF param) $
              testEquality (paramType param) (S.exprType def)
      return $ MapF.Pair param def
readDef _ _ sexpr = E.throwError $ "invalid def: " ++ ((T.unpack (printSExpr mempty sexpr)))


readWithBindings ::
  forall m
  . (Monad m,
     E.MonadError String m,
     MonadIO m)
  => [SExpr]
  -> m FreshNameEnv
readWithBindings bindings = do
  namePairs <- mapM go bindings
  return $ FreshNameEnv $ HM.fromList namePairs
  where go :: SExpr -> m (Text, Text)
        go (WFSList [WFSAtom (AId freshName), WFSAtom (AId oldName)]) = do
          return (freshName, oldName)
        go badBindings = E.throwError $ "Invalid `with` bindings: " ++ (show badBindings)


-- | Parse the whole definition of a templated formula,
-- inside an appropriate monad.
readFormula' :: forall sym arch (sh :: [Symbol]) m.
                (S.IsSymExprBuilder sym,
                 E.MonadError String m,
                 MonadIO m,
                 A.Architecture arch,
                 ShowF (S.SymExpr sym),
                 U.HasLogCfg)
             => sym
             -> FormulaEnv sym arch
             -> A.ShapeRepr arch sh
             -> T.Text
             -> m (ParameterizedFormula sym arch sh)
readFormula' sym env repr text = do
  sexpr <- case parseSExpr text of
             Left err -> E.throwError err
             Right res -> return res
  let firstLine = show $ fmap T.unpack $ take 1 $ T.lines text
  liftIO $ U.logIO U.Info $ "readFormula' of " ++ (show $ T.length text) ++ " bytes " ++ firstLine
  liftIO $ U.logIO U.Debug $ "readFormula' shaperepr " ++ (A.showShapeRepr (Proxy @arch) repr)
  -- Extract the raw s-expression lists for the three components.
  (rawOpList, rawInputList, rawDefList) <- case sexpr of
    WFSList [ WFSList [WFSAtom (AId "operands"), WFSList ops]
            , WFSList [WFSAtom (AId "in"), WFSList inputs]
            , WFSList [WFSAtom (AId "defs"), WFSList defs]] -> return (ops, inputs, defs)
    _ -> E.throwError "invalid top-level structure"

  -- Most of the following bindings have type annotations not because inference
  -- fails, but because with all the type-level hackery we have going on, it
  -- greatly helps human comprehension.

  -- Build the operand list from the given s-expression, validating that it
  -- matches the correct shape as we go.
  let strShape = A.showShapeRepr (Proxy @arch) repr
  operands :: SL.List OpData (OperandTypes arch sh)
    <- prefixError ("invalid operand structure (expected " ++ strShape ++
                    ") from " ++ show rawOpList) $
       buildOperandList' (Proxy @arch) repr rawOpList

  inputs :: [Some (ParsedParameter arch (OperandTypes arch sh))]
    <- readInputs @m @arch @(OperandTypes arch sh) operands rawInputList

  let mkOperandVar :: forall tp. OpData tp -> m (S.BoundVar sym tp)
      mkOperandVar (OpData name tpRepr) =
        let symbol = U.makeSymbol (operandVarPrefix ++ (T.unpack name))
        in liftIO $ S.freshBoundVar sym symbol tpRepr

  opVarList :: SL.List (S.BoundVar sym) (OperandTypes arch sh)
    <- traverseFC mkOperandVar @(OperandTypes arch sh) operands

  -- NOTE: At the moment, we just trust that the semantics definition declares
  -- the correct input operands; instead of validating it, we generate BoundVars
  -- for *all* inputs (partially because it is unclear how to treat immediates
  -- -- do they count as inputs?).

  let mkLiteralVar :: forall tp. BaseTypeRepr tp -> A.Location arch tp -> m (S.BoundVar sym tp)
      mkLiteralVar tpRepr loc =
        let symbol = U.makeSymbol (literalVarPrefix ++ showF loc)
        in liftIO $ S.freshBoundVar sym symbol tpRepr

      buildLitVarMap :: Some (ParsedParameter arch (OperandTypes arch sh))
                     -> MapF.MapF (A.Location arch) (S.BoundVar sym)
                     -> m (MapF.MapF (A.Location arch) (S.BoundVar sym))
      buildLitVarMap (Some (ParsedLiteralParameter loc)) m = (\v -> MapF.insert loc v m) <$> mkLiteralVar (A.locationType loc) loc
      buildLitVarMap (Some (ParsedOperandParameter _ _))        m = return m

  litVars :: MapF.MapF (A.Location arch) (S.BoundVar sym)
    <- foldrM buildLitVarMap MapF.empty inputs
  let defsInfo = DefsInfo { getSym = sym
                          , getEnv = env
                          , getLitVars = litVars
                          , getOpVarList = opVarList
                          , getOpNameList = operands
                          }
  defs <- readDefs defsInfo repr rawDefList

  let finalInputs :: [Some (Parameter arch sh)]
      finalInputs = mapSome (toParameter repr) <$> inputs
      finalOpVarList :: SL.List (BV.BoundVar sym arch) sh
      finalOpVarList =
        -- Wrap each operand variable using 'BV.BoundVar'
        TL.mapFromMapped (Proxy @(OperandTypeWrapper arch)) BV.BoundVar repr opVarList

  return $
    ParameterizedFormula { pfUses = Set.fromList finalInputs
                         , pfOperandVars = finalOpVarList
                         , pfLiteralVars = litVars
                         , pfDefs = defs
                         }

-- | Parse the definition of a templated formula.
readFormula :: (S.IsSymExprBuilder sym,
                A.Architecture arch,
                ShowF (S.SymExpr sym),
                U.HasLogCfg)
            => sym
            -> FormulaEnv sym arch
            -> A.ShapeRepr arch sh
            -> T.Text
            -> IO (Either String (ParameterizedFormula sym arch sh))
readFormula sym env repr text = E.runExceptT $ readFormula' sym env repr text

-- | Read a templated formula definition from file, then parse it.
readFormulaFromFile :: (S.IsSymExprBuilder sym,
                        A.Architecture arch,
                        ShowF (S.SymExpr sym),
                        U.HasLogCfg)
                    => sym
                    -> FormulaEnv sym arch
                    -> A.ShapeRepr arch sh
                    -> FilePath
                    -> IO (Either String (ParameterizedFormula sym arch sh))
readFormulaFromFile sym env repr fp = do
  liftIO $ U.logIO U.Info $ "readFormulaFromFile " ++ fp
  readFormula sym env repr =<< T.readFile fp

-- | Splits @"foo.bar"@ into @("foo", "bar")@ (where
-- `"bar"` can have '.' in it as well).
splitId :: Text -> (Text, Text)
splitId t = (prefix, T.concat rest)
  where (prefix:rest) = T.split (== '.') t


-- | Mapping from fresh names to original names. I.e.,
-- sometimes when we emit an s-expression, a variable in the
-- term will have had an informal (i.e., user readable) name
-- `foo`, but during serialization it was given a fresh
-- formal name `bar` to ensure it didn't clash with any
-- other formal names appearing in the emitted
-- s-expression. This mapping is used to lookup what a fresh
-- name is referring to when needed.
newtype FreshNameEnv = FreshNameEnv (HM.HashMap Text Text)

lookupOrigName :: Text -> FreshNameEnv -> Maybe Text
lookupOrigName nm (FreshNameEnv e) = HM.lookup nm e

-- | Lookup function for parsing names to SymExprs. Names of
-- the form `const.foo` are assumed to be opaque constants
-- and are compiled as an uninterpreted nullary function
-- with name "foo"; all other names are simply looked up in
-- the provided DefsInfo.
exprLookup ::
  ( S.IsSymExprBuilder sym
  , A.Architecture arch)
  => sym
  -> DefsInfo sym arch sh
  -> FreshNameEnv
  -> Text
  -> IO (Maybe (Some (S.SymExpr sym)))
exprLookup sym info freshNameEnv nm
  | Just origName <- lookupOrigName nm freshNameEnv =
      exprLookup sym info freshNameEnv origName
  | otherwise =
    case splitId nm of
      (prefix, suffix) ->
        case prefix of
          "op" ->
            maybe
            (error $ printf "couldn't find operand %s" suffix)
            (viewSome (\(IndexWithType _tpRepr idx) ->
                          return $ Just $ Some . S.varExpr (getSym info)
                          $ ((getOpVarList info) SL.!! idx)))
            (findOpListIndex suffix (getOpNameList info))
          "loc" ->
            maybe
            (error $ printf "%s is an invalid literal for this arch" suffix)
            (\(Some loc) ->
               maybe
               (error $ "Parsed location was not recognized: " ++ (showF loc))
               (viewSome (\x -> return $ Just $ Some $ S.varExpr (getSym info) x))
               (Some <$> (MapF.lookup loc (getLitVars info))))
            (A.readLocation $ T.unpack suffix)
          "const" ->
            case userSymbol (T.unpack suffix) of
              Right constSym -> do
                -- TODO only call `freshTotalUninterpFn` the
                -- first time `constName` is seen, otherwise
                -- used cached version
                let ret = BaseStructRepr Ctx.empty
                e <- liftIO $ S.freshTotalUninterpFn sym constSym Ctx.empty ret
                f <- liftIO $ S.applySymFn sym e Ctx.empty
                return $ Just $ Some f
              Left _ -> error $ printf "couldn't parse semmc constant name %s" (show suffix)
          _ -> return Nothing

-- | Lookup function for parsing names to SomeSymFns. Names
-- of the form `uf.undefinedBV.N` (where N is a positive
-- natural number) are assumed to be fresh bitvector
-- constants of length N; all other names are simply looked
-- up in the DefsInfo function env.
fnLookup ::
  (S.IsSymExprBuilder sym)
  => sym
  -> DefsInfo sym arch sh
  -> FreshNameEnv
  -> Text
  -> IO (Maybe (SomeSymFn sym))
fnLookup sym info freshNameEnv nm
  | Just origName <- lookupOrigName nm freshNameEnv =
      fnLookup sym info freshNameEnv origName
  | otherwise =
      case T.split (=='.') nm of
        ["uf", "undefinedBV", sizeStr] -> do
          case reads (T.unpack sizeStr) :: [(Integer, String)] of
            ((size,""):_) ->
              case NR.someNat @Integer (fromIntegral size) of
                Just (Some nr) -> do
                  case NR.testLeq (knownNat @1) nr of
                    Just NR.LeqProof -> do
                      let rty = BaseBVRepr nr
                      fn <- liftIO (S.freshTotalUninterpFn sym (U.makeSymbol "uf.undefined") Ctx.empty rty)
                      return $ Just $ SomeSymFn fn
                      -- TODO don't use error here, use E.throwError
                    Nothing -> error $ printf "Invalid size for undefined value: %d" (NR.widthVal nr)
                Nothing -> error $ printf "Invalid size for undefined value: %d" sizeStr
            _ -> error $ printf "couldn't parse semmc bitvector size %s" (show sizeStr)
        _ -> case Map.lookup (T.unpack nm) (envFunctions (getEnv info)) of
               Just ((SomeSome fn), _) -> return $ Just $ SomeSymFn fn
               Nothing -> return Nothing



-- | Parse the whole definition of a defined function,
-- inside an appropriate monad.
readDefinedFunction' :: forall sym arch m.
                        (S.IsExprBuilder sym,
                         S.IsSymInterface sym,
                         E.MonadError String m,
                         MonadIO m,
                         A.Architecture arch,
                         ShowF (S.SymExpr sym),
                         U.HasLogCfg)
                     => sym
                     -> FormulaEnv sym arch
                     -> T.Text
                     -> m (Some (FunctionFormula sym))
readDefinedFunction' sym env text = do
  sexpr <- case parseSExpr text of
             Left err -> E.throwError err
             Right res -> return res
  let firstLine = show $ fmap T.unpack $ take 1 $ T.lines text
  liftIO $ U.logIO U.Info $
    "readDefinedFunction' of " ++ (show $ T.length text) ++ " bytes " ++ firstLine
  -- Extract the raw s-expressions for the four components.
  (name, rawArgsList, retTypeRaw, bodyRaw) <- case sexpr of
    WFSList [ WFSList [WFSAtom (AId "function"), WFSAtom (AId name)]
            , WFSList [WFSAtom (AId "arguments"), WFSList args]
            , WFSList [WFSAtom (AId "return"), retType]
            , WFSList [WFSAtom (AId "body"), body]
            ] -> return (name, args, retType, body)
    _ -> E.throwError "invalid top-level structure"

  -- Most of the following bindings have type annotations not because inference
  -- fails, but because with all the type-level hackery we have going on, it
  -- greatly helps human comprehension.

  -- Build the argument list from the given s-expression. Unlike with a formula,
  -- we don't know the arguments' types beforehand.
  Some (arguments :: SL.List OpData sh)
    <- buildArgumentList' rawArgsList

  Some (retTypeRepr :: BaseTypeRepr retType)
    <- E.liftEither $ deserializeBaseType retTypeRaw

  let mkArgumentVar :: forall tp. OpData tp -> m (S.BoundVar sym tp)
      mkArgumentVar (OpData varName tpRepr) =
        let symbol = U.makeSymbol (argumentVarPrefix ++ (T.unpack varName))
        in liftIO $ S.freshBoundVar sym symbol tpRepr

  argVarList :: SL.List (S.BoundVar sym) sh
    <- traverseFC mkArgumentVar arguments

  argTypeReprs :: SL.List BaseTypeRepr sh
    <- traverseFC (\(OpData _ tpRepr) -> return tpRepr) arguments

  (Some body :: (Some (S.SymExpr sym))) <-
    case bodyRaw of
      WFSList [WFSAtom (AId "with"), WFSList rawWBindings, innerBodyRaw] -> do
        let info :: (DefsInfo sym arch sh) = DefsInfo { getSym = sym
                                                       , getEnv = env
                                                       , getLitVars = MapF.empty
                                                       , getOpVarList = argVarList
                                                       , getOpNameList = arguments
                                                       }
        freshNameEnv <- readWithBindings rawWBindings
        let cfg = Config { cSymFnLookup = fnLookup sym info freshNameEnv
                         , cExprLookup = exprLookup sym info freshNameEnv
                         }
        res <- liftIO $ deserializeExprWithConfig sym cfg innerBodyRaw
        case res of
          Left msg -> E.throwError msg
          Right val -> return val
      _ -> E.throwError $ "Invalid def body; expected a `with` bindings form, but got: " ++ (show bodyRaw)

  let actualTypeRepr = S.exprType body
  Refl <- case testEquality retTypeRepr actualTypeRepr of
    Just r -> return r
    Nothing -> E.throwError $
      "Body has wrong type: expected " ++ show retTypeRepr ++
      " but found " ++ show actualTypeRepr

  let symbol = U.makeSymbol (T.unpack name)
      argVarAssignment = TL.toAssignmentFwd argVarList
      expand args = allFC S.baseIsConcrete args

  symFn <- liftIO $ S.definedFn sym symbol argVarAssignment body expand
  return $ Some (FunctionFormula { ffName = (T.unpack name)
                                 , ffArgTypes = argTypeReprs
                                 , ffArgVars = argVarList
                                 , ffRetType = retTypeRepr
                                 , ffDef = symFn })

-- | Parse the definition of a templated formula.
readDefinedFunction :: (S.IsExprBuilder sym,
                        S.IsSymInterface sym,
                        A.Architecture arch,
                        ShowF (S.SymExpr sym),
                        U.HasLogCfg)
                    => sym
                    -> FormulaEnv sym arch
                    -> T.Text
                    -> IO (Either String (Some (FunctionFormula sym)))
readDefinedFunction sym env text = E.runExceptT $ readDefinedFunction' sym env text

-- | Read a defined function definition from a file, then parse it.
readDefinedFunctionFromFile :: (S.IsExprBuilder sym,
                                S.IsSymInterface sym,
                                A.Architecture arch,
                                ShowF (S.SymExpr sym),
                                U.HasLogCfg)
                    => sym
                    -> FormulaEnv sym arch
                    -> FilePath
                    -> IO (Either String (Some (FunctionFormula sym)))
readDefinedFunctionFromFile sym env fp = do
  liftIO $ U.logIO U.Info $ "readDefinedFunctionFromFile " ++ fp
  readDefinedFunction sym env =<< T.readFile fp



buildArgumentList' :: forall m
                    . (E.MonadError String m)
                   => [SExpr]
                   -> m (Some (SL.List OpData))
buildArgumentList' [] = return $ Some (SL.Nil)
buildArgumentList' (s:rest) =
  case s of
    WFSList [WFSAtom (AId operand), tyRaw]-> do
      Some tp <- E.liftEither $ deserializeBaseType tyRaw
      Some rest' <- buildArgumentList' rest
      return $ Some ((OpData operand tp) SL.:< rest')
    _ -> E.throwError $ "Expected (operand . 'type) pair: " ++ show s
