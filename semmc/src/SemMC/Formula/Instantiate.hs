{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Formula.Instantiate
  ( instantiateFormula
  , copyFormula
  , sequenceFormulas
  , condenseFormulas
  , replaceVars
  , replaceLitVars
  , paramToLocation
  ) where

import           Control.Monad.State
import           Data.Foldable                      ( foldlM, foldrM )
import           Data.Maybe                         ( fromJust, isNothing )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context         as Ctx
import           Data.Parameterized.Ctx
import qualified Data.Parameterized.List            as SL
import qualified Data.Parameterized.Map             as MapF
import           Data.Parameterized.Pair            ( Pair(..) )
import           Data.Parameterized.Some            ( Some(..) )
import           Data.Parameterized.TraversableF
import           Data.Proxy                         ( Proxy(..) )
import           GHC.TypeLits                       ( Symbol )
import           Text.Printf                        ( printf )

import           What4.BaseTypes
import qualified What4.Interface     as S
import qualified What4.Expr.Builder as S

import qualified SemMC.Architecture                 as A
import qualified SemMC.BoundVar                     as BV
import qualified SemMC.Formula.Eval                 as FE
import           SemMC.Formula.Formula
import qualified SemMC.Util                         as U

-- I got tired of typing this.
type SB t st = S.ExprBuilder t st

-- | Convert a type-level list of operands to a Crucible-style context of
-- operand types. This reverses it, but we don't care about that for our use
-- case (and not doing so would be harder).
type family ShapeCtx (arch :: *) (sh :: [Symbol]) :: Ctx BaseType where
  ShapeCtx _    '[] = EmptyCtx
  ShapeCtx arch (s ': sh) = ShapeCtx arch sh '::> A.OperandType arch s

data OperandAssignment sym arch sh =
  OperandAssignment { opAssnTaggedExprs :: SL.List (A.TaggedExpr arch sym) sh
                    -- ^ The raw values obtained by calling 'operandValue' on
                    -- each of the operands.
                    , opAssnVars :: Ctx.Assignment (S.BoundVar sym) (ShapeCtx arch sh)
                    -- ^ The original bound variables associated with each
                    -- operand, but turned into 'Ctx.Assignment' form instead of
                    -- a 'SL.List'.
                    , opAssnBareExprs :: Ctx.Assignment (S.SymExpr sym) (ShapeCtx arch sh)
                    -- ^ The bare 'S.SymExpr's corresponding to each
                    -- 'TaggedExpr', but in 'Ctx.Assignment' form.
                    }

extendAssn :: (A.Architecture arch)
           => A.TaggedExpr arch sym s
           -> S.BoundVar sym (A.OperandType arch s)
           -> OperandAssignment sym arch sh
           -> OperandAssignment sym arch (s ': sh)
extendAssn newExpr newVar oldAssn =
  OperandAssignment { opAssnTaggedExprs = newExpr SL.:< opAssnTaggedExprs oldAssn
                    , opAssnVars = opAssnVars oldAssn Ctx.:> newVar
                    , opAssnBareExprs = opAssnBareExprs oldAssn Ctx.:> A.unTagged newExpr
                    }

-- | For a given pair of bound variables and operands, build up:
-- 1. 'TaggedExpr's corresponding to each operand.
-- 2. a 'Ctx.Assignment' form of the bound variables (for use in substitution)
-- 3. a 'Ctx.Assignment' of the created expressions corresponding to each
--    operand
buildOpAssignment :: forall sym arch sh.
                  (A.Architecture arch,
                   S.IsSymExprBuilder sym)
                => sym
                -- ^ Symbolic expression builder
                -> (forall tp'. A.Location arch tp' -> IO (S.SymExpr sym tp'))
                -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
                -> SL.List (BV.BoundVar sym arch) sh
                -- ^ List of variables corresponding to each operand
                -> SL.List (A.Operand arch) sh
                -- ^ List of operand values corresponding to each operand
                -> IO (OperandAssignment sym arch sh)
buildOpAssignment _ _ SL.Nil SL.Nil = return (OperandAssignment SL.Nil Ctx.empty Ctx.empty)
buildOpAssignment sym newVars ((BV.BoundVar var) SL.:< varsRest) (val SL.:< valsRest) =
  extendAssn <$> A.operandValue (Proxy @arch) sym newVars val
             <*> pure var
             <*> buildOpAssignment sym newVars varsRest valsRest

type SomeVarAssignment sym = Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym))

-- | Given
-- 1. a generator for expressions given locations
-- 2. a mapping of all locations used to their corresponding bound variables
-- this function builds an assignment of bound variables to expressions to use
-- as a substitution.
buildLitAssignment :: forall m proxy sym loc
                    . (Monad m)
                   => proxy sym
                   -> (forall tp. loc tp -> m (S.SymExpr sym tp))
                   -> MapF.MapF loc (S.BoundVar sym)
                   -> m (SomeVarAssignment sym)
buildLitAssignment _ exprLookup = foldlM f (MapF.Pair Ctx.empty Ctx.empty) . MapF.toList
  where f (Pair varAssn exprAssn) (Pair loc var) =
          fmap (\expr -> Pair (varAssn Ctx.:> var) (exprAssn Ctx.:> expr))
               (exprLookup loc)

-- | Replace all the variables in the given 'SomeVarAssignment' with their
-- corresponding expressions, in the given expression.
replaceVars :: forall t st tp
             . SB t st
            -> SomeVarAssignment (SB t st)
            -> S.Expr t tp
            -> IO (S.Expr t tp)
replaceVars sym (Pair varAssn exprAssn) expr =
  S.evalBoundVars sym expr varAssn exprAssn

-- | Given a generator for expressions given machine locations and all used
-- machine locations, this replaces bound variables with their corresponding
-- expressions in the given top-level expression.
replaceLitVars :: forall loc t st tp.
                  (OrdF loc)
               => SB t st
               -> (forall tp'. loc tp' -> IO (S.Expr t tp'))
               -> MapF.MapF loc (S.ExprBoundVar t)
               -> S.Expr t tp
               -> IO (S.Expr t tp)
replaceLitVars sym newExprs oldVars expr = do
  assn <- buildLitAssignment (Proxy @(SB t st)) newExprs oldVars
  replaceVars sym assn expr

-- | Get the corresponding location of a parameter, if it actually corresponds
-- to one.
paramToLocation :: forall arch sh tp.
                   (A.Architecture arch)
                => SL.List (A.Operand arch) sh
                -> Parameter arch sh tp
                -> Maybe (A.Location arch tp)
paramToLocation opVals (OperandParameter _ idx) =
 A.operandToLocation (Proxy @arch) (opVals SL.!! idx)
paramToLocation _ (LiteralParameter loc) = Just loc
paramToLocation opVals (FunctionParameter fnName wo rep) =
  case fnName `lookup` A.locationFuncInterpretation (Proxy @arch) of
    Nothing -> error ("No function interpretation for " ++ show fnName)
    Just interp ->
      case A.locationInterp interp of
        LocationFuncInterp fn -> fn opVals wo rep

type Literals arch sym = MapF.MapF (A.Location arch) (S.BoundVar sym)

-- | Create a concrete 'Formula' from the given 'ParameterizedFormula' and
-- operand list. The first result is the list of created 'TaggedExpr's for each
-- operand that are used within the returned formula.
instantiateFormula :: forall arch t st sh.
                      ( A.Architecture arch
                      , S.IsSymExprBuilder (SB t st))
                   => SB t st
                   -> ParameterizedFormula (SB t st) arch sh
                   -> SL.List (A.Operand arch) sh
                   -> IO (SL.List (A.TaggedExpr arch (SB t st)) sh, Formula (SB t st) arch)
instantiateFormula
  sym
  pf@(ParameterizedFormula { pfOperandVars = opVars
                           , pfLiteralVars = litVars
                           , pfDefs = defs
                           })
  opVals = do
    let rewrite :: forall tp . S.Expr t tp -> IO (S.Expr t tp, Literals arch (SB t st))
        rewrite = FE.evaluateFunctions sym pf opVals (fmap A.exprInterp <$> A.locationFuncInterpretation (Proxy @ arch))
    (defs', litVars') <- mapAccumLMF litVars defs $ \m e ->
      fmap (`MapF.union` m) <$> rewrite e
    let addLitVar (Some loc) m = do
          bVar <- S.freshBoundVar sym (U.makeSymbol (showF loc)) (A.locationType loc)
          return (MapF.insert loc bVar m)
    newLitVars <- foldrM addLitVar MapF.empty A.allLocations
    let newLitExprLookup :: A.Location arch tp -> IO (S.Expr t tp)
        -- 'newLitVars' has all locations in it, so this 'fromJust' is total.
        newLitExprLookup loc =
            (return . S.varExpr sym . U.fromJust' ("newLitExprLookup: " ++ showF loc) . flip MapF.lookup newLitVars) loc

    OperandAssignment { opAssnTaggedExprs = opTaggedExprs
                      , opAssnVars = opVarsAssn
                      , opAssnBareExprs = opExprsAssn
                      } <- buildOpAssignment sym newLitExprLookup opVars opVals
    let instantiateDefn :: forall tp. Parameter arch sh tp -> S.Expr t tp -> IO (A.Location arch tp, S.Expr t tp)
        instantiateDefn definingParam definition = do
          definingLoc <- case paramToLocation opVals definingParam of
            Just loc -> return loc
            Nothing -> fail $ printf "parameter %s is not a valid location" (show definingParam)
          opVarsReplaced <- S.evalBoundVars sym definition opVarsAssn opExprsAssn
          litVarsReplaced <-
            replaceLitVars sym newLitExprLookup litVars' opVarsReplaced
          return (definingLoc, litVarsReplaced)

    newDefs <- U.mapFMapBothM instantiateDefn defs'
    -- 'newLitVars' has variables for /all/ of the machine locations. Here we
    -- extract only the ones that are actually used.
    let newActualLitVars = foldrF (MapF.union . U.extractUsedLocs newLitVars) MapF.empty newDefs

    -- TODO: Should we filter out definitions that are syntactically identity
    -- functions?

    return $ (opTaggedExprs, Formula { formParamVars = newActualLitVars
                                     , formDefs = newDefs
                                     })

-- | Create a new formula with the same semantics, but with fresh bound vars.
copyFormula :: forall t st arch.
               (A.IsLocation (A.Location arch), U.HasCallStack)
            => SB t st
            -> Formula (SB t st) arch
            -> IO (Formula (SB t st) arch)
copyFormula sym (Formula { formParamVars = vars, formDefs = defs}) = do
  let mkVar :: forall tp. A.Location arch tp -> IO (S.ExprBoundVar t tp)
      mkVar loc = S.freshBoundVar sym (U.makeSymbol (showF loc)) (A.locationType loc)
  newVars <- MapF.traverseWithKey (const . mkVar) vars
  let lookupNewVar :: forall tp. A.Location arch tp -> S.Expr t tp
      lookupNewVar = S.varExpr sym . U.fromJust' "copyFormula" . flip MapF.lookup newVars
  assn <- buildLitAssignment (Proxy @(SB t st)) (return . lookupNewVar) vars
  newDefs <- traverseF (replaceVars sym assn) defs
  return $ Formula { formParamVars = newVars
                   , formDefs = newDefs
                   }

-- | Combine two formulas in sequential execution
sequenceFormulas :: forall t st arch.
                    (A.Architecture arch)
                 => SB t st
                 -> Formula (SB t st) arch
                 -> Formula (SB t st) arch
                 -> IO (Formula (SB t st) arch)
sequenceFormulas sym form1 form2 = do
  -- First, copy them, just to be safe. This might not be necessary for one of
  -- them, but I'll keep it like this for now.
  Formula { formParamVars = vars1
          , formDefs = defs1
          } <- copyFormula sym form1
  Formula { formParamVars = vars2
          , formDefs = defs2
          } <- copyFormula sym form2

  let varReplace :: forall tp. A.Location arch tp -> S.Expr t tp
      varReplace loc
        -- If this location is defined in the first formula, use the new
        -- definition.
        | Just expr <- MapF.lookup loc defs1 = expr
        -- If this location isn't defined in the first formula, but is used by
        -- it, use the first formula's variable.
        | Just newVar <- MapF.lookup loc vars1 = S.varExpr sym newVar
        -- Otherwise, use the original variable.
        | otherwise = S.varExpr sym $ U.fromJust' "sequenceFormulas" $ MapF.lookup loc vars2
  assn <- buildLitAssignment (Proxy @(SB t st)) (return . varReplace) vars2
  newDefs2 <- traverseF (replaceVars sym assn) defs2

  let newDefs = MapF.union newDefs2 defs1
      -- The new vars are all the vars from the first, plus the vars from the
      -- second that are neither required in the first nor defined in the first.
      notWrittenVars2 = U.filterMapF (\k _ -> isNothing $ MapF.lookup k defs1) vars2
      newVars = MapF.union vars1 notWrittenVars2

  return $ Formula { formParamVars = newVars
                   , formDefs = newDefs
                   }

condenseFormulas :: forall t f st arch.
                    ( A.Architecture arch
                    , Foldable f
                    )
                 => SB t st
                 -> f (Formula (SB t st) arch)
                 -> IO (Formula (SB t st) arch)
condenseFormulas sym = foldrM (sequenceFormulas sym) emptyFormula

mapAccumLMF :: forall acc m t a .
  (Monad m, TraversableF t) =>
  acc ->
  t a ->
  (forall z. acc -> a z -> m (a z, acc)) ->
  m (t a, acc)
mapAccumLMF acc0 tea f = runStateT (traverseF go tea) acc0
  where
    go :: forall z . a z -> StateT acc m (a z)
    go x = StateT $ \acc -> f acc x
