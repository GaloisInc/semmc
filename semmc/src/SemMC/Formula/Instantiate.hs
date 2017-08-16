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
  , replaceLitVars
  , paramToLocation
  ) where

import           Data.Foldable ( foldlM )
import           Data.IORef
import           Data.Maybe ( fromJust, isNothing )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.ShapedList ( indexShapedList, ShapedList(..) )
import           Data.Parameterized.TraversableF
import           Data.Proxy ( Proxy(..) )
import           GHC.TypeLits ( Symbol )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S

import           SemMC.Architecture
import           SemMC.Formula.Formula
import           SemMC.Util

-- I got tired of typing this.
type SB t st = S.SimpleBuilder t st

-- | Monadically map both keys and values of a 'MapF.MapF'.
mapFMapBothM :: forall k1 v1 k2 v2 m.
                (OrdF k2, Monad m)
             => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp))
             -> MapF.MapF k1 v1
             -> m (MapF.MapF k2 v2)
mapFMapBothM f = MapF.foldrWithKey f' (return MapF.empty)
  where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
        f' k v wrappedM = do
          (k', v') <- f k v
          m <- wrappedM
          return $ MapF.insert k' v' m

-- | Filter the elements of a 'MapF.MapF'.
filterMapF :: forall k v. (OrdF k) => (forall tp. k tp -> v tp -> Bool) -> MapF.MapF k v -> MapF.MapF k v
filterMapF f = MapF.foldrWithKey go MapF.empty
  where go :: forall tp. k tp -> v tp -> MapF.MapF k v -> MapF.MapF k v
        go key value m
          | f key value = MapF.insert key value m
          | otherwise   = m

-- | Convert a type-level list of operands to a Crucible-style context of
-- operand types. This reverses it, but we don't care about that for our use
-- case (and not doing so would be harder).
type family ShapeCtx (arch :: *) (sh :: [Symbol]) :: Ctx BaseType where
  ShapeCtx _    '[] = EmptyCtx
  ShapeCtx arch (s ': sh) = ShapeCtx arch sh '::> OperandType arch s

-- | For a given pair of bound variables and operands, build up:
-- 1. 'TaggedExpr's corresponding to each operand.
-- 2. a 'Ctx.Assignment' form of the bound variables (for use in substitution)
-- 3. a 'Ctx.Assignment' of the created expressions corresponding to each
--    operand
buildOpAssignment :: forall sym arch sh.
                  (Architecture arch,
                   S.IsSymInterface sym)
                => sym
                -- ^ Symbolic expression builder
                -> (forall tp'. Location arch tp' -> IO (S.SymExpr sym tp'))
                -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
                -> ShapedList (BoundVar sym arch) sh
                -- ^ List of variables corresponding to each operand
                -> ShapedList (Operand arch) sh
                -- ^ List of operand values corresponding to each operand
                -> IO (ShapedList (TaggedExpr arch sym) sh,
                       Ctx.Assignment (S.BoundVar sym) (ShapeCtx arch sh),
                       Ctx.Assignment (S.SymExpr sym) (ShapeCtx arch sh))
buildOpAssignment _ _ Nil Nil = return (Nil, Ctx.empty, Ctx.empty)
buildOpAssignment sym newVars ((BoundVar var) :> varsRest) (val :> valsRest) = do
  val' <- operandValue (Proxy :: Proxy arch) sym newVars val
  (valsList, varsRest', valsRest') <- buildOpAssignment sym newVars varsRest valsRest
  return (val' :> valsList, varsRest' Ctx.%> var, valsRest' Ctx.%> (unTagged val'))

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
          fmap (\expr -> Pair (varAssn Ctx.%> var) (exprAssn Ctx.%> expr))
               (exprLookup loc)

-- | Replace all the variables in the given 'SomeVarAssignment' with their
-- corresponding expressions, in the given expression.
replaceVars :: forall t st tp
             . SB t st
            -> SomeVarAssignment (SB t st)
            -> S.Elt t tp
            -> IO (S.Elt t tp)
replaceVars sym (Pair varAssn exprAssn) expr =
  S.evalBoundVars sym expr varAssn exprAssn

-- | Given a generator for expressions given machine locations and all used
-- machine locations, this replaces bound variables with their corresponding
-- expressions in the given top-level expression.
replaceLitVars :: forall loc t st tp.
                  (OrdF loc)
               => SB t st
               -> (forall tp'. loc tp' -> IO (S.Elt t tp'))
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> S.Elt t tp
               -> IO (S.Elt t tp)
replaceLitVars sym newExprs oldVars expr = do
  assn <- buildLitAssignment (Proxy @(SB t st)) newExprs oldVars
  replaceVars sym assn expr

-- | Get the corresponding location of a parameter, if it actually corresponds
-- to one.
paramToLocation :: forall arch sh tp.
                   (Architecture arch)
                => ShapedList (Operand arch) sh
                -> Parameter arch sh tp
                -> Maybe (Location arch tp)
paramToLocation opVals (Operand _ idx) =
  operandToLocation (Proxy @arch) $ indexShapedList opVals idx
paramToLocation _ (Literal loc) = Just loc

-- | Return the bound variable corresponding to the given location, by either
-- looking it up in the given map or creating it then inserting it if it doesn't
-- exist yet.
lookupOrCreateVar :: (S.IsSymInterface sym,
                      IsLocation loc)
                  => sym
                  -> IORef (MapF.MapF loc (S.BoundVar sym))
                  -> loc tp
                  -> IO (S.BoundVar sym tp)
lookupOrCreateVar sym litVarsRef loc = do
  litVars <- readIORef litVarsRef
  case MapF.lookup loc litVars of
    Just bVar -> return bVar
    Nothing -> do
      bVar <- S.freshBoundVar sym (makeSymbol (showF loc)) (locationType loc)
      writeIORef litVarsRef (MapF.insert loc bVar litVars)
      return bVar

-- | Create a concrete 'Formula' from the given 'ParameterizedFormula' and
-- operand list. The first result is the list of created 'TaggedExpr's for each
-- operand that are used within the returned formula.
instantiateFormula :: forall arch t st sh.
                      (Architecture arch)
                   => SB t st
                   -> ParameterizedFormula (SB t st) arch sh
                   -> ShapedList (Operand arch) sh
                   -> IO (ShapedList (TaggedExpr arch (SB t st)) sh, Formula (SB t st) arch)
instantiateFormula
  sym
  (ParameterizedFormula { pfOperandVars = opVars
                        , pfLiteralVars = litVars
                        , pfDefs = defs
                        })
  opVals = do
    -- We store the new lit vars in an IORef so we can create them as we go.
    -- IORef should be safe, since we don't have any multithreaded code here.
    newLitVarsRef <- newIORef MapF.empty
    let newLitExprLookup :: forall tp. Location arch tp -> IO (S.Elt t tp)
        newLitExprLookup loc = S.varExpr sym <$> lookupOrCreateVar sym newLitVarsRef loc

    (opValsList, opVarsAssn, opValsAssn) <- buildOpAssignment sym newLitExprLookup opVars opVals

    let mapDef :: forall tp. Parameter arch sh tp -> S.Elt t tp -> IO (Location arch tp, S.Elt t tp)
        mapDef p e = case paramToLocation opVals p of
          Just loc -> (loc,) <$> (replaceLitVars sym newLitExprLookup litVars =<< S.evalBoundVars sym e opVarsAssn opValsAssn)
          Nothing -> error $ unwords ["parameter", show p, "is not a valid location"]

    -- This loads the relevant lit vars into the map, so reading the IORef
    -- must happen afterwards :)
    newDefs <- mapFMapBothM mapDef defs
    newLitVars <- readIORef newLitVarsRef
    let newActualLitVars = foldrF (MapF.union . extractUsedLocs newLitVars) MapF.empty newDefs

    -- TODO: Should we filter out definitions that are syntactically identity
    -- functions?

    return $ (opValsList, Formula { formParamVars = newActualLitVars
                                  , formDefs = newDefs
                                  })

-- | Create a new formula with the same semantics, but with fresh bound vars.
copyFormula :: forall t st arch.
               (IsLocation (Location arch))
            => SB t st
            -> Formula (SB t st) arch
            -> IO (Formula (SB t st) arch)
copyFormula sym (Formula { formParamVars = vars, formDefs = defs}) = do
  let mkVar :: forall tp. Location arch tp -> IO (S.SimpleBoundVar t tp)
      mkVar loc = S.freshBoundVar sym (makeSymbol (showF loc)) (locationType loc)
  newVars <- MapF.traverseWithKey (const . mkVar) vars
  let lookupNewVar :: forall tp. Location arch tp -> S.Elt t tp
      lookupNewVar = S.varExpr sym . fromJust . flip MapF.lookup newVars
  assn <- buildLitAssignment (Proxy @(SB t st)) (return . lookupNewVar) vars
  newDefs <- traverseF (replaceVars sym assn) defs
  return $ Formula { formParamVars = newVars
                   , formDefs = newDefs
                   }

-- | Combine two formulas in sequential execution
sequenceFormulas :: forall t st arch.
                    (Architecture arch)
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

  let varReplace :: forall tp. Location arch tp -> S.Elt t tp
      varReplace loc
        -- If this location is defined in the first formula, use the new
        -- definition.
        | Just expr <- MapF.lookup loc defs1 = expr
        -- If this location isn't defined in the first formula, but is used by
        -- it, use the first formula's variable.
        | Just newVar <- MapF.lookup loc vars1 = S.varExpr sym newVar
        -- Otherwise, use the original variable.
        | otherwise = S.varExpr sym $ fromJust $ MapF.lookup loc vars2
  assn <- buildLitAssignment (Proxy @(SB t st)) (return . varReplace) vars2
  newDefs2 <- traverseF (replaceVars sym assn) defs2

  let newDefs = MapF.union newDefs2 defs1
      -- The new vars are all the vars from the first, plus the vars from the
      -- second that are neither required in the first nor defined in the first.
      notWrittenVars2 = filterMapF (\k _ -> isNothing $ MapF.lookup k defs1) vars2
      newVars = MapF.union vars1 notWrittenVars2

  return $ Formula { formParamVars = newVars
                   , formDefs = newDefs
                   }
