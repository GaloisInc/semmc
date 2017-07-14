{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module SemMC.Formula.Instantiate
  ( instantiateFormula
  , copyFormula
  , sequenceFormulas
  , replaceLitVars
  ) where

import           Data.IORef
import           Data.Maybe ( fromJust, isNothing )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S

import           Dismantle.Instruction ( indexOpList, OperandList(..) )

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Util

-- I got tired of typing this.
type SB t st = S.SimpleBuilder t st

foldlMWithKey :: forall k a b m. (Monad m) => (forall s. b -> k s -> a s -> m b) -> b -> MapF.MapF k a -> m b
foldlMWithKey f z0 m = MapF.foldrWithKey f' return m z0
  where f' :: forall s. k s -> a s -> (b -> m b) -> b -> m b
        f' k x c z = f z k x >>= c

-- This reverses it, but we don't care about that for our use case.
type family ShapeCtx (arch :: *) (sh :: [Symbol]) :: Ctx BaseType where
  ShapeCtx _    '[] = EmptyCtx
  ShapeCtx arch (s ': sh) = ShapeCtx arch sh '::> OperandType arch s

buildOpAssignment :: forall sym arch sh.
                  (Architecture arch,
                   S.IsSymInterface sym)
                => sym
                -- ^ Symbolic expression builder
                -> (forall tp'. Location arch tp' -> IO (S.SymExpr sym tp'))
                -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
                -> OperandList (BoundVar sym arch) sh
                -- ^ List of variables corresponding to each operand
                -> OperandList (Operand arch) sh
                -- ^ List of operand values corresponding to each operand
                -> IO (OperandList (TaggedExpr arch sym) sh,
                       Ctx.Assignment (S.BoundVar sym) (ShapeCtx arch sh),
                       Ctx.Assignment (S.SymExpr sym) (ShapeCtx arch sh))
buildOpAssignment _ _ Nil Nil = return (Nil, Ctx.empty, Ctx.empty)
buildOpAssignment sym newVars ((BoundVar var) :> varsRest) (val :> valsRest) = do
  val' <- operandValue (Proxy :: Proxy arch) sym newVars val
  (valsList, varsRest', valsRest') <- buildOpAssignment sym newVars varsRest valsRest
  return (val' :> valsList, Ctx.extend varsRest' var, Ctx.extend valsRest' (unTagged val'))

buildLitAssignment :: forall sym loc.
                      sym
                   -> (forall tp. loc tp -> IO (S.SymExpr sym tp))
                   -> MapF.MapF loc (S.BoundVar sym)
                   -> IO (MapF.Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym)))
buildLitAssignment _ exprLookup = foldlMWithKey f (MapF.Pair Ctx.empty Ctx.empty)
  where f :: forall (tp :: BaseType).
             MapF.Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym))
          -> loc tp
          -> S.BoundVar sym tp
          -> IO (MapF.Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym)))
        f (MapF.Pair varAssn exprAssn) loc var =
          fmap (\expr -> MapF.Pair (Ctx.extend varAssn var) (Ctx.extend exprAssn expr))
               (exprLookup loc)

replaceLitVars :: forall loc t st tp.
                  (OrdF loc)
               => S.SimpleBuilder t st
               -> (forall tp'. loc tp' -> IO (S.Elt t tp'))
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> S.Elt t tp
               -> IO (S.Elt t tp)
replaceLitVars sym newExprs oldVars expr =
  buildLitAssignment sym newExprs oldVars >>=
    \(MapF.Pair varAssn exprAssn) -> S.evalBoundVars sym expr varAssn exprAssn

mapFMapMBoth :: forall k1 v1 k2 v2 m.
                (OrdF k2, Monad m)
             => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp))
             -> MapF.MapF k1 v1
             -> m (MapF.MapF k2 v2)
mapFMapMBoth f = MapF.foldrWithKey f' (return MapF.empty)
  where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
        f' k v wrappedM = do
          (k', v') <- f k v
          m <- wrappedM
          return $ MapF.insert k' v' m

paramToLocation :: forall arch sh tp.
                   (Architecture arch)
                => OperandList (Operand arch) sh
                -> Parameter arch sh tp
                -> Maybe (Location arch tp)
paramToLocation opVals (Operand _ idx) = operandToLocation (Proxy :: Proxy arch) $ indexOpList opVals idx
paramToLocation _      (Literal loc) = Just loc

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

instantiateFormula :: forall arch t st sh.
                      (Architecture arch)
                   => SB t st
                   -> ParameterizedFormula (SB t st) arch sh
                   -> OperandList (Operand arch) sh
                   -> IO (OperandList (TaggedExpr arch (SB t st)) sh, Formula (SB t st) arch)
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
    newDefs <- mapFMapMBoth mapDef defs
    newLitVars <- readIORef newLitVarsRef
    let newActualLitVars = foldrF (MapF.union . extractUsedLocs newLitVars) MapF.empty newDefs

    let -- mapParam (Some param) = maybe Set.empty (Set.singleton . Some) $ paramToLocation opVals param
        newUses = Set.fromList $ mapFKeys newActualLitVars

    -- TODO: Should we filter out definitions that are syntactically identity
    -- functions?

    return $ (opValsList, Formula { formUses = newUses
                                  , formParamVars = newActualLitVars
                                  , formDefs = newDefs
                                  })

data SomeVarAssignment sym = forall ctx. SomeVarAssignment (Ctx.Assignment (S.BoundVar sym) ctx) (Ctx.Assignment (S.SymExpr sym) ctx)

changeVariablesAssignment :: forall sym var.
                             (S.IsSymInterface sym,
                              OrdF var)
                          => MapF.MapF var (S.BoundVar sym)
                          -- ^ Old variables
                          -- -> MapF.MapF var (S.BoundVar sym)
                          -> (forall tp. var tp -> S.SymExpr sym tp)
                          -- ^ New expressions
                          -> SomeVarAssignment sym
changeVariablesAssignment oldVars newExprs =
  let addAssignment :: var tp -> S.BoundVar sym tp -> SomeVarAssignment sym -> SomeVarAssignment sym
      addAssignment var bVar (SomeVarAssignment varsAssign exprsAssign) =
        SomeVarAssignment (Ctx.extend varsAssign bVar)
                          (Ctx.extend exprsAssign $ newExprs var)
  in MapF.foldrWithKey addAssignment (SomeVarAssignment Ctx.empty Ctx.empty) oldVars

copyFormula :: forall t st arch.
               (IsLocation (Location arch))
            => SB t st
            -> Formula (SB t st) arch
            -> IO (Formula (SB t st) arch)
copyFormula sym (Formula { formUses = uses, formParamVars = vars, formDefs = defs}) = do
  let mkVar :: forall tp. Location arch tp -> IO (S.SimpleBoundVar t tp)
      mkVar loc = S.freshBoundVar sym (makeSymbol (showF loc)) (locationType loc)
  newVars <- MapF.traverseWithKey (const . mkVar) vars
  let lookupNewVar :: forall tp. Location arch tp -> S.Elt t tp
      lookupNewVar = S.varExpr sym . fromJust . flip MapF.lookup newVars
  SomeVarAssignment varAssign exprAssign :: SomeVarAssignment (SB t st)
    <- return $ (changeVariablesAssignment vars lookupNewVar)
  let replaceVars :: forall tp. S.Elt t tp -> IO (S.Elt t tp)
      replaceVars e = S.evalBoundVars sym e varAssign exprAssign
  newDefs <- traverseF replaceVars defs
  return $ Formula { formUses = uses
                   , formParamVars = newVars
                   , formDefs = newDefs
                   }

filterMapF :: forall k v. (OrdF k) => (forall tp. k tp -> v tp -> Bool) -> MapF.MapF k v -> MapF.MapF k v
filterMapF f = MapF.foldrWithKey go MapF.empty
  where go :: forall tp. k tp -> v tp -> MapF.MapF k v -> MapF.MapF k v
        go key value m
          | f key value = MapF.insert key value m
          | otherwise   = m

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
  Formula { formUses = uses1
          , formParamVars = vars1
          , formDefs = defs1
          } <- copyFormula sym form1
  Formula { formUses = uses2
          , formParamVars = vars2
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
  SomeVarAssignment varAssign exprAssign :: SomeVarAssignment (SB t st)
    <- return $ changeVariablesAssignment vars2 varReplace
  let replaceVars :: forall tp. S.Elt t tp -> IO (S.Elt t tp)
      replaceVars e = S.evalBoundVars sym e varAssign exprAssign
  newDefs2 <- traverseF replaceVars defs2

  let newDefs = MapF.union newDefs2 defs1
      -- The new vars are all the vars from the first, plus the vars from the
      -- second that are neither required in the first nor defined in the first.
      newVars = MapF.union vars1 (filterMapF (\k _ -> isNothing $ MapF.lookup k defs1) vars2)
      newUses = Set.union uses1 (Set.filter (\(Some loc) -> isNothing $ MapF.lookup loc defs1) uses2)

  return $ Formula { formUses = newUses
                   , formParamVars = newVars
                   , formDefs = newDefs
                   }
