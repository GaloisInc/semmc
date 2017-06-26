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
  ( WrappedExpr(..)
  , instantiateFormula
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
import           Lang.Crucible.Solver.Symbol ( SolverSymbol, userSymbol )

import           Dismantle.Instruction ( OperandList(..) )

import           SemMC.Formula
import           SemMC.Architecture

evalBoundVars = undefined

-- I got tired of typing this.
type SB t st = S.SimpleBuilder t st

makeSymbol :: String -> SolverSymbol
makeSymbol name = case userSymbol name of
                    Right symbol -> symbol
                    Left _ -> error "tried to create symbol with bad name"

foldlMWithKey :: forall k a b m. (Monad m) => (forall s. b -> k s -> a s -> m b) -> b -> MapF.MapF k a -> m b
foldlMWithKey f z0 m = MapF.foldrWithKey f' return m z0
  where f' :: forall s. k s -> a s -> (b -> m b) -> b -> m b
        f' k x c z = f z k x >>= c

-- This reverses it, but we don't care about that for our use case.
type family ShapeCtx (arch :: *) (sh :: [Symbol]) :: Ctx BaseType where
  ShapeCtx _    '[] = EmptyCtx
  ShapeCtx arch (s ': sh) = ShapeCtx arch sh '::> OperandType arch s

newtype WrappedExpr sym arch s = WrappedExpr { unWrappedExpr :: S.SymExpr sym (OperandType arch s) }

instance (ShowF (S.SymExpr sym)) => ShowF (WrappedExpr sym arch) where
  showF (WrappedExpr e) = showF e

buildAssignment :: forall sym arch sh.
                  (Architecture arch,
                   S.IsSymInterface sym)
                => sym
                -- ^ Symbolic expression builder
                -> (forall tp'. Location arch tp' -> IO (S.BoundVar sym tp'))
                -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
                -> OperandList (BoundVar sym arch) sh
                -- ^ List of variables corresponding to each operand
                -> OperandList (Operand arch) sh
                -- ^ List of operand values corresponding to each operand
                -> IO (OperandList (WrappedExpr sym arch) sh,
                       Ctx.Assignment (S.BoundVar sym) (ShapeCtx arch sh),
                       Ctx.Assignment (S.SymExpr sym) (ShapeCtx arch sh))
buildAssignment   _       _                         Nil                Nil = return (Nil, Ctx.empty, Ctx.empty)
buildAssignment sym newVars ((BoundVar var) :> varsRest) (val :> valsRest) = do
  val' <- operandValue (Proxy :: Proxy arch) sym newVars val
  (valsList, varsRest', valsRest') <- buildAssignment sym newVars varsRest valsRest
  return (WrappedExpr val' :> valsList, Ctx.extend varsRest' var, Ctx.extend valsRest' val')

replaceOpVars :: forall arch t st sh tp.
                 (Architecture arch)
              => SB t st
              -- ^ Symbolic expression builder
              -> (forall tp'. Location arch tp' -> IO (S.SimpleBoundVar t tp'))
              -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
              -> OperandList (BoundVar (SB t st) arch) sh
              -- ^ List of variables corresponding to each operand
              -> OperandList (Operand arch) sh
              -- ^ List of operand values corresponding to each operand
              -> S.Elt t tp
              -- ^ Expression to do the replace in
              -> IO (OperandList (WrappedExpr (SB t st) arch) sh, S.Elt t tp)
replaceOpVars sym newVars vars vals expr =
  buildAssignment sym newVars vars vals >>= \(valsList, vars, vals) ->
    (valsList,) <$> evalBoundVars sym expr vars vals

replaceLitVars :: forall (loc :: BaseType -> *) t st tp.
                  (OrdF loc)
               => SB t st
               -> (forall tp'. loc tp' -> IO (S.SimpleBoundVar t tp'))
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> S.Elt t tp
               -> IO (S.Elt t tp)
replaceLitVars sym newVars oldVars expr0 = foldlMWithKey f expr0 oldVars
  where f :: forall tp'. S.Elt t tp -> loc tp' -> S.SimpleBoundVar t tp' -> IO (S.Elt t tp)
        f expr k oldVar = do
          newExpr <- S.varExpr sym <$> newVars k
          evalBoundVars sym expr (Ctx.extend Ctx.empty oldVar) (Ctx.extend Ctx.empty newExpr)

mapFMapMBoth :: forall k1 v1 k2 v2 m. (OrdF k2, Monad m) => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp)) -> MapF.MapF k1 v1 -> m (MapF.MapF k2 v2)
mapFMapMBoth f = MapF.foldrWithKey f' (return MapF.empty)
  where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
        f' k v wrappedM = do
          (k', v') <- f k v
          m <- wrappedM
          return $ MapF.insert k' v' m

paramToLocation :: forall arch sh tp. (Architecture arch) => OperandList (Operand arch) sh -> Parameter arch sh tp -> Maybe (Location arch tp)
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
                   -> IO (OperandList (WrappedExpr (SB t st) arch) sh, Formula (SB t st) arch)
instantiateFormula
  sym
  (ParameterizedFormula { pfUses = uses
                        , pfOperandVars = opVars
                        , pfLiteralVars = litVars
                        , pfDefs = defs
                        })
  opVals = do
    -- We store the new lit vars in an IORef so we can create them as we go.
    -- IORef should be safe, since we don't have any multithreaded code here.
    newLitVarsRef <- newIORef MapF.empty
    let newLitVarLookup :: forall tp. Location arch tp -> IO (S.SimpleBoundVar t tp)
        newLitVarLookup = lookupOrCreateVar sym newLitVarsRef

    (opValsList, opVarsAssn, opValsAssn) <- buildAssignment sym newLitVarLookup opVars opVals

    let mapDef :: forall tp. Parameter arch sh tp -> S.Elt t tp -> IO (Location arch tp, S.Elt t tp)
        mapDef p e = case paramToLocation opVals p of
          Just loc -> (loc,) <$> (replaceLitVars sym newLitVarLookup litVars =<< evalBoundVars sym e opVarsAssn opValsAssn)
          Nothing -> error "XXX: handle this error case more gracefully"

    newDefs <- mapFMapMBoth mapDef defs
    -- ^ This loads the relevant lit vars into the map, so reading the IORef
    -- must happen afterwards :)
    newLitVars <- readIORef newLitVarsRef

    let mapParam (Some param) = maybe Set.empty (Set.singleton . Some) $ paramToLocation opVals param
        newUses = foldMap mapParam uses

    return $ (opValsList, Formula { formUses = newUses
                                  , formParamVars = newLitVars
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
      replaceVars e = evalBoundVars sym e varAssign exprAssign
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
        | Just expr <- MapF.lookup loc defs1 = expr
        -- ^ If this location is defined in the first formula, use the new
        -- definition.
        | Just newVar <- MapF.lookup loc vars1 = S.varExpr sym newVar
        -- ^ If this location isn't defined in the first formula, but is used by
        -- it, use the first formula's variable.
        | otherwise = S.varExpr sym $ fromJust $ MapF.lookup loc vars2
        -- ^ Otherwise, use the original variable.
  SomeVarAssignment varAssign exprAssign :: SomeVarAssignment (SB t st)
    <- return $ changeVariablesAssignment vars2 varReplace
  let replaceVars :: forall tp. S.Elt t tp -> IO (S.Elt t tp)
      replaceVars e = evalBoundVars sym e varAssign exprAssign
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
