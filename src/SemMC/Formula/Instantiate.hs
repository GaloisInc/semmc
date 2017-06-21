{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Formula.Instantiate where

import           Data.IORef
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import           Lang.Crucible.Solver.Symbol ( SolverSymbol, userSymbol )

import           Dismantle.Instruction ( OperandList(..) )

import           SemMC.Formula
import           SemMC.Architecture

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
                -> IO (Ctx.Assignment (S.BoundVar sym) (ShapeCtx arch sh),
                       Ctx.Assignment (S.SymExpr sym) (ShapeCtx arch sh))
buildAssignment   _       _                         Nil                Nil = return (Ctx.empty, Ctx.empty)
buildAssignment sym newVars ((BoundVar var) :> varsRest) (val :> valsRest) = do
  val' <- operandValue (undefined :: arch) sym newVars val
  (varsRest', valsRest') <- buildAssignment sym newVars varsRest valsRest
  return (Ctx.extend varsRest' var, Ctx.extend valsRest' val')

replaceOpVars :: forall arch t st sh tp.
                 (Architecture arch)
              => S.SimpleBuilder t st
              -- ^ Symbolic expression builder
              -> (forall tp'. Location arch tp' -> IO (S.SimpleBoundVar t tp'))
              -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
              -> OperandList (BoundVar (S.SimpleBuilder t st) arch) sh
              -- ^ List of variables corresponding to each operand
              -> OperandList (Operand arch) sh
              -- ^ List of operand values corresponding to each operand
              -> S.Elt t tp
              -- ^ Expression to do the replace in
              -> IO (S.Elt t tp)
replaceOpVars sym newVars vars vals expr =
  buildAssignment sym newVars vars vals >>= uncurry (S.evalBoundVars sym expr)

replaceLitVars :: forall (loc :: BaseType -> *) t st tp.
                  (OrdF loc)
               => S.SimpleBuilder t st
               -> (forall tp'. loc tp' -> IO (S.SimpleBoundVar t tp'))
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> S.Elt t tp
               -> IO (S.Elt t tp)
replaceLitVars sym newVars oldVars expr0 = foldlMWithKey f expr0 oldVars
  where f :: forall tp'. S.Elt t tp -> loc tp' -> S.SimpleBoundVar t tp' -> IO (S.Elt t tp)
        f expr k oldVar = do
          newExpr <- S.varExpr sym <$> newVars k
          S.evalBoundVars sym expr (Ctx.extend Ctx.empty oldVar) (Ctx.extend Ctx.empty newExpr)

mapFMapMBoth :: forall k1 v1 k2 v2 m. (OrdF k2, Monad m) => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp)) -> MapF.MapF k1 v1 -> m (MapF.MapF k2 v2)
mapFMapMBoth f = MapF.foldrWithKey f' (return MapF.empty)
  where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
        f' k v wrappedM = do
          (k', v') <- f k v
          m <- wrappedM
          return $ MapF.insert k' v' m

paramToLocation :: forall arch sh tp. (Architecture arch) => OperandList (Operand arch) sh -> Parameter arch sh tp -> Maybe (Location arch tp)
paramToLocation opVals (Operand _ idx) = operandToLocation (undefined :: arch) $ indexOpList opVals idx
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

-- XXX: Location: change to Location

instantiateFormula :: forall arch t st sh.
                      (Architecture arch)
                   => S.SimpleBuilder t st
                   -> ParameterizedFormula (S.SimpleBuilder t st) arch sh
                   -> OperandList (Operand arch) sh
                   -> IO (Formula (S.SimpleBuilder t st) arch)
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

    let mapDef :: forall tp. Parameter arch sh tp -> S.Elt t tp -> IO (Location arch tp, S.Elt t tp)
        mapDef p e = case paramToLocation opVals p of
          Just loc -> (loc,) <$> (replaceLitVars sym newLitVarLookup litVars =<< replaceOpVars sym newLitVarLookup opVars opVals e)
          Nothing -> error "XXX: handle this error case more gracefully"

    newDefs <- mapFMapMBoth mapDef defs
    -- ^ This loads the relevant lit vars into the map, so reading the IORef
    -- must happen afterwards :)
    newLitVars <- readIORef newLitVarsRef

    let mapParam (Some param) = maybe Set.empty (Set.singleton . Some) $ paramToLocation opVals param
        newUses = foldMap mapParam uses

    return $ Formula { formUses = newUses
                     , formParamVars = newLitVars
                     , formDefs = newDefs
                     }
