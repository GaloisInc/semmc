{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module SemMC.Instantiate where

import Data.Maybe ( fromJust )
import Data.Parameterized.Classes
import Data.Parameterized.Some
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import qualified Data.Set as Set

import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import Lang.Crucible.Solver.Symbol ( SolverSymbol, userSymbol )
import Lang.Crucible.BaseTypes

import Dismantle.Instruction ( OperandList(..) )

import SemMC.Formula
import SemMC.Architecture

makeSymbol :: String -> SolverSymbol
makeSymbol name = case userSymbol name of
                    Right symbol -> symbol
                    Left _ -> error "tried to create symbol with bad name"

foldlMWithKey :: forall k a b m. (Monad m) => (forall s. b -> k s -> a s -> m b) -> b -> MapF.MapF k a -> m b
foldlMWithKey f z0 m = MapF.foldrWithKey f' return m z0
  where f' :: forall s. k s -> a s -> (b -> m b) -> b -> m b
        f' k x c z = f z k x >>= c
    -- go z' Tip = z'
    -- go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l

replaceOpVars :: forall arch t st sh tp.
                 (Architecture arch)
              => S.SimpleBuilder t st
              -- ^ Symbolic expression builder
              -> MapF.MapF (StateVar arch) (S.SimpleBoundVar t)
              -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
              -> OperandList (BoundVar (S.SimpleBuilder t st) arch) sh
              -- ^ List of variables corresponding to each operand
              -> OperandList (Operand arch) sh
              -- ^ List of operand values corresponding to each operand
              -> S.Elt t tp
              -- ^ Expression to do the replace in
              -> IO (S.Elt t tp)
replaceOpVars   _ _                          Nil               Nil expr = return expr
replaceOpVars sym m ((BoundVar var) :> varsRest) (val :> valsRest) expr = do
  -- Would it be faster to do this all in one replace? Probably, but for now we
  -- do it like this.
  val' <- operandValue (undefined :: arch) sym m val
  replaced <- S.evalBoundVars sym expr (Ctx.extend Ctx.empty var) (Ctx.extend Ctx.empty val')
  replaceOpVars sym m varsRest valsRest replaced

replaceLitVars :: forall (svar :: BaseType -> *) t st tp.
                  (OrdF svar)
               => S.SimpleBuilder t st
               -> MapF.MapF svar (S.SimpleBoundVar t)
               -> MapF.MapF svar (S.SimpleBoundVar t)
               -> S.Elt t tp
               -> IO (S.Elt t tp)
replaceLitVars sym newVars oldVars expr0 = foldlMWithKey f expr0 oldVars
  where f :: forall tp'. S.Elt t tp -> svar tp' -> S.SimpleBoundVar t tp' -> IO (S.Elt t tp)
        f expr k oldVar = let newExpr = S.varExpr sym . fromJust $ MapF.lookup k newVars
                          in S.evalBoundVars sym expr (Ctx.extend Ctx.empty oldVar) (Ctx.extend Ctx.empty newExpr)

mapFMapMBoth :: forall k1 v1 k2 v2 m. (OrdF k2, Monad m) => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp)) -> MapF.MapF k1 v1 -> m (MapF.MapF k2 v2)
mapFMapMBoth f = MapF.foldrWithKey f' (return MapF.empty)
  where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
        f' k v wrappedM = do
          (k', v') <- f k v
          m <- wrappedM
          return $ MapF.insert k' v' m

paramToStateVar :: forall arch sh tp. (Architecture arch) => OperandList (Operand arch) sh -> Parameter arch sh tp -> Maybe (StateVar arch tp)
paramToStateVar opVals (Operand _ idx) = operandToStateVar (undefined :: arch) $ indexOpList opVals idx
paramToStateVar _ (Literal _ var) = Just var

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
    -- First, make variables. For now, create variables for *all* possible state
    -- parts. We can change this later, but for now, it's simple.
    -- newLitVarPairs <- mapM (\v -> Pair v <$> S.freshBoundVar sym (makeSymbol (showF v)) (A.stateVarType v))
    --                        [Reg'1, Reg'2, Reg'3]
    newLitVars <- MapF.fromKeysM (\v -> S.freshBoundVar sym (makeSymbol (showF v)) (stateVarType v))
                                 allStateVars

    let mapDef :: forall tp. Parameter arch sh tp -> S.Elt t tp -> IO (StateVar arch tp, S.Elt t tp)
        mapDef p e = case paramToStateVar opVals p of
          Just var -> (var,) <$> (replaceLitVars sym newLitVars litVars =<< replaceOpVars sym newLitVars opVars opVals e)
          Nothing -> error "XXX: handle this error case more gracefully"
    -- opVarsReplaced <- MapF.map (replaceOpVars newLitVars opVars opVals)
    newDefs <- mapFMapMBoth mapDef defs

    let mapParam (Some param) = maybe Set.empty (Set.singleton . Some) $ paramToStateVar opVals param
        newUses = foldMap mapParam uses

    return $ Formula { formUses = newUses
                     , formParamVars = newLitVars
                     , formDefs = newDefs
                     }
