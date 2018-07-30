{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SemMC.Stochastic.Extract (
  extractFormula
  ) where

import qualified GHC.Err.Located as L

import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC ( fmapFC, foldrFC, traverseFC )
import qualified Data.Parameterized.List as SL
import qualified What4.Interface as C
import qualified What4.Expr.Builder as SB

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.View as V
import qualified SemMC.BoundVar as BV
import qualified SemMC.Formula as F
import qualified SemMC.Log as L
import           SemMC.Symbolic ( Sym )
import qualified SemMC.Util as U

import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.IORelation ( IORelation(..), OperandRef(..) )

-- | Given a formula derived from a learned program (@progForm@), extract a
-- 'F.ParameterizedFormula' that has holes for the given 'Opcode'.
extractFormula :: forall arch t solver sh
                . (SynC arch)
               => AC.RegisterizedInstruction arch
               -> A.Opcode arch (A.Operand arch) sh
               -> SL.List (A.Operand arch) sh
               -> F.Formula (Sym t solver) arch
               -> IORelation arch sh
               -> Syn t solver arch (F.ParameterizedFormula (Sym t solver) arch sh)
extractFormula ri opc ops progForm iorel = do
  L.logM L.Info ("Extracting formula from " ++ show progForm)
  go (MapF.empty, MapF.empty) (F.toList (outputs iorel))
  where
    go (locDefs, locVars) [] = do
      extractedFormula <- makeFreshFormula locDefs locVars
      L.logM L.Info ("Fresh formula is " ++ show extractedFormula)
      pf0 <- parameterizeFormula ri opc ops extractedFormula
      renameVariables ops pf0
    go acc (out:rest) =
      case out of
        ImplicitOperand (Some (V.View _ loc)) ->
          let Just expr = MapF.lookup loc (F.formDefs progForm)
          in go (defineLocation progForm loc expr acc) rest
        OperandRef (Some idx) -> do
          let operand = ops SL.!! idx
              Just loc = A.operandToLocation (Proxy @arch) operand
              Just expr = MapF.lookup loc (F.formDefs progForm)
          go (defineLocation progForm loc expr acc) rest

-- | Take a pass through the formula to rename variables standing in for
-- parameters into friendlier names.
--
-- The names we get out of 'parameterizeFormula' are named after the concrete
-- locations that were used in the candidate program discovered by the
-- stochastic synthesis.  This makes it difficult to visually distinguish
-- between parameter values and implicit parameters.
--
-- We only need to replace the bound vars in 'pfOperandVars' and then use the
-- replaced list with 'S.replaceVars' to update 'pfDefs'.
renameVariables :: forall arch sh t solver
                 . (SynC arch)
                => SL.List (A.Operand arch) sh
                -> F.ParameterizedFormula (Sym t solver) arch sh
                -> Syn t solver arch (F.ParameterizedFormula (Sym t solver) arch sh)
renameVariables oplist pf0 = do
  withSymBackend $ \sym -> do
    (sva, opVars') <- liftIO (buildRenameList sym (F.pfOperandVars pf0))
    defs' <- liftIO (MapF.traverseWithKey (doReplace sym sva) (F.pfDefs pf0))
    return pf0 { F.pfOperandVars = opVars'
               , F.pfDefs = defs'
               }
  where
    doReplace sym sva _k v = F.replaceVars sym sva v

    buildRenameList :: Sym t solver
                    -> SL.List (BV.BoundVar (Sym t solver) arch) sh
                    -> IO (Pair (Ctx.Assignment (C.BoundVar (Sym t solver))) (Ctx.Assignment (C.SymExpr (Sym t solver))), SL.List (BV.BoundVar (Sym t solver) arch) sh)
    buildRenameList sym opVarList = do
      -- A shaped list with the same shape, but with pairs where the first is
      -- the original name and the second is the newly-allocated variable (with
      -- a more sensible name) that will be used as a replacement.
      opVarListRenames <- SL.itraverse (allocateSensibleVariableName sym) opVarList
      varExprPairs <- traverseFC convertToVarExprPair opVarListRenames
      let sva = foldrFC addToAssignmentPair (Pair Ctx.empty Ctx.empty) varExprPairs
      return (sva, fmapFC secondVar opVarListRenames)

    allocateSensibleVariableName :: forall tp
                                  . Sym t solver
                                 -> SL.Index sh tp
                                 -> BV.BoundVar (Sym t solver) arch tp
                                 -> IO (VarPair (BV.BoundVar (Sym t solver) arch) (BV.BoundVar (Sym t solver) arch) tp)
    allocateSensibleVariableName sym ix bv = do
      let operand = oplist SL.!! ix
      fresh <- C.freshBoundVar sym (U.makeSymbol ("operand" ++ show (SL.indexValue ix))) (AC.operandType (Proxy @arch) operand)
      return (VarPair bv (BV.BoundVar fresh))

    -- Given a correspondence between and old var and a new var, create a pair
    -- mapping the old pair to a new expr (which is just the new var wrapped
    -- into a SymExpr)
    convertToVarExprPair :: forall tp
                          . VarPair (BV.BoundVar (Sym t solver) arch) (BV.BoundVar (Sym t solver) arch) tp
                         -> IO (VarPair (BV.BoundVar (Sym t solver) arch) (EltWrapper t arch) tp)
    convertToVarExprPair (VarPair oldVar (BV.BoundVar newVar)) =
      return $ VarPair oldVar (EltWrapper (SB.BoundVarExpr newVar))

    addToAssignmentPair (VarPair (BV.BoundVar v) (EltWrapper e)) (Pair vars exprs) =
      Pair (Ctx.extend vars v) (Ctx.extend exprs e)

newtype EltWrapper sym arch op = EltWrapper (SB.Expr sym (A.OperandType arch op))
data VarPair a b tp = VarPair (a tp) (b tp)

secondVar :: VarPair a b tp -> b tp
secondVar (VarPair _ v) = v

-- | Given the components of a formula, allocate a set of fresh variables for
-- each location and create a new formula.
--
-- The intermediate f0 is invalid, but we use 'F.copyFormula' to create a fresh
-- set of bound variables that are unique.
makeFreshFormula :: (AC.ConcreteArchitecture arch)
                 => MapF.MapF (A.Location arch) (C.SymExpr (Sym t solver))
                 -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t solver))
                 -> Syn t solver arch (F.Formula (Sym t solver) arch)
makeFreshFormula exprs vars = withSymBackend $ \sym -> do
  liftIO $ F.copyFormula sym F.Formula { F.formParamVars = vars
                                       , F.formDefs = exprs
                                       }

-- | Collect the locations that we need to define this formula; we collect into
-- a 'MapF.MapF' instead of a 'F.Formula' so that we don't have a very invalid
-- formula lying around.
defineLocation :: forall t solver arch tp
                . (AC.ConcreteArchitecture arch)
               => F.Formula (Sym t solver) arch
               -> A.Location arch tp
               -> SB.Expr t tp
               -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t solver)), MapF.MapF (A.Location arch) (C.BoundVar (Sym t solver)))
               -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t solver)), MapF.MapF (A.Location arch) (C.BoundVar (Sym t solver)))
defineLocation frm loc expr (defs, vars) =
  (MapF.insert loc expr defs, collectVars frm expr vars)

collectVars :: forall t solver tp arch
             . (C.TestEquality (C.BoundVar (Sym t solver)), P.OrdF (A.Location arch))
            => F.Formula (Sym t solver) arch
            -> SB.Expr t tp
            -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t solver))
            -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t solver))
collectVars frm expr m = MapF.union m neededVars
  where neededVars = U.extractUsedLocs (F.formParamVars frm) expr

-- | Based on the iorelation, identify the inputs of the opcode (and the
-- corresponding operands).  For each input operand backed by a location, create
-- a boundvar and substitute them for the corresponding expressions in formulas.
--
-- For operands not backed by a location, we expect a corresponding index in the
-- 'AC.RegisterizedInstruction', which will tell us the mapping to another
-- location that represents the immediate.  We can use the variable representing
-- that location as the hole for the immediate.
parameterizeFormula :: forall arch sh t solver
                     . (AC.ConcreteArchitecture arch)
                    => AC.RegisterizedInstruction arch
                    -> A.Opcode arch (A.Operand arch) sh
                    -> SL.List (A.Operand arch) sh
                    -> F.Formula (Sym t solver) arch
                    -> Syn t solver arch (F.ParameterizedFormula (Sym t solver) arch sh)
parameterizeFormula ri@AC.RI { AC.riLiteralLocs = regLitLocs } opcode oplist f = do
  -- The pfLiteralVars are the parameters from the original formula not
  -- corresponding to any parameters
  let litVars = U.filterMapF (keepNonParams paramLocs) (F.formParamVars f)
  L.logM L.Info ("litVars = " ++ show litVars)
  let (nonParamDefs, paramDefs) = SL.ifoldr (replaceParameters (Proxy @arch)) (F.formDefs f, MapF.empty) oplist
      defs = MapF.foldrWithKey liftImplicitLocations paramDefs nonParamDefs
  L.logM L.Info ("nonParamDefs = " ++ show nonParamDefs)
  L.logM L.Info ("paramDefs = " ++ show paramDefs)
  L.logM L.Info ("defs = " ++ show defs)
  -- After we have extracted all of the necessary definitions of parameters, we
  -- have all of the formulas we actually care about.  Next, we need to identify
  -- all of the variables in those formulas corresponding to explicit operands
  -- (pfOperandVars) and implicit operands (pfLiteralVars)
  --
  -- FIXME: Do we need to allocate fresh vars when we do that?  Probably not,
  -- since we copied 'f' when we created it.
  --
  -- FIXME: Do we really need variables for output-only locations?  It seems to
  -- produce formulas that reference useless variables
  opVars <- SL.itraverse (findVarForOperand opcode ri (F.formParamVars f)) oplist
  L.logM L.Info ("opVars = " ++ show opVars)

  -- Uses are:
  --
  -- 1) All of the keys of 'pfDefs' (used as in defined here)
  --
  -- 2) The locations in 'pfLiteralVars'
  --
  -- 3) The typed indices of 'pfOperandVars'
  let uses = collectUses oplist litVars defs
  return F.ParameterizedFormula { F.pfUses = uses
                                , F.pfOperandVars = opVars
                                , F.pfLiteralVars = litVars
                                , F.pfDefs = defs
                                }
  where
    -- Param locs needs to include registerization locations
    naturalParamLocs = foldrFC (collectParamLocs (Proxy @arch)) S.empty oplist
    registerizedParamLocs = S.fromList (MapF.elems regLitLocs)
    paramLocs = naturalParamLocs `S.union` registerizedParamLocs

collectUses :: forall arch sh a b
             . (AC.ConcreteArchitecture arch)
            => SL.List (A.Operand arch) sh
            -> MapF.MapF (A.Location arch) a
            -> MapF.MapF (F.Parameter arch sh) b
            -> S.Set (Some (F.Parameter arch sh))
collectUses opVars litVars defs =
  S.unions [defUses, litUses, opUses]
  where
    defUses = MapF.foldrWithKey collectKeys S.empty defs
    litUses = MapF.foldrWithKey wrapLocations S.empty litVars
    opUses = SL.ifoldr wrapIndexes S.empty opVars

    collectKeys k _ = S.insert (Some k)
    wrapLocations loc _ = S.insert (Some (F.LiteralParameter loc))
    wrapIndexes :: forall tp . SL.Index sh tp -> A.Operand arch tp -> S.Set (Some (F.Parameter arch sh)) -> S.Set (Some (F.Parameter arch sh))
    wrapIndexes ix op = S.insert (Some (F.OperandParameter (AC.operandType (Proxy @arch) op) ix))

-- | Given an operand, find the 'BV.BoundVar' the represents it.
--
-- For operands with 'A.Location's (e.g., registers), this can be found in the
-- given 'MapF.MapF'.  For operands without 'A.Location's (i.e., immediates), we
-- have to look up the corresponding mapping in the 'AC.RegisterizedInstruction'
-- that records which 'A.Location' is standing in for the immediate.
findVarForOperand :: forall arch sh tp t solver
                   . (AC.ConcreteArchitecture arch)
                  => A.Opcode arch (A.Operand arch) sh
                  -> AC.RegisterizedInstruction arch
                  -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t solver))
                  -> SL.Index sh tp
                  -> A.Operand arch tp
                  -> Syn t solver arch (BV.BoundVar (Sym t solver) arch tp)
findVarForOperand opc (AC.RI { AC.riLiteralLocs = lls, AC.riOpcode = rop }) formulaBindings ix op
  | Just P.Refl <- P.testEquality rop opc =
    case A.operandToLocation (Proxy @arch) op of
      Just loc ->
        case MapF.lookup loc formulaBindings of
          Nothing -> do
            -- If we didn't find a variable for this binding, it is an output
            -- operand that is not used as an input at all.  That is fine, but
            -- we need to allocate a fresh variable here for use in the
            -- parameterized formula.
            withSymBackend $ \sym -> do
              BV.BoundVar <$> liftIO (C.freshBoundVar sym (U.makeSymbol (P.showF loc)) (A.locationType loc))
          Just bv -> return (BV.BoundVar bv)
      Nothing -> do
        -- In this case, we are dealing with an immediate operand (since it
        -- doesn't have a location representation).  We can find the corresponding
        -- location in the RegisterizedInstruction.
        case MapF.lookup (AC.LiteralRef ix) lls of
          Just loc ->
            case MapF.lookup loc formulaBindings of
              Just bv -> return (BV.BoundVar bv)
              Nothing -> L.error ("Expected a binding for location " ++ P.showF loc)
          Nothing -> L.error ("Expected a location mapping for operand at index " ++ show ix)
  | otherwise = L.error ("Unexpected opcode mismatch: " ++ P.showF opc ++ " vs " ++ P.showF rop)

liftImplicitLocations :: (P.OrdF (A.Location arch))
                      => A.Location arch tp
                      -> C.SymExpr (Sym t solver) tp
                      -> MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t solver))
                      -> MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t solver))
liftImplicitLocations loc expr = MapF.insert (F.LiteralParameter loc) expr

keepNonParams :: (P.OrdF f) => S.Set (Some f) -> f tp -> g tp -> Bool
keepNonParams paramLocs loc _ = S.notMember (Some loc) paramLocs

collectParamLocs :: (AC.ConcreteArchitecture arch)
                 => proxy arch
                 -> A.Operand arch tp
                 -> S.Set (Some (A.Location arch))
                 -> S.Set (Some (A.Location arch))
collectParamLocs proxy op s =
  case AC.operandToSemanticView proxy op of
    Nothing -> s
    Just (V.SemanticView { V.semvView = V.View _ loc }) -> S.insert (Some loc) s


-- | For locations referenced in an operand list *and* defined by a formula (the
-- definitions of the formula are the first element of the pair), add them to a
-- new map (suitable for a ParameterizedFormula).
--
-- This just involves wrapping the location with an 'F.Operand' constructor.
--
-- We remove the promoted locations from the original definition map, since we
-- need to know which locations are left (they are handled by
-- 'liftImplicitLocations').
--
-- FIXME: Do we need to pass in the IORelation so that we are sure we are only
-- defining output registers?  Or is that not a concern because we already used
-- IORelations to limit the formula we are generalizing.
replaceParameters :: (AC.ConcreteArchitecture arch)
                  => proxy arch
                  -> SL.Index sh tp
                  -> A.Operand arch tp
                  -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t solver)), MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t solver)))
                  -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t solver)), MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t solver)))
replaceParameters proxy ix op (defs, m) =
  case A.operandToLocation proxy op of
    Nothing -> (defs, m)
    Just loc ->
      case MapF.lookup loc defs of
        Nothing ->
          -- This lookup would fail on a purely input operand that had no
          -- definition in the final formula
          (defs, m)
        Just expr -> (MapF.delete loc defs, MapF.insert (F.OperandParameter (A.locationType loc) ix) expr m)
