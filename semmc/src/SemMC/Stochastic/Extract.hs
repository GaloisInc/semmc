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
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC ( foldrFC )
import qualified Data.Parameterized.ShapedList as SL
import qualified Lang.Crucible.Solver.Interface as C
import qualified Lang.Crucible.Solver.SimpleBuilder as SB

import qualified SemMC.Architecture as A
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym )
import qualified SemMC.Util as U

import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.IORelation ( IORelation(..), OperandRef(..) )

-- | Given a formula derived from a learned program (@progForm@), extract a
-- 'F.ParameterizedFormula' that has holes for the given 'Opcode'.
extractFormula :: forall arch t sh
                . (CS.ConcreteArchitecture arch, SynC arch)
               => CS.RegisterizedInstruction arch
               -> A.Opcode arch (A.Operand arch) sh
               -> SL.ShapedList (A.Operand arch) sh
               -> F.Formula (Sym t) arch
               -> IORelation arch sh
               -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
extractFormula ri opc ops progForm iorel = go (MapF.empty, MapF.empty) (F.toList (outputs iorel))
  where
    go (locDefs, locVars) [] = do
      extractedFormula <- makeFreshFormula locDefs locVars
      parameterizeFormula ri opc ops extractedFormula
    go acc (out:rest) =
      case out of
        ImplicitOperand (Some (CS.View _ loc)) ->
          let Just expr = MapF.lookup loc (F.formDefs progForm)
          in go (defineLocation progForm loc expr acc) rest
        OperandRef (Some idx) -> do
          let operand = SL.indexShapedList ops idx
              Just loc = A.operandToLocation (Proxy @arch) operand
              Just expr = MapF.lookup loc (F.formDefs progForm)
          go (defineLocation progForm loc expr acc) rest

-- | Given the components of a formula, allocate a set of fresh variables for
-- each location and create a new formula.
--
-- The intermediate f0 is invalid, but we use 'F.copyFormula' to create a fresh
-- set of bound variables that are unique.
makeFreshFormula :: (CS.ConcreteArchitecture arch)
                 => MapF.MapF (A.Location arch) (C.SymExpr (Sym t))
                 -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t))
                 -> Syn t arch (F.Formula (Sym t) arch)
makeFreshFormula exprs vars = withSymBackend $ \sym -> do
  liftIO $ F.copyFormula sym F.Formula { F.formParamVars = vars
                                       , F.formDefs = exprs
                                       }

-- | Collect the locations that we need to define this formula; we collect into
-- a 'MapF.MapF' instead of a 'F.Formula' so that we don't have a very invalid
-- formula lying around.
defineLocation :: forall t arch tp
                . (CS.ConcreteArchitecture arch)
               => F.Formula (Sym t) arch
               -> A.Location arch tp
               -> SB.Elt t tp
               -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t)), MapF.MapF (A.Location arch) (C.BoundVar (Sym t)))
               -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t)), MapF.MapF (A.Location arch) (C.BoundVar (Sym t)))
defineLocation frm loc expr (defs, vars) =
  (MapF.insert loc expr defs, collectVars frm expr vars)

collectVars :: forall t tp arch
             . (C.TestEquality (C.BoundVar (Sym t)), P.OrdF (A.Location arch))
            => F.Formula (Sym t) arch
            -> SB.Elt t tp
            -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t))
            -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t))
collectVars frm expr m = MapF.union m neededVars
  where neededVars = U.extractUsedLocs (F.formParamVars frm) expr

-- | Based on the iorelation, identify the inputs of the opcode (and the
-- corresponding operands).  For each input operand backed by a location, create
-- a boundvar and substitute them for the corresponding expressions in formulas.
--
-- For operands not backed by a location, we expect a corresponding index in the
-- 'CS.RegisterizedInstruction', which will tell us the mapping to another
-- location that represents the immediate.  We can use the variable representing
-- that location as the hole for the immediate.
parameterizeFormula :: forall arch sh t
                     . (CS.ConcreteArchitecture arch)
                    => CS.RegisterizedInstruction arch
                    -> A.Opcode arch (A.Operand arch) sh
                    -> SL.ShapedList (A.Operand arch) sh
                    -> F.Formula (Sym t) arch
                    -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
parameterizeFormula ri opcode oplist f = do
  -- The pfLiteralVars are the parameters from the original formula not
  -- corresponding to any parameters
  let litVars = U.filterMapF (keepNonParams paramLocs) (F.formParamVars f)
  let (nonParamDefs, paramDefs) = SL.foldrFCIndexed (replaceParameters (Proxy @arch)) (F.formDefs f, MapF.empty) oplist
      defs = MapF.foldrWithKey liftImplicitLocations paramDefs nonParamDefs

  -- After we have extracted all of the necessary definitions of parameters, we
  -- have all of the formulas we actually care about.  Next, we need to identify
  -- all of the variables in those formulas corresponding to explicit operands
  -- (pfOperandVars) and implicit operands (pfLiteralVars)
  --
  -- FIXME: Do we need to allocate fresh vars when we do that?  Probably not,
  -- since we copied 'f' when we created it.
  opVars <- SL.traverseFCIndexed (findVarForOperand opcode ri (F.formParamVars f)) oplist

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
    paramLocs = foldrFC (collectParamLocs (Proxy @arch)) S.empty oplist

collectUses :: forall arch sh a b
             . (CS.ConcreteArchitecture arch)
            => SL.ShapedList (A.Operand arch) sh
            -> MapF.MapF (A.Location arch) a
            -> MapF.MapF (F.Parameter arch sh) b
            -> S.Set (Some (F.Parameter arch sh))
collectUses opVars litVars defs =
  S.unions [defUses, litUses, opUses]
  where
    defUses = MapF.foldrWithKey collectKeys S.empty defs
    litUses = MapF.foldrWithKey wrapLocations S.empty litVars
    opUses = SL.foldrFCIndexed wrapIndexes S.empty opVars

    collectKeys k _ = S.insert (Some k)
    wrapLocations loc _ = S.insert (Some (F.Literal loc))
    wrapIndexes :: forall tp . SL.Index sh tp -> A.Operand arch tp -> S.Set (Some (F.Parameter arch sh)) -> S.Set (Some (F.Parameter arch sh))
    wrapIndexes ix op = S.insert (Some (F.Operand (CS.operandType (Proxy @arch) op) ix))

-- | Given an operand, find the 'A.BoundVar' the represents it.
--
-- For operands with 'A.Location's (e.g., registers), this can be found in the
-- given 'MapF.MapF'.  For operands without 'A.Location's (i.e., immediates), we
-- have to look up the corresponding mapping in the 'CS.RegisterizedInstruction'
-- that records which 'A.Location' is standing in for the immediate.
findVarForOperand :: forall arch sh tp t
                   . (CS.ConcreteArchitecture arch)
                  => A.Opcode arch (A.Operand arch) sh
                  -> CS.RegisterizedInstruction arch
                  -> MapF.MapF (A.Location arch) (C.BoundVar (Sym t))
                  -> SL.Index sh tp
                  -> A.Operand arch tp
                  -> Syn t arch (A.BoundVar (Sym t) arch tp)
findVarForOperand opc (CS.RI { CS.riLiteralLocs = lls, CS.riOpcode = rop }) formulaBindings ix op
  | Just P.Refl <- P.testEquality rop opc =
    case A.operandToLocation (Proxy @arch) op of
      Just loc ->
        case MapF.lookup loc formulaBindings of
          Nothing -> L.error ("Expected binding for location " ++ P.showF loc)
          Just bv -> return (A.BoundVar bv)
      Nothing -> do
        -- In this case, we are dealing with an immediate operand (since it
        -- doesn't have a location representation).  We can find the corresponding
        -- location in the RegisterizedInstruction.
        case MapF.lookup (CS.LiteralRef ix) lls of
          Just loc ->
            case MapF.lookup loc formulaBindings of
              Just bv -> return (A.BoundVar bv)
              Nothing -> L.error ("Expected a binding for location " ++ P.showF loc)
          Nothing -> L.error ("Expected a location mapping for operand at index " ++ show ix)
  | otherwise = L.error ("Unexpected opcode mismatch: " ++ P.showF opc ++ " vs " ++ P.showF rop)

liftImplicitLocations :: (P.OrdF (A.Location arch))
                      => A.Location arch tp
                      -> C.SymExpr (Sym t) tp
                      -> MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t))
                      -> MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t))
liftImplicitLocations loc expr = MapF.insert (F.Literal loc) expr

keepNonParams :: (P.OrdF f) => S.Set (Some f) -> f tp -> g tp -> Bool
keepNonParams paramLocs loc _ = S.notMember (Some loc) paramLocs

collectParamLocs :: (CS.ConcreteArchitecture arch)
                 => proxy arch
                 -> A.Operand arch tp
                 -> S.Set (Some (A.Location arch))
                 -> S.Set (Some (A.Location arch))
collectParamLocs proxy op s =
  case A.operandToLocation proxy op of
    Nothing -> s
    Just loc -> S.insert (Some loc) s


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
replaceParameters :: (CS.ConcreteArchitecture arch)
                  => proxy arch
                  -> SL.Index sh tp
                  -> A.Operand arch tp
                  -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t)), MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t)))
                  -> (MapF.MapF (A.Location arch) (C.SymExpr (Sym t)), MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t)))
replaceParameters proxy ix op (defs, m) =
  case A.operandToLocation proxy op of
    Nothing -> (defs, m)
    Just loc ->
      case MapF.lookup loc defs of
        Nothing ->
          -- This lookup would fail on a purely input operand that had no
          -- definition in the final formula
          (defs, m)
        Just expr -> (MapF.delete loc defs, MapF.insert (F.Operand (A.locationType loc) ix) expr m)
