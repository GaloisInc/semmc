{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SemMC.Stochastic.Extract (
  extractFormula
  ) where

import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC ( foldrFC, traverseFC )
import qualified Data.Parameterized.ShapedList as SL
import qualified Lang.Crucible.Solver.Interface as C
import qualified Lang.Crucible.Solver.SimpleBuilder as SB

import           Data.Parameterized.ShapedList ( ShapedList, indexShapedList )

import           SemMC.Architecture ( Opcode, Operand, Location, BoundVar, operandToLocation, locationType )
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Instantiate as F
import           SemMC.Symbolic ( Sym )
import           SemMC.Util ( extractUsedLocs, filterMapF )

import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.IORelation ( IORelation(..), OperandRef(..) )

-- | Given a formula derived from a learned program (@progForm@), extract a
-- 'F.ParameterizedFormula' that has holes for the given 'Opcode'.
extractFormula :: forall arch t sh
                . (CS.ConcreteArchitecture arch, SynC arch)
               => RegisterizedInstruction arch
               -> Opcode arch (Operand arch) sh
               -> ShapedList (Operand arch) sh
               -> F.Formula (Sym t) arch
               -> IORelation arch sh
               -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
extractFormula ri opc ops progForm iorel = go (MapF.empty, MapF.empty) (F.toList (outputs iorel))
  where
    go (locDefs, locVars) [] = do
      extractedFormula <- makeFreshFormula locDefs locVars
      parameterizeFormula opc ops iorel extractedFormula
    go acc (out:rest) =
      case out of
        ImplicitOperand (Some (CS.View _ loc)) ->
          let Just expr = MapF.lookup loc (F.formDefs progForm)
          in go (defineLocation progForm loc expr acc) rest
        OperandRef (Some idx) -> do
          let operand = indexShapedList ops idx
              Just loc = operandToLocation (Proxy @arch) operand
              Just expr = MapF.lookup loc (F.formDefs progForm)
          go (defineLocation progForm loc expr acc) rest

-- | Given the components of a formula, allocate a set of fresh variables for
-- each location and create a new formula.
--
-- The intermediate f0 is invalid, but we use 'F.copyFormula' to create a fresh
-- set of bound variables that are unique.
makeFreshFormula :: (CS.ConcreteArchitecture arch)
                 => MapF.MapF (Location arch) (C.SymExpr (Sym t))
                 -> MapF.MapF (Location arch) (C.BoundVar (Sym t))
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
               -> Location arch tp
               -> SB.Elt t tp
               -> (MapF.MapF (Location arch) (C.SymExpr (Sym t)), MapF.MapF (Location arch) (C.BoundVar (Sym t)))
               -> (MapF.MapF (Location arch) (C.SymExpr (Sym t)), MapF.MapF (Location arch) (C.BoundVar (Sym t)))
defineLocation frm loc expr (defs, vars) =
  (MapF.insert loc expr defs, collectVars frm expr vars)

collectVars :: forall t tp arch
             . (C.TestEquality (C.BoundVar (Sym t)), P.OrdF (Location arch))
            => F.Formula (Sym t) arch
            -> SB.Elt t tp
            -> MapF.MapF (Location arch) (C.BoundVar (Sym t))
            -> MapF.MapF (Location arch) (C.BoundVar (Sym t))
collectVars frm expr m = MapF.union m neededVars
  where neededVars = extractUsedLocs (F.formParamVars frm) expr

-- | Based on the iorelation, identify the inputs of the opcode (and the
-- corresponding operands).  For each input operand backed by a location, create
-- a boundvar and substitute them for the corresponding expressions in formulas.
--
-- This actually might not be so bad.  The list of operands makes it easy to get
-- a list of locations that need to be extracted.  We can then just do a bit of
-- rewriting...
parameterizeFormula :: forall arch sh t
                     . (CS.ConcreteArchitecture arch)
                    => Opcode arch (Operand arch) sh
                    -> ShapedList (Operand arch) sh
                    -> IORelation arch sh
                    -> F.Formula (Sym t) arch
                    -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
parameterizeFormula opcode oplist iorel f = do
  uses <- undefined

  -- We need to assign bound variables to each operand in the operand list
  -- (wrapped in the Parameter type).  For operands corresponding to registers,
  -- that is fairly easy.  Just convert the operand to a location and look it up
  -- in the formParamVars map.  Operands corresponding to literals are a bigger
  -- question...
  opVars <- traverseFC allocateBoundVar oplist

  -- The pfLiteralVars are the parameters from the original formula not
  -- corresponding to any parameters
  let litVars = filterMapF (keepNonParams paramLocs) (F.formParamVars f)
  let (nonParamDefs, paramDefs) = SL.foldrFCIndexed (replaceParameters (Proxy @arch)) (F.formDefs f, MapF.empty) oplist
      defs = MapF.foldrWithKey liftImplicitLocations paramDefs nonParamDefs
  return F.ParameterizedFormula { F.pfUses = uses
                                , F.pfOperandVars = opVars
                                , F.pfLiteralVars = litVars
                                , F.pfDefs = defs
                                }
  where
    paramLocs = foldrFC (collectParamLocs (Proxy @arch)) S.empty oplist
    usedLocs = map (\(Some e) -> extractUsedLocs (F.formParamVars f) e) (MapF.elems (F.formDefs f))

liftImplicitLocations :: (P.OrdF (Location arch))
                      => Location arch tp
                      -> C.SymExpr (Sym t) tp
                      -> MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t))
                      -> MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t))
liftImplicitLocations loc expr = MapF.insert (F.Literal loc) expr

keepNonParams :: (P.OrdF f) => S.Set (Some f) -> f tp -> g tp -> Bool
keepNonParams paramLocs loc _ = S.member (Some loc) paramLocs

collectParamLocs :: (CS.ConcreteArchitecture arch)
                 => proxy arch
                 -> Operand arch tp
                 -> S.Set (Some (Location arch))
                 -> S.Set (Some (Location arch))
collectParamLocs proxy op s =
  case operandToLocation proxy op of
    Nothing -> s
    Just loc -> S.insert (Some loc) s

allocateBoundVar :: Operand arch tp
                 -> Syn t arch (BoundVar (Sym t) arch tp)
allocateBoundVar = undefined

-- | For locations referenced in an operand list *and* defined by a formula (the
-- definitions of the formula are the first element of the pair), add them to a
-- new map (suitable for a ParameterizedFormula).
--
-- This just involves wrapping the location with an 'F.Operand' constructor.
--
-- We remove the promoted locations from the original definition map, since we
-- need to know which locations are left (they are handled by
-- 'liftImplicitLocations').
replaceParameters :: (CS.ConcreteArchitecture arch)
                  => proxy arch
                  -> SL.Index sh tp
                  -> Operand arch tp
                  -> (MapF.MapF (Location arch) (C.SymExpr (Sym t)), MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t)))
                  -> (MapF.MapF (Location arch) (C.SymExpr (Sym t)), MapF.MapF (F.Parameter arch sh) (C.SymExpr (Sym t)))
replaceParameters proxy ix op (defs, m) =
  case operandToLocation proxy op of
    Nothing -> (defs, m)
    Just loc ->
      case MapF.lookup loc defs of
        Nothing ->
          -- This lookup would fail on a purely input operand that had no
          -- definition in the final formula
          (defs, m)
        Just expr -> (MapF.delete loc defs, MapF.insert (F.Operand (locationType loc) ix) expr m)
