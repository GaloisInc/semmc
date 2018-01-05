{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module SemMC.Architecture.Evaluate (
  evaluateInstruction
  ) where

import           Data.Monoid ((<>))
import qualified Data.Map as Map
import           Data.Parameterized.Classes (ShowF(showF))
import           Data.Parameterized.Some (Some(Some), viewSome)
import           Data.Parameterized.NatRepr (knownNat, withKnownNat)
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.TraversableFC (traverseFC, fmapFC)
import           Data.Parameterized.TraversableF (traverseF)
import qualified Data.Word.Indexed as W

import qualified Dismantle.Instruction as I

import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as SI
import qualified Lang.Crucible.BaseTypes as BT

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Value as AV
import qualified SemMC.Architecture.View as V
import qualified SemMC.Formula as F

-- | Evaluate an instruction against the architecture's semantics for
-- that instruction, using an initial state as input and producing a
-- final state.
evaluateInstruction :: (A.Architecture arch, MapF.OrdF (A.Opcode arch (A.Operand arch)))
                    => S.SimpleBuilder t st
                    -- ^ The symbolic backend
                    -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (S.SimpleBuilder t st) arch)
                    -- ^ A collection of all of our semantics
                    -> A.Instruction arch
                    -- ^ The instruction being tested
                    -> V.ConcreteState arch
                    -- ^ The initial state
                    -> IO (V.ConcreteState arch)
evaluateInstruction sb semMap inst initialState =
    case inst of
        I.Instruction opc operands ->
            case MapF.lookup opc semMap of
                Nothing -> return undefined
                Just paramFormula -> do
                    (_, formula) <- F.instantiateFormula sb paramFormula operands
                    -- return $ F.evaluateFormula formula initialState
                    return undefined

data PairF a b tp = PairF (a tp) (b tp)

pairToPairF :: MapF.Pair a b -> Some (PairF a b)
pairToPairF (MapF.Pair a b) = Some (PairF a b)

sndPairF :: PairF a b tp -> b tp
sndPairF (PairF _ b) = b

evaluateFormula :: forall t st arch .
                   (MapF.OrdF (A.Location arch), ShowF (A.Location arch), A.IsLocation (A.Location arch))
                => S.SimpleBuilder t st
                -> F.Formula (S.SimpleBuilder t st) arch
                -> V.ConcreteState arch
                -> IO (V.ConcreteState arch)
evaluateFormula sb formula initialState =
    -- Make an initial assignment of pairs (Location, BoundVar).
    let assignment0 :: Some (Ctx.Assignment (PairF (A.Location arch) (S.SimpleBoundVar t)))
        assignment0 = Ctx.fromList (pairToPairF <$> (MapF.toList (F.formParamVars formula)))
    in case assignment0 of
        Some assignment0' -> do
            -- Make another sequence of the same shape with the concrete
            -- values for each location (taken from the initial concrete
            -- state)
            let bindMatchingLocation :: forall b tp . PairF (A.Location arch) b tp -> IO (S.Elt t tp)
                bindMatchingLocation (PairF loc _) =
                    case MapF.lookup loc initialState of
                        Nothing ->
                            error $ "BUG: architecture state missing location " <> showF loc
                        Just val -> valueToCrucibleElt val
                vars = fmapFC sndPairF assignment0'

                valueToCrucibleElt :: AV.Value tp -> IO (S.Elt t tp)
                valueToCrucibleElt v =
                    case v of
                        AV.ValueMem _ -> error "ValueMem not supported by valueToCrucibleElt"
                        AV.ValueBV (wordValue :: W.W n) ->
                            let w = W.unW wordValue
                            in SI.bvLit sb (knownNat @n) w

            substitutions <- traverseFC bindMatchingLocation assignment0'

            newFormDefs <- traverseF
                             (\e -> S.evalBoundVars sb e vars substitutions)
                             (F.formDefs formula)

            let f :: A.Location arch tp -> S.Elt t tp -> V.ConcreteState arch -> V.ConcreteState arch
                f loc expr m =
                    case A.locationType loc of
                        BT.BaseBVRepr (repr::BT.NatRepr n) ->
                            withKnownNat repr $ case SI.asUnsignedBV expr of
                                Nothing -> error $ "BUG: asUnsignedBV returned Nothing for " <> showF expr
                                Just val ->
                                    let wVal :: W.W n
                                        wVal = W.w val
                                    in MapF.insert loc (AV.ValueBV wVal) m

            return $ MapF.foldrWithKey f initialState newFormDefs
