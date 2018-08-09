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
import           Data.Parameterized.Classes (ShowF(showF))
import           Data.Parameterized.Some (Some(Some))
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.TraversableFC (traverseFC, fmapFC)
import           Data.Parameterized.TraversableF (traverseF)
import qualified Data.Word.Indexed as W

import qualified Dismantle.Instruction as I

import qualified What4.BaseTypes as BT
import qualified Lang.Crucible.Backend as SB
import qualified What4.Interface as SI
import qualified What4.Expr.Builder as S

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Value as AV
import qualified SemMC.Architecture.View as V
import qualified SemMC.Formula as F

-- | Evaluate an instruction against the architecture's semantics for
-- that instruction, using an initial state as input and producing a
-- final state.
evaluateInstruction :: (A.Architecture arch
                       , MapF.OrdF (A.Opcode arch (A.Operand arch))
                       , SB.IsBoolSolver (S.ExprBuilder t st fs))
                    => S.ExprBuilder t st fs
                    -- ^ The symbolic backend
                    -> MapF.MapF (A.Opcode arch (A.Operand arch)) (F.ParameterizedFormula (S.ExprBuilder t st fs) arch)
                    -- ^ A collection of all of our semantics
                    -> A.Instruction arch
                    -- ^ The instruction being tested
                    -> V.ConcreteState arch
                    -- ^ The initial state
                    -> IO (Either String (V.ConcreteState arch))
evaluateInstruction sb semMap inst initialState =
    case inst of
        I.Instruction opc operands ->
            case MapF.lookup opc semMap of
                Nothing -> return $ Left $ "Opcode not found in semantics: " <> showF opc
                Just paramFormula -> do
                    (_, formula) <- F.instantiateFormula sb paramFormula operands
                    Right <$> evaluateFormula sb formula initialState

data PairF a b tp = PairF (a tp) (b tp)

pairToPairF :: MapF.Pair a b -> Some (PairF a b)
pairToPairF (MapF.Pair a b) = Some (PairF a b)

sndPairF :: PairF a b tp -> b tp
sndPairF (PairF _ b) = b

evaluateFormula :: forall t st fs arch .
                   (MapF.OrdF (A.Location arch), ShowF (A.Location arch), A.IsLocation (A.Location arch))
                => S.ExprBuilder t st fs
                -> F.Formula (S.ExprBuilder t st fs) arch
                -> V.ConcreteState arch
                -> IO (V.ConcreteState arch)
evaluateFormula sb formula initialState =
    -- Make an initial assignment of pairs (Location, BoundVar).
    let assignment0 :: Some (Ctx.Assignment (PairF (A.Location arch) (S.ExprBoundVar t)))
        assignment0 = Ctx.fromList (pairToPairF <$> (MapF.toList (F.formParamVars formula)))
    in case assignment0 of
        Some assignment0' -> do
            -- Make another sequence of the same shape with the concrete
            -- values for each location (taken from the initial concrete
            -- state)
            let bindMatchingLocation :: forall tp . PairF (A.Location arch) (S.ExprBoundVar t) tp -> IO (S.Expr t tp)
                bindMatchingLocation (PairF loc bv) =
                    case MapF.lookup loc initialState of
                        Nothing -> return $ S.BoundVarExpr bv
                        Just val -> valueToCrucibleElt val
                vars = fmapFC sndPairF assignment0'

                valueToCrucibleElt :: AV.Value tp -> IO (S.Expr t tp)
                valueToCrucibleElt v =
                    case v of
                        AV.ValueMem _ -> error "ValueMem not supported by valueToCrucibleElt"
                        AV.ValueBV (wordValue :: W.W n) ->
                            let w = W.unW wordValue
                            in SI.bvLit sb (W.rep wordValue) w

            substitutions <- traverseFC bindMatchingLocation assignment0'

            newFormDefs <- traverseF
                             (\e -> S.evalBoundVars sb e vars substitutions)
                             (F.formDefs formula)

            let f :: A.Location arch tp -> S.Expr t tp -> V.ConcreteState arch -> V.ConcreteState arch
                f loc expr m =
                    case A.locationType loc of
                        BT.BaseBVRepr (repr::BT.NatRepr n) ->
                            case SI.asUnsignedBV expr of
                                Nothing -> m
                                Just val ->
                                    let wVal :: W.W n
                                        wVal = W.wRep repr val
                                    in MapF.insert loc (AV.ValueBV wVal) m
                        _ ->
                            -- FIXME one day: we aren't currently
                            -- handling memory locations, which are the
                            -- only other kind of thing we could expect
                            -- here.
                            m

            return $ MapF.foldrWithKey f initialState newFormDefs
