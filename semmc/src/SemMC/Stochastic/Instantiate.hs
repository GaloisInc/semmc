{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SemMC.Stochastic.Instantiate (
  instantiateInstruction
  ) where

import qualified GHC.Err.Located as L

import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.ShapedList as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC ( foldrFC )

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Architecture as A
import qualified SemMC.ConcreteState as CS

import           SemMC.Stochastic.IORelation ( IORelation(..), OperandRef(..) )
import           SemMC.Stochastic.Monad


-- | Generate an arbitrary instruction for the given opcode.
--
-- The "registerized" instruction selects a location in the machine state to
-- stand in for immediates.  We need this to extract formulas (and to learn
-- formulas for instructions with immediates).
--
-- Note that this function should strive to avoid generating instructions with
-- explicit operands that overlap with implicit operands.  It should also avoid
-- generating instructions that re-use registers.
--
-- There is an additional restriction where we do not want to use implicit or
-- explicit operands as stand-in locations for immediates.
--
-- This function can fail (with an error) if we do not have enough registers to
-- act as stand ins for immediates; that seems highly unlikely, but something to
-- watch out for.
instantiateInstruction :: forall arch sh t
                        . (CS.ConcreteArchitecture arch, SynC arch)
                       => A.Opcode arch (A.Operand arch) sh
                       -> Syn t arch (RegisterizedInstruction arch)
instantiateInstruction op = do
  gen <- askGen
  Just iorel <- opcodeIORelation op
  go gen (implicitOperands iorel)
  where
    -- Generate random instructions until we get one with explicit operands that
    -- do not overlap with implicit operands.
    go :: A.Gen -> S.Set (Some (CS.View arch)) -> Syn t arch (RegisterizedInstruction arch)
    go gen implicitOps = do
      target <- liftIO $ D.randomInstruction gen (NES.singleton (Some op))
      case target of
        D.Instruction op' ops
          | Just MapF.Refl <- MapF.testEquality op op' ->
            let (isReusedOrImplicitOp, explicitLocs) = foldrFC (isImplicitOrReusedOperand (Proxy :: Proxy arch) implicitOps) (False, S.empty) ops
            in case isReusedOrImplicitOp of
              True -> go gen implicitOps
              False -> do
                -- If there is a literal operand, find a register that is neither
                -- an implicit operand nor an explicit operand to stand in for the
                -- immediate(s).
                let s0 = (MapF.empty, A.allLocations @(A.Location arch))
                let usedLocs = S.union (S.map liftSomeView implicitOps) (S.foldr (liftSomeOperand (Proxy @arch)) S.empty explicitLocs)
                let (litLocs, _) = SL.foldrFCIndexed (assignLiterals usedLocs) s0 ops
                return RI { riInstruction = target
                          , riOpcode = op'
                          , riOperands = ops
                          , riLiteralLocs = litLocs
                          }
          | otherwise -> L.error ("Invalid opcode: " ++ P.showF op ++ " vs " ++ P.showF op')

liftSomeView :: Some (CS.View arch) -> Some (A.Location arch)
liftSomeView (Some (CS.View _ loc)) = Some loc

liftSomeOperand :: (CS.ConcreteArchitecture arch)
                => proxy arch
                -> Some (A.Operand arch)
                -> S.Set (Some (A.Location arch))
                -> S.Set (Some (A.Location arch))
liftSomeOperand proxy (Some op) s =
  case A.operandToLocation proxy op of
    Nothing -> s
    Just loc -> S.insert (Some loc) s

-- | For each literal, assign a location in the machine state to stand in for
-- it.
--
-- Note: we determine if an operand is a literal by trying to convert it to a
-- location.  If we can't convert it to a location, it is a literal.
--
-- This will throw an error if there isn't a free register to represent all of
-- the literals.  That seems extremely unlikely, but is something to potentially
-- watch out for.
assignLiterals :: forall arch sh tp
                . (CS.ConcreteArchitecture arch)
               => S.Set (Some (A.Location arch))
               -> SL.Index sh tp
               -> A.Operand arch tp
               -> (MapF.MapF (LiteralRef arch sh) (A.Location arch), [Some (A.Location arch)])
               -> (MapF.MapF (LiteralRef arch sh) (A.Location arch), [Some (A.Location arch)])
assignLiterals usedLocs ix op acc@(m, locs) =
  case A.operandToLocation (Proxy @arch) op of
    Just _ -> acc
    Nothing ->
      let (locs', loc) = findUnusedLocation (Proxy @arch) usedLocs op locs
      in (MapF.insert (LiteralRef ix) loc m, locs')

findUnusedLocation :: (CS.ConcreteArchitecture arch)
                   => proxy arch
                   -> S.Set (Some (A.Location arch))
                   -> A.Operand arch tp
                   -> [Some (A.Location arch)]
                   -> ([Some (A.Location arch)], A.Location arch (A.OperandType arch tp))
findUnusedLocation proxy usedLocs op locs =
  case locs of
    [] -> L.error "Not enough locations to find a virtual literal location"
    (Some loc : rest)
      | Just P.Refl <- P.testEquality (A.locationType loc) (A.operandType proxy op)
      , not (S.member (Some loc) usedLocs) -> (rest, loc)
      | otherwise -> findUnusedLocation proxy usedLocs op rest

isImplicitOrReusedOperand :: (CS.ConcreteArchitecture arch, SynC arch)
                          => Proxy arch
                          -> S.Set (Some (CS.View arch))
                          -> A.Operand arch tp
                          -> (Bool, S.Set (Some (A.Operand arch)))
                          -> (Bool, S.Set (Some (A.Operand arch)))
isImplicitOrReusedOperand proxy implicitViews operand (isIorR, seen)
  | isIorR || S.member (Some operand) seen = (True, seen)
  | Just (CS.SemanticView { CS.semvView = view }) <- CS.operandToSemanticView proxy operand
  , S.member (Some view) implicitViews = (True, seen)
  | otherwise = (isIorR, S.insert (Some operand) seen)

implicitOperands :: (CS.ConcreteArchitecture arch) => IORelation arch sh -> S.Set (Some (CS.View arch))
implicitOperands iorel =
  F.foldl' addImplicitLoc S.empty (inputs iorel <> outputs iorel)
  where
    addImplicitLoc s opref =
      case opref of
        ImplicitOperand sloc -> S.insert sloc s
        OperandRef {} -> s
