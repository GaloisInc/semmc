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
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC ( foldrFC )

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.View as V

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
instantiateInstruction :: forall arch sh t solver fs
                        . (SynC arch)
                       => A.Opcode arch (A.Operand arch) sh
                       -> Syn t solver fs arch (AC.RegisterizedInstruction arch)
instantiateInstruction op = do
  gen <- askGen
  miorel <- opcodeIORelation op
  case miorel of
    Just iorel -> liftIO $ go gen (implicitOperands iorel)
    Nothing -> error ("Error loading IORelation for " ++ P.showF op)
  where
    -- Generate random instructions until we get one with explicit operands that
    -- do not overlap with implicit operands.
    go :: DA.Gen -> S.Set (Some (V.View arch)) -> IO (AC.RegisterizedInstruction arch)
    go gen implicitOps = do
      target <- D.randomInstruction gen (NES.singleton (Some op))
      case target of
        D.Instruction op' ops
          | Just MapF.Refl <- MapF.testEquality op op' ->
            let (isReusedOrImplicitOp, explicitLocs) = foldrFC (isImplicitOrReusedOperand (Proxy @arch) implicitOps) (False, S.empty) ops
            in case isReusedOrImplicitOp of
              True -> go gen implicitOps
              False -> do
                -- If there is a literal operand, find a register that is neither
                -- an implicit operand nor an explicit operand to stand in for the
                -- immediate(s).
                let s0 = (MapF.empty, A.registerizationLocations @(A.Location arch))
                let usedLocs = S.union (S.map liftSomeView implicitOps) (S.foldr (liftSomeOperand (Proxy @arch)) S.empty explicitLocs)
                let (litLocs, _) = SL.ifoldr (assignLiterals op' usedLocs) s0 ops
                return AC.RI { AC.riInstruction = target
                             , AC.riOpcode = op'
                             , AC.riOperands = ops
                             , AC.riLiteralLocs = litLocs
                             }
          | otherwise -> L.error ("Invalid opcode: " ++ P.showF op ++ " vs " ++ P.showF op')

liftSomeView :: Some (V.View arch) -> Some (A.Location arch)
liftSomeView (Some (V.View _ loc)) = Some loc

liftSomeOperand :: (AC.ConcreteArchitecture arch)
                => proxy arch
                -> Some (A.Operand arch)
                -> S.Set (Some (A.Location arch))
                -> S.Set (Some (A.Location arch))
liftSomeOperand proxy (Some op) s =
  case AC.operandToSemanticView proxy op of
    Nothing -> s
    Just (V.SemanticView { V.semvView = V.View _ loc }) -> S.insert (Some loc) s

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
                . (AC.ConcreteArchitecture arch)
               => A.Opcode arch (A.Operand arch) sh
               -> S.Set (Some (A.Location arch))
               -> SL.Index sh tp
               -> A.Operand arch tp
               -> (MapF.MapF (AC.LiteralRef arch sh) (A.Location arch), [Some (A.Location arch)])
               -> (MapF.MapF (AC.LiteralRef arch sh) (A.Location arch), [Some (A.Location arch)])
assignLiterals opc usedLocs ix op acc@(m, locs) =
  case AC.operandToSemanticView (Proxy @arch) op of
    Just _ -> acc
    Nothing ->
      let (locs', loc) = findUnusedLocation opc (Proxy @arch) usedLocs op locs
      in (MapF.insert (AC.LiteralRef ix) loc m, locs')

findUnusedLocation :: (AC.ConcreteArchitecture arch)
                   => A.Opcode arch (A.Operand arch) sh
                   -> proxy arch
                   -> S.Set (Some (A.Location arch))
                   -> A.Operand arch tp
                   -> [Some (A.Location arch)]
                   -> ([Some (A.Location arch)], A.Location arch (A.OperandType arch tp))
findUnusedLocation opc proxy usedLocs op locs =
  case locs of
    [] -> L.error ("Not enough locations to find a virtual literal location for " ++ P.showF opc)
    (Some loc : rest)
      | Just P.Refl <- P.testEquality (A.locationType loc) (AC.operandType proxy op)
      , not (S.member (Some loc) usedLocs) -> (rest, loc)
      | otherwise -> findUnusedLocation opc proxy usedLocs op rest

isImplicitOrReusedOperand :: (SynC arch)
                          => Proxy arch
                          -> S.Set (Some (V.View arch))
                          -> A.Operand arch tp
                          -> (Bool, S.Set (Some (A.Operand arch)))
                          -> (Bool, S.Set (Some (A.Operand arch)))
isImplicitOrReusedOperand proxy implicitViews operand (isIorR, seen)
  | isIorR || S.member (Some operand) seen = (True, seen)
  | Just (V.SemanticView { V.semvView = view }) <- AC.operandToSemanticView proxy operand
  , S.member (Some view) implicitViews = (True, seen)
  | otherwise = (isIorR, S.insert (Some operand) seen)

implicitOperands :: (AC.ConcreteArchitecture arch) => IORelation arch sh -> S.Set (Some (V.View arch))
implicitOperands iorel =
  F.foldl' addImplicitLoc S.empty (inputs iorel <> outputs iorel)
  where
    addImplicitLoc s opref =
      case opref of
        ImplicitOperand sloc -> S.insert sloc s
        OperandRef {} -> s
