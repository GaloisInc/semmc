{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.IORelation.Implicit (
  findImplicitOperands
  ) where

import qualified GHC.Err.Located as L

import Control.Monad ( replicateM )
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S

import qualified Data.Set.NonEmpty as NES
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture

import SemMC.Stochastic.Monad ( Sym )
import SemMC.Stochastic.IORelation.Shared
import SemMC.Stochastic.IORelation.Types
import qualified SemMC.Stochastic.Remote as R

-- | Sweep through the parameter space to find locations not mentioned in
-- parameter lists that are modified by the instruction.
--
-- To do this, we generate a bunch of randomized operand lists to cycle through
-- possible registers.
--
-- For this, we will want to focus on generating values that trigger edge cases
-- to make sure we can deal with flags registers.
findImplicitOperands :: forall t arch sh
                      . (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch), R.MachineState (ArchState (Sym t) arch))
                     => Opcode arch (Operand arch) sh
                     -> M t arch (IORelation arch sh)
findImplicitOperands op = do
  mkTest <- St.gets testGen
  g <- St.gets gen
  tests <- concat <$> replicateM 20 (genTestSet mkTest g)
  withTestResults op tests $ computeImplicitOperands op tests
  where
    genTestSet mkTest g = do
      t0 <- liftIO mkTest
      insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
      tb0 <- generateImplicitTests insn t0
      mapM (wrapTestBundle insn) tb0

-- | Interpret the results of the implicit operand search
--
-- The fact records all of the explicit locations.  We just need to look at all
-- of the *other* (unmentioned) locations to find any changes.  Any changed
-- locations are implicit outputs.
--
-- FIXME: To find implicit inputs, we have to keep the explicit operands
-- constant and vary all of the other locations.
computeImplicitOperands :: (Architecture arch)
                        => Opcode arch (Operand arch) sh
                        -> [TestBundle (R.TestCase (ArchState (Sym t) arch)) (ImplicitFact arch)]
                        -> [R.ResultOrError (ArchState (Sym t) arch)]
                        -> M t arch (IORelation arch sh)
computeImplicitOperands op tests results =
  F.foldlM (buildImplicitRelation op idx) mempty tests
  where
    idx = F.foldl' indexResults emptyResultIndex results

-- |
--
-- Note that the operand isn't used - it is acting like a proxy for the @sh@ parameter.
buildImplicitRelation :: (Architecture arch)
                      => Opcode arch (Operand arch) sh
                      -> ResultIndex (ArchState (Sym t) arch)
                      -> IORelation arch sh
                      -> TestBundle (R.TestCase (ArchState (Sym t) arch)) (ImplicitFact arch)
                      -> M t arch (IORelation arch sh)
buildImplicitRelation _op rix iorel tb = do
  implicitOutputLocs <- S.unions <$> mapM (collectImplicitOutputLocations rix (tbResult tb)) (tbTestCases tb)
  let newRel = IORelation { inputs = []
                          , outputs = map ImplicitOperand (F.toList implicitOutputLocs)
                          }
  return (iorel <> newRel)

collectImplicitOutputLocations :: (Architecture arch)
                               => ResultIndex (ArchState (Sym t) arch)
                               -> ImplicitFact arch
                               -> R.TestCase (ArchState (Sym t) arch)
                               -> M t arch (S.Set (Some (Location arch)))
collectImplicitOutputLocations rix f tc =
  case M.lookup (R.testNonce tc) (riSuccesses rix) of
    Nothing -> return S.empty
    Just res ->
      case f of
        ImplicitFact { ifExplicits = explicitOperands } ->
          F.foldrM (addLocIfImplicitAndDifferent (S.fromList explicitOperands)) S.empty (MapF.toList (R.resultContext res))
  where
    addLocIfImplicitAndDifferent explicitOperands pair s =
      case pair of
        MapF.Pair loc postVal
          | Some loc `S.member` explicitOperands -> return s
          | otherwise ->
            case MapF.lookup loc (R.testContext tc) of
              Nothing -> L.error ("Expected location in state: " ++ P.showF loc)
              Just preVal
                | preVal == postVal -> return s
                | otherwise -> return (S.insert (Some loc) s)

-- | For an instruction instance, sweep across the parameter space of all of the
-- interesting values for the operands.  Examine all of the locations that do
-- not appear in the register list for changes.
--
-- Repeat for a number of random instructions
generateImplicitTests :: forall arch t
                       . (Architecture arch)
                      => Instruction arch
                      -> ArchState (Sym t) arch
                      -> M t arch [TestBundle (ArchState (Sym t) arch) (ImplicitFact arch)]
generateImplicitTests = undefined
