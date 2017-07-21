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
import Data.Proxy ( Proxy(..) )
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
  -- We generate 20 random instruction instances with this opcode (and for each
  -- random instruction instance, generate many test vectors).
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
buildImplicitRelation op rix iorel tb = do
  implicitLocs <- mconcat <$> mapM (collectImplicitOutputLocations op rix (tbResult tb)) (tbTestCases tb)
  return (iorel <> implicitLocs)

collectImplicitOutputLocations :: (Architecture arch)
                               => Opcode arch (Operand arch) sh
                               -> ResultIndex (ArchState (Sym t) arch)
                               -> ImplicitFact arch
                               -> R.TestCase (ArchState (Sym t) arch)
                               -> M t arch (IORelation arch sh) -- S.Set (Some (Location arch)))
collectImplicitOutputLocations _op rix f tc =
  case M.lookup (R.testNonce tc) (riSuccesses rix) of
    Nothing -> return mempty
    Just res ->
      case f of
        ImplicitFact { ifExplicits = explicitOperands
                     , ifLocation = loc0
                     } ->
          F.foldrM (addLocIfImplicitAndDifferent loc0 explicitOperands) mempty (MapF.toList (R.resultContext res))
  where
    addLocIfImplicitAndDifferent loc0 explicitOperands (MapF.Pair loc postVal) s
      | Just preVal <- MapF.lookup loc (R.testContext tc) =
          case () of
            () | preVal == postVal -> return s
               | Some loc /= loc0 && S.member (Some loc) explicitOperands ->
                 return s { inputs = S.insert (ImplicitOperand loc0) (inputs s) }
               | Some loc /= loc0 ->
                 return s { inputs = S.insert (ImplicitOperand loc0) (inputs s)
                          , outputs = S.insert (ImplicitOperand (Some loc)) (outputs s)
                          }
               | otherwise -> return s
      | otherwise = L.error ("Expected location in state: " ++ P.showF loc)

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
generateImplicitTests i s0 = do
  let allLocs = testCaseLocations (Proxy :: Proxy arch) s0
  mapM (genTestForLoc i s0) allLocs

genTestForLoc :: forall arch t
               . (Architecture arch)
              => Instruction arch
              -> ArchState (Sym t) arch
              -> Some (Location arch)
              -> M t arch (TestBundle (ArchState (Sym t) arch) (ImplicitFact arch))
genTestForLoc i s0 (Some loc0) = do
  testStates <- replicateM 20 (withGeneratedValueForLocation loc0 (\x -> MapF.insert loc0 x s0))
  case i of
    D.Instruction _ ops -> do
      let explicits = [ Some loc
                      | Some (PairF _ (TL loc)) <- instructionRegisterOperands (Proxy :: Proxy arch) ops
                      ]
      return TestBundle { tbTestCases = testStates
                        , tbResult = ImplicitFact { ifExplicits = S.fromList explicits
                                                  , ifLocation = Some loc0
                                                  , ifInstruction = i
                                                  }
                        }

{- Note [Test Form]

In this module we are trying to learn the set of locations that are *implicit
operands* (i.e., operands not mentioned in operand lists) for each instruction.

Implicit operands can be both inputs and outputs (e.g., flags registers).

To find implicit operands, we generate test inputs where we tweak locations one
at a time.  Let the location to be tweaked be L_0.  Generate a number of test
cases that tweak L_0 with both random and interesting values.  We then compare
the inputs against the corresponding outputs from the test cases after they are
run.  In the output states, for each L_i, if L_i is changed:

 * If L_i is an explicit operand, then L_0 is an implicit input operand
   (unless i == 0, in which case L_0 is an explicit input and output)
 * If L_i is not an explicit operand, then L_0 is an implicit input operand and L_i is an implicit output operand
   (unless i == 0, in which case L_0 is an implicit input and output)

-}
