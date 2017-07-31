{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.IORelation.Explicit (
  generateExplicitInstruction,
  classifyExplicitOperands
  ) where

import qualified GHC.Err.Located as L

import Control.Monad ( replicateM )
import Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S

import qualified Data.Set.NonEmpty as NES
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.NatRepr as NR
import Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture
import qualified SemMC.ConcreteState as CS

import SemMC.Stochastic.IORelation.Shared
import SemMC.Stochastic.IORelation.Types
import qualified SemMC.Stochastic.Remote as R


-- | Make a random instruction that does not reference any implicit operands.
--
-- This could be made more efficient - right now, it just tries to generate
-- random instructions until it gets a match.
generateExplicitInstruction :: (CS.ConcreteArchitecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                            => Proxy arch
                            -> Opcode arch (Operand arch) sh
                            -> [Some (CS.View arch)]
                            -> Learning arch (Instruction arch)
generateExplicitInstruction proxy op implicitOperands = do
  g <- askGen
  insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
  case insn of
    D.Instruction _ ops ->
      case D.foldrOperandList (matchesOperand proxy implicitOperands) False ops of
        True -> generateExplicitInstruction proxy op implicitOperands
        False -> return insn

-- | Generate test cases and send them off to the remote runner.  Collect and
-- interpret the results to create an IORelation that describes the explicit
-- operands of the instruction.
classifyExplicitOperands :: (CS.ConcreteArchitecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                         => Opcode arch (Operand arch) sh
                         -> D.OperandList (Operand arch) sh
                         -> Learning arch (IORelation arch sh)
classifyExplicitOperands op explicitOperands = do
  t0 <- mkRandomTest
  tests <- generateExplicitTestVariants insn t0
  tests' <- mapM (wrapTestBundle insn) tests
  withTestResults op tests' $ computeIORelation op explicitOperands tests'
  where
    insn = D.Instruction op explicitOperands

-- | For all of the explicit operands, map the results of tests to deductions
-- (forming an 'IORelation')
computeIORelation :: (CS.ConcreteArchitecture arch)
                  => Opcode arch (Operand arch) sh
                  -> D.OperandList (Operand arch) sh
                  -> [TestBundle (R.TestCase (CS.ConcreteState arch)) (ExplicitFact arch)]
                  -> [R.ResultOrError (CS.ConcreteState arch)]
                  -> Learning arch (IORelation arch sh)
computeIORelation opcode operands bundles results =
  F.foldlM (buildIORelation opcode operands idx) mempty bundles
  where
    idx = F.foldl' indexResults emptyResultIndex results

-- | Interpret the results of a test (by consulting the 'ResultIndex' based on nonces)
--
-- 1) Look up the result of the concrete run of the original test
--    ('tbTestOrig').  We'll learn information based on deviation of the other
--    test cases from this one.
--
-- 2) If any of the locations referenced by the instruction change, they are
--    (explicit) output locations and the tagged location is an input.  If no
--    locations change due to the tagged location, it could be an output -- we
--    don't know, so don't conclude anything.  Future tests will figure out if
--    it is an output.
buildIORelation :: forall arch sh
                 . (CS.ConcreteArchitecture arch)
                => Opcode arch (Operand arch) sh
                -> D.OperandList (Operand arch) sh
                -> ResultIndex (CS.ConcreteState arch)
                -> IORelation arch sh
                -> TestBundle (R.TestCase (CS.ConcreteState arch)) (ExplicitFact arch)
                -> Learning arch (IORelation arch sh)
buildIORelation op explicitOperands ri iorel tb = do
  -- If the set of explicit output locations discovered by this test bundle is
  -- non-empty, then the location mentioned in the learned fact is an input.
  -- Keep that in mind while augmenting the IORelation
  explicitOutputLocs <- S.unions <$> mapM (collectExplicitLocations explicitOperands explicitLocs ri) (tbTestCases tb)
  -- FIXME: Should it be an error if this is null?  That would be pretty strange...
  case S.null explicitOutputLocs of
    True -> return iorel
    False ->
      case tbResult tb of
        ExplicitFact { lIndex = ix, lOpcode = lop }
          | Just P.Refl <- P.testEquality op lop ->
            let newRel = IORelation { inputs = S.singleton (OperandRef (Some ix))
                                    , outputs = S.fromList $ map OperandRef (F.toList explicitOutputLocs)
                                    }
            in return (iorel <> newRel)
          | otherwise -> L.error ("Opcode mismatch: expected " ++ P.showF op ++ " but got " ++ P.showF lop)
  where
    explicitLocs = instructionRegisterOperands (Proxy :: Proxy arch) explicitOperands

-- | For the given test case, look up the results and compare them to the input
--
-- If the test failed, return an empty set.
collectExplicitLocations :: (CS.ConcreteArchitecture arch)
                         => D.OperandList (Operand arch) sh
                         -> [IndexedView arch sh]
                         -> ResultIndex (CS.ConcreteState arch)
                         -> R.TestCase (CS.ConcreteState arch)
                         -> Learning arch (S.Set (Some (D.Index sh)))
collectExplicitLocations _opList explicitLocs ri tc = do
  case M.lookup (R.testNonce tc) (riSuccesses ri) of
    Nothing -> return S.empty
    Just res -> F.foldrM (addLocIfDifferent (R.resultContext res)) S.empty explicitLocs
  where
    addLocIfDifferent resCtx (IndexedView idx (Some opLoc)) s
      | output <- NR.withKnownNat (CS.viewTypeRepr opLoc) (CS.peekMS resCtx opLoc)
      , input <- NR.withKnownNat (CS.viewTypeRepr opLoc) (CS.peekMS (R.testContext tc) opLoc) =
          case input /= output of
            True -> return (S.insert (Some idx) s)
            False -> return s
      | otherwise = L.error ("Missing location in architecture state: " ++ P.showF opLoc)

-- | Given an initial test state, generate all interesting variants on it.  The
-- idea is to see which outputs change when we tweak an input.
--
-- We learn the *inputs* set by starting with an initial test t0 and tweaking
-- each element in the state in turn.  For each tweaked input, we examine the
-- effects on the output states.  We want to avoid tweaking the registers that
-- are instantiated as operands to the instruction, as we expect those to cause
-- changes.  We really just need to learn which of the operands are inputs vs
-- outputs, and if there are any implicit arguments.
--
-- We learn the *outputs* set by comparing the tweaked input vs the output from
-- that test vector: all modified registers are in the output set.
generateExplicitTestVariants :: forall arch
                              . (CS.ConcreteArchitecture arch)
                             => Instruction arch
                             -> CS.ConcreteState arch
                             -> Learning arch [TestBundle (CS.ConcreteState arch) (ExplicitFact arch)]
generateExplicitTestVariants i s0 =
  case i of
    D.Instruction opcode operands -> do
      mapM (genVar opcode) (instructionRegisterOperands (Proxy :: Proxy arch) operands)
  where
    genVar :: forall sh
            . Opcode arch (Operand arch) sh
           -> IndexedView arch sh
           -> Learning arch (TestBundle (CS.ConcreteState arch) (ExplicitFact arch))
    genVar opcode (IndexedView ix (Some loc)) = do
      cases <- generateVariantsFor s0 opcode ix (Some loc)
      return TestBundle { tbTestCases = cases
                        , tbResult = ExplicitFact { lOpcode = opcode
                                                  , lIndex = ix
                                                  , lLocation = loc
                                                  , lInstruction = i
                                                  }
                        }

-- | Tweak the value in the 'ConcreteState' at the given location to a number of
-- random values.
--
-- This has to be in IO so that we can generate 'S.SymExpr's
--
-- Right now, we only support generating random bitvectors.  That will get more
-- interesting once we start dealing with floats.  Note that we could just
-- generate random bitvectors for floats, too, but we would probably want to
-- tweak the distribution to generate interesting types of floats.
generateVariantsFor :: (CS.ConcreteArchitecture arch)
                    => CS.ConcreteState arch
                    -> Opcode arch (Operand arch) sh
                    -> D.Index sh tp
                    -> Some (CS.View arch)
                    -> Learning arch [CS.ConcreteState arch]
generateVariantsFor s0 _opcode _ix (Some v) = do
  replicateM 20 (withGeneratedValueForLocation v (\x -> CS.pokeMS s0 v x))

matchesOperand :: (CS.ConcreteArchitecture arch)
               => Proxy arch
               -> [Some (CS.View arch)]
               -> D.Index sh tp
               -> Operand arch tp
               -> Bool
               -> Bool
matchesOperand proxy implicits _ix operand matches =
  case CS.operandToView proxy operand of
    Nothing -> matches
    Just loc -> matches || any (== loc) implicits

{- Note [Test Form]

For each explicit operand in an instruction, we generate test cases with
different values for that location.  The ExplicitFact records the index of the
operand so that we can generate appropriate entries in the IORelation later.

If changing the location causes other locations to change after the test, it
means the location was an input location.  At the same time, the locations that
change are output locations.

Note that a location can be both an input and an output.


-}
