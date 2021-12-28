{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.IORelation.Explicit (
  generateExplicitInstruction,
  classifyExplicitOperands
  ) where

import qualified GHC.Err.Located as L

import           Control.Monad ( replicateM )
import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import           Text.Printf ( printf )

import qualified Data.Set.NonEmpty as NES
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE

import           SemMC.Stochastic.IORelation.Shared
import           SemMC.Stochastic.IORelation.Types

import           Prelude

-- | Make a random instruction that does not reference any implicit operands.
--
-- This could be made more efficient - right now, it just tries to generate
-- random instructions until it gets a match.
generateExplicitInstruction :: (AC.ConcreteArchitecture arch, D.ArbitraryOperands (A.Opcode arch) (A.Operand arch))
                            => Proxy arch
                            -> A.Opcode arch (A.Operand arch) sh
                            -> [Some (V.View arch)]
                            -> Learning arch (A.Instruction arch)
generateExplicitInstruction proxy op implicitOperands = do
  g <- askGen
  insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
  case insn of
    D.Instruction _ ops ->
      case SL.ifoldr (matchesOperand proxy implicitOperands) (False, S.empty) ops of
        (False, sops)
          -- The operands are all distinct and no operands are implicit
          | S.size sops == length (instructionRegisterOperands proxy ops) -> return insn
        _ -> generateExplicitInstruction proxy op implicitOperands

-- | Generate test cases and send them off to the remote runner.  Collect and
-- interpret the results to create an IORelation that describes the explicit
-- operands of the instruction.
classifyExplicitOperands :: (AC.ConcreteArchitecture arch, D.ArbitraryOperands (A.Opcode arch) (A.Operand arch))
                         => A.Opcode arch (A.Operand arch) sh
                         -> SL.List (A.Operand arch) sh
                         -> Learning arch (IORelation arch sh)
classifyExplicitOperands op explicitOperands = do
  t0 <- mkRandomTest
  tests <- generateExplicitTestVariants insn t0
  tests' <- mapM (wrapTestBundle insn) tests
  results <- withTestResults op tests'
  computeIORelation op explicitOperands tests' results
  where
    insn = D.Instruction op explicitOperands

-- | For all of the explicit operands, map the results of tests to deductions
-- (forming an 'IORelation')
computeIORelation :: (AC.ConcreteArchitecture arch)
                  => A.Opcode arch (A.Operand arch) sh
                  -> SL.List (A.Operand arch) sh
                  -> [TestBundle (TestCase arch) (ExplicitFact arch)]
                  -> CE.ResultIndex (V.ConcreteState arch)
                  -> Learning arch (IORelation arch sh)
computeIORelation opcode operands bundles idx =
  F.foldlM (buildIORelation opcode operands idx) mempty bundles

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
                 . (AC.ConcreteArchitecture arch)
                => A.Opcode arch (A.Operand arch) sh
                -> SL.List (A.Operand arch) sh
                -> CE.ResultIndex (V.ConcreteState arch)
                -> IORelation arch sh
                -> TestBundle (TestCase arch) (ExplicitFact arch)
                -> Learning arch (IORelation arch sh)
buildIORelation op explicitOperands ri iorel tb = do
  case tbResult tb of
    ExplicitFact { lIndex = alteredIndex, lOpcode = lop }
      | Just P.Refl <- P.testEquality op lop -> do
          explicitOutputLocs <- S.unions <$> mapM (collectExplicitLocations alteredIndex explicitOperands explicitLocs ri) (tbTestCases tb)
          case S.null explicitOutputLocs of
            True -> return iorel
            False ->
              -- If the set of explicit output locations discovered by this test
              -- bundle is non-empty, then the location mentioned in the learned
              -- fact is an input.
              let newRel = IORelation { inputs = S.singleton (OperandRef (Some alteredIndex))
                                      , outputs = S.fromList $ map OperandRef (F.toList explicitOutputLocs)
                                      }
              in return (iorel <> newRel)
      | otherwise -> L.error (printf "Opcode mismatch; expected %s but got %s" (P.showF op) (P.showF lop))
  where
    explicitLocs = instructionRegisterOperands (Proxy :: Proxy arch) explicitOperands

-- | For the given test case, look up the results and compare them to the input
--
-- If the test failed, return an empty set.
collectExplicitLocations :: (AC.ConcreteArchitecture arch)
                         => SL.Index sh tp
                         -> SL.List (A.Operand arch) sh
                         -> [IndexedSemanticView arch sh]
                         -> CE.ResultIndex (V.ConcreteState arch)
                         -> TestCase arch
                         -> Learning arch (S.Set (Some (SL.Index sh)))
collectExplicitLocations alteredIndex _opList explicitLocs ri tc = do
  case M.lookup (CE.testNonce tc) (CE.riSuccesses ri) of
    Nothing -> return S.empty
    Just res -> F.foldrM (addLocIfDifferent (CE.resultContext res)) S.empty explicitLocs
  where
    addLocIfDifferent resCtx (IndexedSemanticView idx (V.SemanticView { V.semvView = opView@(V.View {}) })) s
      | Just P.Refl <- P.testEquality alteredIndex idx = return s
      | output <- V.peekMS resCtx opView
      , input <- V.peekMS (CE.testContext tc) opView = do
          case input /= output of
            True -> return (S.insert (Some idx) s)
            False -> return s

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
                              . (AC.ConcreteArchitecture arch)
                             => A.Instruction arch
                             -> V.ConcreteState arch
                             -> Learning arch [TestBundle (V.ConcreteState arch) (ExplicitFact arch)]
generateExplicitTestVariants i s0 =
  case i of
    D.Instruction opcode operands -> do
      mapM (genVar opcode) (instructionRegisterOperands (Proxy :: Proxy arch) operands)
  where
    genVar :: forall sh
            . A.Opcode arch (A.Operand arch) sh
           -> IndexedSemanticView arch sh
           -> Learning arch (TestBundle (V.ConcreteState arch) (ExplicitFact arch))
    genVar opcode (IndexedSemanticView ix (V.SemanticView { V.semvView = view })) = do
      cases <- generateVariantsFor s0 opcode ix (Some view)
      return TestBundle { tbTestCases = cases
                        , tbTestBase = s0
                        , tbResult = ExplicitFact { lOpcode = opcode
                                                  , lIndex = ix
                                                  , lLocation = view
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
generateVariantsFor :: (AC.ConcreteArchitecture arch)
                    => V.ConcreteState arch
                    -> A.Opcode arch (A.Operand arch) sh
                    -> SL.Index sh tp
                    -> Some (V.View arch)
                    -> Learning arch [V.ConcreteState arch]
generateVariantsFor s0 _opcode _ix (Some v@(V.View {})) = do
  replicateM 20 (withGeneratedValueForLocation v (\x -> V.pokeMS s0 v x))

matchesOperand :: (AC.ConcreteArchitecture arch)
               => Proxy arch
               -> [Some (V.View arch)]
               -> SL.Index sh tp
               -> A.Operand arch tp
               -> (Bool, S.Set (Some (V.View arch)))
               -> (Bool, S.Set (Some (V.View arch)))
matchesOperand proxy implicits _ix operand (matches, sops) =
  case AC.operandToSemanticView proxy operand of
    Nothing -> (matches, sops)
    Just (V.SemanticView { V.semvView = view }) ->
      (matches || any (== (Some view)) implicits, S.insert (Some view) sops)

{- Note [Test Form]

For each explicit operand in an instruction, we generate test cases with
different values for that location.  The ExplicitFact records the index of the
operand so that we can generate appropriate entries in the IORelation later.

If changing the location causes other locations to change after the test, it
means the location was an input location.  At the same time, the locations that
change are output locations.

Note that a location can be both an input and an output.


-}
