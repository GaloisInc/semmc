{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | A module for learning the input and output relations for instructions
module SemMC.Stochastic.IORelation (
  LearnConfig(..),
  IORelation(..),
  OperandRef(..),
  learn,
  readIORelation,
  printIORelation
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent as C
import Control.Monad ( replicateM )
import qualified Control.Monad.Catch as E
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S

import qualified Data.Set.NonEmpty as NES
import qualified Data.Parameterized.Classes as P
import Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Lang.Crucible.BaseTypes as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture
import qualified SemMC.Formula.Parser as F
import SemMC.Util ( Witness(..) )

import SemMC.Stochastic.Monad ( Sym )
import qualified SemMC.Stochastic.Remote as R
import SemMC.Stochastic.IORelation.Parser
import SemMC.Stochastic.IORelation.Types



-- | Find the locations read from and written to by each instruction passed in
--
-- This is determined by observing the behavior of instructions on tests and
-- perturbing inputs randomly.
learn :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
      => LearnConfig t arch
      -> [Some (Witness (F.BuildOperandList arch) (Opcode arch (Operand arch)))]
      -> IO (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
learn config ops = St.evalStateT (runM act) config
  where
    act = F.foldlM (\m (Some (Witness op)) -> testOpcode m op) MapF.empty ops

testOpcode :: forall arch sh t . (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
           => MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
           -> Opcode arch (Operand arch) sh
           -> M t arch (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
testOpcode m op = do
  implicitOperands <- findImplicitOperands op
  insn <- generateExplicitInstruction (Proxy :: Proxy arch) op (implicitLocations implicitOperands)
  case insn of
    D.Instruction op' operandList
      | Just P.Refl <- P.testEquality op op' -> do
        explicitOperands <- classifyExplicitOperands (Proxy :: Proxy arch) op operandList
        let ioRelation = implicitOperands <> explicitOperands
        return $ MapF.insert op ioRelation m
      | otherwise -> L.error ("randomInstruction returned an instruction with the wrong opcode: " ++ P.showF op')

-- | Collect all of the locations that are read from or written to implicitly
implicitLocations :: IORelation arch sh -> [Some (Location arch)]
implicitLocations ior = foldr collectImplicits (foldr collectImplicits [] (inputs ior)) (outputs ior)
  where
    collectImplicits opRef acc =
      case opRef of
        ImplicitOperand sloc -> sloc : acc
        OperandRef {} -> acc

-- | Make a random instruction that does not reference any implicit operands.
--
-- This could be made more efficient - right now, it just tries to generate
-- random instructions until it gets a match.
generateExplicitInstruction :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                            => Proxy arch
                            -> Opcode arch (Operand arch) sh
                            -> [Some (Location arch)]
                            -> M t arch (Instruction arch)
generateExplicitInstruction proxy op implicitOperands = do
  g <- St.gets gen
  insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
  case insn of
    D.Instruction _ ops ->
      case D.foldrOperandList (matchesOperand proxy implicitOperands) False ops of
        True -> generateExplicitInstruction proxy op implicitOperands
        False -> return insn

matchesOperand :: (Architecture arch)
               => Proxy arch
               -> [Some (Location arch)]
               -> D.Index sh tp
               -> Operand arch tp
               -> Bool
               -> Bool
matchesOperand proxy implicits _ix operand matches =
  case operandToLocation proxy operand of
    Nothing -> matches
    Just loc -> matches || any (== Some loc) implicits

-- | Generate test cases and send them off to the remote runner.  Collect and
-- interpret the results to create an IORelation that describes the explicit
-- operands of the instruction.
classifyExplicitOperands :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
                         => Proxy arch
                         -> Opcode arch (Operand arch) sh
                         -> D.OperandList (Operand arch) sh
                         -> M t arch (IORelation arch sh)
classifyExplicitOperands proxy op explicitOperands = do
  mkTest <- St.gets testGen
  t0 <- liftIO mkTest
  tests <- generateTestVariants insn t0
  tests' <- mapM (wrapTestBundle insn) tests
  tchan <- St.gets testChan
  let remoteTestCases = [ t
                        | tb <- tests'
                        , t <- tbTestOrig tb : tbTestCases tb
                        ]
  liftIO $ mapM_ (C.writeChan tchan . Just) remoteTestCases
  rchan <- St.gets resChan
  mresults <- timeout $ replicateM (length remoteTestCases) (C.readChan rchan)
  case mresults of
    Just results -> computeIORelation op explicitOperands tests' results
    Nothing -> liftIO $ E.throwM (LearningTimeout proxy (Some op))
  where
    insn = D.Instruction op explicitOperands

-- | Sweep through the parameter space to find locations not mentioned in
-- parameter lists that are modified by the instruction.
--
-- To do this, we generate a bunch of randomized operand lists to cycle through
-- possible registers.
--
-- For this, we will want to focus on generating values that trigger edge cases
-- to make sure we can deal with flags registers.
findImplicitOperands :: (Architecture arch)
                     => Opcode arch (Operand arch) sh
                     -> M t arch (IORelation arch sh)
findImplicitOperands = undefined

-- | Given a bundle of tests, wrap all of the contained raw test cases with nonces.
wrapTestBundle :: (Architecture arch, R.MachineState (ArchState (Sym t) arch))
               => Instruction arch
               -> TestBundle (ArchState (Sym t) arch) arch
               -> M t arch (TestBundle (R.TestCase (ArchState (Sym t) arch)) arch)
wrapTestBundle i tb = do
  orig <- makeTestCase i (tbTestOrig tb)
  cases <- mapM (makeTestCase i) (tbTestCases tb)
  return TestBundle { tbTestOrig = orig
                    , tbTestCases = cases
                    , tbResult = tbResult tb
                    }

-- | Take a test bundle of raw tests ('ArchState (Sym t) arch') and convert the
-- raw tests to 'R.TestCase' by allocating a nonce
makeTestCase :: (Architecture arch, R.MachineState (ArchState (Sym t) arch))
             => Instruction arch
             -> ArchState (Sym t) arch
             -> M t arch (R.TestCase (ArchState (Sym t) arch))
makeTestCase i c = do
  tid <- St.gets nonce
  asm <- St.gets assemble
  St.modify' $ \s -> s { nonce = nonce s + 1 }
  return R.TestCase { R.testNonce = tid
                    , R.testContext = c
                    , R.testProgram = asm i
                    }


-- | For all of the explicit operands, map the results of tests to deductions
-- (forming an 'IORelation')
computeIORelation :: (Architecture arch)
                  => Opcode arch (Operand arch) sh
                  -> D.OperandList (Operand arch) sh
                  -> [TestBundle (R.TestCase (ArchState (Sym t) arch)) arch]
                  -> [R.ResultOrError (ArchState (Sym t) arch)]
                  -> M t arch (IORelation arch sh)
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
buildIORelation :: forall arch t sh
                 . (Architecture arch)
                => Opcode arch (Operand arch) sh
                -> D.OperandList (Operand arch) sh
                -> ResultIndex (ArchState (Sym t) arch)
                -> IORelation arch sh
                -> TestBundle (R.TestCase (ArchState (Sym t) arch)) arch
                -> M t arch (IORelation arch sh)
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
        Learned { lIndex = ix, lOpcode = lop }
          | Just P.Refl <- P.testEquality op lop ->
            let newRel = IORelation { inputs = [ OperandRef (Some ix) ]
                                    , outputs = map OperandRef (F.toList explicitOutputLocs)
                                    }
            in return (iorel <> newRel)
          | otherwise -> L.error ("Opcode mismatch: expected " ++ P.showF op ++ " but got " ++ P.showF lop)
  where
    explicitLocs = instructionRegisterOperands (Proxy :: Proxy arch) explicitOperands

-- | For the given test case, look up the results and compare them to the input
--
-- If the test failed, return an empty set.
collectExplicitLocations :: (Architecture arch)
                         => D.OperandList (Operand arch) sh
                         -> [Some (PairF (D.Index sh) (TypedLocation arch))]
                         -> ResultIndex (ArchState (Sym t) arch)
                         -> R.TestCase (ArchState (Sym t) arch)
                         -> M t arch (S.Set (Some (D.Index sh)))
collectExplicitLocations _opList explicitLocs ri tc = do
  case M.lookup (R.testNonce tc) (riSuccesses ri) of
    Nothing -> return S.empty
    Just res -> F.foldrM (addLocIfDifferent (R.resultContext res)) S.empty explicitLocs
  where
    addLocIfDifferent resCtx (Some (PairF idx (TL opLoc))) s
      | Just output <- MapF.lookup opLoc resCtx
      , Just input <- MapF.lookup opLoc (R.testContext tc) =
          case input /= output of
            True -> return (S.insert (Some idx) s)
            False -> return s
      | otherwise = L.error ("Missing location in architecture state: " ++ P.showF opLoc)

indexResults :: ResultIndex a -> R.ResultOrError a -> ResultIndex a
indexResults ri res =
  case res of
    R.TestReadError {} -> ri
    R.TestSignalError trNonce trSignum ->
      ri { riExitedWithSignal = M.insert trNonce trSignum (riExitedWithSignal ri) }
    R.TestContextParseFailure -> ri
    R.InvalidTag {} -> ri
    R.TestSuccess tr ->
      ri { riSuccesses = M.insert (R.resultNonce tr) tr (riSuccesses ri) }

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
generateTestVariants :: forall arch t
                      . (Architecture arch)
                     => Instruction arch
                     -> ArchState (Sym t) arch
                     -> M t arch [TestBundle (ArchState (Sym t) arch) arch]
generateTestVariants i s0 =
  case i of
    D.Instruction opcode operands -> do
      mapM (genVar opcode) (instructionRegisterOperands (Proxy :: Proxy arch) operands)
  where
    genVar :: forall sh
            . Opcode arch (Operand arch) sh
           -> Some (PairF (D.Index sh) (TypedLocation arch))
           -> M t arch (TestBundle (ArchState (Sym t) arch) arch)
    genVar opcode (Some (PairF ix (TL loc))) = do
      cases <- generateVariantsFor s0 opcode ix loc
      return TestBundle { tbTestOrig = s0
                        , tbTestCases = cases
                        , tbResult = Learned { lOpcode = opcode
                                             , lIndex = ix
                                             , lLocation = loc
                                             , lInstruction = i
                                             }
                        }

-- FIXME: For each test variant, build a new structure that tells us what we
-- learn if there is a difference from the original.  We'll need to map those to
-- nonces to compare against the results we get back.
--
-- To learn implicit operands, we need the list of all (register) locations for
-- the architecture.  We won't deal with implicit memory locations


-- | Tweak the value in the 'ArchState' at the given location to a number of
-- random values.
--
-- This has to be in IO so that we can generate 'S.SymExpr's
--
-- Right now, we only support generating random bitvectors.  That will get more
-- interesting once we start dealing with floats.  Note that we could just
-- generate random bitvectors for floats, too, but we would probably want to
-- tweak the distribution to generate interesting types of floats.
generateVariantsFor :: (Architecture arch)
                    => ArchState (Sym t) arch
                    -> Opcode arch (Operand arch) sh
                    -> D.Index sh tp
                    -> Location arch (OperandType arch tp)
                    -> M t arch [ArchState (Sym t) arch]
generateVariantsFor s0 _opcode _ix loc = do
  sym <- St.gets backend
  g <- St.gets gen
  replicateM 5 (genOne sym g)
  where
    genOne sym g =
      case locationType loc of
        S.BaseBVRepr w -> do
          randomInt :: Int
                    <- liftIO (A.uniform g)
          bv <- liftIO $ S.bvLit sym w (fromIntegral randomInt)
          return (MapF.insert loc bv s0)
        repr -> error ("Unsupported base type repr in generateVariantsFor: " ++ show repr)

-- | This is a newtype to shuffle type arguments around so that the 'tp'
-- parameter is last (so that we can use it with PairF and Some)
newtype TypedLocation arch tp = TL (Location arch (OperandType arch tp))

data PairF a b tp = PairF (a tp) (b tp)

instructionRegisterOperands :: forall arch sh proxy
                             . (Architecture arch)
                            => proxy arch
                            -> D.OperandList (Operand arch) sh
                            -> [Some (PairF (D.Index sh) (TypedLocation arch))]
instructionRegisterOperands proxy operands =
  D.foldrOperandList collectLocations [] operands
  where
    collectLocations :: forall tp . D.Index sh tp
                     -> Operand arch tp
                     -> [Some (PairF (D.Index sh) (TypedLocation arch))]
                     -> [Some (PairF (D.Index sh) (TypedLocation arch))]
    collectLocations ix operand acc =
      case operandToLocation proxy operand of
        Just loc -> Some (PairF ix (TL loc)) : acc
        Nothing -> acc

{-

We want to generate tests to determine, for each register operand, if it is
input, output, or both.

We'll start off with a single initial register state passed to
generateTestVariants.  All of the variants will be derived from that state.

We need to walk down the operand list and, for each register operand (r0),
generate a set of new states with that register (r0) value tweaked.  For those
nonces, if other registers change in the post state, r0 was an input register.
Registers that change values in the post state are outputs.  If registers that
are not mentioned in the operand list change, they are implicit outputs.  If
changing a register not in the operand list causes a change in outputs, it is an
implicit input.


-}


