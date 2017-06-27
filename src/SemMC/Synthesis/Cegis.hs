{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module SemMC.Synthesis.Cegis
  ( evalFormula
  , cegis
  ) where

import           Data.Foldable
import           Data.Maybe ( fromJust )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.TraversableF
import           GHC.Stack ( HasCallStack )

import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SatResult
import qualified Lang.Crucible.Solver.SimpleBackend as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import qualified Lang.Crucible.Solver.SimpleBuilder as S

import           Dismantle.Instruction

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Formula.Equivalence
import           SemMC.Formula.Instantiate
import           SemMC.Synthesis.Template
-- import           SemMC.Util

data GoodInstruction (arch :: *) where
  GoodInstruction :: forall arch sh. OpcodeGoodShape (Opcode arch) (Operand arch) arch sh -> OperandList (Operand arch) sh -> GoodInstruction arch

ungood :: GoodInstruction arch -> Instruction arch
ungood (GoodInstruction (OpcodeGoodShape op) oplist) = Instruction op oplist

foldlMWithKey :: forall k a b m. (Monad m) => (forall s. b -> k s -> a s -> m b) -> b -> MapF.MapF k a -> m b
foldlMWithKey f z0 m = MapF.foldrWithKey f' return m z0
  where f' :: forall s. k s -> a s -> (b -> m b) -> b -> m b
        f' k x c z = f z k x >>= c

type MachineState sym loc = MapF.MapF loc (S.SymExpr sym)

lookupInState :: forall sym loc tp.
                 (S.IsExprBuilder sym,
                  IsLocation loc)
              => sym
              -> MachineState sym loc
              -> loc tp
              -> IO (S.SymExpr sym tp)
lookupInState sym st loc = maybe (defaultLocationExpr sym loc) return $ MapF.lookup loc st

evalFormula :: (Architecture arch)
            => S.SimpleBuilder t st
            -> Formula (S.SimpleBuilder t st) arch
            -> ArchState (S.SimpleBuilder t st) arch
            -> IO (ArchState (S.SimpleBuilder t st) arch)
evalFormula sym (Formula _ vars defs) input =
  traverseF (replaceLitVars sym (lookupInState sym input) vars) defs

-- XXX: make sure that all locations are actually checked correctly, i.e., in
-- the case where there are different locations in the state map than in the
-- defs map
buildEquality' :: (IsLocation loc)
               => S.SimpleBuilder t st
               -> (MachineState (S.SimpleBuilder t st) loc, MachineState (S.SimpleBuilder t st) loc)
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> loc tp
               -> S.Elt t tp
               -> IO (S.BoolElt t)
buildEquality' sym (input, output) vars outputLoc expr = do
  actuallyIs <- replaceLitVars sym (lookupInState sym input) vars expr
  let shouldBe = maybe (error "outputLoc wasn't in output state") id $ MapF.lookup outputLoc output
  S.isEq sym actuallyIs shouldBe

buildEquality :: forall arch t st.
                 (Architecture arch)
              => S.SimpleBuilder t st
              -> (ArchState (S.SimpleBuilder t st) arch, ArchState (S.SimpleBuilder t st) arch)
              -> Formula (S.SimpleBuilder t st) arch
              -> IO (S.BoolElt t)
buildEquality sym test (Formula _ vars defs) = foldlMWithKey f (S.truePred sym) defs
  where f :: S.BoolElt t -> Location arch tp -> S.Elt t tp -> IO (S.BoolElt t)
        f b loc e = (S.andPred sym b =<< buildEquality' sym test vars loc e)

handleSat :: GroundEvalFn t
          -> [InstructionWTFormula (S.SimpleBackend t) arch]
          -> IO [GoodInstruction arch]
handleSat evalFn = mapM f
  where f (InstructionWTFormula op tf) = GoodInstruction (OpcodeGoodShape op) <$> recoverOperands evalFn (tfOperandList tf) (tfOperandExprs tf)

handleSatResult :: [InstructionWTFormula (S.SimpleBackend t) arch]
                -> SatResult (GroundEvalFn t, Maybe (EltRangeBindings t))
                -> IO (Maybe [GoodInstruction arch])
handleSatResult insns (Sat (evalFn, _)) = Just <$> handleSat evalFn insns
handleSatResult _ Unsat = return Nothing
handleSatResult _ Unknown = fail "got Unknown when checking sat-ness"

instantiateFormula' :: (Architecture arch, HasCallStack)
                    => S.SimpleBuilder t st
                    -> MapF.MapF (OpcodeGoodShape (Opcode arch) (Operand arch) arch) (ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch))
                    -> GoodInstruction arch
                    -> IO (Formula (S.SimpleBuilder t st) arch)
instantiateFormula' sym m (GoodInstruction op oplist) =
  snd <$> instantiateFormula sym (unTemplate . fromJust $ MapF.lookup op m) oplist

condenseFormula :: forall t st arch.
                   (Architecture arch)
                => S.SimpleBuilder t st
                -> [Formula (S.SimpleBuilder t st) arch]
                -> IO (Formula (S.SimpleBuilder t st) arch)
condenseFormula sym = foldrM (sequenceFormulas sym) emptyFormula

type TestCases sym arch = [(ArchState sym arch, ArchState sym arch)]

-- TODO: tidy up this type signature
-- TODO: return new test cases in the case of failure
cegis :: (Architecture arch)
      => S.SimpleBackend t
      -> MapF.MapF (OpcodeGoodShape (Opcode arch) (Operand arch) arch) (ParameterizedFormula (S.SimpleBackend t) (TemplatedArch arch))
      -> Formula (S.SimpleBackend t) arch
      -> TestCases (S.SimpleBackend t) arch
      -> [InstructionWTFormula (S.SimpleBackend t) arch]
      -> Formula (S.SimpleBackend t) arch
      -> IO (Either (TestCases (S.SimpleBackend t) arch) [Instruction arch])
cegis sym semantics target tests trial trialFormula = do
  -- initial dumb thing: return Just [] if all the tests are satisfiable
  check <- foldrM (\test b -> S.andPred sym b =<< buildEquality sym test trialFormula) (S.truePred sym) tests

  insns <- checkSatZ3 sym check (handleSatResult trial)

  case insns of
    Just insns' -> do
      filledInFormula <- condenseFormula sym =<< mapM (instantiateFormula' sym semantics) insns'
      equiv <- formulasEquiv sym target filledInFormula
      case equiv of
        Equivalent -> return . Right $ map ungood insns'
        Mismatching -> return (Left tests)
        DifferentBehavior ctrExample -> do
          ctrExampleOut <- evalFormula sym target ctrExample
          cegis sym semantics target ((ctrExample, ctrExampleOut) : tests) trial trialFormula
    Nothing -> return (Left tests)
