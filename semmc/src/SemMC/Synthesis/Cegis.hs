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
  , ConcreteTest
  , CegisParams(..)
  , CegisResult(..)
  , cegis
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Reader ( ReaderT(..), reader )
import           Data.Foldable
import           Data.Maybe ( fromJust )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.ShapedList ( ShapedList )
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableF
import qualified Data.Set as Set

import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SatResult
import qualified Lang.Crucible.Solver.SimpleBackend as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import qualified Lang.Crucible.Solver.SimpleBuilder as S

import           Data.Parameterized.Witness ( Witness(..) )
import           Dismantle.Instruction

import           SemMC.Architecture
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula
import           SemMC.Synthesis.Template

-- | This is exactly a Dismantle 'Instruction', just with the dictionary of
-- constraints of a templatable opcode available.
data TemplatableInstruction (arch :: *) where
  TemplatableInstruction :: TemplatableOpcode arch sh
                         -> ShapedList (Operand arch) sh
                         -> TemplatableInstruction arch

-- | Disregard the constraints.
templInsnToDism :: TemplatableInstruction arch -> Instruction arch
templInsnToDism (TemplatableInstruction (Witness op) oplist) = Instruction op oplist

type LocExprs sym loc = MapF.MapF loc (S.SymExpr sym)

-- | Look up the given location in the given machine state. If the location is
-- not found, return the default value for that location.
lookupInState :: forall sym loc tp.
                 (S.IsExprBuilder sym,
                  IsLocation loc)
              => sym
              -> LocExprs sym loc
              -> loc tp
              -> IO (S.SymExpr sym tp)
lookupInState sym st loc =
  maybe (defaultLocationExpr sym loc) return $ MapF.lookup loc st

-- | Parameters given to call cegis.
data CegisParams sym arch =
  CegisParams { cpSym :: sym
              -- ^ The symbolic expression builder.
              , cpBaseSet :: BaseSet sym arch
              -- ^ The base set of opcode semantics.
              , cpTarget :: Formula sym arch
              -- ^ The target formula we're trying to match.
              }

type Cegis sym arch = ReaderT (CegisParams sym arch) IO

askSym :: Cegis sym arch sym
askSym = reader cpSym

askBaseSet :: Cegis sym arch (BaseSet sym arch)
askBaseSet = reader cpBaseSet

askTarget :: Cegis sym arch (Formula sym arch)
askTarget = reader cpTarget

-- | Evaluate an expression, substituting in the location values given in the
-- machine state.
evalExpression :: (IsLocation loc)
               => MapF.MapF loc (S.SimpleBoundVar t)
               -> LocExprs (S.SimpleBuilder t st) loc
               -> S.Elt t tp
               -> Cegis (S.SimpleBuilder t st) arch (S.Elt t tp)
evalExpression vars state expr = do
  sym <- askSym
  liftIO $ replaceLitVars sym (lookupInState sym state) vars expr

-- | Evaluate a whole formula, substituting in the location values given in the
-- machine state, returning the transformed machine state.
evalFormula :: (Architecture arch)
            => Formula (S.SimpleBuilder t st) arch
            -> L.ArchState arch (S.Elt t)
            -> Cegis (S.SimpleBuilder t st) arch (L.ArchState arch (S.Elt t))
evalFormula (Formula vars defs) input =
  traverseF (evalExpression vars input) defs

-- | Concrete input and output states of a formula. There's nothing in the types
-- that prevents the values from being symbolic, but please don't let them be!
data ConcreteTest' sym loc =
  ConcreteTest' { testInput :: LocExprs sym loc
                , testOutput :: LocExprs sym loc
                }

type ConcreteTest sym arch = ConcreteTest' sym (Location arch)

-- | Build an equality expression for the given location, under the given
-- concrete test. What this means is all of the machine state variables have
-- been filled in, and the resulting expression is set equal to the known "real"
-- output value.
--
-- For example (simplified): if the ConcreteTest is
-- @
--   ConcreteTest { testInput = { r2 -> 5, r3 -> 7 }
--                , testOutput = { r2 -> 5, r3 -> 10 }
--                }
-- @
-- , the location is @r3@, and the expression is @Just (3*r2 + imm5)@, this
-- function will generate the expression @3*5 + imm5 = 10@. If the expression
-- were instead @Nothing@, this would generate @7 = 10@.
buildEqualityLocation :: (IsLocation loc)
                      => ConcreteTest' (S.SimpleBuilder t st) loc
                      -> MapF.MapF loc (S.SimpleBoundVar t)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> loc tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (S.Elt t tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> Cegis (S.SimpleBuilder t st) arch (S.BoolElt t)
buildEqualityLocation test vars outputLoc expr = do
  sym <- askSym
  actuallyIs <- case expr of
                  Just expr' -> evalExpression vars (testInput test) expr'
                  -- If this location isn't present in the definitions of the
                  -- candidate formula, then its original value is preserved.
                  Nothing -> liftIO $ lookupInState sym (testInput test) outputLoc
  shouldBe <- liftIO $ lookupInState sym (testOutput test) outputLoc
  liftIO $ S.isEq sym actuallyIs shouldBe

-- | Build a conjuction of the equality expressions for /all/ locations in
-- either the outputs of the 'ConcreteTest' or the definitions of the 'Formula'.
buildEqualityMachine :: forall arch t st
                      . (Architecture arch)
                     => Formula (S.SimpleBuilder t st) arch
                     -> ConcreteTest (S.SimpleBuilder t st) arch
                     -> Cegis (S.SimpleBuilder t st) arch (S.BoolElt t)
buildEqualityMachine (Formula vars defs) test = do
  sym <- askSym
  let allOutputLocs = Set.fromList (MapF.keys (testOutput test)) `Set.union`
                      Set.fromList (MapF.keys defs)
      addEquality :: S.BoolElt t -> Some (Location arch) -> Cegis (S.SimpleBuilder t st) arch (S.BoolElt t)
      addEquality soFar (Some loc) = do
        let locDef = MapF.lookup loc defs
        locEquality <- buildEqualityLocation test vars loc locDef
        liftIO $ S.andPred sym soFar locEquality
  foldlM addEquality (S.truePred sym) allOutputLocs

-- | Build an equality of the form
-- > f(test1_in) = test1_out /\ f(test2_in) = test2_out /\ ... /\ f(testn_in) = testn_out
buildEqualityTests :: forall arch t st
                    . (Architecture arch)
                   => Formula (S.SimpleBuilder t st) arch
                   -> [ConcreteTest (S.SimpleBuilder t st) arch]
                   -> Cegis (S.SimpleBuilder t st) arch (S.BoolElt t)
buildEqualityTests form tests = do
  sym <- askSym
  let andTest test soFar = (liftIO . S.andPred sym soFar) =<< buildEqualityMachine form test
  foldrM andTest (S.truePred sym) tests

-- | Given a concrete model from the SMT solver, extract concrete instructions
-- from the templated instructions, so that all of the initially templated
-- operands are filled in concretely.
extractConcreteInstructions :: GroundEvalFn t
                            -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
                            -> IO [TemplatableInstruction arch]
extractConcreteInstructions (GroundEvalFn evalFn) = mapM f
  where f (TemplatedInstructionFormula (TemplatedInstruction op _ _) tf) =
          TemplatableInstruction (Witness op) <$> recoverOperands evalFn (tfOperandExprs tf)

-- | Meant to be used as the callback in a check SAT operation. If the result is
-- Sat, it pulls out concrete instructions corresponding to the SAT model.
-- Otherwise, it returns Nothing.
tryExtractingConcrete :: [TemplatedInstructionFormula (S.SimpleBackend t) arch]
                      -> SatResult (GroundEvalFn t, Maybe (EltRangeBindings t))
                      -> IO (Maybe [TemplatableInstruction arch])
tryExtractingConcrete insns (Sat (evalFn, _)) = Just <$> extractConcreteInstructions evalFn insns
tryExtractingConcrete _ Unsat = return Nothing
tryExtractingConcrete _ Unknown = fail "got Unknown when checking sat-ness"

-- | Build a formula for the given concrete instruction.
instantiateFormula' :: (Architecture arch)
                    => TemplatableInstruction arch
                    -> Cegis (S.SimpleBuilder t st) arch (Formula (S.SimpleBuilder t st) arch)
instantiateFormula' (TemplatableInstruction op oplist) = do
  sym <- askSym
  baseSet <- askBaseSet
  let pf = unTemplate . fromJust $ MapF.lookup op baseSet
  liftIO (snd <$> instantiateFormula sym pf oplist)

-- | Condense a series of instructions in sequential execution into one formula.
condenseInstructions :: (Architecture arch)
                     => [TemplatableInstruction arch]
                     -> Cegis (S.SimpleBuilder t st) arch (Formula (S.SimpleBuilder t st) arch)
condenseInstructions insns = do
  sym <- askSym
  insnFormulas <- traverse instantiateFormula' insns
  liftIO $ condenseFormulas sym insnFormulas

data CegisResult sym arch = CegisUnmatchable [ConcreteTest sym arch]
                          -- ^ There is no way to make the target and the
                          -- candidate do the same thing. This is proven by the
                          -- set of tests.
                          | CegisEquivalent [Instruction arch]
                          -- ^ This series of instructions, an instantiated form
                          -- of the candidate instructions given, has the same
                          -- behavior as the target formula.

cegis' :: (Architecture arch)
       => [TemplatedInstructionFormula (S.SimpleBackend t) arch]
       -- ^ The trial instructions.
       -> Formula (S.SimpleBackend t) arch
       -- ^ A formula representing the sequence of trial instructions.
       -> [ConcreteTest (S.SimpleBackend t) arch]
       -- ^ All the tests we have so far.
       -> Cegis (S.SimpleBackend t) arch (CegisResult (S.SimpleBackend t) arch)
cegis' trial trialFormula tests = do
  sym <- askSym
  -- Is this candidate satisfiable for the concrete tests we have so far? At
  -- this point, the machine state is concrete, but the immediate values of the
  -- instructions are symbolic. If the candidate is satisfiable for the tests,
  -- the SAT solver will give us values for the templated immediates in order to
  -- make the tests pass.
  check <- buildEqualityTests trialFormula tests
  insns <- liftIO $ checkSatZ3 sym check (tryExtractingConcrete trial)

  case insns of
    Just insns' -> do
      -- For the concrete immediate values that the solver just gave us, are the
      -- target formula and the concrete candidate instructions equivalent for
      -- all symbolic machine states?
      filledInFormula <- condenseInstructions insns'
      targetFormula <- askTarget
      equiv <- liftIO $ formulasEquivSym sym targetFormula filledInFormula
      case equiv of
        -- FIXME: Is the correct behavior in a timeout to give up on this
        -- branch?
        Timeout -> return (CegisUnmatchable tests)
        Equivalent -> return . CegisEquivalent $ map templInsnToDism insns'
        DifferentBehavior ctrExample -> do
          ctrExampleOut <- evalFormula targetFormula ctrExample
          let newTest = ConcreteTest' ctrExample ctrExampleOut
          cegis' trial trialFormula (newTest : tests)
    Nothing -> return (CegisUnmatchable tests)

cegis :: (Architecture arch)
      => CegisParams (S.SimpleBackend t) arch
      -- ^ Parameters not specific to the candidate. See 'CegisParams' for
      -- details.
      -> [ConcreteTest (S.SimpleBackend t) arch]
      -- ^ The tests we know so far for the target formula.
      -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
      -- ^ The candidate program.
      -> IO (CegisResult (S.SimpleBackend t) arch)
cegis params tests trial = do
  trialFormula <- condenseFormulas (cpSym params) (map tifFormula trial)
  runReaderT (cegis' trial trialFormula tests) params
