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
  , ConcreteTest
  ) where

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
import           SemMC.Formula
import           SemMC.Formula.Equivalence
import           SemMC.Formula.Instantiate
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

-- | Evaluate an expression, substituting in the location values given in the
-- machine state.
evalExpression :: (IsLocation loc)
               => S.SimpleBuilder t st
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> LocExprs (S.SimpleBuilder t st) loc
               -> S.Elt t tp
               -> IO (S.Elt t tp)
evalExpression sym vars state = replaceLitVars sym (lookupInState sym state) vars

-- | Evaluate a whole formula, substituting in the location values given in the
-- machine state, returning the transformed machine state.
evalFormula :: (Architecture arch)
            => S.SimpleBuilder t st
            -> Formula (S.SimpleBuilder t st) arch
            -> ArchState arch (S.Elt t)
            -> IO (ArchState arch (S.Elt t))
evalFormula sym (Formula vars defs) input =
  traverseF (evalExpression sym vars input) defs

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
                      => S.SimpleBuilder t st
                      -> ConcreteTest' (S.SimpleBuilder t st) loc
                      -> MapF.MapF loc (S.SimpleBoundVar t)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> loc tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (S.Elt t tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> IO (S.BoolElt t)
buildEqualityLocation sym test vars outputLoc expr = do
  actuallyIs <- case expr of
                  Just expr' -> evalExpression sym vars (testInput test) expr'
                  -- If this location isn't present in the definitions of the
                  -- candidate formula, then its original value is preserved.
                  Nothing -> lookupInState sym (testInput test) outputLoc
  shouldBe <- lookupInState sym (testOutput test) outputLoc
  S.isEq sym actuallyIs shouldBe

-- | Build a conjuction of the equality expressions for /all/ locations in
-- either the outputs of the 'ConcreteTest' or the definitions of the 'Formula'.
buildEqualityMachine :: forall arch t st
                      . (Architecture arch)
                     => S.SimpleBuilder t st
                     -> Formula (S.SimpleBuilder t st) arch
                     -> ConcreteTest (S.SimpleBuilder t st) arch
                     -> IO (S.BoolElt t)
buildEqualityMachine sym (Formula vars defs) test =
  foldlM addEquality (S.truePred sym) allOutputLocs
  where allOutputLocs = Set.fromList (MapF.keys (testOutput test)) `Set.union`
                        Set.fromList (MapF.keys defs)
        addEquality :: S.BoolElt t -> Some (Location arch) -> IO (S.BoolElt t)
        addEquality soFar (Some loc) = do
          let locDef = MapF.lookup loc defs
          locEquality <- buildEqualityLocation sym test vars loc locDef
          S.andPred sym soFar locEquality

-- | Build an equality of the form
-- > f(test1_in) = test1_out /\ f(test2_in) = test2_out /\ ... /\ f(testn_in) = testn_out
buildEqualityTests :: forall arch t st
                    . (Architecture arch)
                   => S.SimpleBuilder t st
                   -> Formula (S.SimpleBuilder t st) arch
                   -> [ConcreteTest (S.SimpleBuilder t st) arch]
                   -> IO (S.BoolElt t)
buildEqualityTests sym form = foldrM andTest (S.truePred sym)
  where andTest test soFar = S.andPred sym soFar =<< buildEqualityMachine sym form test

extractConcreteInstructions :: GroundEvalFn t
                            -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
                            -> IO [TemplatableInstruction arch]
extractConcreteInstructions (GroundEvalFn evalFn) = mapM f
  where f (TemplatedInstructionFormula (TemplatedInstruction op _ _) tf) =
          TemplatableInstruction (Witness op) <$> recoverOperands evalFn (tfOperandExprs tf)

-- TODO: rename
handleSatResult :: [TemplatedInstructionFormula (S.SimpleBackend t) arch]
                -> SatResult (GroundEvalFn t, Maybe (EltRangeBindings t))
                -> IO (Maybe [TemplatableInstruction arch])
handleSatResult insns (Sat (evalFn, _)) = Just <$> extractConcreteInstructions evalFn insns
handleSatResult _ Unsat = return Nothing
handleSatResult _ Unknown = fail "got Unknown when checking sat-ness"

-- | Build a formula for the given concrete instruction.
instantiateFormula' :: (Architecture arch)
                    => S.SimpleBuilder t st
                    -> BaseSet (S.SimpleBuilder t st) arch
                    -> TemplatableInstruction arch
                    -> IO (Formula (S.SimpleBuilder t st) arch)
instantiateFormula' sym m (TemplatableInstruction op oplist) =
  snd <$> instantiateFormula sym pf oplist
  where pf = unTemplate . fromJust $ MapF.lookup op m

-- TODO: tidy up this type signature
cegis :: (Architecture arch)
      => S.SimpleBackend t
      -> BaseSet (S.SimpleBackend t) arch
      -> Formula (S.SimpleBackend t) arch
      -> [ConcreteTest (S.SimpleBackend t) arch]
      -> [TemplatedInstructionFormula (S.SimpleBackend t) arch]
      -> IO (Either [ConcreteTest (S.SimpleBackend t) arch] [Instruction arch])
cegis sym semantics target tests trial = do
  trialFormula <- condenseFormulas sym (map tifFormula trial)
  check <- buildEqualityTests sym trialFormula tests

  -- Is this candidate satisfiable for the concrete tests we have so far? At
  -- this point, the machine state is concrete, but the immediate values of the
  -- instructions are symbolic.
  insns <- checkSatZ3 sym check (handleSatResult trial)

  case insns of
    Just insns' -> do
      -- For the concrete immediate values that the solver just gave us, are the
      -- target formula and the concrete candidate instructions equivalent for
      -- all symbolic machine states?
      filledInFormula <- condenseFormulas sym =<< mapM (instantiateFormula' sym semantics) insns'
      equiv <- formulasEquivSym sym target filledInFormula
      case equiv of
        Equivalent -> return . Right $ map templInsnToDism insns'
        Mismatching -> return (Left tests)
        DifferentBehavior ctrExample -> do
          ctrExampleOut <- evalFormula sym target ctrExample
          let newTest = ConcreteTest' ctrExample ctrExampleOut
          cegis sym semantics target (newTest : tests) trial
    Nothing -> return (Left tests)
