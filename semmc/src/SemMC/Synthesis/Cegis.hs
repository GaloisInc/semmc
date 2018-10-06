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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Synthesis.Cegis
  ( evalFormula
  , ConcreteTest
  , initTest
  , CegisParams(..)
  , CegisResult(..)
  , cegis
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Reader ( ReaderT(..), reader )
import           Data.Foldable
import           Data.Maybe ( fromJust )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.HasRepr as HR
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableF
import qualified Data.Set as Set

import qualified Lang.Crucible.Backend.Online as CBO
import qualified What4.Interface as S
import           What4.SatResult
import           What4.Expr.GroundEval
import qualified What4.Expr as WE
import qualified What4.Protocol.Online as WPO

import           Dismantle.Instruction

import           SemMC.Architecture
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula
import           SemMC.Synthesis.Template

-- | This is exactly a Dismantle 'Instruction', just with the dictionary of
-- constraints of a templatable opcode available.
data TemplatableInstruction (arch :: *) where
  TemplatableInstruction :: Opcode arch (Operand arch) sh
                         -> SL.List (Operand arch) sh
                         -> TemplatableInstruction arch

instance (MapF.ShowF (Operand arch), MapF.ShowF (Opcode arch (Operand arch)))
      => Show (TemplatableInstruction arch) where
  show (TemplatableInstruction opcode operand) = MapF.showF opcode ++ " " ++ show operand


-- | Disregard the constraints.
templInsnToDism :: TemplatableInstruction arch -> Instruction arch
templInsnToDism (TemplatableInstruction op oplist) = Instruction op oplist

-- | Note that ArchState arch (S.SymExpr sym) = LocExprs sym (Location arch)
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
evalExpression' :: (IsLocation loc)
               => WE.ExprBuilder t st fs
               -> MapF.MapF loc (WE.ExprBoundVar t)
               -> LocExprs (WE.ExprBuilder t st fs) loc
               -> WE.Expr t tp
               -> IO (WE.Expr t tp)
evalExpression' sym vars state expr = replaceLitVars sym (lookupInState sym state) vars expr

evalExpression :: (IsLocation loc)
               => MapF.MapF loc (WE.ExprBoundVar t)
               -> LocExprs (WE.ExprBuilder t st fs) loc
               -> WE.Expr t tp
               -> Cegis (WE.ExprBuilder t st fs) arch (WE.Expr t tp)
evalExpression vars state expr = do
  sym <- askSym
  liftIO $ evalExpression' sym vars state expr


-- | Evaluate a whole formula, substituting in the location values given in the
-- machine state, returning the transformed machine state.
evalFormula :: (Architecture arch)
            => Formula (WE.ExprBuilder t st fs) arch
            -> L.ArchState arch (WE.Expr t)
            -> Cegis (WE.ExprBuilder t st fs) arch (L.ArchState arch (WE.Expr t))
evalFormula (Formula vars defs) input = do
  traverseF (evalExpression vars input) defs

evalFormula' :: (Architecture arch)
             => WE.ExprBuilder t st fs
             -> Formula (WE.ExprBuilder t st fs) arch
             -> L.ArchState arch (WE.Expr t)
             -> IO (L.ArchState arch (WE.Expr t))
evalFormula' sym (Formula vars defs) input = traverseF (evalExpression' sym vars input) defs


-- | Concrete input and output states of a formula. There's nothing in the types
-- that prevents the values from being symbolic, but please don't let them be!
data ConcreteTest' sym loc =
  ConcreteTest' { testInput :: LocExprs sym loc
                , testOutput :: LocExprs sym loc
                }

type ConcreteTest sym arch = ConcreteTest' sym (Location arch)
instance (MapF.ShowF loc, MapF.ShowF (S.SymExpr sym)) 
      => Show (ConcreteTest' sym loc) 
  where
    show test = "⟨ " ++ show (testInput test) ++ " \t|||\t " ++ show (testOutput test) ++ " ⟩"

-- Returns a 'LocExprs' data structure that maps all locations to the constant 0.
defaultLocExprs :: forall loc sym. 
                ( L.IsLocation loc 
                , MapF.OrdF loc 
                , S.IsExprBuilder sym
                )
               => sym
               -> IO (LocExprs sym loc) -- MapF.MapF loc (S.SymExpr sym)
defaultLocExprs sym = do locExprList <- mapM pairDefault (L.allLocations @loc)
                         return $ MapF.fromList locExprList
  where
    pairDefault :: Some loc -> IO (MapF.Pair loc (S.SymExpr sym))
    pairDefault (Some l) = MapF.Pair l <$> L.defaultLocationExpr sym l


initTest :: (L.IsLocation (Location arch), Architecture arch)
         => WE.ExprBuilder t st fs
         -> Formula (WE.ExprBuilder t st fs) arch
         -> IO (ConcreteTest (WE.ExprBuilder t st fs) arch)
initTest sym f = do locExprs <- defaultLocExprs sym
                    res <- evalFormula' sym f locExprs 
                    return $ ConcreteTest' locExprs res


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
                      => ConcreteTest' (WE.ExprBuilder t st fs) loc
                      -> MapF.MapF loc (WE.ExprBoundVar t)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> loc tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (WE.Expr t tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
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
buildEqualityMachine :: forall arch t st fs
                      . (Architecture arch)
                     => Formula (WE.ExprBuilder t st fs) arch
                     -> ConcreteTest (WE.ExprBuilder t st fs) arch
                     -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
buildEqualityMachine (Formula vars defs) test = do
  sym <- askSym
  let allOutputLocs = Set.fromList (MapF.keys (testOutput test)) `Set.union`
                      Set.fromList (MapF.keys defs)
      addEquality :: WE.BoolExpr t -> Some (Location arch) -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
      addEquality soFar (Some loc) = do
        let locDef = MapF.lookup loc defs
        locEquality <- buildEqualityLocation test vars loc locDef
        liftIO $ S.andPred sym soFar locEquality
  foldlM addEquality (S.truePred sym) allOutputLocs

-- | Build an equality of the form
-- > f(test1_in) = test1_out /\ f(test2_in) = test2_out /\ ... /\ f(testn_in) = testn_out
buildEqualityTests :: forall arch t st fs
                    . (Architecture arch)
                   => Formula (WE.ExprBuilder t st fs) arch
                   -> [ConcreteTest (WE.ExprBuilder t st fs) arch]
                   -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
buildEqualityTests form tests = do
  sym <- askSym
  let andTest test soFar = (liftIO . S.andPred sym soFar) =<< buildEqualityMachine form test
  foldrM andTest (S.truePred sym) tests

-- | Given a concrete model from the SMT solver, extract concrete instructions
-- from the templated instructions, so that all of the initially templated
-- operands are filled in concretely.
extractConcreteInstructions :: (ArchRepr arch)
                            => GroundEvalFn t
                            -> [TemplatedInstructionFormula (WE.ExprBuilder t st fs) arch]
                            -> IO [TemplatableInstruction arch]
extractConcreteInstructions (GroundEvalFn evalFn) = mapM f
  where f (TemplatedInstructionFormula (TemplatedInstruction op _ _) tf) =
          TemplatableInstruction op <$> recoverOperands (HR.typeRepr op) evalFn (tfOperandExprs tf)

-- | Meant to be used as the callback in a check SAT operation. If the result is
-- Sat, it pulls out concrete instructions corresponding to the SAT model.
-- Otherwise, it returns Nothing.
tryExtractingConcrete :: (ArchRepr arch)
                      => [TemplatedInstructionFormula (WE.ExprBuilder t st fs) arch]
                      -> SatResult (GroundEvalFn t)
                      -> IO (Maybe [TemplatableInstruction arch])
tryExtractingConcrete insns (Sat evalFn) = Just <$> extractConcreteInstructions evalFn insns
tryExtractingConcrete _ Unsat = return Nothing
tryExtractingConcrete _ Unknown = fail "got Unknown when checking sat-ness"

-- | Build a formula for the given concrete instruction.
instantiateFormula' :: (Architecture arch
                       -- , WE.IsBoolSolver (WE.ExprBuilder t st)
                       )
                    => TemplatableInstruction arch
                    -> Cegis (WE.ExprBuilder t st fs) arch (Formula (WE.ExprBuilder t st fs) arch)
instantiateFormula' (TemplatableInstruction op oplist) = do
  sym <- askSym
  baseSet <- askBaseSet
  let pf = unTemplate . fromJust $ MapF.lookup op baseSet
  liftIO (snd <$> instantiateFormula sym pf oplist)

-- | Condense a series of instructions in sequential execution into one formula.
condenseInstructions :: (Architecture arch
                       -- , WE.IsBoolSolver (WE.ExprBuilder t st)
                       )
                     => [TemplatableInstruction arch]
                     -> Cegis (WE.ExprBuilder t st fs) arch (Formula (WE.ExprBuilder t st fs) arch)
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

cegis' :: (Architecture arch, ArchRepr arch, WPO.OnlineSolver t solver)
       => [TemplatedInstructionFormula (CBO.OnlineBackend t solver fs) arch]
       -- ^ The trial instructions.
       -> Formula (CBO.OnlineBackend t solver fs) arch
       -- ^ A formula representing the sequence of trial instructions.
       -> [ConcreteTest (CBO.OnlineBackend t solver fs) arch]
       -- ^ All the tests we have so far.
       -> Cegis (CBO.OnlineBackend t solver fs) arch (CegisResult (CBO.OnlineBackend t solver fs) arch)
cegis' trial trialFormula tests = do
  -- liftIO . putStrLn $ "Number of tests: " ++ show (length tests)
  liftIO . putStrLn $ "\n\nTESTS:\n\t" ++ show tests
  sym <- askSym
  -- Is this candidate satisfiable for the concrete tests we have so far? At
  -- this point, the machine state is concrete, but the immediate values of the
  -- instructions are symbolic. If the candidate is satisfiable for the tests,
  -- the SAT solver will give us values for the templated immediates in order to
  -- make the tests pass.
  check <- buildEqualityTests trialFormula tests
  insns <- liftIO $ checkSat sym check (tryExtractingConcrete trial)

  case insns of
    Just insns' -> do
      -- For the concrete immediate values that the solver just gave us, are the
      -- target formula and the concrete candidate instructions equivalent for
      -- all symbolic machine states?
      liftIO . putStrLn $ "TRIAL INSTRUCTIONS:\n\t" ++ show insns'
      filledInFormula <- condenseInstructions insns'
      targetFormula <- askTarget
      equiv <- liftIO $ formulasEquivSym sym 
                                         (getMemAccesses filledInFormula) 
                                         targetFormula 
                                         filledInFormula
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

-- | Returns a list of memory locations (as bit vectors) that are accessed or
-- written to in a formula.
-- 
-- TODO: what should the dimensions of the bit-vector be?
getMemAccesses :: Formula sym arch
               -> [Integer]
getMemAccesses = undefined


cegis :: (Architecture arch, ArchRepr arch, WPO.OnlineSolver t solver)
      => CegisParams (CBO.OnlineBackend t solver fs) arch
      -- ^ Parameters not specific to the candidate. See 'CegisParams' for
      -- details.
      -> [ConcreteTest (CBO.OnlineBackend t solver fs) arch]
      -- ^ The tests we know so far for the target formula.
      -> [TemplatedInstructionFormula (CBO.OnlineBackend t solver fs) arch]
      -- ^ The candidate program.
      -> IO (CegisResult (CBO.OnlineBackend t solver fs) arch)
cegis params tests trial = do
  trialFormula <- condenseFormulas (cpSym params) (map tifFormula trial)
  runReaderT (cegis' trial trialFormula tests) params
