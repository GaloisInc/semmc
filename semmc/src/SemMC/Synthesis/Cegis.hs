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
import           Data.Parameterized.Some ( Some(..), viewSome, mapSome, traverseSome )
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Classes as P
import           GHC.TypeNats (KnownNat)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Proxy (Proxy(..))
import           Debug.Trace (trace)

import qualified Lang.Crucible.Backend.Online as CBO
import qualified What4.Interface as S
import           What4.SatResult
import           What4.Expr.GroundEval
import qualified What4.Expr as WE
import qualified What4.Protocol.Online as WPO
import qualified What4.Symbol as WS

import           Dismantle.Instruction

import           SemMC.Architecture
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula
import           SemMC.Synthesis.Template
import           SemMC.Formula.MemAccesses

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
evalExpression' sym vars state expr = 
  replaceLitVars sym (lookupInState sym state) vars expr

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

-- Returns a 'LocExprs' data structure that maps all locations to a default value.
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
-- concrete test, of the form 
-- > e[i ↦ testInput(i)] = testOutput(l)]
-- What this means is all of the machine state variables have
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
buildEqualityLocation :: (IsLocation loc, P.ShowF loc)
                      => WE.ExprBuilder t st fs
                      -> ConcreteTest' (WE.ExprBuilder t st fs) loc
                      -> MapF.MapF loc (WE.ExprBoundVar t)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> loc tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (WE.Expr t tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> IO (WE.Expr t tp, WE.Expr t tp)
buildEqualityLocation sym test vars outputLoc expr = do
  actuallyIs <- case expr of
                  Just expr' -> evalExpression' sym vars (testInput test) expr'
                  -- If this location isn't present in the definitions of the
                  -- candidate formula, then its original value is preserved.
                  Nothing -> liftIO $ lookupInState sym (testInput test) outputLoc
  shouldBe <- liftIO $ lookupInState sym (testOutput test) outputLoc
--  liftIO . putStrLn $ "Built equation for location " ++ P.showF outputLoc 
--  liftIO . putStrLn $ "FOR test " ++ show test
--  liftIO . putStrLn $ "WITH expression " ++ show expr
--  liftIO . putStrLn $ "ACTUALLY IS:\t" ++ show actuallyIs
--  liftIO . putStrLn $ "SHOULD BE:\t" ++ show shouldBe
  return (actuallyIs, shouldBe)


-- | Build a conjuction of the equality expressions for all registers given by
-- the input list of locations.
buildEqualityLocations :: forall arch t st fs
                       . (Architecture arch)
                       => WE.ExprBuilder t st fs
                       ->Formula (WE.ExprBuilder t st fs) arch
                       -> ConcreteTest (WE.ExprBuilder t st fs) arch
                       -> Set.Set (Some (Location arch))
                       -> IO (WE.BoolExpr t)
buildEqualityLocations sym (Formula vars defs) test locs = do
  let addEquality :: WE.BoolExpr t -> Some (Location arch) -> IO (WE.BoolExpr t)
      addEquality soFar (Some loc) = do
        let locDef = MapF.lookup loc defs
        (actuallyIs,shouldBe) <- buildEqualityLocation sym test vars loc locDef
        locEquality <- liftIO $ S.isEq sym actuallyIs shouldBe
        liftIO $ S.andPred sym soFar locEquality
  foldlM addEquality (S.truePred sym) locs

{-

-- | Build a conjuction of the equality expressions for all the
-- indices/locations in memory touched by the input formula
buildEqualityMem :: forall arch t st fs
                 . (Architecture arch)
                 => Formula (WE.ExprBuilder t st fs) arch
                 -> ConcreteTest (WE.ExprBuilder t st fs) arch
                 -> Maybe (L.MemLoc (L.Location arch))
                 -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
buildEqualityMem _ _ Nothing = do sym <- askSym
                                  return $ S.truePred sym
buildEqualityMem f@(Formula vars defs) test (Just (L.MemLoc w mem)) = do
  sym <- askSym
  let locDef = MapF.lookup mem defs
  (actualMem,targetMem) <- buildEqualityLocation test vars mem locDef
  liftIO . putStrLn $ "ACTUALLY IS:\t" ++ show actualMem
  liftIO . putStrLn $ "SHOULD BE:\t" ++ show targetMem
  let indices = liveMemInExpr f actualMem `Set.union` liveMemInExpr f targetMem
  liftIO . putStrLn $ "INDICES: " ++ show indices
  let addEquality :: WE.BoolExpr t -> Integer -> IO (WE.BoolExpr t)
      addEquality soFar i = do
        iExp        <- S.bvLit sym w i
        actualMem_i <- S.arrayLookup sym actualMem (Ctx.Empty Ctx.:> iExp)
        targetMem_i <- S.arrayLookup sym targetMem (Ctx.Empty Ctx.:> iExp)
        liftIO . putStrLn $ "ACTUALLY IS[" ++ show i ++ "]:\t" ++ show actualMem_i
        liftIO . putStrLn $ "SHOULD BE[" ++ show i ++ "]:\t" ++ show targetMem_i
        locEquality <- S.isEq sym actualMem_i targetMem_i
--        liftIO . putStrLn $ "actually=should:\t" ++ show locEquality
        S.andPred sym soFar locEquality
  liftIO $ foldlM addEquality (S.truePred sym) indices
-}
buildEqualityMem :: forall arch t st fs sym
                 . (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                 => sym
                 -> Formula sym arch
                 -> ConcreteTest sym arch
                 -> Maybe (L.MemLoc (L.Location arch))
                 -> IO (WE.BoolExpr t)
buildEqualityMem sym _ _ Nothing = return $ S.truePred sym
buildEqualityMem sym f@(Formula vars defs) test (Just (L.MemLoc w mem)) = do
  let locDef = MapF.lookup mem defs
--  putStrLn $ "\n\nTEST: " ++ show test
--  putStrLn $ "Formula: " ++ show f
--  putStrLn $ "LocDef: " ++ show locDef
  (actualMem,targetMem) <- buildEqualityLocation sym test vars mem locDef
--  putStrLn $ "LocDef: " ++ show locDef
  (actualMem,targetMem) <- buildEqualityLocation sym test vars mem locDef
--  putStrLn $ "ACTUALLY IS:\t" ++ show actualMem
--  putStrLn $ "SHOULD BE:\t" ++ show targetMem

  res <- S.isEq sym actualMem targetMem
--  putStrLn $ "ACTUALLY IS = SHOULD BE: " ++ show res
  return res
--  WB.sbMakeExpr sym $ WE.ArrayEq actualMem targetMem

  where
    toBVExpr :: S.NatRepr w 
             -> Some (S.SymExpr sym) 
             -> IO (S.SymExpr sym (S.BaseBVType w))
    toBVExpr w0 (Some e) = case S.exprType e of
      S.BaseBVRepr w' -> case S.compareNat w0 w' of 
                           S.NatEQ -> return e
                           _       -> error $ "Memory index " ++ show e ++ " is not a bit vector of size " ++ show w0
      _ -> error $ "Memory index " ++ show e ++ " is not a bit vector of size " ++ show w0





partitionLocs' :: forall arch t st fs
                . Architecture arch
               => Formula (WE.ExprBuilder t st fs) arch
               -> ConcreteTest (WE.ExprBuilder t st fs) arch
               -> ( Set.Set (Some (Location arch))
                 , Maybe (L.MemLoc (L.Location arch)))
partitionLocs' (Formula _ defs) test =
    let allOutputLocs = Set.fromList (MapF.keys (testOutput test)) `Set.union`
                         Set.fromList (MapF.keys defs)
    in partitionLocs @arch allOutputLocs

{-
-- | Given a formula and a concrete test, return (1) the set of non-memory
-- locations touched by the formula and test; (2) the location referring to
-- memory
--
-- Unfortunately, the xs type is escaping its scope.
partitionLocs :: forall arch t st fs
               . Architecture arch
              => Formula (WE.ExprBuilder t st fs) arch
              -> ConcreteTest (WE.ExprBuilder t st fs) arch
              -> ( Set.Set (Some (Location arch))
                 , Maybe (Some (MemLoc arch)))
partitionLocs (Formula _ defs) test =
    let allOutputLocs = Set.fromList (MapF.keys (testOutput test)) `Set.union`
                        Set.fromList (MapF.keys defs)
        (memLocs, nonMemLocs) = Set.partition (\(Some loc) -> isMemoryLocation loc) allOutputLocs
        mem = Set.lookupMin memLocs -- memLocs should either be empty or a singleton set, so this should be deterministic
    in (nonMemLocs, mapSome toMemLoc <$> mem)
  where 
    toMemLoc :: Location arch tp -> MemLoc arch tp
    toMemLoc loc 
      | S.BaseArrayRepr (Ctx.Empty Ctx.:> S.BaseBVRepr w) _ <- locationType loc
        = MemLoc w loc
      | otherwise = error "The type of the memory Location in this architecture is unsupported"
-}

{-
data MemLoc arch ty where
  MemLoc :: 1 S.<= w
         => S.NatRepr w
         -> Location arch (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType w)) xs)
         -> MemLoc arch (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType w)) xs)
instance P.ShowF (Location arch) => Show (MemLoc arch ty) where 
  show (MemLoc _ l) = P.showF l
-}

-- | Build a conjuction of the equality expressions for /all/ locations in
-- either the outputs of the 'ConcreteTest' or the definitions of the 'Formula'.
-- That is, builds an equality expression of the form
-- > f.l1 (test_in.l1) = test_out.l1 /\ ... /\ (f.lm (test_in.lm) = test_out.lm)
-- for all locations li possibly touched by f or the test.
buildEqualityMachine :: forall arch t st fs
                      . (Architecture arch)
                     => WE.ExprBuilder t st fs
                     -> Formula (WE.ExprBuilder t st fs) arch
                     -> ConcreteTest (WE.ExprBuilder t st fs) arch
                     -> IO (WE.BoolExpr t)
buildEqualityMachine sym f test = do
  let (nonMemLocs, mem) = partitionLocs' f test
  nonMemPred <- buildEqualityLocations sym f test nonMemLocs
--  liftIO . putStrLn $ "Non-mem: " ++ show nonMemPred
  memPred <- buildEqualityMem sym f test mem
--  liftIO . putStrLn $ "Mem: " ++ show nonMemPred
  liftIO $ S.andPred sym nonMemPred memPred


-- | Build an equality of the form
-- > f(test1_in) = test1_out /\ f(test2_in) = test2_out /\ ... /\ f(testn_in) = testn_out
-- where f(test_in) = test_out is an "equality machine" as in 'buildEqualityMachine'.
buildEqualityTests :: forall arch t st fs
                    . (Architecture arch)
                   => Formula (WE.ExprBuilder t st fs) arch
                   -> [ConcreteTest (WE.ExprBuilder t st fs) arch]
                   -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
buildEqualityTests form tests = do
  sym <- askSym
  let andTest test soFar = do test1 <- buildEqualityMachine sym form test
                              liftIO $ S.andPred sym soFar test1
  liftIO $ foldrM andTest (S.truePred sym) tests


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
--   liftIO . putStrLn $ "Number of tests: " ++ show (length tests)
--   liftIO . putStrLn $ "\n\nTESTS:\n\t" ++ show tests
--  liftIO . putStrLn $ "\n\nTRIAL FORMULA:\n" ++ show trialFormula
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
--      liftIO . putStrLn $ "TRIAL INSTRUCTIONS:\n\t" ++ show insns'
      filledInFormula <- condenseInstructions insns'
      targetFormula <- askTarget
      equiv <- liftIO $ formulasEquivSym sym 
                                         (liveMemConst filledInFormula) 
                                         targetFormula 
                                         filledInFormula
      case equiv of
        -- FIXME: Is the correct behavior in a timeout to give up on this
        -- branch?
        Timeout -> do
          liftIO . putStrLn $ "Timeout"
          return (CegisUnmatchable tests)
        Equivalent -> do
          liftIO . putStrLn $ "Equivalent"
          return . CegisEquivalent $ map templInsnToDism insns'
        DifferentBehavior ctrExample -> do
          ctrExampleOut <- evalFormula targetFormula ctrExample
          let newTest = ConcreteTest' ctrExample ctrExampleOut
          liftIO . putStrLn $ "=============Added counterexample:=============== \n" ++ show newTest
          cegis' trial trialFormula (newTest : tests)
    Nothing -> return (CegisUnmatchable tests)






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


