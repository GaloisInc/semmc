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
{-# LANGUAGE LambdaCase #-}
module SemMC.Synthesis.Cegis
  ( evalFormula
  , ConcreteTest
  , initTest
  , CegisParams(..)
  , CegisResult(..)
  , cegis
  ) where

import           Control.Monad.IO.Class ( liftIO, MonadIO )
import           Control.Monad.Trans.Reader ( ReaderT(..), reader )
import           Data.Foldable
import           Data.Maybe ( fromJust, listToMaybe )
import           Data.Kind
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.HasRepr as HR
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableF
import qualified Data.Parameterized.Classes as P
import qualified Data.Set as Set

import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Backend as CB
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
import           SemMC.Formula.MemAccesses
import qualified SemMC.Formula.LLVMMem as LLVM

-- | This is exactly a Dismantle 'Instruction', just with the dictionary of
-- constraints of a templatable opcode available.
data TemplatableInstruction (arch :: Type) where
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
              , cpSemantics :: TemplatedSemantics sym arch
              -- ^ The base set of opcode semantics.
              , cpTarget :: Formula sym arch
              -- ^ The target formula we're trying to match.
              , cpUFEnv :: FormulaEnv sym arch
              -- ^ The uninterpreted functions in scope
              }

type Cegis sym arch = ReaderT (CegisParams sym arch) IO

askSym :: Cegis sym arch sym
askSym = reader cpSym

askUF :: Cegis sym arch (FormulaEnv sym arch)
askUF = reader cpUFEnv

askSemantics :: Cegis sym arch (TemplatedSemantics sym arch)
askSemantics = reader cpSemantics

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
data ConcreteTest' sym arch =
  ConcreteTest' { testInput  :: LocExprs sym (Location arch)
                , testOutput :: LocExprs sym (Location arch)
                , memInput   :: [AccessData sym arch]
                , memOutput  :: [AccessData sym arch]
--                , defaultMem :: Some (S.SymExpr sym)
                }

-- nonMemTestInput :: P.OrdF loc
--                => L.MemLoc loc -> ConcreteTest' sym loc -> LocExprs sym loc
--nonMemTestInput (L.MemLoc _ mem) test = MapF.delete mem $ testInput test

type ConcreteTest sym arch = ConcreteTest' sym arch
instance (Architecture arch, P.ShowF (S.SymExpr sym))
      => Show (ConcreteTest' sym arch)
  where
    show test = "⟨\t" ++ show (testInput test)
                ++ "\n|||\t" ++ show (testOutput test)
                ++ "\n|||\t" ++ show (memInput test) 
                ++ "\n|||\t" ++ show (memOutput test) ++ "\n⟩"

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


-- | Given a formula and a counterexample provided from the solver, construct
-- the concrete test illustrated by the counterexample.
--
-- TODO: adapt this to deal with the case when the architecture has several memory locations (e.g. A32)
mkTest :: forall sym arch t st fs.
          (Architecture arch, sym ~ WE.ExprBuilder t st fs)
       => sym
       -> Formula sym arch
       -> L.ArchState arch (S.SymExpr sym)
       -> IO (ConcreteTest sym arch)
mkTest sym targetFormula ctrExample
  | [L.MemLoc _ mem] <- memLocation @(L.Location arch) = do
    -- putStrLn $ "Constructing test from " ++ show ctrExample
    -- the testInput is exactly the counter example for non-memory locations
    let testInput' = MapF.delete mem ctrExample

    -- substitute the non-memory locations from the test into the target
    -- formula. For non-memory locations, this gives us the testOutput.
    ctrExampleOut <- evalFormula' sym targetFormula testInput'
    let testOutput' = MapF.delete mem ctrExampleOut

    -- to construct the memInput/memOutput, we need to find all memory locations that
    -- occur in the input/output counterexamples respectively and record the values they map to.
    let inputAddrMap  = liveMemMap $ Formula @sym @arch (formParamVars targetFormula) ctrExample
        outputAddrMap = liveMemMap $ Formula @sym @arch (formParamVars targetFormula) ctrExampleOut

    return $ ConcreteTest' testInput' testOutput'
                           (Set.toList inputAddrMap) (Set.toList outputAddrMap)
mkTest _ _ _ | otherwise = error "Cannot make test for this architecture"

-- | Construct an initial test from concrete values
initTest :: (L.IsLocation (Location arch), Architecture arch)
         => WE.ExprBuilder t st fs
         -> Formula (WE.ExprBuilder t st fs) arch
         -> IO (ConcreteTest (WE.ExprBuilder t st fs) arch)
initTest sym f = do locExprs <- defaultLocExprs sym
                    mkTest sym f locExprs


-- | Build an equality expression for the given non-memory location, under the given
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
buildEqualityLocation :: Architecture arch
                      => WE.ExprBuilder t st fs
                      -> ConcreteTest' (WE.ExprBuilder t st fs) arch
                      -> MapF.MapF (Location arch) (WE.ExprBoundVar t)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> Location arch tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (WE.Expr t tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> IO (WE.BoolExpr t)
buildEqualityLocation sym test _vars loc Nothing
  -- If the location does not appear in the test output, then the location is preserved
  | Nothing <- MapF.lookup loc (testOutput test) = return $ S.truePred sym

buildEqualityLocation sym test vars outputLoc expr = do
  actuallyIs <- case expr of
                  Just expr' -> evalExpression' sym vars (testInput test) expr'
                  -- If this location isn't present in the definitions of the
                  -- candidate formula, then its original value is preserved.
                  Nothing -> liftIO $ lookupInState sym (testInput test) outputLoc
  shouldBe <- liftIO $ lookupInState sym (testOutput test) outputLoc
  liftIO . putStrLn $ "Built equation for location " ++ P.showF outputLoc 
--  liftIO . putStrLn $ "FOR test " ++ show test
  liftIO . putStrLn $ "WITH expression " ++ show expr
  liftIO . putStrLn $ "ACTUALLY IS:\t" ++ show actuallyIs
  liftIO . putStrLn $ "SHOULD BE:\t" ++ show shouldBe
  liftIO $ S.isEq sym actuallyIs shouldBe


simplifyWithTestLLVM :: ( Architecture arch
                        , sym ~ WE.ExprBuilder t st fs
                        , CB.IsSymInterface sym)
                     => Formula sym arch
                     -> ConcreteTest sym arch
                     -> Cegis sym arch (WE.BoolExpr t)
simplifyWithTestLLVM f test = do
    liftIO . putStrLn $ "\nConstructing predicate for test " ++ show test ++ "\n"
    nonMemPred <- simplifyWithTestNonMem f test
    liftIO . putStrLn $ "\nNON-MEM PRED: " ++ show nonMemPred ++ "\n"
    memPred <- simplifyWithTestLLVMMem (formMem f) f test
    sym <- askSym
    res <- liftIO $ S.andPred sym nonMemPred memPred
--    liftIO . putStrLn $ "RESULT: " ++ show res ++ "\n"
    return res

-- | Get the @L.MemLoc@ associated with the formula
formMem :: Architecture arch
        => Formula sym arch 
        -> Maybe (L.MemLoc (L.Location arch))
formMem f | any (\(Some l) -> L.isMemLoc l) (MapF.keys $ formParamVars f) = listToMaybe L.memLocation
formMem _ | otherwise = Nothing


-- | Substitute test input (for non-memory locations) into the target formula,
-- producing a new formula f' such that the only free variables in f' are Mem
-- and any uninstantiated immediates to be generated. We then construct a
-- predicate of the form 
-- @\forall l <> Mem, f'(l) = testOutput(l)@
simplifyWithTestNonMem :: forall arch t st fs sym.
                         (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                      => Formula sym arch
                      -- ^ the target formula
                      -> ConcreteTest sym arch
                      -- ^ a single test case
                      -> Cegis sym arch (S.Pred sym)
simplifyWithTestNonMem trialFormula test = do
  sym <- askSym
  defs' <- liftIO $ evalFormula' sym trialFormula (testInput test)
  let nonMemLocs = fst . partitionLocs @arch $ formInputs  trialFormula 
                                   `Set.union` formOutputs trialFormula
  liftIO . putStrLn $ "Non-mem locs: " ++ show nonMemLocs
  liftIO . putStrLn $ "Formula: " ++ show trialFormula
  liftIO . putStrLn $ "  evaluated with respect to the test: " ++ show defs'
  liftIO $ andPred sym nonMemLocs $ \(Some l) ->
    buildEqualityLocation sym test vars l (MapF.lookup l defs')
  where
    vars = formParamVars trialFormula

-- | Take the conjunction of (f a) for each a in some foldable data structure
andPred :: forall t sym m a. (Foldable t, S.IsExprBuilder sym, MonadIO m)
        => sym -> t a -> (a -> m (S.Pred sym)) -> m (S.Pred sym)
andPred sym ls f = foldrM go (S.truePred sym) ls
  where
    go :: a -> S.Pred sym -> m (S.Pred sym)
    go accA accP = f accA >>= liftIO . (S.andPred sym accP)



simplifyWithTestLLVMMem :: forall t st fs sym arch.
                           ( Architecture arch
                           , sym ~ WE.ExprBuilder t st fs
                           , CB.IsSymInterface sym
                           )
                        => Maybe (L.MemLoc (L.Location arch))
                        -> Formula sym arch
                        -> ConcreteTest sym arch
                        -> Cegis sym arch (S.Pred sym)
-- if there is no memory, return True
simplifyWithTestLLVMMem Nothing _ _ = do
  sym <- askSym
  return $ S.truePred sym
simplifyWithTestLLVMMem (Just (L.MemLoc w_mem mem)) (Formula vars defs) test 
  | Just S.Refl <- S.testEquality w_mem (S.knownNat @(RegWidth arch)) = do
  sym <- askSym
  liftIO $ LLVM.withMem @arch sym $ do
    -- 1) Instantiate the non-mem variables in the formula
    let vars' = MapF.delete mem vars
    defs' <- liftIO $ evalFormula' sym (Formula @sym @arch vars' defs) (testInput test)

    -- 2) Prepare the memory to model the input part of the test
    doMemAccesses (memInput test)

    -- 3) Perform the operations specified by the formula
    LLVM.instantiateMemOpsLLVM (MapF.lookup mem defs')

    -- 4) Check that the default value does not occur anywhere in the test
    defaultInTest <- checkDefaultInTest test
--    liftIO . putStrLn $ "Default in test: " ++ show defaultInTest

    -- 5) Compare the prepared state with the output part of the test
    actualIsDesired <- andPred sym (memOutput test) $ \case
      ReadData _    -> error "Ill-formed concrete test"
      WriteData i desired_v -> do
        let numBytes = S.bvWidth desired_v
        actual_v <- LLVM.readMem @arch numBytes i
        liftIO $ S.isEq sym actual_v desired_v
--    liftIO . putStrLn $ "ActualIsDesired: " ++ show actualIsDesired

    liftIO $ S.andPred sym defaultInTest actualIsDesired

simplifyWithTestLLVMMem _ _ _ = error "Memory location for this architecture does not match architecture"



-- | Check that a bit-vector of dimension w=8^k is not equal to 'd++..++d' where
-- 'd' is the 'defaultValue' stored in the 'MemM' monad
checkDefault :: forall w sym arch.
                (S.IsExprBuilder sym, 1 S.<= w)
             => S.SymBV sym w -> LLVM.MemM sym arch (S.Pred sym)
checkDefault v = do
    sym <- LLVM.askSym
    d   <- LLVM.askDefault
    ds  <- liftIO $ appendN sym (S.bvWidth v) d
    liftIO $ S.isEq sym v ds


-- | If w = 8^k, concatinate a byte k times with itself
appendN :: forall w sym.
           (S.IsExprBuilder sym, 1 S.<= w)
        => sym -> S.NatRepr w -> S.SymBV sym 8 -> IO (S.SymBV sym w)
appendN sym w b = case S.compareNat w (S.knownNat @8) of
    S.NatEQ   -> return b
    
    -- if w < 8, then take the first 'w' elements from 'b'
    S.NatLT _ -> case S.leqTrans (S.addIsLeq w (S.knownNat @1)) (S.LeqProof @(w S.+ 1) @8) of
                   S.LeqProof -> S.bvSelect sym (S.knownNat @0) w b

    S.NatGT y -> case S.addPrefixIsLeq y (S.knownNat @1) of
      S.LeqProof -> do x <- appendN sym (S.incNat y) b
                       S.bvConcat sym b x


-- | Check that the 'defaultValue' in the 'MemM' monad does not occur in the
-- domain of either the input or the output memory in the test
checkDefaultInTest :: forall sym arch.
                      S.IsExprBuilder sym
                   => ConcreteTest sym arch -> LLVM.MemM sym arch (S.Pred sym)
checkDefaultInTest test = do 
  sym <- LLVM.askSym
  inInput <- andPred sym (memInput test) $ go sym
  inOutput <- andPred sym (memOutput test) $ go sym
  liftIO $ S.andPred sym inInput inOutput
  where
    go :: sym -> AccessData sym arch -> LLVM.MemM sym arch (S.Pred sym)
    go sym (WriteData _ v) = do b <- checkDefault v
                                liftIO $ S.notPred sym b
    go _ _ = error "Ill-formed concrete test"

doMemAccesses :: (Architecture arch, LLVM.HasPtrWidth (RegWidth arch), CB.IsSymInterface sym)
              => [AccessData sym arch]
              -> LLVM.MemM sym arch ()
doMemAccesses [] = return ()
doMemAccesses (ReadData _ : ls) = doMemAccesses ls
doMemAccesses (WriteData i v : ls) = do
  LLVM.writeMem i v
  doMemAccesses ls

-- | Build an equality of the form
-- > f(test1_in) = test1_out /\ f(test2_in) = test2_out /\ ... /\ f(testn_in) = testn_out
-- where f(test_in) = test_out is an "equality machine" as in 'buildEqualityMachine'.
buildEqualityTests :: forall arch t st fs
                    . (Architecture arch, CB.IsSymInterface (WE.ExprBuilder t st fs))
                   => Formula (WE.ExprBuilder t st fs) arch
                   -> [ConcreteTest (WE.ExprBuilder t st fs) arch]
                   -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
buildEqualityTests form tests 
  | [L.MemLoc _ _] <- L.memLocation @(Location arch) = do
  sym <- askSym
  andPred sym tests $ simplifyWithTestLLVM form
buildEqualityTests form tests = do
  sym <- askSym
  andPred sym tests $ simplifyWithTestNonMem form

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
                      -> SatResult (GroundEvalFn t) a
                      -> IO (Maybe [TemplatableInstruction arch])
tryExtractingConcrete insns (Sat evalFn) = Just <$> extractConcreteInstructions evalFn insns
tryExtractingConcrete _ Unsat{} = return Nothing
tryExtractingConcrete _ Unknown = fail "got Unknown when checking sat-ness"

-- | Build a formula for the given concrete instruction.
instantiateFormula' :: Architecture arch
                    => TemplatableInstruction arch
                    -> Cegis (WE.ExprBuilder t st fs) arch (Formula (WE.ExprBuilder t st fs) arch)
instantiateFormula' (TemplatableInstruction op oplist) = do
  sym <- askSym
  semantics <- askSemantics
  let pf = unTemplate . fromJust $ MapF.lookup op semantics
  liftIO (snd <$> instantiateFormula sym pf oplist)

-- | Condense a series of instructions in sequential execution into one formula.
condenseInstructions :: Architecture arch
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

cegis' :: (Architecture arch, ArchRepr arch, WPO.OnlineSolver t solver, CB.IsSymInterface (CBO.OnlineBackend t solver fs))
       => [TemplatedInstructionFormula (CBO.OnlineBackend t solver fs) arch]
       -- ^ The trial instructions.
       -> Formula (CBO.OnlineBackend t solver fs) arch
       -- ^ A formula representing the sequence of trial instructions.
       -> [ConcreteTest (CBO.OnlineBackend t solver fs) arch]
       -- ^ All the tests we have so far.
       -> Cegis (CBO.OnlineBackend t solver fs) arch (CegisResult (CBO.OnlineBackend t solver fs) arch)
cegis' trial trialFormula tests = do
--  liftIO . putStrLn $ "Number of tests: " ++ show (length tests)
--   liftIO . putStrLn $ "\n\nTESTS:\n\t" ++ show tests
  sym <- askSym
  -- Is this candidate satisfiable for the concrete tests we have so far? At
  -- this point, the machine state is concrete, but the immediate values of the
  -- instructions are symbolic. If the candidate is satisfiable for the tests,
  -- the SAT solver will give us values for the templated immediates in order to
  -- make the tests pass.
  check <- buildEqualityTests trialFormula tests
--  liftIO . putStrLn $ "Equality tests: " ++ show check
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
          newTest <- liftIO $ mkTest sym targetFormula ctrExample
          liftIO . putStrLn $ "=============Added counterexample:=============== \n" ++ show newTest
          cegis' trial trialFormula (newTest : tests)
    Nothing -> return (CegisUnmatchable tests)


cegis :: (Architecture arch, ArchRepr arch, WPO.OnlineSolver t solver, CB.IsSymInterface (CBO.OnlineBackend t solver fs))
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
  liftIO . putStrLn $ "\n\n=============\nTRIAL FORMULA:\n============\n" ++ show trialFormula
  runReaderT (cegis' trial trialFormula tests) params


