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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module SemMC.Synthesis.Cegis
  ( ConcreteTest
  , initTest
  , CegisParams
  , cpMem
  , mkCegisParams
  , CegisResult(..)
  , cegis
  ) where

import           Control.Monad.IO.Class ( liftIO, MonadIO )
import           Control.Monad.Trans.Reader ( ReaderT(..), reader )
import           Control.Monad.Trans.State  (StateT(..), evalStateT, get, put )
import           Control.Monad.Trans.Class  (lift)
import           Data.Foldable
import           Data.Maybe ( fromJust, listToMaybe, isJust )
import           Data.Kind
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.HasRepr as HR
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableF
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Set as Set

import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Backend as CB
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
import qualified SemMC.Formula.MemAccesses as MA
import qualified SemMC.Synthesis.LLVMMem as LLVM
import qualified SemMC.Synthesis.ReadWriteEval as RW


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
lookupInState sym st loc | Just e <- MapF.lookup loc st = return e
                         | otherwise = defaultLocationExpr sym loc

-- | Look up the given location in the given machine state. If the location is
-- 'Mem', return the mem-expression. Otherwise, if the location is not found, return the
-- default value for that location.
lookupInStateMem :: forall arch sym tp.
                    (S.IsExprBuilder sym, Architecture arch)
                 => sym
                 -> LocExprs sym (Location arch)
                 -> S.SymExpr sym (MemType arch)
                 -> Location arch tp
                 -> IO (S.SymExpr sym tp)
lookupInStateMem sym st memExpr loc
  | isMemLoc loc
  , Just S.Refl <- S.testEquality (memTypeRepr @arch) (locationType loc) = 
    return memExpr
  | otherwise = lookupInState sym st loc

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
              , cpMem :: S.SymExpr sym (MemType arch)
              -- ^ A symbolic expression representing the memory in all tests
              }

-- | Construct parameters for Cegis
mkCegisParams :: (S.IsSymExprBuilder sym, Architecture arch)
              => sym 
              -> TemplatedSemantics sym arch
              -- ^ The base set of opcode semantics
              -> Formula sym arch
              -- ^ The target formula we're trying to match
              -> FormulaEnv sym arch
              -- ^ The uninterpreted functions in scope
              -> IO (CegisParams sym arch)
mkCegisParams sym sem target ufEnv = case S.userSymbol "Mem" of
    Left  err       -> error (show $ WS.ppSolverSymbolError err)
    Right memSymbol -> do
      memExpr <- S.freshConstant sym memSymbol S.knownRepr
      return $ CegisParams { cpSym = sym
                           , cpSemantics = sem
                           , cpTarget = target
                           , cpUFEnv = ufEnv
                           , cpMem   = memExpr
                           }

mkCegisState :: (S.IsSymExprBuilder sym, Architecture arch)
             => sym
             -> CegisState sym arch
mkCegisState sym = CegisState { csTests = []
                             , csCheck = S.truePred sym
                             }



data CegisState sym arch =
  CegisState { csTests :: [ConcreteTest sym arch]
             -- ^ The tests so far
             , csCheck :: S.Pred sym
             -- ^ A predicate represnting the validity of the current tests
             }
newtype Cegis sym arch a = Cegis (ReaderT (CegisParams sym arch) (StateT (CegisState sym arch) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runCegis :: CegisParams sym arch -> CegisState sym arch -> Cegis sym arch a -> IO a
runCegis params st (Cegis op) = evalStateT (runReaderT op params) st

askSym :: Cegis sym arch sym
askSym = Cegis $ reader cpSym

askSemantics :: Cegis sym arch (TemplatedSemantics sym arch)
askSemantics = Cegis $ reader cpSemantics

askTarget :: Cegis sym arch (Formula sym arch)
askTarget = Cegis $ reader cpTarget

askTests :: Cegis sym arch [ConcreteTest sym arch]
askTests = Cegis . lift $ csTests <$> get

askCheck :: Cegis sym arch (S.Pred sym)
askCheck = Cegis . lift $ csCheck <$> get

askMemExpr :: Cegis sym arch (S.SymExpr sym (MemType arch))
askMemExpr = Cegis $ reader cpMem

addTest ::  ( Architecture arch
            , sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym
            )
        => Formula sym arch
        -> ConcreteTest sym arch
        -> Cegis sym arch ()
addTest form test = do
  liftIO . putStrLn $ "Adding test " ++ show test
  sym <- askSym
  st <- Cegis $ lift get
  let newTests = test : csTests st

  check <- simplifyWithTestLLVM form test
  -- liftIO . putStrLn $ "Resulting in: " ++ show check
  newCheck  <- liftIO $ S.andPred sym check (csCheck st)
  Cegis . lift $ put (st{csTests = newTests, csCheck = newCheck})

addTests :: ( Architecture arch
            , sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym)
        => Formula sym arch -> [ConcreteTest sym arch] -> Cegis sym arch ()
addTests form tests = mapM_ (addTest form) tests

-- | Evaluate an expression, substituting in the location values given in the
-- machine state.
evalExpression :: (IsLocation loc)
               => WE.ExprBuilder t st fs
               -> MapF.MapF loc (WE.ExprBoundVar t)
               -> LocExprs (WE.ExprBuilder t st fs) loc
               -> WE.Expr t tp
               -> IO (WE.Expr t tp)
evalExpression sym vars state = evalExpression' sym vars (lookupInState sym state)

-- Both replace literal vars and call 'instantiateReadMem'
evalExpression' :: (IsLocation loc)
               => WE.ExprBuilder t st fs
               -> MapF.MapF loc (WE.ExprBoundVar t)
               -> (forall tp'. loc tp' -> IO (WE.Expr t tp'))
               -> WE.Expr t tp
               -> IO (WE.Expr t tp)
evalExpression' sym vars evalLoc e = replaceLitVars sym evalLoc vars e

evalFormula' :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                , Architecture arch
                )
             => sym
             -> Formula sym arch
             -> (forall tp'. Location arch tp' -> IO (WE.Expr t tp'))
--             -> IO (L.ArchState arch (WE.Expr t))
             -> IO (Formula sym arch)
evalFormula' sym f@(Formula vars defs) evalLoc = do
  -- 1) replace all the locations in the formula with their interpretations by evalLoc
  defs' <- traverseF (evalExpression' sym vars evalLoc) defs
  return (Formula vars defs')

evalFormulaNoMem :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                , Architecture arch
                )
             => sym
             -> Formula sym arch
             -> L.ArchState arch (WE.Expr t)
             -> Cegis sym arch (Formula sym arch)
evalFormulaNoMem sym f state = do
  memExpr <- askMemExpr
  let evalLoc :: forall tp. Location arch tp -> IO (WE.Expr t tp)
      evalLoc = lookupInStateMem @arch sym state memExpr
  f' <- liftIO $ evalFormula' sym f evalLoc
  liftIO $ evalFormulaReadMem sym f' evalLoc

-- | instantiate all calls to read_mem in the formula
evalFormulaReadMem :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                , Architecture arch
                )
             => sym
             -> Formula sym arch
             -> (forall tp'. Location arch tp' -> IO (WE.Expr t tp'))
             -> IO (Formula sym arch)
evalFormulaReadMem sym f@(Formula vars defs) evalLoc = do
  defs' <- traverseF (RW.instantiateReadMem sym f evalLoc) defs
  return (Formula vars defs')

evalFormula :: forall arch t st fs sym.
             ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
             , Architecture arch
             )
             => sym
             -> Formula sym arch
             -> L.ArchState arch (WE.Expr t)
             -> IO (Formula sym arch)
evalFormula sym f st = evalFormula' sym f (lookupInState sym st)

-- | Concrete input and output states of a formula. There's nothing in the types
-- that prevents the values from being symbolic, but please don't let them be!
data ConcreteTest sym arch =
  ConcreteTest { testInput  :: LocExprs sym (Location arch)
                , testOutput :: LocExprs sym (Location arch)
                , memInput   :: [AccessData sym arch]
                , memOutput  :: [AccessData sym arch]
                }

instance (Architecture arch, P.ShowF (S.SymExpr sym))
      => Show (ConcreteTest sym arch)
  where
    show test = "⟨\t" ++ show (testInput test)
                ++ "\n|||\t" ++ show (testOutput test)
                ++ "\n|||\t" ++ show (memInput test) 
                ++ "\n|||\t" ++ show (memOutput test) ++ "\n⟩"

-- | The equality of concrete tests only relies on their input
concreteTestEq :: forall sym arch.
                  (S.IsExprBuilder sym, Architecture arch)
               => ConcreteTest sym arch -> ConcreteTest sym arch -> Bool
concreteTestEq test1 test2 =
    concreteLocExprsEq (testInput test1) (testInput test2)
    && and (zipWith concreteAccessDataEq (memInput test1) (memInput test2))
  where
    concreteLocExprsEq :: S.TestEquality loc => LocExprs sym loc -> LocExprs sym loc -> Bool
    concreteLocExprsEq (MapF.toList -> l1) (MapF.toList -> l2) =
        length l1 == length l2
        && and (zipWith (\(MapF.Pair loc1 v1) (MapF.Pair loc2 v2) ->
                            isJust (S.testEquality loc1 loc2) && concreteSymExprEq v1 v2)
                l1 l2)

    concreteAccessDataEq :: AccessData sym arch -> AccessData sym arch -> Bool
    concreteAccessDataEq (ReadData idx1) (ReadData idx2) = concreteSymExprEq idx1 idx2
    concreteAccessDataEq (WriteData idx1 v1) (WriteData idx2 v2) =
        concreteSymExprEq idx1 idx2 && concreteSymExprEq v1 v2
    concreteAccessDataEq _ _ = False

    concreteSymExprEq :: S.SymExpr sym tp1 -> S.SymExpr sym tp2 -> Bool
    concreteSymExprEq e1 e2 | Just c1 <- S.asConcrete e1, Just c2 <- S.asConcrete e2 =
        isJust (S.testEquality c1 c2)
                            | otherwise = False

isExistingTest :: (Architecture arch, S.IsExprBuilder sym)
               => ConcreteTest sym arch
               -> Cegis sym arch Bool
isExistingTest test = do
  tests <- askTests
  return . and $ not . concreteTestEq test <$> tests

-- Returns a 'LocExprs' data structure that maps all locations to a default value.
defaultLocExprs :: forall loc sym. 
                ( L.IsLocation loc 
                , MapF.OrdF loc 
                , S.IsExprBuilder sym
                )
               => sym
               -> IO (LocExprs sym loc) -- MapF.MapF loc (S.SymExpr sym)
defaultLocExprs sym = do locExprList <- mapM pairDefault $ L.nonIPLocations @loc
                         return $ MapF.fromList locExprList
  where
    pairDefault :: Some loc -> IO (MapF.Pair loc (S.SymExpr sym))
    pairDefault (Some l) = MapF.Pair l <$> L.defaultLocationExpr sym l


-- | Given a formula and a counterexample provided from the solver, construct
-- the concrete test illustrated by the counterexample.
--
-- TODO: adapt this to deal with the case when the architecture has several memory locations (e.g. A32)
mkTest :: forall sym arch t st fs.
          (Architecture arch, sym ~ WE.ExprBuilder t st fs
          , CB.IsSymInterface sym
          )
       => sym
       -> Formula sym arch
       -> L.ArchState arch (S.SymExpr sym)
       -> S.SymExpr sym (MemType arch)
       -> IO (ConcreteTest sym arch)
mkTest sym targetFormula ctrExample memExpr
  | [L.MemLoc _ mem] <- memLocation @(L.Location arch) = do
    -- putStrLn $ "Constructing test from " ++ show ctrExample
    -- the testInput is exactly the counter example for non-memory, non-IP locations
    let testInput' = MapF.filterWithKey (\l _ -> not (L.isIP l) && not (L.isMemLoc l)) ctrExample

    -- substitute the non-memory locations from the test into the target
    -- formula. For non-memory locations, this gives us the testOutput.
    let evalLoc :: forall tp. Location arch tp -> IO (WE.Expr t tp)
        evalLoc = lookupInStateMem @arch sym testInput' memExpr
    targetFormula' <- evalFormula' sym targetFormula evalLoc
    Formula _ ctrExampleOut <- evalFormulaReadMem sym targetFormula' evalLoc
    let testOutput' = MapF.filterWithKey (\l _ -> not (L.isIP l) && not (L.isMemLoc l)) ctrExampleOut

    -- to construct the memInput/memOutput, we need to find all memory locations that
    -- occur in the input/output counterexamples respectively and record the values they map to.
    let inputAddrMap  = MA.liveMemMap $ Formula @sym @arch (formParamVars targetFormula) ctrExample
        outputAddrMap = MA.liveMemMap $ Formula @sym @arch (formParamVars targetFormula) ctrExampleOut

    return $ ConcreteTest testInput' testOutput'
                          (Set.toList inputAddrMap) (Set.toList outputAddrMap)
mkTest _ _ _ _ | otherwise = error "Cannot make test for this architecture"

-- | Construct an initial test from concrete values
initTest :: ( Architecture arch, sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym
            )
         => sym
         -> Formula sym arch
         -> S.SymExpr sym (MemType arch)
         -> IO (ConcreteTest sym arch)
initTest sym f memExpr = do locExprs <- defaultLocExprs sym
                            mkTest sym f locExprs memExpr


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
buildEqualityLocation :: forall arch t st fs tp sym.
                         (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                      => sym
                      -> ConcreteTest sym arch
                      -> MapF.MapF (Location arch) (S.BoundVar sym)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> Location arch tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (S.SymExpr sym tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> Cegis sym arch (WE.BoolExpr t)
buildEqualityLocation sym test _vars loc Nothing
  -- If the location does not appear in the test output, then the location is preserved
  | Nothing <- MapF.lookup loc (testOutput test) = return $ S.truePred sym

buildEqualityLocation sym test vars outputLoc expr = do
  actuallyIs <- case expr of
                  Just expr' -> liftIO $ evalExpression sym vars (testInput test) expr'
                  -- If this location isn't present in the definitions of the
                  -- candidate formula, then its original value is preserved.
                  Nothing -> liftIO $ lookupInState sym (testInput test) outputLoc
  shouldBe <- liftIO $ lookupInState sym (testOutput test) outputLoc
  liftIO . putStrLn $ "Built equation for location " ++ P.showF outputLoc
  -- liftIO . putStrLn $ "FOR test " ++ show test
  liftIO . putStrLn $ "WITH expression " ++ show expr
  liftIO . putStrLn $ "ACTUALLY IS:\t" ++ show actuallyIs
  liftIO . putStrLn $ "SHOULD BE:\t" ++ show shouldBe
  actuallyShould <- liftIO $ S.isEq sym actuallyIs shouldBe
  
  -- Check that these results don't overlap with memory
  noOverlap <- checkNoOverlap @arch sym actuallyIs shouldBe
  liftIO . putStrLn $ "NO OVERLAP CHECK:\t" ++ show noOverlap

  liftIO $ S.andPred sym actuallyShould noOverlap

-- | @checkNoOverlap sym e1 e2@ produces the predicate:
-- for all i \in dom(e1), j \in dom(e2), i <> j -> Mem[i] <> Mem[j]
checkNoOverlap :: forall arch t st fs sym tp.
                  (Architecture arch, sym ~ WE.ExprBuilder t st fs)
               => sym
               -> S.SymExpr sym tp
               -> S.SymExpr sym tp
               -> Cegis sym arch (S.Pred sym)
checkNoOverlap sym e1 e2 = do
  let acc1 = MA.liveMemInExpr @arch e1
  let acc2 = MA.liveMemInExpr @arch e2
  andPred sym acc1 $ \i ->
    andPred sym acc2 $ \j ->
      inMemAccesses $ getIndices i j
  where
    getIndices (ReadData i) (ReadData j) = (i,j)
    getIndices (ReadData i) (WriteData j _) = (i,j)
    getIndices (WriteData i _) (ReadData j) = (i,j)
    getIndices (WriteData i _) (WriteData j _) = (i,j)

-- | @inMemAccesses (i,j)@ produces the predicate:
-- forall j \in accesses, i <> j -> Mem[i] <> Mem[j]
inMemAccesses :: forall arch sym.
                 (S.IsExprBuilder sym)
              => (S.SymBV sym (RegWidth arch), S.SymBV sym (RegWidth arch))
              -> Cegis sym arch (S.Pred sym)
inMemAccesses (i,j) = do
  sym <- askSym
  mem <- askMemExpr
  liftIO $ do
    i_neq_j <- S.notPred sym =<< S.isEq sym i j
    mem_i <- S.arrayLookup sym mem (Ctx.empty Ctx.:> i)
    mem_j <- S.arrayLookup sym mem (Ctx.empty Ctx.:> j)
    mem_i_neq_mem_j <- S.notPred sym =<< S.isEq sym mem_i mem_j
    S.impliesPred sym i_neq_j mem_i_neq_mem_j

simplifyWithTestLLVM :: ( Architecture arch
                        , sym ~ WE.ExprBuilder t st fs
                        , CB.IsSymInterface sym
                        )
                     => Formula sym arch
                     -> ConcreteTest sym arch
                     -> Cegis sym arch (WE.BoolExpr t)
simplifyWithTestLLVM f test = do
--    liftIO . putStrLn $ "\nConstructing predicate for test " ++ show test ++ "\n"
    nonMemPred <- simplifyWithTestNonMem f test
    liftIO . putStrLn $ "\nNON-MEM PRED: " ++ show nonMemPred ++ "\n"
    memPred <- simplifyWithTestLLVMMem (formMem f) f test
    liftIO . putStrLn $ "\nMEM PRED: " ++ show memPred ++ "\n"
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
                        ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                        , Architecture arch
                        )
                      => Formula sym arch
                      -- ^ the target formula
                      -> ConcreteTest sym arch
                      -- ^ a single test case
                      -> Cegis sym arch (S.Pred sym)
simplifyWithTestNonMem trialFormula test = do
  sym <- askSym
  Formula _ defs' <- evalFormulaNoMem sym trialFormula (testInput test)
  targetFormula <- askTarget -- Make sure to also include relevant locations in the target
  let nonMemLocs' = fst . partitionLocs @arch $ formInputs trialFormula
                                   `Set.union` formOutputs trialFormula
                                   `Set.union` formInputs  targetFormula
                                   `Set.union` formOutputs targetFormula
      nonMemLocs = Set.filter (\(Some l) -> not (L.isIP l) && not (L.isMemLoc l)) nonMemLocs'
--  liftIO . putStrLn $ "Non-mem locs: " ++ show nonMemLocs
  liftIO . putStrLn $ "Trial Formula: " ++ show trialFormula
  liftIO . putStrLn $ "  evaluated with respect to the test: " ++ show defs'
  andPred sym nonMemLocs $ \(Some l) ->
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
                        ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                        , Architecture arch
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
  liftIO $ LLVM.withMem @arch sym $ \defaultValue -> do
    -- 1) Instantiate the non-mem, non-IP variables in the formula
    let vars' = MapF.filterWithKey (\l _ -> not (L.isIP l) && not (L.isMemLoc l)) vars
    Formula _ defs' <- liftIO $ evalFormula sym (Formula @sym @arch vars' defs) (testInput test)

    -- 2) Prepare the memory to model the input part of the test
    LLVM.doMemAccesses (memInput test)

    -- 3) Perform the operations specified by the formula
    LLVM.instantiateMemOpsLLVM (MapF.lookup mem defs')

    -- 4) Check that the default value does not occur anywhere in the test
    defaultInTest <- checkDefaultInTest defaultValue test
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
--    return actualIsDesired

simplifyWithTestLLVMMem _ _ _ = error "Memory location for this architecture does not match architecture"



-- | Check that a bit-vector of dimension w=8^k is not equal to 'd++..++d' where
-- 'd' is the 'defaultValue' stored in the 'MemM' monad
checkDefault :: forall w sym arch.
                (S.IsExprBuilder sym, 1 S.<= w)
             => S.SymBV sym 8 -> S.SymBV sym w -> LLVM.MemM sym arch (S.Pred sym)
checkDefault defaultValue v = do
    sym <- LLVM.askSym
    ds  <- liftIO $ appendN sym (S.bvWidth v) defaultValue
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
                   => S.SymBV sym 8 -> ConcreteTest sym arch -> LLVM.MemM sym arch (S.Pred sym)
checkDefaultInTest defaultValue test = do
  sym <- LLVM.askSym
  inInput <- andPred sym (memInput test) $ go sym
  inOutput <- andPred sym (memOutput test) $ go sym
  liftIO $ S.andPred sym inInput inOutput
  where
    go :: sym -> AccessData sym arch -> LLVM.MemM sym arch (S.Pred sym)
    go sym (WriteData _ v) = do b <- checkDefault defaultValue v
                                liftIO $ S.notPred sym b
    go _ _ = error "Ill-formed concrete test"


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

cegis' :: (Architecture arch, ArchRepr arch
          , WPO.OnlineSolver t solver
          , CB.IsSymInterface (CBO.OnlineBackend t solver fs)
          )
       => [TemplatedInstructionFormula (CBO.OnlineBackend t solver fs) arch]
       -- ^ The trial instructions.
       -> Formula (CBO.OnlineBackend t solver fs) arch
       -- ^ A formula representing the sequence of trial instructions.
--       -> [ConcreteTest (CBO.OnlineBackend t solver fs) arch]
--       -- ^ All the tests we have so far.
       -> Cegis (CBO.OnlineBackend t solver fs) arch (CegisResult (CBO.OnlineBackend t solver fs) arch)
cegis' trial trialFormula = do
--  liftIO . putStrLn $ "Number of tests: " ++ show (length tests)
--   liftIO . putStrLn $ "\n\nTESTS:\n\t" ++ show tests
  sym <- askSym
--  check <- buildEqualityTests trialFormula tests
  -- liftIO . putStrLn $ "Equality tests: " ++ show check
  check <- askCheck
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
                                         (MA.liveMemConst filledInFormula)
                                         targetFormula 
                                         filledInFormula
      case equiv of
        -- FIXME: Is the correct behavior in a timeout to give up on this
        -- branch?
        Timeout -> do
          liftIO . putStrLn $ "Timeout"
          tests <- askTests
          return (CegisUnmatchable tests)
        Equivalent -> do
          liftIO . putStrLn $ "Equivalent"
          return . CegisEquivalent $ map templInsnToDism insns'
        DifferentBehavior ctrExample -> do
          memExpr <- askMemExpr
          newTest <- liftIO $ mkTest sym targetFormula ctrExample memExpr

          -- JP: it is probably better not to do this check since if the
          -- algorithm works correctly it should never generate duplicate
          -- checks. However, for debugging purposes this check is useful to
          -- avoid infinite loops
          testIsDuplicate <- isExistingTest newTest
          if testIsDuplicate
          then -- error "Generated a duplicate test in Cegis"
               askTests >>= return . CegisUnmatchable
          else do
            liftIO . putStrLn $ "=============Added counterexample:=============== \n" ++ show newTest
            -- If the template has no degrees of freedom, then we have checked
            -- that this template is not equivalent to this formula, and we should
            -- move on.

            -- Adds newTest to params
            addTest trialFormula newTest
            cegis' trial trialFormula
    Nothing -> do
        tests <- askTests
        return (CegisUnmatchable tests)


cegis :: forall arch sym t solver fs.
        ( Architecture arch, ArchRepr arch
        , WPO.OnlineSolver t solver, sym ~ CBO.OnlineBackend t solver fs
        , CB.IsSymInterface sym
        )
      => CegisParams sym arch
      -- ^ Parameters not specific to the candidate. See 'CegisParams' for
      -- details.
      -> [ConcreteTest sym arch]
      -- ^ The concrete tests
      -> [TemplatedInstructionFormula sym arch]
      -- ^ The candidate program.
      -> IO (CegisResult sym arch)
cegis params tests trial = do
  let sym = cpSym params
  trialFormula <- condenseFormulas sym (map tifFormula trial)
  liftIO . putStrLn $ "=============\nTRIAL FORMULA:\n============\n" ++ show trialFormula

  -- Is this candidate satisfiable for the concrete tests we have so far? At
  -- this point, the machine state is concrete, but the immediate values of the
  -- instructions are symbolic. If the candidate is satisfiable for the tests,
  -- the SAT solver will give us values for the templated immediates in order to
  -- make the tests pass.

--  liftIO . putStrLn $ "Target formula: " ++ show (cpTarget params)
  let cst = mkCegisState sym
  runCegis params cst $ do addTests trialFormula tests
                           cegis' trial trialFormula
