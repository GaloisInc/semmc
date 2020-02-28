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


module SemMC.Synthesis.Cegis.Tests
  ( addTest
  , addTests
  , mkTest
  ) where

import           Control.Monad.IO.Class ( MonadIO,  liftIO )
import           Control.Monad.Trans.Class  (lift)
import qualified Data.Set as Set
import           Control.Monad.Trans.State  (get, put )
import           Data.Maybe ( listToMaybe )
import           Data.Foldable (traverse_)

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Context as Ctx

import qualified Lang.Crucible.Backend as CB
import qualified What4.Interface as S
import qualified What4.Expr as WE

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F

import qualified SemMC.Synthesis.Cegis.Types as T
import qualified SemMC.Synthesis.Cegis.LLVMMem as LLVM
import qualified SemMC.Synthesis.Cegis.EvalFormula as CE
import qualified SemMC.Synthesis.Cegis.MemAccesses as MA


-- | Compute the boolean predicate associated with evaluating the concrete test
-- with respect to the formula. Records two predicates corresponding to
-- non-memory locations and memory, respectively.
--
-- 1) For all non-memory/non-IP registers @r@: Suppose the concrete test
-- specified that register @r@ initially has value @i@ and outputs the value
-- @o@. If the formula maps @r@ to an expression @e@, then let @e'@ be the
-- result of initializing all registers and memory accesses in @e@ with the
-- input state of the concrete test. The predicate records that @e' = o@.
--
-- 2a) Let @memExpr@ be the expression corresponding to memory in the formula.
-- For all memory writes (value @v@ to index @i@) in the concrete test, check
-- that @read_mem memExpr i = v@.
--
-- 2b) In addition, we want to make sure that two different memory writes are
-- not conflated in this equation, which would make the check less precise. For
-- all memory writes (vlaue @v@ to index @i@) in the concrete test and all
-- memory writes (value @v'@ to index @i'@) in the formula, check that if @i <>
-- i'@ then @v <> v'@.
simplifyWithTest :: ( A.Architecture arch
                        , sym ~ WE.ExprBuilder t st fs
                        , CB.IsSymInterface sym
                        )
                     => F.Formula sym arch
                     -> T.ConcreteTest sym arch
                     -> T.Cegis sym arch (WE.BoolExpr t)
simplifyWithTest f test = do
    -- liftIO . putStrLn $ "\nConstructing predicate for test " ++ show test ++ "\n"
    nonMemPred <- simplifyWithTestNonMem f test
    -- liftIO . putStrLn $ "\nNON-MEM PRED: " ++ show nonMemPred ++ "\n"
    memPred <- simplifyWithTestMem (formMem f) f test
    -- liftIO . putStrLn $ "\nMEM PRED: " ++ show memPred ++ "\n"
    sym <- T.askSym
    res <- liftIO $ S.andPred sym nonMemPred memPred
    -- liftIO . putStrLn $ "RESULT: " ++ show res ++ "\n"
    return res

-- | Get the @L.MemLoc@ associated with the formula
formMem :: A.Architecture arch
        => F.Formula sym arch 
        -> Maybe (L.MemLoc (L.Location arch))
formMem f | any (\(Some l) -> L.isMemoryLocation l) (MapF.keys $ F.formParamVars f) = listToMaybe L.memLocation
formMem _ | otherwise = Nothing


-- | Substitute test input (for non-memory locations) into the target formula,
-- producing a new formula f' such that the only free variables in f' are Mem
-- and any uninstantiated immediates to be generated. We then construct a
-- predicate of the form 
-- @forall l <> Mem, f'(l) = testOutput(l)@
simplifyWithTestNonMem :: forall arch t st fs sym.
                        ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                        , A.Architecture arch
                        )
                      => F.Formula sym arch
                      -- ^ the target formula
                      -> T.ConcreteTest sym arch
                      -- ^ a single test case
                      -> T.Cegis sym arch (S.Pred sym)
simplifyWithTestNonMem trialFormula test = do
  sym <- T.askSym
  F.Formula _ defs' <- CE.evalFormulaMem trialFormula $ stripMemLoc @arch @sym (T.testInput test)
  targetFormula <- T.askTarget
  let locs = MA.nonMemIPLocs @arch $ F.formInputs trialFormula
                         `Set.union` F.formOutputs trialFormula
                         `Set.union` F.formInputs  targetFormula
                         `Set.union` F.formOutputs targetFormula
  T.andPred sym locs $ \(Some l) ->
    buildEqualityLocation sym test vars l (MapF.lookup l defs')
  where
    vars = F.formParamVars trialFormula


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
                         (A.Architecture arch, sym ~ WE.ExprBuilder t st fs)
                      => sym
                      -> T.ConcreteTest sym arch
                      -> MapF.MapF (L.Location arch) (S.BoundVar sym)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> L.Location arch tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (S.SymExpr sym tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> T.Cegis sym arch (WE.BoolExpr t)
buildEqualityLocation sym test _vars loc Nothing
  -- If the location does not appear in the test output, then the location is preserved
  | Nothing <- MapF.lookup loc (T.testOutput test) = return $ S.truePred sym

buildEqualityLocation sym test vars outputLoc expr = do
  actuallyIs <- case expr of
                  Just expr' -> liftIO $ CE.evalExpression @arch sym vars (T.testInput test) expr'
                  -- If this location isn't present in the definitions of the
                  -- candidate formula, then its original value is preserved.
                  Nothing -> liftIO $ CE.lookupInState sym (T.testInput test) outputLoc
  shouldBe <- liftIO $ CE.lookupInState sym (T.testOutput test) outputLoc
  -- liftIO . putStrLn $ "Built equation for location " ++ P.showF outputLoc
  -- liftIO . putStrLn $ "FOR test " ++ show test
  -- liftIO . putStrLn $ "WITH expression " ++ show expr
  -- liftIO . putStrLn $ "ACTUALLY IS:\t" ++ show actuallyIs
  -- liftIO . putStrLn $ "SHOULD BE:\t" ++ show shouldBe
  actuallyShould <- liftIO $ S.isEq sym actuallyIs shouldBe
  
  -- Check that these results don't overlap with memory
  noOverlap <- checkNoOverlap @arch sym actuallyIs shouldBe
  -- liftIO . putStrLn $ "NO OVERLAP CHECK:\t" ++ show noOverlap

  liftIO $ S.andPred sym actuallyShould noOverlap

-- | @checkNoOverlap sym e1 e2@ produces the predicate:
-- for all i \in dom(e1), j \in dom(e2), i <> j -> Mem[i] <> Mem[j]
checkNoOverlap :: forall arch t st fs sym tp m.
                  ( T.HasMemExpr m, T.HasSym m, MonadIO (m sym arch)
                  , A.Architecture arch, sym ~ WE.ExprBuilder t st fs)
               => sym
               -> S.SymExpr sym tp
               -> S.SymExpr sym tp
               -> m sym arch (S.Pred sym)
checkNoOverlap sym e1 e2 = do
  let acc1 = MA.liveMemInExpr @arch e1
  let acc2 = MA.liveMemInExpr @arch e2
  T.andPred sym acc1 $ \i ->
    T.andPred sym acc2 $ \j ->
      inMemAccesses $ getIndices i j
  where
    getIndices (A.ReadData i) (A.ReadData j) = (i,j)
    getIndices (A.ReadData i) (A.WriteData j _) = (i,j)
    getIndices (A.WriteData i _) (A.ReadData j) = (i,j)
    getIndices (A.WriteData i _) (A.WriteData j _) = (i,j)

-- | @inMemAccesses (i,j)@ produces the predicate:
-- i <> j -> Mem[i] <> Mem[j]
inMemAccesses :: forall arch sym m.
                 ( T.HasMemExpr m, T.HasSym m, MonadIO (m sym arch)
                 , S.IsExprBuilder sym)
              => (S.SymBV sym (A.RegWidth arch), S.SymBV sym (A.RegWidth arch))
              -> m sym arch (S.Pred sym)
inMemAccesses (i,j) = do
  sym <- T.askSym
  mem <- T.askMemExpr
  liftIO $ do
    i_neq_j <- S.notPred sym =<< S.isEq sym i j
    mem_i <- S.arrayLookup sym mem (Ctx.empty Ctx.:> i)
    mem_j <- S.arrayLookup sym mem (Ctx.empty Ctx.:> j)
    mem_i_neq_mem_j <- S.notPred sym =<< S.isEq sym mem_i mem_j
    S.impliesPred sym i_neq_j mem_i_neq_mem_j


-- | See documentation of @simplifyWithTest@
simplifyWithTestMem :: forall t st fs sym arch.
                        ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                        , A.Architecture arch
                        )
                        => Maybe (L.MemLoc (L.Location arch))
                        -> F.Formula sym arch
                        -> T.ConcreteTest sym arch
                        -> T.Cegis sym arch (S.Pred sym)
-- if there is no memory, return True
simplifyWithTestMem Nothing _ _ = do
  sym <- T.askSym
  return $ S.truePred sym
simplifyWithTestMem (Just (L.MemLoc w_mem mem)) f test
  | Just S.Refl <- S.testEquality w_mem (S.knownNat @(A.RegWidth arch)) = do
  sym <- T.askSym
  memExpr <- T.askMemExpr
  liftIO $ LLVM.withMem @arch sym memExpr $ do
    -- 1) Instantiate the non-mem, non-IP variables in the formula
    F.Formula _ defs' <- liftIO $ CE.evalFormula' sym f $ CE.mkEvalLoc @arch sym (T.testInput test)

    -- 2) Prepare the memory to model the input part of the test
    LLVM.doWrites (T.memInput test)

    -- 3) Perform the operations specified by the formula
    traverse_ LLVM.instantiateMemOps $ MapF.lookup mem defs'

    -- 4) Compare the prepared state with the output part of the test
    T.andPred sym (T.memOutput test) $ \case
      A.ReadData _    -> error "Ill-formed concrete test"
      A.WriteData i shouldBe -> do
        let numBytes = S.bvWidth shouldBe
        actuallyIs <- LLVM.readMem @arch numBytes i
        actuallyShould <- liftIO $ S.isEq sym actuallyIs shouldBe

        -- 5) Check that the default value does not occur anywhere in the test
        noOverlap <- checkNoOverlap @arch sym actuallyIs shouldBe

        liftIO $ S.andPred sym noOverlap actuallyShould

simplifyWithTestMem _ _ _ = error "Memory location for this architecture does not match architecture"

-- | Add a concrete test to the Cegis state. This both adds the test to the set
-- of tests, and also computes the predicate associated with this test, and
-- conjoins it with the current cashed check.
addTest ::  ( A.Architecture arch
            , sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym
            )
        => F.Formula sym arch
        -> T.ConcreteTest sym arch
        -> T.Cegis sym arch ()
addTest form test = do
  sym <- T.askSym
  st <- T.Cegis $ lift get
  let newTests = test : T.csTests st

  check <- simplifyWithTest form test
  newCheck  <- liftIO $ S.andPred sym check (T.csCheck st)
  T.Cegis . lift $ put (st{T.csTests = newTests, T.csCheck = newCheck})

-- | Adds a list of concrete tests to the Cegis state. This both adds the tests
-- to the underlying set of tests, and also computes the predicates associated
-- with the tests, and conjoins them with the current cashed check @csCheck@.
addTests :: ( A.Architecture arch
            , sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym)
        => F.Formula sym arch -> [T.ConcreteTest sym arch] -> T.Cegis sym arch ()
addTests form tests = mapM_ (addTest form) tests


stripMemLoc :: forall arch sym.
               A.Architecture arch
            => L.ArchState arch (S.SymExpr sym)
            -> L.ArchState arch (S.SymExpr sym)
stripMemLoc = MapF.filterWithKey (\l _ -> not (L.isMemoryLocation l))


-- | Given a formula and a counterexample provided from the solver, construct
-- the concrete test illustrated by the counterexample.
--
-- TODO: adapt this to deal with the case when the architecture has several
-- memory locations (e.g. A32)
mkTest :: forall sym arch t st fs.
          (A.Architecture arch, sym ~ WE.ExprBuilder t st fs
          , CB.IsSymInterface sym
          )
       => sym
       -> F.Formula sym arch
       -> L.ArchState arch (S.SymExpr sym)
       -> S.SymExpr sym (A.MemType arch)
       -> IO (T.ConcreteTest sym arch)
mkTest sym targetFormula ctrExample memExpr = do

    -- Substitute the non-memory locations from the test into the target
    -- formula, and evaluate @readMem@s that occur. For non-memory locations,
    -- this gives us the testOutput.
    let testInput' = stripMemLoc @arch @sym ctrExample
    let el = CE.mkMemEvalLoc @arch sym testInput' memExpr
    F.Formula _ ctrExampleOut <- CE.evalFormulaMem' @arch sym targetFormula el
    let testOutput' = stripMemLoc @arch @sym ctrExampleOut

    -- To construct the memInput/memOutput, find all memory locations that occur
    -- in the input/output counterexamples respectively and record the values
    -- they map to.
    let inputAddrMap  = MA.liveMemMap $ F.Formula @sym @arch (F.formParamVars targetFormula) ctrExample
        outputAddrMap = MA.liveMemMap $ F.Formula @sym @arch (F.formParamVars targetFormula) ctrExampleOut

    return $ T.ConcreteTest testInput' testOutput'
                          (Set.toList inputAddrMap) (Set.toList outputAddrMap)


{-
-- | Construct an initial test from concrete values
--
-- FIXME: In order to do this properly, we need to select default locExprs that
-- do not lead to any address space overflows; see the way we search for
-- counterexamples in @SemMC.Synthesis.Cegis@.
--
-- For now, we do not seed Cegis with an initial test, so this function is
-- unused.
initTest :: ( A.Architecture arch, sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym
            )
         => sym
         -> F.Formula sym arch
         -> S.SymExpr sym (A.MemType arch)
         -> IO (T.ConcreteTest sym arch)
initTest sym f memExpr = do locExprs <- defaultLocExprs sym
                            mkTest sym f locExprs memExpr

-- Returns a 'LocExprs' data structure that maps all locations to a default value.
defaultLocExprs :: forall loc sym.
                ( L.IsLocation loc
                , MapF.OrdF loc
                , S.IsExprBuilder sym
                )
               => sym
               -> IO (T.LocExprs sym loc)
defaultLocExprs sym = do locExprList <- mapM pairDefault $ L.nonIPLocations @loc
                         return $ MapF.fromList locExprList
  where
    pairDefault :: Some loc -> IO (MapF.Pair loc (S.SymExpr sym))
    pairDefault (Some l) = MapF.Pair l <$> L.defaultLocationExpr sym l
-}
