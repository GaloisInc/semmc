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
  , initTest
  ) where

import           Control.Monad.IO.Class ( MonadIO,  liftIO )
import           Control.Monad.Trans.Class  (lift)
import qualified Data.Set as Set
import           Control.Monad.Trans.State  (get, put )
import           Data.Maybe ( fromJust, listToMaybe )
import           Data.Foldable (foldrM)

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Context as Ctx

import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Backend as CB
import qualified What4.Interface as S
import qualified What4.Expr as WE
import qualified What4.Protocol.Online as WPO

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F

import qualified SemMC.Synthesis.Cegis.Types as T
import qualified SemMC.Synthesis.Cegis.LLVMMem as LLVM
import qualified SemMC.Synthesis.Cegis.EvalFormula as CE
import qualified SemMC.Synthesis.Cegis.MemAccesses as MA


simplifyWithTestLLVM :: ( A.Architecture arch
                        , sym ~ WE.ExprBuilder t st fs
                        , CB.IsSymInterface sym
                        )
                     => F.Formula sym arch
                     -> T.ConcreteTest sym arch
                     -> T.Cegis sym arch (WE.BoolExpr t)
simplifyWithTestLLVM f test = do
--    liftIO . putStrLn $ "\nConstructing predicate for test " ++ show test ++ "\n"
    nonMemPred <- simplifyWithTestNonMem f test
    liftIO . putStrLn $ "\nNON-MEM PRED: " ++ show nonMemPred ++ "\n"
    memPred <- simplifyWithTestLLVMMem (formMem f) f test
    liftIO . putStrLn $ "\nMEM PRED: " ++ show memPred ++ "\n"
    sym <- T.askSym
    res <- liftIO $ S.andPred sym nonMemPred memPred
--    liftIO . putStrLn $ "RESULT: " ++ show res ++ "\n"
    return res

-- | Get the @L.MemLoc@ associated with the formula
formMem :: A.Architecture arch
        => F.Formula sym arch 
        -> Maybe (L.MemLoc (L.Location arch))
formMem f | any (\(Some l) -> L.isMemLoc l) (MapF.keys $ F.formParamVars f) = listToMaybe L.memLocation
formMem _ | otherwise = Nothing


-- | Substitute test input (for non-memory locations) into the target formula,
-- producing a new formula f' such that the only free variables in f' are Mem
-- and any uninstantiated immediates to be generated. We then construct a
-- predicate of the form 
-- @\forall l <> Mem, f'(l) = testOutput(l)@
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
  targetFormula <- T.askTarget -- Make sure to also include relevant locations in the target
  let locs = MA.nonMemIPLocs @arch $ F.formInputs trialFormula
                         `Set.union` F.formOutputs trialFormula
                         `Set.union` F.formInputs  targetFormula
                         `Set.union` F.formOutputs targetFormula
--  liftIO . putStrLn $ "Non-mem locs: " ++ show locs
  liftIO . putStrLn $ "Trial Formula: " ++ show trialFormula
  liftIO . putStrLn $ "  evaluated with respect to the test: " ++ show defs'
  andPred sym locs $ \(Some l) ->
    buildEqualityLocation sym test vars l (MapF.lookup l defs')
  where
    vars = F.formParamVars trialFormula

-- | Take the conjunction of (f a) for each a in some foldable data structure
andPred :: forall t sym m a. (Foldable t, S.IsExprBuilder sym, MonadIO m)
        => sym -> t a -> (a -> m (S.Pred sym)) -> m (S.Pred sym)
andPred sym ls f = foldrM go (S.truePred sym) ls
  where
    go :: a -> S.Pred sym -> m (S.Pred sym)
    go accA accP = f accA >>= liftIO . (S.andPred sym accP)



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
                  (A.Architecture arch, sym ~ WE.ExprBuilder t st fs)
               => sym
               -> S.SymExpr sym tp
               -> S.SymExpr sym tp
               -> T.Cegis sym arch (S.Pred sym)
checkNoOverlap sym e1 e2 = do
  let acc1 = MA.liveMemInExpr @arch e1
  let acc2 = MA.liveMemInExpr @arch e2
  andPred sym acc1 $ \i ->
    andPred sym acc2 $ \j ->
      inMemAccesses $ getIndices i j
  where
    getIndices (A.ReadData i) (A.ReadData j) = (i,j)
    getIndices (A.ReadData i) (A.WriteData j _) = (i,j)
    getIndices (A.WriteData i _) (A.ReadData j) = (i,j)
    getIndices (A.WriteData i _) (A.WriteData j _) = (i,j)

-- | @inMemAccesses (i,j)@ produces the predicate:
-- i <> j -> Mem[i] <> Mem[j]
inMemAccesses :: forall arch sym.
                 (S.IsExprBuilder sym)
              => (S.SymBV sym (A.RegWidth arch), S.SymBV sym (A.RegWidth arch))
              -> T.Cegis sym arch (S.Pred sym)
inMemAccesses (i,j) = do
  sym <- T.askSym
  mem <- T.askMemExpr
  liftIO $ do
    i_neq_j <- S.notPred sym =<< S.isEq sym i j
    mem_i <- S.arrayLookup sym mem (Ctx.empty Ctx.:> i)
    mem_j <- S.arrayLookup sym mem (Ctx.empty Ctx.:> j)
    mem_i_neq_mem_j <- S.notPred sym =<< S.isEq sym mem_i mem_j
    S.impliesPred sym i_neq_j mem_i_neq_mem_j



simplifyWithTestLLVMMem :: forall t st fs sym arch.
                        ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                        , A.Architecture arch
                        )
                        => Maybe (L.MemLoc (L.Location arch))
                        -> F.Formula sym arch
                        -> T.ConcreteTest sym arch
                        -> T.Cegis sym arch (S.Pred sym)
-- if there is no memory, return True
simplifyWithTestLLVMMem Nothing _ _ = do
  sym <- T.askSym
  return $ S.truePred sym
simplifyWithTestLLVMMem (Just (L.MemLoc w_mem mem)) f test 
  | Just S.Refl <- S.testEquality w_mem (S.knownNat @(A.RegWidth arch)) = do
  sym <- T.askSym
  liftIO $ LLVM.withMem @arch sym $ \defaultValue -> do
    -- 1) Instantiate the non-mem, non-IP variables in the formula
    F.Formula _ defs' <- liftIO $ CE.evalFormula' sym f $ CE.mkEvalLoc @arch sym (T.testInput test)

    -- 2) Prepare the memory to model the input part of the test
    LLVM.doMemAccesses (T.memInput test)

    -- 3) Perform the operations specified by the formula
    LLVM.instantiateMemOpsLLVM (MapF.lookup mem defs')

    -- 4) Check that the default value does not occur anywhere in the test
    defaultInTest <- checkDefaultInTest defaultValue test

    -- 5) Compare the prepared state with the output part of the test
    actualIsDesired <- andPred sym (T.memOutput test) $ \case
      A.ReadData _    -> error "Ill-formed concrete test"
      A.WriteData i desired_v -> do
        let numBytes = S.bvWidth desired_v
        actual_v <- LLVM.readMem @arch numBytes i
        liftIO $ S.isEq sym actual_v desired_v
    -- liftIO . putStrLn $ "ActualIsDesired: " ++ show actualIsDesired

    liftIO $ S.andPred sym defaultInTest actualIsDesired
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
                   => S.SymBV sym 8 -> T.ConcreteTest sym arch -> LLVM.MemM sym arch (S.Pred sym)
checkDefaultInTest defaultValue test = do
  sym <- LLVM.askSym
  inInput <- andPred sym (T.memInput test) $ go sym
  inOutput <- andPred sym (T.memOutput test) $ go sym
  liftIO $ S.andPred sym inInput inOutput
  where
    go :: sym -> A.AccessData sym arch -> LLVM.MemM sym arch (S.Pred sym)
    go sym (A.WriteData _ v) = do b <- checkDefault defaultValue v
                                  liftIO $ S.notPred sym b
    go _ _ = error "Ill-formed concrete test"

addTest ::  ( A.Architecture arch
            , sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym
            )
        => F.Formula sym arch
        -> T.ConcreteTest sym arch
        -> T.Cegis sym arch ()
addTest form test = do
  liftIO . putStrLn $ "Adding test " ++ show test
  sym <- T.askSym
  st <- T.Cegis $ lift get
  let newTests = test : T.csTests st

  check <- simplifyWithTestLLVM form test
  -- liftIO . putStrLn $ "Resulting in: " ++ show check
  newCheck  <- liftIO $ S.andPred sym check (T.csCheck st)
  T.Cegis . lift $ put (st{T.csTests = newTests, T.csCheck = newCheck})

addTests :: ( A.Architecture arch
            , sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym)
        => F.Formula sym arch -> [T.ConcreteTest sym arch] -> T.Cegis sym arch ()
addTests form tests = mapM_ (addTest form) tests



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


stripMemLoc :: forall arch sym.
               A.Architecture arch
            => L.ArchState arch (S.SymExpr sym)
            -> L.ArchState arch (S.SymExpr sym)
stripMemLoc = MapF.filterWithKey (\l _ -> not (L.isMemLoc l))


-- | Given a formula and a counterexample provided from the solver, construct
-- the concrete test illustrated by the counterexample.
--
-- TODO: adapt this to deal with the case when the architecture has several memory locations (e.g. A32)
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
    -- putStrLn $ "Constructing test from " ++ show ctrExample

    -- substitute the non-memory locations from the test into the target
    -- formula. For non-memory locations, this gives us the testOutput.
    let testInput' = stripMemLoc @arch @sym ctrExample
    let el = CE.mkMemEvalLoc @arch sym testInput' memExpr
    F.Formula _ ctrExampleOut <- CE.evalFormulaMem' @arch sym targetFormula el
    let testOutput' = stripMemLoc @arch @sym ctrExampleOut

    -- to construct the memInput/memOutput, we need to find all memory locations that
    -- occur in the input/output counterexamples respectively and record the values they map to.
    let inputAddrMap  = MA.liveMemMap $ F.Formula @sym @arch (F.formParamVars targetFormula) ctrExample
        outputAddrMap = MA.liveMemMap $ F.Formula @sym @arch (F.formParamVars targetFormula) ctrExampleOut

    return $ T.ConcreteTest testInput' testOutput'
                          (Set.toList inputAddrMap) (Set.toList outputAddrMap)

-- | Construct an initial test from concrete values
initTest :: ( A.Architecture arch, sym ~ WE.ExprBuilder t st fs
            , CB.IsSymInterface sym
            )
         => sym
         -> F.Formula sym arch
         -> S.SymExpr sym (A.MemType arch)
         -> IO (T.ConcreteTest sym arch)
initTest sym f memExpr = do locExprs <- defaultLocExprs sym
                            mkTest sym f locExprs memExpr
