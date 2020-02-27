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
  , CegisParams
  , mkCegisParams
  , CegisResult(..)
  , cegis
  ) where

import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Parameterized.Map as MapF

import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Backend as CB
import qualified What4.Interface as S
import qualified What4.Expr as WE
import qualified What4.Protocol.Online as WPO

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula
import qualified SemMC.Synthesis.Template as T

import qualified SemMC.Synthesis.Cegis.MemAccesses as MA
import qualified SemMC.Synthesis.Cegis.LLVMMem as LLVM
import           SemMC.Synthesis.Cegis.Types
import qualified SemMC.Synthesis.Cegis.EvalFormula as CE
import qualified SemMC.Synthesis.Cegis.Tests as CT


data CegisResult sym arch = CegisUnmatchable [ConcreteTest sym arch]
                          -- ^ There is no way to make the target and the
                          -- candidate do the same thing. This is proven by the
                          -- set of tests.
                          | CegisEquivalent [A.Instruction arch]
                          -- ^ This series of instructions, an instantiated form
                          -- of the candidate instructions given, has the same
                          -- behavior as the target formula.

cegis' :: forall arch t solver fs.
          (A.Architecture arch, A.ArchRepr arch, A.Architecture (T.TemplatedArch arch)
          , WPO.OnlineSolver t solver
          , CB.IsSymInterface (CBO.OnlineBackend t solver fs)
          )
       => [T.TemplatedInstructionFormula (CBO.OnlineBackend t solver fs) arch]
       -- ^ The trial instructions.
       -> Formula (CBO.OnlineBackend t solver fs) arch
       -- ^ A formula representing the sequence of trial instructions.
       -> Cegis (CBO.OnlineBackend t solver fs) arch (CegisResult (CBO.OnlineBackend t solver fs) arch)
cegis' trial trialFormula = do
  sym <- askSym
  check <- askCheck
  insns' <- liftIO $ checkSat sym check (tryExtractingConcreteWithParamsCheck sym trial)
  case insns' of
    Nothing -> do
        tests <- askTests
        return (CegisUnmatchable tests)
    Just (insns,paramsCheck) -> do

      -- For the concrete immediate values that the solver just gave us, are the
      -- target formula and the concrete candidate instructions equivalent for
      -- all symbolic machine states?
      liftIO . putStrLn $ "TRIAL INSTRUCTIONS:\n\t" ++ show insns

      -- TODO: are these formStripIP's necessary?
      filledInFormula <- formStripIP <$> CE.condenseInstructions insns
      targetFormula <- askTarget

      -- We want to check whether the formulas are equivalent under certain conditions:
      -- (1) We never access memory that might overflow the address space
      -- (2) We are not generating an existing test case
      --
      -- This could potentially return @Equivalence@ not because the two
      -- formulae are actually equivalent, but because these side conditions are
      -- unsatisfiable. This situation will be caught e.g. by the function
      -- 'synthesizeAndCheck' in SemMC.Synthesis.Testing.
      existingTests <- askTests
      equiv <- liftIO $ formulasEquivSymWithCondition sym
                                         (checkBounds @arch sym)
                                         (checkNoDuplicates @arch sym existingTests)
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
          return . CegisEquivalent $ map templInsnToDism insns
        DifferentBehavior ctrExample -> do
          memExpr <- askMemExpr
          newTest <- liftIO $ CT.mkTest sym targetFormula ctrExample memExpr

          liftIO . putStrLn $ "=============Added counterexample:=============== \n" ++ show newTest

          CT.addTest trialFormula newTest
          -- Add the params check for future iterations of cegis
          addModelOverlapCheck paramsCheck
          cegis' trial trialFormula

cegis :: forall arch sym t solver fs.
        ( A.Architecture arch, A.ArchRepr arch, A.Architecture (T.TemplatedArch arch)
        , WPO.OnlineSolver t solver, sym ~ CBO.OnlineBackend t solver fs
        , CB.IsSymInterface sym
        )
      => CegisParams sym arch
      -- ^ Parameters not specific to the candidate. See 'CegisParams' for
      -- details.
      -> [ConcreteTest sym arch]
      -- ^ The concrete tests
      -> [T.TemplatedInstructionFormula sym arch]
      -- ^ The candidate template program.
      -> IO (CegisResult sym arch)
cegis params tests trial = do
  let sym = cpSym params

  -- Don't want to consider IP here
  trialFormula <- formStripIP <$> condenseFormulas sym (map T.tifFormula trial)

  liftIO . putStrLn $ "=============\nTRIAL FORMULA:\n============\n" ++ show trialFormula

  let cst = emptyCegisState sym
  runCegis params cst $ do CT.addTests trialFormula tests
                           cegis' trial trialFormula

-------------------------------------------------------


-- For each memory access @e@ in the expression, assert that 0 < i < 2^64-2^12.
-- This ensures that we will never overflow the address space, assuming that the
-- largest read/write is no more than e.g. 8 bytes = 64 bits.
checkBounds :: forall arch sym tp t st fs.
             (S.IsExprBuilder sym, A.Architecture arch, sym ~ WE.ExprBuilder t st fs)
          => sym
          -> S.SymExpr sym tp
          -> IO (S.Pred sym)
checkBounds sym e | dat <- MA.liveMemInExpr @arch e = do
  andPred sym dat $ \case
    A.WriteData i _ -> inBounds i
    A.ReadData  i   -> inBounds i
  where
    -- 0 < i < 2^64-1 - 2^12
    inBounds i = do
      bv0 <- S.bvLit sym rw 0
      bv4096 <- S.bvLit sym rw (-4096)
      iGt0 <-S.bvUlt sym bv0 i
      iLtMinus4096 <- S.bvUlt sym i bv4096
      S.andPred sym iGt0 iLtMinus4096

    rw = S.knownNat @(A.RegWidth arch)


-- Produces a check that we do not generate duplicate tests: check that for all
-- tests, the LocExprs given is not symbolically equal to the test input.
checkNoDuplicates :: forall arch sym s t fs.
             (S.IsExprBuilder sym, A.Architecture arch, CB.IsSymInterface sym, sym ~ WE.ExprBuilder s t fs)
          => sym
          -> [ConcreteTest sym arch]
          -> LocExprs sym (L.Location arch)
          -> IO (S.Pred sym)
checkNoDuplicates sym tests lExprs = andPred sym tests $ \test -> do
    tInput <- checkTestInput (testInput test)
    mInput <- checkMemInput (memInput test)
    S.notPred sym =<< S.andPred sym tInput mInput
  where
    -- for all l \in domain(test), test(l) = lExprs(l)
    checkTestInput :: LocExprs sym (L.Location arch) -> IO (S.Pred sym)
    checkTestInput test = andPred sym (MapF.toList test) $ \(MapF.Pair l testL) ->
      case MapF.lookup l lExprs of
        Nothing   -> return $ S.truePred sym
        Just lExp -> S.isEq sym lExp testL

    -- for all @WriteData i v \in memInput(test)@, lExprs(l)[i] = v
    checkMemInput test | Just memE <- getMemExpr = andPred sym test $ \case
      A.ReadData _ -> return $ S.truePred sym
      A.WriteData i v -> do
        actuallyIs <- LLVM.readMemIO @arch sym (S.bvWidth v) i memE
        S.isEq sym actuallyIs v
    -- if there is no memory in the formula
    checkMemInput _ | otherwise = return $ S.truePred sym

    getMemExpr :: Maybe (S.SymExpr sym (A.MemType arch))
    getMemExpr | L.MemLoc w l:_ <- L.memLocation @(L.Location arch)
               , Just S.Refl <- S.testEquality w (S.knownNat @(A.RegWidth arch))
               = MapF.lookup l lExprs
               | otherwise = Nothing


-- | Conjoin the given predicate to the 'csCheck' field of the 'CegisState'.
addModelOverlapCheck :: forall sym t st fs arch.
                      ( sym ~ CBO.OnlineBackend t st fs
                      , A.Architecture arch
                      , A.Architecture (T.TemplatedArch arch)
                      )
                     => S.Pred sym
                     -> Cegis sym arch ()
addModelOverlapCheck paramsCheck = do
  sym <- askSym
  oldCheck <- askCheck
  newCheck <- liftIO $ S.andPred sym oldCheck paramsCheck
  putCheck newCheck
