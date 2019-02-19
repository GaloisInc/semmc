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
  , CT.initTest
  , CegisParams
  , cpMem
  , mkCegisParams
  , CegisResult(..)
  , cegis
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.State  (get, put )
import           Control.Monad.Trans.Class  (lift)
import           Data.Foldable
import           Data.Maybe ( fromJust, listToMaybe )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Set as Set

import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Backend as CB
import qualified What4.Interface as S
import qualified What4.Expr as WE
import qualified What4.Protocol.Online as WPO

import           SemMC.Architecture
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula
import           SemMC.Synthesis.Template

import qualified SemMC.Synthesis.Cegis.MemAccesses as MA
import qualified SemMC.Synthesis.Cegis.LLVMMem as LLVM
import           SemMC.Synthesis.Cegis.Types
import qualified SemMC.Synthesis.Cegis.EvalFormula as CE
import qualified SemMC.Synthesis.Cegis.Tests as CT



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
  check <- askCheck
  liftIO . putStrLn $ "Equality tests: " ++ show check
  insns <- liftIO $ checkSat sym check (tryExtractingConcrete trial)

  case insns of
    Nothing -> do
        tests <- askTests
        return (CegisUnmatchable tests)
    Just insns' -> do
      -- For the concrete immediate values that the solver just gave us, are the
      -- target formula and the concrete candidate instructions equivalent for
      -- all symbolic machine states?
      liftIO . putStrLn $ "TRIAL INSTRUCTIONS:\n\t" ++ show insns'

      -- TODO: are these formStripIP's necessary?
      filledInFormula <- formStripIP <$> condenseInstructions insns'
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
          newTest <- liftIO $ CT.mkTest sym targetFormula ctrExample memExpr

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
            CT.addTest trialFormula newTest
            cegis' trial trialFormula

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

  -- Don't want to consider IP here
  trialFormula <- formStripIP <$> condenseFormulas sym (map tifFormula trial)

  liftIO . putStrLn $ "=============\nTRIAL FORMULA:\n============\n" ++ show trialFormula

  -- Is this candidate satisfiable for the concrete tests we have so far? At
  -- this point, the machine state is concrete, but the immediate values of the
  -- instructions are symbolic. If the candidate is satisfiable for the tests,
  -- the SAT solver will give us values for the templated immediates in order to
  -- make the tests pass.

--  liftIO . putStrLn $ "Target formula: " ++ show (cpTarget params)
  let cst = mkCegisState sym
  runCegis params cst $ do CT.addTests trialFormula tests
                           cegis' trial trialFormula
