{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Stochastic.Strata (
  SynEnv,
  Config(..),
  loadInitialState,
  stratifiedSynthesis
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import Data.Monoid
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( Instruction, Opcode, Operand )
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym )

import qualified SemMC.Stochastic.Classify as C
import qualified SemMC.Stochastic.Remote as R
import SemMC.Stochastic.Generalize ( generalize )
import SemMC.Stochastic.IORelation ( IORelation(..), OperandRef(..) )
import SemMC.Stochastic.Monad
import SemMC.Stochastic.Synthesize ( synthesize )

{-

Goal: Have a basic setup function to establish the environment (e.g., parse the
base set and build a worklist).  The caller should be able to pass that
environment to multiple threads running the strata function.  Key point: the
caller controls the number and placement of threads.

-}

stratifiedSynthesis :: (CS.ConcreteArchitecture arch, SynC arch)
                    => SynEnv t arch
                    -> IO (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
stratifiedSynthesis env0 = do
  A.replicateConcurrently_ (threadCount (seConfig env0)) $ do
    gen <- A.createGen
    let localEnv = LocalSynEnv { seGlobalEnv = env0
                               , seRandomGen = gen
                               }
    tChan <- C.newChan
    rChan <- C.newChan
    logChan <- C.newChan
    ssh <- A.async $ do
      _ <- R.runRemote (remoteHost (seConfig env0)) (machineState (seConfig env0)) tChan rChan logChan
      return ()
    A.link ssh
    runSyn localEnv strata
  STM.readTVarIO (seFormulas env0)

strata :: (CS.ConcreteArchitecture arch, SynC arch)
       => Syn t arch (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
strata = processWorklist >> generalize

processWorklist :: (CS.ConcreteArchitecture arch, SynC arch)
                => Syn t arch ()
processWorklist = do
  mwork <- takeWork
  case mwork of
    Nothing -> return ()
    Just (Some so) -> do
      res <- strataOne so
      case res of
        -- Timeout, so we can't learn it yet.  Come back later
        Nothing -> addWork so
        -- Success, record the formula
        Just formula -> recordLearnedFormula so formula
      processWorklist

-- | Attempt to learn a formula for the given opcode
--
-- Return 'Nothing' if we time out trying to find a formula
strataOne :: (CS.ConcreteArchitecture arch, SynC arch)
          => Opcode arch (Operand arch) sh
          -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
strataOne op = do
  instr <- instantiateInstruction op
  mprog <- synthesize instr
  case mprog of
    Nothing -> return Nothing
    Just prog -> strataOneLoop op instr (C.equivalenceClasses prog)

strataOneLoop :: (CS.ConcreteArchitecture arch, SynC arch)
              => Opcode arch (Operand arch) sh
              -> Instruction arch
              -> C.EquivalenceClasses arch
              -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
strataOneLoop op instr eqclasses = do
  cfg <- askConfig
  mprog <- synthesize instr
  case mprog of
    Nothing -> do
      -- We hit a timeout, so just try to build a formula based on what we have
      Just <$> finishStrataOne op instr eqclasses
    Just prog -> do
      meqclasses' <- C.classify prog eqclasses
      case meqclasses' of
        Nothing -> return Nothing
        Just eqclasses'
          | C.countPrograms eqclasses' > programCountThreshold cfg ->
            Just <$> finishStrataOne op instr eqclasses'
          | otherwise -> strataOneLoop op instr eqclasses'

finishStrataOne :: (CS.ConcreteArchitecture arch)
                => Opcode arch (Operand arch) sh
                -> Instruction arch
                -> C.EquivalenceClasses arch
                -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
finishStrataOne op instr eqclasses = do
  bestClass <- C.chooseClass eqclasses
  prog <- C.chooseProgram bestClass
  buildFormula op instr prog

-- | Construct a formula for the given instruction based on the selected representative program.
--
-- We pass in the opcode because we need the shape of the opcode in the type signature.
--
-- FIXME: Here we need the set of output locations (which could include implicit locations)
buildFormula :: (CS.ConcreteArchitecture arch)
             => Opcode arch (Operand arch) sh
             -> Instruction arch
             -> [Instruction arch]
             -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
buildFormula o i prog = do
  Just iorel <- opcodeIORelation o
  undefined i prog iorel

-- | Generate an arbitrary instruction for the given opcode.
--
-- Note that this function should strive to avoid generating instructions with
-- explicit operands that overlap with implicit operands.  It should also avoid
-- generating instructions that re-use registers.
--
-- We'll also want to return a mapping from locations to input values to be used
-- to inform the initial state generation.
instantiateInstruction :: forall arch sh t
                        . (CS.ConcreteArchitecture arch, SynC arch)
                       => Opcode arch (Operand arch) sh
                       -> Syn t arch (Instruction arch)
instantiateInstruction op = do
  gen <- askGen
  Just iorel <- opcodeIORelation op
  go gen (implicitOperands iorel)
  where
    -- Generate random instructions until we get one with explicit operands that
    -- do not overlap with implicit operands.
    go :: A.Gen -> S.Set (Some (CS.View arch)) -> Syn t arch (Instruction arch)
    go gen implicitOps = do
      target <- liftIO $ D.randomInstruction gen (NES.singleton (Some op))
      case target of
        D.Instruction op' ops
          | Just MapF.Refl <- MapF.testEquality op op'
          , fst (D.foldrOperandList (isImplicitOrReusedOperand (Proxy :: Proxy arch) implicitOps) (False, S.empty) ops) ->
            go gen implicitOps
          | otherwise -> return target

isImplicitOrReusedOperand :: (CS.ConcreteArchitecture arch, SynC arch)
                          => Proxy arch
                          -> S.Set (Some (CS.View arch))
                          -> ix
                          -> Operand arch tp
                          -> (Bool, S.Set (Some (Operand arch)))
                          -> (Bool, S.Set (Some (Operand arch)))
isImplicitOrReusedOperand proxy implicitLocs _ix operand (isIorR, seen)
  | isIorR || S.member (Some operand) seen = (True, seen)
  | Just loc <- CS.operandToView proxy operand
  , S.member loc implicitLocs = (True, seen)
  | otherwise = (isIorR, S.insert (Some operand) seen)

implicitOperands :: (CS.ConcreteArchitecture arch) => IORelation arch sh -> S.Set (Some (CS.View arch))
implicitOperands iorel =
  F.foldl' addImplicitLoc S.empty (inputs iorel <> outputs iorel)
  where
    addImplicitLoc s opref =
      case opref of
        ImplicitOperand sloc -> S.insert sloc s
        OperandRef {} -> s
