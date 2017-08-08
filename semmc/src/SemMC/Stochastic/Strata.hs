{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SemMC.Stochastic.Strata (
  SynEnv,
  Config(..),
  loadInitialState,
  stratifiedSynthesis
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent as C
import qualified Control.Exception as C
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import Data.Maybe ( catMaybes, isNothing )
import Data.Monoid
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.TraversableFC ( foldrFC )
import qualified Lang.Crucible.Solver.Interface as C

import Data.Parameterized.ShapedList ( ShapedList, indexShapedList )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( Instruction, Opcode, Operand, Location, operandToLocation )
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Instantiate as F
import           SemMC.Symbolic ( Sym )

import qualified SemMC.Stochastic.Classify as C
import SemMC.Stochastic.Generalize ( generalize )
import SemMC.Stochastic.IORelation ( IORelation(..), OperandRef(..) )
import qualified SemMC.Stochastic.Remote as R
import SemMC.Stochastic.Monad
import SemMC.Stochastic.Pseudo ( SynthInstruction )
import SemMC.Stochastic.Synthesize ( synthesize )

{-

Goal: Have a basic setup function to establish the environment (e.g., parse the
base set and build a worklist).  The caller should be able to pass that
environment to multiple threads running the strata function.  Key point: the
caller controls the number and placement of threads.

-}

stratifiedSynthesis :: forall arch t
                     . (CS.ConcreteArchitecture arch, SynC arch)
                    => SynEnv t arch
                    -> IO (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
stratifiedSynthesis env0 = do
  A.replicateConcurrently_ (threadCount (seConfig env0)) $ do
    gen <- A.createGen
    tChan <- C.newChan
    rChan <- C.newChan
    logChan <- C.newChan
    testRunner' <- A.async $ testRunner (seConfig env0) tChan rChan logChan
    A.link testRunner'
    let localEnv = LocalSynEnv { seGlobalEnv = env0
                               , seRandomGen = gen
                               , seRunTest = runTest tChan rChan
                               }
    runSyn localEnv strata `C.finally` A.cancel testRunner'
  STM.readTVarIO (seFormulas env0)
  where
    -- A naive test runner: it's synchronous, and dies on test
    -- failures. Will need to be improved later.
    runTest tChan rChan c p = liftIO $ do
      let nonce = 0
      C.writeChan tChan (Just (R.TestCase nonce c p))
      r <- C.readChan rChan
      case r of
        R.TestSuccess tr
          | R.resultNonce tr == nonce -> return $ R.resultContext tr
        _ -> L.error "Unexpected test result in Strata.runTest!"

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

finishStrataOne :: (CS.ConcreteArchitecture arch, SynC arch)
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
buildFormula :: (CS.ConcreteArchitecture arch, SynC arch)
             => Opcode arch (Operand arch) sh
             -> Instruction arch
             -> [SynthInstruction arch]
             -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
buildFormula o i prog = do
  Just iorel <- opcodeIORelation o
  mfrms <- mapM instantiateFormula prog
  case any isNothing mfrms of
    True -> fail ""
    False -> do
      let formulas = catMaybes mfrms
      withSymBackend $ \sym -> do
        progFormula <- liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula formulas
        case i of
          D.Instruction opcode operands
            | Just MapF.Refl <- MapF.testEquality opcode o -> do
                -- Now, for all of the outputs (implicit and explicit) in the target
                -- instruction, look up the corresponding formula in `progFormula`
                extractFormula opcode operands progFormula iorel
            | otherwise -> L.error ("Unexpected opcode mismatch: " ++ MapF.showF o)

extractFormula :: forall arch t sh
                . (CS.ConcreteArchitecture arch, SynC arch)
               => Opcode arch (Operand arch) sh
               -> ShapedList (Operand arch) sh
               -> F.Formula (Sym t) arch
               -> IORelation arch sh
               -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
extractFormula opc ops progForm iorel = go F.emptyFormula (F.toList (outputs iorel))
  where
    go acc [] = parameterizeFormula opc ops iorel acc
    go acc (out:rest) =
      case out of
        ImplicitOperand _ -> L.error "Implicit output operands not supported yet"
        OperandRef (Some idx) -> do
          let operand = indexShapedList ops idx
              Just loc = operandToLocation (Proxy @arch) operand
              Just expr = MapF.lookup loc (F.formDefs progForm)
          go (defineLocation loc expr acc) rest

defineLocation :: (CS.ConcreteArchitecture arch)
               => Location arch tp
               -> C.SymExpr sym tp
               -> F.Formula sym arch
               -> F.Formula sym arch
defineLocation loc expr acc = acc { F.formDefs = MapF.insert loc expr (F.formDefs acc)
                                  , F.formUses = S.insert (Some loc) (F.formUses acc)
                                  }

-- | Based on the iorelation, identify the inputs of the opcode (and the
-- corresponding operands).  For each input operand backed by a location, create
-- a boundvar and substitute them for the corresponding expressions in formulas.
parameterizeFormula :: Opcode arch (Operand arch) sh
                    -> ShapedList (Operand arch) sh
                    -> IORelation arch sh
                    -> F.Formula (Sym t) arch
                    -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
parameterizeFormula opcode oplist iorel f = undefined

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
          , fst (foldrFC (isImplicitOrReusedOperand (Proxy :: Proxy arch) implicitOps) (False, S.empty) ops) ->
            go gen implicitOps
          | otherwise -> return target

isImplicitOrReusedOperand :: (CS.ConcreteArchitecture arch, SynC arch)
                          => Proxy arch
                          -> S.Set (Some (CS.View arch))
                          -> Operand arch tp
                          -> (Bool, S.Set (Some (Operand arch)))
                          -> (Bool, S.Set (Some (Operand arch)))
isImplicitOrReusedOperand proxy implicitViews operand (isIorR, seen)
  | isIorR || S.member (Some operand) seen = (True, seen)
  | Just (CS.SemanticView { CS.semvView = view }) <- CS.operandToSemanticView proxy operand
  , S.member (Some view) implicitViews = (True, seen)
  | otherwise = (isIorR, S.insert (Some operand) seen)

implicitOperands :: (CS.ConcreteArchitecture arch) => IORelation arch sh -> S.Set (Some (CS.View arch))
implicitOperands iorel =
  F.foldl' addImplicitLoc S.empty (inputs iorel <> outputs iorel)
  where
    addImplicitLoc s opref =
      case opref of
        ImplicitOperand sloc -> S.insert sloc s
        OperandRef {} -> s
