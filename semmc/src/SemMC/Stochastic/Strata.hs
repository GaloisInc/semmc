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
  stratifiedSynthesis,
  naiveRunTest
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent as C
import qualified Control.Exception as C
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import           Data.Maybe ( catMaybes, isNothing )
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.ShapedList as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC ( foldrFC )

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import           SemMC.Architecture ( Instruction, Opcode, Operand, Location, allLocations, OperandType, operandToLocation, operandType, locationType )
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Instantiate as F
import           SemMC.Symbolic ( Sym )

import qualified SemMC.Stochastic.Classify as C
import           SemMC.Stochastic.Extract ( extractFormula )
import           SemMC.Stochastic.Generalize ( generalize )
import           SemMC.Stochastic.IORelation ( IORelation(..), OperandRef(..) )
import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.Pseudo ( SynthInstruction )
import qualified SemMC.Stochastic.Remote as R
import           SemMC.Stochastic.Synthesize ( synthesize )
import qualified SemMC.Stochastic.IORelation.Types as I

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
                               , seRunTest = naiveRunTest tChan rChan
                               }
    runSyn localEnv strata `C.finally` A.cancel testRunner'
  STM.readTVarIO (seFormulas env0)

-- | A naive test runner: it's synchronous, and dies on test
-- failures.
--
-- Will need to be improved later. There is more sophisticated test
-- runner code in the @IORelation@ modules.
naiveRunTest :: C.Chan (Maybe (I.TestCase arch))
             -> C.Chan (I.ResultOrError arch)
             -> Test arch
             -> [Instruction arch]
             -> Syn t arch (Test arch)
naiveRunTest tChan rChan c p = liftIO $ do
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
              -> RegisterizedInstruction arch
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
                -> RegisterizedInstruction arch
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
             -> RegisterizedInstruction arch
             -> [SynthInstruction arch]
             -> Syn t arch (F.ParameterizedFormula (Sym t) arch sh)
buildFormula o i prog = do
  Just iorel <- opcodeIORelation o
  mfrms <- mapM instantiateFormula prog
  case any isNothing mfrms of
    True -> fail ("Failed to instantiate program: " ++ show prog)
    False -> do
      let formulas = catMaybes mfrms
      withSymBackend $ \sym -> do
        progFormula <- liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula formulas
        case riInstruction i of
          D.Instruction opcode operands
            | Just MapF.Refl <- MapF.testEquality opcode o -> do
                -- Now, for all of the outputs (implicit and explicit) in the target
                -- instruction, look up the corresponding formula in `progFormula`
                extractFormula i opcode operands progFormula iorel
            | otherwise -> L.error ("Unexpected opcode mismatch: " ++ MapF.showF o)

-- | Generate an arbitrary instruction for the given opcode.
--
-- The "registerized" instruction selects a location in the machine state to
-- stand in for immediates.  We need this to extract formulas (and to learn
-- formulas for instructions with immediates).
--
-- Note that this function should strive to avoid generating instructions with
-- explicit operands that overlap with implicit operands.  It should also avoid
-- generating instructions that re-use registers.
--
-- There is an additional restriction where we do not want to use implicit or
-- explicit operands as stand-in locations for immediates.
--
-- This function can fail (with an error) if we do not have enough registers to
-- act as stand ins for immediates; that seems highly unlikely, but something to
-- watch out for.
instantiateInstruction :: forall arch sh t
                        . (CS.ConcreteArchitecture arch, SynC arch)
                       => Opcode arch (Operand arch) sh
                       -> Syn t arch (RegisterizedInstruction arch)
instantiateInstruction op = do
  gen <- askGen
  Just iorel <- opcodeIORelation op
  go gen (implicitOperands iorel)
  where
    -- Generate random instructions until we get one with explicit operands that
    -- do not overlap with implicit operands.
    go :: A.Gen -> S.Set (Some (CS.View arch)) -> Syn t arch (RegisterizedInstruction arch)
    go gen implicitOps = do
      target <- liftIO $ D.randomInstruction gen (NES.singleton (Some op))
      case target of
        D.Instruction op' ops
          | Just MapF.Refl <- MapF.testEquality op op' ->
            let (isReusedOrImplicitOp, explicitLocs) = foldrFC (isImplicitOrReusedOperand (Proxy :: Proxy arch) implicitOps) (False, S.empty) ops
            in case isReusedOrImplicitOp of
              True -> go gen implicitOps
              False -> do
                -- If there is a literal operand, find a register that is neither
                -- an implicit operand nor an explicit operand to stand in for the
                -- immediate(s).
                let s0 = (MapF.empty, allLocations @(Location arch))
                let usedLocs = S.union (S.map liftSomeView implicitOps) (S.foldr (liftSomeOperand (Proxy @arch)) S.empty explicitLocs)
                let (litLocs, _) = SL.foldrFCIndexed (assignLiterals usedLocs) s0 ops
                return RI { riInstruction = target
                          , riOpcode = op'
                          , riOperands = ops
                          , riLiteralLocs = litLocs
                          }
          | otherwise -> L.error ("Invalid opcode: " ++ P.showF op ++ " vs " ++ P.showF op')

liftSomeView :: Some (CS.View arch) -> Some (Location arch)
liftSomeView (Some (CS.View _ loc)) = Some loc

liftSomeOperand :: (CS.ConcreteArchitecture arch)
                => proxy arch
                -> Some (Operand arch)
                -> S.Set (Some (Location arch))
                -> S.Set (Some (Location arch))
liftSomeOperand proxy (Some op) s =
  case operandToLocation proxy op of
    Nothing -> s
    Just loc -> S.insert (Some loc) s

-- | For each literal, assign a location in the machine state to stand in for
-- it.
--
-- Note: we determine if an operand is a literal by trying to convert it to a
-- location.  If we can't convert it to a location, it is a literal.
--
-- This will throw an error if there isn't a free register to represent all of
-- the literals.  That seems extremely unlikely, but is something to potentially
-- watch out for.
assignLiterals :: forall arch sh tp
                . (CS.ConcreteArchitecture arch)
               => S.Set (Some (Location arch))
               -> SL.Index sh tp
               -> Operand arch tp
               -> (MapF.MapF (LiteralRef arch sh) (Location arch), [Some (Location arch)])
               -> (MapF.MapF (LiteralRef arch sh) (Location arch), [Some (Location arch)])
assignLiterals usedLocs ix op acc@(m, locs) =
  case operandToLocation (Proxy @arch) op of
    Just _ -> acc
    Nothing ->
      let (locs', loc) = findUnusedLocation (Proxy @arch) usedLocs op locs
      in (MapF.insert (LiteralRef ix) loc m, locs')

findUnusedLocation :: (CS.ConcreteArchitecture arch)
                   => proxy arch
                   -> S.Set (Some (Location arch))
                   -> Operand arch tp
                   -> [Some (Location arch)]
                   -> ([Some (Location arch)], Location arch (OperandType arch tp))
findUnusedLocation proxy usedLocs op locs =
  case locs of
    [] -> L.error "Not enough locations to find a virtual literal location"
    (Some loc : rest)
      | Just P.Refl <- P.testEquality (locationType loc) (operandType proxy op)
      , not (S.member (Some loc) usedLocs) -> (rest, loc)
      | otherwise -> findUnusedLocation proxy usedLocs op rest

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
