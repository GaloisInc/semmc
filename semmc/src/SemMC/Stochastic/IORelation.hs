{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | A module for learning the input and output relations for instructions
module SemMC.Stochastic.IORelation (
  IORelation(..),
  OperandRef(..),
  LearningConfig(..),
  loadIORelations,
  learnIORelations,
  readIORelation,
  printIORelation
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.Monoid
import Data.Proxy ( Proxy(..) )
import qualified Data.Text.IO as T

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.Witness ( Witness(..) )
import qualified Data.Parameterized.Unfold as U

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Worklist as WL

import SemMC.Stochastic.IORelation.Explicit ( generateExplicitInstruction,
                                              classifyExplicitOperands
                                            )
import SemMC.Stochastic.IORelation.Implicit ( findImplicitOperands )
import SemMC.Stochastic.IORelation.Parser
import SemMC.Stochastic.IORelation.Types
import qualified SemMC.Stochastic.Remote as R

data LearningConfig arch =
  LearningConfig { lcIORelationDirectory :: FilePath
                 , lcNumThreads :: Int
                 , lcAssemble :: Instruction arch -> BS.ByteString
                 , lcTestGen :: IO (CS.ConcreteState arch)
                 , lcMachineState :: R.MachineState (CS.ConcreteState arch)
                 , lcTimeoutSeconds :: Int
                 , lcRemoteHost :: String
                 }

loadIORelations :: forall arch
                 . (CS.ConcreteArchitecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                => Proxy arch
                -> (forall sh . Opcode arch (Operand arch) sh -> FilePath)
                -> [Some (Witness U.UnfoldShape (Opcode arch (Operand arch)))]
                -> IO (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
loadIORelations proxy toFP ops = do
  F.foldlM (\m (Some (Witness oc)) -> addIfJust m oc) MapF.empty ops
  where
    addIfJust :: (U.UnfoldShape sh)
              => MapF.MapF (Opcode arch (Operand arch)) (IORelation arch)
              -> Opcode arch (Operand arch) sh
              -> IO (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
    addIfJust m oc = do
      t <- T.readFile (toFP oc)
      case readIORelation proxy t oc of
        Nothing -> return m
        Just iorel -> return (MapF.insert oc iorel m)

-- | Given a list of opcodes, load up all of the existing learned IORelations
-- and learn the rest through randomized testing.
--
-- This function spins up a user-specified number of worker threads.
learnIORelations :: forall arch
                  . (CS.ConcreteArchitecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                 => LearningConfig arch
                 -> Proxy arch
                 -> (forall sh . (Opcode arch (Operand arch)) sh -> FilePath)
                 -> [Some (Witness U.UnfoldShape (Opcode arch (Operand arch)))]
                 -> IO (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
learnIORelations cfg proxy toFP ops = do
  rels0 <- loadIORelations proxy toFP ops
  wlref <- STM.newTVarIO (WL.fromList (map (unWitness (Proxy :: Proxy arch)) ops))
  lrref <- STM.newTVarIO rels0
  let glv = GlobalLearningEnv { assemble = lcAssemble cfg
                              , resWaitSeconds = lcTimeoutSeconds cfg
                              , worklist = wlref
                              , learnedRelations = lrref
                              }
  A.replicateConcurrently_ (lcNumThreads cfg) $ do
    tChan <- C.newChan
    rChan <- C.newChan
    logChan <- C.newChan
    ssh <- A.async $ do
      _ <- R.runRemote (lcRemoteHost cfg) (lcMachineState cfg) tChan rChan logChan
      return ()
    A.link ssh
    nref <- STM.newTVarIO 0
    agen <- A.createGen
    let lle = LocalLearningEnv { globalLearningEnv = glv
                               , gen = agen
                               , nonce = nref
                               , testGen = lcTestGen cfg
                               , testChan = tChan
                               , resChan = rChan
                               }
    runLearning lle learn
  STM.readTVarIO lrref
  where
    unWitness :: proxy arch -> Some (Witness U.UnfoldShape (Opcode arch (Operand arch))) -> Some (Opcode arch (Operand arch))
    unWitness _ (Some (Witness o)) = Some o

-- | Find the locations read from and written to by each instruction passed in
--
-- This is determined by observing the behavior of instructions on tests and
-- perturbing inputs randomly.
learn :: (CS.ConcreteArchitecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
      => Learning arch ()
learn = do
  mop <- nextOpcode
  case mop of
    Nothing -> return ()
    Just (Some op) -> do
      rel <- testOpcode op
      recordLearnedRelation op rel
      learn

testOpcode :: forall arch sh
            . (CS.ConcreteArchitecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
           => Opcode arch (Operand arch) sh
           -> Learning arch (IORelation arch sh)
testOpcode op = do
  implicitOperands <- findImplicitOperands op
  insn <- generateExplicitInstruction (Proxy :: Proxy arch) op (implicitLocations implicitOperands)
  case insn of
    D.Instruction op' operandList
      | Just P.Refl <- P.testEquality op op' -> do
        explicitOperands <- classifyExplicitOperands op operandList
        return (implicitOperands <> explicitOperands)
      | otherwise -> L.error ("randomInstruction returned an instruction with the wrong opcode: " ++ P.showF op')

-- | Collect all of the locations that are read from or written to implicitly
implicitLocations :: IORelation arch sh -> [Some (CS.View arch)]
implicitLocations ior = foldr collectImplicits (foldr collectImplicits [] (inputs ior)) (outputs ior)
  where
    collectImplicits opRef acc =
      case opRef of
        ImplicitOperand sloc -> sloc : acc
        OperandRef {} -> acc




{-

We want to generate tests to determine, for each register operand, if it is
input, output, or both.

We'll start off with a single initial register state passed to
generateTestVariants.  All of the variants will be derived from that state.

We need to walk down the operand list and, for each register operand (r0),
generate a set of new states with that register (r0) value tweaked.  For those
nonces, if other registers change in the post state, r0 was an input register.
Registers that change values in the post state are outputs.  If registers that
are not mentioned in the operand list change, they are implicit outputs.  If
changing a register not in the operand list causes a change in outputs, it is an
implicit input.


-}


