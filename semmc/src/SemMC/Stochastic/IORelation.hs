{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Text.IO as T
import           System.FilePath ( (</>) )
import qualified System.IO.Error as IOE

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Pair as P
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Data.Parameterized.Unfold as U

import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Architecture as A
import qualified SemMC.ConcreteState as CS
import qualified SemMC.Stochastic.Remote as R
import qualified SemMC.Worklist as WL

import SemMC.Stochastic.IORelation.Explicit ( generateExplicitInstruction,
                                              classifyExplicitOperands
                                            )
import           SemMC.Stochastic.IORelation.Implicit ( findImplicitOperands )
import           SemMC.Stochastic.IORelation.Parser
import           SemMC.Stochastic.IORelation.Types

data LearningConfig arch =
  LearningConfig { lcIORelationDirectory :: FilePath
                 , lcNumThreads :: Int
                 , lcAssemble :: A.Instruction arch -> LBS.ByteString
                 , lcTestGen :: IO (CS.ConcreteState arch)
                 , lcTimeoutSeconds :: Int
                 , lcTestRunner :: TestRunner arch
                 , lcLog :: C.Chan R.LogMessage
                 }

loadIORelations :: forall arch
                 . (CS.ConcreteArchitecture arch, D.ArbitraryOperands (A.Opcode arch) (A.Operand arch))
                => Proxy arch
                -> FilePath
                -> (forall sh . A.Opcode arch (A.Operand arch) sh -> FilePath)
                -> [Some (Witness U.UnfoldShape (A.Opcode arch (A.Operand arch)))]
                -> IO (MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch))
loadIORelations proxy relDir toFP ops = do
  F.foldlM (\m (Some (Witness oc)) -> IOE.catchIOError (addIfJust m oc) (\_ -> return m)) MapF.empty ops
  where
    addIfJust :: (U.UnfoldShape sh)
              => MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch)
              -> A.Opcode arch (A.Operand arch) sh
              -> IO (MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch))
    addIfJust m oc = do
      t <- T.readFile (relDir </> toFP oc)
      case readIORelation proxy t oc of
        Left _err -> return m
        Right iorel -> return (MapF.insert oc iorel m)

-- | Given a list of opcodes, load up all of the existing learned IORelations
-- and learn the rest through randomized testing.
--
-- This function spins up a user-specified number of worker threads.
--
-- Note that this function assumes that the lcIORelationDirectory already exists
learnIORelations :: forall arch
                  . (CS.ConcreteArchitecture arch, D.ArbitraryOperands (A.Opcode arch) (A.Operand arch))
                 => LearningConfig arch
                 -> Proxy arch
                 -> (forall sh . (A.Opcode arch (A.Operand arch)) sh -> FilePath)
                 -> [Some (Witness U.UnfoldShape (A.Opcode arch (A.Operand arch)))]
                 -> IO (MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch),
                        S.Set (Some (A.Opcode arch (A.Operand arch)), Maybe Int))
learnIORelations cfg proxy toFP ops = do
  rels0 <- loadIORelations proxy (lcIORelationDirectory cfg) toFP ops
  -- Remove IORelations we already have before we construct the worklist
  let someOps = map (unWitness (Proxy @arch)) ops
      opsWithoutRels = filter (\(Some op) -> MapF.notMember op rels0) someOps
  wlref <- STM.newTVarIO (WL.fromList opsWithoutRels)
  lrref <- STM.newTVarIO rels0
  errref <- STM.newTVarIO S.empty
  serializeChan <- C.newChan
  serializer <- A.async $ (serializeLearnedRelations (lcIORelationDirectory cfg) toFP serializeChan)
  A.link serializer
  let glv = GlobalLearningEnv { assemble = lcAssemble cfg
                              , resWaitSeconds = lcTimeoutSeconds cfg
                              , worklist = wlref
                              , learnedRelations = lrref
                              , serializationChan = serializeChan
                              , learningFailures = errref
                              }

  -- Spawn a bunch of worker threads to process the worklist in parallel
  A.replicateConcurrently_ (lcNumThreads cfg) $ do
    tChan <- C.newChan
    rChan <- C.newChan
    testRunner <- A.async $ lcTestRunner cfg tChan rChan (lcLog cfg)
    A.link testRunner
    nref <- STM.newTVarIO 0
    agen <- DA.createGen
    let lle = LocalLearningEnv { globalLearningEnv = glv
                               , gen = agen
                               , nonce = nref
                               , testGen = lcTestGen cfg
                               , testChan = tChan
                               , resChan = rChan
                               }
    runLearning lle learn

  -- Set the serialization channel down cleanly (giving it time to write out the
  -- last few relations)
  C.writeChan serializeChan Nothing
  () <- A.wait serializer

  (,) <$> STM.readTVarIO lrref <*> STM.readTVarIO errref
  where
    unWitness :: proxy arch -> Some (Witness U.UnfoldShape (A.Opcode arch (A.Operand arch))) -> Some (A.Opcode arch (A.Operand arch))
    unWitness _ (Some (Witness o)) = Some o

serializeLearnedRelations :: (CS.ConcreteArchitecture arch)
                          => FilePath
                          -> (forall sh . (A.Opcode arch (A.Operand arch)) sh -> FilePath)
                          -> C.Chan (Maybe (P.Pair (A.Opcode arch (A.Operand arch)) (IORelation arch)))
                          -> IO ()
serializeLearnedRelations dir toFP c = do
  mp <- C.readChan c
  case mp of
    Nothing -> return ()
    Just (P.Pair op iorel) -> do
      T.writeFile (dir </> toFP op) (printIORelation iorel)
      serializeLearnedRelations dir toFP c

-- | Find the locations read from and written to by each instruction passed in
--
-- This is determined by observing the behavior of instructions on tests and
-- perturbing inputs randomly.
learn :: (CS.ConcreteArchitecture arch, D.ArbitraryOperands (A.Opcode arch) (A.Operand arch))
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
            . (CS.ConcreteArchitecture arch, D.ArbitraryOperands (A.Opcode arch) (A.Operand arch))
           => A.Opcode arch (A.Operand arch) sh
           -> Learning arch (IORelation arch sh)
testOpcode op = do
  implicitOperands <- findImplicitOperands op
  insn <- generateExplicitInstruction (Proxy @arch) op (implicitLocations implicitOperands)
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


