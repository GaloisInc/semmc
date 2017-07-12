{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A module for learning the input and output relations for instructions
module SemMC.Stochastic.IORelation (
  LearnConfig(..),
  IORelation(..),
  learn
  ) where

import qualified Control.Concurrent as C
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Word ( Word64 )

import qualified Data.Set.NonEmpty as NES
import Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture
import qualified SemMC.Formula.Parser as F
import SemMC.Util ( Witness(..) )

import SemMC.Stochastic.Monad ( Sym )
import qualified SemMC.Stochastic.Remote as R

data LearnConfig t arch =
  LearnConfig { testChan :: C.Chan (Maybe (R.TestCase (ArchState (Sym t) arch)))
              , resChan :: C.Chan (R.ResultOrError (ArchState (Sym t) arch))
              , backend :: Sym t
              , testGen :: IO (ArchState (Sym t) arch)
              , gen :: A.Gen
              , assemble :: Instruction arch -> BS.ByteString
              , nonce :: !Word64
              -- ^ Nonces for test vectors
              }

data IORelation arch =
  IORelation { inputs :: [Some (Location arch)]
             -- ^ Locations read by an instruction
             , outputs :: [Some (Location arch)]
             -- ^ Locations written by an instruction
             }

newtype M t arch a = M { runM :: St.StateT (LearnConfig t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            St.MonadState (LearnConfig t arch),
            MonadIO)

-- | Find the locations read from and written to by each instruction passed in
--
-- This is determined by observing the behavior of instructions on tests and
-- perturbing inputs randomly.
learn :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
      => LearnConfig t arch
      -> [Some (Witness (F.BuildOperandList arch) (Opcode arch (Operand arch)))]
      -> IO (M.Map (Some (Opcode arch (Operand arch))) (IORelation arch))
learn config ops = St.evalStateT (runM act) config
  where
    act = F.foldlM (\m (Some (Witness op)) -> testOpcode m op) M.empty ops

testOpcode :: (Architecture arch, R.MachineState (ArchState (Sym t) arch), D.ArbitraryOperands (Opcode arch) (Operand arch))
           => M.Map (Some (Opcode arch (Operand arch))) (IORelation arch)
           -> Opcode arch (Operand arch) sh
           -> M t arch (M.Map (Some (Opcode arch (Operand arch))) (IORelation arch))
testOpcode m op = do
  g <- St.gets gen
  mkTest <- St.gets testGen
  t0 <- liftIO mkTest
  insn <- liftIO $ D.randomInstruction g (NES.singleton (Some op))
  tests <- generateTestVariants insn t0
  tests' <- mapM (makeTestCase insn) tests
  tchan <- St.gets testChan
  liftIO $ mapM_ (C.writeChan tchan . Just) tests'
  return undefined

makeTestCase :: (Architecture arch, R.MachineState (ArchState (Sym t) arch))
             => Instruction arch
             -> ArchState (Sym t) arch
             -> M t arch (R.TestCase (ArchState (Sym t) arch))
makeTestCase i c = do
  tid <- St.gets nonce
  asm <- St.gets assemble
  St.modify' $ \s -> s { nonce = nonce s + 1 }
  return R.TestCase { R.testNonce = tid
                    , R.testContext = c
                    , R.testProgram = asm i
                    }

-- | Given an initial test state, generate all interesting variants on it.  The
-- idea is to see which outputs change when we tweak an input.
--
-- We learn the *inputs* set by starting with an initial test t0 and tweaking
-- each element in the state in turn.  For each tweaked input, we examine the
-- effects on the output states.  We want to avoid tweaking the registers that
-- are instantiated as operands to the instruction, as we expect those to cause
-- changes.  We really just need to learn which of the operands are inputs vs
-- outputs, and if there are any implicit arguments.
--
-- We learn the *outputs* set by comparing the tweaked input vs the output from
-- that test vector: all modified registers are in the output set.
generateTestVariants :: Instruction arch -> ArchState (Sym t) arch -> M t arch [ArchState (Sym t) arch]
generateTestVariants = undefined
