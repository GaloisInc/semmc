{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Strata (
  Config(..),
  strata
  ) where

import Control.Monad.Trans ( liftIO )
import qualified Data.Set as S

import Prelude

import Data.Parameterized.Some ( Some(..) )

import qualified Lang.Crucible.Solver.Interface as CRU

import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( Architecture, Opcode, Operand )
import qualified SemMC.Statistics as Stats
import qualified SemMC.Formula.Parser as FP
import SemMC.Util ( Witness(..) )
import SemMC.Stochastic.Monad

-- Have this process the worklist in a single-threaded way, but for each work
-- item spawn off an Async thread to process it.  Wait once there are enough
-- outstanding threads.


{-

Goal: Have a basic setup function to establish the environment (e.g., parse the
base set and build a worklist).  The caller should be able to pass that
environment to multiple threads running the strata function.  Key point: the
caller controls the number and placement of threads.

-}

strata :: (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
       => Config
       -> [Some (Witness (FP.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
       -> Syn sym arch [(instr, semantics)]
strata allOpcodes = do
  -- stats <- Stats.createStatisticsThread
--  threads <- replicateM 1 (C.forkIO (processWorklist stats))
--  liftIO $ Stats.terminate stats
  return undefined

processWorklist :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                => Stats.StatisticsThread
                -> Syn sym arch ()
processWorklist _stats = do
  mwork <- takeWork
  case mwork of
    Nothing -> return ()
    Just so -> do
      gen <- askGen
      Just _target <- liftIO $ D.randomInstruction gen (S.singleton so)
      return ()



