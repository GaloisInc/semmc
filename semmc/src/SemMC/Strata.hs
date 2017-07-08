{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Strata (
  Config(..),
  strata
  ) where

import qualified Control.Concurrent as C
import Control.Monad ( replicateM )
import qualified Control.Monad.Catch as E
import Data.Maybe ( catMaybes )
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Traversable as T
import System.FilePath ( (</>), (<.>) )

import Prelude

import qualified Data.Parameterized.Some as P
import qualified Data.Parameterized.Classes as P

import qualified Lang.Crucible.Solver.Interface as CRU

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( Architecture, Opcode, Operand )
import qualified SemMC.Statistics as Stats
import qualified SemMC.Worklist as WL
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as FP
import SemMC.Util ( Witness(..) )

-- Have this process the worklist in a single-threaded way, but for each work
-- item spawn off an Async thread to process it.  Wait once there are enough
-- outstanding threads.

data Config = Config { numThreads :: Int
                     , baseSetDir :: FilePath
                     , learnedSetDir :: FilePath
                     }

strata :: (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
       => Config
       -> sym
       -> proxy arch
       -> [P.Some (Witness (FP.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
       -> IO [(instr, semantics)]
strata cfg sym backend allOpcodes = do
  stats <- Stats.createStatisticsThread
  baseSet <- loadFormulas sym allOpcodes (baseSetDir cfg)
  learnedSet <- loadFormulas sym allOpcodes (learnedSetDir cfg)
  let initialFormulas = baseSet <> learnedSet
  worklist <- buildWorklist backend (map dropWitness allOpcodes) initialFormulas
  threads <- replicateM (numThreads cfg) (C.forkIO (processWorklist stats backend worklist))
  Stats.terminate stats
  return undefined

dropWitness :: P.Some (Witness c f) -> P.Some f
dropWitness (P.Some (Witness a)) = P.Some a

processWorklist :: (Architecture arch, D.ArbitraryOperands (Opcode arch) (Operand arch))
                => Stats.StatisticsThread
                -> proxy arch -- Backend opcode operand
                -> WL.Worklist (P.Some (Opcode arch (Operand arch)))
                -> IO ()
processWorklist _stats _backend wl = do
  mwork <- WL.takeWork wl
  case mwork of
    Nothing -> return ()
    Just (P.Some someOp) -> do
      -- FIXME: Put this into a state
      gen <- A.createGen
      Just target <- D.randomInstruction gen (S.singleton (D.SomeOpcode someOp))
      return ()

data FormulaSet sym arch = FormulaSet [(P.Some (F.ParameterizedFormula sym arch))]

instance Monoid (FormulaSet sym arch) where
  mempty = FormulaSet []
  mappend = undefined

data StratifyError = FormulaParseError String
  deriving (Show)

instance E.Exception StratifyError

loadFormulas :: forall sym arch
              . (CRU.IsExprBuilder sym, CRU.IsSymInterface sym, Architecture arch)
             => sym
             -> [P.Some (Witness (FP.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
             -> FilePath
             -> IO (FormulaSet sym arch)
loadFormulas sym allOpcodes formulaDir = do
  mFormulas <- T.forM allOpcodes $ \(P.Some (Witness oc)) -> fmap P.Some <$> readFormulaForOpcode oc
  return (FormulaSet (catMaybes mFormulas))
  where
    readFormulaForOpcode :: (FP.BuildOperandList arch sh)
                         => (Opcode arch) (Operand arch) sh
                         -> IO (Maybe (F.ParameterizedFormula sym arch sh))
    readFormulaForOpcode oc = do
      let fp = formulaDir </> P.showF oc <.> "sem"
      ef <- FP.readFormulaFromFile sym fp
      case ef of
        Left _ -> return Nothing
        Right f -> return (Just f)

buildWorklist :: forall arch sym proxy
               . proxy arch
              -> [P.Some (Opcode arch (Operand arch))]
              -> FormulaSet sym arch
              -> IO (WL.Worklist (P.Some (Opcode arch (Operand arch))))
buildWorklist _proxy allOps knownFormulas = do
  let workitems :: [P.Some (Opcode arch (Operand arch))]
      workitems = undefined allOps knownFormulas
  WL.newWorklist workitems

