{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SemMC.Stochastic.Monad (
  Syn,
  loadInitialState,
  runSyn,
  Config(..),
  -- * Operations
  askGen,
  takeWork
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Reader as R
import Control.Monad.Trans ( MonadIO, liftIO )

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )

import qualified Lang.Crucible.Solver.Interface as CRU

import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction.Random as D

import SemMC.Architecture ( Architecture, Opcode, Operand )
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Formula.Load as F
import qualified SemMC.Worklist as WL
import SemMC.Util ( Witness(..) )

data SynEnv sym arch =
  SymEnv { seFormulas :: STM.TVar (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch))
         -- ^ All of the known formulas (base set + learned set)
         , seWorklist :: STM.TVar (WL.Worklist (Some (Opcode arch (Operand arch))))
         -- ^ Work items
         , seAllOpcodes :: [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
         , seRandomGen :: A.Gen
         , seSymBackend :: sym
         , seConfig :: Config
         }

newtype Syn sym arch a = Syn { unSyn :: R.ReaderT (SynEnv sym arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            R.MonadReader (SynEnv sym arch))

runSyn :: SynEnv sym arch -> Syn sym arch a -> IO a
runSyn e a = R.runReaderT (unSyn a) e

takeWork :: Syn sym arch (Maybe (Some (Opcode arch (Operand arch))))
takeWork = do
  wlref <- R.asks seWorklist
  liftIO $ STM.atomically $ do
    wl0 <- STM.readTVar wlref
    case WL.takeWork wl0 of
      Nothing -> return Nothing
      Just (work, rest) -> do
        STM.writeTVar wlref rest
        return (Just work)

askGen :: Syn sym arch A.Gen
askGen = R.asks seRandomGen

data Config = Config { baseSetDir :: FilePath
                     , learnedSetDir :: FilePath
                     }

loadInitialState :: (CRU.IsExprBuilder sym,
                     CRU.IsSymInterface sym,
                     Architecture arch,
                     D.ArbitraryOperands (Opcode arch) (Operand arch))
                 => Config
                 -> sym
                 -> [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
                 -> IO (SynEnv sym arch)
loadInitialState cfg sym allOpcodes = do
  baseSet <- F.loadFormulas sym allOpcodes (baseSetDir cfg)
  learnedSet <- F.loadFormulas sym allOpcodes (learnedSetDir cfg)
  let initialFormulas = MapF.union baseSet learnedSet
  fref <- STM.newTVarIO initialFormulas
  wlref <- STM.newTVarIO (makeWorklist allOpcodes initialFormulas)
  gen <- A.createGen
  return SymEnv { seFormulas = fref
                , seWorklist = wlref
                , seAllOpcodes = allOpcodes
                , seRandomGen = gen
                , seSymBackend = sym
                , seConfig = cfg
                }

makeWorklist :: [Some (Witness (F.BuildOperandList arch) ((Opcode arch) (Operand arch)))]
              -> MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula sym arch)
              -> WL.Worklist (Some (Opcode arch (Operand arch)))
makeWorklist allOps knownFormulas = WL.fromList []
