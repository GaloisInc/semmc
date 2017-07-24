{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module SemMC.Stochastic.IORelation.Types (
  IORelation(..),
  LocalLearningEnv(..),
  GlobalLearningEnv(..),
  TypedLocation(..),
  TestBundle(..),
  ExplicitFact(..),
  ImplicitFact(..),
  OperandRef(..),
  LearningException(..),
  ResultIndex(..),
  emptyResultIndex,
  Learning,
  askTestChan,
  askResultChan,
  askGen,
  askBackend,
  askAssembler,
  mkRandomTest,
  nextNonce,
  nextOpcode,
  recordLearnedRelation,
  runLearning,
  timeout
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Catch as E
import qualified Control.Monad.Reader as Rd
import Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString as BS
import Data.Int ( Int32 )
import qualified Data.Map.Strict as M
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import Data.Typeable ( Typeable )
import Data.Word ( Word64 )
import qualified System.Timeout as T

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D

import SemMC.Architecture
import SemMC.Stochastic.Monad ( Sym )
import qualified SemMC.Stochastic.Remote as R
import qualified SemMC.Worklist as WL

data GlobalLearningEnv arch =
  GlobalLearningEnv { assemble :: Instruction arch -> BS.ByteString
                    , resWaitSeconds :: Int
                    -- ^ Number of seconds to wait to receive all of the results over the 'resChan'
                    , worklist :: STM.TVar (WL.Worklist (Some (Opcode arch (Operand arch))))
                    , learnedRelations :: STM.TVar (MapF.MapF (Opcode arch (Operand arch)) (IORelation arch))
                    }

data LocalLearningEnv t arch =
  LocalLearningEnv { globalLearningEnv :: GlobalLearningEnv arch
                   , testChan :: C.Chan (Maybe (R.TestCase (ArchState (Sym t) arch)))
                   , resChan :: C.Chan (R.ResultOrError (ArchState (Sym t) arch))
                   , backend :: Sym t
                   , gen :: A.Gen
                   , testGen :: Sym t -> IO (ArchState (Sym t) arch)
                   -- ^ The test generator is part of local state because it
                   -- might not be thread safe.  Be sure to allocate one per
                   -- runner.
                   , nonce :: STM.TVar Word64
                   }

askTestChan :: Learning t arch (C.Chan (Maybe (R.TestCase (ArchState (Sym t) arch))))
askTestChan = Rd.asks testChan

askResultChan :: Learning t arch (C.Chan (R.ResultOrError (ArchState (Sym t) arch)))
askResultChan = Rd.asks resChan

askBackend :: Learning t arch (Sym t)
askBackend = Rd.asks backend

askGen :: Learning t arch A.Gen
askGen = Rd.asks gen

-- | Record a learned IORelation into the global environment
recordLearnedRelation :: (Architecture arch) => Opcode arch (Operand arch) sh -> IORelation arch sh -> Learning t arch ()
recordLearnedRelation op rel = do
  ref <- Rd.asks (learnedRelations . globalLearningEnv)
  liftIO $ STM.atomically $ do
    STM.modifyTVar' ref (MapF.insert op rel)

nextNonce :: Learning t arch Word64
nextNonce = do
  nvar <- Rd.asks nonce
  liftIO $ STM.atomically $ do
    nn <- STM.readTVar nvar
    STM.modifyTVar' nvar (+1)
    return nn

nextOpcode :: Learning t arch (Maybe (Some (Opcode arch (Operand arch))))
nextOpcode = do
  wlref <- Rd.asks (worklist . globalLearningEnv)
  liftIO $ STM.atomically $ do
    wl <- STM.readTVar wlref
    case WL.takeWork wl of
      Nothing -> return Nothing
      Just (op, wl') -> do
        STM.writeTVar wlref wl'
        return (Just op)

mkRandomTest :: Learning t arch (ArchState (Sym t) arch)
mkRandomTest = do
  mkTest <- Rd.asks testGen
  sym <- askBackend
  liftIO $ mkTest sym

askAssembler :: Learning t arch (Instruction arch -> BS.ByteString)
askAssembler = Rd.asks (assemble . globalLearningEnv)

data OperandRef arch sh = ImplicitOperand (Some (Location arch))
                        -- ^ A location that is implicitly read from or written to by an instruction
                        | OperandRef (Some (D.Index sh))
                        -- ^ An index into an operand list

deriving instance (Architecture arch) => Eq (OperandRef arch sh)
deriving instance (Architecture arch) => Ord (OperandRef arch sh)

data TestBundle t l =
  TestBundle { tbTestCases :: [t]
             -- ^ The variants to run
             , tbResult :: l
             -- ^ The fact we learn if the test cases differ
             }

-- | If the given location changes, it was an output location.  Otherwise, if
-- the test cases differ from the original test case, it was an input operand.
data ExplicitFact arch =
  forall sh tp . ExplicitFact { lOpcode :: Opcode arch (Operand arch) sh
                              , lIndex :: D.Index sh tp
                              -- ^ The index into the operand list of the location we are watching
                              , lLocation :: Location arch (OperandType arch tp)
                              -- ^ The location we are watching
                              , lInstruction :: Instruction arch
                              }

-- | We just need to track the explicitly-referenced locations.  Changes to any
-- state variables that are not explicitly mentioned in the operand list
-- indicate implicit operands.
data ImplicitFact arch =
  ImplicitFact { ifExplicits :: S.Set (Some (Location arch))
               , ifLocation :: Some (Location arch)
               -- ^ The location that was modified for this test
               , ifInstruction :: Instruction arch
               }

data IORelation arch sh =
  IORelation { inputs :: S.Set (OperandRef arch sh)
             -- ^ Locations read by an instruction
             , outputs :: S.Set (OperandRef arch sh)
             -- ^ Locations written by an instruction
             }

instance (Architecture arch) => Monoid (IORelation arch sh) where
  mempty = emptyIORelation
  mappend = mergeIORelations

emptyIORelation  :: IORelation arch sh
emptyIORelation = IORelation { inputs = S.empty, outputs = S.empty }

mergeIORelations :: (Architecture arch) => IORelation arch sh -> IORelation arch sh -> IORelation arch sh
mergeIORelations ior1 ior2 =
  IORelation { inputs = inputs ior1 `S.union` inputs ior2
             , outputs = outputs ior1 `S.union` outputs ior2
             }

newtype Learning t arch a = Learning { runM :: Rd.ReaderT (LocalLearningEnv t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            Rd.MonadReader (LocalLearningEnv t arch),
            MonadIO)

runLearning :: LocalLearningEnv t arch -> Learning t arch a -> IO a
runLearning env a = Rd.runReaderT (runM a) env

data LearningException arch = LearningTimeout (Proxy arch) (Some (Opcode arch (Operand arch)))
deriving instance (Architecture arch) => Show (LearningException arch)
instance (Architecture arch, Typeable arch) => E.Exception (LearningException arch)

data ResultIndex a = ResultIndex { riExitedWithSignal :: !(M.Map Word64 Int32)
                                 -- ^ A set of nonces for tests that failed with a signal
                                 , riSuccesses :: !(M.Map Word64 (R.TestResult a))
                                 -- ^ The results of tests, keyed by nonce
                                 }

emptyResultIndex :: ResultIndex a
emptyResultIndex = ResultIndex { riExitedWithSignal = M.empty
                               , riSuccesses = M.empty
                               }

-- | Number of microseconds to wait for all results to come in over the channel
askWaitMicroseconds :: Learning t arch Int
askWaitMicroseconds = (* 1000000) <$> Rd.asks (resWaitSeconds . globalLearningEnv)

-- | Execute an 'IO' action with a timeout (provided by the 'M' environment)
timeout :: IO a -> Learning t arch (Maybe a)
timeout a = do
  ms <- askWaitMicroseconds
  liftIO $ T.timeout ms a


-- | This is a newtype to shuffle type arguments around so that the 'tp'
-- parameter is last (so that we can use it with PairF and Some)
newtype TypedLocation arch tp = TL (Location arch (OperandType arch tp))
