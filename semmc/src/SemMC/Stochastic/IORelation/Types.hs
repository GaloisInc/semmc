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
--  TypedLocation(..),
  TestBundle(..),
  TestCase,
  TestResult,
  ResultOrError,
  TestSerializer,
  TestRunner,
  ExplicitFact(..),
  ImplicitFact(..),
  OperandRef(..),
  LearningException(..),
  Learning,
  askTestChan,
  askResultChan,
  askGen,
  askAssembler,
  mkRandomTest,
  nextNonce,
  nextOpcode,
  recordFailure,
  recordLearnedRelation,
  runLearning,
  timeout
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Catch as E
import qualified Control.Monad.Reader as Rd
import           Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import           Data.Typeable ( Typeable )
import           Data.Word ( Word64 )
import qualified System.Timeout as T

import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Pair as P
import qualified Data.Parameterized.ShapedList as SL
import           Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.Arbitrary as DA

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Log as L
import qualified SemMC.Worklist as WL

type TestCase arch       = CE.TestCase (V.ConcreteState arch) (A.Instruction arch)
type TestResult arch     = CE.TestResult (V.ConcreteState arch)
type ResultOrError arch  = CE.ResultOrError (V.ConcreteState arch)
type TestSerializer arch = CE.TestSerializer (V.ConcreteState arch) (A.Instruction arch)
type TestRunner arch     = CE.TestRunner (V.ConcreteState arch) (A.Instruction arch)

data GlobalLearningEnv arch =
  GlobalLearningEnv { assemble :: A.Instruction arch -> LBS.ByteString
                    , resWaitSeconds :: Int
                    -- ^ Number of seconds to wait to receive all of the results over the 'resChan'
                    , worklist :: STM.TVar (WL.Worklist (Some (A.Opcode arch (A.Operand arch))))
                    , learnedRelations :: STM.TVar (MapF.MapF (A.Opcode arch (A.Operand arch)) (IORelation arch))
                    , serializationChan :: C.Chan (Maybe (P.Pair (A.Opcode arch (A.Operand arch)) (IORelation arch)))
                    , learningFailures :: STM.TVar (S.Set (Some (A.Opcode arch (A.Operand arch)), Maybe Int))
                    -- ^ Opcodes for which we received a signal while processing
                    -- and couldn't learn
                    , logCfg :: L.LogCfg
                    -- ^ Logger configuration
                    }

data LocalLearningEnv arch =
  LocalLearningEnv { globalLearningEnv :: GlobalLearningEnv arch
                   , testChan :: C.Chan (Maybe [TestCase arch])
                   , resChan :: C.Chan (CE.ResultOrError (V.ConcreteState arch))
                   , gen :: DA.Gen
                   , testGen :: IO (V.ConcreteState arch)
                   -- ^ The test generator is part of local state because it
                   -- might not be thread safe.  Be sure to allocate one per
                   -- runner.
                   , nonce :: STM.TVar Word64
                   }

recordFailure :: (P.OrdF (A.Opcode arch (A.Operand arch))) => A.Opcode arch (A.Operand arch) sh -> Maybe Int -> Learning arch ()
recordFailure op sigNum = do
  ref <- Rd.asks (learningFailures . globalLearningEnv)
  liftIO $ STM.atomically $ STM.modifyTVar' ref (S.insert (Some op, sigNum))

askTestChan :: Learning arch (C.Chan (Maybe [TestCase arch]))
askTestChan = Rd.asks testChan

askResultChan :: Learning arch (C.Chan (CE.ResultOrError (V.ConcreteState arch)))
askResultChan = Rd.asks resChan

askGen :: Learning arch DA.Gen
askGen = Rd.asks gen

-- | Record a learned IORelation into the global environment
recordLearnedRelation :: (A.Architecture arch) => A.Opcode arch (A.Operand arch) sh -> IORelation arch sh -> Learning arch ()
recordLearnedRelation op rel = do
  ref <- Rd.asks (learnedRelations . globalLearningEnv)
  liftIO $ STM.atomically $ do
    STM.modifyTVar' ref (MapF.insert op rel)
  c <- Rd.asks (serializationChan . globalLearningEnv)
  liftIO $ C.writeChan c (Just (P.Pair op rel))

nextNonce :: Learning arch Word64
nextNonce = do
  nvar <- Rd.asks nonce
  liftIO $ STM.atomically $ do
    nn <- STM.readTVar nvar
    STM.modifyTVar' nvar (+1)
    return nn

nextOpcode :: Learning arch (Maybe (Some (A.Opcode arch (A.Operand arch))))
nextOpcode = do
  wlref <- Rd.asks (worklist . globalLearningEnv)
  liftIO $ STM.atomically $ do
    wl <- STM.readTVar wlref
    case WL.takeWork wl of
      Nothing -> return Nothing
      Just (op, wl') -> do
        STM.writeTVar wlref wl'
        return (Just op)

mkRandomTest :: Learning arch (V.ConcreteState arch)
mkRandomTest = liftIO =<< Rd.asks testGen

askAssembler :: Learning arch (A.Instruction arch -> LBS.ByteString)
askAssembler = Rd.asks (assemble . globalLearningEnv)

data OperandRef arch sh = ImplicitOperand (Some (V.View arch))
                        -- ^ A location that is implicitly read from or written to by an instruction
                        | OperandRef (Some (SL.Index sh))
                        -- ^ An index into an operand list

deriving instance (A.Architecture arch) => Show (OperandRef arch sh)
deriving instance (A.Architecture arch) => Eq (OperandRef arch sh)
deriving instance (A.Architecture arch) => Ord (OperandRef arch sh)

data TestBundle t l =
  TestBundle { tbTestCases :: [t]
             -- ^ The variants to run
             , tbTestBase :: t
             -- ^ The base test case from which the test cases were derived
             --
             -- We want this so that we can determine the difference that the
             -- tweak in the test case made
             , tbResult :: l
             -- ^ The fact we learn if the test cases differ
             }

-- | If the given location changes, it was an output location.  Otherwise, if
-- the test cases differ from the original test case, it was an input operand.
data ExplicitFact arch =
  forall sh tp n . ExplicitFact { lOpcode :: A.Opcode arch (A.Operand arch) sh
                                , lIndex :: SL.Index sh tp
                                -- ^ The index into the operand list of the location we are watching
                                , lLocation :: V.View arch n
                                -- ^ The location we are watching
                                , lInstruction :: A.Instruction arch
                                }

-- | We just need to track the explicitly-referenced locations.  Changes to any
-- state variables that are not explicitly mentioned in the operand list
-- indicate implicit operands.
data ImplicitFact arch =
  ImplicitFact { ifExplicits :: S.Set (Some (V.View arch))
               , ifLocation :: Some (V.View arch)
               -- ^ The location that was modified for this test
               , ifInstruction :: A.Instruction arch
               }

data IORelation arch sh =
  IORelation { inputs :: S.Set (OperandRef arch sh)
             -- ^ Locations read by an instruction
             , outputs :: S.Set (OperandRef arch sh)
             -- ^ Locations written by an instruction
             }
  deriving (Show)

instance (A.Architecture arch) => Monoid (IORelation arch sh) where
  mempty = emptyIORelation
  mappend = mergeIORelations

emptyIORelation  :: IORelation arch sh
emptyIORelation = IORelation { inputs = S.empty, outputs = S.empty }

mergeIORelations :: (A.Architecture arch) => IORelation arch sh -> IORelation arch sh -> IORelation arch sh
mergeIORelations ior1 ior2 =
  IORelation { inputs = inputs ior1 `S.union` inputs ior2
             , outputs = outputs ior1 `S.union` outputs ior2
             }

newtype Learning arch a = Learning { runM :: Rd.ReaderT (LocalLearningEnv arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            Rd.MonadReader (LocalLearningEnv arch),
            MonadIO)

instance L.MonadHasLogCfg (Learning arch) where
  getLogCfgM = Rd.asks (logCfg . globalLearningEnv)

runLearning :: LocalLearningEnv arch -> Learning arch a -> IO a
runLearning env a = Rd.runReaderT (runM a) env

data LearningException arch = LearningTimeout (Proxy arch) (Some (A.Opcode arch (A.Operand arch)))
deriving instance (A.Architecture arch) => Show (LearningException arch)
instance (A.Architecture arch, Typeable arch) => E.Exception (LearningException arch)

-- | Number of microseconds to wait for all results to come in over the channel
askWaitMicroseconds :: Learning arch Int
askWaitMicroseconds = (* 1000000) <$> Rd.asks (resWaitSeconds . globalLearningEnv)

-- | Execute an 'IO' action with a timeout (provided by the 'Learning' environment)
timeout :: IO a -> Learning arch (Maybe a)
timeout a = do
  ms <- askWaitMicroseconds
  liftIO $ T.timeout ms a


-- | This is a newtype to shuffle type arguments around so that the 'tp'
-- parameter is last (so that we can use it with PairF and Some)
-- newtype TypedLocation arch tp = TL (Location arch (OperandType arch tp))
