{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module SemMC.Stochastic.IORelation.Types (
  IORelation(..),
  LearnConfig(..),
  TypedLocation(..),
  TestBundle(..),
  ExplicitFact(..),
  ImplicitFact(..),
  OperandRef(..),
  LearningException(..),
  ResultIndex(..),
  emptyResultIndex,
  M,
  runM,
  timeout
  ) where

import qualified Control.Concurrent as C
import qualified Control.Monad.Catch as E
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString as BS
import Data.Int ( Int32 )
import qualified Data.Map.Strict as M
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import Data.Typeable ( Typeable )
import Data.Word ( Word64 )
import qualified System.Timeout as T

import Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as D

import SemMC.Architecture
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
              , resWaitSeconds :: Int
              -- ^ Number of seconds to wait to receive all of the results over the 'resChan'
              }

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
  forall sh tp . ImplicitFact { ifOpcode :: Opcode arch (Operand arch) sh
                              , ifExplicits :: S.Set (Some (Location arch))
                              , ifLocation :: Location arch (OperandType arch tp)
                              -- ^ The location that was modified for this test
                              , ifInstruction :: Instruction arch
                              , ifProxy :: Proxy tp
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

newtype M t arch a = M { runM :: St.StateT (LearnConfig t arch) IO a }
  deriving (Functor,
            Applicative,
            Monad,
            St.MonadState (LearnConfig t arch),
            MonadIO)

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
askWaitMicroseconds :: M t arch Int
askWaitMicroseconds = (* 1000000) <$> St.gets resWaitSeconds

-- | Execute an 'IO' action with a timeout (provided by the 'M' environment)
timeout :: IO a -> M t arch (Maybe a)
timeout a = do
  ms <- askWaitMicroseconds
  liftIO $ T.timeout ms a


-- | This is a newtype to shuffle type arguments around so that the 'tp'
-- parameter is last (so that we can use it with PairF and Some)
newtype TypedLocation arch tp = TL (Location arch (OperandType arch tp))
