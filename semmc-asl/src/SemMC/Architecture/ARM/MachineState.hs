module SemMC.Architecture.ARM.MachineState
  ( MachineState(..)
  , Instruction
  , machineStateToBS
  , machineStateFromBS
  , testSerializer
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as LB

import qualified SemMC.Concrete.Execution as CE


data MachineState = MachineState
  deriving (Show, Eq)

type Instruction = LB.ByteString

machineStateToBS :: MachineState -> B.ByteString
machineStateToBS ms = error ""

machineStateFromBS :: B.ByteString -> Maybe MachineState
machineStateFromBS bs = error ""

getMachineState :: G.Get MachineState
getMachineState =  error ""

testSerializer :: CE.TestSerializer MachineState Instruction
testSerializer = CE.TestSerializer { CE.flattenMachineState = machineStateToBS
                                   , CE.parseMachineState = machineStateFromBS
                                   , CE.flattenProgram = mconcat
                                   }
