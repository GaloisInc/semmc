{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
-- | Description: A toy example architecture.
--
-- A toy example architecture and end-to-end semantic synthesis
-- example. The goal is to get something simple working, that
-- abstracts away as many practical details as possible, and teases
-- out the necessary difficulties.
--
-- Q: how to handle undefined values? Some instructions may have
-- non-deterministic effects on some parts of the machine state -- the
-- stratified synthesis paper mentions this -- and we may want to
-- capture this in the semantics (then there are at least three
-- possibilities for how an instruction interacts with a bit of
-- machine state: 1) modifies it in a deterministic way; 2) modifies
-- it in an undefined way; 3) does not modify it).
--
-- TODO:
--
-- - [ ] generate random instructions.
--
-- - [ ] specify what locations each instruction mutates.
--
-- - [ ] implement a metric (probably number of bits different) on
--   machine states, and on machine states restricted to the mutation
--   set of a specific instruction.
--
-- - [ ] synthesize programs and check equality on concrete inputs
--   using 'evalProg'.
--
-- - [ ] check equality using formal SMT semantics, after generating
--   candidates using concrete inputs and 'evalProg'.
--
-- - [ ] add flags.
module SemMC.ToyExample where

import           Data.Map ( Map )
import qualified Data.Map as M
import           Data.Word ( Word32 )
import           GHC.TypeLits ( Symbol )

import           Dismantle.Instruction ( OperandList(Nil,(:>)) )
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D

-- | A specific machine register.
data Reg = Reg1 | Reg2 | Reg3 deriving ( Eq, Ord, Show )

-- | An operand, indexed so that we can compute the type of its
-- values, and describe the shape of instructions that can take it as
-- an argument.
data Operand :: Symbol -> * where
  R32 :: Reg -> Operand "R32"
  -- | The use of @Value "I32"@ here seems a little inconsistent with
  -- 'Reg' as the argument to 'R32' above: the 'Reg' stands for a
  -- register name, and which bits in that register are being referred
  -- to is determined by the 'R32' constructor (e.g. we could have a
  -- @R16l@ and @R16h@ for the low and high 16 bits, respectively). In
  -- the analogy with 'R32', might it then make more sense to simply
  -- have 'I32' take an @Imm@ argument?
  I32 :: Value "I32" -> Operand "I32"
deriving instance Show (Operand a)

-- Q: Why index by the 'Operand' type, if it's always the same?
--
-- A: Because the 'D.GenericInstruction' expects its first argument to
-- be indexed in this way.
--
-- Q: So, why is 'D.GenericInstruction' expect an indexed first arg?
data Opcode (o :: Symbol -> *) (sh :: [Symbol]) where
  -- Three instructions for my simplest example.
  AddRr :: Opcode Operand '["R32","R32"]
  SubRr :: Opcode Operand '["R32","R32"]
  NegR  :: Opcode Operand '["R32"]
{-
  -- A few more to add once the first example is working.
  AddRi :: Opcode Operand '["R32","I32"]
  SubRi :: Opcode Operand '["R32","I32"]
  SetRi :: Opcode Operand '["R32","I32"]
-}
deriving instance Show (Opcode o sh)

type Instruction = D.GenericInstruction Opcode Operand

-- | Registers, flags, and memory.
data MachineState = MachineState
  { msRegs :: !(Map Reg (Value "R32"))
--  , msFlags :: *
--  , msMem :: *
  }

initialMachineState :: MachineState
initialMachineState = MachineState
  { msRegs = M.fromList [ (Reg1, 0)
                        , (Reg2, 0)
                        , (Reg3, 0)
                        ]
  }

-- | The value type of a symbol, e.g. @"R32"@ registers are 32 bits.
type family Value (s :: Symbol) :: *
type instance Value "R32" = Word32
type instance Value "I32" = Word32

-- | Get the value of an operand in a machine state.
getOperand :: MachineState -> Operand s -> Value s
getOperand ms (R32 r) = msRegs ms M.! r
getOperand _ (I32 x) = x

-- | Update the value of an operand in a machine state.
putOperand :: MachineState -> Operand s -> Value s -> MachineState
putOperand ms (R32 r) x = ms { msRegs = M.insert r x $ msRegs ms }
putOperand _ I32{} _ = error "putOperand: putting an immediate does not make sense!"

-- | Evaluate an instruction, updating the machine state.
evalInstruction :: MachineState -> Instruction -> MachineState
evalInstruction ms (D.Instruction op args) = case (op, args) of
  (AddRr, r1 :> r2 :> Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (x1 + x2)
  (SubRr, r1 :> r2 :> Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (x1 - x2)
  (NegR, r1 :> Nil) ->
    let x1 = getOperand ms r1
    in putOperand ms r1 (- x1)

evalProg :: MachineState -> [Instruction] -> MachineState
evalProg ms is = foldl evalInstruction ms is

-- TODO: also need a notion of flags.
--
-- If all flags are one bit, then perhaps we can do away with the
-- indexing.
--
-- - [ ] set flags in operational semantics.
{-
data Flag :: G.Symbol -> * where
  Flag

getFlag :: Flag s -> MachineState -> Value s
getFlag = undefined
-}
