{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

import           Data.EnumF ( congruentF, EnumF, enumF )
import           Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Parameterized.Classes as ParamClasses
import           Data.Parameterized.Classes hiding ( ShowF, showF )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Set as Set
import           Data.ShowF ( ShowF, showF )
import           Data.Word ( Word32 )
import           GHC.TypeLits ( Symbol )

import           Dismantle.Instruction ( OperandList(Nil,(:>)) )
import qualified Dismantle.Instruction as D
-- import qualified Dismantle.Instruction.Random as D

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

import qualified SemMC.Architecture as A

data Reg :: BaseType -> * where
  Reg1 :: Reg (BaseBVType 32)
  Reg2 :: Reg (BaseBVType 32)
  Reg3 :: Reg (BaseBVType 32)
deriving instance Show (Reg tp)
deriving instance Eq (Reg tp)
deriving instance Ord (Reg tp)

instance ParamClasses.ShowF Reg where
  showF = show

instance ShowF Reg where
  showF = show

instance OrdF Reg where
  Reg1 `compareF` Reg1 = EQF
  Reg1 `compareF` Reg2 = LTF
  Reg1 `compareF` Reg3 = LTF
  Reg2 `compareF` Reg1 = GTF
  Reg2 `compareF` Reg2 = EQF
  Reg2 `compareF` Reg3 = LTF
  Reg3 `compareF` Reg1 = GTF
  Reg3 `compareF` Reg2 = GTF
  Reg3 `compareF` Reg3 = EQF

instance TestEquality Reg where
  Reg1 `testEquality` Reg1 = Just Refl
  Reg2 `testEquality` Reg2 = Just Refl
  Reg3 `testEquality` Reg3 = Just Refl
  _     `testEquality`     _ = Nothing

instance A.IsLocation Reg where
  readLocation "r1" = Just (Some Reg1)
  readLocation "r2" = Just (Some Reg2)
  readLocation "r3" = Just (Some Reg3)
  readLocation    _ = Nothing

  locationType Reg1 = knownRepr :: BaseTypeRepr (BaseBVType 32)
  locationType Reg2 = knownRepr :: BaseTypeRepr (BaseBVType 32)
  locationType Reg3 = knownRepr :: BaseTypeRepr (BaseBVType 32)

-- | An operand, indexed so that we can compute the type of its
-- values, and describe the shape of instructions that can take it as
-- an argument.
data Operand :: Symbol -> * where
  R32 :: Reg (BaseBVType 32) -> Operand "R32"
  -- | The use of @Value "I32"@ here seems a little inconsistent with
  -- 'Reg' as the argument to 'R32' above: the 'Reg' stands for a
  -- register name, and which bits in that register are being referred
  -- to is determined by the 'R32' constructor (e.g. we could have a
  -- @R16l@ and @R16h@ for the low and high 16 bits, respectively). In
  -- the analogy with 'R32', might it then make more sense to simply
  -- have 'I32' take an @Imm@ argument?
  I32 :: Value "I32" -> Operand "I32"
deriving instance Show (Operand a)
deriving instance Eq (Operand a)
deriving instance Ord (Operand a)

instance ShowF Operand where
  showF (R32 reg) = "R32 " ++ show reg
  showF (I32 val) = "R32 " ++ show val

instance TestEquality Operand where
  testEquality (R32 r1) (R32 r2)
    | r1 == r2 = Just Refl
    | otherwise = Nothing
  testEquality (I32 i1) (I32 i2)
    | i1 == i2 = Just Refl
    | otherwise = Nothing
  testEquality _ _ = Nothing

instance OrdF Operand where
  compareF (R32 _) (I32 _) = LTF
  compareF (R32 r1) (R32 r2) = fromOrdering $ compare r1 r2
  compareF (I32 _) (R32 _) = GTF
  compareF (I32 i1) (I32 i2) = fromOrdering $ compare i1 i2

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
deriving instance Eq (Opcode o sh)
deriving instance Ord (Opcode o sh)

instance ShowF (Opcode o) where
  showF = show

instance EnumF (Opcode o) where
  enumF AddRr = 0
  enumF SubRr = 1
  enumF NegR = 2

  congruentF AddRr = Set.fromList [AddRr, SubRr]
  congruentF SubRr = Set.fromList [AddRr, SubRr]
  congruentF NegR = Set.fromList [NegR]

instance TestEquality (Opcode o) where
  testEquality AddRr AddRr = Just Refl
  testEquality SubRr SubRr = Just Refl
  testEquality  NegR  NegR = Just Refl
  testEquality     _     _ = Nothing

instance OrdF (Opcode o) where
  AddRr `compareF` AddRr = EQF
  AddRr `compareF` SubRr = LTF
  AddRr `compareF`  NegR = LTF
  SubRr `compareF` AddRr = GTF
  SubRr `compareF` SubRr = EQF
  SubRr `compareF`  NegR = LTF
  NegR  `compareF` AddRr = GTF
  NegR  `compareF` SubRr = GTF
  NegR  `compareF`  NegR = EQF

type Instruction = D.GenericInstruction Opcode Operand

-- | Registers, flags, and memory.
data MachineState = MachineState
  { msRegs :: !(Map (Reg (BaseBVType 32)) (Value "R32"))
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

data Toy = Toy

type instance A.Operand Toy = Operand
type instance A.Opcode Toy = Opcode

type instance A.OperandType Toy "R32" = BaseBVType 32
type instance A.OperandType Toy "I32" = BaseBVType 32

type instance A.Location Toy = Reg

instance A.Architecture Toy where
  operandValue _ sym newVars (R32 reg) = S.varExpr sym <$> newVars reg
  operandValue _ sym _       (I32 imm) = S.bvLit sym (knownNat :: NatRepr 32) (toInteger imm)

  operandToLocation _ (R32 reg) = Just reg
  operandToLocation _ (I32 _) = Nothing
