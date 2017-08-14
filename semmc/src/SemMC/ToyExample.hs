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
module SemMC.ToyExample where

import           Control.Monad
import           Data.EnumF ( congruentF, EnumF, enumF )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import           Data.Word ( Word32 )
import qualified GHC.Err.Located as L
import           GHC.TypeLits ( KnownSymbol, Symbol, sameSymbol )

import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.ShapedList ( ShapedList(Nil, (:>)), Index(IndexHere, IndexThere), ShapeRepr )
import           Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.Arbitrary as D

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval

import qualified SemMC.Architecture as A
import qualified SemMC.ConcreteState as C
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Stochastic.IORelation as I
import qualified SemMC.Stochastic.Pseudo as P
import           SemMC.Synthesis.Template ( TemplatedOperandFn, TemplatableOperand(..), TemplatedOperand(..), WrappedRecoverOperandFn(..) )
import           SemMC.Util ( makeSymbol )

-- import Debug.Trace

----------------------------------------------------------------
-- * Instructions

-- | Registers.
--
-- Note that this is the underlying register, not the view, on
-- architectures like x86 where e.g. @eax@ is a "view" into @rax@. The
-- view part corresponds to the 'Operand' type, e.g. @'R32' 'Reg1'@ is
-- the full view of the register @Reg1@.
data Reg = Reg1 | Reg2 | Reg3
  deriving (Show, Eq, Ord)

-- | An operand, indexed so that we can compute the type of its
-- values, and describe the shape of instructions that can take it as
-- an argument.
data Operand :: Symbol -> * where
  -- | A 32-bit register. A 16-bit register would look like
  --
  -- > RL16 :: Reg -> Operand "R16"
  -- > RH16 :: Reg -> Operand "R16"
  --
  -- for the low and high 16 bits, respectively, of the underlying
  -- 32-bit register.
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
deriving instance Eq (Operand a)
deriving instance Ord (Operand a)

instance ShowF Operand where
  showF (R32 reg) = "R32 " ++ show reg
  showF (I32 val) = "I32 " ++ show val

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
  AddRr :: Opcode Operand '["R32", "R32"]
  SubRr :: Opcode Operand '["R32", "R32"]
  NegR  :: Opcode Operand '["R32"]

  MovRi :: Opcode Operand '["R32", "I32"]
{-
  -- A few more to add once the first example is working.
  AddRi :: Opcode Operand '["R32","I32"]
  SubRi :: Opcode Operand '["R32","I32"]
  SetRi :: Opcode Operand '["R32","I32"]
-}
deriving instance Show (Opcode o sh)
deriving instance Eq (Opcode o sh)
deriving instance Ord (Opcode o sh)

-- | The set of all opcodes, e.g. for use with
-- 'D.Random.randomInstruction'.
opcodes :: Set.Set (Some (Opcode Operand))
opcodes = Set.fromList
  [ Some AddRr
  , Some SubRr
  , Some NegR
  , Some MovRi
  ]

-- | For use with @loadInitialState@
opcodesWitnessingBuildOperandList ::
  [Some (Witness (F.BuildOperandList Toy) (Opcode Operand))]
opcodesWitnessingBuildOperandList =
  [ Some (Witness AddRr)
  , Some (Witness SubRr)
  , Some (Witness NegR)
  , Some (Witness MovRi) ]

-- | The map of 'IORelation's for all opcodes.
--
-- This will need to include implicit operands once we add flags.
ioRelations :: MapF.MapF (Opcode Operand) (I.IORelation Toy)
ioRelations = MapF.fromList
  [ MapF.Pair AddRr $ I.IORelation
      { I.inputs = Set.fromList [o0, o1]
      , I.outputs = Set.fromList [o0] }
  , MapF.Pair SubRr $ I.IORelation
      { I.inputs = Set.fromList [o0, o1]
      , I.outputs = Set.fromList [o0] }
  , MapF.Pair NegR $ I.IORelation
      { I.inputs = Set.fromList [o0]
      , I.outputs = Set.fromList [o0] }
  , MapF.Pair MovRi $ I.IORelation
      { I.inputs = Set.fromList [o0, o1]
      , I.outputs = Set.fromList [o0] } ]
  where
    o0 = I.OperandRef (Some IndexHere)
    o1 = I.OperandRef (Some (IndexThere IndexHere))

instance ShowF (Opcode o) where
  showF = show

instance EnumF (Opcode o) where
  enumF AddRr = 0
  enumF SubRr = 1
  enumF NegR = 2
  enumF MovRi = 3

  congruentF AddRr = NES.fromList AddRr [AddRr, SubRr]
  congruentF SubRr = NES.fromList SubRr [AddRr, SubRr]
  congruentF NegR  = NES.fromList NegR [NegR]
  congruentF MovRi = NES.fromList MovRi [MovRi]

instance TestEquality (Opcode o) where
  testEquality AddRr AddRr = Just Refl
  testEquality SubRr SubRr = Just Refl
  testEquality  NegR  NegR = Just Refl
  testEquality MovRi MovRi = Just Refl
  testEquality     _     _ = Nothing

instance OrdF (Opcode o) where
  AddRr `compareF` AddRr = EQF
  AddRr `compareF` SubRr = LTF
  AddRr `compareF`  NegR = LTF
  AddRr `compareF` MovRi = LTF
  SubRr `compareF` AddRr = GTF
  SubRr `compareF` SubRr = EQF
  SubRr `compareF`  NegR = LTF
  SubRr `compareF` MovRi = LTF
  NegR  `compareF` AddRr = GTF
  NegR  `compareF` SubRr = GTF
  NegR  `compareF`  NegR = EQF
  NegR  `compareF` MovRi = LTF
  MovRi `compareF` AddRr = GTF
  MovRi `compareF` SubRr = GTF
  MovRi `compareF`  NegR = GTF
  MovRi `compareF` MovRi = EQF

instance HasRepr (Opcode Operand) ShapeRepr where
  typeRepr AddRr = knownRepr
  typeRepr SubRr = knownRepr
  typeRepr NegR  = knownRepr
  typeRepr MovRi = knownRepr

type Instruction = D.GenericInstruction Opcode Operand

----------------------------------------------------------------
-- * Virtual Machine / Concrete Evaluation

type MachineState = C.ConcreteState Toy

initialMachineState :: MachineState
initialMachineState = MapF.fromList
  [ MapF.Pair (RegLoc r) (C.ValueBV 0)
  | r <- [Reg1, Reg2, Reg3] ]

-- | The value type of a symbol, e.g. @"R32"@ registers are 32 bits.
type family Value (s :: Symbol) :: *
type instance Value "R32" = Word32
type instance Value "I32" = Word32

type family   BitWidth (s :: Symbol) :: C.Nat
type instance BitWidth "R32" = 32
type instance BitWidth "I32" = 32

-- | Get the value of an operand in a machine state.
getOperand :: MachineState -> Operand s -> C.Value (BaseBVType (BitWidth s))
getOperand ms (R32 r) = C.peekMS ms (regView r)
getOperand _ (I32 x) = C.ValueBV (fromIntegral x)

-- | Update the value of an operand in a machine state.
putOperand :: MachineState -> Operand s -> C.Value (BaseBVType (BitWidth s)) -> MachineState
putOperand ms (R32 r) x = C.pokeMS ms (regView r) x
putOperand _ I32{} _ = L.error "putOperand: putting an immediate does not make sense!"

-- | Evaluate an instruction, updating the machine state.
evalInstruction :: MachineState -> Instruction -> MachineState
evalInstruction ms (D.Instruction op args) = case (op, args) of
  (AddRr, r1 :> r2 :> Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (C.liftValueBV2 (+) x1 x2)
  (SubRr, r1 :> r2 :> Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (C.liftValueBV2 (-) x1 x2)
  (NegR, r1 :> Nil) ->
    let x1 = getOperand ms r1
    in putOperand ms r1 (C.liftValueBV1 negate x1)
  (MovRi, r1 :> i1 :> Nil) ->
    let x1 = getOperand ms i1
    in putOperand ms r1 x1

evalProg :: MachineState -> [Instruction] -> MachineState
evalProg ms is = foldl evalInstruction ms is

----------------------------------------------------------------
-- * Architecture Instantiation

data Toy = Toy

instance A.IsOperand Operand

instance A.IsOpcode Opcode

instance TemplatableOperand Toy "R32" where
  opTemplates = mkTemplate <$> [Reg1, Reg2, Reg3]
    where mkTemplate reg = TemplatedOperand (Just (RegLoc reg)) mkTemplate' :: TemplatedOperand Toy "R32"
            where mkTemplate' :: TemplatedOperandFn Toy "R32"
                  mkTemplate' _ locLookup = do
                    expr <- locLookup (RegLoc reg)
                    return (expr, WrappedRecoverOperandFn $ const (return (R32 reg)))

instance TemplatableOperand Toy "I32" where
  opTemplates = [TemplatedOperand Nothing mkConst]
    where mkConst :: TemplatedOperandFn Toy "I32"
          mkConst sym _ = do
            v <- S.freshConstant sym (makeSymbol "I32") knownRepr
            let recover evalFn = I32 . fromInteger <$> evalFn v
            return (v, WrappedRecoverOperandFn recover)

type instance A.Operand Toy = Operand
type instance A.Opcode Toy = Opcode

type instance A.OperandType Toy "R32" = BaseBVType 32
type instance A.OperandType Toy "I32" = BaseBVType 32

type instance A.Location Toy = Location

valueToOperand :: forall s. (KnownSymbol s) => GroundValue (A.OperandType Toy s) -> Operand s
valueToOperand val
  | Just Refl <- sameSymbol (Proxy :: Proxy s) (Proxy :: Proxy "I32") = I32 (fromInteger val)
  | Just Refl <- sameSymbol (Proxy :: Proxy s) (Proxy :: Proxy "R32") =
      L.error "can't get register operand from value"
  | otherwise = undefined

instance A.Architecture Toy where
  data TaggedExpr Toy sym s = TaggedExpr { unTaggedExpr :: S.SymExpr sym (A.OperandType Toy s) }

  unTagged = unTaggedExpr

  operandValue _ _ newVars (R32 reg) = TaggedExpr <$> newVars (RegLoc reg)
  operandValue _ sym _     (I32 imm) = TaggedExpr <$> S.bvLit sym (knownNat :: NatRepr 32) (toInteger imm)

  operandToLocation _ (R32 reg) = Just (RegLoc reg)
  operandToLocation _ (I32 _) = Nothing

----------------------------------------------------------------
-- ** Locations

-- A location for storing data, i.e. register or memory.
data Location :: BaseType -> * where
  RegLoc :: Reg -> Location (BaseBVType 32)
  -- MemLoc :: Mem -> Location (BaseBVType 32)
deriving instance Show (Location tp)
deriving instance Eq (Location tp)
deriving instance Ord (Location tp)

instance ShowF Location where
  showF = show

instance OrdF Location where
  RegLoc r1 `compareF` RegLoc r2 = fromOrdering $ r1 `compare` r2

instance TestEquality Location where
  RegLoc r1 `testEquality` RegLoc r2 | r1 == r2 = Just Refl
                                     | otherwise = Nothing

instance A.IsLocation Location where
  readLocation "r1" = Just (Some (RegLoc Reg1))
  readLocation "r2" = Just (Some (RegLoc Reg2))
  readLocation "r3" = Just (Some (RegLoc Reg3))
  readLocation    _ = Nothing

  locationType RegLoc{} = knownRepr :: BaseTypeRepr (BaseBVType 32)

  defaultLocationExpr sym RegLoc{} = S.bvLit sym (knownNat :: NatRepr 32) 0

-- If we got rid of the 'NatRepr' / 'knownNat' stuff we could make
-- this a pattern synonym.
regView :: Reg -> C.View Toy 32
regView r = C.trivialView (Proxy :: Proxy Toy) (RegLoc r)

operandToSemanticViewImpl :: Operand sh -> Maybe (C.SemanticView Toy)
operandToSemanticViewImpl (R32 r) =
  Just $ C.SemanticView { C.semvView = regView r
                        , C.semvCongruentViews = [ regView r' | r' <- [Reg1, Reg2, Reg3], r /= r' ]
                        , C.semvDiff = C.diffInt
                        }
operandToSemanticViewImpl (I32 _) = Nothing

randomStateImpl :: D.Gen -> IO (C.ConcreteState Toy)
randomStateImpl gen = do
  pairs <- forM [Reg1, Reg2, Reg3] $ \r -> do
    value <- D.arbitrary gen
    return $ MapF.Pair (RegLoc r) value
  return $ MapF.fromList pairs

instance C.ConcreteArchitecture Toy where
  operandToSemanticView _ = operandToSemanticViewImpl
  zeroState _ = initialMachineState
  randomState _ = randomStateImpl

  serialize = L.error "Toy: serialize"
  deserialize = L.error "Toy: deserialize"
  readView = L.error "Toy: readView"
  showView = L.error "Toy: showView"

----------------------------------------------------------------
-- * Random Instruction Generation

-- | An random but *not* uniform choice.
--
-- This is used generate immediates for synthesis, so we don't want to
-- generate an uniform value, but rather one from a distinguished set:
-- [-16..16] and powers of 2.
instance D.Arbitrary (Operand "I32") where
  arbitrary gen = I32 <$> D.choose
    (NES.fromList 0 $ [-16..16] ++
                      concat [[2^k, -(2^k)] | k <- [(5::Integer)..31]])
    gen

instance D.Arbitrary (Operand "R32") where
  arbitrary gen = R32 <$> D.choose (NES.fromList Reg1 [Reg2, Reg3]) gen

instance D.ArbitraryOperand Operand where
  arbitraryOperand gen R32{} = D.arbitrary gen
  arbitraryOperand gen I32{} = D.arbitrary gen

instance D.ArbitraryOperands Opcode Operand where
  arbitraryOperands gen op = case op of
    -- ??? Is there a way to avoid the repetition here? We are
    -- implicitly using the shape of the operand to choose instances
    -- in 'D.arbitraryOperandList', and so without duplicating here it
    -- seems we need a way to universally quantify the existence of
    -- needed instances ...
    AddRr -> D.arbitraryShapedList gen
    SubRr -> D.arbitraryShapedList gen
    NegR  -> D.arbitraryShapedList gen
    MovRi -> D.arbitraryShapedList gen

----------------------------------------------------------------
-- * Pseudo Ops
--
-- We need to define a pseudo op type, even if it's empty.

type instance P.Pseudo Toy = P.EmptyPseudo

instance P.ArchitectureWithPseudo Toy where
  assemblePseudo _ = P.pseudoAbsurd

pseudoOpcodesWitnessingBuildOperandList ::
  [Some (Witness (F.BuildOperandList Toy) ((P.Pseudo Toy) Operand))]
pseudoOpcodesWitnessingBuildOperandList = []
