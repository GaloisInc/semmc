{-# LANGUAGE TemplateHaskell #-}
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
module SemMC.Toy where

import           Control.Monad
import           Data.Bits ( complement )
import           Data.EnumF ( congruentF, EnumF, enumF, enumCompareF )
import           Data.Int ( Int32 )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Word ( Word32 )
import qualified GHC.Err.Located as L
import           GHC.TypeLits ( KnownSymbol, Nat, Symbol, sameSymbol )

import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.SymbolRepr as SR
import qualified Data.Parameterized.TH.GADT as TH
import qualified Data.Word.Indexed as W

import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.Arbitrary as D

import           What4.BaseTypes
import qualified What4.Interface as S
import           What4.Expr.GroundEval

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.View as V
import qualified SemMC.Stochastic.IORelation as I
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Stochastic.RvwpOptimization as R
import           SemMC.Synthesis.Template ( TemplatedOperandFn, TemplatableOperand(..), TemplatedOperand(..), RecoverOperandFn(..) )
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
  AddRr :: Opcode Operand '["R32", "R32"]
  SubRr :: Opcode Operand '["R32", "R32"]
  NegR  :: Opcode Operand '["R32"]
  -- Bitwise complement.
  NotR  :: Opcode Operand '["R32"]
  -- Set register to immediate value.
  MovRi :: Opcode Operand '["R32", "I32"]
{-
  -- A few more to add once the first example is working.
  AddRi :: Opcode Operand '["R32","I32"]
  SubRi :: Opcode Operand '["R32","I32"]
-}
deriving instance Show (Opcode o sh)
deriving instance Eq (Opcode o sh)
deriving instance Ord (Opcode o sh)

-- | The set of all opcodes, e.g. for use with
-- 'D.Random.randomInstruction'.
allOpcodes :: Set.Set (Some (Opcode Operand))
allOpcodes = Set.fromList
  [ Some AddRr
  , Some SubRr
  , Some NegR
  , Some NotR
  , Some MovRi
  ]

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
  , MapF.Pair NotR $ I.IORelation
      { I.inputs = Set.fromList [o0]
      , I.outputs = Set.fromList [o0] }
  , MapF.Pair MovRi $ I.IORelation
      { I.inputs = Set.fromList [o0, o1]
      , I.outputs = Set.fromList [o0] } ]
  where
    o0 = I.OperandRef (Some SL.IndexHere)
    o1 = I.OperandRef (Some (SL.IndexThere SL.IndexHere))

instance ShowF (Opcode o) where
  showF = show

instance EnumF (Opcode o) where
  enumF AddRr = 0
  enumF SubRr = 1
  enumF NegR = 2
  enumF NotR = 3
  enumF MovRi = 4

  congruentF AddRr = NES.fromList AddRr [AddRr, SubRr]
  congruentF SubRr = NES.fromList SubRr [AddRr, SubRr]
  congruentF NegR  = NES.fromList NegR [NegR, NotR]
  congruentF NotR  = NES.fromList NotR [NegR, NotR]
  congruentF MovRi = NES.fromList MovRi [MovRi]

instance TestEquality (Opcode o) where
  testEquality AddRr AddRr = Just Refl
  testEquality SubRr SubRr = Just Refl
  testEquality NegR  NegR  = Just Refl
  testEquality NotR  NotR  = Just Refl
  testEquality MovRi MovRi = Just Refl
  testEquality     _     _ = Nothing

instance OrdF (Opcode o) where
  compareF = enumCompareF

instance HasRepr (Opcode Operand) (SL.List SR.SymbolRepr) where
  typeRepr AddRr = knownRepr
  typeRepr SubRr = knownRepr
  typeRepr NegR  = knownRepr
  typeRepr NotR  = knownRepr
  typeRepr MovRi = knownRepr

type Instruction = D.GenericInstruction Opcode Operand

----------------------------------------------------------------
-- * Virtual Machine / Concrete Evaluation

type MachineState = V.ConcreteState Toy

initialMachineState :: MachineState
initialMachineState = MapF.fromList
  [ MapF.Pair (RegLoc r) (V.ValueBV 0)
  | r <- [Reg1, Reg2, Reg3] ]

-- | The value type of a symbol, e.g. @"R32"@ registers are 32 bits.
type family Value (s :: Symbol) :: *
type instance Value "R32" = Word32
type instance Value "I32" = Word32

type family   BitWidth (s :: Symbol) :: Nat
type instance BitWidth "R32" = 32
type instance BitWidth "I32" = 32

-- | Get the value of an operand in a machine state.
getOperand :: MachineState -> Operand s -> V.Value (BaseBVType (BitWidth s))
getOperand ms (R32 r) = V.peekMS ms (regView r)
getOperand _ (I32 x) = V.ValueBV (fromIntegral x)

-- | Update the value of an operand in a machine state.
putOperand :: MachineState -> Operand s -> V.Value (BaseBVType (BitWidth s)) -> MachineState
putOperand ms (R32 r) x = V.pokeMS ms (regView r) x
putOperand _ I32{} _ = L.error "putOperand: putting an immediate does not make sense!"

-- | Evaluate an instruction, updating the machine state.
evalInstruction :: MachineState -> Instruction -> MachineState
evalInstruction ms (D.Instruction op args) = case (op, args) of
  (AddRr, r1 SL.:< r2 SL.:< SL.Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (V.liftValueBV2 (+) x1 x2)
  (SubRr, r1 SL.:< r2 SL.:< SL.Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (V.liftValueBV2 (-) x1 x2)
  (NegR, r1 SL.:< SL.Nil) ->
    let x1 = getOperand ms r1
    in putOperand ms r1 (V.liftValueBV1 negate x1)
  (NotR, r1 SL.:< SL.Nil) ->
    let x1 = getOperand ms r1
    in putOperand ms r1 (V.liftValueBV1 complement x1)
  (MovRi, r1 SL.:< i1 SL.:< SL.Nil) ->
    let x1 = getOperand ms i1
    in putOperand ms r1 x1

evalProg :: MachineState -> [Instruction] -> MachineState
evalProg ms is = foldl evalInstruction ms is

----------------------------------------------------------------
-- * Architecture Instantiation

data Toy = Toy

instance A.IsOperand Operand

instance A.IsOpcode Opcode

instance A.IsOperandTypeRepr Toy where
  type OperandTypeRepr Toy = SR.SymbolRepr
  operandTypeReprSymbol _ = T.unpack . SR.symbolRepr

type instance A.RegWidth Toy = 16

instance TemplatableOperand Toy where
  opTemplates sr =
    case SR.symbolRepr sr of
      "R32"
        | Just Refl <- testEquality sr (SR.knownSymbol @"R32") ->
          mkTemplate <$> [Reg1, Reg2, Reg3]
            where mkTemplate reg = TemplatedOperand (Just (RegLoc reg)) (Set.singleton (Some (RegLoc reg))) mkTemplate' :: TemplatedOperand Toy "R32"
                    where mkTemplate' :: TemplatedOperandFn Toy "R32"
                          mkTemplate' _ locLookup = do
                            let loc = RegLoc reg
                            expr <- locLookup loc
                            return (A.LocationOperand loc expr, RecoverOperandFn $ const (return (R32 reg)))
      "I32"
        | Just Refl <- testEquality sr (SR.knownSymbol @"I32") ->
            [TemplatedOperand Nothing Set.empty mkConst]
              where mkConst :: TemplatedOperandFn Toy "I32"
                    mkConst sym _ = do
                      v <- S.freshConstant sym (makeSymbol "I32") knownRepr
                      let recover evalFn = I32 . fromInteger <$> evalFn v
                      return (A.ValueOperand v, RecoverOperandFn recover)
      r -> error $ "opTemplates: unexpected symbolRepr: "++show r

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

shapeReprType :: forall tp . SR.SymbolRepr tp -> BaseTypeRepr (A.OperandType Toy tp)
shapeReprType sr =
  case SR.symbolRepr sr of
    "R32"
      | Just Refl <- testEquality sr (SR.knownSymbol @"R32") ->
        knownRepr :: BaseTypeRepr (A.OperandType Toy "R32")
    "I32"
      | Just Refl <- testEquality sr (SR.knownSymbol @"I32") ->
        knownRepr :: BaseTypeRepr (A.OperandType Toy "I32")
    _ -> error ("Unhandled shape repr: " ++ show sr)

data OperandComponents sym s = NoComponents
type instance A.OperandComponents Toy sym = OperandComponents sym

instance A.Architecture Toy where
  data TaggedExpr Toy sym s = TaggedExpr { unTaggedExpr :: A.AllocatedOperand Toy sym s }

  unTagged te =
    case unTaggedExpr te of
      A.ValueOperand se -> Just se
      A.LocationOperand _ se -> Just se
      A.CompoundOperand {} -> Nothing
  taggedOperand = unTaggedExpr

  uninterpretedFunctions _ = []
  readMemUF _ = undefined
  writeMemUF _ = undefined
  archEndianForm _ = undefined

  allocateSymExprsForOperand _ _ newVars (R32 reg) =
    let loc = RegLoc reg
    in TaggedExpr <$> A.LocationOperand loc <$> newVars loc
  allocateSymExprsForOperand _ sym _     (I32 imm) = TaggedExpr <$> A.ValueOperand <$> S.bvLit sym (knownNat :: NatRepr 32) (toInteger imm)

  operandToLocation _ (R32 reg) = Just (RegLoc reg)
  operandToLocation _ (I32 _) = Nothing

  locationFuncInterpretation _ = []

  shapeReprToTypeRepr _proxy = shapeReprType

  operandComponentsImmediate _ _ = Nothing

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

  registerizationLocations = A.allLocations

  isMemoryLocation _ = False
  isIP _ = False
  nonMemLocations = map (Some . RegLoc) [Reg1, Reg2, Reg3]
  memLocation = []

interestingStates :: [MachineState]
interestingStates =
  [ mkState r1 v1 r2 v2
  | r1 <- regs
  , r2 <- regs
  , Nothing == testEquality r1 r2
  , v1 <- values
  , v2 <- values
  ]
  where
    regs = map RegLoc [Reg1, Reg2, Reg3]
    i32Min :: Int32
    i32Min = minBound
    i32Max :: Int32
    i32Max = maxBound
    values = [ V.ValueBV (W.w 0)
             , V.ValueBV (W.w 1)
             , V.ValueBV (W.w (fromIntegral i32Min))
             , V.ValueBV (W.w (fromIntegral i32Max))
             ]

    mkState r1 v1 r2 v2 = MapF.insert r1 v1 $ MapF.insert r2 v2 initialMachineState

-- If we got rid of the 'NatRepr' / 'knownNat' stuff we could make
-- this a pattern synonym.
regView :: Reg -> V.View Toy 32
regView r = V.trivialView (Proxy :: Proxy Toy) (RegLoc r)

operandToSemanticViewImpl :: Operand sh -> Maybe (V.SemanticView Toy)
operandToSemanticViewImpl (R32 r) =
  Just $ V.SemanticView { V.semvView = regView r
                        , V.semvCongruentViews = [ regView r' | r' <- [Reg1, Reg2, Reg3], r /= r' ]
                        , V.semvDiff = V.diffInt
                        }
operandToSemanticViewImpl (I32 _) = Nothing

randomStateImpl :: D.Gen -> IO (V.ConcreteState Toy)
randomStateImpl gen = do
  pairs <- forM [Reg1, Reg2, Reg3] $ \r -> do
    value <- V.arbitraryBV gen knownNat
    return $ MapF.Pair (RegLoc r) value
  return $ MapF.fromList pairs

toyOperandType :: Operand s -> BaseTypeRepr (A.OperandType Toy s)
toyOperandType o =
  case o of
    R32 _ -> knownRepr :: BaseTypeRepr (BaseBVType 32)
    I32 _ -> knownRepr :: BaseTypeRepr (BaseBVType 32)

-- | This is trivial for the Toy architecture, since it doesn't have any opcodes
-- with immediates.  If we add opcodes with immediates, they will need to be
-- patched up here.
toyRegisterizeInstruction :: AC.RegisterizedInstruction Toy
                          -> V.ConcreteState Toy
                          -> (A.Instruction Toy, V.ConcreteState Toy)
toyRegisterizeInstruction ri cs = (AC.riInstruction ri, cs)

instance AC.ConcreteArchitecture Toy where
  zeroState _ = initialMachineState
  randomState _ = randomStateImpl

  operandType _ = toyOperandType
  registerizeInstruction = toyRegisterizeInstruction

  operandToSemanticView _ = operandToSemanticViewImpl

  heuristicallyInterestingStates _proxy = interestingStates

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
    (NES.fromList 0 $ bitCast <$> [-16..16] ++
                      concat [[2^k, -(2^k)] | k <- [(5::Integer)..31]])
    gen
    where
    bitCast = fromIntegral :: Int32 -> Word32

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
    NotR  -> D.arbitraryShapedList gen
    MovRi -> D.arbitraryShapedList gen

----------------------------------------------------------------
-- * Pseudo Ops

data PseudoOpcode :: (Symbol -> *) -> [Symbol] -> * where
  -- | The @MovRr dst src@ copies the value of @src@ into @dst@.
  MovRr :: PseudoOpcode Operand '["R32", "R32"]

deriving instance Show (PseudoOpcode op sh)

instance ShowF (PseudoOpcode op)

-- Eliminate
--
--   'PseudoOpcode' is not in the type environment at a reify
--
-- error. No idea why this helps; copied from
-- 'SemMC.Architecture.PPC.Pseudo'.
$(return [])

instance TestEquality (PseudoOpcode op) where
  testEquality = $(TH.structuralTypeEquality [t| PseudoOpcode |] [])

instance OrdF (PseudoOpcode op) where
  compareF = $(TH.structuralTypeOrd [t| PseudoOpcode |] [])

instance HasRepr (PseudoOpcode op) (SL.List SR.SymbolRepr) where
  typeRepr MovRr = knownRepr

instance D.ArbitraryOperands PseudoOpcode Operand where
  arbitraryOperands gen op = case op of
    MovRr -> D.arbitraryShapedList gen

type instance P.Pseudo Toy = PseudoOpcode

instance P.ArchitectureWithPseudo Toy where
  assemblePseudo _proxy opcode oplist = case opcode of
    MovRr -> case oplist of
      (dst SL.:< src SL.:< SL.Nil) ->
        [ D.Instruction MovRi (dst SL.:< I32 0 SL.:< SL.Nil)
        , D.Instruction AddRr (dst SL.:< src SL.:< SL.Nil) ]

allPseudoOpcodes :: [Some ((P.Pseudo Toy) Operand)]
allPseudoOpcodes = [ Some MovRr ]

----------------------------------------------------------------
-- Rvwp optimization support

instance R.RvwpOptimization Toy where
  rvwpMov (V.View dstSlice (RegLoc dst)) (V.View srcSlice (RegLoc src)) = do
    guard $ dst /= src
    when (not (isTrivialR32Slice dstSlice) || not (isTrivialR32Slice srcSlice)) $
      error "Toy.rvwpMov: needs to be updated for new cases that aren't handled."
    return [ P.SynthInstruction (P.PseudoOpcode MovRr) (R32 dst SL.:< R32 src SL.:< SL.Nil) ]
    where
      -- | Returns true iff the slice is the whole 32 bit register.
      isTrivialR32Slice :: V.Slice m n -> Bool
      isTrivialR32Slice (V.Slice m n a b) =
        natValue m == 32 && natValue n == 32 && natValue a == 0 && natValue b == 32
