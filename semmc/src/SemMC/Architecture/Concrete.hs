{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module SemMC.Architecture.Concrete (
  RegisterizedInstruction(..),
  LiteralRef(..),
  ConcreteArchitecture(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.ShapedList as SL
import           Data.Parameterized.Some ( Some(..) )
import           Lang.Crucible.BaseTypes

import qualified Dismantle.Arbitrary as DA

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import           SemMC.Architecture.View

data LiteralRef arch sh tp where
  LiteralRef :: SL.Index sh tp -> LiteralRef arch sh (A.OperandType arch tp)

instance P.ShowF (LiteralRef arch sh) where
  showF (LiteralRef ix) = P.showF ix

instance Show (LiteralRef arch sh tp) where
  show = P.showF

instance P.TestEquality (LiteralRef arch sh) where
  testEquality (LiteralRef ix1) (LiteralRef ix2) = do
    P.Refl <- P.testEquality ix1 ix2
    return P.Refl

instance P.OrdF (LiteralRef arch sh) where
  compareF (LiteralRef ix1) (LiteralRef ix2) =
    case P.compareF ix1 ix2 of
      P.LTF -> P.LTF
      P.GTF -> P.GTF
      P.EQF -> P.EQF


-- | A wrapper around an instruction that notes part of the machine
-- state that will be used to represent immediate operands.
--
-- The extra map indicates which literals in the instruction are
-- mapped to locations in the state.  The key operation that is
-- required to support this is to be able to rewrite test programs
-- (i.e., single instructions we are trying to learn semantics for)
-- such that their immediates have the same value as the value in the
-- indicated location.
--
-- For example, assume we have a concrete state C that is to be used
-- for a test case and a side note that the literal for our
-- instruction I is stored in r15:
--
-- > let t = Test { testMachineState = C, testLiterals = MapF.fromList [Pair imm0 r15] }
--
-- Before sending the test, we would need to rewrite the test
-- instruction to use the immediate in r15 as its value.  This
-- effectively lets us pretend that an instruction like @ADDI r1, r2,
-- imm@ is actually @ADDI r1, r2, r3@ where @r3@ happens to hold our
-- immediate value.  The one difficulty here is that we need to ensure
-- that the value is in range for the literal in question.
--
-- The major use of this infrastructure is during formula extraction:
-- specifically, to figure out which part of the formula represents
-- the immediate of the instruction.  If we don't have an anchor to
-- record what part of the formula stands in for the immediate, we
-- can't extract a formula since we can't tell which literals in a
-- formula might or might not correspond to immediates.  If we instead
-- pretend that immediates came from the machine state, we will have a
-- distinguished variable to pull out of the formula and turn into a
-- parameter.  That means that the 'testLiterals' map will need to be
-- an input to 'extractFormula'.
--
-- Note that, to construct the state to send to the remote host, we
-- just need to extract the 'testMachineState'.
data RegisterizedInstruction arch =
  forall sh .
  RI { riInstruction :: A.Instruction arch
     , riOpcode :: A.Opcode arch (A.Operand arch) sh
     , riOperands :: SL.ShapedList (A.Operand arch) sh
     , riLiteralLocs :: MapF.MapF (LiteralRef arch sh) (L.Location arch)
     }

-- | An architecture with certain operations needed for concrete work.
class (A.Architecture arch) => ConcreteArchitecture arch where
  -- | Convert an operand into a *view* onto the underlying location that holds
  -- it (if any).
  --
  -- The view is a slice of the backing location.
  operandToSemanticView :: proxy arch -> A.Operand arch s -> Maybe (SemanticView arch)

  -- | Obtain the type of an operand (even an operand with no associated location)
  operandType :: proxy arch -> A.Operand arch s -> BaseTypeRepr (A.OperandType arch s)

  -- | Construct a complete state with all locations set to zero
  --
  -- This is a useful starting point for constructing a desired state to ensure
  -- that all locations are filled in.
  zeroState :: proxy arch -> ConcreteState arch

  -- | A list of initial heuristically-interesting states for the architecture.
  --
  -- This isn't fully generic because it is hard to fully abstract the aliasing
  -- relationships between registers.
  heuristicallyInterestingStates :: proxy arch -> [ConcreteState arch]

  -- | Update the immediate operands of the wrapped instruction (if any) based on
  -- the test environment.
  --
  -- Note that this may require constraining the range of the value assigned to
  -- the immediate, as the immediate may (probably will) have a restricted range
  -- compared to the full width of a register.  Because of this, we also return
  -- the modified 'Test' to reflect the range restriction of the chosen value.
  --
  -- This is safe, as long as we use the same modified test for both the candidate
  -- and target programs.
  registerizeInstruction :: RegisterizedInstruction arch -> ConcreteState arch -> (A.Instruction arch, ConcreteState arch)

  -- | Generate a completely random state
  --
  -- The random state has all locations filled in
  randomState :: proxy arch -> DA.Gen -> IO (ConcreteState arch)

  -- | Convert a 'ConcreteState' into a 'B.ByteString'
  serialize :: proxy arch -> ConcreteState arch -> B.ByteString

  -- | Try to convert a 'B.ByteString' into a 'ConcreteState'
  deserialize :: proxy arch -> B.ByteString -> Maybe (ConcreteState arch)

  readView :: String -> Maybe (Some (View arch))

  showView :: View arch n -> String
