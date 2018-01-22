-- | Representations of the ARM architecture for semantics learning

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module SemMC.ARM
    ( ARM
    , MachineState(..)
    , Instruction
    , testSerializer
    )
    where

import           Control.Monad ( replicateM )
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Vector.Sized as V
import           Data.Word ( Word8, Word32 )
import qualified Dismantle.ARM as ARM
import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.ARM.Components as ARMComp
import           SemMC.Architecture.ARM.Eval
import           SemMC.Architecture.ARM.Location
import qualified SemMC.Architecture.ARM.UF as UF
import qualified SemMC.Concrete.Execution as CE
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P


-- | Define the arch type for this processor.  There are no
-- inhabitants, but this is used as a phantom type selector.
data ARM  -- arch type


-- ----------------------------------------------------------------------

data MachineState =
  MachineState { gprs :: V.Vector 16 Word32
               -- ^ 16 general purpose registers
               , gprs_mask :: V.Vector 16 Word32
               , fprs :: V.Vector 32 Word32
               -- ^ 32 32-bit locations
               , cpsr :: Word32
               -- ^ Current program status register (CPSR)
               , mem1 :: V.Vector 32 Word8
               -- ^ 32 bytes
               , mem2 :: V.Vector 32 Word8
               -- ^ 32 bytes
               }
  deriving (Show,Eq)

-- The instruction representation is literal machine code.
type Instruction = LB.ByteString

testSerializer :: CE.TestSerializer MachineState Instruction
testSerializer = CE.TestSerializer { CE.flattenMachineState = toBS
                                   , CE.parseMachineState = fromBS
                                   , CE.flattenProgram = mconcat
                                   }

toBS :: MachineState -> B.ByteString
toBS ms = LB.toStrict (B.toLazyByteString bld)
  where
    bld = mconcat [ mconcat (map B.word32LE (V.toList (gprs ms)))
                  , mconcat (map B.word32LE (V.toList (gprs_mask ms)))
                  , mconcat (map B.word32LE (V.toList (fprs ms)))
                  , B.word32LE (cpsr ms)
                  , mconcat (map B.word8 (V.toList (mem1 ms)))
                  , mconcat (map B.word8 (V.toList (mem2 ms)))
                  ]

fromBS :: B.ByteString -> Maybe MachineState
fromBS bs =
  case G.pushChunk (G.runGetIncremental getMachineState) bs of
    G.Done _ _ ms -> Just ms
    G.Fail {} -> Nothing
    G.Partial {} -> Nothing

getMachineState :: G.Get MachineState
getMachineState = do
  Just grs <- V.fromList <$> replicateM 16 G.getWord32le
  -- Note that we have to parse out the mask, even though it isn't populated
  -- here.
  Just grs_mask <- V.fromList <$> replicateM 16 G.getWord32le
  Just frs <- V.fromList <$> replicateM 32 G.getWord32le
  cpsr_reg <- G.getWord32le
  Just m1 <- V.fromList <$> replicateM 32 G.getWord8
  Just m2 <- V.fromList <$> replicateM 32 G.getWord8
  return MachineState { gprs = grs
                      , gprs_mask = grs_mask
                      , fprs = frs
                      , cpsr = cpsr_reg
                      , mem1 = m1
                      , mem2 = m2
                      }

-- ----------------------------------------------------------------------

type instance A.Operand  ARM = ARM.Operand
type instance A.Opcode   ARM = ARM.Opcode
type instance A.Location ARM = Location ARM

instance A.IsOperand ARM.Operand
instance A.IsOpcode  ARM.Opcode

type instance A.OperandType ARM "GPR" = BaseBVType 32


instance A.IsOperandTypeRepr ARM where
    type OperandTypeRepr ARM = ARM.OperandRepr
    operandTypeReprSymbol _ = ARM.operandReprString


operandValue :: forall sym s.
                (S.IsSymInterface sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location ARM tp -> IO (S.SymExpr sym tp))
             -> ARM.Operand s
             -> IO (A.TaggedExpr ARM sym s)
operandValue sym locLookup op = TaggedExpr <$> operandValue' op
  where operandValue' :: ARM.Operand s -> IO (S.SymExpr sym (A.OperandType ARM s))
        operandValue' (ARM.GPR gpr) = locLookup (LocGPR gpr)
        -- operandValue' (ARM.DBG
        -- operandValue' (ARM.Imm0_15 (ARM.DBG absTarget)) =
        --   S.bvLit sym knownNat (toInteger absTarget)


operandToLocation :: ARM.Operand s -> Maybe (Location ARM (A.OperandType ARM s))
operandToLocation _ = Nothing

-- ----------------------------------------------------------------------

instance A.IsLocation (Location ARM) where

  isMemoryLocation l = case l of
                         -- LocMem -> True
                         _ -> False

  readLocation = P.parseMaybe parseLocation

  -- locationType (LocGPR _) = knownRepr
  locationType LocIP = knownRepr

  -- defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocIP = S.bvLit sym knownNat 0

  allLocations = concat
    [ -- map (Some . LocGPR . ARM.GPR) [0..31],
      [ Some LocIP
      ]
    ]

  registerizationLocations = [] -- map (Some . LocGPR . ARM.GPR) (0 : [3..4])

parseLocation :: ARMComp.Parser (Some (Location ARM))
parseLocation = do
  c <- P.lookAhead (P.anyChar)
  case c of
    'I' -> Some LocIP <$ P.string "IP"

-- ----------------------------------------------------------------------

-- ShowF (A.Operand ARM)
--       ShowF (A.Opcode ARM (A.Operand ARM))
--             OrdF (A.Opcode ARM (A.Operand ARM))
--                  (Data.EnumF.EnumF (A.Opcode ARM (A.Operand ARM)))

type instance ArchRegWidth ARM = 32


instance A.Architecture ARM where
    data TaggedExpr ARM sym s = TaggedExpr (S.SymExpr sym (A.OperandType ARM s))
    unTagged (TaggedExpr e) = e
    operandValue _ = operandValue
    operandToLocation _ = operandToLocation
    uninterpretedFunctions = UF.uninterpretedFunctions
    locationFuncInterpretation _proxy = createSymbolicEntries locationFuncInterpretation
    shapeReprToTypeRepr _proxy = shapeReprType


locationFuncInterpretation :: [(String, A.FunctionInterpretation t ARM)]
locationFuncInterpretation = [
   ("arm.is_r15", A.FunctionInterpretation { A.exprInterpName = 'interpIsR15
                                           })]

shapeReprType :: forall tp . ARM.OperandRepr tp -> BaseTypeRepr (A.OperandType ARM tp)
shapeReprType orep =
  case orep of
    ARM.GPRRepr -> knownRepr
    -- "Imm0_15"
    --   | Just Refl <- testEquality sr (SR.knownSymbol @"Imm0_15") ->
    --     knownRepr :: BaseTypeRepr (A.OperandType ARM "Imm0_15")
