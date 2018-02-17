-- | Representations of the ARM architecture for semantics learning.
--
-- There are two operational modes for ARM: A32 and T32 (i.e. Thumb).
-- The current mode is dictated by the ISETSETATE register in the
-- processor, and the low-bit of the PC (0 for A32, 1 for T32,
-- although this low-bit is not expressed for instruction fetches).
--
-- There are separate dismantle definitions for A32 (called ARM) and
-- T32 (called Thumb), and these are treated as parallel instruction
-- sets.  The rest of the architecture (e.g. register definitions,
-- etc.) are common between the two.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
import           Data.List.NonEmpty ( NonEmpty(..), fromList )
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Vector.Sized as V
import           Data.Word ( Word8, Word32 )
import qualified Dismantle.ARM as ARM
import qualified Dismantle.ARM.Operands as ARMOperands
import           GHC.TypeLits
import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified SemMC.Architecture as A
import           SemMC.Architecture.ARM.BaseSemantics ( numGPR )
import qualified SemMC.Architecture.ARM.Components as ARMComp
import           SemMC.Architecture.ARM.Eval
import           SemMC.Architecture.ARM.Location
import qualified SemMC.Architecture.ARM.UF as UF
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Formula as F
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P


-- | Define the arch type for this processor.  There are no
-- inhabitants, but this is used as a phantom type selector.
data ARM  -- arch type


-- ----------------------------------------------------------------------

data MachineState =
  MachineState { gprs :: V.Vector 16 Word32
               , pctr :: Word32  -- ^ the current Program Counter (PC)
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
                  , B.word32LE (pctr ms)
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
  pcv <- G.getWord32le
  -- Note that we have to parse out the mask, even though it isn't populated
  -- here.
  Just grs_mask <- V.fromList <$> replicateM 16 G.getWord32le
  Just frs <- V.fromList <$> replicateM 32 G.getWord32le
  cpsr_reg <- G.getWord32le
  Just m1 <- V.fromList <$> replicateM 32 G.getWord8
  Just m2 <- V.fromList <$> replicateM 32 G.getWord8
  return MachineState { gprs = grs
                      , pctr = pcv
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

type instance A.OperandType ARM "Addrmode_imm12_pre" = BaseBVType 32
type instance A.OperandType ARM "Am2offset_imm" = BaseBVType 32
type instance A.OperandType ARM "Addr_offset_none" = BaseBVType 32
type instance A.OperandType ARM "Arm_blx_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType ARM "Cc_out" = BaseBVType 1
type instance A.OperandType ARM "GPR" = BaseBVType 32
type instance A.OperandType ARM "Mod_imm" = BaseBVType 32
type instance A.OperandType ARM "Pred" = BaseBVType 4
type instance A.OperandType ARM "So_reg_imm" = BaseBVType 32
type instance A.OperandType ARM "So_reg_reg" = BaseBVType 32
type instance A.OperandType ARM "ThumbBlxTarget" = BaseBVType 32 -- double-instr val


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
operandValue sym locLookup op = TaggedExpr <$> opV op
  where opV :: ARM.Operand s -> IO (S.SymExpr sym (A.OperandType ARM s))
        opV (ARM.Addrmode_imm12_pre v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.addrModeImm12ToBits v
        opV (ARM.Arm_blx_target v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.branchExecuteTargetToBits v
        opV (ARM.Cc_out v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.sBitToBits v
        opV (ARM.GPR gpr) = locLookup (LocGPR gpr)
        opV (ARM.Mod_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.modImmToBits v
        opV (ARM.Pred bits4) = S.bvLit sym knownNat $ toInteger $ ARMOperands.predToBits bits4
        opV (ARM.So_reg_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.soRegImmToBits v
        opV (ARM.So_reg_reg v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.soRegRegToBits v
        -- opV (Thumb.ThumbBlxTarget v) = S.bvLit sym knownNat $ toInteger $ ThumbOperands.thumbBlxTargetToBits v
        -- opV unhandled = error $ "operandValue not implemented for " <> show unhandled


operandToLocation :: ARM.Operand s -> Maybe (Location ARM (A.OperandType ARM s))
operandToLocation (ARM.GPR gpr) = Just $ LocGPR gpr
operandToLocation _ = Nothing

-- ----------------------------------------------------------------------

instance A.IsLocation (Location ARM) where

  isMemoryLocation LocMem = True
  isMemoryLocation _ = False

  readLocation = P.parseMaybe parseLocation

  locationType (LocGPR _) = knownRepr
  locationType LocPC = knownRepr
  locationType LocCPSR = knownRepr
  locationType LocMem = knownRepr

  defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocPC = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCPSR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMem =
      S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0

  allLocations = concat
    [ map (Some . LocGPR . ARMOperands.gpr) [0..numGPR-1],
      [ Some LocPC
      , Some LocCPSR
      , Some LocMem
      ]
    ]

  registerizationLocations = [] -- map (Some . LocGPR . ARM.GPR) (0 : [3..4])

parseLocation :: ARMComp.Parser (Some (Location ARM))
parseLocation = do
  c <- P.lookAhead (P.anyChar)
  case c of
    'C' -> Some LocCPSR <$ P.string "CPSR"
    'M' -> Some LocMem <$ P.string "Mem"
    'P' -> Some LocPC <$ P.string "PC"
    'R' -> parsePrefixedRegister (Some . LocGPR . ARMOperands.gpr) 'R'
    _ -> P.failure (Just $ P.Tokens $ (c:|[])) (Set.fromList $ [ P.Label $ fromList "Location" ])

parsePrefixedRegister :: (Word8 -> b) -> Char -> ARMComp.Parser b
parsePrefixedRegister f c = do
  _ <- P.char c
  n <- P.decimal
  case n >= 0 && n <= (numGPR-1) of
    True -> return (f n)
    False -> P.failure (Just $ P.Tokens $ fromList $ show n)
                      (Set.fromList $ [ P.Label $ fromList $ "Register number 0-" <> show (numGPR-1) ])

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
locationFuncInterpretation =
    [ ("arm.is_r15", A.FunctionInterpretation { A.exprInterpName = 'interpIsR15 })

    , ("a32.imm12_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpImm12Reg
                                                 , A.exprInterpName = 'interpImm12RegExtractor
                                                 })
    , ("a32.imm12_off", A.FunctionInterpretation { A.exprInterpName = 'interpImm12OffsetExtractor })
    , ("a32.imm12_add", A.FunctionInterpretation { A.exprInterpName = 'interpImm12AddFlgExtractor })

    , ("a32.modimm_imm", A.FunctionInterpretation { A.exprInterpName = 'interpModimmImmExtractor })
    , ("a32.modimm_rot", A.FunctionInterpretation { A.exprInterpName = 'interpModimmRotExtractor })

    ]

shapeReprType :: forall tp . ARM.OperandRepr tp -> BaseTypeRepr (A.OperandType ARM tp)
shapeReprType orep =
  case orep of
    ARM.Addrmode_imm12_preRepr -> knownRepr
    ARM.Arm_blx_targetRepr -> knownRepr
    ARM.Cc_outRepr -> knownRepr
    ARM.GPRRepr -> knownRepr
    ARM.Mod_immRepr -> knownRepr
    ARM.PredRepr -> knownRepr
    ARM.So_reg_immRepr -> knownRepr
    ARM.So_reg_regRepr -> knownRepr
    -- Thumb.ThumbBlxTargetRepr -> knownRepr
    _ -> error $ "Unknown OperandRepr: " <> show (A.operandTypeReprSymbol (Proxy @ARM) orep)
    -- "Imm0_15"
    --   | Just Refl <- testEquality sr (SR.knownSymbol @"Imm0_15") ->
    --     knownRepr :: BaseTypeRepr (A.OperandType ARM "Imm0_15")


-- ----------------------------------------------------------------------

data Signed = Signed | Unsigned deriving (Eq, Show)

instance T.TemplatableOperand ARM where
  opTemplates sr =
    case sr of
      ARM.Addrmode_imm12_preRepr ->
          mkTemplate <$> [0..numGPR-1]
            where mkTemplate gprNum = T.TemplatedOperand Nothing
                                      (Set.singleton (Some (LocGPR (ARMOperands.gpr gprNum)))) mkTemplate'
                                          :: T.TemplatedOperand ARM "Addrmode_imm12_pre"
                    where mkTemplate' :: T.TemplatedOperandFn ARM "Addrmode_imm12_pre"
                          mkTemplate' sym locLookup = do
                            let gprN = ARMOperands.gpr gprNum
                            base <- A.unTagged <$> A.operandValue (Proxy @ARM) sym locLookup (ARM.GPR $ gprN)
                            offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_off") knownRepr
                            addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_add") knownRepr
                            expr <- S.bvAdd sym base offset
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  addflagVal <- fromInteger <$> evalFn addflag
                                  return $ ARM.Addrmode_imm12_pre $ ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                            return (expr, T.WrappedRecoverOperandFn recover)
      ARM.Arm_blx_targetRepr -> undefined
      ARM.Cc_outRepr -> undefined
      ARM.GPRRepr -> concreteTemplatedOperand ARM.GPR LocGPR . ARMOperands.gpr <$> [0..numGPR-1]
      ARM.Mod_immRepr -> undefined
      ARM.PredRepr -> [symbolicTemplatedOperand (Proxy @4) Unsigned "Pred" (ARM.Pred . ARM.mkPred . fromInteger)]
      -- ARM.ThumbBlxTargetRepr -> undefined


concreteTemplatedOperand :: forall arch s a.
                            (A.Architecture arch)
                         => (a -> A.Operand arch s)
                         -> (a -> A.Location arch (A.OperandType arch s))
                         -> a
                         -> T.TemplatedOperand arch s
concreteTemplatedOperand op loc x =
  T.TemplatedOperand { T.templOpLocation = Just (loc x)
                     , T.templUsedLocations = Set.singleton (Some (loc x))
                     , T.templOpFn = mkTemplate'
                     }
  where mkTemplate' :: T.TemplatedOperandFn arch s
        mkTemplate' sym locLookup = do
          expr <- A.unTagged <$> A.operandValue (Proxy @arch) sym locLookup (op x)
          return (expr, T.WrappedRecoverOperandFn $ const (return (op x)))


symbolicTemplatedOperand :: forall arch s (bits :: Nat) extended
                          . (A.OperandType arch s ~ BaseBVType extended,
                             KnownNat bits,
                             KnownNat extended,
                             1 <= bits,
                             bits <= extended)
                         => Proxy bits
                         -> Signed
                         -> String
                         -> (Integer -> A.Operand arch s)
                         -> T.TemplatedOperand arch s
symbolicTemplatedOperand Proxy signed name constr =
  T.TemplatedOperand { T.templOpLocation = Nothing
                     , T.templUsedLocations = Set.empty
                     , T.templOpFn = mkTemplate'
                     }
  where mkTemplate' :: T.TemplatedOperandFn arch s
        mkTemplate' sym _ = do
          v <- S.freshConstant sym (U.makeSymbol name) (knownRepr :: BaseTypeRepr (BaseBVType bits))
          let bitsRepr = knownNat @bits
              extendedRepr = knownNat @extended
          extended <- case testNatCases bitsRepr extendedRepr of
            NatCaseLT LeqProof ->
              case signed of
                Signed   -> S.bvSext sym knownNat v
                Unsigned -> S.bvZext sym knownNat v
            NatCaseEQ -> return v
            NatCaseGT LeqProof -> error "impossible"
          let recover evalFn = constr <$> evalFn v
          return (extended, T.WrappedRecoverOperandFn recover)
