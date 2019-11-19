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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module SemMC.Architecture.AArch32
    ( AArch32
    , MachineState(..)
    , Instruction
    , ARMOpcode(..)
    , ARMOperand(..)
    , numGPR
    , testSerializer
    , machineStateToBS
    , machineStateFromBS
    , module SemMC.Architecture.ARM.Combined  -- for the instances
    )
    where

import           Control.Monad ( replicateM )
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty ( NonEmpty(..), fromList )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Vector.Sized as V
import           Data.Word ( Word8, Word32 )
import qualified Data.Word.Indexed as W
import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMOperands
import qualified Dismantle.Thumb as ThumbDis
import qualified Dismantle.Thumb.Operands as ThumbOperands
import           GHC.TypeLits
import           Language.Haskell.TH hiding ( recover )
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as AL
import           SemMC.Architecture.ARM.BaseSemantics.Registers ( numGPR, regWidth )
import           SemMC.Architecture.ARM.Combined
import qualified SemMC.Architecture.ARM.Components as ARMComp
import           SemMC.Architecture.ARM.Eval
import           SemMC.Architecture.ARM.Location
import qualified SemMC.Architecture.ARM.OperandComponents as AOC
import qualified SemMC.Architecture.ARM.UF as UF
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Formula as F
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import           What4.BaseTypes
import qualified What4.Expr as WE
import qualified What4.Interface as S

-- | Define the arch type for this processor.  There are no
-- inhabitants, but this is used as a phantom type selector.  This
-- includes both A32 and T32 instruction modes.
data AArch32  -- arch type


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
testSerializer = CE.TestSerializer { CE.flattenMachineState = machineStateToBS
                                   , CE.parseMachineState = machineStateFromBS
                                   , CE.flattenProgram = mconcat
                                   }

machineStateToBS :: MachineState -> B.ByteString
machineStateToBS ms = LB.toStrict (B.toLazyByteString bld)
  where
    bld = mconcat [ mconcat (map B.word32LE (V.toList (gprs ms)))
                  , B.word32LE (pctr ms)
                  , mconcat (map B.word32LE (V.toList (gprs_mask ms)))
                  , mconcat (map B.word32LE (V.toList (fprs ms)))
                  , B.word32LE (cpsr ms)
                  , mconcat (map B.word8 (V.toList (mem1 ms)))
                  , mconcat (map B.word8 (V.toList (mem2 ms)))
                  ]

machineStateFromBS :: B.ByteString -> Maybe MachineState
machineStateFromBS bs =
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

type instance A.Opcode   AArch32 = ARMOpcode
type instance A.Operand  AArch32 = ARMOperand
type instance A.Location AArch32 = Location AArch32

instance A.IsOpcode  ARMOpcode
instance A.IsOperand ARMOperand

type instance A.OperandType AArch32 "Addr_offset_none" = BaseBVType 32
type instance A.OperandType AArch32 "Addrmode_imm12" = BaseBVType 32
type instance A.OperandType AArch32 "Addrmode_imm12_pre" = BaseBVType 32
type instance A.OperandType AArch32 "Am2offset_imm" = BaseBVType 32
type instance A.OperandType AArch32 "Arm_bl_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType AArch32 "Arm_blx_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType AArch32 "Arm_br_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType AArch32 "Cc_out" = BaseBVType 1
type instance A.OperandType AArch32 "GPR" = BaseBVType 32
type instance A.OperandType AArch32 "GPRnopc" = BaseBVType 32
type instance A.OperandType AArch32 "Imm0_7" = BaseBVType 3
type instance A.OperandType AArch32 "Imm0_15" = BaseBVType 4
type instance A.OperandType AArch32 "Imm0_31" = BaseBVType 5
type instance A.OperandType AArch32 "Imm0_255" = BaseBVType 8
type instance A.OperandType AArch32 "Imm0_4095" = BaseBVType 16
type instance A.OperandType AArch32 "Ldst_so_reg" = BaseBVType 32
type instance A.OperandType AArch32 "Mod_imm" = BaseBVType 32
type instance A.OperandType AArch32 "Pred" = BaseBVType 4
type instance A.OperandType AArch32 "Reglist" = BaseBVType 16
type instance A.OperandType AArch32 "RGPR" = BaseBVType 32
type instance A.OperandType AArch32 "Shift_so_reg_imm" = BaseBVType 16
type instance A.OperandType AArch32 "So_reg_imm" = BaseBVType 32
type instance A.OperandType AArch32 "So_reg_reg" = BaseBVType 32
type instance A.OperandType AArch32 "T2_so_imm" = BaseBVType 16
-- TODO: Is this the right width for T2_so_reg? Ask Kevin how to figure this out
-- myself.
type instance A.OperandType AArch32 "T2_so_reg" = BaseBVType 32
type instance A.OperandType AArch32 "T_addrmode_is2" = BaseBVType 32
type instance A.OperandType AArch32 "T_addrmode_is4" = BaseBVType 32
type instance A.OperandType AArch32 "T_addrmode_pc" = BaseBVType 8
type instance A.OperandType AArch32 "T_imm0_1020s4" = BaseBVType 8
type instance A.OperandType AArch32 "T_imm0_508s4" = BaseBVType 8
type instance A.OperandType AArch32 "Thumb_bcc_target" = BaseBVType 8
type instance A.OperandType AArch32 "Thumb_blx_target" = BaseBVType 32 -- double-instr val
type instance A.OperandType AArch32 "TGPR" = BaseBVType 32
type instance A.OperandType AArch32 "Unpredictable" = BaseBVType 32

instance A.IsOperandTypeRepr AArch32 where
    type OperandTypeRepr AArch32 = ARMOperandRepr
    operandTypeReprSymbol _ (A32OperandRepr o) = ARMDis.operandReprString o
    operandTypeReprSymbol _ (T32OperandRepr o) = ThumbDis.operandReprString o


operandValue :: forall sym s.
                (S.IsSymExprBuilder sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location AArch32 tp -> IO (S.SymExpr sym tp))
             -> ARMOperand s
             -> IO (A.TaggedExpr AArch32 sym s)
operandValue sym locLookup op = TaggedExpr <$> opV op
  where opV :: ARMOperand s -> IO (A.AllocatedOperand AArch32 sym s)
        opV (A32Operand o) = opVa o
        opV (T32Operand o) = opVt o

        opVa :: ARMDis.Operand s -> IO (A.AllocatedOperand AArch32 sym s)
        opVa (ARMDis.Addr_offset_none gpr) =
          let loc = LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
          in A.LocationOperand loc <$> locLookup loc
        opVa (ARMDis.Addrmode_imm12 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.addrModeImm12ToBits v))
        opVa (ARMDis.Addrmode_imm12_pre v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.addrModeImm12ToBits v))
        opVa (ARMDis.Am2offset_imm v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.am2OffsetImmToBits v))
        opVa (ARMDis.Arm_bl_target v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.branchTargetToBits v))
        opVa (ARMDis.Arm_blx_target v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.branchExecuteTargetToBits v))
        opVa (ARMDis.Arm_br_target v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.branchTargetToBits v))
        opVa (ARMDis.Cc_out v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.sBitToBits v)) -- KWQ: Bool? size?
        opVa (ARMDis.GPR gpr) =
          let loc = LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
          in A.LocationOperand loc <$> locLookup loc
        opVa (ARMDis.GPRnopc gpr) =
          let loc = LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
          in A.LocationOperand loc <$> locLookup loc
        opVa (ARMDis.Ldst_so_reg v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.ldstSoRegToBits v))
        opVa (ARMDis.Mod_imm v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.modImmToBits v))
        opVa (ARMDis.Pred bits4) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.predToBits bits4))
        opVa (ARMDis.Shift_so_reg_imm v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger v)
        opVa (ARMDis.So_reg_imm v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.soRegImmToBits v))
        opVa (ARMDis.So_reg_reg v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.soRegRegToBits v))
        opVa (ARMDis.Unpredictable v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger v)
        opVa unhandled = error $ "operandValue not implemented for " <> show unhandled

        opVt :: ThumbDis.Operand s -> IO (A.AllocatedOperand AArch32 sym s)
        opVt (ThumbDis.Cc_out v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.sBitToBits v))
        opVt (ThumbDis.GPR gpr) =
          let loc = LocGPR (fromIntegral (ThumbOperands.unGPR gpr))
          in A.LocationOperand loc <$> locLookup loc
        opVt (ThumbDis.GPRnopc gpr) =
          let loc = LocGPR (fromIntegral (ThumbOperands.unGPR gpr))
          in A.LocationOperand loc <$> locLookup loc
        opVt (ThumbDis.Imm0_7 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.opcodeToBits v)) -- KWQ: (.&. 7)?
        opVt (ThumbDis.Imm0_15 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.opcodeToBits v)) -- KWQ: (.&. 15)?
        opVt (ThumbDis.Imm0_31 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.imm5ToBits v))
        opVt (ThumbDis.Imm0_255 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger v)  -- v :: Word8
        opVt (ThumbDis.Imm0_4095 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger v) -- v :: Word16
        opVt (ThumbDis.Pred bits4) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.predToBits bits4))
        opVt (ThumbDis.Reglist v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.regListToBits v))
        opVt (ThumbDis.RGPR gpr) =
          let loc = LocGPR (fromIntegral (ThumbOperands.unGPR gpr))
          in A.LocationOperand loc <$> locLookup loc
        opVt (ThumbDis.T_addrmode_is2 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.addrModeIs2ToBits v))
        opVt (ThumbDis.T_addrmode_is4 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.addrModeIs4ToBits v))
        opVt (ThumbDis.T_addrmode_pc v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.addrModePcToBits v))
        opVt (ThumbDis.T_imm0_1020s4 v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.tImm01020S4ToBits v))
        opVt (ThumbDis.T2_so_imm v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.t2SoImmToBits v))
        opVt (ThumbDis.Thumb_bcc_target v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger v)  -- v :: Word8
        opVt (ThumbDis.Thumb_blx_target v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ThumbOperands.thumbBlxTargetToBits v))
        opVt (ThumbDis.TGPR gpr) =
          let loc = LocGPR (fromIntegral (ThumbOperands.unLowGPR gpr))
          in A.LocationOperand loc <$> locLookup loc
        opVt x = error $ "operandValue T32 not implemented for " <> show x


operandToLocation :: ARMOperand s -> Maybe (Location AArch32 (A.OperandType AArch32 s))
operandToLocation (A32Operand (ARMDis.GPR gpr)) = Just $ LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.GPR gpr)) = Just $ LocGPR $ fromIntegral $ ThumbOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.GPRnopc gpr)) = Just $ LocGPR $ fromIntegral $ ThumbOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.RGPR gpr)) = Just $ LocGPR $ fromIntegral $ ThumbOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.TGPR gpr)) = Just $ LocGPR $ fromIntegral $ ThumbOperands.unLowGPR gpr
operandToLocation _ = Nothing

-- ----------------------------------------------------------------------

instance (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm) =>
         A.IsLocation (Location arm) where

  isMemoryLocation LocMem = True
  isMemoryLocation _ = False

  isIP LocPC = True
  isIP _     = False

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

  nonMemLocations = concat
    [ map (Some . LocGPR) [0..numGPR-1],
      [ Some LocPC
      , Some LocCPSR
      ]
    ]
  memLocation = [AL.toMemLoc LocMem]

  registerizationLocations = [] -- map (Some . LocGPR . ARMDis.GPR) (0 : [3..4])

parseLocation :: ARMComp.Parser (Some (Location arm))
parseLocation = do
  c <- P.lookAhead (P.anySingle)
  case c of
    'C' -> Some LocCPSR <$ P.string "CPSR"
    'M' -> Some LocMem <$ P.string "Mem"
    'P' -> Some LocPC <$ P.string "PC"
    'R' -> do
      parsePrefixedRegister (Some . LocGPR) 'R'
    _ -> do
      P.failure (Just $ P.Tokens $ (c:|[])) (Set.fromList $ [ P.Label $ fromList "Location" ])

parsePrefixedRegister :: (Word32 -> b) -> Char -> ARMComp.Parser b
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

type instance A.RegWidth AArch32 = $(litT $ numTyLit regWidth)


instance A.Architecture AArch32 where
    data TaggedExpr AArch32 sym s = TaggedExpr (A.AllocatedOperand AArch32 sym s)
    unTagged (TaggedExpr te) =
      case te of
        A.ValueOperand se -> Just se
        A.LocationOperand _ se -> Just se
        A.CompoundOperand {} -> Nothing
    taggedOperand (TaggedExpr e) = e
    allocateSymExprsForOperand _ = operandValue
    operandToLocation _ = operandToLocation
    uninterpretedFunctions = UF.uninterpretedFunctions
    readMemUF = A.uninterpFnName . UF.mkReadMemUF @AArch32
    writeMemUF = A.uninterpFnName . UF.mkWriteMemUF @AArch32
    locationFuncInterpretation _proxy = A.createSymbolicEntries locationFuncInterpretation
    shapeReprToTypeRepr _proxy = shapeReprType
    operandComponentsImmediate = AOC.operandComponentsImmediate
    -- FIXME: architecture endianness is configurable, not sure how to represent this
    archEndianForm _ = A.LittleEndian

noLocation :: PL.List (A.AllocatedOperand arch sym) sh
           -> F.WrappedOperand arch sh s
           -> BaseTypeRepr tp
           -> Maybe (Location arch tp)
noLocation _ _ _ = Nothing

locationFuncInterpretation :: [(String, A.FunctionInterpretation t st fs AArch32)]
locationFuncInterpretation =
    [ ("arm.is_r15", A.FunctionInterpretation
                       { A.locationInterp = F.LocationFuncInterp noLocation
                       , A.exprInterpName = 'interpIsR15
                       })

    , ("a32.am2offset_imm_imm", A.FunctionInterpretation
                                  { A.locationInterp = F.LocationFuncInterp noLocation
                                  , A.exprInterpName = 'interpAm2offsetimmImmExtractor
                                  })
    , ("a32.am2offset_imm_add", A.FunctionInterpretation
                                  { A.locationInterp = F.LocationFuncInterp noLocation
                                  , A.exprInterpName = 'interpAm2offsetimmAddExtractor
                                  })

    , ("a32.imm12_reg", A.FunctionInterpretation
                          { A.locationInterp = F.LocationFuncInterp interpImm12Reg
                          , A.exprInterpName = 'interpImm12RegExtractor
                          })
    , ("a32.imm12_off", A.FunctionInterpretation
                          { A.locationInterp = F.LocationFuncInterp noLocation
                          , A.exprInterpName = 'interpImm12OffsetExtractor
                          })
    , ("a32.imm12_add", A.FunctionInterpretation
                          { A.locationInterp = F.LocationFuncInterp noLocation
                          , A.exprInterpName = 'interpImm12AddFlgExtractor
                          })

    , ("a32.ldst_so_reg_base_register", A.FunctionInterpretation
                                          { A.locationInterp = F.LocationFuncInterp interpLdstsoregBaseReg
                                          , A.exprInterpName = 'interpLdstsoregBaseRegExtractor
                                          })
    , ("a32.ldst_so_reg_offset_register", A.FunctionInterpretation
                                            { A.locationInterp = F.LocationFuncInterp interpLdstsoregOffReg
                                            , A.exprInterpName = 'interpLdstsoregOffRegExtractor
                                            })
    , ("a32.ldst_so_reg_add", A.FunctionInterpretation
                                { A.locationInterp = F.LocationFuncInterp noLocation
                                , A.exprInterpName = 'interpLdstsoregAddExtractor
                                })
    , ("a32.ldst_so_reg_immediate", A.FunctionInterpretation
                                      { A.locationInterp = F.LocationFuncInterp noLocation
                                      , A.exprInterpName = 'interpLdstsoregImmExtractor
                                      })
    , ("a32.ldst_so_reg_shift_type", A.FunctionInterpretation
                                       { A.locationInterp = F.LocationFuncInterp noLocation
                                       , A.exprInterpName = 'interpLdstsoregTypeExtractor
                                       })

    , ("a32.modimm_imm", A.FunctionInterpretation
                           { A.locationInterp = F.LocationFuncInterp noLocation
                           , A.exprInterpName = 'interpModimmImmExtractor
                           })
    , ("a32.modimm_rot", A.FunctionInterpretation
                           { A.locationInterp = F.LocationFuncInterp noLocation
                           , A.exprInterpName = 'interpModimmRotExtractor
                           })

    , ("a32.soregimm_type", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp noLocation
                              , A.exprInterpName = 'interpSoregimmTypeExtractor
                              })
    , ("a32.soregimm_imm", A.FunctionInterpretation
                             { A.locationInterp = F.LocationFuncInterp noLocation
                             , A.exprInterpName = 'interpSoregimmImmExtractor
                             })
    , ("a32.soregimm_reg", A.FunctionInterpretation
                             { A.locationInterp = F.LocationFuncInterp interpSoregimmReg
                             , A.exprInterpName = 'interpSoregimmRegExtractor })

    , ("a32.soregreg_type", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp noLocation
                              , A.exprInterpName = 'interpSoregregTypeExtractor
                              })
    , ("a32.soregreg_reg1", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp interpSoregregReg1
                              , A.exprInterpName = 'interpSoregregReg1Extractor })
    , ("a32.soregreg_reg2", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp interpSoregregReg2
                              , A.exprInterpName = 'interpSoregregReg2Extractor })

    , ("t32.blxtarget_S", A.FunctionInterpretation
                            { A.locationInterp = F.LocationFuncInterp noLocation
                            , A.exprInterpName = 'interpBlxTarget_S
                            })
    , ("t32.blxtarget_imm10H", A.FunctionInterpretation
                                 { A.locationInterp = F.LocationFuncInterp noLocation
                                 , A.exprInterpName = 'interpBlxTarget_imm10H
                                 })
    , ("t32.blxtarget_imm10L", A.FunctionInterpretation
                                 { A.locationInterp = F.LocationFuncInterp noLocation
                                 , A.exprInterpName = 'interpBlxTarget_imm10L
                                 })
    , ("t32.blxtarget_J1", A.FunctionInterpretation
                             { A.locationInterp = F.LocationFuncInterp noLocation
                             , A.exprInterpName = 'interpBlxTarget_J1
                             })
    , ("t32.blxtarget_J2", A.FunctionInterpretation
                             { A.locationInterp = F.LocationFuncInterp noLocation
                             , A.exprInterpName = 'interpBlxTarget_J2
                             })

    , ("t32.imm0_1020S4_imm", A.FunctionInterpretation
                                { A.locationInterp = F.LocationFuncInterp noLocation
                                , A.exprInterpName = 'interpImm01020s4ImmExtractor
                                })
    , ("t32.imm0_508S4_imm", A.FunctionInterpretation
                             { A.locationInterp = F.LocationFuncInterp noLocation
                             , A.exprInterpName = 'interpImm0508s4ImmExtractor
                             })
    , ("t32.reglist", A.FunctionInterpretation
                        { A.locationInterp = F.LocationFuncInterp noLocation
                        , A.exprInterpName = 'interpTReglistExtractor
                        })
    , ("t32.addrmode_is2_imm", A.FunctionInterpretation
                                 { A.locationInterp = F.LocationFuncInterp noLocation
                                 , A.exprInterpName = 'interpTaddrmodeis2ImmExtractor
                                 })
    , ("t32.addrmode_is2_reg", A.FunctionInterpretation
                                 { A.locationInterp = F.LocationFuncInterp interpTaddrmodeis2Reg
                                 , A.exprInterpName = 'interpTaddrmodeis2RegExtractor
                                 })
    , ("t32.addrmode_is4_imm", A.FunctionInterpretation
                                 { A.locationInterp = F.LocationFuncInterp noLocation
                                 , A.exprInterpName = 'interpTaddrmodeis4ImmExtractor
                                 })
    , ("t32.addrmode_is4_reg", A.FunctionInterpretation
                                 { A.locationInterp = F.LocationFuncInterp interpTaddrmodeis4Reg
                                 , A.exprInterpName = 'interpTaddrmodeis4RegExtractor
                                 })
    , ("t32.addrmode_pc", A.FunctionInterpretation
                            { A.locationInterp = F.LocationFuncInterp noLocation
                            , A.exprInterpName = 'interpTaddrmodepcExtractor
                            })
    , ("t32.t2soimm_imm", A.FunctionInterpretation
                            { A.locationInterp = F.LocationFuncInterp noLocation
                            , A.exprInterpName = 'interpT2soimmImmExtractor
                            })
    , ("t32.t2soreg_reg", A.FunctionInterpretation
                            { A.locationInterp = F.LocationFuncInterp interpT2soregReg
                            , A.exprInterpName = 'interpT2soregRegExtractor
                            })
    , ("t32.t2soreg_imm", A.FunctionInterpretation
                            { A.locationInterp = F.LocationFuncInterp noLocation
                            , A.exprInterpName = 'interpT2soregImmExtractor
                            })
    , ("t32.t2soreg_type", A.FunctionInterpretation
                            { A.locationInterp = F.LocationFuncInterp noLocation
                            , A.exprInterpName = 'interpT2soregTypeExtractor
                            })
    ]

shapeReprType :: forall tp . ARMOperandRepr tp -> BaseTypeRepr (A.OperandType AArch32 tp)
shapeReprType orep =
    case orep of
      A32OperandRepr a32rep ->
          case a32rep of
            ARMDis.Addr_offset_noneRepr -> knownRepr
            ARMDis.Addrmode_imm12Repr -> knownRepr
            ARMDis.Addrmode_imm12_preRepr -> knownRepr
            ARMDis.Am2offset_immRepr -> knownRepr
            ARMDis.Arm_bl_targetRepr -> knownRepr
            ARMDis.Arm_blx_targetRepr -> knownRepr
            ARMDis.Arm_br_targetRepr -> knownRepr
            ARMDis.Cc_outRepr -> knownRepr
            ARMDis.GPRRepr -> knownRepr
            ARMDis.GPRnopcRepr -> knownRepr
            ARMDis.Ldst_so_regRepr -> knownRepr
            ARMDis.Mod_immRepr -> knownRepr
            ARMDis.PredRepr -> knownRepr
            ARMDis.Shift_so_reg_immRepr -> knownRepr
            ARMDis.So_reg_immRepr -> knownRepr
            ARMDis.So_reg_regRepr -> knownRepr
            ARMDis.UnpredictableRepr -> knownRepr
            _ -> error $ "Unknown A32 OperandRepr: " <> show (A.operandTypeReprSymbol (Proxy @AArch32) orep)
      T32OperandRepr t32rep ->
          case t32rep of
            ThumbDis.Cc_outRepr -> knownRepr
            ThumbDis.GPRRepr -> knownRepr
            ThumbDis.GPRnopcRepr -> knownRepr
            ThumbDis.Imm0_7Repr -> knownRepr
            ThumbDis.Imm0_15Repr -> knownRepr
            ThumbDis.Imm0_31Repr -> knownRepr
            ThumbDis.Imm0_255Repr -> knownRepr
            ThumbDis.Imm0_4095Repr -> knownRepr
            ThumbDis.PredRepr -> knownRepr
            ThumbDis.ReglistRepr -> knownRepr
            ThumbDis.RGPRRepr -> knownRepr
            ThumbDis.T_addrmode_is2Repr -> knownRepr
            ThumbDis.T_addrmode_is4Repr -> knownRepr
            ThumbDis.T_addrmode_pcRepr -> knownRepr
            ThumbDis.T_imm0_1020s4Repr -> knownRepr
            ThumbDis.T_imm0_508s4Repr -> knownRepr
            ThumbDis.T2_so_immRepr -> knownRepr
            ThumbDis.T2_so_regRepr -> knownRepr
            ThumbDis.Thumb_blx_targetRepr -> knownRepr
            ThumbDis.Thumb_bcc_targetRepr -> knownRepr
            ThumbDis.TGPRRepr -> knownRepr
            ThumbDis.UnpredictableRepr -> knownRepr
            _ -> error $ "Unknown T32 OperandRepr: " <> show (A.operandTypeReprSymbol (Proxy @AArch32) orep)


-- ----------------------------------------------------------------------

data Signed = Signed | Unsigned deriving (Eq, Show)


type instance A.OperandComponents AArch32 sym = AOC.OperandComponents AArch32 sym

instance T.TemplatableOperand AArch32 where
  opTemplates sr =
      case sr of
        (A32OperandRepr a) -> a32template a
        (T32OperandRepr a) -> t32template a

a32template :: ARMDis.OperandRepr s -> [T.TemplatedOperand AArch32 s]
a32template a32sr =
    case a32sr of
      ARMDis.Addrmode_imm12Repr ->
          mkTemplate <$> [0..numGPR-1]
              where mkTemplate gprNum = T.TemplatedOperand Nothing
                                        (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
                                            :: T.TemplatedOperand AArch32 "Addrmode_imm12"
                        where mkTemplate' :: forall sym
                                           . (S.IsSymExprBuilder sym)
                                          => sym
                                          -> (forall tp . Location AArch32 tp -> IO (S.SymExpr sym tp))
                                          -> IO (A.AllocatedOperand AArch32 sym "Addrmode_imm12",
                                                 T.RecoverOperandFn sym (A.Operand AArch32 "Addrmode_imm12"))
                              mkTemplate' sym locLookup = do
                                let gprN = ARMOperands.gpr $ fromIntegral gprNum
                                let loc = LocGPR gprNum
                                base <- locLookup loc
                                offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_off") knownRepr
                                addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_add") knownRepr
                                let recover :: (forall tp . S.SymExpr sym tp -> IO (WE.GroundValue tp)) -> IO (A.Operand AArch32 "Addrmode_imm12")
                                    recover evalFn = do
                                      offsetVal <- fromInteger <$> evalFn offset
                                      addflagVal <- fromInteger <$> evalFn addflag
                                      return $ A32Operand $ ARMDis.Addrmode_imm12 $
                                             ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                                return ( A.CompoundOperand (AOC.OCAddrmodeImm12 loc base offset addflag)
                                       , T.RecoverOperandFn recover
                                       )
      ARMDis.Addrmode_imm12_preRepr ->
          mkTemplate <$> [0..numGPR-1]
            where mkTemplate gprNum = T.TemplatedOperand Nothing
                                      (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
                                          :: T.TemplatedOperand AArch32 "Addrmode_imm12_pre"
                    where mkTemplate' :: forall sym
                                       . (S.IsSymExprBuilder sym)
                                      => sym
                                      -> (forall tp . Location AArch32 tp -> IO (S.SymExpr sym tp))
                                      -> IO (A.AllocatedOperand AArch32 sym "Addrmode_imm12_pre",
                                             T.RecoverOperandFn sym (A.Operand AArch32 "Addrmode_imm12_pre"))
                          mkTemplate' sym locLookup = do
                            let gprN = ARMOperands.gpr $ fromIntegral gprNum
                            let loc = LocGPR gprNum
                            base <- locLookup loc
                            offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_off") knownRepr
                            addflag :: S.SymExpr sym (BaseBVType 1)
                                    <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_add") knownRepr
                            let recover :: (forall tp . S.SymExpr sym tp -> IO (WE.GroundValue tp)) -> IO (A.Operand AArch32 "Addrmode_imm12_pre")
                                recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  addflagVal <- fromInteger <$> evalFn addflag
                                  return $ A32Operand $ ARMDis.Addrmode_imm12_pre $
                                         ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                            return ( A.CompoundOperand (AOC.OCAddrmodeImm12 loc base offset addflag)
                                   , T.RecoverOperandFn recover
                                   )
      ARMDis.Arm_bl_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_blx_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_br_targetRepr -> error "opTemplate ARM_br_targetRepr TBD"
      ARMDis.Cc_outRepr -> error "opTemplate ARM_Cc_outRepr TBD"
      ARMDis.GPRRepr -> concreteTemplatedOperand (A32Operand . ARMDis.GPR . ARMOperands.gpr . fromIntegral) LocGPR <$> [0..numGPR-1]
      ARMDis.Mod_immRepr -> error "opTemplate ARM_Mod_immRepr TBD"
      ARMDis.PredRepr -> [symbolicTemplatedOperand (Proxy @4) Unsigned "Pred"
                          (A32Operand . ARMDis.Pred . ARMDis.mkPred . fromInteger)]
      ARMDis.Shift_so_reg_immRepr -> error "opTemplate Shift_so_reg_immRepr TBD"
      ARMDis.So_reg_immRepr -> error "opTemplate So_reg_immRepr TBD"
      -- ARMDis.So_reg_regRepr ->
      --     mkTemplate <$> [0..numGPR-1]
      --       where mkTemplate gprNum = T.TemplatedOperand Nothing
      --                                 (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
      --                                     :: T.TemplatedOperand ARM "So_reg_reg"
      --               where mkTemplate' :: T.TemplatedOperandFn ARM "So_reg_reg"
      --                     mkTemplate' sym locLookup = do
      --                       let gprN = ARMOperands.gpr gprNum
      --                       base <- A.unTagged <$> A.operandValue (Proxy @ARM) sym locLookup (ARMDis.GPR $ gprN)
      --                       offset <- S.freshConstant sym (U.makeSymbol "So_reg_reg_shift") knownRepr
      --                       expr <- S.bvAdd sym offset offset -- KWQ!
      --                       let recover evalFn = do
      --                             offsetVal <- fromInteger <$> evalFn offset
      --                             return $ ARMDis.So_reg_reg $ ARMOperands.SoRegReg gprN gprN offsetVal
      --                       return (expr, T.RecoverOperandFn recover)
      ARMDis.UnpredictableRepr -> error "opTemplate ARM_UnpredictableRepr TBD... and are you sure?"

t32template :: ThumbDis.OperandRepr s -> [T.TemplatedOperand AArch32 s]
t32template t32sr =
    case t32sr of
      _ -> error "opTemplate T32 ?? TBD"



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
          ao <- A.taggedOperand <$> A.allocateSymExprsForOperand (Proxy @arch) sym locLookup (op x)
          return (ao, T.RecoverOperandFn $ const (return (op x)))


symbolicTemplatedOperand :: forall arch s (bits :: Nat)
                          . (A.OperandType arch s ~ BaseBVType bits,
                             KnownNat bits,
                             1 <= bits)
                         => Proxy bits
                         -> Signed
                         -> String
                         -> (Integer -> A.Operand arch s)
                         -> T.TemplatedOperand arch s
symbolicTemplatedOperand Proxy _signed name constr =
  T.TemplatedOperand { T.templOpLocation = Nothing
                     , T.templUsedLocations = Set.empty
                     , T.templOpFn = mkTemplate'
                     }
  where mkTemplate' :: T.TemplatedOperandFn arch s
        mkTemplate' sym _ = do
          v <- S.freshConstant sym (U.makeSymbol name) (knownRepr :: BaseTypeRepr (BaseBVType bits))
          let recover evalFn = constr <$> evalFn v
          return (A.ValueOperand v, T.RecoverOperandFn recover)
