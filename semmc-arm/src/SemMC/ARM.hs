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

{-# OPTIONS_GHC -Wno-orphans #-}

module SemMC.ARM
    ( ARM
    , MachineState(..)
    , Instruction
    , ARMOpcode(..)
    , ARMOperand(..)
    , testSerializer
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
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Vector.Sized as V
import           Data.Word ( Word8, Word32 )
import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMOperands
import qualified Dismantle.Thumb as ThumbDis
import qualified Dismantle.Thumb.Operands as ThumbOperands
import           GHC.TypeLits
import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified SemMC.Architecture as A
import           SemMC.Architecture.ARM.BaseSemantics ( numGPR )
import           SemMC.Architecture.ARM.Combined
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
-- inhabitants, but this is used as a phantom type selector.  This
-- includes both A32 and T32 instruction modes.
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

type instance A.Opcode   ARM = ARMOpcode
type instance A.Operand  ARM = ARMOperand
type instance A.Location ARM = Location ARM

instance A.IsOpcode  ARMOpcode
instance A.IsOperand ARMOperand

type instance A.OperandType ARM "Addr_offset_none" = BaseBVType 32
type instance A.OperandType ARM "Addrmode_imm12" = BaseBVType 32
type instance A.OperandType ARM "Addrmode_imm12_pre" = BaseBVType 32
type instance A.OperandType ARM "Am2offset_imm" = BaseBVType 32
type instance A.OperandType ARM "Arm_bl_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType ARM "Arm_blx_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType ARM "Arm_br_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType ARM "Cc_out" = BaseBVType 1
type instance A.OperandType ARM "GPR" = BaseBVType 32
type instance A.OperandType ARM "GPRnopc" = BaseBVType 32
type instance A.OperandType ARM "Imm0_7" = BaseBVType 3
type instance A.OperandType ARM "Imm0_31" = BaseBVType 5
type instance A.OperandType ARM "Imm0_255" = BaseBVType 8
type instance A.OperandType ARM "Imm0_4095" = BaseBVType 16
type instance A.OperandType ARM "Ldst_so_reg" = BaseBVType 32
type instance A.OperandType ARM "Mod_imm" = BaseBVType 32
type instance A.OperandType ARM "Pred" = BaseBVType 4
type instance A.OperandType ARM "RGPR" = BaseBVType 32
type instance A.OperandType ARM "Shift_so_reg_imm" = BaseBVType 16
type instance A.OperandType ARM "So_reg_imm" = BaseBVType 32
type instance A.OperandType ARM "So_reg_reg" = BaseBVType 32
type instance A.OperandType ARM "T2_so_imm" = BaseBVType 16
type instance A.OperandType ARM "T_imm0_1020s4" = BaseBVType 8
type instance A.OperandType ARM "Thumb_blx_target" = BaseBVType 32 -- double-instr val
type instance A.OperandType ARM "TGPR" = BaseBVType 32
type instance A.OperandType ARM "Unpredictable" = BaseBVType 32

instance A.IsOperandTypeRepr ARM where
    type OperandTypeRepr ARM = ARMOperandRepr
    operandTypeReprSymbol _ (A32OperandRepr o) = ARMDis.operandReprString o
    operandTypeReprSymbol _ (T32OperandRepr o) = ThumbDis.operandReprString o


operandValue :: forall sym s.
                (S.IsSymInterface sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location ARM tp -> IO (S.SymExpr sym tp))
             -> ARMOperand s
             -> IO (A.TaggedExpr ARM sym s)
operandValue sym locLookup op = TaggedExpr <$> opV op
  where opV :: ARMOperand s -> IO (S.SymExpr sym (A.OperandType ARM s))
        opV (A32Operand o) = opVa o
        opV (T32Operand o) = opVt o

        opVa :: ARMDis.Operand s -> IO (S.SymExpr sym (A.OperandType ARM s))
        opVa (ARMDis.Addr_offset_none gpr) = locLookup (LocGPR $ ARMOperands.unGPR gpr)
        opVa (ARMDis.Addrmode_imm12 v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.addrModeImm12ToBits v
        opVa (ARMDis.Addrmode_imm12_pre v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.addrModeImm12ToBits v
        opVa (ARMDis.Am2offset_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.am2OffsetImmToBits v
        opVa (ARMDis.Arm_bl_target v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.branchTargetToBits v
        opVa (ARMDis.Arm_blx_target v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.branchExecuteTargetToBits v
        opVa (ARMDis.Arm_br_target v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.branchTargetToBits v
        opVa (ARMDis.Cc_out v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.sBitToBits v -- KWQ: Bool? size?
        opVa (ARMDis.GPR gpr) = locLookup (LocGPR $ ARMOperands.unGPR gpr)
        opVa (ARMDis.Ldst_so_reg v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.ldstSoRegToBits v
        opVa (ARMDis.Mod_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.modImmToBits v
        opVa (ARMDis.Pred bits4) = S.bvLit sym knownNat $ toInteger $ ARMOperands.predToBits bits4
        opVa (ARMDis.Shift_so_reg_imm v) = S.bvLit sym knownNat $ toInteger v
        opVa (ARMDis.So_reg_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.soRegImmToBits v
        opVa (ARMDis.So_reg_reg v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.soRegRegToBits v
        opVa (ARMDis.Unpredictable v) = S.bvLit sym knownNat $ toInteger v
        -- opV unhandled = error $ "operandValue not implemented for " <> show unhandled

        opVt :: ThumbDis.Operand s -> IO (S.SymExpr sym (A.OperandType ARM s))
        opVt (ThumbDis.Cc_out v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.sBitToBits v
        opVt (ThumbDis.GPR gpr) = locLookup (LocGPR $ ThumbOperands.unGPR gpr)
        opVt (ThumbDis.GPRnopc gpr) = locLookup (LocGPR $ ThumbOperands.unGPR gpr)
        opVt (ThumbDis.Imm0_7 v) = S.bvLit sym knownNat $ toInteger $ ThumbOperands.opcodeToBits v -- KWQ: (.&. 7)?
        opVt (ThumbDis.Imm0_31 v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.imm5ToBits v
        opVt (ThumbDis.Imm0_255 v) = S.bvLit sym knownNat $ toInteger v  -- v :: Word8
        opVt (ThumbDis.Imm0_4095 v) = S.bvLit sym knownNat $ toInteger v -- v :: Word16
        opVt (ThumbDis.RGPR gpr) = locLookup (LocGPR $ ThumbOperands.unGPR gpr)
        opVt (ThumbDis.T_imm0_1020s4 v) = S.bvLit sym knownNat $ toInteger $ ThumbOperands.tImm01020S4ToBits v
        opVt (ThumbDis.T2_so_imm v) = S.bvLit sym knownNat $ toInteger $ ThumbOperands.t2SoImmToBits v
        opVt (ThumbDis.Thumb_blx_target v) = S.bvLit sym knownNat $ toInteger $ ThumbOperands.thumbBlxTargetToBits v
        opVt (ThumbDis.TGPR gpr) = locLookup (LocGPR $ ThumbOperands.unLowGPR gpr)
        opVt x = error $ "operandValue T32 not implemented for " <> show x


operandToLocation :: ARMOperand s -> Maybe (Location ARM (A.OperandType ARM s))
operandToLocation (A32Operand (ARMDis.GPR gpr)) = Just $ LocGPR $ ARMOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.GPR gpr)) = Just $ LocGPR $ ThumbOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.GPRnopc gpr)) = Just $ LocGPR $ ThumbOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.RGPR gpr)) = Just $ LocGPR $ ThumbOperands.unGPR gpr
operandToLocation (T32Operand (ThumbDis.TGPR gpr)) = Just $ LocGPR $ ThumbOperands.unLowGPR gpr
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
    [ map (Some . LocGPR) [0..numGPR-1],
      [ Some LocPC
      , Some LocCPSR
      , Some LocMem
      ]
    ]

  registerizationLocations = [] -- map (Some . LocGPR . ARMDis.GPR) (0 : [3..4])

parseLocation :: ARMComp.Parser (Some (Location ARM))
parseLocation = do
  c <- P.lookAhead (P.anyChar)
  case c of
    'C' -> Some LocCPSR <$ P.string "CPSR"
    'M' -> Some LocMem <$ P.string "Mem"
    'P' -> Some LocPC <$ P.string "PC"
    'R' -> parsePrefixedRegister (Some . LocGPR) 'R'
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

    , ("a32.am2offset_imm_imm", A.FunctionInterpretation { A.exprInterpName = 'interpAm2offsetimmImmExtractor })
    , ("a32.am2offset_imm_add", A.FunctionInterpretation { A.exprInterpName = 'interpAm2offsetimmAddExtractor })

    , ("a32.imm12_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpImm12Reg
                                                 , A.exprInterpName = 'interpImm12RegExtractor
                                                 })
    , ("a32.imm12_off", A.FunctionInterpretation { A.exprInterpName = 'interpImm12OffsetExtractor })
    , ("a32.imm12_add", A.FunctionInterpretation { A.exprInterpName = 'interpImm12AddFlgExtractor })

    , ("a32.ldst_so_reg_base_register", A.FunctionInterpretation
                                          { A.locationInterp = F.LocationFuncInterp interpLdstsoregBaseReg
                                          , A.exprInterpName = 'interpLdstsoregBaseRegExtractor })
    , ("a32.ldst_so_reg_offset_register", A.FunctionInterpretation
                                            { A.locationInterp = F.LocationFuncInterp interpLdstsoregOffReg
                                            , A.exprInterpName = 'interpLdstsoregOffRegExtractor })
    , ("a32.ldst_so_reg_add", A.FunctionInterpretation { A.exprInterpName = 'interpLdstsoregAddExtractor })
    , ("a32.ldst_so_reg_immediate", A.FunctionInterpretation { A.exprInterpName = 'interpLdstsoregImmExtractor })
    , ("a32.ldst_so_reg_shift_type", A.FunctionInterpretation { A.exprInterpName = 'interpLdstsoregTypeExtractor })

    , ("a32.modimm_imm", A.FunctionInterpretation { A.exprInterpName = 'interpModimmImmExtractor })
    , ("a32.modimm_rot", A.FunctionInterpretation { A.exprInterpName = 'interpModimmRotExtractor })

    , ("a32.soregimm_type", A.FunctionInterpretation { A.exprInterpName = 'interpSoregimmTypeExtractor })
    , ("a32.soregimm_imm",  A.FunctionInterpretation { A.exprInterpName = 'interpSoregimmImmExtractor })
    , ("a32.soregimm_reg",  A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpSoregimmReg
                                                     , A.exprInterpName = 'interpSoregimmRegExtractor })

    , ("a32.soregreg_type", A.FunctionInterpretation { A.exprInterpName = 'interpSoregregTypeExtractor })
    , ("a32.soregreg_reg1", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpSoregregReg1
                                                     , A.exprInterpName = 'interpSoregregReg1Extractor })
    , ("a32.soregreg_reg2", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpSoregregReg2
                                                     , A.exprInterpName = 'interpSoregregReg2Extractor })

    ]

shapeReprType :: forall tp . ARMOperandRepr tp -> BaseTypeRepr (A.OperandType ARM tp)
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
            ARMDis.Ldst_so_regRepr -> knownRepr
            ARMDis.Mod_immRepr -> knownRepr
            ARMDis.PredRepr -> knownRepr
            ARMDis.Shift_so_reg_immRepr -> knownRepr
            ARMDis.So_reg_immRepr -> knownRepr
            ARMDis.So_reg_regRepr -> knownRepr
            ARMDis.UnpredictableRepr -> knownRepr
            _ -> error $ "Unknown A32 OperandRepr: " <> show (A.operandTypeReprSymbol (Proxy @ARM) orep)
      T32OperandRepr t32rep ->
          case t32rep of
            ThumbDis.Cc_outRepr -> knownRepr
            ThumbDis.GPRRepr -> knownRepr
            ThumbDis.GPRnopcRepr -> knownRepr
            ThumbDis.Imm0_7Repr -> knownRepr
            ThumbDis.Imm0_31Repr -> knownRepr
            ThumbDis.Imm0_255Repr -> knownRepr
            ThumbDis.Imm0_4095Repr -> knownRepr
            ThumbDis.RGPRRepr -> knownRepr
            ThumbDis.T_imm0_1020s4Repr -> knownRepr
            ThumbDis.T2_so_immRepr -> knownRepr
            ThumbDis.Thumb_blx_targetRepr -> knownRepr
            ThumbDis.TGPRRepr -> knownRepr
            _ -> error $ "Unknown T32 OperandRepr: " <> show (A.operandTypeReprSymbol (Proxy @ARM) orep)


-- ----------------------------------------------------------------------

data Signed = Signed | Unsigned deriving (Eq, Show)

instance T.TemplatableOperand ARM where
  opTemplates sr =
      case sr of
        (A32OperandRepr a) -> a32template a
        (T32OperandRepr a) -> t32template a

a32template :: ARMDis.OperandRepr s -> [T.TemplatedOperand ARM s]
a32template a32sr =
    case a32sr of
      ARMDis.Addrmode_imm12Repr ->
          mkTemplate <$> [0..numGPR-1]
              where mkTemplate gprNum = T.TemplatedOperand Nothing
                                        (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
                                            :: T.TemplatedOperand ARM "Addrmode_imm12"
                        where mkTemplate' :: T.TemplatedOperandFn ARM "Addrmode_imm12"
                              mkTemplate' sym locLookup = do
                                let gprN = ARMOperands.gpr gprNum
                                base <- A.unTagged <$> A.operandValue (Proxy @ARM) sym locLookup
                                                          (A32Operand $ ARMDis.GPR gprN)
                                offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_off") knownRepr
                                addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_add") knownRepr
                                expr <- S.bvAdd sym base offset -- KWQ: need to reproduce offset manipulation
                                let recover evalFn = do
                                      offsetVal <- fromInteger <$> evalFn offset
                                      addflagVal <- fromInteger <$> evalFn addflag
                                      return $ A32Operand $ ARMDis.Addrmode_imm12 $
                                             ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                                return (expr, T.WrappedRecoverOperandFn recover)
      ARMDis.Addrmode_imm12_preRepr ->
          mkTemplate <$> [0..numGPR-1]
            where mkTemplate gprNum = T.TemplatedOperand Nothing
                                      (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
                                          :: T.TemplatedOperand ARM "Addrmode_imm12_pre"
                    where mkTemplate' :: T.TemplatedOperandFn ARM "Addrmode_imm12_pre"
                          mkTemplate' sym locLookup = do
                            let gprN = ARMOperands.gpr $ gprNum
                            base <- A.unTagged <$> A.operandValue (Proxy @ARM) sym locLookup (A32Operand $ ARMDis.GPR gprN)
                            offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_off") knownRepr
                            addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_add") knownRepr
                            expr <- S.bvAdd sym base offset -- KWQ: need to reproduce offset manipulation
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  addflagVal <- fromInteger <$> evalFn addflag
                                  return $ A32Operand $ ARMDis.Addrmode_imm12_pre $
                                         ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                            return (expr, T.WrappedRecoverOperandFn recover)
      ARMDis.Arm_bl_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_blx_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_br_targetRepr -> error "opTemplate ARM_br_targetRepr TBD"
      ARMDis.Cc_outRepr -> error "opTemplate ARM_Cc_outRepr TBD"
      ARMDis.GPRRepr -> concreteTemplatedOperand (A32Operand . ARMDis.GPR . ARMOperands.gpr) LocGPR <$> [0..numGPR-1]
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
      --                       return (expr, T.WrappedRecoverOperandFn recover)
      ARMDis.UnpredictableRepr -> error "opTemplate ARM_UnpredictableRepr TBD... and are you sure?"

t32template :: ThumbDis.OperandRepr s -> [T.TemplatedOperand ARM s]
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
