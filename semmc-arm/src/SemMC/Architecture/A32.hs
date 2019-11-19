{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SemMC.Architecture.A32
    ( A32
    , Instruction
    , ConcreteState
    , numGPR
    , testSerializer
    )
    where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( replicateM, forM )
import qualified Control.Monad.State.Strict as St
import           Control.Monad.Trans ( liftIO )
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty ( NonEmpty(..), fromList )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Proxy ( Proxy(..) )
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Word.Indexed as W
import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMOperands
import qualified Dismantle.Instruction as D
import           GHC.Stack ( HasCallStack )
import           GHC.TypeLits
import qualified GHC.Err.Located as L
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as AL
import           SemMC.Architecture.ARM.BaseSemantics.Registers ( numGPR, GPRIdent )
import qualified SemMC.Architecture.ARM.Components as ARMComp
import           SemMC.Architecture.ARM.Eval
import qualified SemMC.Architecture.ARM.UF as UF
import           SemMC.Architecture.A32.Location
import qualified SemMC.Architecture.ARM.OperandComponents as AOC
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Eval as FE
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import           What4.BaseTypes
import qualified What4.Expr as WEB
import qualified What4.Interface as S


-- | Define the arch type for only the A32 instruction set.
data A32

type Instruction = LB.ByteString

type ConcreteState = MapF.MapF (Location A32) V.Value

testSerializer :: CE.TestSerializer (V.ConcreteState A32) (A.Instruction A32)
testSerializer = CE.TestSerializer { CE.flattenMachineState = serializeState
                                   , CE.parseMachineState = deserializeState
                                   , CE.flattenProgram = mconcat . fmap ARMDis.assembleInstruction
                                   }

-- ----------------------------------------------------------------------

instance AC.ConcreteArchitecture A32 where
  registerizeInstruction = registerizeInstructionA32
  operandType _proxy = operandTypeA32
  zeroState _proxy = zeroState
  randomState _proxy = mkRandomState
  serialize _proxy = serializeState
  deserialize _proxy = deserializeState
  operandToSemanticView _proxy = undefined -- operandToSemanticViewPPC
  heuristicallyInterestingStates _proxy = interestingStates
  readView = P.parseMaybe (V.parseView parseLocation)
  showView = V.printView show

operandTypeA32 :: ARMDis.Operand s -> BaseTypeRepr (A.OperandType A32 s)
operandTypeA32 o =
  case o of
      ARMDis.Addr_offset_none {}   -> knownRepr
      ARMDis.Addrmode3 {}          -> knownRepr
      ARMDis.Addrmode3_pre {}      -> knownRepr
      ARMDis.Addrmode5 {}          -> knownRepr
      ARMDis.Addrmode5_pre {}      -> knownRepr
      ARMDis.Addrmode_imm12 {}     -> knownRepr
      ARMDis.Addrmode_imm12_pre {} -> knownRepr
      ARMDis.Adrlabel {}           -> knownRepr
      ARMDis.Am2offset_imm {}      -> knownRepr
      ARMDis.Am2offset_reg {}      -> knownRepr
      ARMDis.Am3offset {}          -> knownRepr
      ARMDis.Arm_bl_target {}      -> knownRepr
      ARMDis.Arm_blx_target {}     -> knownRepr
      ARMDis.Arm_br_target {}      -> knownRepr
      ARMDis.Bf_inv_mask_imm {}    -> knownRepr
      ARMDis.C_imm {}              -> knownRepr
      ARMDis.Cc_out {}             -> knownRepr
      ARMDis.Coproc_option_imm {}  -> knownRepr
      ARMDis.Dpr {}                -> knownRepr
      ARMDis.GPR {}                -> knownRepr
      ARMDis.GPRPairOp {}          -> knownRepr
      ARMDis.GPRnopc {}            -> knownRepr
      ARMDis.Iflags_op {}          -> knownRepr
      ARMDis.Imm0_1 {}             -> knownRepr
      ARMDis.Imm0_15 {}            -> knownRepr
      ARMDis.Imm0_31 {}            -> knownRepr
      ARMDis.Imm0_65535 {}         -> knownRepr
      ARMDis.Imm0_7 {}             -> knownRepr
      ARMDis.Imm0_239 {}           -> knownRepr
      ARMDis.Imm0_65535_expr {}    -> knownRepr
      ARMDis.Imm1_16 {}            -> knownRepr
      ARMDis.Imm1_32 {}            -> knownRepr
      ARMDis.Imm24b {}             -> knownRepr
      ARMDis.Imod_op {}            -> knownRepr
      ARMDis.Ldst_so_reg {}        -> knownRepr
      ARMDis.Memb_opt {}           -> knownRepr
      ARMDis.Instsyncb_opt {}      -> knownRepr
      ARMDis.Mod_imm {}            -> knownRepr
      ARMDis.Msr_mask {}           -> knownRepr
      ARMDis.P_imm {}              -> knownRepr
      ARMDis.Pkh_asr_amt {}        -> knownRepr
      ARMDis.Pkh_lsl_amt {}        -> knownRepr
      ARMDis.Postidx_imm8 {}       -> knownRepr
      ARMDis.Postidx_imm8s4 {}     -> knownRepr
      ARMDis.Postidx_reg {}        -> knownRepr
      ARMDis.Pred {}               -> knownRepr
      ARMDis.Qpr {}                -> knownRepr
      ARMDis.Qqpr {}               -> knownRepr
      ARMDis.GPRwithAPSR {}        -> knownRepr
      ARMDis.Reglist {}            -> knownRepr
      ARMDis.Rot_imm {}            -> knownRepr
      ARMDis.Setend_op {}          -> knownRepr
      ARMDis.Shift_imm {}          -> knownRepr
      ARMDis.Shift_so_reg_imm {}   -> knownRepr
      ARMDis.Shift_so_reg_reg {}   -> knownRepr
      ARMDis.So_reg_imm {}         -> knownRepr
      ARMDis.So_reg_reg {}         -> knownRepr
      ARMDis.TcGPR {}              -> knownRepr
      ARMDis.Unpredictable {}      -> knownRepr

registerizeInstructionA32 :: AC.RegisterizedInstruction A32
                          -> V.ConcreteState A32
                          -> (A.Instruction A32, V.ConcreteState A32)
registerizeInstructionA32 ri s =
  case ri of
    AC.RI { AC.riOpcode = opc
          , AC.riOperands = ops
          , AC.riLiteralLocs = lls
          } ->
      case MapF.foldrWithKey replaceLiterals (ops, s) lls of
        (ops', s') -> (D.Instruction opc ops', s')

replaceLiterals :: AC.LiteralRef A32 sh s
                -> Location A32 s
                -> (SL.List ARMDis.Operand sh, V.ConcreteState A32)
                -> (SL.List ARMDis.Operand sh, V.ConcreteState A32)
replaceLiterals (AC.LiteralRef ix) loc (ops, s) =
  case MapF.lookup loc s of
    Nothing -> L.error ("Location not defined in state: " ++ showF loc)
    Just val ->
      let (clampedValue, op') = truncateValue (ops SL.!! ix) val
      in (SL.update ops ix (const op'), MapF.insert loc clampedValue s)

-- | Replace the value in the given immediate operand with the value in a
-- 'V.Value', truncating it if necessary.  The truncated value is returned so
-- that the test case can be updated.
--
-- Note that this function calls error on operands that are not immediates.
truncateValue :: ARMDis.Operand s
              -> V.Value (A.OperandType A32 s)
              -> (V.Value (A.OperandType A32 s), ARMDis.Operand s)
truncateValue op _v =
  case op of
    _ -> L.error "truncateValue for A32 not yet implemented"

type instance A.Opcode   A32 = ARMDis.Opcode
type instance A.Operand  A32 = ARMDis.Operand
type instance A.Location A32 = Location A32

instance A.IsOpcode  ARMDis.Opcode
instance A.IsOperand ARMDis.Operand

type instance A.OperandType A32 "Addr_offset_none" = BaseBVType 32
type instance A.OperandType A32 "Addrmode3" = BaseBVType 14
type instance A.OperandType A32 "Addrmode3_pre" = BaseBVType 14
type instance A.OperandType A32 "Addrmode5" = BaseBVType 13
type instance A.OperandType A32 "Addrmode5_pre" = BaseBVType 13
type instance A.OperandType A32 "Addrmode_imm12" = BaseBVType 32
type instance A.OperandType A32 "Addrmode_imm12_pre" = BaseBVType 32
type instance A.OperandType A32 "Adrlabel" = BaseBVType 14
type instance A.OperandType A32 "Am2offset_imm" = BaseBVType 32
type instance A.OperandType A32 "Am2offset_reg" = BaseBVType 12
type instance A.OperandType A32 "Am3offset" = BaseBVType 10
type instance A.OperandType A32 "Arm_bl_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType A32 "Arm_blx_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType A32 "Arm_br_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType A32 "Bf_inv_mask_imm" = BaseBVType 16
type instance A.OperandType A32 "C_imm" = BaseBVType 8
type instance A.OperandType A32 "Cc_out" = BaseBVType 1
type instance A.OperandType A32 "Coproc_option_imm" = BaseBVType 8
type instance A.OperandType A32 "Dpr" = BaseBVType 64
type instance A.OperandType A32 "GPR" = BaseBVType 32
type instance A.OperandType A32 "GPRPairOp" = BaseBVType 64
type instance A.OperandType A32 "GPRnopc" = BaseBVType 32
type instance A.OperandType A32 "GPRwithAPSR" = BaseBVType 32
type instance A.OperandType A32 "Iflags_op" = BaseBVType 8
type instance A.OperandType A32 "Imm0_1" = BaseBVType 1
type instance A.OperandType A32 "Imm0_15" = BaseBVType 4
type instance A.OperandType A32 "Imm0_239" = BaseBVType 8
type instance A.OperandType A32 "Imm0_31" = BaseBVType 5
type instance A.OperandType A32 "Imm0_65535" = BaseBVType 16
type instance A.OperandType A32 "Imm0_65535_expr" = BaseBVType 16
type instance A.OperandType A32 "Imm0_7" = BaseBVType 3
type instance A.OperandType A32 "Imm1_16" = BaseBVType 8
type instance A.OperandType A32 "Imm1_32" = BaseBVType 5
type instance A.OperandType A32 "Imm24b" = BaseBVType 32
type instance A.OperandType A32 "Imod_op" = BaseBVType 8
type instance A.OperandType A32 "Instsyncb_opt" = BaseBVType 4
type instance A.OperandType A32 "Ldst_so_reg" = BaseBVType 32
type instance A.OperandType A32 "Memb_opt" = BaseBVType 4
type instance A.OperandType A32 "Mod_imm" = BaseBVType 32
type instance A.OperandType A32 "Msr_mask" = BaseBVType 5
type instance A.OperandType A32 "P_imm" = BaseBVType 8
type instance A.OperandType A32 "Pkh_asr_amt" = BaseBVType 8
type instance A.OperandType A32 "Pkh_lsl_amt" = BaseBVType 8
type instance A.OperandType A32 "Postidx_imm8" = BaseBVType 10
type instance A.OperandType A32 "Postidx_imm8s4" = BaseBVType 9
type instance A.OperandType A32 "Postidx_reg" = BaseBVType 5
type instance A.OperandType A32 "Pred" = BaseBVType 4
type instance A.OperandType A32 "Qpr" = BaseBVType 128
type instance A.OperandType A32 "Qqpr" = BaseBVType 256
type instance A.OperandType A32 "Reglist" = BaseBVType 16
type instance A.OperandType A32 "Rot_imm" = BaseBVType 8
type instance A.OperandType A32 "Setend_op" = BaseBVType 1
type instance A.OperandType A32 "Shift_imm" = BaseBVType 6
type instance A.OperandType A32 "Shift_so_reg_imm" = BaseBVType 16
type instance A.OperandType A32 "Shift_so_reg_reg" = BaseBVType 16
type instance A.OperandType A32 "So_reg_imm" = BaseBVType 32
type instance A.OperandType A32 "So_reg_reg" = BaseBVType 32
type instance A.OperandType A32 "TcGPR" = BaseBVType 32
type instance A.OperandType A32 "Unpredictable" = BaseBVType 32

instance A.IsOperandTypeRepr A32 where
    type OperandTypeRepr A32 = ARMDis.OperandRepr
    operandTypeReprSymbol _ = ARMDis.operandReprString

operandValue :: forall sym s.
                (S.IsSymExprBuilder sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location A32 tp -> IO (S.SymExpr sym tp))
             -> ARMDis.Operand s
             -> IO (A.TaggedExpr A32 sym s)
operandValue sym locLookup op = TaggedExpr <$> opVa op
    where
        opVa :: ARMDis.Operand s -> IO (A.AllocatedOperand A32 sym s)
        opVa (ARMDis.Addr_offset_none gpr) =
          let loc = LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
          in A.LocationOperand loc <$> locLookup loc
        opVa (ARMDis.Addrmode_imm12 v) = do
          let loc = LocGPR (fromIntegral (W.unW (ARMOperands.unGPR (ARMOperands.addrModeImm12Register v))))
          reg <- locLookup loc
          imm <- S.bvLit sym knownNat (W.unW (ARMOperands.addrModeImm12Immediate v))
          add <- S.bvLit sym knownNat (W.unW (ARMOperands.addrModeImm12Add v))
          let am = AOC.OCAddrmodeImm12 { AOC.addrmodeImm12Reg = loc
                                       , AOC.addrmodeImm12RegExpr = reg
                                       , AOC.addrmodeImm12OffsetExpr = imm
                                       , AOC.addrmodeImm12AddExpr = add
                                       }
          return (A.CompoundOperand am)
        opVa (ARMDis.Addrmode_imm12_pre v) = do
          let loc = LocGPR (fromIntegral (W.unW (ARMOperands.unGPR (ARMOperands.addrModeImm12Register v))))
          reg <- locLookup loc
          imm <- S.bvLit sym knownNat (W.unW (ARMOperands.addrModeImm12Immediate v))
          add <- S.bvLit sym knownNat (W.unW (ARMOperands.addrModeImm12Add v))
          let am = AOC.OCAddrmodeImm12 { AOC.addrmodeImm12Reg = loc
                                       , AOC.addrmodeImm12RegExpr = reg
                                       , AOC.addrmodeImm12OffsetExpr = imm
                                       , AOC.addrmodeImm12AddExpr = add
                                       }
          return (A.CompoundOperand am)
        opVa (ARMDis.Am2offset_imm v) = do
          imm <- S.bvLit sym knownNat (W.unW (ARMOperands.am2OffsetImmImmediate v))
          add <- S.bvLit sym knownNat (W.unW (ARMOperands.am2OffsetImmAdd v))
          let am = AOC.OCAm2OffsetImm { AOC.am2OffsetImmImmExpr = imm
                                      , AOC.am2OffsetImmAddExpr = add
                                      }
          return (A.CompoundOperand am)
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
        opVa (ARMDis.Ldst_so_reg v) = do
          let baseLoc = LocGPR (fromIntegral (W.unW (ARMOperands.unGPR (ARMOperands.ldstSoRegBaseRegister v))))
          let offsetLoc = LocGPR (fromIntegral (W.unW (ARMOperands.unGPR (ARMOperands.ldstSoRegOffsetRegister v))))
          base <- locLookup baseLoc
          offset <- locLookup offsetLoc
          add <- S.bvLit sym knownNat (W.unW (ARMOperands.ldstSoRegAdd v))
          imm <- S.bvLit sym knownNat (W.unW (ARMOperands.ldstSoRegImmediate v))
          shiftType <- S.bvLit sym knownNat (W.unW (ARMOperands.ldstSoRegShiftType v))
          let ldst = AOC.OCLdstSoReg { AOC.ldstSoRegBaseLoc = baseLoc
                                     , AOC.ldstSoRegBaseExpr = base
                                     , AOC.ldstSoRegOffsetLoc = offsetLoc
                                     , AOC.ldstSoRegOffsetExpr = offset
                                     , AOC.ldstSoRegAddExpr = add
                                     , AOC.ldstSoRegImmExpr = imm
                                     , AOC.ldstSoRegTypeExpr = shiftType
                                     }
          return (A.CompoundOperand ldst)
        opVa (ARMDis.Mod_imm v) = do
          imm <- S.bvLit sym knownNat (fromIntegral (W.unW (ARMOperands.modImmOrigImmediate v)))
          rot <- S.bvLit sym knownNat (fromIntegral (W.unW (ARMOperands.modImmOrigRotate v)))
          let mi = AOC.OCModImm { AOC.modImmImmExpr = imm
                                , AOC.modImmRotExpr = rot
                                }
          return (A.CompoundOperand mi)
        opVa (ARMDis.Pred bits4) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger (ARMOperands.predToBits bits4))
        opVa (ARMDis.Shift_so_reg_imm v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger v)
        opVa (ARMDis.So_reg_imm v) = do
          let loc = LocGPR (fromIntegral (W.unW (ARMOperands.unGPR (ARMOperands.soRegImmReg v))))
          base <- locLookup loc
          shiftType <- S.bvLit sym knownNat (fromIntegral (W.unW (ARMOperands.soRegImmShiftType v)))
          imm <- S.bvLit sym knownNat (fromIntegral (W.unW (ARMOperands.soRegImmImmediate v)))
          let sri = AOC.OCSoRegImm { AOC.soRegImmReg = loc
                                   , AOC.soRegImmRegExpr = base
                                   , AOC.soRegImmShiftTypeExpr = shiftType
                                   , AOC.soRegImmImmExpr = imm
                                   }
          return (A.CompoundOperand sri)
        opVa (ARMDis.So_reg_reg v) = do
          let reg1 = LocGPR (fromIntegral (W.unW (ARMOperands.unGPR (ARMOperands.soRegRegReg1 v))))
          let reg2 = LocGPR (fromIntegral (W.unW (ARMOperands.unGPR (ARMOperands.soRegRegReg2 v))))
          reg1e <- locLookup reg1
          reg2e <- locLookup reg2
          shiftType <- S.bvLit sym knownNat (fromIntegral (W.unW (ARMOperands.soRegRegShiftType v)))
          let srr = AOC.OCSoRegReg { AOC.soRegRegReg1 = reg1
                                   , AOC.soRegRegReg1Expr = reg1e
                                   , AOC.soRegRegReg2 = reg2
                                   , AOC.soRegRegReg2Expr = reg2e
                                   , AOC.soRegRegTypeExpr = shiftType
                                   }
          return (A.CompoundOperand srr)
        opVa (ARMDis.Unpredictable v) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger v)
        -- opV unhandled = error $ "operandValue not implemented for " <> show unhandled

operandToLocation :: ARMDis.Operand s -> Maybe (Location A32 (A.OperandType A32 s))
operandToLocation (ARMDis.GPR gpr) = Just $ LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
operandToLocation (ARMDis.Addr_offset_none gpr) = Just $ LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
operandToLocation (ARMDis.GPRnopc gpr) = Just $ LocGPR $ fromIntegral $ W.unW $ ARMOperands.unGPR gpr
operandToLocation _ = Nothing

instance A.IsLocation (Location A32) where
  isMemoryLocation LocMem1 = True
  isMemoryLocation LocMem2 = True
  isMemoryLocation _ = False

  isIP LocPC = True
  isIP _     = False

  readLocation = P.parseMaybe parseLocation

  locationType (LocGPR _) = knownRepr
  locationType (LocFPR _) = knownRepr
  locationType (LocGPRMask _) = knownRepr
  locationType LocPC = knownRepr
  locationType LocCPSR = knownRepr
  locationType LocMem1 = knownRepr
  locationType LocMem2 = knownRepr

  defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocFPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocGPRMask _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocPC = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCPSR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMem1 =
      S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMem2 =
      S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0

  nonMemLocations = concat
    [ map (Some . LocGPR) [0..numGPR-1]
    , map (Some . LocGPRMask) [0..numGPR-1]
    , [ Some LocPC
      , Some LocCPSR
      ]
    ]

  memLocation = [AL.toMemLoc  LocMem1, AL.toMemLoc LocMem2]

  registerizationLocations = []

parseLocation :: ARMComp.Parser (Some (Location A32))
parseLocation = do
  c <- P.lookAhead (P.anySingle)
  case c of
    'C' -> Some LocCPSR <$ P.string "CPSR"
    'M' -> (Some LocMem1 <$ P.string "Mem")
           <|> (Some LocMem2 <$ P.string "Mem2")
           <|> (parsePrefixedRegister (Some . LocGPRMask) "Mask_R")
    'P' -> Some LocPC <$ P.string "PC"
    'R' -> do
      parsePrefixedRegister (Some . LocGPR) "R"
    'S' -> do
      parsePrefixedRegister (Some . LocFPR) "S"
    _ -> do
      P.failure (Just $ P.Tokens $ (c:|[])) (Set.fromList $ [ P.Label $ fromList "Location" ])

parsePrefixedRegister :: (GPRIdent -> b) -> String -> ARMComp.Parser b
parsePrefixedRegister f prefix = do
  _ <- P.string prefix
  n <- P.decimal
  case n >= 0 && n <= (numGPR-1) of
    True -> return (f n)
    False -> P.failure (Just $ P.Tokens $ fromList $ show n)
                      (Set.fromList $ [ P.Label $ fromList $ "Register number 0-" <> show (numGPR-1) ])

-- ----------------------------------------------------------------------

type instance A.RegWidth A32 = 32

instance A.Architecture A32 where
    data TaggedExpr A32 sym s = TaggedExpr (A.AllocatedOperand A32 sym s)
    unTagged (TaggedExpr te) =
      case te of
        A.ValueOperand se -> Just se
        A.LocationOperand _ se -> Just se
        A.CompoundOperand {} -> Nothing
    taggedOperand (TaggedExpr e) = e
    allocateSymExprsForOperand _ = operandValue
    operandToLocation _ = operandToLocation
    uninterpretedFunctions = UF.uninterpretedFunctions
    readMemUF = A.uninterpFnName . UF.mkReadMemUF @A32
    writeMemUF = A.uninterpFnName . UF.mkWriteMemUF @A32
    locationFuncInterpretation _proxy = A.createSymbolicEntries locationFuncInterpretation
    shapeReprToTypeRepr _proxy = shapeReprType
    operandComponentsImmediate = AOC.operandComponentsImmediate
    -- FIXME: architecture endianness is configurable, not sure how to represent this
    archEndianForm _ = A.LittleEndian

-- | Deconstruct an argument list for the 'a32.is_r15' pseudo-operation and
-- interpret the arguments.
--
-- The expected argument list is a single 'S.BoundVarExpr' that corresponds to a
-- GPR.  If we find exactly that, we check to see if the register number is 15
-- (returning the True symbolic expression if it is).
--
-- Note that this doesn't need to be polymorphic across architectures, as Thumb
-- mode can't access r15 this way.
eval_isR15 :: forall t st fs sh u tp sym
            . (sym ~ WEB.ExprBuilder t st fs)
           => sym
           -> F.ParameterizedFormula (WEB.ExprBuilder t st fs) A32 sh
           -> SL.List (A.AllocatedOperand A32 sym) sh
           -> Ctx.Assignment (WEB.Expr t) u
           -> (forall tp' . Location A32 tp' -> IO (S.SymExpr (WEB.ExprBuilder t st fs) tp'))
           -> BaseTypeRepr tp
           -> IO (WEB.Expr t tp)
eval_isR15 sym pf operands ufArguments _locToExpr resultRepr = do
  let typedReturn :: forall tp' . WEB.Expr t tp' -> IO (WEB.Expr t tp)
      typedReturn e =
        case testEquality (S.exprType e) resultRepr of
          Just Refl -> return e
          Nothing -> error ("isR15 returns expressions of BaseBoolType, but the caller expected " ++ show resultRepr)
  case ufArguments of
    Ctx.Empty Ctx.:> WEB.BoundVarExpr gprArg ->
      case gprArg `FE.lookupVarInFormulaOperandList` pf of
        Nothing -> do
          case MapF.lookup (LocGPR 15) (F.pfLiteralVars pf) of
            Nothing -> typedReturn (S.falsePred sym)
            Just r15Var
              | Just Refl <- testEquality r15Var gprArg -> typedReturn (S.truePred sym)
              | otherwise -> typedReturn (S.falsePred sym)
        Just (Some idx) -> do
          case operands SL.!! idx of
            A.LocationOperand loc _
              | Just Refl <- testEquality loc (LocGPR 15) -> typedReturn (S.truePred sym)
              | otherwise -> typedReturn (S.falsePred sym)
            op -> error ("Non-location argument passed to isR15: " ++ show op)
    _ -> error "Unexpected argument list to isR15"

-- | An evaluator that cracks open a 'ARMOperands.Am2OffsetImm' operand value
-- and extracts the immediate value as a @BaseBVType 12@ (i.e., a 12 bit bitvector)
--
-- Note that this function can only be applied to instruction operands, as these
-- immediate types only appear as function operands.  This means that we only
-- need to look found the bound variable in the 'Ctx.Assignment' in the argument
-- list and can disregard the literals.
eval_am2offset_imm_imm :: A.Evaluator A32 t st fs
eval_am2offset_imm_imm =
  FE.evalBitvectorExtractor "am2offset_imm_imm" (knownNat @12) $ \ao nr' ->
    case ao of
      AOC.OCAm2OffsetImm { AOC.am2OffsetImmImmExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

-- | An evaluator that cracks open a 'ARMOperands.Am2OffsetImm' operand value and extracts
-- the "Add" field as a @BaseBVType 1@ (i.e., a 1 bit bitvector)
eval_am2offset_imm_add :: HasCallStack => A.Evaluator A32 t st fs
eval_am2offset_imm_add =
  FE.evalBitvectorExtractorWith bitToBool "am2offset_imm_add" (knownNat @1) $ \ao nr' ->
    case ao of
      AOC.OCAm2OffsetImm { AOC.am2OffsetImmAddExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_imm12_reg :: A.Evaluator A32 t st fs
eval_imm12_reg =
  FE.evalRegExtractor "imm12_reg" $ \case
    A.CompoundOperand (AOC.OCAddrmodeImm12 { AOC.addrmodeImm12Reg = loc }) -> Just (Some loc)
    _ -> Nothing

eval_imm12_off :: A.Evaluator A32 t st fs
eval_imm12_off =
  FE.evalBitvectorExtractor "imm12_off" (knownNat @12) $ \ao nr' ->
    case ao of
      AOC.OCAddrmodeImm12 { AOC.addrmodeImm12OffsetExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_imm12_add :: HasCallStack => A.Evaluator A32 t st fs
eval_imm12_add =
  FE.evalBitvectorExtractorWith bitToBool "imm12_add" (knownNat @1) $ \ao nr' ->
    case ao of
      AOC.OCAddrmodeImm12 { AOC.addrmodeImm12AddExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_ldst_so_reg_base_register :: A.Evaluator A32 t st fs
eval_ldst_so_reg_base_register =
  FE.evalRegExtractor "ldst_so_reg_base_register" $ \case
    A.CompoundOperand (AOC.OCLdstSoReg { AOC.ldstSoRegBaseLoc = loc }) -> Just (Some loc)
    _ -> Nothing

eval_ldst_so_reg_offset_register :: A.Evaluator A32 t st fs
eval_ldst_so_reg_offset_register =
  FE.evalRegExtractor "ldst_so_reg_offset_register" $ \case
    A.CompoundOperand (AOC.OCLdstSoReg { AOC.ldstSoRegOffsetLoc = loc }) -> Just (Some loc)
    _ -> Nothing

eval_ldst_so_reg_add :: HasCallStack => A.Evaluator A32 t st fs
eval_ldst_so_reg_add =
  FE.evalBitvectorExtractorWith bitToBool "ldst_so_reg_add" (knownNat @1) $ \ao nr' ->
    case ao of
      AOC.OCLdstSoReg { AOC.ldstSoRegAddExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_ldst_so_reg_imm :: A.Evaluator A32 t st fs
eval_ldst_so_reg_imm =
  FE.evalBitvectorExtractor "ldst_so_reg_imm" (knownNat @5) $ \ao nr' ->
    case ao of
      AOC.OCLdstSoReg { AOC.ldstSoRegImmExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_ldst_so_reg_st :: A.Evaluator A32 t st fs
eval_ldst_so_reg_st =
  FE.evalBitvectorExtractor "ldst_so_reg_shift_type" (knownNat @2) $ \ao nr' ->
    case ao of
      AOC.OCLdstSoReg { AOC.ldstSoRegTypeExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_modimm_imm :: A.Evaluator A32 t st fs
eval_modimm_imm =
  FE.evalBitvectorExtractor "modimm_imm" (knownNat @8) $ \ao nr' ->
    case ao of
      AOC.OCModImm { AOC.modImmImmExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_modimm_rot :: A.Evaluator A32 t st fs
eval_modimm_rot =
  FE.evalBitvectorExtractor "modimm_rot" (knownNat @4) $ \ao nr' ->
    case ao of
      AOC.OCModImm { AOC.modImmRotExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_soregimm_type :: A.Evaluator A32 t st fs
eval_soregimm_type =
  FE.evalBitvectorExtractor "soregimm_type" (knownNat @2) $ \ao nr' ->
    case ao of
      AOC.OCSoRegImm { AOC.soRegImmShiftTypeExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_soregimm_imm :: A.Evaluator A32 t st fs
eval_soregimm_imm =
  FE.evalBitvectorExtractor "soregimm_imm" (knownNat @5) $ \ao nr' ->
    case ao of
      AOC.OCSoRegImm { AOC.soRegImmImmExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_soregimm_reg :: A.Evaluator A32 t st fs
eval_soregimm_reg =
  FE.evalRegExtractor "soregimm_reg" $ \case
    A.CompoundOperand (AOC.OCSoRegImm { AOC.soRegImmReg = loc }) -> Just (Some loc)
    _ -> Nothing

eval_soregreg_type :: A.Evaluator A32 t st fs
eval_soregreg_type =
  FE.evalBitvectorExtractor "soregreg_type" (knownNat @2) $ \ao nr' ->
    case ao of
      AOC.OCSoRegReg { AOC.soRegRegTypeExpr = e }
        | Just Refl <- testEquality (S.exprType e) (BaseBVRepr nr') -> Just e
      _ -> Nothing

eval_soregreg_reg1 :: A.Evaluator A32 t st fs
eval_soregreg_reg1 =
  FE.evalRegExtractor "soregreg_reg1" $ \case
    A.CompoundOperand (AOC.OCSoRegReg { AOC.soRegRegReg1 = loc }) -> Just (Some loc)
    _ -> Nothing

eval_soregreg_reg2 :: A.Evaluator A32 t st fs
eval_soregreg_reg2 =
  FE.evalRegExtractor "soregreg_reg2" $ \case
    A.CompoundOperand (AOC.OCSoRegReg { AOC.soRegRegReg2 = loc }) -> Just (Some loc)
    _ -> Nothing

bitToBool :: (S.IsExprBuilder sym, 1 <= w)
          => sym
          -> S.SymExpr sym (BaseBVType w)
          -> IO (S.SymExpr sym BaseBoolType)
bitToBool sym = S.testBitBV sym 0

noLocation :: SL.List (A.AllocatedOperand arch sym) sh
           -> F.WrappedOperand arch sh s
           -> BaseTypeRepr tp
           -> Maybe (Location arch tp)
noLocation _ _ _ = Nothing

locationFuncInterpretation :: [(String, A.FunctionInterpretation t st fs A32)]
locationFuncInterpretation =
    [ ("arm.is_r15", A.FunctionInterpretation
                       { A.locationInterp = F.LocationFuncInterp noLocation
                       , A.exprInterpName = 'interpIsR15
                       , A.exprInterp = A.Evaluator eval_isR15
                       })

    , ("a32.am2offset_imm_imm", A.FunctionInterpretation
                                  { A.locationInterp = F.LocationFuncInterp noLocation
                                  , A.exprInterpName = 'interpAm2offsetimmImmExtractor
                                  , A.exprInterp = eval_am2offset_imm_imm
                                  })
    , ("a32.am2offset_imm_add", A.FunctionInterpretation
                                  { A.locationInterp = F.LocationFuncInterp noLocation
                                  , A.exprInterpName = 'interpAm2offsetimmAddExtractor
                                  , A.exprInterp = eval_am2offset_imm_add
                                  })

    , ("a32.imm12_reg", A.FunctionInterpretation
                          { A.locationInterp = F.LocationFuncInterp interpImm12Reg
                          , A.exprInterpName = 'interpImm12RegExtractor
                          , A.exprInterp = eval_imm12_reg
                          })
    , ("a32.imm12_off", A.FunctionInterpretation
                          { A.locationInterp = F.LocationFuncInterp noLocation
                          , A.exprInterpName = 'interpImm12OffsetExtractor
                          , A.exprInterp = eval_imm12_off
                          })
    , ("a32.imm12_add", A.FunctionInterpretation
                          { A.locationInterp = F.LocationFuncInterp noLocation
                          , A.exprInterpName = 'interpImm12AddFlgExtractor
                          , A.exprInterp = eval_imm12_add
                          })

    , ("a32.ldst_so_reg_base_register", A.FunctionInterpretation
                                          { A.locationInterp = F.LocationFuncInterp interpLdstsoregBaseReg
                                          , A.exprInterpName = 'interpLdstsoregBaseRegExtractor
                                          , A.exprInterp = eval_ldst_so_reg_base_register
                                          })
    , ("a32.ldst_so_reg_offset_register", A.FunctionInterpretation
                                            { A.locationInterp = F.LocationFuncInterp interpLdstsoregOffReg
                                            , A.exprInterpName = 'interpLdstsoregOffRegExtractor
                                            , A.exprInterp = eval_ldst_so_reg_offset_register
                                            })
    , ("a32.ldst_so_reg_add", A.FunctionInterpretation
                                { A.locationInterp = F.LocationFuncInterp noLocation
                                , A.exprInterpName = 'interpLdstsoregAddExtractor
                                , A.exprInterp = eval_ldst_so_reg_add
                                })
    , ("a32.ldst_so_reg_immediate", A.FunctionInterpretation
                                      { A.locationInterp = F.LocationFuncInterp noLocation
                                      , A.exprInterpName = 'interpLdstsoregImmExtractor
                                      , A.exprInterp = eval_ldst_so_reg_imm
                                      })
    , ("a32.ldst_so_reg_shift_type", A.FunctionInterpretation
                                       { A.locationInterp = F.LocationFuncInterp noLocation
                                       , A.exprInterpName = 'interpLdstsoregTypeExtractor
                                       , A.exprInterp = eval_ldst_so_reg_st
                                       })

    , ("a32.modimm_imm", A.FunctionInterpretation
                           { A.locationInterp = F.LocationFuncInterp noLocation
                           , A.exprInterpName = 'interpModimmImmExtractor
                           , A.exprInterp = eval_modimm_imm
                           })
    , ("a32.modimm_rot", A.FunctionInterpretation
                           { A.locationInterp = F.LocationFuncInterp noLocation
                           , A.exprInterpName = 'interpModimmRotExtractor
                           , A.exprInterp = eval_modimm_rot
                           })

    , ("a32.soregimm_type", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp noLocation
                              , A.exprInterpName = 'interpSoregimmTypeExtractor
                              , A.exprInterp = eval_soregimm_type
                              })
    , ("a32.soregimm_imm", A.FunctionInterpretation
                             { A.locationInterp = F.LocationFuncInterp noLocation
                             , A.exprInterpName = 'interpSoregimmImmExtractor
                             , A.exprInterp = eval_soregimm_imm
                             })
    , ("a32.soregimm_reg", A.FunctionInterpretation
                             { A.locationInterp = F.LocationFuncInterp interpSoregimmReg
                             , A.exprInterpName = 'interpSoregimmRegExtractor
                             , A.exprInterp = eval_soregimm_reg
                             })

    , ("a32.soregreg_type", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp noLocation
                              , A.exprInterpName = 'interpSoregregTypeExtractor
                              , A.exprInterp = eval_soregreg_type
                              })
    , ("a32.soregreg_reg1", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp interpSoregregReg1
                              , A.exprInterpName = 'interpSoregregReg1Extractor
                              , A.exprInterp = eval_soregreg_reg1
                              })
    , ("a32.soregreg_reg2", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp interpSoregregReg2
                              , A.exprInterpName = 'interpSoregregReg2Extractor
                              , A.exprInterp = eval_soregreg_reg2
                              })

    ]

shapeReprType :: forall tp . ARMDis.OperandRepr tp -> BaseTypeRepr (A.OperandType A32 tp)
shapeReprType orep =
    case orep of
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
        _ -> error $ "Unknown A32 OperandRepr: " <> show (A.operandTypeReprSymbol (Proxy @A32) orep)

-- ----------------------------------------------------------------------

data Signed = Signed | Unsigned deriving (Eq, Show)

type instance A.OperandComponents A32 sym = AOC.OperandComponents A32 sym

instance T.TemplatableOperand A32 where
  opTemplates = a32template

a32template :: ARMDis.OperandRepr s -> [T.TemplatedOperand A32 s]
a32template a32sr =
    case a32sr of
      ARMDis.Addrmode_imm12Repr ->
          mkTemplate <$> [0..numGPR-1]
              where mkTemplate gprNum = T.TemplatedOperand Nothing
                                        (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
                                            :: T.TemplatedOperand A32 "Addrmode_imm12"
                        where mkTemplate' :: forall sym
                                           . (S.IsSymExprBuilder sym)
                                          => sym
                                          -> (forall tp . Location A32 tp -> IO (S.SymExpr sym tp))
                                          -> IO ( A.AllocatedOperand A32 sym "Addrmode_imm12",
                                                  T.RecoverOperandFn sym (A.Operand A32 "Addrmode_imm12")
                                                )
                              mkTemplate' sym locLookup = do
                                let gprN = ARMOperands.gpr $ fromIntegral gprNum
                                let loc = LocGPR gprNum
                                base <- locLookup loc
                                offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_off") knownRepr
                                addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_add") knownRepr
                                let recover :: (forall tp . S.SymExpr sym tp -> IO (WEB.GroundValue tp)) -> IO (A.Operand A32 "Addrmode_imm12")
                                    recover evalFn = do
                                      offsetVal <- fromInteger <$> evalFn offset
                                      addflagVal <- fromInteger <$> evalFn addflag
                                      return $ ARMDis.Addrmode_imm12 $
                                             ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                                return ( A.CompoundOperand (AOC.OCAddrmodeImm12 loc base offset addflag)
                                       , T.RecoverOperandFn recover
                                       )
      ARMDis.Addrmode_imm12_preRepr ->
          mkTemplate <$> [0..numGPR-1]
            where mkTemplate gprNum = T.TemplatedOperand Nothing
                                      (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
                                          :: T.TemplatedOperand A32 "Addrmode_imm12_pre"
                    where mkTemplate' :: forall sym
                                       . (S.IsSymExprBuilder sym)
                                      => sym
                                      -> (forall tp . Location A32 tp -> IO (S.SymExpr sym tp))
                                      -> IO ( A.AllocatedOperand A32 sym "Addrmode_imm12_pre",
                                              T.RecoverOperandFn sym (A.Operand A32 "Addrmode_imm12_pre")
                                            )
                          mkTemplate' sym locLookup = do
                            let gprN = ARMOperands.gpr $ fromIntegral gprNum
                            let loc = LocGPR gprNum
                            base <- locLookup loc
                            offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_off") knownRepr
                            addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_add") knownRepr
                            let recover :: (forall tp . S.SymExpr sym tp -> IO (WEB.GroundValue tp)) -> IO (A.Operand A32 "Addrmode_imm12_pre")
                                recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  addflagVal <- fromInteger <$> evalFn addflag
                                  return $ ARMDis.Addrmode_imm12_pre $
                                         ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                            return ( A.CompoundOperand (AOC.OCAddrmodeImm12 loc base offset addflag)
                                   , T.RecoverOperandFn recover
                                   )
      ARMDis.Arm_bl_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_blx_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_br_targetRepr -> error "opTemplate ARM_br_targetRepr TBD"
      ARMDis.Cc_outRepr -> error "opTemplate ARM_Cc_outRepr TBD"
      ARMDis.GPRRepr -> concreteTemplatedOperand (ARMDis.GPR . ARMOperands.gpr . fromIntegral) LocGPR <$> [0..numGPR-1]
      ARMDis.Mod_immRepr -> error "opTemplate ARM_Mod_immRepr TBD"
      ARMDis.PredRepr -> [symbolicTemplatedOperand (Proxy @4) Unsigned "Pred"
                          (ARMDis.Pred . ARMDis.mkPred . fromInteger)]
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

----------------------------------------------------------------------
-- Concrete state functionality

mkRandomState :: DA.Gen -> IO ConcreteState
mkRandomState gen = St.execStateT randomize MapF.empty
  where
    randomize = do
      mapM_ addRandomBV gprList

      mapM_ addZero gprMaskList
      mapM_ addZero fprList

      addZeroMem LocMem1
      addZeroMem LocMem2

      -- We don't want to set these at all
      addZero LocCPSR
      -- PC
      addZero $ LocGPR 15

    addZero :: Location A32 (BaseBVType 32) -> St.StateT ConcreteState IO ()
    addZero loc = St.modify' $ MapF.insert loc (V.ValueBV (W.w 0))

    addZeroMem :: Location A32 (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)) -> St.StateT ConcreteState IO ()
    addZeroMem loc = St.modify' $ MapF.insert loc (V.ValueMem $ B.replicate 32 0)

    addRandomBV :: Location A32 (BaseBVType 32) -> St.StateT ConcreteState IO ()
    addRandomBV loc = do
      bv <- V.ValueBV <$> liftIO (DA.arbitrary gen)
      St.modify' $ MapF.insert loc bv

-- | States that include (pairs of) registers with interesting bit patterns.
-- For each pair of registers, combinations of interesting bit patterns are
-- chosen.  The other registers all have zeros.
interestingStates :: [ConcreteState]
interestingStates = []

zeroState :: ConcreteState
zeroState = St.execState addZeros MapF.empty
  where
    addZero :: Location A32 (BaseBVType 32) -> St.State ConcreteState ()
    addZero loc = St.modify' $ MapF.insert loc (V.ValueBV (W.w 0))

    addZeros = do
      mapM_ addZero gprList
      mapM_ addZero fprList

deserializeState :: B.ByteString -> Maybe ConcreteState
deserializeState bs =
    case G.pushChunk (G.runGetIncremental getMachineState) bs of
        G.Done _ _ ms -> Just ms
        G.Fail {} -> Nothing
        G.Partial {} -> Nothing

getMachineState :: G.Get ConcreteState
getMachineState = do
  gprs <- forM gprList $ \loc ->
      (MapF.Pair loc . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  gprs_mask <- forM gprMaskList $ \loc ->
      (MapF.Pair loc . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  fprs <- forM fprList $ \loc ->
      (MapF.Pair loc . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  cpsr <- (MapF.Pair LocCPSR . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  m1 <- (MapF.Pair LocMem1 . V.ValueMem . B.pack) <$> replicateM 32 G.getWord8
  m2 <- (MapF.Pair LocMem2 . V.ValueMem . B.pack) <$> replicateM 32 G.getWord8

  return $ MapF.fromList $ concat [ gprs
                                  , gprs_mask
                                  , fprs
                                  , [m1, m2, cpsr]
                                  ]

-- | Convert a machine state to the wire protocol.
--
-- Note that we perform a byte swap to put data in big endian so that the
-- machine on the receiving end doesn't need to do anything special besides map
-- the data.
serializeState :: ConcreteState -> B.ByteString
serializeState s = LB.toStrict (B.toLazyByteString b)
  where
    b = mconcat [ mconcat (map (serializeSymVal (B.word32LE . fromInteger)) (extractLocs s gprList))
                , mconcat (map (serializeSymVal (B.word32LE . fromInteger)) (extractLocs s gprMaskList))
                , mconcat (map (serializeSymVal (B.word32LE . fromInteger)) (extractLocs s fprList))
                -- CPSR, unused
                , serializeSymVal (B.word32LE . fromInteger) (extractLoc s LocCPSR)
                -- , serializeMem (extractLoc s LocMem1)
                , serializeMem (V.ValueMem $ B.replicate 32 0)
                -- , serializeMem (extractLoc s LocMem2)
                , serializeMem (V.ValueMem $ B.replicate 32 0)
                ]

serializeSymVal :: (KnownNat n) => (Integer -> B.Builder) -> V.Value (BaseBVType n) -> B.Builder
serializeSymVal toBuilder sv =
  case sv of
    V.ValueBV (W.unW -> w) -> toBuilder (toInteger w)

serializeMem :: V.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)) -> B.Builder
serializeMem val =
  case val of
    V.ValueMem bs -> B.byteString bs

extractLoc :: ConcreteState
           -> Location A32 tp
           -> V.Value tp
extractLoc s l =
    case MapF.lookup l s of
        Nothing -> error $ "extractLoc: not found: " <> show l <> ", " <> show s
        Just v -> v

extractLocs :: ConcreteState
            -> [Location A32 tp]
            -> [V.Value tp]
extractLocs s locs = map (extractLoc s) locs

gprList :: [Location A32 (BaseBVType 32)]
gprList = fmap LocGPR [0..15]

gprMaskList :: [Location A32 (BaseBVType 32)]
gprMaskList = fmap LocGPRMask [0..15]

fprList :: [Location A32 (BaseBVType 32)]
fprList = fmap LocFPR [0..31]
