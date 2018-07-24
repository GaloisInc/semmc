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
import           Data.Word ( Word8 )
import qualified Data.Word.Indexed as W
import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMOperands
import qualified Dismantle.Instruction as D
import           GHC.TypeLits
import qualified GHC.Err.Located as L
import qualified Lang.Crucible.Backend as SB
import qualified SemMC.Architecture as A
import           SemMC.Architecture.ARM.BaseSemantics.Registers ( numGPR )
import qualified SemMC.Architecture.ARM.Components as ARMComp
import           SemMC.Architecture.ARM.Eval
import qualified SemMC.Architecture.ARM.UF as UF
import           SemMC.Architecture.ARM.Location ( ArchRegWidth )
import           SemMC.Architecture.A32.Location
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
import qualified What4.Expr.Builder as WEB
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
truncateValue op v =
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
                (SB.IsSymInterface sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location A32 tp -> IO (S.SymExpr sym tp))
             -> ARMDis.Operand s
             -> IO (A.TaggedExpr A32 sym s)
operandValue sym locLookup op = TaggedExpr <$> opVa op
    where
        opVa :: ARMDis.Operand s -> IO (S.SymExpr sym (A.OperandType A32 s))
        opVa (ARMDis.Addr_offset_none gpr) = locLookup (LocGPR $ ARMOperands.unGPR gpr)
        opVa (ARMDis.Addrmode_imm12 v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.addrModeImm12ToBits v
        opVa (ARMDis.Addrmode_imm12_pre v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.addrModeImm12ToBits v
        opVa (ARMDis.Am2offset_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.am2OffsetImmToBits v
        opVa (ARMDis.Arm_bl_target v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.branchTargetToBits v
        opVa (ARMDis.Arm_blx_target v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.branchExecuteTargetToBits v
        opVa (ARMDis.Arm_br_target v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.branchTargetToBits v
        opVa (ARMDis.Cc_out v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.sBitToBits v -- KWQ: Bool? size?
        opVa (ARMDis.GPR gpr) = locLookup (LocGPR $ ARMOperands.unGPR gpr)
        opVa (ARMDis.GPRnopc gpr) = locLookup (LocGPR $ ARMOperands.unGPR gpr)
        opVa (ARMDis.Ldst_so_reg v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.ldstSoRegToBits v
        opVa (ARMDis.Mod_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.modImmToBits v
        opVa (ARMDis.Pred bits4) = S.bvLit sym knownNat $ toInteger $ ARMOperands.predToBits bits4
        opVa (ARMDis.Shift_so_reg_imm v) = S.bvLit sym knownNat $ toInteger v
        opVa (ARMDis.So_reg_imm v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.soRegImmToBits v
        opVa (ARMDis.So_reg_reg v) = S.bvLit sym knownNat $ toInteger $ ARMOperands.soRegRegToBits v
        opVa (ARMDis.Unpredictable v) = S.bvLit sym knownNat $ toInteger v
        -- opV unhandled = error $ "operandValue not implemented for " <> show unhandled

operandToLocation :: ARMDis.Operand s -> Maybe (Location A32 (A.OperandType A32 s))
operandToLocation (ARMDis.GPR gpr) = Just $ LocGPR $ ARMOperands.unGPR gpr
operandToLocation _ = Nothing

instance A.IsLocation (Location A32) where
  isMemoryLocation LocMem1 = True
  isMemoryLocation LocMem2 = True
  isMemoryLocation _ = False

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

  allLocations = concat
    [ map (Some . LocGPR) [0..numGPR-1]
    , map (Some . LocGPRMask) [0..numGPR-1]
    , [ Some LocPC
      , Some LocCPSR
      , Some LocMem1
      , Some LocMem2
      ]
    ]

  registerizationLocations = []

parseLocation :: ARMComp.Parser (Some (Location A32))
parseLocation = do
  c <- P.lookAhead (P.anyChar)
  case c of
    'C' -> Some LocCPSR <$ P.string "CPSR"
    'M' -> (Some LocMem1 <$ P.string "Mem")
           -- <|> (Some LocMem2 <$ P.string "Mem2")
    'P' -> Some LocPC <$ P.string "PC"
    'R' -> do
      parsePrefixedRegister (Some . LocGPR) 'R'
    'S' -> do
      parsePrefixedRegister (Some . LocFPR) 'S'
    _ -> do
      P.failure (Just $ P.Tokens $ (c:|[])) (Set.fromList $ [ P.Label $ fromList "Location" ])

parsePrefixedRegister :: (Word8 -> b) -> Char -> ARMComp.Parser b
parsePrefixedRegister f c = do
  _ <- P.char c
  n <- P.decimal
  case n >= 0 && n <= (numGPR-1) of
    True -> return (f n)
    False -> P.failure (Just $ P.Tokens $ fromList $ show n)
                      (Set.fromList $ [ P.Label $ fromList $ "Register number 0-" <> show (numGPR-1) ])

-- ----------------------------------------------------------------------

type instance ArchRegWidth A32 = 32

instance A.Architecture A32 where
    data TaggedExpr A32 sym s = TaggedExpr (S.SymExpr sym (A.OperandType A32 s))
    unTagged (TaggedExpr e) = e
    operandValue _ = operandValue
    operandToLocation _ = operandToLocation
    uninterpretedFunctions = UF.uninterpretedFunctions
    locationFuncInterpretation _proxy = A.createSymbolicEntries locationFuncInterpretation
    shapeReprToTypeRepr _proxy = shapeReprType

-- | Deconstruct an argument list for the 'a32.is_r15' pseudo-operation and
-- interpret the arguments.
--
-- The expected argument list is a single 'S.BoundVarExpr' that corresponds to a
-- GPR.  If we find exactly that, we check to see if the register number is 15
-- (returning the True symbolic expression if it is).
--
-- Note that this doesn't need to be polymorphic across architectures, as Thumb
-- mode can't access r15 this way.
eval_isR15 :: forall t st sh u tp
            . WEB.ExprBuilder t st
           -> F.ParameterizedFormula (WEB.ExprBuilder t st) A32 sh
           -> SL.List (A.Operand A32) sh
           -> Ctx.Assignment (WEB.Expr t) u
           -> BaseTypeRepr tp
           -> IO (WEB.Expr t tp, MapF.MapF (A.Location A32) (S.BoundVar (WEB.ExprBuilder t st)))
eval_isR15 sym pf operands ufArguments resultRepr =
  case ufArguments of
    Ctx.Empty Ctx.:> WEB.BoundVarExpr gprArg ->
      case gprArg `FE.lookupVarInFormulaOperandList` pf of
        Nothing -> do
          p <- case MapF.lookup (LocGPR 15) (F.pfLiteralVars pf) of
            Nothing -> return (S.falsePred sym)
            Just r15Var
              | Just Refl <- testEquality r15Var gprArg -> return (S.truePred sym)
              | otherwise -> return (S.falsePred sym)
          case testEquality (S.exprType p) resultRepr of
            Just Refl -> return (p, MapF.empty)
            Nothing -> error ("isR15 returns expressions of BaseBoolType, but the caller expected " ++ show resultRepr)
        Just (Some idx) -> do
          let ARMDis.GPR rnum = operands SL.!! idx
          let p = if ARMOperands.unGPR rnum == 15 then S.truePred sym else S.falsePred sym
          case testEquality (S.exprType p) resultRepr of
            Just Refl -> return (p, MapF.empty)
            Nothing -> error ("isR15 returns expressions of BaseBoolType, but the caller expected " ++ show resultRepr)

-- | An evaluator that cracks open a 'ARMOperands.Am2OffsetImm' operand value
-- and extracts the immediate value as a @BaseBVType 12@ (i.e., a 12 bit bitvector)
--
-- Note that this function can only be applied to instruction operands, as these
-- immediate types only appear as function operands.  This means that we only
-- need to look found the bound variable in the 'Ctx.Assignment' in the argument
-- list and can disregard the literals.
eval_am2offset_imm_imm :: forall t st sh u tp
                        . WEB.ExprBuilder t st
                       -> F.ParameterizedFormula (WEB.ExprBuilder t st) A32 sh
                       -> SL.List (A.Operand A32) sh
                       -> Ctx.Assignment (WEB.Expr t) u
                       -> BaseTypeRepr tp
                       -> IO (WEB.Expr t tp, MapF.MapF (A.Location A32) (S.BoundVar (WEB.ExprBuilder t st)))
eval_am2offset_imm_imm sym pf operands ufArguments resultRepr =
  case ufArguments of
    Ctx.Empty Ctx.:> WEB.BoundVarExpr ufArg ->
      case ufArg `FE.lookupVarInFormulaOperandList` pf of
        Nothing -> error "Argument to am2offset_imm_imm is not a formula parameter"
        Just (Some idx) -> do
          case operands SL.!! idx of
            ARMDis.Am2offset_imm oimm -> do
              bv <- S.bvLit sym (knownNat @12) (fromIntegral (ARMOperands.am2OffsetImmImmediate oimm))
              case testEquality (S.exprType bv) resultRepr of
                Just Refl -> return (bv, MapF.empty)
                Nothing -> error ("am2offset_imm_imm returns a BaseBVType 12, but the caller expected " ++ show resultRepr)

-- | An evaluator that cracks open a 'ARMOperands.Am2OffsetImm' operand value and extracts
-- the "Add" field as a @BaseBVType 1@ (i.e., a 1 bit bitvector)
eval_am2offset_imm_add :: forall t st sh u tp
                        . WEB.ExprBuilder t st
                       -> F.ParameterizedFormula (WEB.ExprBuilder t st) A32 sh
                       -> SL.List (A.Operand A32) sh
                       -> Ctx.Assignment (WEB.Expr t) u
                       -> BaseTypeRepr tp
                       -> IO (WEB.Expr t tp, MapF.MapF (A.Location A32) (S.BoundVar (WEB.ExprBuilder t st)))
eval_am2offset_imm_add sym pf operands ufArguments resultRepr =
  case ufArguments of
    Ctx.Empty Ctx.:> WEB.BoundVarExpr ufArg ->
      case ufArg `FE.lookupVarInFormulaOperandList` pf of
        Nothing -> error "Argument to am2offset_imm_add is not a formula parameter"
        Just (Some idx) -> do
          case operands SL.!! idx of
            ARMDis.Am2offset_imm oimm -> do
              bv <- S.bvLit sym (knownNat @1) (fromIntegral (ARMOperands.am2OffsetImmAdd oimm))
              case testEquality (S.exprType bv) resultRepr of
                Just Refl -> return (bv, MapF.empty)
                Nothing -> error ("am2offset_imm_add returns a BaseBVType 1, but the caller expected " ++ show resultRepr)

eval_imm12_reg :: forall t st sh u tp
                . WEB.ExprBuilder t st
               -> F.ParameterizedFormula (WEB.ExprBuilder t st) A32 sh
               -> SL.List (A.Operand A32) sh
               -> Ctx.Assignment (WEB.Expr t) u
               -> BaseTypeRepr tp
               -> IO (WEB.Expr t tp, MapF.MapF (A.Location A32) (S.BoundVar (WEB.ExprBuilder t st)))
eval_imm12_reg sym pf operands ufArguments resultRepr =
  case ufArguments of
    Ctx.Empty Ctx.:> WEB.BoundVarExpr  ufArg ->
      case ufArg `FE.lookupVarInFormulaOperandList` pf of
        Nothing -> error "Argument to imm12_reg is not a formula parameter"
        Just (Some idx) -> do
          case operands SL.!! idx of
            ARMDis.Addrmode_imm12 ami12 -> do
              let reg = ARMOperands.addrModeImm12Register ami12
              let regNum = ARMOperands.unGPR reg
              FE.exprForRegister sym pf operands (testRegisterEquality reg) (LocGPR regNum) resultRepr

testRegisterEquality :: ARMOperands.GPR -> ARMDis.Operand tp -> Bool
testRegisterEquality regNum op =
  case op of
    ARMDis.GPR gpr
      | gpr == regNum -> True
    _ -> False

noLocation :: SL.List (A.Operand arch) sh
           -> F.WrappedOperand arch sh s
           -> BaseTypeRepr tp
           -> Maybe (Location arch tp)
noLocation _ _ _ = Nothing

locationFuncInterpretation :: [(String, A.FunctionInterpretation t A32)]
locationFuncInterpretation =
    [ ("arm.is_r15", A.FunctionInterpretation
                       { A.locationInterp = F.LocationFuncInterp noLocation
                       , A.exprInterpName = 'interpIsR15
                       , A.exprInterp = FE.Evaluator eval_isR15
                       })

    , ("a32.am2offset_imm_imm", A.FunctionInterpretation
                                  { A.locationInterp = F.LocationFuncInterp noLocation
                                  , A.exprInterpName = 'interpAm2offsetimmImmExtractor
                                  , A.exprInterp = FE.Evaluator eval_am2offset_imm_imm
                                  })
    , ("a32.am2offset_imm_add", A.FunctionInterpretation
                                  { A.locationInterp = F.LocationFuncInterp noLocation
                                  , A.exprInterpName = 'interpAm2offsetimmAddExtractor
                                  , A.exprInterp = FE.Evaluator eval_am2offset_imm_add
                                  })

    , ("a32.imm12_reg", A.FunctionInterpretation
                          { A.locationInterp = F.LocationFuncInterp (interpImm12Reg Just LocGPR)
                          , A.exprInterpName = 'interpImm12RegExtractor
                          , A.exprInterp = FE.Evaluator eval_imm12_reg
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
                                          { A.locationInterp = F.LocationFuncInterp (interpLdstsoregBaseReg Just LocGPR)
                                          , A.exprInterpName = 'interpLdstsoregBaseRegExtractor
                                          })
    , ("a32.ldst_so_reg_offset_register", A.FunctionInterpretation
                                            { A.locationInterp = F.LocationFuncInterp (interpLdstsoregOffReg Just LocGPR)
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
                             { A.locationInterp = F.LocationFuncInterp (interpSoregimmReg Just LocGPR)
                             , A.exprInterpName = 'interpSoregimmRegExtractor })

    , ("a32.soregreg_type", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp noLocation
                              , A.exprInterpName = 'interpSoregregTypeExtractor
                              })
    , ("a32.soregreg_reg1", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp (interpSoregregReg1 Just LocGPR)
                              , A.exprInterpName = 'interpSoregregReg1Extractor })
    , ("a32.soregreg_reg2", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp (interpSoregregReg2 Just LocGPR)
                              , A.exprInterpName = 'interpSoregregReg2Extractor })

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
                        where mkTemplate' :: T.TemplatedOperandFn A32 "Addrmode_imm12"
                              mkTemplate' sym locLookup = do
                                let gprN = ARMOperands.gpr gprNum
                                base <- A.unTagged <$> A.operandValue (Proxy @A32) sym locLookup
                                                          (ARMDis.GPR gprN)
                                offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_off") knownRepr
                                addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_add") knownRepr
                                expr <- S.bvAdd sym base offset -- KWQ: need to reproduce offset manipulation
                                let recover evalFn = do
                                      offsetVal <- fromInteger <$> evalFn offset
                                      addflagVal <- fromInteger <$> evalFn addflag
                                      return $ ARMDis.Addrmode_imm12 $
                                             ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                                return (expr, T.WrappedRecoverOperandFn recover)
      ARMDis.Addrmode_imm12_preRepr ->
          mkTemplate <$> [0..numGPR-1]
            where mkTemplate gprNum = T.TemplatedOperand Nothing
                                      (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
                                          :: T.TemplatedOperand A32 "Addrmode_imm12_pre"
                    where mkTemplate' :: T.TemplatedOperandFn A32 "Addrmode_imm12_pre"
                          mkTemplate' sym locLookup = do
                            let gprN = ARMOperands.gpr $ gprNum
                            base <- A.unTagged <$> A.operandValue (Proxy @A32) sym locLookup (ARMDis.GPR gprN)
                            offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_off") knownRepr
                            addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_add") knownRepr
                            expr <- S.bvAdd sym base offset -- KWQ: need to reproduce offset manipulation
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  addflagVal <- fromInteger <$> evalFn addflag
                                  return $ ARMDis.Addrmode_imm12_pre $
                                         ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
                            return (expr, T.WrappedRecoverOperandFn recover)
      ARMDis.Arm_bl_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_blx_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
      ARMDis.Arm_br_targetRepr -> error "opTemplate ARM_br_targetRepr TBD"
      ARMDis.Cc_outRepr -> error "opTemplate ARM_Cc_outRepr TBD"
      ARMDis.GPRRepr -> concreteTemplatedOperand (ARMDis.GPR . ARMOperands.gpr) LocGPR <$> [0..numGPR-1]
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
      --                       return (expr, T.WrappedRecoverOperandFn recover)
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

----------------------------------------------------------------------
-- Concrete state functionality

mkRandomState :: DA.Gen -> IO ConcreteState
mkRandomState gen = St.execStateT randomize MapF.empty
  where
    randomize = do
      mapM_ addRandomBV gprList
      mapM_ addRandomBV fprList

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

  pc <- (MapF.Pair LocPC . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  gprs_mask <- forM gprMaskList $ \loc ->
      (MapF.Pair loc . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  fprs <- forM fprList $ \loc ->
      (MapF.Pair loc . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  cpsr <- (MapF.Pair LocCPSR . V.ValueBV . W.w . fromIntegral) <$> G.getWord32le

  m1 <- (MapF.Pair LocMem1 . V.ValueMem . B.pack) <$> replicateM 32 G.getWord8
  m2 <- (MapF.Pair LocMem1 . V.ValueMem . B.pack) <$> replicateM 32 G.getWord8

  return $ MapF.fromList $ concat [ gprs
                                  , gprs_mask
                                  , fprs
                                  , [m1, m2, pc, cpsr]
                                  ]

-- | Convert a machine state to the wire protocol.
--
-- Note that we perform a byte swap to put data in big endian so that the
-- machine on the receiving end doesn't need to do anything special besides map
-- the data.
serializeState :: ConcreteState -> B.ByteString
serializeState s = mempty
  LB.toStrict (B.toLazyByteString b)
  where
    b = mconcat [ mconcat (map (serializeSymVal (B.word32LE . fromInteger)) (extractLocs s gprList))
                , serializeSymVal (B.word32LE . fromInteger) (extractLoc s LocPC)
                , mconcat (map (serializeSymVal (B.word32LE . fromInteger)) (extractLocs s gprMaskList))
                , mconcat (map (serializeSymVal (B.word32LE . fromInteger)) (extractLocs s fprList))
                , serializeSymVal (B.word32LE . fromInteger) (extractLoc s LocCPSR)
                , serializeMem (extractLoc s LocMem1)
                , serializeMem (extractLoc s LocMem2)
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
    let Just v = MapF.lookup l s
    in v

extractLocs :: ConcreteState
            -> [Location A32 tp]
            -> [V.Value tp]
extractLocs s locs = map (extractLoc s) locs

gprList :: [Location A32 (BaseBVType 32)]
gprList = fmap LocGPR [0..31]

gprMaskList :: [Location A32 (BaseBVType 32)]
gprMaskList = fmap LocGPRMask [0..31]

fprList :: [Location A32 (BaseBVType 32)]
fprList = fmap LocFPR [0..31]
