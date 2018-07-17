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
{-# OPTIONS_GHC -Wno-orphans #-}
module SemMC.Architecture.A32
    ( A32
    , MachineState(..)
    , Instruction
    , ConcreteState
    , numGPR
    , testSerializer
    )
    where

import           Control.Monad ( replicateM )
import qualified Control.Monad.State.Strict as St
import           Control.Monad.Trans ( liftIO )
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.Int ( Int32 )
import           Data.List.NonEmpty ( NonEmpty(..), fromList )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Proxy ( Proxy(..) )
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Vector.Sized as V
import           Data.Word ( Word8, Word32 )
import qualified Data.Word.Indexed as W
import qualified Dismantle.Arbitrary as DA
import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMOperands
import qualified Dismantle.Instruction as D
import           GHC.TypeLits
import qualified GHC.Err.Located as L
import qualified Lang.Crucible.Backend as SB
import           Language.Haskell.TH hiding ( recover )
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
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import           What4.BaseTypes
import qualified What4.Interface as S


-- | Define the arch type for only the A32 instruction set.
data A32

type Instruction = LB.ByteString

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

testSerializer :: CE.TestSerializer MachineState LB.ByteString
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
      _ -> error "not implemented"
      -- PPC.Fprc {}              -> knownRepr

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
type instance A.OperandType A32 "Addrmode_imm12" = BaseBVType 32
type instance A.OperandType A32 "Addrmode_imm12_pre" = BaseBVType 32
type instance A.OperandType A32 "Am2offset_imm" = BaseBVType 32
type instance A.OperandType A32 "Arm_bl_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType A32 "Arm_blx_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType A32 "Arm_br_target" = BaseBVType 32 -- 24 bits in instr
type instance A.OperandType A32 "Cc_out" = BaseBVType 1
type instance A.OperandType A32 "GPR" = BaseBVType 32
type instance A.OperandType A32 "GPRnopc" = BaseBVType 32
type instance A.OperandType A32 "Imm0_7" = BaseBVType 3
type instance A.OperandType A32 "Imm0_15" = BaseBVType 4
type instance A.OperandType A32 "Imm0_31" = BaseBVType 5
type instance A.OperandType A32 "Ldst_so_reg" = BaseBVType 32
type instance A.OperandType A32 "Mod_imm" = BaseBVType 32
type instance A.OperandType A32 "Pred" = BaseBVType 4
type instance A.OperandType A32 "Reglist" = BaseBVType 16
type instance A.OperandType A32 "Shift_so_reg_imm" = BaseBVType 16
type instance A.OperandType A32 "So_reg_imm" = BaseBVType 32
type instance A.OperandType A32 "So_reg_reg" = BaseBVType 32
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

  registerizationLocations = []

parseLocation :: ARMComp.Parser (Some (Location A32))
parseLocation = do
  c <- P.lookAhead (P.anyChar)
  case c of
    'C' -> Some LocCPSR <$ P.string "CPSR"
    'M' -> Some LocMem <$ P.string "Mem"
    'P' -> Some LocPC <$ P.string "PC"
    'R' -> do
      parsePrefixedRegister (Some . LocGPR) 'R'
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

noLocation _ _ _ = Nothing

locationFuncInterpretation :: [(String, A.FunctionInterpretation t A32)]
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
                          { A.locationInterp = F.LocationFuncInterp (interpImm12Reg Just LocGPR)
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

type ConcreteState = MapF.MapF (Location A32) V.Value

-- | FIXME: Does not include memory
mkRandomState :: DA.Gen -> IO ConcreteState
mkRandomState gen = St.execStateT randomize MapF.empty
  where
    randomize = do
      mapM_ addRandomBV gprList
      -- mapM_ addRandomBV64 frs
      -- mapM_ addRandomBV vsrs
      -- mapM_ addZeroBV specialRegs32
      -- mapM_ addZeroBV specialRegs64
--      St.modify' $ MapF.insert LocMem (V.ValueMem (B.replicate 64 0))

    -- | Create a random 128 bit bitvector with the high 64 bits as zero.  We
    -- want this for the FRs, which would normally overlap with the VSRs.  If we
    -- had the VSRs, then we would want to generate full 128 bit values instead.
    -- addRandomBV64 :: Location A32 (BaseBVType 128) -> St.StateT (ConcreteState arm) IO ()
    -- addRandomBV64 loc = do
    --   bv :: V.Value (BaseBVType 64)
    --      <- V.ValueBV <$> liftIO (DA.arbitrary gen)
    --   St.modify' $ MapF.insert loc (PPCS.extendBV bv)

    addRandomBV :: Location A32 (BaseBVType 32) -> St.StateT ConcreteState IO ()
    addRandomBV loc = do
      bv <- V.ValueBV <$> liftIO (DA.arbitrary gen)
      St.modify' $ MapF.insert loc bv

    addZeroBV :: Location A32 (BaseBVType 32) -> St.StateT ConcreteState IO ()
    addZeroBV loc = do
      let bv = V.ValueBV (W.w 0)
      St.modify' $ MapF.insert loc bv

-- | States that include (pairs of) registers with interesting bit patterns.
-- For each pair of registers, combinations of interesting bit patterns are
-- chosen.  The other registers all have zeros.
--
-- FIXME: Doesn't include FP registers yet.  We'll want NaN and INF values there
interestingStates :: [ConcreteState]
interestingStates = []

-- | FIXME: Does not include memory
zeroState :: ConcreteState
zeroState = St.execState addZeros MapF.empty
  where
    addZero :: Location A32 (BaseBVType 32) -> St.State ConcreteState ()
    addZero loc = St.modify' $ MapF.insert loc (V.ValueBV (W.w 0))
    addZeros = do
      mapM_ addZero gprList
      -- mapM_ addZero vsrs
      -- mapM_ addZero specialRegs32
      -- mapM_ addZero specialRegs64
--      St.modify' $ MapF.insert LocMem (V.ValueMem (B.replicate 64 0))

-- | Convert a machine state to the wire protocol.
--
-- Note that we perform a byte swap to put data in big endian so that the
-- machine on the receiving end doesn't need to do anything special besides map
-- the data.
serializeState :: ConcreteState -> B.ByteString
serializeState s = mempty
  -- LB.toStrict (B.toLazyByteString b)
  -- where
  --   b = mconcat [ mconcat (map (PPCS.serializeSymVal (B.word32BE . fromInteger)) (extractLocs s gprList))
  --               , mconcat (map (PPCS.serializeSymVal (B.word32BE . fromInteger)) (extractLocs s specialRegs32))
  --               , mconcat (map (PPCS.serializeSymVal (B.word64BE . fromInteger)) (extractLocs s specialRegs64))
  --               , mconcat (map (PPCS.serializeSymVal PPCS.serializeVec) (extractLocs s vsrs))
  --               -- , mconcat (map serializeMem (extractLocs s [LocMem]))
  --               ]

serializeMem :: V.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)) -> B.Builder
serializeMem val =
  case val of
    V.ValueMem bs -> B.byteString bs

extractLocs :: ConcreteState
            -> [Location A32 tp]
            -> [V.Value tp]
extractLocs s locs = map extractLoc locs
  where
    extractLoc l =
      let Just v = MapF.lookup l s
      in v

deserializeState :: B.ByteString -> Maybe ConcreteState
deserializeState bs =
  case G.runGetOrFail getArchState (LB.fromStrict bs) of
    Left _ -> Nothing
    Right (_, _, s) -> Just s

getArchState :: G.Get ConcreteState
getArchState = do
    return MapF.empty

  -- gprs' <- mapM (getWith (PPCS.getValue G.getWord32be 32)) gprs
  -- spregs32' <- mapM (getWith (PPCS.getValue G.getWord32be PPCS.repr32)) specialRegs32
  -- spregs64' <- mapM (getWith (PPCS.getValue G.getWord64be 32)) specialRegs64
  -- frs' <- mapM (getWith (PPCS.getValue (PPCS.getWord128be PPCS.IgnoreHighBits) PPCS.repr128)) frs
  -- vsrs' <- mapM (getWith (PPCS.getValue (PPCS.getWord128be PPCS.KeepHighBits) PPCS.repr128)) vsrs
  -- -- mem' <- getBS
  -- return (St.execState (addLocs gprs' spregs32' spregs64' (frs' ++ vsrs') {- >> addLoc (LocMem, mem') -}) MapF.empty)
  -- where
  --   addLoc :: forall tp . (Location A32 tp, V.Value tp) -> St.State (ConcreteState A32) ()
  --   addLoc (loc, v) = St.modify' $ MapF.insert loc v

  --   addLocs gprs' spregs32' spregs64' vsrs' = do
  --     mapM_ addLoc gprs'
  --     mapM_ addLoc spregs32'
  --     mapM_ addLoc spregs64'
  --     mapM_ addLoc vsrs'

getWith :: G.Get (V.Value tp)
        -> Location A32 tp
        -> G.Get (Location A32 tp, V.Value tp)
getWith g loc = do
  w <- g
  return (loc, w)

getBS :: G.Get (V.Value (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8)))
getBS = V.ValueMem <$> G.getBytes 32

gprList :: [Location A32 (BaseBVType 32)]
gprList = fmap LocGPR [0..31]

-- vsrs :: [Location A32 (BaseBVType 128)]
-- vsrs = fmap (LocVSR . PPC.VSReg) [0..63]

-- frs :: [Location A32 (BaseBVType 128)]
-- frs = fmap (LocVSR . PPC.VSReg) [0..31]

-- vrs :: [Location A32 (BaseBVType 128)]
-- vrs = fmap (LocVSR . PPC.VSReg) [32..63]

-- specialRegs32 :: [Location A32 (BaseBVType 32)]
-- specialRegs32 = [ LocFPSCR
--                 , LocCR
--                 , LocVSCR
--                   -- Lets not randomly generate an MSR.  That would
--                   -- be problematic (e.g., it would switch endianness)
--                   --
--                   -- , LocMSR
--                 ]

-- specialRegs64 :: [Location A32 (BaseBVType 32)]
-- specialRegs64 = [ LocCTR
--                 , LocLNK
--                 , LocXER
--                 ]
