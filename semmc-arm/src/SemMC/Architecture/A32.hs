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
    , numGPR
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
import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMOperands
import           GHC.TypeLits
import qualified Lang.Crucible.Backend as SB
import           Language.Haskell.TH hiding ( recover )
import qualified SemMC.Architecture as A
import           SemMC.Architecture.AArch32 hiding ( testSerializer )
import           SemMC.Architecture.ARM.BaseSemantics.Registers ( numGPR )
import qualified SemMC.Architecture.ARM.Components as ARMComp
import           SemMC.Architecture.ARM.Eval
import           SemMC.Architecture.ARM.Location
import qualified SemMC.Architecture.ARM.UF as UF
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

testSerializer :: CE.TestSerializer MachineState LB.ByteString
testSerializer = CE.TestSerializer { CE.flattenMachineState = machineStateToBS
                                   , CE.parseMachineState = machineStateFromBS
                                   , CE.flattenProgram = mconcat
                                   }

-- ----------------------------------------------------------------------

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
type instance A.OperandType A32 "Unpredictable" = A.OperandType AArch32 "Unpredictable"

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

-- ----------------------------------------------------------------------

type instance ArchRegWidth A32 = 32


instance A.Architecture A32 where
    data TaggedExpr A32 sym s = TaggedExpr (S.SymExpr sym (A.OperandType A32 s))
    unTagged (TaggedExpr e) = e
    operandValue _ = operandValue
    operandToLocation _ = operandToLocation
    uninterpretedFunctions = UF.uninterpretedFunctions
    locationFuncInterpretation _proxy = createSymbolicEntries locationFuncInterpretation
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
                          { A.locationInterp = F.LocationFuncInterp (interpImm12Reg Just)
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
                                          { A.locationInterp = F.LocationFuncInterp (interpLdstsoregBaseReg Just)
                                          , A.exprInterpName = 'interpLdstsoregBaseRegExtractor
                                          })
    , ("a32.ldst_so_reg_offset_register", A.FunctionInterpretation
                                            { A.locationInterp = F.LocationFuncInterp (interpLdstsoregOffReg Just)
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
                             { A.locationInterp = F.LocationFuncInterp (interpSoregimmReg Just)
                             , A.exprInterpName = 'interpSoregimmRegExtractor })

    , ("a32.soregreg_type", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp noLocation
                              , A.exprInterpName = 'interpSoregregTypeExtractor
                              })
    , ("a32.soregreg_reg1", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp (interpSoregregReg1 Just)
                              , A.exprInterpName = 'interpSoregregReg1Extractor })
    , ("a32.soregreg_reg2", A.FunctionInterpretation
                              { A.locationInterp = F.LocationFuncInterp (interpSoregregReg2 Just)
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

