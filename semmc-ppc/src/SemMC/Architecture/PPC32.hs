{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module SemMC.Architecture.PPC32
  ( PPC
  , Location(..)
  , testSerializer
  , PPCP.PseudoOpcode(..)
  ) where

import qualified Data.Int.Indexed as I
import qualified Data.List.NonEmpty as NEL
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import qualified Data.Word.Indexed as W
import qualified Dismantle.Instruction as D
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()
import qualified GHC.Err.Located as L
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as AL
import qualified SemMC.Architecture.Concrete as AC
import           SemMC.Architecture.PPC ( AnyPPC, V32 )
import           SemMC.Architecture.PPC.Location
import qualified SemMC.Architecture.PPC.Pseudo as PPCP
import qualified SemMC.Architecture.PPC.Shared as PPCS
import qualified SemMC.Architecture.PPC.OperandComponents as POC
import qualified SemMC.Architecture.PPC.UF as UF
import qualified SemMC.Architecture.PPC32.ConcreteState as PPCS
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE
import           SemMC.Stochastic.Pseudo ( Pseudo, ArchitectureWithPseudo(..) )
import qualified SemMC.Stochastic.RvwpOptimization as R
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import           What4.BaseTypes
import qualified What4.Interface as S

type PPC = AnyPPC V32

-- Assuming 32-bit PPC for now.
type instance A.OperandType PPC "Abscalltarget" = BaseBVType 24
type instance A.OperandType PPC "Abscondbrtarget" = BaseBVType 14
type instance A.OperandType PPC "Absdirectbrtarget" = BaseBVType 24
type instance A.OperandType PPC "Calltarget" = BaseBVType 24
type instance A.OperandType PPC "Condbrtarget" = BaseBVType 14
type instance A.OperandType PPC "Crbitm" = BaseBVType 8
type instance A.OperandType PPC "Crbitrc" = BaseBVType 5
type instance A.OperandType PPC "Crrc" = BaseBVType 3
type instance A.OperandType PPC "Directbrtarget" = BaseBVType 24
-- The floating point registers are logically 64 bits, but are actually backed
-- by 128 bit vector registers (where the extra bits are probably left
-- undefined during non-vector operations).
type instance A.OperandType PPC "Fprc" = BaseBVType 128
type instance A.OperandType PPC "Gprc" = BaseBVType 32
type instance A.OperandType PPC "Gprc_nor0" = BaseBVType 32
type instance A.OperandType PPC "I1imm" = BaseBVType 1
type instance A.OperandType PPC "I32imm" = BaseBVType 32
type instance A.OperandType PPC "Memri" = BaseBVType 32
type instance A.OperandType PPC "Memrix" = BaseBVType 32
type instance A.OperandType PPC "Memrix16" = BaseBVType 32
type instance A.OperandType PPC "Memrr" = BaseBVType 32
type instance A.OperandType PPC "S16imm" = BaseBVType 16
type instance A.OperandType PPC "S16imm64" = BaseBVType 16
type instance A.OperandType PPC "S17imm" = BaseBVType 16
type instance A.OperandType PPC "S17imm64" = BaseBVType 16
type instance A.OperandType PPC "S5imm" = BaseBVType 5
type instance A.OperandType PPC "U10imm" = BaseBVType 10
type instance A.OperandType PPC "U16imm" = BaseBVType 16
type instance A.OperandType PPC "U16imm64" = BaseBVType 16
type instance A.OperandType PPC "U1imm" = BaseBVType 1
type instance A.OperandType PPC "U2imm" = BaseBVType 2
type instance A.OperandType PPC "U4imm" = BaseBVType 4
type instance A.OperandType PPC "U5imm" = BaseBVType 5
type instance A.OperandType PPC "U6imm" = BaseBVType 6
type instance A.OperandType PPC "U7imm" = BaseBVType 7
type instance A.OperandType PPC "U8imm" = BaseBVType 8
type instance A.OperandType PPC "Vrrc" = BaseBVType 128
type instance A.OperandType PPC "Vsrc" = BaseBVType 128

shapeReprType :: forall tp . PPC.OperandRepr tp -> BaseTypeRepr (A.OperandType PPC tp)
shapeReprType sr =
  case sr of
    PPC.AbscalltargetRepr -> knownRepr
    PPC.AbscondbrtargetRepr -> knownRepr
    PPC.AbsdirectbrtargetRepr -> knownRepr
    PPC.CalltargetRepr -> knownRepr
    PPC.CondbrtargetRepr -> knownRepr
    PPC.CrbitmRepr -> knownRepr
    PPC.CrbitrcRepr -> knownRepr
    PPC.CrrcRepr -> knownRepr
    PPC.DirectbrtargetRepr -> knownRepr
    PPC.FprcRepr -> knownRepr
    PPC.GprcRepr -> knownRepr
    PPC.VrrcRepr -> knownRepr
    PPC.VsrcRepr -> knownRepr
    PPC.Gprc_nor0Repr -> knownRepr
    PPC.I1immRepr -> knownRepr
    PPC.I32immRepr -> knownRepr
    PPC.MemriRepr -> knownRepr
    PPC.MemrixRepr -> knownRepr
    PPC.Memrix16Repr -> knownRepr
    PPC.MemrrRepr -> knownRepr
    PPC.S16immRepr -> knownRepr
    PPC.S16imm64Repr -> knownRepr
    PPC.S17immRepr -> knownRepr
    PPC.S17imm64Repr -> knownRepr
    PPC.S5immRepr -> knownRepr
    PPC.U10immRepr -> knownRepr
    PPC.U16immRepr -> knownRepr
    PPC.U16imm64Repr -> knownRepr
    PPC.U1immRepr -> knownRepr
    PPC.U2immRepr -> knownRepr
    PPC.U4immRepr -> knownRepr
    PPC.U5immRepr -> knownRepr
    PPC.U6immRepr -> knownRepr
    PPC.U7immRepr -> knownRepr
    PPC.U8immRepr -> knownRepr

type instance A.OperandComponents PPC sym = POC.OperandComponents PPC sym

instance ArchRepr PPC where
  regWidthRepr _ = PPCS.repr32

instance T.TemplatableOperand PPC where
  opTemplates sr =
    case sr of
      PPC.VsrcRepr -> PPCS.concreteTemplatedOperand (PPC.Vsrc . PPC.VSReg) (LocVSR . PPC.VSReg) <$> [0..63]
      PPC.S17imm64Repr -> [PPCS.symbolicTemplatedOperand (Proxy @16) True "S17imm64" (PPC.S17imm64 . fromInteger)]
      PPC.MemrrRepr -> mkTemplate <$> [0..31] <*> [0..31]
            where mkTemplate gprNum gprOffset =
                      T.TemplatedOperand Nothing (Set.fromList [Some (LocGPR (PPC.GPR gprNum)),Some (LocGPR (PPC.GPR gprOffset))]) mkTemplate' :: T.TemplatedOperand PPC "Memrr"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memrr"
                          mkTemplate' _sym locLookup = do
                            let offLoc = LocGPR (PPC.GPR gprOffset)
                            let baseLoc = LocGPR (PPC.GPR gprNum)
                            base <- locLookup baseLoc
                            offset <- locLookup offLoc
                            let recover _ = do
                                       let gpr | gprNum /= 0 = Just (PPC.GPR gprNum)
                                               | otherwise = Nothing
                                       return $ PPC.Memrr $ PPC.MemRR gpr (PPC.GPR gprOffset)
                            return ( A.CompoundOperand (POC.OCMemrr baseLoc base offLoc offset)
                                   , T.RecoverOperandFn recover
                                   )
      PPC.MemrixRepr ->
         mkTemplate <$> [0..31]
            where mkTemplate gprNum = T.TemplatedOperand Nothing (Set.singleton (Some (LocGPR (PPC.GPR gprNum)))) mkTemplate' :: T.TemplatedOperand PPC "Memrix"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memrix"
                          mkTemplate' sym locLookup = do
                            let baseReg = LocGPR (PPC.GPR gprNum)
                            base <- locLookup baseReg
                            offset <- S.freshConstant sym (U.makeSymbol "Memrix_off") knownRepr
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  let gpr
                                        | gprNum /= 0 = Just (PPC.GPR gprNum)
                                        | otherwise = Nothing
                                  return $ PPC.Memrix $ PPC.MemRIX gpr offsetVal
                            return ( A.CompoundOperand (POC.OCMemrix baseReg base offset)
                                   , T.RecoverOperandFn recover
                                   )
      PPC.Memrix16Repr ->
         mkTemplate <$> [0..31]
            where mkTemplate gprNum = T.TemplatedOperand Nothing (Set.singleton (Some (LocGPR (PPC.GPR gprNum)))) mkTemplate' :: T.TemplatedOperand PPC "Memrix16"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memrix16"
                          mkTemplate' sym locLookup = do
                            let baseReg = LocGPR (PPC.GPR gprNum)
                            base <- locLookup baseReg
                            offset <- S.freshConstant sym (U.makeSymbol "Memrix16_off") knownRepr
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  let gpr
                                        | gprNum /= 0 = Just (PPC.GPR gprNum)
                                        | otherwise = Nothing
                                  return $ PPC.Memrix16 $ PPC.MemRIX gpr offsetVal
                            return ( A.CompoundOperand (POC.OCMemrix baseReg base offset)
                                   , T.RecoverOperandFn recover
                                   )
      PPC.VrrcRepr -> PPCS.concreteTemplatedOperand (PPC.Vrrc . PPC.VR) (LocVSR . PPC.VSReg) <$> [0..31]
      PPC.U4immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @4) True "U4imm" (PPC.U4imm . fromInteger)]
      PPC.U7immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @7) True "U7imm" (PPC.U7imm . fromInteger)]
      PPC.U8immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @8) True "U8imm" (PPC.U8imm . fromInteger)]
      PPC.U10immRepr->  [PPCS.symbolicTemplatedOperand (Proxy @10) True "U10imm" (PPC.U10imm . fromInteger)]
      PPC.S5immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @5) True "S5imm" (PPC.S5imm . fromInteger)]
      PPC.U1immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @1) True "U1imm" (PPC.U1imm . fromInteger)]
      PPC.U2immRepr ->  [PPCS.symbolicTemplatedOperand (Proxy @2) True "U2imm" (PPC.U2imm . fromInteger)]
      PPC.AbscondbrtargetRepr ->
         [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Abscondbrtarget"
                    mkDirect sym _ = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Abscondbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 14))
                      let recover evalFn =
                            PPC.Abscondbrtarget . PPC.mkAbsCondBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (A.ValueOperand offsetRaw, T.RecoverOperandFn recover)
      PPC.CondbrtargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Condbrtarget"
                    mkDirect sym _locLookup = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Condbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 14))
                      let recover evalFn =
                            PPC.Condbrtarget . PPC.mkCondBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (A.ValueOperand offsetRaw, T.RecoverOperandFn recover)

      PPC.CrbitmRepr ->
        [T.TemplatedOperand Nothing Set.empty mkDirect]
               where mkDirect :: T.TemplatedOperandFn PPC "Crbitm"
                     mkDirect sym _ = do
                       crrc <- S.freshConstant sym (U.makeSymbol "Crbitm") (knownRepr :: BaseTypeRepr (BaseBVType 8))
                       let recover evalFn =
                             PPC.Crbitm . PPC.CRBitM . fromInteger <$> evalFn crrc
                       return (A.ValueOperand crrc, T.RecoverOperandFn recover)
      PPC.I1immRepr ->
        [PPCS.symbolicTemplatedOperand (Proxy @1) True "I1imm" (PPC.I1imm . fromInteger)]
      PPC.FprcRepr -> PPCS.concreteTemplatedOperand (PPC.Fprc . PPC.FR) (LocVSR . PPC.VSReg) <$> [0..31]
      PPC.GprcRepr -> PPCS.concreteTemplatedOperand PPC.Gprc LocGPR . PPC.GPR <$> [0..31]
      PPC.Gprc_nor0Repr -> PPCS.concreteTemplatedOperand PPC.Gprc_nor0 LocGPR . PPC.GPR <$> [0..31]
      PPC.S16immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @16) True "S16imm" (PPC.S16imm . fromInteger)]
      PPC.S16imm64Repr -> [PPCS.symbolicTemplatedOperand (Proxy @16) True "S16imm64" (PPC.S16imm64 . fromInteger)]
      PPC.U16immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @16) False "U16imm" (PPC.U16imm . fromInteger)]
      PPC.U16imm64Repr -> [PPCS.symbolicTemplatedOperand (Proxy @16) False "U16imm64" (PPC.U16imm64 . fromInteger)]
      PPC.MemriRepr ->
          mkTemplate <$> [0..31]
            where mkTemplate gprNum = T.TemplatedOperand Nothing (Set.singleton (Some (LocGPR (PPC.GPR gprNum)))) mkTemplate' :: T.TemplatedOperand PPC "Memri"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memri"
                          mkTemplate' sym locLookup = do
                            let baseReg = LocGPR (PPC.GPR gprNum)
                            base <- locLookup baseReg
                            offset <- S.freshConstant sym (U.makeSymbol "Memri_off") knownRepr
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  let gpr
                                        | gprNum /= 0 = Just (PPC.GPR gprNum)
                                        | otherwise = Nothing
                                  return $ PPC.Memri $ PPC.MemRI gpr offsetVal
                            return ( A.CompoundOperand (POC.OCMemri baseReg base offset)
                                   , T.RecoverOperandFn recover
                                   )
      PPC.DirectbrtargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Directbrtarget"
                    mkDirect sym _locLookup = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Directbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                      let recover evalFn =
                            PPC.Directbrtarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (A.ValueOperand offsetRaw, T.RecoverOperandFn recover)
      PPC.U5immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @5) False "U5imm" (PPC.U5imm . fromInteger)]
      PPC.U6immRepr -> [PPCS.symbolicTemplatedOperand (Proxy @6) False "U6imm" (PPC.U6imm . fromInteger)]
      PPC.S17immRepr ->
                    [T.TemplatedOperand Nothing Set.empty mkImm]
              where mkImm :: T.TemplatedOperandFn PPC "S17imm"
                    mkImm sym _ = do
                      v <- S.freshConstant sym (U.makeSymbol "S17imm") (knownRepr :: BaseTypeRepr (BaseBVType 16))
                      let recover evalFn = PPC.S17imm . fromInteger <$> evalFn v
                      return (A.ValueOperand v, T.RecoverOperandFn recover)
      PPC.AbsdirectbrtargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Absdirectbrtarget"
                    mkDirect sym _ = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Absdirectbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                      let recover evalFn =
                            PPC.Absdirectbrtarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (A.ValueOperand offsetRaw, T.RecoverOperandFn recover)
      PPC.CalltargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Calltarget"
                    mkDirect sym _locLookup = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Calltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                      let recover evalFn =
                            PPC.Calltarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (A.ValueOperand offsetRaw, T.RecoverOperandFn recover)
      PPC.AbscalltargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
               where mkDirect :: T.TemplatedOperandFn PPC "Abscalltarget"
                     mkDirect sym _ = do
                       offsetRaw <- S.freshConstant sym (U.makeSymbol "Abscalltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                       let recover evalFn =
                             PPC.Abscalltarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
                       return (A.ValueOperand offsetRaw, T.RecoverOperandFn recover)
      PPC.CrrcRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Crrc"
                    mkDirect sym _ = do
                      crrc <- S.freshConstant sym (U.makeSymbol "Crrc") (knownRepr :: BaseTypeRepr (BaseBVType 3))
                      let recover evalFn =
                            PPC.Crrc . PPC.CRRC . fromInteger <$> evalFn crrc
                      return (A.ValueOperand crrc, T.RecoverOperandFn recover)
      PPC.CrbitrcRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
               where mkDirect :: T.TemplatedOperandFn PPC "Crbitrc"
                     mkDirect sym _ = do
                       crrc <- S.freshConstant sym (U.makeSymbol "Crbitrc") (knownRepr :: BaseTypeRepr (BaseBVType 5))
                       let recover evalFn =
                             PPC.Crbitrc . PPC.CRBitRC . fromInteger <$> evalFn crrc
                       return (A.ValueOperand crrc, T.RecoverOperandFn recover)
      PPC.I32immRepr ->
            [T.TemplatedOperand Nothing Set.empty mkImm]
              where mkImm :: T.TemplatedOperandFn PPC "I32imm"
                    mkImm sym _ = do
                      v <- S.freshConstant sym (U.makeSymbol "I32imm") knownRepr
                      let recover evalFn = PPC.I32imm . fromInteger <$> evalFn v
                      return (A.ValueOperand v, T.RecoverOperandFn recover)

type instance A.Location PPC = Location PPC

operandValue :: forall sym s.
                (S.IsSymExprBuilder sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location PPC tp -> IO (S.SymExpr sym tp))
             -> PPC.Operand s
             -> IO (A.TaggedExpr PPC sym s)
operandValue sym locLookup op = TaggedExpr <$> operandValue' op
  where operandValue' :: PPC.Operand s -> IO (A.AllocatedOperand PPC sym s)
        operandValue' (PPC.Abscalltarget (PPC.ABT absTarget)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Abscondbrtarget (PPC.ACBT absTarget)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Absdirectbrtarget (PPC.ABT absTarget)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Calltarget (PPC.BT off)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger off)
        operandValue' (PPC.Condbrtarget (PPC.CBT off)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger off)
        operandValue' (PPC.Crbitm (PPC.CRBitM n)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Crbitrc (PPC.CRBitRC n)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Crrc (PPC.CRRC n)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Directbrtarget (PPC.BT off)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger off)
        operandValue' (PPC.Fprc (PPC.FR fr)) =
          let loc = LocVSR (PPC.VSReg fr)
          in A.LocationOperand loc <$> locLookup loc
        operandValue' (PPC.Gprc gpr) =
          let loc = LocGPR gpr
          in A.LocationOperand loc <$> locLookup loc
        operandValue' (PPC.Gprc_nor0 (PPC.GPR gpr)) =
          let loc = LocGPR (PPC.GPR gpr)
          in if gpr /= 0
             then A.LocationOperand loc <$> locLookup loc
             else A.ValueOperand <$> S.bvLit sym knownNat 0
        operandValue' (PPC.I1imm (I.I x)) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger x)
        operandValue' (PPC.I32imm (I.I x)) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger x)
        operandValue' (PPC.Memri (PPC.MemRI gpr offset)) = do
          base <- PPCS.fromMaybeGPRBase sym gpr locLookup
          offset' <- S.bvLit sym knownNat (toInteger offset)
          return (A.CompoundOperand (POC.OCMemri (PPCS.fromMaybeGPRLoc gpr) base offset'))
        operandValue' (PPC.Memrix (PPC.MemRIX gpr offset)) = do
          base <- PPCS.fromMaybeGPRBase sym gpr locLookup
          offset' <- S.bvLit sym knownNat (toInteger (I.unI offset))
          return (A.CompoundOperand (POC.OCMemrix (PPCS.fromMaybeGPRLoc gpr) base offset'))
        operandValue' (PPC.Memrix16 (PPC.MemRIX gpr offset)) = do
          base <- PPCS.fromMaybeGPRBase sym gpr locLookup
          offset' <- S.bvLit sym knownNat (toInteger (I.unI offset))
          return (A.CompoundOperand (POC.OCMemrix (PPCS.fromMaybeGPRLoc gpr) base offset'))
        operandValue' (PPC.Memrr (PPC.MemRR gpr1 gpr2)) = do
          gpr1Val <- PPCS.fromMaybeGPRBase sym gpr1 locLookup
          gpr2Val <- locLookup (LocGPR gpr2)
          return (A.CompoundOperand (POC.OCMemrr (PPCS.fromMaybeGPRLoc gpr1) gpr1Val (LocGPR gpr2) gpr2Val))
        operandValue' (PPC.S16imm i16) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S16imm64 i16) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S17imm i16) =
          -- The s17 imm type is a bit strange.  It is actually represented as
          -- 16 bits in the instruction, but is interpreted as shifted left by
          -- 16 bits.  We handle that correction in the semantics, so we just
          -- treat s17imm as a plain 16 bit value.
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S17imm64 i16) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S5imm (I.I i5)) = A.ValueOperand <$> S.bvLit sym knownNat (toInteger i5)
        operandValue' (PPC.U10imm (W.unW ->  w10)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w10)
        operandValue' (PPC.U16imm (W.unW ->  w16)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w16)
        operandValue' (PPC.U16imm64 (W.unW ->  w16)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w16)
        operandValue' (PPC.U1imm (W.unW ->  w1)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w1)
        operandValue' (PPC.U2imm (W.unW ->  w2)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w2)
        operandValue' (PPC.U4imm (W.unW ->  w4)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w4)
        operandValue' (PPC.U5imm (W.unW ->  w5)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w5)
        operandValue' (PPC.U6imm (W.unW ->  w6)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w6)
        operandValue' (PPC.U7imm (W.unW ->  w7)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w7)
        operandValue' (PPC.U8imm (W.unW ->  w8)) =
          A.ValueOperand <$> S.bvLit sym knownNat (toInteger w8)
        operandValue' (PPC.Vrrc (PPC.VR vr)) =
          let loc = LocVSR (PPC.VSReg (vr + 32))
          in A.LocationOperand loc <$> locLookup loc
        operandValue' (PPC.Vsrc vsr) =
          let loc = LocVSR vsr
          in A.LocationOperand loc <$> locLookup loc


operandToLocation :: PPC.Operand s -> Maybe (Location PPC (A.OperandType PPC s))
operandToLocation (PPC.Fprc (PPC.FR fr)) = Just $ LocVSR (PPC.VSReg fr)
operandToLocation (PPC.Gprc gpr) = Just $ LocGPR gpr
operandToLocation (PPC.Gprc_nor0 gpr) = Just (LocGPR gpr)
operandToLocation (PPC.Vrrc (PPC.VR vr)) = Just $ LocVSR (PPC.VSReg (vr + 32))
operandToLocation (PPC.Vsrc vr) = Just $ LocVSR vr
operandToLocation _ = Nothing

instance A.Architecture PPC where
  data TaggedExpr PPC sym s = TaggedExpr (A.AllocatedOperand PPC sym s)
  unTagged (TaggedExpr te) =
    case te of
      A.ValueOperand se -> Just se
      A.LocationOperand _ se -> Just se
      A.CompoundOperand {} -> Nothing
  taggedOperand (TaggedExpr e) = e
  allocateSymExprsForOperand _ = operandValue
  operandToLocation _ = operandToLocation
  uninterpretedFunctions = UF.uninterpretedFunctions
  readMemUF  = A.uninterpFnName . UF.mkUninterpFnReadMem @PPC
  writeMemUF = A.uninterpFnName . UF.mkUninterpFnWriteMem @PPC
  locationFuncInterpretation _proxy = A.createSymbolicEntries PPCS.locationFuncInterpretation
  archEndianForm _proxy = A.BigEndian
  shapeReprToTypeRepr _proxy = shapeReprType
  operandComponentsImmediate = POC.operandComponentsImmediate

operandTypePPC :: PPC.Operand s -> BaseTypeRepr (A.OperandType PPC s)
operandTypePPC o =
  case o of
    PPC.Fprc {}              -> knownRepr
    PPC.Gprc {}              -> knownRepr
    PPC.Gprc_nor0 {}         -> knownRepr
    PPC.Vrrc {}              -> knownRepr
    PPC.Vsrc {}              -> knownRepr
    PPC.Abscalltarget {}     -> knownRepr
    PPC.Abscondbrtarget {}   -> knownRepr
    PPC.Absdirectbrtarget {} -> knownRepr
    PPC.Calltarget {}        -> knownRepr
    PPC.Condbrtarget {}      -> knownRepr
    PPC.Crbitm {}            -> knownRepr
    PPC.Crbitrc {}           -> knownRepr
    PPC.Crrc {}              -> knownRepr
    PPC.Directbrtarget {}    -> knownRepr
    PPC.Memri {}             -> knownRepr
    PPC.Memrix {}            -> knownRepr
    PPC.Memrix16 {}          -> knownRepr
    PPC.Memrr {}             -> knownRepr
    PPC.I1imm {}             -> knownRepr
    PPC.I32imm {}            -> knownRepr
    PPC.S16imm {}            -> knownRepr
    PPC.S16imm64 {}          -> knownRepr
    PPC.S17imm {}            -> knownRepr
    PPC.S17imm64 {}          -> knownRepr
    PPC.S5imm {}             -> knownRepr
    PPC.U10imm {}            -> knownRepr
    PPC.U16imm {}            -> knownRepr
    PPC.U16imm64 {}          -> knownRepr
    PPC.U1imm {}             -> knownRepr
    PPC.U2imm {}             -> knownRepr
    PPC.U4imm {}             -> knownRepr
    PPC.U5imm {}             -> knownRepr
    PPC.U6imm {}             -> knownRepr
    PPC.U7imm {}             -> knownRepr
    PPC.U8imm {}             -> knownRepr

registerizeInstructionPPC :: AC.RegisterizedInstruction PPC
                          -> V.ConcreteState PPC
                          -> (A.Instruction PPC, V.ConcreteState PPC)
registerizeInstructionPPC ri s =
  case ri of
    AC.RI { AC.riOpcode = opc
          , AC.riOperands = ops
          , AC.riLiteralLocs = lls
          } ->
      case MapF.foldrWithKey replaceLiterals (ops, s) lls of
        (ops', s') -> (D.Instruction opc ops', s')

replaceLiterals :: AC.LiteralRef PPC sh s
                -> Location PPC s
                -> (SL.List PPC.Operand sh, V.ConcreteState PPC)
                -> (SL.List PPC.Operand sh, V.ConcreteState PPC)
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
truncateValue :: PPC.Operand s
              -> V.Value (A.OperandType PPC s)
              -> (V.Value (A.OperandType PPC s), PPC.Operand s)
truncateValue op v =
  case op of
    PPC.I1imm {}             -> PPCS.withTruncIVal v (W.w 0x1) PPC.I1imm
    PPC.I32imm {}            -> PPCS.withTruncIVal v (W.w 0xffffffff) PPC.I32imm
    PPC.S16imm {}            -> PPCS.withTruncI16Val v 0xffff PPC.S16imm
    PPC.S16imm64 {}          -> PPCS.withTruncI16Val v 0xffff PPC.S16imm64
    PPC.S17imm {}            -> PPCS.withTruncI16Val v 0xffff PPC.S17imm
    PPC.S5imm {}             -> PPCS.withTruncIVal v (W.w 0x1f) PPC.S5imm
    PPC.U1imm {}             -> PPCS.withTruncWVal v (W.w 0x1) PPC.U1imm
    PPC.U2imm {}             -> PPCS.withTruncWVal v (W.w 0x3) PPC.U2imm
    PPC.U4imm {}             -> PPCS.withTruncWVal v (W.w 0xf) PPC.U4imm
    PPC.U5imm {}             -> PPCS.withTruncWVal v (W.w 0x1f) PPC.U5imm
    PPC.U6imm {}             -> PPCS.withTruncWVal v (W.w 0x3f) PPC.U6imm
    PPC.U7imm {}             -> PPCS.withTruncWVal v (W.w 0x7f) PPC.U7imm
    PPC.U8imm {}             -> PPCS.withTruncWVal v (W.w 0xff) PPC.U8imm
    PPC.U10imm {}            -> PPCS.withTruncWVal v (W.w 0x3ff) PPC.U10imm
    PPC.U16imm {}            -> PPCS.withTruncWVal v (W.w 0xffff) PPC.U16imm
    PPC.U16imm64 {}          -> PPCS.withTruncWVal v (W.w 0xffff) PPC.U16imm64
    PPC.S17imm64 {}          -> PPCS.withTruncI16Val v 0xffff PPC.S17imm64
    PPC.Memrr {}             -> L.error "Unexpected non-literal operand"
    PPC.Memri {}             -> L.error "Unexpected non-literal operand"
    PPC.Memrix {}            -> L.error "Unexpected non-literal operand"
    PPC.Memrix16 {}          -> L.error "Unexpected non-literal operand"
    PPC.Vrrc {}              -> L.error "Unexpected non-literal operand"
    PPC.Vsrc {}              -> L.error "Unexpected non-literal operand"
    PPC.Gprc_nor0 {}         -> L.error "Unexpected non-literal operand"
    PPC.Gprc {}              -> L.error "Unexpected non-literal operand"
    PPC.Fprc {}              -> L.error "Unexpected non-literal operand"
    PPC.Abscondbrtarget {}   -> L.error "Control flow transfer instructions unsupported"
    PPC.Absdirectbrtarget {} -> L.error "Control flow transfer instructions unsupported"
    PPC.Condbrtarget {}      -> L.error "Control flow transfer instructions unsupported"
    PPC.Directbrtarget {}    -> L.error "Control flow transfer instructions unsupported"
    PPC.Calltarget {}        -> L.error "Control flow transfer instructions unsupported"
    PPC.Abscalltarget {}     -> L.error "Control flow transfer instructions unsupported"
    PPC.Crbitm {}            -> L.error "Control flow transfer instructions unsupported"
    PPC.Crbitrc {}           -> L.error "Control flow transfer instructions unsupported"
    PPC.Crrc {}              -> L.error "Control flow transfer instructions unsupported"

instance AC.ConcreteArchitecture PPC where
  registerizeInstruction = registerizeInstructionPPC
  operandType _proxy = operandTypePPC
  zeroState _proxy = PPCS.zeroState
  randomState _proxy = PPCS.randomState
  serialize _proxy = PPCS.serialize
  deserialize _proxy = PPCS.deserialize
  operandToSemanticView _proxy = operandToSemanticViewPPC
  heuristicallyInterestingStates _proxy = PPCS.interestingStates
  readView = P.parseMaybe (V.parseView parseLocation)
  showView = V.printView show

testSerializer :: CE.TestSerializer (V.ConcreteState PPC) (A.Instruction PPC)
testSerializer = CE.TestSerializer { CE.flattenMachineState = PPCS.serialize
                                   , CE.parseMachineState = PPCS.deserialize
                                   , CE.flattenProgram = mconcat . map PPC.assembleInstruction
                                   }

vsrLowerHalf :: V.Slice 64 128
vsrLowerHalf = V.Slice knownNat knownNat (knownNat @0) (knownNat @64)

operandToSemanticViewPPC :: PPC.Operand s -> Maybe (V.SemanticView PPC)
operandToSemanticViewPPC op =
  case op of
    PPC.Fprc fr -> frSemanticView fr
    PPC.Gprc gpr -> gprSemanticView gpr
    PPC.Gprc_nor0 gpr -> gprSemanticView gpr
    PPC.Vrrc vr -> vrSemanticView vr
    PPC.Vsrc vsr -> vsrSemanticView vsr
    _ -> Nothing
  where frSemanticView (PPC.FR rno) =
          Just $ V.SemanticView { V.semvView = frView rno
                                 , V.semvCongruentViews = [ frView rno' | rno' <- [0..31], rno' /= rno ]
                                 , V.semvDiff = V.diffFloat
                                 }
        frView rno = V.View vsrLowerHalf (LocVSR (PPC.VSReg rno))

        gprSemanticView (PPC.GPR rno) =
          Just $ V.SemanticView { V.semvView = gprView rno
                                 , V.semvCongruentViews = [ gprView rno' | rno' <- [0..31], rno' /= rno ]
                                 , V.semvDiff = V.diffInt
                                 }
        gprView rno = V.trivialView Proxy (LocGPR (PPC.GPR rno))

        vrSemanticView (PPC.VR rno) =
          Just $ V.SemanticView { V.semvView = vrView rno
                                 , V.semvCongruentViews = [ vrView rno' | rno' <- [0..31], rno' /= rno ]
                                 -- FIXME: we'll have to decide the diff function based on opcode
                                 , V.semvDiff = V.diffInt
                                 }
        vrView rno = V.trivialView Proxy (LocVSR (PPC.VSReg (rno + 32)))

        vsrSemanticView (PPC.VSReg rno) =
          Just $ V.SemanticView { V.semvView = vsrView rno
                                 , V.semvCongruentViews = [ vsrView rno' | rno' <- [0..63], rno' /= rno ]
                                 -- FIXME: we'll have to decide the diff function based on opcode
                                 , V.semvDiff = V.diffInt
                                 }
        vsrView rno = V.trivialView Proxy (LocVSR (PPC.VSReg rno))


type instance Pseudo PPC = PPCP.PseudoOpcode

instance ArchitectureWithPseudo PPC where
  assemblePseudo _ = PPCP.ppcAssemblePseudo (Proxy @PPC)

instance A.IsLocation (Location PPC) where

  isMemoryLocation l =
    case l of
      LocMem -> True
      _ -> False

  readLocation = P.parseMaybe parseLocation

  locationType (LocGPR _) = knownRepr
  locationType LocIP = knownRepr
  locationType LocMSR = knownRepr
  locationType LocCTR = knownRepr
  locationType LocLNK = knownRepr
  locationType LocXER = knownRepr
  locationType LocCR = knownRepr
  locationType (LocVSR _) = knownRepr
  locationType LocFPSCR = knownRepr
  locationType LocVSCR = knownRepr
  locationType LocMem = knownRepr

  defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocIP = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMSR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCTR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocLNK = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocXER = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocVSR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocFPSCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocVSCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMem =
    S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0

  nonMemLocations = concat
    [ map (Some . LocGPR . PPC.GPR) [0..31]
    , map (Some . LocVSR . PPC.VSReg) [0..63]
    , [ Some LocIP
      , Some LocMSR
      , Some LocCTR
      , Some LocLNK
      , Some LocXER
      , Some LocCR
      , Some LocFPSCR
      , Some LocVSCR
      ]
    ]

  memLocation = [AL.toMemLoc LocMem]


  registerizationLocations = map (Some . LocGPR . PPC.GPR) (0 : [3..10])

  isIP LocIP = True
  isIP _     = False

parseLocation :: PPCS.Parser (Some (Location PPC))
parseLocation = do
  c <- P.lookAhead (P.anySingle)
  case c of
    'I' -> Some LocIP <$ P.string "IP"
    'X' -> Some LocXER <$ P.string "XER"
    'L' -> Some LocLNK <$ P.string "LNK"
    'r' -> PPCS.parsePrefixedRegister (Some . LocGPR . PPC.GPR) 'r'
    'x' -> PPCS.parsePrefixedRegister (Some . LocVSR . PPC.VSReg) 'x'
    'C' -> PPCS.tryOne [ Some LocCTR <$ P.string "CTR"
                       , Some LocCR <$ P.string "CR"
                       ]
    'M' -> PPCS.tryOne [ Some LocMSR <$ P.string "MSR"
                       , Some LocMem <$ P.string "Mem"
                       ]
    'F' -> Some LocFPSCR <$ P.string "FPSCR"
    'V' -> Some LocVSCR <$ P.string "VSCR"
    _ -> P.failure (Just $ P.Tokens $ c NEL.:| [])
                (Set.fromList $ [ P.Label $ NEL.fromList "Location" ])

----------------------------------------------------------------

instance R.RvwpOptimization PPC where
  rvwpMov _ _ = Nothing -- TODO. Was needed to compile semmc-ppc.
