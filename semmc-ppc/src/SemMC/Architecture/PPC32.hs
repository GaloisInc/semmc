{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module SemMC.Architecture.PPC32
  ( PPC
  , Location(..)
  , testSerializer
  , PPCP.PseudoOpcode(..)
  ) where

import qualified GHC.Err.Located as L

import qualified Data.Int.Indexed as I
import qualified Data.List.NonEmpty as NEL
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Context
import           Data.Parameterized.TraversableFC
import           Data.Parameterized.NatRepr
import           Data.Proxy ( Proxy(..) )
import           Data.Maybe
import           Data.List
import qualified Data.Set as Set
import           Data.Word
import qualified Data.Word.Indexed as W
import           GHC.TypeLits ( KnownNat, Nat )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.Instruction as D
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as AC
import qualified SemMC.Architecture.Value as V
import qualified SemMC.Architecture.View as V
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Formula as F
import qualified SemMC.BoundVar as BoundVar

import qualified SemMC.Formula.Eval as E
import           SemMC.Stochastic.Pseudo ( Pseudo, ArchitectureWithPseudo(..) )
import qualified SemMC.Stochastic.RvwpOptimization as R
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U

import           SemMC.Architecture.PPC.Eval
import           SemMC.Architecture.PPC.Location
import qualified SemMC.Architecture.PPC32.ConcreteState as PPCS
import qualified SemMC.Architecture.PPC.Pseudo as PPCP
import qualified SemMC.Architecture.PPC.Shared as PPCS
import qualified SemMC.Architecture.PPC.UF as UF

data PPC

type instance A.Operand PPC = PPC.Operand

instance A.IsOperand PPC.Operand

type instance A.Opcode PPC = PPC.Opcode

instance A.IsOpcode PPC.Opcode

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

type instance ArchRegWidth PPC = 32

instance ArchRepr PPC where
  regWidthRepr _ = PPCS.repr32

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
                         -> Bool
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
              if signed
              then S.bvSext sym knownNat v
              else S.bvZext sym knownNat v
            NatCaseEQ -> return v
            NatCaseGT LeqProof -> error "impossible"
          let recover' evalFn = constr <$> evalFn v
          return (extended, T.WrappedRecoverOperandFn recover')

instance T.TemplatableOperand PPC where
  opTemplates sr =
    case sr of
      PPC.VsrcRepr -> concreteTemplatedOperand (PPC.Vsrc . PPC.VSReg) (LocVSR . PPC.VSReg) <$> [0..31]
      PPC.S17imm64Repr -> [symbolicTemplatedOperand (Proxy @16) True "S17imm64" (PPC.S17imm64 . fromInteger)]
      PPC.MemrrRepr -> mkTemplate <$> [0..31] <*> [0..31]
            where mkTemplate gprNum gprOffset =
                      T.TemplatedOperand Nothing (Set.fromList [Some (LocGPR (PPC.GPR gprNum)),Some (LocGPR (PPC.GPR gprOffset))]) mkTemplate' :: T.TemplatedOperand PPC "Memrr"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memrr"
                          mkTemplate' sym locLookup = do
                            base <- A.unTagged <$> A.operandValue (Proxy @PPC) sym locLookup (PPC.Gprc_nor0 (PPC.GPR gprNum))
                            offset <- A.unTagged <$> A.operandValue (Proxy @PPC) sym locLookup (PPC.Gprc_nor0 (PPC.GPR gprOffset))
                            expr <- S.bvAdd sym base offset
                            let recover = const $ do
                                       let gpr | gprNum /= 0 = Just (PPC.GPR gprNum)
                                               | otherwise = Nothing
                                       return $ PPC.Memrr $ PPC.MemRR gpr (PPC.GPR gprOffset)
                            return (expr, T.WrappedRecoverOperandFn recover)
      PPC.MemrixRepr ->
         mkTemplate <$> [0..31]
            where mkTemplate gprNum = T.TemplatedOperand Nothing (Set.singleton (Some (LocGPR (PPC.GPR gprNum)))) mkTemplate' :: T.TemplatedOperand PPC "Memrix"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memrix"
                          mkTemplate' sym locLookup = do
                            base <- A.unTagged <$> A.operandValue (Proxy @PPC) sym locLookup (PPC.Gprc_nor0 (PPC.GPR gprNum))
                            offset <- S.freshConstant sym (U.makeSymbol "Memrix_off") knownRepr
                            expr <- S.bvAdd sym base offset
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  let gpr
                                        | gprNum /= 0 = Just (PPC.GPR gprNum)
                                        | otherwise = Nothing
                                  return $ PPC.Memrix $ PPC.MemRIX gpr offsetVal
                            return (expr, T.WrappedRecoverOperandFn recover)
      PPC.Memrix16Repr ->
         mkTemplate <$> [0..31]
            where mkTemplate gprNum = T.TemplatedOperand Nothing (Set.singleton (Some (LocGPR (PPC.GPR gprNum)))) mkTemplate' :: T.TemplatedOperand PPC "Memrix16"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memrix16"
                          mkTemplate' sym locLookup = do
                            base <- A.unTagged <$> A.operandValue (Proxy @PPC) sym locLookup (PPC.Gprc_nor0 (PPC.GPR gprNum))
                            offset <- S.freshConstant sym (U.makeSymbol "Memrix16_off") knownRepr
                            expr <- S.bvAdd sym base offset
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  let gpr
                                        | gprNum /= 0 = Just (PPC.GPR gprNum)
                                        | otherwise = Nothing
                                  return $ PPC.Memrix16 $ PPC.MemRIX gpr offsetVal
                            return (expr, T.WrappedRecoverOperandFn recover)
      PPC.VrrcRepr -> concreteTemplatedOperand (PPC.Vrrc . PPC.VR) (LocVSR . PPC.VSReg) <$> [0..31]
      PPC.U4immRepr -> [symbolicTemplatedOperand (Proxy @4) True "U4imm" (PPC.U4imm . fromInteger)]
      PPC.U7immRepr -> [symbolicTemplatedOperand (Proxy @7) True "U7imm" (PPC.U7imm . fromInteger)]
      PPC.U8immRepr -> [symbolicTemplatedOperand (Proxy @8) True "U8imm" (PPC.U8imm . fromInteger)]
      PPC.U10immRepr->  [symbolicTemplatedOperand (Proxy @10) True "U10imm" (PPC.U10imm . fromInteger)]
      PPC.S5immRepr -> [symbolicTemplatedOperand (Proxy @5) True "S5imm" (PPC.S5imm . fromInteger)]
      PPC.U1immRepr -> [symbolicTemplatedOperand (Proxy @1) True "U1imm" (PPC.U1imm . fromInteger)]
      PPC.U2immRepr ->  [symbolicTemplatedOperand (Proxy @2) True "U2imm" (PPC.U2imm . fromInteger)]
      PPC.AbscondbrtargetRepr ->
         [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Abscondbrtarget"
                    mkDirect sym _ = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Abscondbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 14))
                      let recover evalFn =
                            PPC.Abscondbrtarget . PPC.mkAbsCondBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (offsetRaw, T.WrappedRecoverOperandFn recover)
      PPC.CondbrtargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Condbrtarget"
                    mkDirect sym _locLookup = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Condbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 14))
                      let recover evalFn =
                            PPC.Condbrtarget . PPC.mkCondBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (offsetRaw, T.WrappedRecoverOperandFn recover)

      PPC.CrbitmRepr ->
        [T.TemplatedOperand Nothing Set.empty mkDirect]
               where mkDirect :: T.TemplatedOperandFn PPC "Crbitm"
                     mkDirect sym _ = do
                       crrc <- S.freshConstant sym (U.makeSymbol "Crbitm") (knownRepr :: BaseTypeRepr (BaseBVType 8))
                       let recover evalFn =
                             PPC.Crbitm . PPC.CRBitM . fromInteger <$> evalFn crrc
                       return (crrc, T.WrappedRecoverOperandFn recover)
      PPC.I1immRepr ->
        [symbolicTemplatedOperand (Proxy @1) True "I1imm" (PPC.I1imm . fromInteger)]
      PPC.FprcRepr -> concreteTemplatedOperand (PPC.Fprc . PPC.FR) (LocVSR . PPC.VSReg) <$> [0..31]
      PPC.GprcRepr -> concreteTemplatedOperand PPC.Gprc LocGPR . PPC.GPR <$> [0..31]
      PPC.Gprc_nor0Repr -> concreteTemplatedOperand PPC.Gprc_nor0 LocGPR . PPC.GPR <$> [0..31]
      PPC.S16immRepr -> [symbolicTemplatedOperand (Proxy @16) True "S16imm" (PPC.S16imm . fromInteger)]
      PPC.S16imm64Repr -> [symbolicTemplatedOperand (Proxy @16) True "S16imm64" (PPC.S16imm64 . fromInteger)]
      PPC.U16immRepr -> [symbolicTemplatedOperand (Proxy @16) False "U16imm" (PPC.U16imm . fromInteger)]
      PPC.U16imm64Repr -> [symbolicTemplatedOperand (Proxy @16) False "U16imm64" (PPC.U16imm64 . fromInteger)]
      PPC.MemriRepr ->
          mkTemplate <$> [0..31]
            where mkTemplate gprNum = T.TemplatedOperand Nothing (Set.singleton (Some (LocGPR (PPC.GPR gprNum)))) mkTemplate' :: T.TemplatedOperand PPC "Memri"
                    where mkTemplate' :: T.TemplatedOperandFn PPC "Memri"
                          mkTemplate' sym locLookup = do
                            base <- A.unTagged <$> A.operandValue (Proxy @PPC) sym locLookup (PPC.Gprc_nor0 (PPC.GPR gprNum))
                            offset <- S.freshConstant sym (U.makeSymbol "Memri_off") knownRepr
                            expr <- S.bvAdd sym base offset
                            let recover evalFn = do
                                  offsetVal <- fromInteger <$> evalFn offset
                                  let gpr
                                        | gprNum /= 0 = Just (PPC.GPR gprNum)
                                        | otherwise = Nothing
                                  return $ PPC.Memri $ PPC.MemRI gpr offsetVal
                            return (expr, T.WrappedRecoverOperandFn recover)
      PPC.DirectbrtargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Directbrtarget"
                    mkDirect sym _locLookup = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Directbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                      let recover evalFn =
                            PPC.Directbrtarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (offsetRaw, T.WrappedRecoverOperandFn recover)
      PPC.U5immRepr -> [symbolicTemplatedOperand (Proxy @5) False "U5imm" (PPC.U5imm . fromInteger)]
      PPC.U6immRepr -> [symbolicTemplatedOperand (Proxy @6) False "U6imm" (PPC.U6imm . fromInteger)]
      PPC.S17immRepr ->
                    [T.TemplatedOperand Nothing Set.empty mkImm]
              where mkImm :: T.TemplatedOperandFn PPC "S17imm"
                    mkImm sym _ = do
                      v <- S.freshConstant sym (U.makeSymbol "S17imm") (knownRepr :: BaseTypeRepr (BaseBVType 16))
                      let recover evalFn = PPC.S17imm . fromInteger <$> evalFn v
                      return (v, T.WrappedRecoverOperandFn recover)
      PPC.AbsdirectbrtargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Absdirectbrtarget"
                    mkDirect sym _ = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Absdirectbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                      let recover evalFn =
                            PPC.Absdirectbrtarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (offsetRaw, T.WrappedRecoverOperandFn recover)
      PPC.CalltargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Calltarget"
                    mkDirect sym _locLookup = do
                      offsetRaw <- S.freshConstant sym (U.makeSymbol "Calltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                      let recover evalFn =
                            PPC.Calltarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
                      return (offsetRaw, T.WrappedRecoverOperandFn recover)
      PPC.AbscalltargetRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
               where mkDirect :: T.TemplatedOperandFn PPC "Abscalltarget"
                     mkDirect sym _ = do
                       offsetRaw <- S.freshConstant sym (U.makeSymbol "Abscalltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
                       let recover evalFn =
                             PPC.Abscalltarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
                       return (offsetRaw, T.WrappedRecoverOperandFn recover)
      PPC.CrrcRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
              where mkDirect :: T.TemplatedOperandFn PPC "Crrc"
                    mkDirect sym _ = do
                      crrc <- S.freshConstant sym (U.makeSymbol "Crrc") (knownRepr :: BaseTypeRepr (BaseBVType 3))
                      let recover evalFn =
                            PPC.Crrc . PPC.CRRC . fromInteger <$> evalFn crrc
                      return (crrc, T.WrappedRecoverOperandFn recover)
      PPC.CrbitrcRepr ->
            [T.TemplatedOperand Nothing Set.empty mkDirect]
               where mkDirect :: T.TemplatedOperandFn PPC "Crbitrc"
                     mkDirect sym _ = do
                       crrc <- S.freshConstant sym (U.makeSymbol "Crbitrc") (knownRepr :: BaseTypeRepr (BaseBVType 5))
                       let recover evalFn =
                             PPC.Crbitrc . PPC.CRBitRC . fromInteger <$> evalFn crrc
                       return (crrc, T.WrappedRecoverOperandFn recover)
      PPC.I32immRepr ->
            [T.TemplatedOperand Nothing Set.empty mkImm]
              where mkImm :: T.TemplatedOperandFn PPC "I32imm"
                    mkImm sym _ = do
                      v <- S.freshConstant sym (U.makeSymbol "I32imm") knownRepr
                      let recover' evalFn = PPC.I32imm . fromInteger <$> evalFn v
                      return (v, T.WrappedRecoverOperandFn recover')

type instance A.Location PPC = Location PPC

instance A.IsOperandTypeRepr PPC where
  type OperandTypeRepr PPC = PPC.OperandRepr
  operandTypeReprSymbol _ = PPC.operandReprString

operandValue :: forall sym s.
                (S.IsSymInterface sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location PPC tp -> IO (S.SymExpr sym tp))
             -> PPC.Operand s
             -> IO (A.TaggedExpr PPC sym s)
operandValue sym locLookup op = TaggedExpr <$> operandValue' op
  where operandValue' :: PPC.Operand s -> IO (S.SymExpr sym (A.OperandType PPC s))
        operandValue' (PPC.Abscalltarget (PPC.ABT absTarget)) =
          S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Abscondbrtarget (PPC.ACBT absTarget)) =
          S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Absdirectbrtarget (PPC.ABT absTarget)) =
          S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Calltarget (PPC.BT off)) =
          S.bvLit sym knownNat (toInteger off)
        operandValue' (PPC.Condbrtarget (PPC.CBT off)) =
          S.bvLit sym knownNat (toInteger off)
        operandValue' (PPC.Crbitm (PPC.CRBitM n)) =
          S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Crbitrc (PPC.CRBitRC n)) =
          S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Crrc (PPC.CRRC n)) =
          S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Directbrtarget (PPC.BT off)) =
          S.bvLit sym knownNat (toInteger off)
        operandValue' (PPC.Fprc (PPC.FR fr)) = locLookup (LocVSR (PPC.VSReg fr))
        operandValue' (PPC.Gprc gpr) = locLookup (LocGPR gpr)
        operandValue' (PPC.Gprc_nor0 (PPC.GPR gpr)) =
          if gpr /= 0
          then locLookup (LocGPR (PPC.GPR gpr))
          else S.bvLit sym knownNat 0
        operandValue' (PPC.I1imm (I.I x)) = S.bvLit sym knownNat (toInteger x)
        operandValue' (PPC.I32imm (I.I x)) = S.bvLit sym knownNat (toInteger x)
        operandValue' (PPC.Memri (PPC.MemRI gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (LocGPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Memrix (PPC.MemRIX gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (LocGPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger (I.unI offset))
          S.bvAdd sym base offset'
        operandValue' (PPC.Memrix16 (PPC.MemRIX gpr offset)) = do
          -- ?
          base <- case gpr of
                    Just gpr' -> locLookup (LocGPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger (I.unI offset))
          S.bvAdd sym base offset'
        operandValue' (PPC.Memrr (PPC.MemRR gpr1 gpr2)) = do
          gpr1Val <- case gpr1 of
                       Just gpr -> locLookup (LocGPR gpr)
                       Nothing -> S.bvLit sym knownNat 0
          gpr2Val <- locLookup (LocGPR gpr2)
          S.bvAdd sym gpr1Val gpr2Val
        operandValue' (PPC.S16imm i16) = S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S16imm64 i16) = S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S17imm i16) =
          -- The s17 imm type is a bit strange.  It is actually represented as
          -- 16 bits in the instruction, but is interpreted as shifted left by
          -- 16 bits.  We handle that correction in the semantics, so we just
          -- treat s17imm as a plain 16 bit value.
          S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S17imm64 i16) =
          S.bvLit sym knownNat (toInteger i16)
        operandValue' (PPC.S5imm (I.I i5)) = S.bvLit sym knownNat (toInteger i5)
        operandValue' (PPC.U10imm (W.unW ->  w10)) =
          S.bvLit sym knownNat (toInteger w10)
        operandValue' (PPC.U16imm (W.unW ->  w16)) =
          S.bvLit sym knownNat (toInteger w16)
        operandValue' (PPC.U16imm64 (W.unW ->  w16)) =
          S.bvLit sym knownNat (toInteger w16)
        operandValue' (PPC.U1imm (W.unW ->  w1)) =
          S.bvLit sym knownNat (toInteger w1)
        operandValue' (PPC.U2imm (W.unW ->  w2)) =
          S.bvLit sym knownNat (toInteger w2)
        operandValue' (PPC.U4imm (W.unW ->  w4)) =
          S.bvLit sym knownNat (toInteger w4)
        operandValue' (PPC.U5imm (W.unW ->  w5)) =
          S.bvLit sym knownNat (toInteger w5)
        operandValue' (PPC.U6imm (W.unW ->  w6)) =
          S.bvLit sym knownNat (toInteger w6)
        operandValue' (PPC.U7imm (W.unW ->  w7)) =
          S.bvLit sym knownNat (toInteger w7)
        operandValue' (PPC.U8imm (W.unW ->  w8)) =
          S.bvLit sym knownNat (toInteger w8)
        operandValue' (PPC.Vrrc (PPC.VR vr)) = locLookup (LocVSR (PPC.VSReg (vr + 32)))
        operandValue' (PPC.Vsrc vsr) = locLookup (LocVSR vsr)


operandToLocation :: PPC.Operand s -> Maybe (Location PPC (A.OperandType PPC s))
operandToLocation (PPC.Fprc (PPC.FR fr)) = Just $ LocVSR (PPC.VSReg fr)
operandToLocation (PPC.Gprc gpr) = Just $ LocGPR gpr
operandToLocation (PPC.Gprc_nor0 gpr) = Just (LocGPR gpr)
operandToLocation (PPC.Vrrc (PPC.VR vr)) = Just $ LocVSR (PPC.VSReg (vr + 32))
operandToLocation (PPC.Vsrc vr) = Just $ LocVSR vr
operandToLocation _ = Nothing

instance A.Architecture PPC where
  data TaggedExpr PPC sym s = TaggedExpr (S.SymExpr sym (A.OperandType PPC s))
  unTagged (TaggedExpr e) = e
  operandValue _ = operandValue
  operandToLocation _ = operandToLocation
  uninterpretedFunctions = UF.uninterpretedFunctions
  locationFuncInterpretation _proxy = createSymbolicEntries locationFuncInterpretation
  shapeReprToTypeRepr _proxy = shapeReprType

type IsOffset = Bool

evalMemReg :: IsOffset -> E.Evaluator PPC t
evalMemReg isOffset = E.Evaluator (handler isOffset)

handler
  :: forall t st sh u tp . Bool
  -> S.SimpleBuilder t st
  -> F.ParameterizedFormula (S.SimpleBuilder t st) PPC sh
  -> PPC.List (A.Operand PPC) sh
  -> Assignment (S.Elt t) u
  -> BaseTypeRepr tp
  -> IO (S.Elt t tp)
handler isOffset sym pf operands assignment bv =
  case assignment of
    Empty :> S.BoundVarElt b ->
      case Some b `boundVarElemIndex` stripped pf of
        Nothing -> error "BoundVar not present in ParameterizedFormula"
        Just index ->
          case findOperandByIndex index operands of
            Some reg ->
              case reg of
                PPC.Memri (PPC.MemRI maybeBase offset) ->
                  if isOffset
                    then handleOffset (fromIntegral offset) (knownNat :: NatRepr 16)
                    else handleBase maybeBase
                PPC.Memrix (PPC.MemRIX maybeBase (I.I offset)) ->
                  if isOffset
                    then handleOffset (fromIntegral offset) (knownNat :: NatRepr 14)
                    else handleBase maybeBase
                PPC.Memrr (PPC.MemRR maybeBase offset) ->
                  if isOffset
                    then handleBase (Just offset)
                    else handleBase maybeBase
                _ -> error "Unexpected operand type"
         where
           handleOffset
             :: ( KnownNat n
                , 1 <= n
                ) => Integer
                  -> NatRepr n
                  -> IO (S.Elt t tp)
           handleOffset offset natRepr = do
             s <- S.bvLit sym natRepr offset
             case S.exprType s `testEquality` bv of
               Just Refl -> pure s
               Nothing -> error "Couldn't unify offset types"
           handleBase maybeBase =
             case maybeBase of
               Nothing ->
                 findAtRegister 0x0 (U.makeSymbol "r0")
               Just (PPC.GPR base) ->
                 findAtRegister base $ U.makeSymbol $ "r" ++ show base
           findAtRegister reg symbol =
             case MapF.lookup (LocGPR (PPC.GPR reg)) (F.pfLiteralVars pf) of
               Nothing ->
                 case findReg reg operands pf of
                   Just (Some (BoundVar.BoundVar k)) ->
                     case testEquality bv (S.bvarType k) of
                       Just Refl -> pure $ S.varExpr sym k
                       _ -> error "Type equality failure"
                   Nothing ->
                     S.BoundVarElt <$>
                       S.freshBoundVar sym symbol bv
               Just k ->
                 case testEquality bv (S.bvarType k) of
                   Just Refl -> pure $ S.varExpr sym k
                   _ -> error "Type equality failure"

stripped
  :: F.ParameterizedFormula sym arch c
  -> [Some (BoundVar.BoundVar sym arch)]
stripped pf = toListFC Some (F.pfOperandVars pf)

findOperandByIndex
  :: Int
  -> PPC.List PPC.Operand sh
  -> Some PPC.Operand
findOperandByIndex index list =
  fromMaybe (error "Find operand by index") $
    lookup index $
      Prelude.zip [0..] (toListFC Some list)

findReg
  :: (FoldableFC t, TestEquality (S.BoundVar sym))
  => Word8
  -> t PPC.Operand c2
  -> F.ParameterizedFormula sym arch c2
  -> Maybe (Some (BoundVar.BoundVar sym arch))
findReg b list pf = do
  index <- getIndex
  lookup index $ zip [0..] $
    toListFC Some (F.pfOperandVars pf)
      where
        getIndex = go (toListFC Some list) 0
          where
            go :: [Some PPC.Operand] -> Int -> Maybe Int
            go [] _ = Nothing
            go (Some x : xs) n =
              case x of
                PPC.Gprc (PPC.GPR base) | base == b -> Just n
                _ -> go xs (n+1)

boundVarElemIndex
  :: TestEquality (S.BoundVar sym)
  => Some (S.BoundVar sym)
  -> [Some (BoundVar.BoundVar sym PPC)]
  -> Maybe Int
boundVarElemIndex x xs = do
  let xs' = [ Some y | Some (BoundVar.BoundVar y) <- xs ]
  x `elemIndex` xs'

locationFuncInterpretation :: [(String, A.FunctionInterpretation t PPC sh)]
locationFuncInterpretation =
  [ ("ppc.memri_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpMemriReg
                                               , A.exprInterpName = 'interpMemriRegExtractor
                                               , A.exprInterp = evalMemReg False
                                               })
  , ("ppc.memrix_reg", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpMemrixReg
                                                , A.exprInterpName = 'interpMemrixRegExtractor
                                                , A.exprInterp = evalMemReg False
                                                })
  , ("ppc.memrr_base", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpMemrrBase
                                                , A.exprInterpName = 'interpMemrrBaseExtractor
                                                , A.exprInterp = evalMemReg False
                                                })
  , ("ppc.memrr_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp interpMemrrOffset
                                                  , A.exprInterpName = 'interpMemrrOffsetExtractor
                                                  , A.exprInterp = evalMemReg True
                                                  })
  , ("ppc.memrix_offset", A.FunctionInterpretation { A.locationInterp = F.LocationFuncInterp undefined
                                                   , A.exprInterpName = 'interpMemrixOffsetExtractor
                                                   , A.exprInterp = evalMemReg True
                                                   })
  , ("ppc.memri_offset", A.FunctionInterpretation { A.locationInterp = undefined
                                                  , A.exprInterpName = 'interpMemriOffsetExtractor
                                                  , A.exprInterp = evalMemReg True
                                                  })
  , ("ppc.is_r0", A.FunctionInterpretation { A.exprInterpName = 'interpIsR0
                                           , A.exprInterp = undefined
                                           , A.locationInterp = undefined
                                           })
    ]

-- , pfOperandVars :: SL.List (BV.BoundVar sym arch) sh

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

  allLocations = concat
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
      , Some LocMem
      ]
    ]

  registerizationLocations = map (Some . LocGPR . PPC.GPR) (0 : [3..10])

parseLocation :: PPCS.Parser (Some (Location PPC))
parseLocation = do
  c <- P.lookAhead (P.anyChar)
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
