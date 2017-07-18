{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Architecture.PPC
  ( PPC
  , Location(..)
  , loadBaseSet
  ) where

import           Data.Bits ( shiftL )
import           Data.EnumF ( EnumF(..) )
import           Data.Foldable ( foldrM )
import           Data.Int ( Int32 )
import qualified Data.Int.Indexed as I
import           Data.Monoid ( (<>) )
import qualified Data.Parameterized.Ctx as Ctx
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Data.Parameterized.Some
import           Data.Proxy ( Proxy(..) )
import           Data.Void ( absurd, Void )
import qualified Data.Word.Indexed as W
import           Text.PrettyPrint.HughesPJClass ( pPrint )
import           Text.Printf ( printf )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture as A
import           SemMC.Formula
import           SemMC.Formula.Parser ( BuildOperandList, readFormulaFromFile )
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch, TemplatedOperandFn, TemplatableOperand(..), TemplatedOperand(..), WrappedRecoverOperandFn(..), TemplatableOperands )
import           SemMC.Util ( makeSymbol, Equal, Witness(..) )

data PPC

type instance A.Operand PPC = PPC.Operand

instance A.IsOperand PPC.Operand

type instance A.Opcode PPC = PPC.Opcode

instance A.IsOpcode PPC.Opcode

-- Assuming 32-bit PPC for now.
type instance A.OperandType PPC "Abscalltarget" = BaseBVType 32
type instance A.OperandType PPC "Abscondbrtarget" = BaseBVType 32
type instance A.OperandType PPC "Absdirectbrtarget" = BaseBVType 32
type instance A.OperandType PPC "Calltarget" = BaseBVType 32
type instance A.OperandType PPC "Condbrtarget" = BaseBVType 32
type instance A.OperandType PPC "Crbitm" = BaseBVType 3
type instance A.OperandType PPC "Crbitrc" = BaseBVType 5
type instance A.OperandType PPC "Crrc" = BaseBVType 3
type instance A.OperandType PPC "Directbrtarget" = BaseBVType 32
type instance A.OperandType PPC "F4rc" = BaseBVType 32
type instance A.OperandType PPC "F8rc" = BaseBVType 64
type instance A.OperandType PPC "F8rc" = BaseBVType 64
type instance A.OperandType PPC "G8rc" = BaseBVType 64
type instance A.OperandType PPC "G8rc_nox0" = BaseBVType 64
type instance A.OperandType PPC "Gprc" = BaseBVType 32
type instance A.OperandType PPC "Gprc_nor0" = BaseBVType 32
type instance A.OperandType PPC "I1imm" = BaseBVType 1
type instance A.OperandType PPC "I32imm" = BaseBVType 32
type instance A.OperandType PPC "Memri" = BaseBVType 32
type instance A.OperandType PPC "Memrix" = BaseBVType 32
type instance A.OperandType PPC "Memrix16" = BaseBVType 32
type instance A.OperandType PPC "Memrr" = BaseBVType 32
-- Yes, "S16" is supposed to be 32 bits. See the tgen file.
type instance A.OperandType PPC "S16imm" = BaseBVType 32
type instance A.OperandType PPC "S16imm64" = BaseBVType 64
type instance A.OperandType PPC "S17imm" = BaseBVType 32
type instance A.OperandType PPC "S5imm" = BaseBVType 32
type instance A.OperandType PPC "Spe2dis" = BaseBVType 32
type instance A.OperandType PPC "Spe4dis" = BaseBVType 32
type instance A.OperandType PPC "Spe8dis" = BaseBVType 32
type instance A.OperandType PPC "Spe8dis" = BaseBVType 32
type instance A.OperandType PPC "Tlscall" = BaseBVType 64
type instance A.OperandType PPC "Tlscall32" = BaseBVType 32
type instance A.OperandType PPC "Tlsreg" = BaseBVType 64
type instance A.OperandType PPC "Tlsreg32" = BaseBVType 32
type instance A.OperandType PPC "U10imm" = BaseBVType 32
type instance A.OperandType PPC "U16imm" = BaseBVType 32
type instance A.OperandType PPC "U16imm64" = BaseBVType 64
type instance A.OperandType PPC "U1imm" = BaseBVType 32
type instance A.OperandType PPC "U2imm" = BaseBVType 32
type instance A.OperandType PPC "U4imm" = BaseBVType 32
type instance A.OperandType PPC "U5imm" = BaseBVType 32
type instance A.OperandType PPC "U6imm" = BaseBVType 32
type instance A.OperandType PPC "U7imm" = BaseBVType 32
type instance A.OperandType PPC "U8imm" = BaseBVType 32
type instance A.OperandType PPC "Vrrc" = BaseBVType 128
type instance A.OperandType PPC "Vsfrc" = BaseBVType 128
type instance A.OperandType PPC "Vsrc" = BaseBVType 128
type instance A.OperandType PPC "Vssrc" = BaseBVType 128

concreteTemplatedOperand :: forall arch s a.
                            (A.Architecture arch)
                         => (a -> A.Operand arch s)
                         -> (a -> A.Location arch (A.OperandType arch s))
                         -> a
                         -> TemplatedOperand arch s
concreteTemplatedOperand op loc x = TemplatedOperand (Just (loc x)) mkTemplate' :: TemplatedOperand arch s
  where mkTemplate' :: TemplatedOperandFn arch s
        mkTemplate' sym locLookup = do
          expr <- A.unTagged <$> A.operandValue (Proxy @arch) sym locLookup (op x)
          return (expr, WrappedRecoverOperandFn $ const (return (op x)))

instance TemplatableOperand PPC "Gprc" where
  opTemplates = concreteTemplatedOperand PPC.Gprc LocGPR . PPC.GPR <$> [0..31]

instance TemplatableOperand PPC "Gprc_nor0" where
  opTemplates = concreteTemplatedOperand PPC.Gprc_nor0 LocGPR . PPC.GPR <$> [0..31]

instance TemplatableOperand PPC "S16imm" where
  opTemplates = [TemplatedOperand Nothing mkConst]
    where mkConst :: TemplatedOperandFn PPC "S16imm"
          mkConst sym _ = do
            v <- S.freshConstant sym (makeSymbol "S16imm") (knownRepr :: BaseTypeRepr (BaseBVType 16))
            extended <- S.bvSext sym knownNat v
            let recover evalFn = PPC.S16imm . fromInteger <$> evalFn v
            return (extended, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "Memri" where
  opTemplates = mkTemplate <$> [0..31]
    where mkTemplate gprNum = TemplatedOperand Nothing mkTemplate' :: TemplatedOperand PPC "Memri"
            where mkTemplate' :: TemplatedOperandFn PPC "Memri"
                  mkTemplate' sym locLookup = do
                    base <- A.unTagged <$> A.operandValue (Proxy @PPC) sym locLookup (PPC.Gprc_nor0 (PPC.GPR gprNum))
                    offset <- S.freshConstant sym (makeSymbol "Memri_off") knownRepr
                    expr <- S.bvAdd sym base offset
                    let recover evalFn = do
                          offsetVal <- fromInteger <$> evalFn offset
                          let gpr
                                | gprNum /= 0 = Just (PPC.GPR gprNum)
                                | otherwise = Nothing
                          return $ PPC.Memri $ PPC.MemRI gpr offsetVal
                    return (expr, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "Directbrtarget" where
  opTemplates = [TemplatedOperand Nothing mkDirect]
    where mkDirect :: TemplatedOperandFn PPC "Directbrtarget"
          mkDirect sym locLookup = do
            ip <- locLookup LocIP
            offsetRaw <- S.freshConstant sym (makeSymbol "Directbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            expr <- S.bvAdd sym ip extended
            let recover evalFn =
                  PPC.Directbrtarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
            return (expr, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "U5imm" where
  opTemplates = [TemplatedOperand Nothing mkImm]
    where mkImm :: TemplatedOperandFn PPC "U5imm"
          mkImm sym _ = do
            v <- S.freshConstant sym (makeSymbol "U5imm") (knownRepr :: BaseTypeRepr (BaseBVType 5))
            extended <- S.bvSext sym knownNat v
            let recover evalFn = PPC.U5imm . fromInteger <$> evalFn v
            return (extended, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "S17imm" where
  opTemplates = [TemplatedOperand Nothing mkImm]
    where mkImm :: TemplatedOperandFn PPC "S17imm"
          mkImm sym _ = do
            v <- S.freshConstant sym (makeSymbol "S17imm") (knownRepr :: BaseTypeRepr (BaseBVType 16))
            zeroes <- S.bvLit sym knownNat 0
            extended <- S.bvConcat sym v zeroes
            let recover evalFn = PPC.S17imm . fromInteger <$> evalFn v
            return (extended, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "Absdirectbrtarget" where
  opTemplates = [TemplatedOperand Nothing mkDirect]
    where mkDirect :: TemplatedOperandFn PPC "Absdirectbrtarget"
          mkDirect sym _ = do
            offsetRaw <- S.freshConstant sym (makeSymbol "Absdirectbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            let recover evalFn =
                  PPC.Absdirectbrtarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
            return (extended, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "Calltarget" where
  opTemplates = [TemplatedOperand Nothing mkDirect]
    where mkDirect :: TemplatedOperandFn PPC "Calltarget"
          mkDirect sym locLookup = do
            ip <- locLookup LocIP
            offsetRaw <- S.freshConstant sym (makeSymbol "Calltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            expr <- S.bvAdd sym ip extended
            let recover evalFn =
                  PPC.Calltarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
            return (expr, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "Abscalltarget" where
  opTemplates = [TemplatedOperand Nothing mkDirect]
    where mkDirect :: TemplatedOperandFn PPC "Abscalltarget"
          mkDirect sym _ = do
            offsetRaw <- S.freshConstant sym (makeSymbol "Abscalltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            let recover evalFn =
                  PPC.Abscalltarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
            return (extended, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "Crrc" where
  opTemplates = [TemplatedOperand Nothing mkDirect]
    where mkDirect :: TemplatedOperandFn PPC "Crrc"
          mkDirect sym _ = do
            crrc <- S.freshConstant sym (makeSymbol "Crrc") (knownRepr :: BaseTypeRepr (BaseBVType 3))
            let recover evalFn =
                  PPC.Crrc . PPC.CRRC . fromInteger <$> evalFn crrc
            return (crrc, WrappedRecoverOperandFn recover)

instance TemplatableOperand PPC "I32imm" where
  -- XXX: What to do here? seems very instruction-specific
  opTemplates = [TemplatedOperand Nothing mkImm]
    where mkImm :: TemplatedOperandFn PPC "I32imm"
          mkImm sym _ = do
            v <- S.freshConstant sym (makeSymbol "I32imm") knownRepr
            let recover evalFn = PPC.I32imm . fromInteger <$> evalFn v
            return (v, WrappedRecoverOperandFn recover)

data Location :: BaseType -> * where
  LocGPR :: PPC.GPR -> Location (BaseBVType 32)
  LocIP :: Location (BaseBVType 32)
  LocMSR :: Location (BaseBVType 32)
  LocCTR :: Location (BaseBVType 32)
  LocLNK :: Location (BaseBVType 32)
  LocXER :: Location (BaseBVType 32)
  LocCR :: Location (BaseBVType 32)
  LocFR :: PPC.FR -> Location (BaseBVType 64)
  LocVR :: PPC.VR -> Location (BaseBVType 128)
  LocMem :: Location (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8))

instance Show (Location tp) where
  show (LocGPR gpr) = show (pPrint gpr)
  show LocIP = "IP"
  show LocMSR = "MSR"
  show LocCTR = "CTR"
  show LocLNK = "LNK"
  show LocXER = "XER"
  show LocCR = "CR"
  show (LocFR fr) = show (pPrint fr)
  show (LocVR vr) = show (pPrint vr)
  show LocMem = "Mem"
instance ShowF Location

$(return [])

fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
fakeTestEq x y = if x == y
                 then Just Refl
                 else Nothing

instance TestEquality Location where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [ (ConType [t|PPC.GPR|], [|fakeTestEq|])
                   , (ConType [t|PPC.FR|], [|fakeTestEq|])
                   , (ConType [t|PPC.VR|], [|fakeTestEq|])
                   ]
                  )

fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
fakeCompareF x y = fromOrdering (compare x y)

instance OrdF Location where
  compareF = $(structuralTypeOrd [t|Location|]
               [ (ConType [t|PPC.GPR|], [|fakeCompareF|])
               , (ConType [t|PPC.FR|], [|fakeCompareF|])
               , (ConType [t|PPC.VR|], [|fakeCompareF|])
               ]
              )

instance A.IsLocation Location where
  readLocation s
    | s `elem` ["r" ++ show i | i <- [(0 :: Int)..31]] =
      (Just . Some . LocGPR . PPC.GPR . read . tail) s
    | s == "ip" = Just (Some LocIP)
    | s == "msr" = Just (Some LocMSR)
    | s == "ctr" = Just (Some LocCTR)
    | s == "lnk" = Just (Some LocLNK)
    | s == "xer" = Just (Some LocXER)
    | s == "cr" = Just (Some LocCR)
    | s `elem` ["f" ++ show i | i <- [(0 :: Int)..31]] =
      (Just . Some . LocFR . PPC.FR . read . tail) s
    | s `elem` ["vr" ++ show i | i <- [(0 :: Int)..31]] =
      (Just . Some . LocVR . PPC.VR . read . tail . tail) s
    | s == "mem" = Just (Some LocMem)
    | otherwise = Nothing

  locationType (LocGPR _) = knownRepr
  locationType LocIP = knownRepr
  locationType LocMSR = knownRepr
  locationType LocCTR = knownRepr
  locationType LocLNK = knownRepr
  locationType LocXER = knownRepr
  locationType LocCR = knownRepr
  locationType (LocFR _) = knownRepr
  locationType (LocVR _) = knownRepr
  locationType LocMem = knownRepr

  defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocIP = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMSR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCTR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocLNK = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocXER = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocFR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocVR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMem =
    S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0

type instance A.Location PPC = Location

operandValue :: forall sym s.
                (S.IsSymInterface sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location tp -> IO (S.SymExpr sym tp))
             -> PPC.Operand s
             -> IO (A.TaggedExpr PPC sym s)
operandValue sym locLookup op = TaggedExpr <$> operandValue' op
  where operandValue' :: PPC.Operand s -> IO (S.SymExpr sym (A.OperandType PPC s))
        operandValue' (PPC.Abscalltarget (PPC.ABT absTarget)) =
          S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Abscondbrtarget (PPC.ABT absTarget)) =
          S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Absdirectbrtarget (PPC.ABT absTarget)) =
          S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.Calltarget bt) = btVal bt
        operandValue' (PPC.Condbrtarget bt) = btVal bt
        operandValue' (PPC.Crbitm (PPC.CRBitM n)) =
          S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Crbitrc (PPC.CRBitRC n)) =
          S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Crrc (PPC.CRRC n)) =
          S.bvLit sym knownNat (toInteger n)
        operandValue' (PPC.Directbrtarget bt) = btVal bt
        operandValue' (PPC.F4rc _) = error "F4rc not yet implemented"
        operandValue' (PPC.F8rc fr) = locLookup (LocFR fr)
        operandValue' (PPC.G8rc _) = error "Found a G8rc operand, but PPC64 not supported"
        operandValue' (PPC.G8rc_nox0 _) = error "Found a G8rc_nox0 operand, but PPC64 not supported"
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
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Memrix16 (PPC.MemRIX gpr offset)) = do
          -- ?
          base <- case gpr of
                    Just gpr' -> locLookup (LocGPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
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
          -- Though it's called an "S17", this appears to be the correct operation.
          let val = (fromIntegral i16 :: Int32) `shiftL` 16
          in S.bvLit sym knownNat (toInteger val)
        operandValue' (PPC.S5imm (I.I i5)) = S.bvLit sym knownNat (toInteger i5)
        operandValue' (PPC.Spe2dis (PPC.SPEDis gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (LocGPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Spe4dis (PPC.SPEDis gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (LocGPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Spe8dis (PPC.SPEDis gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (LocGPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Tlscall _) = error "Tlscall not implemented"
        operandValue' (PPC.Tlscall32 bt) = btVal bt
        operandValue' (PPC.Tlsreg _) = error "Tlsreg not implemented"
        operandValue' (PPC.Tlsreg32 gpr) = locLookup (LocGPR gpr)
        operandValue' (PPC.U10imm (W.W w10)) =
          S.bvLit sym knownNat (toInteger w10)
        operandValue' (PPC.U16imm (W.W w16)) =
          S.bvLit sym knownNat (toInteger w16)
        operandValue' (PPC.U16imm64 (W.W w16)) =
          S.bvLit sym knownNat (toInteger w16)
        operandValue' (PPC.U1imm (W.W w1)) =
          S.bvLit sym knownNat (toInteger w1)
        operandValue' (PPC.U2imm (W.W w2)) =
          S.bvLit sym knownNat (toInteger w2)
        operandValue' (PPC.U4imm (W.W w4)) =
          S.bvLit sym knownNat (toInteger w4)
        operandValue' (PPC.U5imm (W.W w5)) =
          S.bvLit sym knownNat (toInteger w5)
        operandValue' (PPC.U6imm (W.W w6)) =
          S.bvLit sym knownNat (toInteger w6)
        operandValue' (PPC.U7imm (W.W w7)) =
          S.bvLit sym knownNat (toInteger w7)
        operandValue' (PPC.U8imm (W.W w8)) =
          S.bvLit sym knownNat (toInteger w8)
        operandValue' (PPC.Vrrc vr) = locLookup (LocVR vr)
        operandValue' (PPC.Vsfrc vr) = locLookup (LocVR vr)
        operandValue' (PPC.Vsrc vr) = locLookup (LocVR vr)
        operandValue' (PPC.Vssrc vr) = locLookup (LocVR vr)

        btVal (PPC.BT bt) = do
          ip <- locLookup LocIP
          offset <- S.bvLit sym knownNat (toInteger bt)
          S.bvAdd sym ip offset

operandToLocation :: PPC.Operand s -> Maybe (Location (A.OperandType PPC s))
operandToLocation (PPC.F4rc _) = error "F4rc operandToLocation ?"
operandToLocation (PPC.F8rc fr) = Just $ LocFR fr
operandToLocation (PPC.G8rc _) = error "G8rc operandToLocation ?"
operandToLocation (PPC.G8rc_nox0 _) = error "G8rc_nox0 operandToLocation ?"
operandToLocation (PPC.Gprc gpr) = Just $ LocGPR gpr
operandToLocation (PPC.Tlsreg _) = error "Tlsreg operandToLocation?"
operandToLocation (PPC.Tlsreg32 gpr) = Just $ LocGPR gpr
operandToLocation (PPC.Vrrc vr) = Just $ LocVR vr
operandToLocation (PPC.Vsfrc vr) = Just $ LocVR vr
operandToLocation (PPC.Vsrc vr) = Just $ LocVR vr
operandToLocation (PPC.Vssrc vr) = Just $ LocVR vr
operandToLocation _ = Nothing

instance A.Architecture PPC where
  data TaggedExpr PPC sym s = TaggedExpr (S.SymExpr sym (A.OperandType PPC s))
  unTagged (TaggedExpr e) = e
  operandValue _ = operandValue
  operandToLocation _ = operandToLocation

opcodeToVoid :: (Equal o PPC.Operand ~ 'False) => PPC.Opcode o sh -> Void
opcodeToVoid x = case x of {}

-- This is a hack.
instance OrdF (PPC.Opcode (TemplatedOperand PPC)) where
  compareF = absurd . opcodeToVoid

instance ShowF (PPC.Opcode (TemplatedOperand PPC))

instance EnumF (PPC.Opcode (TemplatedOperand PPC)) where
  enumF = absurd . opcodeToVoid
  congruentF = absurd . opcodeToVoid

fromRight :: (Monad m) => Either String a -> m a
fromRight = either fail return

class (BuildOperandList (TemplatedArch PPC) sh, TemplatableOperands PPC sh) => Foo sh
instance (BuildOperandList (TemplatedArch PPC) sh, TemplatableOperands PPC sh) => Foo sh

loadBaseSet :: forall sym.
               (S.IsExprBuilder sym,
                S.IsSymInterface sym)
            => sym
            -> IO (BaseSet sym PPC)
loadBaseSet sym = do
  let readOp :: (BuildOperandList (TemplatedArch PPC) sh)
             => FilePath
             -> IO (Either String (ParameterizedFormula sym (TemplatedArch PPC) sh))
      readOp fp = readFormulaFromFile sym ("semmc-ppc/data/base/" <> fp)
      addOp :: Some (Witness Foo (PPC.Opcode PPC.Operand))
            -> BaseSet sym PPC
            -> IO (BaseSet sym PPC)
      addOp (Some (Witness op)) m = do
        printf "reading op %s\n" (showF op)
        op' <- fromRight =<< readOp (showF op <> ".sem")
        return $ MapF.insert (Witness op) op' m
  foldrM addOp MapF.empty [ Some (Witness PPC.ADD4)
                          , Some (Witness PPC.ADDI)
                          , Some (Witness PPC.ADDIS)
                          , Some (Witness PPC.B)
                          , Some (Witness PPC.BA)
                          -- , Some (Witness PPC.BC)
                          -- , Some (Witness PPC.BCA)
                          -- , Some (Witness PPC.BCL)
                          -- , Some (Witness PPC.BCLA)
                          , Some (Witness PPC.BL)
                          , Some (Witness PPC.BLA)
                          , Some (Witness PPC.CMPLW)
                          , Some (Witness PPC.CMPW)
                          , Some (Witness PPC.LBZ)
                          , Some (Witness PPC.MFCR)
                          , Some (Witness PPC.MFSPR)
                          , Some (Witness PPC.MTCRF)
                          , Some (Witness PPC.MTSPR)
                          , Some (Witness PPC.STB)
                          , Some (Witness PPC.SUBF)
                          ]
