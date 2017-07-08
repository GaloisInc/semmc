{-# LANGUAGE DataKinds #-}
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
module SemMC.Architecture.PPC
  ( PPC
  , Location(..)
  , loadBaseSet
  ) where

import           Data.Bits ( shiftL )
import           Data.EnumF ( EnumF(..) )
import           Data.Int ( Int32 )
import qualified Data.Int.Indexed as I
import           Data.Monoid ( (<>) )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Data.Parameterized.Some
import           Data.Proxy ( Proxy(..) )
import qualified Data.Word.Indexed as W
import           GHC.TypeLits ( KnownSymbol, sameSymbol )
import           Text.PrettyPrint.HughesPJClass ( pPrint )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval ( GroundValue )

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture as A
import           SemMC.Formula.Parser ( readFormulaFromFile )
import           SemMC.Synthesis.Template ( BaseSet, TemplatedOperand )
import           SemMC.Util ( Witness(..) )

data PPC

type instance A.Operand PPC = PPC.Operand

-- TODO: rename IsReg to IsImm and negate everything
type instance A.IsReg PPC "Abscalltarget" = 'False
type instance A.IsReg PPC "Abscondbrtarget" = 'False
type instance A.IsReg PPC "Absdirectbrtarget" = 'False
type instance A.IsReg PPC "Calltarget" = 'False
type instance A.IsReg PPC "Condbrtarget" = 'False
type instance A.IsReg PPC "Crbitm" = 'False
type instance A.IsReg PPC "Crbitrc" = 'False
type instance A.IsReg PPC "Crrc" = 'False
type instance A.IsReg PPC "Directbrtarget" = 'False
type instance A.IsReg PPC "F4rc" = 'True
type instance A.IsReg PPC "F8rc" = 'True
type instance A.IsReg PPC "G8rc" = 'True
type instance A.IsReg PPC "G8rc_nox0" = 'True
type instance A.IsReg PPC "Gprc" = 'True
type instance A.IsReg PPC "Gprc_nor0" = 'True
type instance A.IsReg PPC "I1imm" = 'False
type instance A.IsReg PPC "I32imm" = 'False
type instance A.IsReg PPC "I32imm" = 'False
-- What to do about these combinations?
-- type instance A.IsReg PPC "Memri" =
-- type instance A.IsReg PPC "Memrix" =
-- type instance A.IsReg PPC "Memrix16" =
type instance A.IsReg PPC "Memrr" = 'True
type instance A.IsReg PPC "S16imm" = 'False
type instance A.IsReg PPC "S16imm64" = 'False
type instance A.IsReg PPC "S17imm" = 'False
type instance A.IsReg PPC "S5imm" = 'False
-- What to do about these combinations?
-- type instance A.IsReg PPC "Spe2dis" =
-- type instance A.IsReg PPC "Spe4dis" =
-- type instance A.IsReg PPC "Spe8dis" =
type instance A.IsReg PPC "Tlscall" = 'False
type instance A.IsReg PPC "Tlscall32" = 'False
type instance A.IsReg PPC "Tlsreg" = 'True
type instance A.IsReg PPC "Tlsreg32" = 'True
type instance A.IsReg PPC "U10imm" = 'False
type instance A.IsReg PPC "U16imm" = 'False
type instance A.IsReg PPC "U16imm64" = 'False
type instance A.IsReg PPC "U1imm" = 'False
type instance A.IsReg PPC "U2imm" = 'False
type instance A.IsReg PPC "U4imm" = 'False
type instance A.IsReg PPC "U5imm" = 'False
type instance A.IsReg PPC "U6imm" = 'False
type instance A.IsReg PPC "U7imm" = 'False
type instance A.IsReg PPC "U8imm" = 'False
type instance A.IsReg PPC "Vrrc" = 'True
type instance A.IsReg PPC "Vsfrc" = 'True
type instance A.IsReg PPC "Vsrc" = 'True
type instance A.IsReg PPC "Vssrc" = 'True

instance A.IsOperand PPC.Operand

-- TODO: IsSpecificOperand instances

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

instance A.IsSpecificOperand PPC.Operand "Gprc" where
  allOperandValues = PPC.Gprc . PPC.GPR <$> [0..31]

instance A.IsSpecificOperand PPC.Operand "Gprc_nor0" where
  allOperandValues = PPC.Gprc_nor0 . PPC.GPR <$> [0..31]

instance A.IsSpecificOperand PPC.Operand "S16imm" where
  allOperandValues = PPC.S16imm <$> [minBound..]

data Location :: BaseType -> * where
  GPR :: PPC.GPR -> Location (BaseBVType 32)
  IP :: Location (BaseBVType 32)
  MSR :: Location (BaseBVType 32)
  CTR :: Location (BaseBVType 32)
  LNK :: Location (BaseBVType 32)
  XER :: Location (BaseBVType 32)
  CR :: Location (BaseBVType 32)
  FR :: PPC.FR -> Location (BaseBVType 64)
  VR :: PPC.VR -> Location (BaseBVType 128)

instance Show (Location tp) where
  show (GPR gpr) = show (pPrint gpr)
  show IP = "IP"
  show MSR = "MSR"
  show CTR = "CTR"
  show LNK = "LNK"
  show XER = "XER"
  show CR = "CR"
  show (FR fr) = show (pPrint fr)
  show (VR vr) = show (pPrint vr)
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
    -- | s `elem` ["r" ++ show i | i <- [0..31]] = Some (GPR (PPC.GPR (read (tail s))))
    | s `elem` ["r" ++ show i | i <- [(0 :: Int)..31]] = (Just . Some . GPR . PPC.GPR . read . tail) s
    | s == "ip" = Just (Some IP)
    | s == "msr" = Just (Some MSR)
    | s == "ctr" = Just (Some CTR)
    | s == "lnk" = Just (Some LNK)
    | s == "xer" = Just (Some XER)
    | s == "cr" = Just (Some CR)
    -- | floating point
    -- | vector
    | otherwise = Nothing

  locationType (GPR _) = BaseBVRepr (knownNat :: NatRepr 32)
  locationType IP = BaseBVRepr (knownNat :: NatRepr 32)
  locationType MSR = BaseBVRepr (knownNat :: NatRepr 32)
  locationType CTR = BaseBVRepr (knownNat :: NatRepr 32)
  locationType LNK = BaseBVRepr (knownNat :: NatRepr 32)
  locationType XER = BaseBVRepr (knownNat :: NatRepr 32)
  locationType CR = BaseBVRepr (knownNat :: NatRepr 32)
  locationType (FR _) = BaseBVRepr (knownNat :: NatRepr 64)
  locationType (VR _) = BaseBVRepr (knownNat :: NatRepr 128)

  defaultLocationExpr sym (GPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym IP = S.bvLit sym knownNat 0
  defaultLocationExpr sym MSR = S.bvLit sym knownNat 0
  defaultLocationExpr sym CTR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LNK = S.bvLit sym knownNat 0
  defaultLocationExpr sym XER = S.bvLit sym knownNat 0
  defaultLocationExpr sym CR = S.bvLit sym knownNat 0
  defaultLocationExpr sym (FR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym (VR _) = S.bvLit sym knownNat 0

type instance A.Location PPC = Location

operandValue :: forall sym s. (S.IsSymInterface sym, S.IsExprBuilder sym) => sym -> (forall tp. Location tp -> IO (S.SymExpr sym tp)) -> PPC.Operand s -> IO (S.SymExpr sym (A.OperandType PPC s))
operandValue sym locLookup = operandValue'
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
        operandValue' (PPC.Directbrtarget (PPC.ABT absTarget)) =
          S.bvLit sym knownNat (toInteger absTarget)
        operandValue' (PPC.F4rc _) = error "F4rc not yet implemented"
        operandValue' (PPC.F8rc fr) = locLookup (FR fr)
        operandValue' (PPC.G8rc _) = error "Found a G8rc operand, but PPC64 not supported"
        operandValue' (PPC.G8rc_nox0 _) = error "Found a G8rc_nox0 operand, but PPC64 not supported"
        operandValue' (PPC.Gprc gpr) = locLookup (GPR gpr)
        operandValue' (PPC.Gprc_nor0 (PPC.GPR gpr)) =
          if gpr /= 0
          then locLookup (GPR (PPC.GPR gpr))
          else S.bvLit sym knownNat 0
        operandValue' (PPC.I1imm (I.I x)) = S.bvLit sym knownNat (toInteger x)
        operandValue' (PPC.I32imm (I.I x)) = S.bvLit sym knownNat (toInteger x)
        operandValue' (PPC.Memri (PPC.MemRI gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (GPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Memrix (PPC.MemRIX gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (GPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Memrix16 (PPC.MemRIX gpr offset)) = do
          -- ?
          base <- case gpr of
                    Just gpr' -> locLookup (GPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Memrr (PPC.MemRR gpr1 gpr2)) = do
          gpr1Val <- case gpr1 of
                       Just gpr -> locLookup (GPR gpr)
                       Nothing -> S.bvLit sym knownNat 0
          gpr2Val <- locLookup (GPR gpr2)
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
                    Just gpr' -> locLookup (GPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Spe4dis (PPC.SPEDis gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (GPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Spe8dis (PPC.SPEDis gpr offset)) = do
          base <- case gpr of
                    Just gpr' -> locLookup (GPR gpr')
                    Nothing -> S.bvLit sym knownNat 0
          offset' <- S.bvLit sym knownNat (toInteger offset)
          S.bvAdd sym base offset'
        operandValue' (PPC.Tlscall _) = error "Tlscall not implemented"
        operandValue' (PPC.Tlscall32 bt) = btVal bt
        operandValue' (PPC.Tlsreg _) = error "Tlsreg not implemented"
        operandValue' (PPC.Tlsreg32 gpr) = locLookup (GPR gpr)
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
        operandValue' (PPC.Vrrc vr) = locLookup (VR vr)
        operandValue' (PPC.Vsfrc vr) = locLookup (VR vr)
        operandValue' (PPC.Vsrc vr) = locLookup (VR vr)
        operandValue' (PPC.Vssrc vr) = locLookup (VR vr)

        btVal (PPC.BT bt) = do
          ip <- locLookup IP
          offset <- S.bvLit sym knownNat (toInteger bt)
          S.bvAdd sym ip offset

operandToLocation :: PPC.Operand s -> Maybe (Location (A.OperandType PPC s))
operandToLocation (PPC.Crbitrc _) = error "Crbitrc operandToLocation ?"
operandToLocation (PPC.Crrc _) = error "Crrc operandToLocation ?"
operandToLocation (PPC.F4rc _) = error "F4rc operandToLocation ?"
operandToLocation (PPC.F8rc fr) = Just $ FR fr
operandToLocation (PPC.G8rc _) = error "G8rc operandToLocation ?"
operandToLocation (PPC.G8rc_nox0 _) = error "G8rc_nox0 operandToLocation ?"
operandToLocation (PPC.Gprc gpr) = Just $ GPR gpr
operandToLocation (PPC.Gprc_nor0 gpr@(PPC.GPR gpr'))
  | gpr' /= 0 = Just $ GPR gpr
  | otherwise = error "can't get the location of (Gprc_nor0 (GPR 0))"
operandToLocation (PPC.Memrr _) = error "MemRR operandToLocation?"
operandToLocation (PPC.Tlsreg _) = error "Tlsreg operandToLocation?"
operandToLocation (PPC.Tlsreg32 gpr) = Just $ GPR gpr
operandToLocation (PPC.Vrrc vr) = Just $ VR vr
operandToLocation (PPC.Vsfrc vr) = Just $ VR vr
operandToLocation (PPC.Vsrc vr) = Just $ VR vr
operandToLocation (PPC.Vssrc vr) = Just $ VR vr

operandToLocation (PPC.Memri _) = error "Memri operandToLocation?"
operandToLocation (PPC.Memrix _) = error "Memrix operandToLocation?"
operandToLocation (PPC.Memrix16 _) = error "Memrix16 operandToLocation?"
operandToLocation (PPC.Spe2dis _) = error "Spe2dis operandToLocation?"
operandToLocation (PPC.Spe4dis _) = error "Spe4dis operandToLocation?"
operandToLocation (PPC.Spe8dis _) = error "Spe8dis operandToLocation?"

operandToLocation _ = Nothing

valueToOperand :: forall s. (KnownSymbol s) => GroundValue (A.OperandType PPC s) -> PPC.Operand s
valueToOperand
  | Just Refl <- sameSymbol @s @"Abscalltarget" Proxy Proxy =
      PPC.Abscalltarget . PPC.ABT . fromInteger
  | Just Refl <- sameSymbol @s @"Abscondbrtarget" Proxy Proxy =
      PPC.Abscondbrtarget . PPC.ABT . fromInteger
  | Just Refl <- sameSymbol @s @"Absdirectbrtarget" Proxy Proxy =
      PPC.Absdirectbrtarget . PPC.ABT . fromInteger
  | Just Refl <- sameSymbol @s @"Calltarget" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Condbrtarget" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Crbitm" Proxy Proxy =
      PPC.Crbitm . PPC.CRBitM . fromInteger
  | Just Refl <- sameSymbol @s @"Crbitrc" Proxy Proxy =
      PPC.Crbitrc . PPC.CRBitRC . fromInteger
  | Just Refl <- sameSymbol @s @"Crrc" Proxy Proxy =
      PPC.Crrc . PPC.CRRC . fromInteger
  | Just Refl <- sameSymbol @s @"Directbrtarget" Proxy Proxy =
      PPC.Directbrtarget . PPC.ABT . fromInteger
  | Just Refl <- sameSymbol @s @"F4rc" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"F8rc" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"G8rc" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"G8rc_nox0" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Gprc" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Gprc_nor0" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"I1imm" Proxy Proxy =
      PPC.I1imm . fromInteger
  | Just Refl <- sameSymbol @s @"I32imm" Proxy Proxy =
      PPC.I32imm . fromInteger
  | Just Refl <- sameSymbol @s @"Memri" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Memrix" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Memrix16" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Memrr" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"S16imm" Proxy Proxy =
      PPC.S16imm . fromInteger
  | Just Refl <- sameSymbol @s @"S16imm64" Proxy Proxy =
      PPC.S16imm64 . fromInteger
  | Just Refl <- sameSymbol @s @"S17imm" Proxy Proxy =
      PPC.S17imm . fromInteger
  | Just Refl <- sameSymbol @s @"S5imm" Proxy Proxy =
      PPC.S5imm . fromInteger
  | Just Refl <- sameSymbol @s @"Spe2dis" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Spe4dis" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Spe8dis" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Tlscall" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Tlscall32" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Tlsreg" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Tlsreg32" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"U10imm" Proxy Proxy =
      PPC.U10imm . fromInteger
  | Just Refl <- sameSymbol @s @"U16imm" Proxy Proxy =
      PPC.U16imm . fromInteger
  | Just Refl <- sameSymbol @s @"U16imm64" Proxy Proxy =
      PPC.U16imm64 . fromInteger
  | Just Refl <- sameSymbol @s @"U1imm" Proxy Proxy =
      PPC.U1imm . fromInteger
  | Just Refl <- sameSymbol @s @"U2imm" Proxy Proxy =
      PPC.U2imm . fromInteger
  | Just Refl <- sameSymbol @s @"U4imm" Proxy Proxy =
      PPC.U4imm . fromInteger
  | Just Refl <- sameSymbol @s @"U5imm" Proxy Proxy =
      PPC.U5imm . fromInteger
  | Just Refl <- sameSymbol @s @"U6imm" Proxy Proxy =
      PPC.U6imm . fromInteger
  | Just Refl <- sameSymbol @s @"U7imm" Proxy Proxy =
      PPC.U7imm . fromInteger
  | Just Refl <- sameSymbol @s @"U8imm" Proxy Proxy =
      PPC.U8imm . fromInteger
  | Just Refl <- sameSymbol @s @"Vrrc" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Vsfrc" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Vsrc" Proxy Proxy =
      -- XXX:
      undefined
  | Just Refl <- sameSymbol @s @"Vssrc" Proxy Proxy =
      -- XXX:
      undefined
  | otherwise =
      undefined

instance A.Architecture PPC where
  operandValue _ = operandValue

  operandToLocation _ = operandToLocation

  valueToOperand _ = valueToOperand

-- This is a hack.
instance OrdF (PPC.Opcode (TemplatedOperand PPC)) where
  -- This is valid because no values inhabit this type.
  compareF = undefined

instance ShowF (PPC.Opcode (TemplatedOperand PPC))

instance EnumF (PPC.Opcode (TemplatedOperand PPC)) where
  -- This is valid because no values inhabit this type.
  enumF = undefined
  congruentF = undefined

fromRight :: (Monad m) => Either String a -> m a
fromRight = either fail return

loadBaseSet :: S.SimpleBuilder t st -> IO (BaseSet (S.SimpleBuilder t st) PPC)
loadBaseSet sym = do
  let readOp fp = readFormulaFromFile sym ("semmc-ppc/data/base/" <> fp)
  addi <- fromRight =<< readOp "ADDI.sem"
  return $ MapF.insert (Witness PPC.ADDI) addi
         $ MapF.empty
