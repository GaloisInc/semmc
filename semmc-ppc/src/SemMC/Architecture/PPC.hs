{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
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

{-# OPTIONS_GHC -Wno-orphans #-}
module SemMC.Architecture.PPC
  ( PPC
  , Location(..)
  , testSerializer
  , loadBaseSet
  , PseudoOpcode(..)
  ) where

import qualified GHC.Err.Located as L

import           Data.Bits
import           Data.EnumF ( EnumF(..) )
import           Data.Foldable ( foldrM )
import           Data.Int ( Int16, Int32 )
import qualified Data.Int.Indexed as I
import qualified Data.Map.Strict as Map
import           Data.Monoid ( (<>) )
import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.ShapedList as SL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TH.GADT as TH
import           Data.Parameterized.Witness ( Witness(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           Data.Type.Equality ( type (==) )
import           Data.Void ( absurd, Void )
import           Data.Word ( Word16 )
import qualified Data.Word.Indexed as W
import           GHC.TypeLits ( KnownNat, Nat, Symbol )
import           System.FilePath ( (</>) )
import qualified Text.Megaparsec as P
import           Text.Printf ( printf )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S

import           Data.Parameterized.ShapedList ( ShapedList(Nil, (:>)) )
import qualified Dismantle.Instruction as D
import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()

import qualified SemMC.Architecture as A
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Parser as FP
import           SemMC.Formula.Env ( FormulaEnv(..), SomeSome(..), UninterpretedFunctions )
import           SemMC.Stochastic.Pseudo ( Pseudo, ArchitectureWithPseudo(..) )
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U

import           SemMC.Architecture.PPC.Location
import qualified SemMC.Architecture.PPC.ConcreteState as PPCS

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
type instance A.OperandType PPC "F4rc" = BaseBVType 128
type instance A.OperandType PPC "F8rc" = BaseBVType 128
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
          let recover evalFn = constr <$> evalFn v
          return (extended, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "F4rc" where
  opTemplates = concreteTemplatedOperand (PPC.F4rc . PPC.FR) (LocVSR . PPC.VSReg) <$> [0..31]

instance T.TemplatableOperand PPC "F8rc" where
  opTemplates = concreteTemplatedOperand (PPC.F8rc . PPC.FR) (LocVSR . PPC.VSReg) <$> [0..31]

instance T.TemplatableOperand PPC "Gprc" where
  opTemplates = concreteTemplatedOperand PPC.Gprc LocGPR . PPC.GPR <$> [0..31]

instance T.TemplatableOperand PPC "Gprc_nor0" where
  opTemplates = concreteTemplatedOperand PPC.Gprc_nor0 LocGPR . PPC.GPR <$> [0..31]

instance T.TemplatableOperand PPC "S16imm" where
  opTemplates = [symbolicTemplatedOperand (Proxy @16) True "S16imm" (PPC.S16imm . fromInteger)]

instance T.TemplatableOperand PPC "Memri" where
  opTemplates = mkTemplate <$> [0..31]
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

instance T.TemplatableOperand PPC "Directbrtarget" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Directbrtarget"
          mkDirect sym locLookup = do
            ip <- locLookup LocIP
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Directbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            expr <- S.bvAdd sym ip extended
            let recover evalFn =
                  PPC.Directbrtarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
            return (expr, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "U5imm" where
  opTemplates = [symbolicTemplatedOperand (Proxy @5) False "U5imm" (PPC.U5imm . fromInteger)]

instance T.TemplatableOperand PPC "S17imm" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkImm]
    where mkImm :: T.TemplatedOperandFn PPC "S17imm"
          mkImm sym _ = do
            v <- S.freshConstant sym (U.makeSymbol "S17imm") (knownRepr :: BaseTypeRepr (BaseBVType 16))
            zeroes <- S.bvLit sym knownNat 0
            extended <- S.bvConcat sym v zeroes
            let recover evalFn = PPC.S17imm . fromInteger <$> evalFn v
            return (extended, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Absdirectbrtarget" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Absdirectbrtarget"
          mkDirect sym _ = do
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Absdirectbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            let recover evalFn =
                  PPC.Absdirectbrtarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
            return (extended, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Calltarget" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Calltarget"
          mkDirect sym locLookup = do
            ip <- locLookup LocIP
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Calltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            expr <- S.bvAdd sym ip extended
            let recover evalFn =
                  PPC.Calltarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
            return (expr, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Abscalltarget" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Abscalltarget"
          mkDirect sym _ = do
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Abscalltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            zeroes <- S.bvLit sym (knownNat @2) 0
            shifted <- S.bvConcat sym offsetRaw zeroes
            extended <- S.bvSext sym knownNat shifted
            let recover evalFn =
                  PPC.Abscalltarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
            return (extended, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Crrc" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Crrc"
          mkDirect sym _ = do
            crrc <- S.freshConstant sym (U.makeSymbol "Crrc") (knownRepr :: BaseTypeRepr (BaseBVType 3))
            let recover evalFn =
                  PPC.Crrc . PPC.CRRC . fromInteger <$> evalFn crrc
            return (crrc, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "I32imm" where
  -- XXX: What to do here? seems very instruction-specific
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkImm]
    where mkImm :: T.TemplatedOperandFn PPC "I32imm"
          mkImm sym _ = do
            v <- S.freshConstant sym (U.makeSymbol "I32imm") knownRepr
            let recover evalFn = PPC.I32imm . fromInteger <$> evalFn v
            return (v, T.WrappedRecoverOperandFn recover)

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
        operandValue' (PPC.F4rc (PPC.FR fr)) = locLookup (LocVSR (PPC.VSReg fr))
        operandValue' (PPC.F8rc (PPC.FR fr)) = locLookup (LocVSR (PPC.VSReg fr))
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
        operandValue' (PPC.Vsfrc vsr) = locLookup (LocVSR vsr)
        operandValue' (PPC.Vsrc vsr) = locLookup (LocVSR vsr)
        operandValue' (PPC.Vssrc vsr) = locLookup (LocVSR vsr)

        btVal (PPC.BT bt) = do
          ip <- locLookup LocIP
          offset <- S.bvLit sym knownNat (toInteger bt)
          S.bvAdd sym ip offset

operandToLocation :: PPC.Operand s -> Maybe (Location (A.OperandType PPC s))
operandToLocation (PPC.F4rc (PPC.FR fr)) = Just $ LocVSR (PPC.VSReg fr)
operandToLocation (PPC.F8rc (PPC.FR fr)) = Just $ LocVSR (PPC.VSReg fr)
operandToLocation (PPC.G8rc _) = error "G8rc operandToLocation ?"
operandToLocation (PPC.G8rc_nox0 _) = error "G8rc_nox0 operandToLocation ?"
operandToLocation (PPC.Gprc gpr) = Just $ LocGPR gpr
operandToLocation (PPC.Gprc_nor0 gpr) = Just (LocGPR gpr)
operandToLocation (PPC.Tlsreg _) = error "Tlsreg operandToLocation?"
operandToLocation (PPC.Tlsreg32 gpr) = Just $ LocGPR gpr
operandToLocation (PPC.Vrrc (PPC.VR vr)) = Just $ LocVSR (PPC.VSReg (vr + 32))
operandToLocation (PPC.Vsfrc vr) = Just $ LocVSR vr
operandToLocation (PPC.Vsrc vr) = Just $ LocVSR vr
operandToLocation (PPC.Vssrc vr) = Just $ LocVSR vr
operandToLocation _ = Nothing

instance A.Architecture PPC where
  data TaggedExpr PPC sym s = TaggedExpr (S.SymExpr sym (A.OperandType PPC s))
  unTagged (TaggedExpr e) = e
  operandValue _ = operandValue
  operandToLocation _ = operandToLocation

opcodeToVoid :: ((o == PPC.Operand) ~ 'False) => PPC.Opcode o sh -> Void
opcodeToVoid x = case x of {}

-- This is a hack.
instance OrdF (PPC.Opcode (T.TemplatedOperand PPC)) where
  compareF = absurd . opcodeToVoid

instance ShowF (PPC.Opcode (T.TemplatedOperand PPC))

instance EnumF (PPC.Opcode (T.TemplatedOperand PPC)) where
  enumF = absurd . opcodeToVoid
  congruentF = absurd . opcodeToVoid

fromRight :: (Monad m) => Either String a -> m a
fromRight = either fail return

class (FP.BuildOperandList (T.TemplatedArch PPC) sh, T.TemplatableOperands PPC sh) => BuildableAndTemplatable sh
instance (FP.BuildOperandList (T.TemplatedArch PPC) sh, T.TemplatableOperands PPC sh) => BuildableAndTemplatable sh

loadBaseSet :: forall sym.
               (S.IsExprBuilder sym,
                S.IsSymInterface sym)
            => FilePath
            -> sym
            -> IO (T.BaseSet sym PPC)
loadBaseSet baseSetDir sym = do
  let addFn :: (String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)
            -> UninterpretedFunctions sym
            -> IO (UninterpretedFunctions sym)
      addFn (name, Some args, Some ret) m =
        flip (Map.insert name) m . SomeSome <$> S.freshTotalUninterpFn sym (U.makeSymbol name) args ret
  fns <- foldrM addFn Map.empty [ ("fp.add64",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
                                , ("fp.add32",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 32 Ctx.::> BaseBVType 32)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
                                , ("fp.sub64",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
                                , ("fp.sub32",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 32 Ctx.::> BaseBVType 32)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
                                ]
  undefinedBit <- S.freshConstant sym (U.makeSymbol "undefined_bit") knownRepr
  let env = FormulaEnv { envFunctions = fns
                       , envUndefinedBit = undefinedBit
                       }
  let readOp :: (FP.BuildOperandList (T.TemplatedArch PPC) sh)
             => FilePath
             -> IO (Either String (F.ParameterizedFormula sym (T.TemplatedArch PPC) sh))
      readOp fp = FP.readFormulaFromFile sym env (baseSetDir </> fp)
      addOp :: Some (Witness BuildableAndTemplatable (PPC.Opcode PPC.Operand))
            -> T.BaseSet sym PPC
            -> IO (T.BaseSet sym PPC)
      addOp (Some (Witness op)) m = do
        printf "reading op %s\n" (showF op)
        op' <- fromRight =<< readOp (showF op <> ".sem")
        return $ MapF.insert (Witness op) op' m
  foldrM addOp MapF.empty [ Some (Witness PPC.ADD4)
                          , Some (Witness PPC.ADDI)
                          , Some (Witness PPC.ADDIS)
                          , Some (Witness PPC.AND)
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
                          , Some (Witness PPC.EQV)
                          , Some (Witness PPC.FADD)
                          , Some (Witness PPC.FADDS)
                          , Some (Witness PPC.LBZ)
                          , Some (Witness PPC.MFCR)
                          , Some (Witness PPC.MFSPR)
                          , Some (Witness PPC.MTCRF)
                          , Some (Witness PPC.MTSPR)
                          , Some (Witness PPC.NAND)
                          , Some (Witness PPC.NOR)
                          , Some (Witness PPC.OR)
                          , Some (Witness PPC.STB)
                          , Some (Witness PPC.SUBF)
                          , Some (Witness PPC.XOR)
                          ]

operandTypePPC :: PPC.Operand s -> BaseTypeRepr (A.OperandType PPC s)
operandTypePPC o =
  case o of
    PPC.F4rc {}              -> knownRepr
    PPC.F8rc {}              -> knownRepr
    PPC.G8rc {}              -> knownRepr
    PPC.G8rc_nox0 {}         -> knownRepr
    PPC.Gprc {}              -> knownRepr
    PPC.Gprc_nor0 {}         -> knownRepr
    PPC.Tlsreg {}            -> knownRepr
    PPC.Tlsreg32 {}          -> knownRepr
    PPC.Vrrc {}              -> knownRepr
    PPC.Vsfrc {}             -> knownRepr
    PPC.Vsrc {}              -> knownRepr
    PPC.Vssrc {}             -> knownRepr
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
    PPC.S5imm {}             -> knownRepr
    PPC.Spe2dis {}           -> knownRepr
    PPC.Spe4dis {}           -> knownRepr
    PPC.Spe8dis {}           -> knownRepr
    PPC.Tlscall {}           -> knownRepr
    PPC.Tlscall32 {}         -> knownRepr
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

registerizeInstructionPPC :: CS.RegisterizedInstruction PPC
                          -> CS.ConcreteState PPC
                          -> (A.Instruction PPC, CS.ConcreteState PPC)
registerizeInstructionPPC ri s =
  case ri of
    CS.RI { CS.riOpcode = opc
          , CS.riOperands = ops
          , CS.riLiteralLocs = lls
          } ->
      case MapF.foldrWithKey replaceLiterals (ops, s) lls of
        (ops', s') -> (D.Instruction opc ops', s')

replaceLiterals :: CS.LiteralRef PPC sh s
                -> Location s
                -> (SL.ShapedList PPC.Operand sh, CS.ConcreteState PPC)
                -> (SL.ShapedList PPC.Operand sh, CS.ConcreteState PPC)
replaceLiterals (CS.LiteralRef ix) loc (ops, s) =
  case MapF.lookup loc s of
    Nothing -> L.error ("Location not defined in state: " ++ showF loc)
    Just val ->
      let (clampedValue, op') = truncateValue (SL.indexShapedList ops ix) val
      in (SL.updateShapedList ops ix (const op'), MapF.insert loc clampedValue s)

-- | Replace the value in the given immediate operand with the value in a
-- 'CS.Value', truncating it if necessary.  The truncated value is returned so
-- that the test case can be updated.
--
-- Note that this function calls error on operands that are not immediates.
truncateValue :: PPC.Operand s
              -> CS.Value (A.OperandType PPC s)
              -> (CS.Value (A.OperandType PPC s), PPC.Operand s)
truncateValue op v =
  case op of
    PPC.I1imm {}             -> withTruncIVal v (W.w 0x1) PPC.I1imm
    PPC.I32imm {}            -> withTruncIVal v (W.w 0xffffffff) PPC.I32imm
    PPC.S16imm {}            -> withTruncI16Val v 0xffff PPC.S16imm
    PPC.S16imm64 {}          -> withTruncI16Val v 0xffff PPC.S16imm64
    PPC.S17imm {}            -> withTruncI16Val v 0xffff PPC.S17imm
    PPC.S5imm {}             -> withTruncIVal v (W.w 0x1f) PPC.S5imm
    PPC.U1imm {}             -> withTruncWVal v (W.w 0x1) PPC.U1imm
    PPC.U2imm {}             -> withTruncWVal v (W.w 0x3) PPC.U2imm
    PPC.U4imm {}             -> withTruncWVal v (W.w 0xf) PPC.U4imm
    PPC.U5imm {}             -> withTruncWVal v (W.w 0x1f) PPC.U5imm
    PPC.U6imm {}             -> withTruncWVal v (W.w 0x3f) PPC.U6imm
    PPC.U7imm {}             -> withTruncWVal v (W.w 0x7f) PPC.U7imm
    PPC.U8imm {}             -> withTruncWVal v (W.w 0xff) PPC.U8imm
    PPC.U10imm {}            -> withTruncWVal v (W.w 0x3ff) PPC.U10imm
    PPC.U16imm {}            -> withTruncWVal v (W.w 0xffff) PPC.U16imm
    PPC.U16imm64 {}          -> withTruncWVal v (W.w 0xffff) PPC.U16imm64
    PPC.Memrr {}             -> L.error "Unexpected non-literal operand"
    PPC.Memri {}             -> L.error "Unexpected non-literal operand"
    PPC.Memrix {}            -> L.error "Unexpected non-literal operand"
    PPC.Memrix16 {}          -> L.error "Unexpected non-literal operand"
    PPC.Vrrc {}              -> L.error "Unexpected non-literal operand"
    PPC.Vsfrc {}             -> L.error "Unexpected non-literal operand"
    PPC.Vsrc {}              -> L.error "Unexpected non-literal operand"
    PPC.Vssrc {}             -> L.error "Unexpected non-literal operand"
    PPC.Tlsreg {}            -> L.error "Unexpected non-literal operand"
    PPC.Tlsreg32 {}          -> L.error "Unexpected non-literal operand"
    PPC.Gprc_nor0 {}         -> L.error "Unexpected non-literal operand"
    PPC.G8rc_nox0 {}         -> L.error "Unexpected non-literal operand"
    PPC.G8rc {}              -> L.error "Unexpected non-literal operand"
    PPC.Gprc {}              -> L.error "Unexpected non-literal operand"
    PPC.F4rc {}              -> L.error "Unexpected non-literal operand"
    PPC.F8rc {}              -> L.error "Unexpected non-literal operand"
    PPC.Spe2dis {}           -> L.error "Unexpected non-literal operand"
    PPC.Spe4dis {}           -> L.error "Unexpected non-literal operand"
    PPC.Spe8dis {}           -> L.error "Unexpected non-literal operand"
    PPC.Abscondbrtarget {}   -> L.error "Control flow transfer instructions unsupported"
    PPC.Absdirectbrtarget {} ->  L.error "Control flow transfer instructions unsupported"
    PPC.Condbrtarget {}      ->  L.error "Control flow transfer instructions unsupported"
    PPC.Directbrtarget {}    ->  L.error "Control flow transfer instructions unsupported"
    PPC.Calltarget {}        ->  L.error "Control flow transfer instructions unsupported"
    PPC.Abscalltarget {}     ->  L.error "Control flow transfer instructions unsupported"
    PPC.Tlscall {}           ->  L.error "Control flow transfer instructions unsupported"
    PPC.Tlscall32 {}         ->  L.error "Control flow transfer instructions unsupported"

withTruncI16Val :: (KnownNat n)
                => CS.Value (BaseBVType n)
                -> Word16
                -> (Int16 -> PPC.Operand s)
                -> (CS.Value (BaseBVType n), PPC.Operand s)
withTruncI16Val (CS.ValueBV w) mask con =
  let w' = W.unW w .&. fromIntegral mask
  in (CS.ValueBV (W.w w'), con (fromIntegral w'))

withTruncIVal :: (KnownNat n)
             => CS.Value (BaseBVType n)
             -> W.W n
             -> (I.I n' -> PPC.Operand s)
             -> (CS.Value (BaseBVType n), PPC.Operand s)
withTruncIVal (CS.ValueBV w) mask con =
  let w' = w .&. mask
  in (CS.ValueBV w', con (I.I (fromIntegral (W.unW w'))))

withTruncWVal :: (KnownNat n, KnownNat n')
              => CS.Value (BaseBVType n)
              -> W.W n
              -> (W.W n' -> PPC.Operand s)
              -> (CS.Value (BaseBVType n), PPC.Operand s)
withTruncWVal (CS.ValueBV w) mask con =
  let w' = w .&. mask
  in (CS.ValueBV w', con (W.w (fromIntegral (W.unW w'))))

instance CS.ConcreteArchitecture PPC where
  operandToSemanticView _proxy = operandToSemanticViewPPC
  registerizeInstruction = registerizeInstructionPPC
  operandType _proxy = operandTypePPC
  zeroState _proxy = PPCS.zeroState
  randomState _proxy = PPCS.randomState
  serialize _proxy = PPCS.serialize
  deserialize _proxy = PPCS.deserialize
  readView = P.parseMaybe (CS.parseView parseLocation)
  showView = CS.printView show

testSerializer :: CE.TestSerializer (CS.ConcreteState PPC) (A.Instruction PPC)
testSerializer = CE.TestSerializer { CE.flattenMachineState = PPCS.serialize
                                   , CE.parseMachineState = PPCS.deserialize
                                   , CE.flattenProgram = mconcat . map PPC.assembleInstruction
                                   }

vsrLowerHalf :: CS.Slice 64 128
vsrLowerHalf = CS.Slice knownNat knownNat (knownNat @0) (knownNat @64)

operandToSemanticViewPPC :: PPC.Operand s -> Maybe (CS.SemanticView PPC)
operandToSemanticViewPPC op =
  case op of
    PPC.F4rc fr -> frSemanticView fr
    PPC.F8rc fr -> frSemanticView fr
    PPC.G8rc gpr -> gprSemanticView gpr -- L.error "G8rc not handled"
    PPC.G8rc_nox0 _ -> L.error "G8rc_nox0 not handled"
    PPC.Gprc gpr -> gprSemanticView gpr
    PPC.Gprc_nor0 gpr -> gprSemanticView gpr
    PPC.Tlsreg _ -> L.error "Tlsreg not handled"
    PPC.Tlsreg32 gpr -> gprSemanticView gpr
    PPC.Vrrc vr -> vrSemanticView vr
    PPC.Vsfrc vsr -> vsrSemanticView vsr
    PPC.Vsrc vsr -> vsrSemanticView vsr
    PPC.Vssrc vsr -> vsrSemanticView vsr
    _ -> Nothing
  where frSemanticView (PPC.FR rno) =
          Just $ CS.SemanticView { CS.semvView = frView rno
                                 , CS.semvCongruentViews = [ frView rno' | rno' <- [0..31], rno' /= rno ]
                                 , CS.semvDiff = CS.diffFloat
                                 }
        frView rno = CS.View vsrLowerHalf (LocVSR (PPC.VSReg rno))

        gprSemanticView (PPC.GPR rno) =
          Just $ CS.SemanticView { CS.semvView = gprView rno
                                 , CS.semvCongruentViews = [ gprView rno' | rno' <- [0..31], rno' /= rno ]
                                 , CS.semvDiff = CS.diffInt
                                 }
        gprView rno = CS.trivialView Proxy (LocGPR (PPC.GPR rno))

        vrSemanticView (PPC.VR rno) =
          Just $ CS.SemanticView { CS.semvView = vrView rno
                                 , CS.semvCongruentViews = [ vrView rno' | rno' <- [0..31], rno' /= rno ]
                                 -- FIXME: we'll have to decide the diff function based on opcode
                                 , CS.semvDiff = CS.diffInt
                                 }
        vrView rno = CS.trivialView Proxy (LocVSR (PPC.VSReg (rno + 32)))

        vsrSemanticView (PPC.VSReg rno) =
          Just $ CS.SemanticView { CS.semvView = vsrView rno
                                 , CS.semvCongruentViews = [ vsrView rno' | rno' <- [0..63], rno' /= rno ]
                                 -- FIXME: we'll have to decide the diff function based on opcode
                                 , CS.semvDiff = CS.diffInt
                                 }
        vsrView rno = CS.trivialView Proxy (LocVSR (PPC.VSReg rno))

data PseudoOpcode :: (Symbol -> *) -> [Symbol] -> * where
  -- | @ReplaceByteGPR rA, n, rB@ replaces the @n@th byte of @rA@ with the low
  -- byte of @rB@.
  ReplaceByteGPR :: PseudoOpcode PPC.Operand '["Gprc", "U2imm", "Gprc"]
  -- | @ExtractByteGPR rA, rB, n@ extracts the @n@th byte of @rB@ into the low
  -- byte of @rA@, zero-extending it.
  ExtractByteGPR :: PseudoOpcode PPC.Operand '["Gprc", "Gprc", "U2imm"]
  -- | @ReplaceWordVR vrA, n, rB@ replaces the @n@th word of @vrA@ with the
  -- value of @rB@.
  ReplaceWordVR :: PseudoOpcode PPC.Operand '["Vrrc", "U2imm", "Gprc"]
  -- | @ExtractWordVR rA, vrB, n@ extracts the @n@th word of @vrB@ into @rA@.
  ExtractWordVR :: PseudoOpcode PPC.Operand '["Gprc", "Vrrc", "U2imm"]

deriving instance Show (PseudoOpcode op sh)

instance ShowF (PseudoOpcode op)

$(return [])

instance TestEquality (PseudoOpcode op) where
  testEquality = $(TH.structuralTypeEquality [t| PseudoOpcode |] [])

instance OrdF (PseudoOpcode op) where
  compareF = $(TH.structuralTypeOrd [t| PseudoOpcode |] [])

instance HasRepr (PseudoOpcode op) SL.ShapeRepr where
  typeRepr ReplaceByteGPR = knownRepr
  typeRepr ExtractByteGPR = knownRepr
  typeRepr ReplaceWordVR = knownRepr
  typeRepr ExtractWordVR = knownRepr

type instance Pseudo PPC = PseudoOpcode

ppcAssemblePseudo :: PseudoOpcode op sh -> ShapedList op sh -> [A.Instruction PPC]
ppcAssemblePseudo opcode oplist =
  case opcode of
    ReplaceByteGPR ->
      case (oplist :: ShapedList PPC.Operand '["Gprc", "U2imm", "Gprc"]) of
        (target :> PPC.U2imm (W.unW -> n) :> source :> Nil) ->
          let n' :: W.W 5 = fromIntegral n
          in [ D.Instruction PPC.RLWIMI ( target :>
                                          PPC.U5imm (n' * 8 + 7) :>
                                          PPC.U5imm (n' * 8) :>
                                          PPC.U5imm (n' * 8) :>
                                          source :>
                                          source :>
                                          Nil
                                        )
             ]
    ExtractByteGPR ->
      case (oplist :: ShapedList PPC.Operand '["Gprc", "Gprc", "U2imm"]) of
        (target :> source :> PPC.U2imm (W.unW -> n) :> Nil) ->
          let n' :: W.W 5 = fromIntegral n
          in [ D.Instruction PPC.RLWINM ( target :>
                                          PPC.U5imm 31 :>
                                          PPC.U5imm (0 - n') :>
                                          PPC.U5imm (8 + n') :>
                                          source :>
                                          Nil
                                        )
             ]
    ReplaceWordVR ->
      case (oplist :: ShapedList PPC.Operand '["Vrrc", "U2imm", "Gprc"]) of
        (target :> PPC.U2imm (W.unW -> n) :> source :> Nil) ->
          -- Assumes there's a free chunk of memory pointed to by R31.
          let vrLocation = PPC.Memrr (PPC.MemRR Nothing (PPC.GPR 31))
              gprWriteLocation = PPC.Memri (PPC.MemRI (Just (PPC.GPR 31)) (fromIntegral (n * 4)))
          in [ -- First, store the current contents of the target into memory.
               D.Instruction PPC.STVX ( vrLocation :>
                                        target :>
                                        Nil
                                      )
             , -- Next, write the GPR into the appropriate spot.
               D.Instruction PPC.STW ( gprWriteLocation :>
                                       source :>
                                       Nil
                                     )
             , -- Finally, read the target back from memory.
               D.Instruction PPC.LVX ( target :>
                                       vrLocation :>
                                       Nil
                                     )
             ]
    ExtractWordVR ->
      case (oplist :: ShapedList PPC.Operand '["Gprc", "Vrrc", "U2imm"]) of
        (target :> source :> PPC.U2imm (W.unW -> n) :> Nil) ->
          -- Assumes there's a free chunk of memory pointed to by R31.
          let vrLocation = PPC.Memrr (PPC.MemRR Nothing (PPC.GPR 31))
              gprReadLocation = PPC.Memri (PPC.MemRI (Just (PPC.GPR 31)) (fromIntegral (n * 4)))
          in [ -- First, write the contents of the vector register into memory.
               D.Instruction PPC.STVX ( vrLocation :>
                                        source :>
                                        Nil
                                      )
             , -- Then, read the GPR from an offset into that saved register.
               D.Instruction PPC.LWZ ( target :>
                                       gprReadLocation :>
                                       Nil
                                     )
             ]

instance D.ArbitraryOperands PseudoOpcode PPC.Operand where
  arbitraryOperands gen op = case op of
    ReplaceByteGPR -> D.arbitraryShapedList gen
    ExtractByteGPR -> D.arbitraryShapedList gen
    ReplaceWordVR  -> D.arbitraryShapedList gen
    ExtractWordVR  -> D.arbitraryShapedList gen

instance ArchitectureWithPseudo PPC where
  assemblePseudo _ = ppcAssemblePseudo
