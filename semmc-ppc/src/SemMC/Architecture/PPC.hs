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
  , machineState
  , loadBaseSet
  ) where

import qualified GHC.Err.Located as L

import           Data.Bits
import           Data.EnumF ( EnumF(..) )
import           Data.Foldable ( foldrM )
import           Data.Int ( Int32 )
import qualified Data.Int.Indexed as I
import qualified Data.Map as Map
import           Data.Monoid ( (<>) )
import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Parameterized.Witness ( Witness(..) )
import           Data.Proxy ( Proxy(..) )
import           Data.Void ( absurd, Void )
import qualified Data.Word.Indexed as W
import qualified Text.Megaparsec as P
import           Text.Printf ( printf )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture as A
import qualified SemMC.ConcreteState as CS
import           SemMC.Formula
import           SemMC.Formula.Parser ( BuildOperandList, readFormulaFromFile )
import           SemMC.Formula.Env ( FormulaEnv(..), SomeSome(..), UninterpretedFunctions )
import qualified SemMC.Stochastic.Remote as R
import           SemMC.Synthesis.Template ( BaseSet, TemplatedArch, TemplatedOperandFn, TemplatableOperand(..), TemplatedOperand(..), WrappedRecoverOperandFn(..), TemplatableOperands )
import           SemMC.Util ( makeSymbol, Equal )

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
                         -> TemplatedOperand arch s
concreteTemplatedOperand op loc x = TemplatedOperand (Just (loc x)) mkTemplate' :: TemplatedOperand arch s
  where mkTemplate' :: TemplatedOperandFn arch s
        mkTemplate' sym locLookup = do
          expr <- A.unTagged <$> A.operandValue (Proxy @arch) sym locLookup (op x)
          return (expr, WrappedRecoverOperandFn $ const (return (op x)))

instance TemplatableOperand PPC "F4rc" where
  opTemplates = concreteTemplatedOperand (PPC.F4rc . PPC.FR) (LocVSR . PPC.VSReg) <$> [0..31]

instance TemplatableOperand PPC "F8rc" where
  opTemplates = concreteTemplatedOperand (PPC.F8rc . PPC.FR) (LocVSR . PPC.VSReg) <$> [0..31]

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
  let addFn :: (String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)
            -> UninterpretedFunctions sym
            -> IO (UninterpretedFunctions sym)
      addFn (name, Some args, Some ret) m =
        flip (Map.insert name) m . SomeSome <$> S.freshTotalUninterpFn sym (makeSymbol name) args ret
  fns <- foldrM addFn Map.empty [ ("fp.add64",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
                                , ("fp.add32",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
                                , ("fp.sub64",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
                                , ("fp.sub32",
                                   Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
                                   Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
                                ]
  undefinedBit <- S.freshConstant sym (makeSymbol "undefined_bit") knownRepr
  let env = FormulaEnv { envFunctions = fns
                       , envUndefinedBit = undefinedBit
                       }
  let readOp :: (BuildOperandList (TemplatedArch PPC) sh)
             => FilePath
             -> IO (Either String (ParameterizedFormula sym (TemplatedArch PPC) sh))
      readOp fp = readFormulaFromFile sym env ("semmc-ppc/data/base/" <> fp)
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

instance CS.ConcreteArchitecture PPC where
  operandToSemanticView _proxy = operandToSemanticViewPPC
  zeroState _proxy = PPCS.zeroState
  randomState _proxy = PPCS.randomState
  serialize _proxy = PPCS.serialize
  deserialize _proxy = PPCS.deserialize
  readView = P.parseMaybe (CS.parseView parseLocation)
  showView = CS.printView show

machineState :: R.MachineState (CS.ConcreteState PPC)
machineState = R.MachineState { R.flattenMachineState = PPCS.serialize
                              , R.parseMachineState = PPCS.deserialize
                              }

vsrLowerHalf :: CS.Slice 64 128
vsrLowerHalf = CS.Slice knownNat knownNat (knownNat @0) (knownNat @64)

operandToSemanticViewPPC :: PPC.Operand s -> Maybe (CS.SemanticView PPC)
operandToSemanticViewPPC op =
  case op of
    PPC.F4rc fr -> frSemanticView fr
    PPC.F8rc fr -> frSemanticView fr
    PPC.G8rc _ -> L.error "G8rc not handled"
    PPC.G8rc_nox0 _ -> L.error "G8rc_nox0 not handled"
    PPC.Gprc gpr -> gprSemanticView gpr
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
