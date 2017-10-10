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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module SemMC.Architecture.PPC32
  ( PPC
  , Location(..)
  , testSerializer
  , loadBaseSet
  , PPCP.PseudoOpcode(..)
  , BuildableAndTemplatable
  ) where

import qualified GHC.Err.Located as L

import qualified Data.Constraint as C
import           Data.EnumF ( EnumF(..) )
import qualified Data.Functor.Identity as I
import qualified Data.Int.Indexed as I
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.ShapedList as SL
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           Data.Type.Equality ( type (==) )
import           Data.Void ( absurd, Void )
import qualified Data.Word.Indexed as W
import           GHC.TypeLits ( KnownNat, Nat )
import           System.FilePath ( (</>), (<.>) )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.Instruction as D
import qualified Dismantle.PPC as PPC
import           Dismantle.PPC.Random ()

import qualified SemMC.Architecture as A
import qualified SemMC.Concrete.State as CS
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Formula as F
import           SemMC.Stochastic.Pseudo ( Pseudo, ArchitectureWithPseudo(..) )
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U

import           SemMC.Architecture.PPC.Eval
import           SemMC.Architecture.PPC.Location
import qualified SemMC.Architecture.PPC32.ConcreteState as PPCS
import qualified SemMC.Architecture.PPC.Pseudo as PPCP
import qualified SemMC.Architecture.PPC.Shared as PPCS

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
type instance A.OperandType PPC "Crbitm" = BaseBVType 3
type instance A.OperandType PPC "Crbitrc" = BaseBVType 5
type instance A.OperandType PPC "Crrc" = BaseBVType 3
type instance A.OperandType PPC "Directbrtarget" = BaseBVType 24
type instance A.OperandType PPC "F4rc" = BaseBVType 128
type instance A.OperandType PPC "F8rc" = BaseBVType 128
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
type instance A.OperandType PPC "Vsfrc" = BaseBVType 128
type instance A.OperandType PPC "Vsrc" = BaseBVType 128
type instance A.OperandType PPC "Vssrc" = BaseBVType 128

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

instance T.TemplatableOperand PPC "S16imm64" where
  opTemplates = [symbolicTemplatedOperand (Proxy @16) True "S16imm64" (PPC.S16imm64 . fromInteger)]

instance T.TemplatableOperand PPC "U16imm" where
  opTemplates = [symbolicTemplatedOperand (Proxy @16) True "U16imm" (PPC.U16imm . fromInteger)]

instance T.TemplatableOperand PPC "U16imm64" where
  opTemplates = [symbolicTemplatedOperand (Proxy @16) True "U16imm64" (PPC.U16imm64 . fromInteger)]

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
          mkDirect sym _locLookup = do
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Directbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            let recover evalFn =
                  PPC.Directbrtarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
            return (offsetRaw, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "U5imm" where
  opTemplates = [symbolicTemplatedOperand (Proxy @5) False "U5imm" (PPC.U5imm . fromInteger)]

instance T.TemplatableOperand PPC "U6imm" where
  opTemplates = [symbolicTemplatedOperand (Proxy @6) False "U6imm" (PPC.U6imm . fromInteger)]

instance T.TemplatableOperand PPC "S17imm" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkImm]
    where mkImm :: T.TemplatedOperandFn PPC "S17imm"
          mkImm sym _ = do
            v <- S.freshConstant sym (U.makeSymbol "S17imm") (knownRepr :: BaseTypeRepr (BaseBVType 16))
            let recover evalFn = PPC.S17imm . fromInteger <$> evalFn v
            return (v, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Absdirectbrtarget" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Absdirectbrtarget"
          mkDirect sym _ = do
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Absdirectbrtarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            let recover evalFn =
                  PPC.Absdirectbrtarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
            return (offsetRaw, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Calltarget" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Calltarget"
          mkDirect sym _locLookup = do
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Calltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            let recover evalFn =
                  PPC.Calltarget . PPC.mkBranchTarget . fromInteger <$> evalFn offsetRaw
            return (offsetRaw, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Abscalltarget" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Abscalltarget"
          mkDirect sym _ = do
            offsetRaw <- S.freshConstant sym (U.makeSymbol "Abscalltarget") (knownRepr :: BaseTypeRepr (BaseBVType 24))
            let recover evalFn =
                  PPC.Abscalltarget . PPC.mkAbsBranchTarget . fromInteger <$> evalFn offsetRaw
            return (offsetRaw, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Crrc" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Crrc"
          mkDirect sym _ = do
            crrc <- S.freshConstant sym (U.makeSymbol "Crrc") (knownRepr :: BaseTypeRepr (BaseBVType 3))
            let recover evalFn =
                  PPC.Crrc . PPC.CRRC . fromInteger <$> evalFn crrc
            return (crrc, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "Crbitrc" where
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkDirect]
    where mkDirect :: T.TemplatedOperandFn PPC "Crbitrc"
          mkDirect sym _ = do
            crrc <- S.freshConstant sym (U.makeSymbol "Crbitrc") (knownRepr :: BaseTypeRepr (BaseBVType 5))
            let recover evalFn =
                  PPC.Crbitrc . PPC.CRBitRC . fromInteger <$> evalFn crrc
            return (crrc, T.WrappedRecoverOperandFn recover)

instance T.TemplatableOperand PPC "I32imm" where
  -- XXX: What to do here? seems very instruction-specific
  opTemplates = [T.TemplatedOperand Nothing Set.empty mkImm]
    where mkImm :: T.TemplatedOperandFn PPC "I32imm"
          mkImm sym _ = do
            v <- S.freshConstant sym (U.makeSymbol "I32imm") knownRepr
            let recover evalFn = PPC.I32imm . fromInteger <$> evalFn v
            return (v, T.WrappedRecoverOperandFn recover)

type instance A.Location PPC = Location PPC

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
        operandValue' (PPC.F4rc (PPC.FR fr)) = locLookup (LocVSR (PPC.VSReg fr))
        operandValue' (PPC.F8rc (PPC.FR fr)) = locLookup (LocVSR (PPC.VSReg fr))
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
          -- The s17 imm type is a bit strange.  It is actually represented as
          -- 16 bits in the instruction, but is interpreted as shifted left by
          -- 16 bits.  We handle that correction in the semantics, so we just
          -- treat s17imm as a plain 16 bit value.
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
        operandValue' (PPC.Vsfrc vsr) = locLookup (LocVSR vsr)
        operandValue' (PPC.Vsrc vsr) = locLookup (LocVSR vsr)
        operandValue' (PPC.Vssrc vsr) = locLookup (LocVSR vsr)


operandToLocation :: PPC.Operand s -> Maybe (Location PPC (A.OperandType PPC s))
operandToLocation (PPC.F4rc (PPC.FR fr)) = Just $ LocVSR (PPC.VSReg fr)
operandToLocation (PPC.F8rc (PPC.FR fr)) = Just $ LocVSR (PPC.VSReg fr)
operandToLocation (PPC.Gprc gpr) = Just $ LocGPR gpr
operandToLocation (PPC.Gprc_nor0 gpr) = Just (LocGPR gpr)
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
  uninterpretedFunctions _proxy = PPCS.uninterpretedFunctions
  locationFuncInterpretation _proxy = locationFuncInterpretation

locationFuncInterpretation :: [(String, F.LocationFuncInterp PPC)]
locationFuncInterpretation =
  [ ("ppc.memri_reg", F.LocationFuncInterp interpMemriReg)
  , ("ppc.memrix_reg", F.LocationFuncInterp interpMemrixReg)
  , ("ppc.memrr_base", F.LocationFuncInterp interpMemrrBase)
  ]

opcodeToVoid :: ((o == PPC.Operand) ~ 'False) => PPC.Opcode o sh -> Void
opcodeToVoid x = case x of {}

-- This is a hack.
instance OrdF (PPC.Opcode (T.TemplatedOperand PPC)) where
  compareF = absurd . opcodeToVoid

instance ShowF (PPC.Opcode (T.TemplatedOperand PPC))

instance EnumF (PPC.Opcode (T.TemplatedOperand PPC)) where
  enumF = absurd . opcodeToVoid
  congruentF = absurd . opcodeToVoid

class (F.BuildOperandList (T.TemplatedArch PPC) sh, T.TemplatableOperands PPC sh) => BuildableAndTemplatable sh
instance (F.BuildOperandList (T.TemplatedArch PPC) sh, T.TemplatableOperands PPC sh) => BuildableAndTemplatable sh

weakenConstraint :: MapF.MapF (Witness BuildableAndTemplatable (PPC.Opcode PPC.Operand)) v
                 -> MapF.MapF (Witness (T.TemplatableOperands PPC) (PPC.Opcode PPC.Operand)) v
weakenConstraint = I.runIdentity . U.mapFMapBothM f
  where
    f :: Witness BuildableAndTemplatable a sh -> v sh -> I.Identity (Witness (T.TemplatableOperands PPC) a sh, v sh)
    f (Witness a) v = return (Witness a, v)

loadBaseSet :: forall sym.
               ( S.IsExprBuilder sym
               , S.IsSymInterface sym
               , U.HasLogCfg )
            => FilePath
            -> sym
            -> [Some (Witness BuildableAndTemplatable (PPC.Opcode PPC.Operand))]
            -> IO (T.BaseSet sym PPC)
loadBaseSet baseSetDir sym opcodes = do
  weakenConstraint <$> F.loadFormulasFromFiles sym toFP (C.Sub C.Dict) opcodes
  where
    toFP :: forall sh . PPC.Opcode PPC.Operand sh -> FilePath
    toFP op = baseSetDir </> showF op <.> "sem"

operandTypePPC :: PPC.Operand s -> BaseTypeRepr (A.OperandType PPC s)
operandTypePPC o =
  case o of
    PPC.F4rc {}              -> knownRepr
    PPC.F8rc {}              -> knownRepr
    PPC.Gprc {}              -> knownRepr
    PPC.Gprc_nor0 {}         -> knownRepr
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
                -> Location PPC s
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
    PPC.Memrr {}             -> L.error "Unexpected non-literal operand"
    PPC.Memri {}             -> L.error "Unexpected non-literal operand"
    PPC.Memrix {}            -> L.error "Unexpected non-literal operand"
    PPC.Memrix16 {}          -> L.error "Unexpected non-literal operand"
    PPC.Vrrc {}              -> L.error "Unexpected non-literal operand"
    PPC.Vsfrc {}             -> L.error "Unexpected non-literal operand"
    PPC.Vsrc {}              -> L.error "Unexpected non-literal operand"
    PPC.Vssrc {}             -> L.error "Unexpected non-literal operand"
    PPC.Gprc_nor0 {}         -> L.error "Unexpected non-literal operand"
    PPC.Gprc {}              -> L.error "Unexpected non-literal operand"
    PPC.F4rc {}              -> L.error "Unexpected non-literal operand"
    PPC.F8rc {}              -> L.error "Unexpected non-literal operand"
    PPC.Abscondbrtarget {}   -> L.error "Control flow transfer instructions unsupported"
    PPC.Absdirectbrtarget {} ->  L.error "Control flow transfer instructions unsupported"
    PPC.Condbrtarget {}      ->  L.error "Control flow transfer instructions unsupported"
    PPC.Directbrtarget {}    ->  L.error "Control flow transfer instructions unsupported"
    PPC.Calltarget {}        ->  L.error "Control flow transfer instructions unsupported"
    PPC.Abscalltarget {}     ->  L.error "Control flow transfer instructions unsupported"

instance CS.ConcreteArchitecture PPC where
  operandToSemanticView _proxy = operandToSemanticViewPPC
  registerizeInstruction = registerizeInstructionPPC
  operandType _proxy = operandTypePPC
  zeroState _proxy = PPCS.zeroState
  randomState _proxy = PPCS.randomState
  serialize _proxy = PPCS.serialize
  deserialize _proxy = PPCS.deserialize
  heuristicallyInterestingStates _proxy = PPCS.interestingStates
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
    PPC.Gprc gpr -> gprSemanticView gpr
    PPC.Gprc_nor0 gpr -> gprSemanticView gpr
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


type instance Pseudo PPC = PPCP.PseudoOpcode

instance ArchitectureWithPseudo PPC where
  assemblePseudo _ = PPCP.ppcAssemblePseudo (Proxy @PPC)

-- data Location :: BaseType -> * where
--   LocGPR :: PPC.GPR -> Location (BaseBVType 32)
--   LocIP :: Location (BaseBVType 32)
--   LocMSR :: Location (BaseBVType 32)
--   LocCTR :: Location (BaseBVType 32)
--   LocLNK :: Location (BaseBVType 32)
--   LocXER :: Location (BaseBVType 64)
--   LocCR :: Location (BaseBVType 32)
--   LocVSR :: PPC.VSReg -> Location (BaseBVType 128)
--   LocFPSCR :: Location (BaseBVType 32)
--   LocMem :: Location (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8))

-- instance Show (Location tp) where
--   show (LocGPR gpr) = show (pPrint gpr)
--   show LocIP = "IP"
--   show LocMSR = "MSR"
--   show LocCTR = "CTR"
--   show LocLNK = "LNK"
--   show LocXER = "XER"
--   show LocCR = "CR"
--   show (LocVSR vsr) = show (pPrint vsr)
--   show LocFPSCR = "FPSCR"
--   show LocMem = "Mem"
-- instance ShowF Location

-- $(return [])

-- fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
-- fakeTestEq x y = if x == y
--                  then Just Refl
--                  else Nothing

-- instance TestEquality Location where
--   testEquality = $(structuralTypeEquality [t|Location|]
--                    [ (ConType [t|PPC.GPR|], [|fakeTestEq|])
--                    , (ConType [t|PPC.VSReg|], [|fakeTestEq|])
--                    ]
--                   )

-- fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
-- fakeCompareF x y = fromOrdering (compare x y)

-- instance OrdF Location where
--   compareF = $(structuralTypeOrd [t|Location|]
--                [ (ConType [t|PPC.GPR|], [|fakeCompareF|])
--                , (ConType [t|PPC.VSReg|], [|fakeCompareF|])
--                ]
--               )

instance A.IsLocation (Location PPC) where
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
    _ -> fail ("Unexpected location prefix character: " ++ (c :[]))
