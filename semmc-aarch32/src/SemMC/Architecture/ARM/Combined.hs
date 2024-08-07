-- This module creates a set of definitions
-- that represent the combined set of A32 and T32 Opcodes, Operands,
-- and locations.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.ARM.Combined where

import           Data.Data
import           Data.EnumF ( congruentF, EnumF, enumF, enumCompareF )
import           Data.Kind ( Type )
import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr
import           Data.Parameterized.Lift
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.TraversableFC
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.T32 as T32
import           GHC.TypeLits


data ARMOperand :: Symbol -> Type where
    A32Operand :: A32.Operand tp -> ARMOperand tp
    T32Operand :: T32.Operand tp -> ARMOperand tp
                  deriving (Typeable)

data ARMOpcode :: (Symbol -> Type) -> [Symbol] -> Type where
    A32Opcode :: (A32.Opcode A32.Operand sh)   -> ARMOpcode ARMOperand sh
    T32Opcode :: (T32.Opcode T32.Operand sh) -> ARMOpcode ARMOperand sh
                 deriving (Typeable)

instance Show (ARMOperand sh) where
    show (A32Operand x) = show x
    show (T32Operand x) = show x

instance Show (ARMOpcode ARMOperand sh) where
    show (A32Opcode x) = show x
    show (T32Opcode x) = show x

instance ShowF ARMOperand where
instance ShowF (ARMOpcode ARMOperand) where

-- instance (Typeable sh) => Data (ARMOpcode ARMOperand sh)
instance LiftF (ARMOpcode ARMOperand)
    where withLift _ _ _ = undefined

instance TestEquality (ARMOpcode ARMOperand) where
    testEquality (A32Opcode x) (A32Opcode y) = testEquality x y
    testEquality (T32Opcode x) (T32Opcode y) = testEquality x y
    testEquality _ _ = Nothing

instance Eq (ARMOpcode ARMOperand sh) where
    x == y = case compareF x y of
               EQF -> True
               _ -> False

instance Ord (ARMOpcode ARMOperand sh) where
    compare x y = case compareF x y of
                    LTF -> LT
                    EQF -> EQ
                    GTF -> GT
    -- compare (A32Opcode x) (A32Opcode y) = compare x y
    -- compare (A32Opcode x) (A32Opcode y) = compare x y
    -- compare (T32Opcode _) (A32Opcode _) = LT
    -- compare (T32Opcode _) (A32Opcode _) = LT

instance OrdF (ARMOpcode ARMOperand) where
    compareF = enumCompareF

instance EnumF (ARMOpcode ARMOperand) where
    -- T32 operands are smaller than A32 operands (arbitrarily, but also factually)
    enumF (A32Opcode x) = 1000000 + enumF x
    enumF (T32Opcode x) = enumF x

    congruentF opcode =
      case opcode of
        A32Opcode x -> congruentOpcodes A32Opcode x
        T32Opcode x -> congruentOpcodes T32Opcode x
      where
        -- Data.Set.NonEmpty doesn't provide a `map` function, so we have to
        -- map over a NonEmpty Set in a somewhat laborious fashion.
        congruentOpcodes ::
          forall opcode opcode' (sh :: [Symbol]).
          (EnumF opcode, Ord (opcode' sh)) =>
          (opcode sh -> opcode' sh) ->
          opcode sh ->
          NES.Set (opcode' sh)
        congruentOpcodes mkOpcode x =
          let (l, ll) = NES.view (congruentF x) in
          let l'  = mkOpcode l in
          let ll' = map mkOpcode (Set.toList ll) in
          NES.fromList l' ll'

-- ----------------------------------------------------------------------

data ARMOperandRepr (tp :: Symbol) where
    A32OperandRepr :: A32.OperandRepr tp -> ARMOperandRepr tp
    T32OperandRepr :: T32.OperandRepr tp -> ARMOperandRepr tp

instance TestEquality ARMOperandRepr where
    testEquality (A32OperandRepr x) (A32OperandRepr y) = testEquality x y
    testEquality (T32OperandRepr x) (T32OperandRepr y) = testEquality x y
    testEquality _ _ = Nothing

instance OrdF ARMOperandRepr where
    compareF (A32OperandRepr x) (A32OperandRepr y) = compareF x y
    compareF (T32OperandRepr x) (T32OperandRepr y) = compareF x y
    -- T32 operands are smaller than A32 operands (arbitrarily)
    compareF (A32OperandRepr _) (T32OperandRepr _) = GTF
    compareF (T32OperandRepr _) (A32OperandRepr _) = LTF

instance HasRepr (ARMOpcode ARMOperand) (SL.List ARMOperandRepr) where
    typeRepr (A32Opcode x) = fmapFC A32OperandRepr $ typeRepr x
    typeRepr (T32Opcode x) = fmapFC T32OperandRepr $ typeRepr x
