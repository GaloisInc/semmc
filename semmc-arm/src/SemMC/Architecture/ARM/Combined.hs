-- | The ARM has two execution modes: A32 and T32 (actually, there are
-- 4, with the addition of T32EE and Jazelle, but those are not
-- currently supported).  This module creates a set of definitions
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
{-# LANGUAGE UndecidableInstances #-} -- KWQ
{-# LANGUAGE AllowAmbiguousTypes #-} -- KWQ

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module SemMC.Architecture.ARM.Combined
    -- (
    -- )
    where

import           Data.EnumF ( congruentF, EnumF, enumF, enumCompareF )
import qualified Data.Foldable as F
import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr
import           Data.Parameterized.Lift
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.Map
import           Data.Parameterized.TraversableFC
import qualified Data.Set.NonEmpty as NES
import           Data.Word ( Word8, Word32 )
import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMOperands
import qualified Dismantle.Thumb as ThumbDis
import qualified Dismantle.Thumb.Operands as ThumbOperands
import           GHC.TypeLits
import           Lang.Crucible.BaseTypes
import           Language.Haskell.TH.Syntax ( Lift(..) )
import qualified SemMC.Architecture as A
import Data.Data
import Data.Typeable
import Data.Proxy -- ( Proxy(..) )

-- data ARMOperand :: Symbol -> * where
data ARMOperand (tp :: Symbol) where
    A32Operand :: ARMDis.Operand tp -> ARMOperand tp
    T32Operand :: ThumbDis.Operand tp -> ARMOperand tp
                  deriving (Typeable)

-- data ARMOpcode :: (Symbol -> *) -> [Symbol] -> * where
data ARMOpcode (o :: Symbol -> *) (sh :: [Symbol]) where
    A32Opcode :: (ARMDis.Opcode ARMDis.Operand sh)   -> ARMOpcode ARMOperand sh
    T32Opcode :: (ThumbDis.Opcode ThumbDis.Operand sh) -> ARMOpcode ARMOperand sh
                 deriving (Typeable)

instance Show (ARMOperand sh) where
    show (A32Operand x) = show x
    show (T32Operand x) = show x

instance Show (ARMOpcode ARMOperand sh) where
    show (A32Opcode x) = show x
    show (T32Opcode x) = show x

instance ShowF ARMOperand where
instance ShowF (ARMOpcode ARMOperand) where

instance (Typeable sh) => Data (ARMOpcode ARMOperand sh)
-- instance (Typeable sh) => Lift (ARMOpcode ARMOperand sh)
-- instance LiftF (ARMOpcode ARMOperand)
--     where withLift :: Typeable tp => p (ARMOpcode ARMOperand) -> q tp -> ((Typeable tp, Lift (ARMOpcode ARMOperand tp)) => a) -> a
--           withLift _ _ x = x
--     -- where withLift :: (Lift ((ARMOpcode o) (tp :: [Symbol]))) => p (ARMOpcode o) -> q tp -> (Lift (ARMOpcode o tp) => a) -> a
--     --       withLift _ _ x = x
-- -- instance (Typeable o, Typeable tp) => LiftF (ARMOpcode o)
-- --     where withLift :: (Lift ((ARMOpcode o) tp), Typeable tp) => p (ARMOpcode o) -> q tp -> (Lift ((ARMOpcode o) tp) => a) -> a
-- --           withLift _ _ x = x
--     -- where
--     -- withLift _ _ (A32Operand x) = A32Operand x
-- -- instance (Lift tp) => LiftF (ARMOpcode ARMOperand)
-- --     where
-- --       withLift (Proxy :: Proxy (ARMOpcode ARMOperand)) (Proxy :: Proxy [Symbol]) x = x
-- --       -- withLift (Proxy :: Proxy (ARMOpcode ARMOperand)) (Proxy :: sh) (x :: ARMOpcode ARMOperand sh) = x
-- -- -- instance LiftF (ARMOpcode ARMOperand) where
-- -- --     withLift _ _ (Typeable x = x
-- --     -- withLift (proxy (ARMOpcode ARMOperand)) (Proxy tp) x = x
-- --     --                                             -- (Lift (ARMOpcode ARMOperand tp) => x) = x

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

    congruentF (A32Opcode x) = let ll = fmap A32Opcode $ F.toList (congruentF x) in NES.fromList (head ll) (tail ll)
    congruentF (T32Opcode x) = let ll = fmap T32Opcode $ F.toList (congruentF x) in NES.fromList (head ll) (tail ll)

-- ----------------------------------------------------------------------

data ARMOperandRepr (tp :: Symbol) where
    A32OperandRepr :: ARMDis.OperandRepr tp -> ARMOperandRepr tp
    T32OperandRepr :: ThumbDis.OperandRepr tp -> ARMOperandRepr tp

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

    {-
instance (KnownRepr (SL.List ARMOperandRepr) ty) => HasRepr (ARMOpcode op) (SL.List ARMOperandRepr) where
  typeRepr (A32Opcode (x :: ARMDis.Opcode ARMDis.Operand ty)) = knownRepr
  typeRepr (T32Opcode y) = knownRepr
    -}
