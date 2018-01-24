-- | Evaluators for location functions in formula definitions (e.g., memri_reg)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Eval
    ( createSymbolicEntries
    , interpIsR15
    , interpImm12Reg
    , interpImm12RegExtractor
    , interpImm12OffsetExtractor
    , interpImm12AddFlgExtractor
    )
    where

import           Data.Int ( Int16, Int8 )
import qualified Data.Parameterized.List as PL
import           Data.Word
import qualified Dismantle.ARM as ARM
import qualified Dismantle.ARM.Operands as ARMOperands
import           Lang.Crucible.BaseTypes
import           SemMC.Architecture.ARM.Location
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F


-- | Uninterpreted function names are mangled in SimpleBuilder, so we need to
-- create extra entries to match their mangled names.
--
-- In particular, periods in names are converted to underscores.
--
-- This function creates copies of entries with periods in their names with the
-- escaped version as it appears in a SimpleBuilder symbolic function.  For
-- example, if there is an entry with the name @arm.foo@, this function retains
-- that entry in the input list and adds an additional entry under @arm_foo@.
createSymbolicEntries :: [(String, a)] -> [(String, a)]
createSymbolicEntries = foldr duplicateIfDotted []
  where
    duplicateIfDotted elt@(s, e) acc =
      case '.' `elem` s of
        False -> acc
        True ->
          let newElt = (map (\c -> if c == '.' then '_' else c) s, e)
          in newElt : elt : acc


-- | Extract the register value from an addrmode_imm12[_pre] via
-- the arm.imm12_reg user function.
interpImm12Reg :: forall sh s arm tp
                   . (L.IsLocation (Location arm), L.Location arm ~ Location arm)
                   => PL.List ARM.Operand sh
                 -> F.WrappedOperand arm sh s
                 -> BaseTypeRepr tp
                 -> L.Location arm tp
interpImm12Reg operands (F.WrappedOperand _orep ix) rep =
  case operands PL.!! ix of
    ARM.Addrmode_imm12_pre oprnd ->
      let loc :: Location arm (BaseBVType (ArchRegWidth arm))
          loc = LocGPR $ ARMOperands.addrModeImm12Register oprnd
      in case () of
        _ | Just Refl <- testEquality (L.locationType loc) rep -> loc
          | otherwise -> error ("Invalid return type for location function 'imm12_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

interpImm12RegExtractor :: ARMOperands.AddrModeImm12 -> ARMOperands.GPR
interpImm12RegExtractor = ARMOperands.addrModeImm12Register

interpImm12OffsetExtractor :: ARMOperands.AddrModeImm12 -> Int16
interpImm12OffsetExtractor = fromInteger . toInteger . ARMOperands.addrModeImm12Immediate

interpImm12AddFlgExtractor :: ARMOperands.AddrModeImm12 -> Int8
interpImm12AddFlgExtractor = fromInteger . toInteger . ARMOperands.addrModeImm12Add


class InterpIsR15 a where
  interpIsR15 :: a -> Bool

instance InterpIsR15 ARMOperands.GPR where
    interpIsR15 gprReg = ARMOperands.unGPR gprReg == 15

instance InterpIsR15 (Maybe ARMOperands.GPR) where
  interpIsR15 mr =
    case mr of
      Nothing -> True
      Just r -> interpIsR15 r
