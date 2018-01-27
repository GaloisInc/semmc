{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Base
    where

import Data.Bits
import Data.Word
import SemMC.DSL


data ArchSubtype = InstrSet_A32 | InstrSet_T32 | InstrSet_Jazelle | InstrSet_T32EE
                 deriving (Eq, Show)

naturalBitSize :: Int
naturalBitSize = 32

naturalZero :: Word32
naturalZero = zeroBits

naturalBV :: ExprType 'TBV
naturalBV = EBV naturalBitSize

naturalLitBV :: Integer -> Expr 'TBV
naturalLitBV n = LitBV naturalBitSize n


-- Note: all ARM documentation references are to:
--   ARM Architecture Reference Manual
--      ARMv8, for ARMv8-A architecture profile
--         Beta (ARM DDI 0487A.a (ID090413)
--         Copyright 2013  (release 04 Sep 2013)
