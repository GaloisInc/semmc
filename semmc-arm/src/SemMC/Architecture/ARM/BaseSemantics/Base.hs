{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Base
    where

import SemMC.DSL


naturalBitSize :: Int
naturalBitSize = 32

naturalBV :: ExprType 'TBV
naturalBV = EBV naturalBitSize

naturalLitBV :: Integer -> Expr 'TBV
naturalLitBV n = LitBV naturalBitSize n


-- Note: all ARM documentation references are to:
--   ARM Architecture Reference Manual
--      ARMv8, for ARMv8-A architecture profile
--         Beta (ARM DDI 0487A.a (ID090413)
--         Copyright 2013  (release 04 Sep 2013)
