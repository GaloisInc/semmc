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
