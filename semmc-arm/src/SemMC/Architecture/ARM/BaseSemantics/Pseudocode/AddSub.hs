-- | Pseudocode definitions of addition and subtraction from F2.6.2
-- (page F4-2423) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.AddSub
    ( addWithCarry
    )
    where

import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.DSL


-- | Pseudocode AddWithCarry (E1-2292 or F2-2423)
addWithCarry :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
             -> (Expr 'TBV, Expr 'TBV)
                -- ^ 32-bit result, NZCV result bits  (E1-2292 or F2-2423)
addWithCarry x y carry_in =
    let eres = bvadd (bvadd (extval x) (extval y)) (extval carry_in)
        extval = zext' (naturalBitSize+1)
        signBit = extract (naturalBitSize-1) (naturalBitSize-1)
        res = extract (naturalBitSize-1) 0 eres
        n = signBit res
        z = ite (bveq res (naturalLitBV 0)) (LitBV 1 1) (LitBV 1 0)
        c = extract naturalBitSize naturalBitSize eres
        v = bvand n (extract naturalBitSize naturalBitSize eres)
    in ("addResult" =: res, "addCarry" =: (concat n $ concat z $ concat c v))
