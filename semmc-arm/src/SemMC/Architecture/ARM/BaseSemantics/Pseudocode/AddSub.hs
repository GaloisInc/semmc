-- | Pseudocode definitions of addition and subtraction from F2.6.2
-- (page F4-2423) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.AddSub
    ( addWithCarry
    )
    where

import Data.Parameterized.Some
import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.DSL

-- What4 only allows defined functions to return single values, so we have to
-- pack together the result and the NZCV bits into a single 36-bit vector. We 

addWithCarry_impl :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
                  -> Expr 'TBV
                     -- ^ 32-bit result concatenated with NZCV result bits
                     -- (E1-2292 or F2-2423)
addWithCarry_impl x y carry_in =
    let eres = bvadd (bvadd (extval x) (extval y)) (extval carry_in)
        extval = zext' (naturalBitSize+1)
        signBit = extract (naturalBitSize-1) (naturalBitSize-1)
        res = extract (naturalBitSize-1) 0 eres
        n = signBit res
        z = ite (bveq res (naturalLitBV 0)) (LitBV 1 1) (LitBV 1 0)
        c = extract naturalBitSize naturalBitSize eres
        v = bvand n (extract naturalBitSize naturalBitSize eres)
        nzcv = concat n $ concat z $ concat c v
    in concat ("addResult" =: res) ("addCarry" =: nzcv)

-- | Pseudocode AddWithCarry (E1-2292 or F2-2423)
addWithCarry' :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
              -> SemARM 'Def (Expr 'TBV)
                 -- ^ 32-bit result, NZCV result bits  (E1-2292 or F2-2423)
addWithCarry' x y carry_in = do
  --addLibraryFunction (LibraryFunction "addWithCarry")
  return (DefinedFunc (EBV 36) "addWithCarry" [Some x, Some y, Some carry_in])

addWithCarry :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
             -> SemARM 'Def (Expr 'TBV, Expr 'TBV)
addWithCarry x y carry_in = do
  res_nzcv <- addWithCarry' x y carry_in
  return (extract 31 0 res_nzcv, extract 35 32 res_nzcv)

