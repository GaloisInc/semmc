-- | Pseudocode definitions of bitstring manipulation from H5.3
-- (page AppxH-5069) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Bitstring
    ( isZeroBit
    )
    where

import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.DSL

isZeroBitLF :: LibraryFunctionDef '( '[ 'TBV], 'TBV)
isZeroBitLF =
  defineLibraryFunction "isZeroBit" (Arg "x" naturalBV :< Nil) $
    \x -> ite (bveq x (naturalLitBV 0)) (LitBV 1 0b1) (LitBV 1 0b0)

-- | Pseudocode IsZeroBit (H5.3, AppxH-5071) returns 1 if all of the
-- bits of x are zeros and 0 if any of them are ones.  Expects a
-- 32-bit input and returns a 1-bit output.
isZeroBit :: Expr 'TBV -> Expr 'TBV
isZeroBit x = lf isZeroBitLF (x :< Nil)
