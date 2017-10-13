{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.FP (
  floatingPoint
  ) where

import Prelude hiding ( concat )
import Data.Parameterized.Some ( Some(..) )

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

fadd64 :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fadd64 e1 e2 = uf (EBV 64) "fp.add64" [ Some e1, Some e2 ]

fadd32 :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fadd32 e1 e2 = uf (EBV 32) "fp.add32" [ Some e1, Some e2 ]

fsub64 :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fsub64 e1 e2 = uf (EBV 64) "fp.sub64" [ Some e1, Some e2 ]

fsub32 :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
fsub32 e1 e2 = uf (EBV 32) "fp.sub32" [ Some e1, Some e2 ]

-- | Extract the single-precision part of a vector register
extractSingle :: Expr 'TBV -> Expr 'TBV
extractSingle = lowBits128 32

-- | Extend a single-precision value out to 128 bits
extendSingle :: Expr 'TBV -> Expr 'TBV
extendSingle = concat (LitBV 96 0x0)

-- | Extract the double-precision part of a vector register
extractDouble :: Expr 'TBV -> Expr 'TBV
extractDouble = lowBits128 64

-- | Extend a double-precision value out to 128 bits
extendDouble :: Expr 'TBV -> Expr 'TBV
extendDouble = concat (LitBV 64 0x0)

-- | Lift a two-operand operation to single-precision values
--
-- Or maybe better thought of as lifting a single precision operation onto 128
-- bit registers.
liftSingle2 :: Location 'TBV
            -- ^ The 128 bit location to define
            -> (Expr 'TBV -> Expr 'TBV -> Expr 'TBV)
            -- ^ An operation over 32 bit (single-precision) floats
            -> Expr 'TBV
            -- ^ 128 bit operand 1
            -> Expr 'TBV
            -- ^ 128-bit operand 2
            -> SemM 'Def ()
liftSingle2 loc operation op1 op2 = do
  defLoc loc (extendSingle (operation (extractSingle op1) (extractSingle op2)))

liftDouble2 :: Location 'TBV
            -> (Expr 'TBV -> Expr 'TBV -> Expr 'TBV)
            -> Expr 'TBV
            -> Expr 'TBV
            -> SemM 'Def ()
liftDouble2 loc operation op1 op2 = do
  defLoc loc (extendDouble (operation (extractDouble op1) (extractDouble op2)))

floatingPoint :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPoint = do
  defineOpcode "FADD" $ do
    (frT, frA, frB) <- aform
    liftDouble2 frT fadd64 (Loc frA) (Loc frB)

  defineOpcode "FADDS" $ do
    (frT, frA, frB) <- aform
    liftSingle2 frT fadd32 (Loc frA) (Loc frB)

  defineOpcode "FSUB" $ do
    (frT, frA, frB) <- aform
    liftDouble2 frT fsub64 (Loc frA) (Loc frB)

  defineOpcode "FSUBS" $ do
    (frT, frA, frB) <- aform
    liftSingle2 frT fsub32 (Loc frA) (Loc frB)
