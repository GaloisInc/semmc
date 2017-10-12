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

floatingPoint :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPoint = do
  defineOpcode "FADD" $ do
    (frT, frA, frB) <- aform8
    defLoc frT (fadd64 (Loc frA) (Loc frB))

  defineOpcode "FADDS" $ do
    (frT, frA, frB) <- aform4
    defLoc frT (fadd32 (Loc frA) (Loc frB))
