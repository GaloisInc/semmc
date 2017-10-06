{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Bitwise (
  baseBitwise
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

baseBitwise :: (?bitSize :: BitSize) => SemM 'Top ()
baseBitwise = do
  defineOpcode "XOR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvxor (Loc rS) (Loc rB))
  defineOpcode "OR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvor (Loc rS) (Loc rB))
  defineOpcode "AND" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvand (Loc rS) (Loc rB))
  defineOpcode "NAND" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvand (Loc rS) (Loc rB)))
  defineOpcode "NOR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvor (Loc rS) (Loc rB)))
  defineOpcode "EQV" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvxor (Loc rS) (Loc rB)))
  defineOpcode "ANDC" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvand (Loc rS) (bvnot (Loc rB)))
  defineOpcode "ORC" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvor (Loc rS) (bvnot (Loc rB)))
  defineOpcode "EXTSB" $ do
    comment "Extend Sign Byte (X-form)"
    (rA, rS) <- xform2
    defLoc rA (sext (lowBits 8 (Loc rS)))
  defineOpcode "EXTSH" $ do
    comment "Extend Sign Halfword (X-form)"
    (rA, rS) <- xform2
    defLoc rA (sext (lowBits 16 (Loc rS)))
  defineOpcode "SLW" $ do
    comment "Shift Left Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    defLoc rA (zext (bvshl w n))
  defineOpcode "SRW" $ do
    comment "Shift Right Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    defLoc rA (zext (bvlshr w n))
  when (?bitSize == Size64) $ do
    defineOpcode "SLD" $ do
      comment "Shift Left Doubleword (X-form)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits 6 (Loc rB))
      defLoc rA  (bvshl (Loc rS) n)
    defineOpcode "SRD" $ do
      comment "Shift Right Doubleword (X-form)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits64 6 (Loc rB))
      defLoc rA (bvlshr (Loc rS) n)
    defineOpcode "EXTSW" $ do
      comment "Extend Sign Word (X-form)"
      (rA, rS) <- xform2
      defLoc rA (sext (lowBits 32 (Loc rS)))
