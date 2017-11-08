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
  defineOpcodeWithIP "XOR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvxor (Loc rS) (Loc rB))
  defineOpcodeWithIP "OR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvor (Loc rS) (Loc rB))
  defineOpcodeWithIP "AND" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvand (Loc rS) (Loc rB))
  defineOpcodeWithIP "NAND" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvand (Loc rS) (Loc rB)))
  defineOpcodeWithIP "NOR" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvor (Loc rS) (Loc rB)))
  defineOpcodeWithIP "EQV" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvnot (bvxor (Loc rS) (Loc rB)))
  defineOpcodeWithIP "ANDC" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvand (Loc rS) (bvnot (Loc rB)))
  defineOpcodeWithIP "ORC" $ do
    (rA, rS, rB) <- xform3
    defLoc rA (bvor (Loc rS) (bvnot (Loc rB)))
  defineOpcodeWithIP "EXTSB" $ do
    comment "Extend Sign Byte (X-form)"
    (rA, rS) <- xform2
    defLoc rA (sext (lowBits 8 (Loc rS)))
  defineOpcodeWithIP "EXTSH" $ do
    comment "Extend Sign Halfword (X-form)"
    (rA, rS) <- xform2
    defLoc rA (sext (lowBits 16 (Loc rS)))
  defineOpcodeWithIP "SLW" $ do
    comment "Shift Left Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    defLoc rA (zext (bvshl w n))
  defineOpcodeWithIP "SRW" $ do
    comment "Shift Right Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    defLoc rA (zext (bvlshr w n))
  when (?bitSize == Size64) $ do
    defineOpcodeWithIP "SLD" $ do
      comment "Shift Left Doubleword (X-form)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits 6 (Loc rB))
      defLoc rA  (bvshl (Loc rS) n)
    defineOpcodeWithIP "SRD" $ do
      comment "Shift Right Doubleword (X-form)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits64 6 (Loc rB))
      defLoc rA (bvlshr (Loc rS) n)
    defineOpcodeWithIP "EXTSW" $ do
      comment "Extend Sign Word (X-form)"
      (rA, rS) <- xform2
      defLoc rA (sext (lowBits 32 (Loc rS)))


    defineOpcodeWithIP "RLDICR" $ do
      (rA, sh, mb, rS) <- mdform4
      -- n = sh
      -- b = mb
      let k = 64
      let r = rotl k (Loc rS) (zext (Loc sh))
      let m = mask 64 (LitBV k 0) (zext (Loc mb))
      defLoc rA (bvand r m)
