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
  defineOpcodeWithIP "CNTLZW" $ do
    comment "Count Leading Zeros Word (X-form)"
    (rA, rS) <- xform2
    defLoc rA (zext (bvclz (lowBits 32 (Loc rS))))

  rotates
  temporary

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
    defineOpcodeWithIP "CNTLZD" $ do
      comment "Count Leading Zeros Doubleword (X-form)"
      (rA, rS) <- xform2
      defLoc rA (bvclz (Loc rS))

-- | Operations we are temporarily defining, but would like to eventually learn
temporary :: (?bitSize :: BitSize) => SemM 'Top ()
temporary = do
  defineOpcodeWithIP "ORI" $ do
    (rT, rA, ui) <- dformu
    defLoc rT (bvor (Loc rA) (zext (Loc ui)))

  defineOpcodeWithIP "ORIS" $ do
    (rT, rA, ui) <- dformu
    defLoc rT (bvor (Loc rA) (zext (concat (Loc ui) (LitBV 16 0x0))))

rotates :: (?bitSize :: BitSize) => SemM 'Top ()
rotates = do
  defineOpcodeWithIP "RLWINM" $ do
    comment "Rotate Left Word Immediate then AND with Mask (M-form)"
    (rA, sh, mb, me, rS) <- mform5i
    let k = 32
    let n = zext' k (Loc sh)
    let r = rotl k (lowBits k (Loc rS)) n
    let m = mask k (zext' k (Loc mb)) (zext' k (Loc me))
    defLoc rA (zext (bvand r m))

  defineOpcodeWithIP "RLWNM" $ do
    comment "Rotate Left Word then AND with Mask (M-form)"
    (rA, mb, me, rS, rB) <- mform5r
    let k = 32
    let n = zext' k (lowBits 5 (Loc rB))
    let r = rotl k (lowBits k (Loc rS)) n
    let m = mask k (zext' k (Loc mb)) (zext' k (Loc me))
    defLoc rA (zext (bvand r m))

  defineOpcodeWithIP "RLWIMI" $ do
    comment "Rotate Left Word Immediate then Mask Insert (M-form)"
    (rA, sh, mb, me, rS) <- mform5i
    input rA

    let k = 32
    let n = zext' k (Loc sh)
    let r = rotl k (lowBits k (Loc rS)) n
    let m = mask k (zext' k (Loc mb)) (zext' k (Loc me))
    defLoc rA (zext (bvor (bvand r m) (bvand (lowBits 32 (Loc rA)) (bvnot m))))

  when (?bitSize == Size64) $ do

    defineOpcodeWithIP "RLDICR" $ do
      comment "Rotate Left Doubleword Immediate then Clear Right (MD-form)"
      (rA, sh, mb, rS) <- mdform4
      -- n = sh
      -- b = mb
      let k = 64
      let r = rotl k (Loc rS) (zext (Loc sh))
      let m = mask k (LitBV k 0) (zext (Loc mb))
      defLoc rA (bvand r m)

    defineOpcodeWithIP "RLDICL" $ do
      comment "Rotate Left Doubleword Immediate then Clear Left (MD-form)"
      (rA, sh, mb, rS) <- mdform4
      let k = 64
      let r = rotl k (Loc rS) (zext (Loc sh))
      let m = mask k (zext (Loc mb)) (LitBV k 63)
      defLoc rA (bvand r m)

    defineOpcodeWithIP "RLDIC" $ do
      comment "Rotate Left Doubleword Immediate then Clear (MD-form)"
      (rA, sh, mb, rS) <- mdform4
      let k = 64
      let r = rotl k (Loc rS) (zext (Loc sh))
      let m = mask k (zext (Loc mb)) (zext (bvsub (LitBV 6 63) (Loc sh)))
      defLoc rA (bvand r m)

    defineOpcodeWithIP "RLDCL" $ do
      comment "Rotate Left Doubleword then Clear Left (MDS-form)"
      (rA, mb, rS, rB) <- mdsform4
      let n = lowBits 6 (Loc rB)
      let k = 64
      let r = rotl k (Loc rS) (zext n)
      let m = mask k (zext (Loc mb)) (LitBV k 63)
      defLoc rA (bvand r m)

    defineOpcodeWithIP "RLDCR" $ do
      comment "Rotate Left Doubleword then Clear Right (MDS-form)"
      (rA, mb, rS, rB) <- mdsform4
      let n = lowBits 6 (Loc rB)
      let k = 64
      let r = rotl k (Loc rS) (zext n)
      let m = mask k (LitBV k 0) (zext (Loc mb))
      defLoc rA (bvand r m)

    defineOpcodeWithIP "RLDIMI" $ do
      comment "Rotate Left Doubleword Immediate then Mask Insert (MD-form)"
      (rA, sh, mb, rS) <- mdform4
      input rA
      let k = 64
      let n = Loc sh
      let r = rotl k (Loc rS) (zext n)
      let b = Loc mb
      let m = mask k (zext b) (zext (bvsub (LitBV 6 63) n))
      defLoc rA (bvor (bvand r m) (bvand (Loc rA) (bvnot m)))
