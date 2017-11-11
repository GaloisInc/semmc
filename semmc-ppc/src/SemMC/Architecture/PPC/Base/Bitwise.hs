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
  defineOpcodeWithIP "ANDIo" $ do
    comment "AND Immediate (D-form)"
    (rT, rA, ui) <- dformu
    let res = bvand (Loc rA) (zext (Loc ui))
    defLoc rT res
    input cr
    input xer
    defLoc cr (cmpImm bvslt bvsgt (LitBV 3 0x0) (naturalLitBV 0x0) res)
  defineOpcodeWithIP "ANDISo" $ do
    comment "AND Immediage Shifted (D-form)"
    (rT, rA, ui) <- dformu
    let res = bvand (Loc rA) (zext (concat (Loc ui) (LitBV 16 0x0)))
    defLoc rT res
    input cr
    input xer
    defLoc cr (cmpImm bvslt bvsgt (LitBV 3 0x0) (naturalLitBV 0x0) res)
  defineOpcodeWithIP "XOR" $ do
    comment "XOR (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvxor (Loc rS) (Loc rB)
    defLoc rA res
    defineRCVariant "XORo" res $ do
      comment "XOR (X-form, RC=1)"
  defineOpcodeWithIP "OR" $ do
    comment "OR (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvor (Loc rS) (Loc rB)
    defLoc rA res
    defineRCVariant "ORo" res $ do
      comment "OR (X-form, RC=1)"
  defineOpcodeWithIP "AND" $ do
    comment "AND (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvand (Loc rS) (Loc rB)
    defLoc rA res
    defineRCVariant "ANDo" res $ do
      comment "AND (X-form, RC=1)"
  defineOpcodeWithIP "NAND" $ do
    comment "NAND (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvnot (bvand (Loc rS) (Loc rB))
    defLoc rA res
    defineRCVariant "NANDo" res $ do
      comment "NAND (X-form, RC=1)"
  defineOpcodeWithIP "NOR" $ do
    comment "NOR (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvnot (bvor (Loc rS) (Loc rB))
    defLoc rA res
    defineRCVariant "NORo" res $ do
      comment "NOR (X-form, RC=1)"
  defineOpcodeWithIP "EQV" $ do
    comment "EQV (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvnot (bvxor (Loc rS) (Loc rB))
    defLoc rA res
    defineRCVariant "EQVo" res $ do
      comment "EQV (X-form, RC=1)"
  defineOpcodeWithIP "ANDC" $ do
    comment "AND with Complement (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvand (Loc rS) (bvnot (Loc rB))
    defLoc rA res
    defineRCVariant "ANDCo" res $ do
      comment "AND with Complement (X-form, RC=1)"
  defineOpcodeWithIP "ORC" $ do
    comment "OR with Complement (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let res = bvor (Loc rS) (bvnot (Loc rB))
    defLoc rA res
    defineRCVariant "ORCo" res $ do
      comment "OR with Complement (X-form, RC=1)"
  defineOpcodeWithIP "EXTSB" $ do
    comment "Extend Sign Byte (X-form, RC=0)"
    (rA, rS) <- xform2
    let res = sext (lowBits 8 (Loc rS))
    defLoc rA res
    defineRCVariant "EXTSBo" res $ do
      comment "Extend Sign Byte (X-form, RC=1)"
  defineOpcodeWithIP "EXTSH" $ do
    comment "Extend Sign Halfword (X-form, RC=0)"
    (rA, rS) <- xform2
    let res = sext (lowBits 16 (Loc rS))
    defLoc rA res
    defineRCVariant "EXTSHo" res $ do
      comment "Extend Sign Halfword (X-form, RC=1)"
  defineOpcodeWithIP "SLW" $ do
    comment "Shift Left Word (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    let res = zext (bvshl w n)
    defLoc rA res
    defineRCVariant "SLWo" res $ do
      comment "Shift Left Word (X-form, RC=1)"
  defineOpcodeWithIP "SRW" $ do
    comment "Shift Right Word (X-form, RC=0)"
    (rA, rS, rB) <- xform3
    let n = zext' 32 (lowBits 5 (Loc rB))
    let w = lowBits 32 (Loc rS)
    let res = zext (bvlshr w n)
    defLoc rA res
    defineRCVariant "SRWo" res $ do
      comment "Shift Right Word (X-form, RC=1)"
  defineOpcodeWithIP "CNTLZW" $ do
    comment "Count Leading Zeros Word (X-form, RC=0)"
    (rA, rS) <- xform2
    let res = zext (bvclz (lowBits 32 (Loc rS)))
    defLoc rA res
    defineRCVariant "CNTLZWo" res $ do
      comment "Count Leading Zeros Word (X-form, RC=1)"
  defineOpcodeWithIP "POPCNTW" $ do
    comment "Population Count Words (X-form)"
    (rA, rS) <- xform2
    let lowRes = bvpopcnt (lowBits 32 (Loc rS))
    case ?bitSize of
      Size32 -> defLoc rA lowRes
      Size64 -> do
        let highRes = bvpopcnt (highBits 32 (Loc rS))
        defLoc rA (concat highRes lowRes)

  rotates
  temporary

  when (?bitSize == Size64) $ do
    defineOpcodeWithIP "SLD" $ do
      comment "Shift Left Doubleword (X-form, RC=0)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits 6 (Loc rB))
      let res = bvshl (Loc rS) n
      defLoc rA res
      defineRCVariant "SLDo" res $ do
        comment "Shift Left Doubleword (x-form, RC=1)"
    defineOpcodeWithIP "SRD" $ do
      comment "Shift Right Doubleword (X-form, RC=0)"
      (rA, rS, rB) <- xform3
      let n = zext (lowBits64 6 (Loc rB))
      let res = bvlshr (Loc rS) n
      defLoc rA res
      defineRCVariant "SRDo" res $ do
        comment "Shift Right Doubleword (X-form, RC=1)"
    defineOpcodeWithIP "EXTSW" $ do
      comment "Extend Sign Word (X-form, RC=0)"
      (rA, rS) <- xform2
      let res = sext (lowBits 32 (Loc rS))
      defLoc rA res
      defineRCVariant "EXTSWo" res $ do
        comment "Extend Sign Word (X-form, RC=1)"
    defineOpcodeWithIP "CNTLZD" $ do
      comment "Count Leading Zeros Doubleword (X-form, RC=0)"
      (rA, rS) <- xform2
      let res = bvclz (Loc rS)
      defLoc rA res
      defineRCVariant "CNTLZDo" res $ do
        comment "Count Leading Zeros Doubleword (X-form, RC=1)"

    defineOpcodeWithIP "POPCNTD" $ do
      comment "Population Count Doubleword (X-form)"
      (rA, rS) <- xform2
      defLoc rA (bvpopcnt (Loc rS))

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
    comment "Rotate Left Word Immediate then AND with Mask (M-form, RC=0)"
    (rA, sh, mb, me, rS) <- mform5i
    let k = 32
    let n = zext' k (Loc sh)
    let r = rotl k (lowBits k (Loc rS)) n
    let m = mask k (zext' k (Loc mb)) (zext' k (Loc me))
    let res = zext (bvand r m)
    defLoc rA res
    defineRCVariant "RLWINMo" res $ do
      comment "Rotate Left Word Immediate then AND with Mask (M-form, RC=1)"

  defineOpcodeWithIP "RLWNM" $ do
    comment "Rotate Left Word then AND with Mask (M-form, RC=0)"
    (rA, mb, me, rS, rB) <- mform5r
    let k = 32
    let n = zext' k (lowBits 5 (Loc rB))
    let r = rotl k (lowBits k (Loc rS)) n
    let m = mask k (zext' k (Loc mb)) (zext' k (Loc me))
    let res = zext (bvand r m)
    defLoc rA res
    defineRCVariant "RLWNMo" res $ do
      comment "Rotate Left Word then AND with Mask (M-form, RC=1)"

  defineOpcodeWithIP "RLWIMI" $ do
    comment "Rotate Left Word Immediate then Mask Insert (M-form, RC=0)"
    (rA, sh, mb, me, rS) <- mform5i
    input rA

    let k = 32
    let n = zext' k (Loc sh)
    let r = rotl k (lowBits k (Loc rS)) n
    let m = mask k (zext' k (Loc mb)) (zext' k (Loc me))
    let res = zext (bvor (bvand r m) (bvand (lowBits 32 (Loc rA)) (bvnot m)))
    defLoc rA res
    defineRCVariant "RLWIMIo" res $ do
      comment "Rotate Left Word Immediate then Mask Insert (M-form, RC=1)"

  when (?bitSize == Size64) $ do

    defineOpcodeWithIP "RLDICR" $ do
      comment "Rotate Left Doubleword Immediate then Clear Right (MD-form, RC=0)"
      (rA, sh, mb, rS) <- mdform4
      -- n = sh
      -- b = mb
      let k = 64
      let r = rotl k (Loc rS) (zext (Loc sh))
      let m = mask k (LitBV k 0) (zext (Loc mb))
      let res = bvand r m
      defLoc rA res
      defineRCVariant "RLDICRo" res $ do
        comment "Rotate Left Doubleword Immediate then Clear Right (MC-form, RC=1)"

    defineOpcodeWithIP "RLDICL" $ do
      comment "Rotate Left Doubleword Immediate then Clear Left (MD-form, RC=0)"
      (rA, sh, mb, rS) <- mdform4
      let k = 64
      let r = rotl k (Loc rS) (zext (Loc sh))
      let m = mask k (zext (Loc mb)) (LitBV k 63)
      let res = bvand r m
      defLoc rA res
      defineRCVariant "RLDICLo" res $ do
        comment "Rotate Left Doubleword Immediate then Clear Left (MD-form, RC=1)"

    defineOpcodeWithIP "RLDIC" $ do
      comment "Rotate Left Doubleword Immediate then Clear (MD-form, RC=0)"
      (rA, sh, mb, rS) <- mdform4
      let k = 64
      let r = rotl k (Loc rS) (zext (Loc sh))
      let m = mask k (zext (Loc mb)) (zext (bvsub (LitBV 6 63) (Loc sh)))
      let res = bvand r m
      defLoc rA res
      defineRCVariant "RLDICo" res $ do
        comment "Rotate Left Doubleword Immediate then Clear (MD-form, RC=1)"

    defineOpcodeWithIP "RLDCL" $ do
      comment "Rotate Left Doubleword then Clear Left (MDS-form, RC=0)"
      (rA, mb, rS, rB) <- mdsform4
      let n = lowBits 6 (Loc rB)
      let k = 64
      let r = rotl k (Loc rS) (zext n)
      let m = mask k (zext (Loc mb)) (LitBV k 63)
      let res = bvand r m
      defLoc rA res
      defineRCVariant "RLDCLo" res $ do
        comment "Rotate Left Doubleword then Clear Left (MDS-form, RC=1)"

    defineOpcodeWithIP "RLDCR" $ do
      comment "Rotate Left Doubleword then Clear Right (MDS-form, RC=0)"
      (rA, mb, rS, rB) <- mdsform4
      let n = lowBits 6 (Loc rB)
      let k = 64
      let r = rotl k (Loc rS) (zext n)
      let m = mask k (LitBV k 0) (zext (Loc mb))
      let res = bvand r m
      defLoc rA res
      defineRCVariant "RLDCRo" res $ do
        comment "Rotate Left Doubleword then Clear Right (MDS-form, RC=1)"

    defineOpcodeWithIP "RLDIMI" $ do
      comment "Rotate Left Doubleword Immediate then Mask Insert (MD-form, RC=0)"
      (rA, sh, mb, rS) <- mdform4
      input rA
      let k = 64
      let n = Loc sh
      let r = rotl k (Loc rS) (zext n)
      let b = Loc mb
      let m = mask k (zext b) (zext (bvsub (LitBV 6 63) n))
      let res = bvor (bvand r m) (bvand (Loc rA) (bvnot m))
      defLoc rA res
      defineRCVariant "RLDIMIo" res $ do
        comment "Rotate Left Doubleword Immediate then Mask Insert (MD-form, RC=1)"
