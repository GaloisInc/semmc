{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Bitwise (
  baseBitwise
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )

import qualified Dismantle.PPC as P

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
  definePPCOpcode P.XOR xform3c $ \rA rB rS -> do
    comment "XOR (X-form, RC=0)"
    let res = bvxor (Loc rS) (Loc rB)
    defLoc rA res
    definePPCOpcodeRC P.XORo res $ do
      comment "XOR (X-form, RC=1)"
  definePPCOpcode P.OR xform3c $ \rA rB rS -> do
    comment "OR (X-form, RC=0)"
    let res = bvor (Loc rS) (Loc rB)
    defLoc rA res
    definePPCOpcodeRC P.ORo res $ do
      comment "OR (X-form, RC=1)"
  definePPCOpcode P.AND xform3c $ \rA rB rS -> do
    comment "AND (X-form, RC=0)"
    let res = bvand (Loc rS) (Loc rB)
    defLoc rA res
    definePPCOpcodeRC P.ANDo res $ do
      comment "AND (X-form, RC=1)"
  definePPCOpcode P.NAND xform3c $ \rA rB rS -> do
    comment "NAND (X-form, RC=0)"
    let res = bvnot (bvand (Loc rS) (Loc rB))
    defLoc rA res
    definePPCOpcodeRC P.NANDo res $ do
      comment "NAND (X-form, RC=1)"
  definePPCOpcode P.NOR xform3c $ \rA rB rS -> do
    comment "NOR (X-form, RC=0)"
    let res = bvnot (bvor (Loc rS) (Loc rB))
    defLoc rA res
    definePPCOpcodeRC P.NORo res $ do
      comment "NOR (X-form, RC=1)"
  definePPCOpcode P.EQV xform3c $ \rA rB rS -> do
    comment "EQV (X-form, RC=0)"
    let res = bvnot (bvxor (Loc rS) (Loc rB))
    defLoc rA res
    definePPCOpcodeRC P.EQVo res $ do
      comment "EQV (X-form, RC=1)"
  definePPCOpcode P.ANDC xform3c $ \rA rB rS -> do
    comment "AND with Complement (X-form, RC=0)"
    let res = bvand (Loc rS) (bvnot (Loc rB))
    defLoc rA res
    definePPCOpcodeRC P.ANDCo res $ do
      comment "AND with Complement (X-form, RC=1)"
  definePPCOpcode P.ORC xform3c $ \rA rB rS -> do
    comment "OR with Complement (X-form, RC=0)"
    let res = bvor (Loc rS) (bvnot (Loc rB))
    defLoc rA res
    definePPCOpcodeRC P.ORCo res $ do
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
  definePPCOpcode P.SLW xform3c $ \rA rB rS -> do
    comment "Shift Left Word (X-form, RC=0)"
    let n = zext' 32 (lowBits 6 (Loc rB))
    let w = lowBits 32 (Loc rS)
    let res = zext (bvshl w n)
    defLoc rA res
    definePPCOpcodeRC P.SLWo res $ do
      comment "Shift Left Word (X-form, RC=1)"
  definePPCOpcode P.SRW xform3c $ \rA rB rS -> do
    comment "Shift Right Word (X-form, RC=0)"
    let n = zext' 32 (lowBits 6 (Loc rB))
    let w = lowBits 32 (Loc rS)
    let res = zext (bvlshr w n)
    defLoc rA res
    definePPCOpcodeRC P.SRWo res $ do
      comment "Shift Right Word (X-form, RC=1)"
  defineOpcodeWithIP "SRAWI" $ do
    comment "Shift Right Algebraic Word Immediate (X-form, RC=0)"
    rA <- param "rA" gprc naturalBV
    sh <- param "sh" u5imm (EBV 5)
    rS <- param "rS" gprc naturalBV
    input sh
    input rS
    input xer
    let w = lowBits 32 (Loc rS)
    let res = sext (bvashr w (zext' 32 (Loc sh)))
    let nShiftedOutBits = bvsub (LitBV 32 32) (zext' 32 (Loc sh))
    let shiftedOutBits = bvlshr (bvshl w nShiftedOutBits) nShiftedOutBits
    let hasShiftedOutOnes = bvne shiftedOutBits (LitBV 32 0x0)
    let s = highBits 1 res
    defLoc rA res
    defLoc xer (updateXER CA (Loc xer) (ite hasShiftedOutOnes s (LitBV 1 0x0)))
    defineRCVariant "SRAWIo" res $ do
      comment "Shift Right Algebraic Word Immediate (X-form, RC=1)"
  definePPCOpcode P.SRAW xform3c $ \rA rB rS -> do
    comment "Shift Right Algebraic Word (X-form, RC=0)"
    input xer
    let n = lowBits 6 (Loc rB)
    let w = lowBits 32 (Loc rS)
    let r = sext (bvashr w (zext' 32 n))
    let s = highBits 1 w
    let nShiftedOutBits = ite
          (bvuge n (LitBV 6 32))
          (LitBV 32 0)
          (bvsub (LitBV 32 32) (zext' 32 n))
    let shiftedOutBits = bvlshr (bvshl w nShiftedOutBits) nShiftedOutBits
    let hasShiftedOutOnes = bvne shiftedOutBits (LitBV 32 0x0)
    defLoc rA r
    defLoc xer (updateXER CA (Loc xer) (ite hasShiftedOutOnes s (LitBV 1 0x0)))
    definePPCOpcodeRC P.SRAWo r $ do
      comment "Shift Right Algebraic Word (X-form, RC=1)"

  rotates
  special
  crOps
  temporary

  when (?bitSize == Size64) $ do
    definePPCOpcode P.SLD xform3c $ \rA rB rS -> do
      comment "Shift Left Doubleword (X-form, RC=0)"
      let n = zext (lowBits 6 (Loc rB))
      let res = bvshl (Loc rS) n
      defLoc rA res
      definePPCOpcodeRC P.SLDo res $ do
        comment "Shift Left Doubleword (x-form, RC=1)"
    definePPCOpcode P.SRD xform3c $ \rA rB rS -> do
      comment "Shift Right Doubleword (X-form, RC=0)"
      let n = zext (lowBits64 6 (Loc rB))
      let res = bvlshr (Loc rS) n
      defLoc rA res
      definePPCOpcodeRC P.SRDo res $ do
        comment "Shift Right Doubleword (X-form, RC=1)"
    defineOpcodeWithIP "SRADI" $ do
      comment "Shift Right Algebraic Doubleword Immediate (XS-form, RC=0)"
      rA <- param "rA" gprc naturalBV
      sh <- param "sh" u6imm (EBV 6)
      rS <- param "rS" gprc naturalBV
      input sh
      input rS
      input xer
      let w = Loc rS
      let res = sext (bvashr w (zext (Loc sh)))
      let nShiftedOutBits = bvsub (LitBV 64 64) (zext (Loc sh))
      let shiftedOutBits = bvlshr (bvshl w nShiftedOutBits) nShiftedOutBits
      let hasShiftedOutOnes = bvne shiftedOutBits (naturalLitBV 0x0)
      let s = highBits 1 res
      defLoc rA res
      defLoc xer (updateXER CA (Loc xer) (ite hasShiftedOutOnes s (LitBV 1 0x0)))
      defineRCVariant "SRADIo" res $ do
        comment "Shift Right Algebraic Doubleword Immediate (XS-form, RC=1)"
    definePPCOpcode P.SRAD xform3c $ \rA rB rS -> do
      comment "Shift Right Algebraic Doubleword (X-form, RC=0)"
      input xer
      let n = lowBits 6 (Loc rB)
      let w = Loc rS
      let r = sext (bvashr w (zext n))
      let s = highBits 1 w
      let nShiftedOutBits = bvsub (LitBV 64 64) (zext n)
      let shiftedOutBits = bvlshr (bvshl w nShiftedOutBits) nShiftedOutBits
      let hasShiftedOutOnes = bvne shiftedOutBits (naturalLitBV 0x0)
      defLoc rA r
      defLoc xer (updateXER CA (Loc xer) (ite hasShiftedOutOnes s (LitBV 1 0x0)))
      definePPCOpcodeRC P.SRADo r $ do
        comment "Shift Right Algebraic Doubleword (X-form, RC=1)"
    defineOpcodeWithIP "EXTSW" $ do
      comment "Extend Sign Word (X-form, RC=0)"
      (rA, rS) <- xform2
      let res = sext (lowBits 32 (Loc rS))
      defLoc rA res
      defineRCVariant "EXTSWo" res $ do
        comment "Extend Sign Word (X-form, RC=1)"

crbitOperation :: (?bitSize :: BitSize) => (Expr 'TBV -> Expr 'TBV -> Expr 'TBV) -> SemM 'Def ()
crbitOperation op = do
  (bt, ba, bb) <- xlformcr
  let extractBit n = highBits' 1 (bvshl (Loc cr) (zext' 32 n))
  let abit = extractBit (Loc ba)
  let bbit = extractBit (Loc bb)
  let clearedCR = bvand (Loc cr) (bvnot (bvshl (LitBV 32 0x1) (bvsub (LitBV 32 31) (zext' 32 (Loc bt)))))
  let res = bvor clearedCR (bvshl (zext' 32 (op abit bbit)) (zext' 32 (Loc bt)))
  defLoc cr res

-- | Bitwise operations over CR bits
crOps :: (?bitSize :: BitSize) => SemM 'Top ()
crOps = do
  defineOpcodeWithIP "CRAND" $ do
    comment "Condition Register AND (XL-form)"
    crbitOperation bvand

  defineOpcodeWithIP "CRXOR" $ do
    comment "Condition Register XOR (XL-form)"
    crbitOperation bvxor

  defineOpcodeWithIP "CROR" $ do
    comment "Condition Register OR (XL-form)"
    crbitOperation bvor

  defineOpcodeWithIP "CRNAND" $ do
    comment "Condition Register NAND (XL-form)"
    crbitOperation (\a b -> bvnot (bvand a b))

  defineOpcodeWithIP "CRNOR" $ do
    comment "Condition Register NOR (XL-form)"
    crbitOperation (\a b -> bvnot (bvor a b))

  defineOpcodeWithIP "CREQV" $ do
    comment "Condition Register Equivalent (XL-form)"
    crbitOperation (\a b -> bvnot (bvxor a b))

  defineOpcodeWithIP "CRANDC" $ do
    comment "Condition Register AND with Complement (XL-form)"
    crbitOperation (\a b -> bvand a (bvnot b))

  defineOpcodeWithIP "CRORC" $ do
    comment "Condition Register OR with Complement (XL-form)"
    crbitOperation (\a b -> bvor a (bvnot b))

special :: (?bitSize :: BitSize) => SemM 'Top ()
special = do
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

  when (?bitSize == Size64) $ do
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

    definePPCOpcode P.BPERMD xform3c $ \rA rB rS -> do
      comment "Bit Permute Doubleword (X-form)"
      let computePermBit i =
            let idx = extract (8 * i + 7) (8 * i) (Loc rS)
                rbVal = ite (testBitDynamic64 (zext idx) (Loc rB)) (LitBV 1 1) (LitBV 1 0)
            in ite (bvult idx (LitBV 8 64)) rbVal (LitBV 1 0)
      let perm_i = map computePermBit [0..7]
      defLoc rA (concat (LitBV 56 0x0) (foldr1 concat perm_i))

-- | Operations we are temporarily defining, but would like to eventually learn
temporary :: (?bitSize :: BitSize) => SemM 'Top ()
temporary = do
  defineOpcodeWithIP "ORI" $ do
    (rT, rA, ui) <- dformu
    defLoc rT (bvor (Loc rA) (zext (Loc ui)))

  defineOpcodeWithIP "ORIS" $ do
    (rT, rA, ui) <- dformu
    defLoc rT (bvor (Loc rA) (zext (concat (Loc ui) (LitBV 16 0x0))))

  defineOpcodeWithIP "XORI" $ do
    comment "XOR Immediate (D-form)"
    (rT, rA, ui) <- dformu
    defLoc rT (bvxor (Loc rA) (zext (Loc ui)))

  defineOpcodeWithIP "XORIS" $ do
    comment "XOR Immediate Shifted (D-form)"
    (rT, rA, ui) <- dformu
    defLoc rT (bvxor (Loc rA) (zext (concat (Loc ui) (LitBV 16 0x0))))

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
    let r = zext (rotl k (lowBits k (Loc rS)) n)
    let m = zext (mask k (zext' k (Loc mb)) (zext' k (Loc me)))
    let res = bvor (bvand r m) (bvand (Loc rA) (bvnot m))
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
