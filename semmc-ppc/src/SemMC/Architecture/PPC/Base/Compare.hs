{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Compare (
  baseCompare
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

baseCompare :: (?bitSize :: BitSize) => SemM 'Top ()
baseCompare = do
  defineOpcodeWithIP "CMPW" $ do
    comment "Compare Word (X-form)"
    comment "This is a word variant (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    rA <- param "rA" gprc naturalBV
    rB <- param "rB" gprc naturalBV
    input rA
    input rB
    input xer
    input cr
    let lowA = lowBits 32 (Loc rA)
    let lowB = lowBits 32 (Loc rB)
    let newCR = cmpImm bvslt bvsgt (Loc fld) lowA lowB
    defLoc cr newCR
  defineOpcodeWithIP "CMPWI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the word variant (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    imm <- param "imm" s16imm (EBV 16)
    rA <- param "rA" gprc naturalBV
    input imm
    input rA
    input cr
    input xer
    let ximm = sext (Loc imm)
    let lowreg = lowBits 32 (Loc rA)
    let newCR = cmpImm bvslt bvsgt (Loc fld) ximm (sext lowreg)
    defLoc cr newCR
  defineOpcodeWithIP "CMPLW" $ do
    comment "Compare Logical (X-form)"
    comment "Compare word (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    rA <- param "rA" gprc naturalBV
    rB <- param "rB" gprc naturalBV
    input rA
    input rB
    input cr
    input xer
    let lowa = lowBits 32 (Loc rA)
    let lowb = lowBits 32 (Loc rB)
    let newCR = cmpImm bvult bvugt (Loc fld) (zext lowa) (zext lowb)
    defLoc cr newCR
  defineOpcodeWithIP "CMPLWI" $ do
    comment "Compare Logical Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
    fld <- param "fld" crrc (EBV 3)
    imm <- param "imm" u16imm (EBV 16)
    rA <- param "rA" gprc naturalBV
    input imm
    input rA
    input cr
    input xer
    let ximm = zext (Loc imm)
    let lowreg = lowBits 32 (Loc rA)
    let newCR = cmpImm bvult bvugt (Loc fld) ximm (zext lowreg)
    defLoc cr newCR

  defineOpcodeWithIP "CMPB" $ do
    comment "Compare Bytes (X-form)"
    (rA, rS, rB) <- xform3
    let extractByte n bv = extract (8 * n + 7) (8 * n) bv
    let compareByte n = ite (bveq (extractByte n (Loc rS)) (extractByte n (Loc rB))) (LitBV 8 0xff) (LitBV 8 0x0)
    let high = concat (compareByte 0) (concat (compareByte 1) (concat (compareByte 2) (compareByte 3)))
    let low = concat (compareByte 4) (concat (compareByte 5) (concat (compareByte 6) (compareByte 7)))
    let res = if ?bitSize == Size32 then high else concat high low
    defLoc rA res

  when (?bitSize == Size64) $ do
    defineOpcodeWithIP "CMPLD" $ do
      comment "Compare Logical (X-form)"
      comment "Compare double word (where L=1)"
      fld <- param "fld" crrc (EBV 3)
      rA <- param "rA" gprc naturalBV
      rB <- param "rB" gprc naturalBV
      input rA
      input rB
      input cr
      input xer
      let newCR = cmpImm bvult bvugt (Loc fld) (Loc rA) (Loc rB)
      defLoc cr newCR
    defineOpcodeWithIP "CMPLDI" $ do
      comment "Compare Logical Immediate (D-form)"
      comment "This variant is the double word variant (where L=1)"
      fld <- param "fld" crrc (EBV 3)
      imm <- param "imm" u16imm64 (EBV 16)
      rA <- param "rA" gprc naturalBV
      input imm
      input rA
      input cr
      input xer
      let ximm = zext (Loc imm)
      let newCR = cmpImm bvult bvugt (Loc fld) ximm (Loc rA)
      defLoc cr newCR
    defineOpcodeWithIP "CMPDI" $ do
      comment "Compare Immediate (D-form)"
      comment "This variant is the double word variant (where L=1)"
      fld <- param "fld" crrc (EBV 3)
      imm <- param "imm" s16imm64 (EBV 16)
      rA <- param "rA" gprc naturalBV
      input imm
      input rA
      input cr
      input xer
      let ximm = sext (Loc imm)
      let newCR = cmpImm bvslt bvsgt (Loc fld) ximm (Loc rA)
      defLoc cr newCR
    defineOpcodeWithIP "CMPD" $ do
      comment "Compare (X-form)"
      comment "Compare double word where L=1"
      fld <- param "fld" crrc (EBV 3)
      rA <- param "rA" gprc naturalBV
      rB <- param "rB" gprc naturalBV
      input rA
      input rB
      input cr
      input xer
      let newCR = cmpImm bvslt bvsgt (Loc fld) (Loc rA) (Loc rB)
      defLoc cr newCR
