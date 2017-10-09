{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Compare (
  baseCompare
  ) where

import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

baseCompare :: (?bitSize :: BitSize) => SemM 'Top ()
baseCompare = do
  defineOpcode "CMPDI" $ do
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
  defineOpcode "CMPWI" $ do
    comment "Compare Immediate (D-form)"
    comment "This variant is the double word variant (where L=0)"
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
  defineOpcode "CMPD" $ do
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
  defineOpcode "CMPLW" $ do
    comment "Compare (X-form)"
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
    let newCR = cmpImm bvslt bvsgt (Loc fld) (zext lowa) (zext lowb)
    defLoc cr newCR
  defineOpcode "CMPLDI" $ do
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
  defineOpcode "CMPLWI" $ do
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
  defineOpcode "CMPLD" $ do
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
  defineOpcode "CMPLW" $ do
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
