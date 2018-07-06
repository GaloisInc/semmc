{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Core.Forms (
  vectorBV,
  xlformcr,
  mdform4,
  mdsform4,
  mform5i,
  mform5r,
  xoform3,
  xoform2,
  xform3,
  xform2,
  xform2f,
  dform,
  dformr0,
  dformu,
  iform,
  aform,
  aform4,
  vxform3,
  vxform3u,
  vxform2s,
  vxform2,
  vaform,
  vaform4u
  ) where

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core.BitSize
import SemMC.Architecture.PPC.Base.Core.OperandClasses
import SemMC.Architecture.PPC.Base.Core.Registers

vectorBV :: ExprTypeRepr 'TBV
vectorBV = EBV 128

-- | The XL-form for the CR logical operations
xlformcr :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
xlformcr = do
  bt <- param "BT" crbitrc (EBV 5)
  ba <- param "BA" crbitrc (EBV 5)
  bb <- param "BB" crbitrc (EBV 5)
  input ba
  input bb
  input cr
  return (bt, ba, bb)

-- | The M-form for RLWINM with three 5 bit immediates
mform5i :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mform5i = do
  rA <- param "rA" gprc naturalBV
  me <- param "me" u5imm (EBV 5)
  mb <- param "mb" u5imm (EBV 5)
  sh <- param "sh" u5imm (EBV 5)
  rS <- param "rS" gprc naturalBV
  input sh
  input mb
  input me
  input rS
  return (rA, sh, mb, me, rS)

mform5r :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mform5r = do
  rA <- param "rA" gprc naturalBV
  mb <- param "mb" u5imm (EBV 5)
  me <- param "me" u5imm (EBV 5)
  rS <- param "rS" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input rB
  input mb
  input me
  input rS
  return (rA, mb, me, rS, rB)


mdform4 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mdform4 = do
  rA <- param "rA" gprc naturalBV
  mb <- param "mb" u6imm (EBV 6)
  sh <- param "sh" u6imm (EBV 6)
  rS <- param "rS" gprc naturalBV
  input rS
  input sh
  input mb
  return (rA, sh, mb, rS)

mdsform4 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
mdsform4 = do
  rA <- param "rA" gprc naturalBV
  mb <- param "mb" u6imm (EBV 6)
  rS <- param "rS" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input mb
  input rS
  input rB
  return (rA, mb, rS, rB)

xoform3 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
xoform3 = do
  rT <- param "rT" gprc naturalBV
  rA <- param "rA" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  input rA
  input rB
  return (rT, rA, rB)

vxform2s :: SemM 'Def (Location 'TBV, Location 'TBV)
vxform2s = do
  vrT <- param "vrT" vrrc vectorBV
  sim <- param "sim" s5imm (EBV 5)
  return (vrT, sim)

vxform3u :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
vxform3u = do
  vrT <- param "vrT" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  uim <- param "uim" u5imm (EBV 5)
  input vrB
  return (vrT, vrB, uim)

vxform3 :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
vxform3 = do
  vrT <- param "vrT" vrrc vectorBV
  vrA <- param "vrA" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  input vrA
  input vrB
  return (vrT, vrA, vrB)

vxform2 :: SemM 'Def (Location 'TBV, Location 'TBV)
vxform2 = do
  vrT <- param "vrT" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  input vrB
  return (vrT, vrB)

vaform :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
vaform = do
  vrT <- param "vrT" vrrc vectorBV
  vrA <- param "vrA" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  vrC <- param "vrC" vrrc vectorBV
  input vrA
  input vrB
  input vrC
  return (vrT, vrA, vrB, vrC)

vaform4u :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
vaform4u = do
  vrT <- param "vrT" vrrc vectorBV
  shb <- param "shb" u5imm (EBV 5)
  vrA <- param "vrA" vrrc vectorBV
  vrB <- param "vrB" vrrc vectorBV
  input vrA
  input vrB
  return (vrT, shb, vrA, vrB)

xoform2 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV)
xoform2 = do
  rT <- param "rT" gprc naturalBV
  rA <- param "rA" gprc naturalBV
  input rA
  return (rT, rA)

xform3 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
xform3 = do
  rA <- param "rA" gprc naturalBV
  rB <- param "rB" gprc naturalBV
  rS <- param "rS" gprc naturalBV
  input rS
  input rB
  return (rA, rS, rB)

xform2 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV)
xform2 = do
  rA <- param "rA" gprc naturalBV
  rS <- param "rS" gprc naturalBV
  input rS
  return (rA, rS)

xform2f :: SemM 'Def (Location 'TBV, Location 'TBV)
xform2f = do
  frT <- param "frT" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  input frB
  return (frT, frB)

dform :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
dform = do
  rT <- param "rT" gprc naturalBV
  si <- param "si" s16imm (EBV 16)
  rA <- param "rA" gprc_nor0 naturalBV
  input rA
  input si
  return (rT, rA, si)

-- | D-form instructions where the operand is allowed to contain r0
dformr0 :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
dformr0 = do
  rT <- param "rT" gprc naturalBV
  si <- param "si" s16imm (EBV 16)
  rA <- param "rA" gprc naturalBV
  input rA
  input si
  return (rT, rA, si)

-- | The unsigned immediate version of the dform
dformu :: (?bitSize :: BitSize) => SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
dformu = do
  rT <- param "rT" gprc naturalBV
  ui <- param "ui" u16imm (EBV 16)
  rA <- param "rA" gprc naturalBV
  input rA
  input ui
  return (rT, rA, ui)

iform :: (?bitSize :: BitSize) => String -> SemM 'Def (Location 'TBV)
iform tag = do
  target <- param "target" tag (EBV 24)
  input target
  return target

-- Our floating point registers are considered as 128 bit registers, as they are
-- actually backed by the 128 bit vector registers.
--
-- It is the job of each semantics definition to extract the necessary parts of
-- the registers.
aform :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV)
aform = do
  frT <- param "frT" fprc (EBV 128)
  frA <- param "frA" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  input frA
  input frB
  return (frT, frA, frB)

aform4 :: SemM 'Def (Location 'TBV, Location 'TBV, Location 'TBV, Location 'TBV)
aform4 = do
  frT <- param "frT" fprc (EBV 128)
  frA <- param "frA" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  frC <- param "frC" fprc (EBV 128)
  input frA
  input frB
  input frC
  return (frT, frA, frB, frC)
