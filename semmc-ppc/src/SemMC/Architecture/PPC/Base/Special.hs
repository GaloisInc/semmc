{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Special (
  baseSpecial
  ) where

import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

baseSpecial :: (?bitSize :: BitSize) => SemM 'Top ()
baseSpecial = do
  defineOpcodeWithIP "MTSPR" $ do
    comment "Move To Special Purpose Register (XFX-form)"
    -- Even though SPR is a 10-bit field, it gets decoded into a 32-bit field by
    -- dismantle. This is an artifact of the tablegen data; for whatever reason, this
    -- is how it handles SPR.
    rS      <- param "rS"  gprc     naturalBV
    sprbits <- param "SPR" "I32imm" (EBV 32)
    input rS
    input sprbits
    input ctr
    input xer
    input lnk

    let xerRes = ite (bveq (Loc sprbits) (LitBV 32 0x1)) (Loc rS) (Loc xer)
    let ctrRes = ite (bveq (Loc sprbits) (LitBV 32 0x8)) (Loc rS) (Loc ctr)
    let lnkRes = ite (bveq (Loc sprbits) (LitBV 32 0x9)) (Loc rS) (Loc lnk)
    defLoc ctr ctrRes
    defLoc xer xerRes
    defLoc lnk lnkRes

  defineOpcodeWithIP "MFSPR" $ do
    comment "Move From Special Purpose Register (XFX-form)"
    -- Even though SPR is a 10-bit field, it gets decoded into a 32-bit field by
    -- dismantle. This is an artifact of the tablegen data; for whatever reason, this
    -- is how it handles SPR.
    rT      <- param "rT"  gprc     naturalBV
    sprbits <- param "SPR" "I32imm" (EBV 32)

    input sprbits
    input ctr
    input xer
    input lnk

    defLoc rT (ite (bveq (Loc sprbits) (LitBV 32 0x1))
               (Loc xer)
               (ite (bveq (Loc sprbits) (LitBV 32 0x8))
                (Loc lnk)
                -- FIXME: add one more check for equality to 9 and exception
                (Loc ctr)))

  defineOpcodeWithIP "MFCR" $ do
    comment "Move From Condition Register"
    rT <- param "rT" gprc naturalBV
    input cr
    defLoc rT (zext (Loc cr))

  defineOpcodeWithIP "MTOCRF" $ do
    comment "Move To One Condition Register Field (XFX-form)"
    crbit <- param "FXM" crbitm (EBV 8)
    rS <- param "rS" gprc naturalBV
    input rS
    input crbit
    input cr
    -- Check the number of bits set in the field; if it is 1, then we set that
    -- field.  Otherwise, we are undefined.  FIXME: We don't have a great way to
    -- set something undefined for now...
    let check = bvpopcnt (zext' 32 (Loc crbit))
    let fldIdx = bvclz (zext' 32 (Loc crbit))
    let regContents = lowBits 32 (Loc rS)
    -- Generate a 4 bit mask to select the field.
    let fieldBitStart = bvmul fldIdx (LitBV 32 0x4)
    -- This is the mask we use to extract a new value from the source register
    let fieldMask = mask 32 fieldBitStart (bvadd fieldBitStart (LitBV 32 0x3))
    -- The mask we apply to CR to clear the space for the new value
    let crmask = bvnot fieldMask

    -- Save the high bits in a word with the target and low bits cleared (via shifting)
    -- Save the low bits in a word with the target and high bits cleared (via shifting)
    -- Shift the new field into place and OR everything together
    let newCR = bvor (bvand crmask (Loc cr)) (bvand fieldMask regContents)
    let res = ite (bveq check (LitBV 32 0x1)) newCR (LitBV 32 0x0)
    defLoc cr res

  defineOpcodeWithIP "MFOCRF" $ do
    comment "Move From One Condition Register Field (XFX-form)"
    rT <- param "rT" gprc naturalBV
    crbit <- param "FXM" crbitm (EBV 8)
    input crbit
    -- FIXME: The other bits of rT are actually undefined - we need a way to
    -- talk about undefined bits
    input rT
    input cr

    let check = bvpopcnt (zext' 32 (Loc crbit))
    let fldIdx = bvclz (zext' 32 (Loc crbit))
    let fieldBitStart = bvmul fldIdx (LitBV 32 0x4)
    let fieldMask = mask 32 fieldBitStart (bvadd fieldBitStart (LitBV 32 0x3))
    let crmask = bvnot fieldMask
    let newRT = bvand crmask (Loc cr)
    let res = ite (bveq check (LitBV 32 0x1)) newRT (LitBV 32 0x0)
    defLoc rT (zext res)
