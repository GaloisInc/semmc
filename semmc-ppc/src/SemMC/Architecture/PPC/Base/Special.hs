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

  defineOpcodeWithIP "MCRF" $ do
    comment "Move Condition Register Field (XL-form)"
    bf <- param "BF" crrc (EBV 3)
    bfa <- param "BFA" crrc (EBV 3)
    input bf
    input bfa
    input cr
    let selField = crField (Loc bfa)
    defLoc cr (updateCRField (Loc bf) selField)

  defineOpcodeWithIP "MFCR" $ do
    comment "Move From Condition Register"
    rT <- param "rT" gprc naturalBV
    input cr
    defLoc rT (zext (Loc cr))

  defineOpcodeWithIP "MTCRF" $ do
    comment "Move To Condition Register Fields (XFX-form)"
    rS <- param "rS" gprc naturalBV
    fxm <- param "FXM" "I32imm" (EBV 8)
    input rS
    input cr
    let mkFldMask n = sext' 4 (extract n n (Loc fxm))
    let crMask = concat (mkFldMask 0)
                        (concat (mkFldMask 1)
                                (concat (mkFldMask 2)
                                        (concat (mkFldMask 3)
                                                (concat (mkFldMask 4)
                                                        (concat (mkFldMask 5)
                                                                (concat (mkFldMask 6) (mkFldMask 7)))))))
    defLoc cr (bvor (bvand (lowBits 32 (Loc rS)) crMask) (bvand (Loc cr) (bvnot crMask)))

  defineOpcodeWithIP "MTOCRF" $ do
    comment "Move To One Condition Register Field (XFX-form)"
    crbit <- param "FXM" crbitm (EBV 8)
    rS <- param "rS" gprc naturalBV
    input rS
    input crbit
    input cr
    -- -- Check the number of bits set in the field; if it is 1, then we set that
    -- -- field.  Otherwise, we are undefined.
    -- let check = bvpopcnt (zext' 32 (Loc crbit))
    -- let fieldIndex = bvclz (zext' 32 (Loc crbit))
    let fieldIndex = (zext' 32 (Loc crbit))
    let fieldBitStart = bvmul fieldIndex (LitBV 32 0x4)
    let fieldBitEnd = bvadd fieldBitStart (LitBV 32 0x3)
    -- This is the mask we use to extract a new value from the source register
    let fieldMask = mask 32 fieldBitStart fieldBitEnd
    -- The mask we apply to CR to clear the space for the new value
    let crMask = bvnot fieldMask

    let newCR = bvor
          (bvand crMask (Loc cr))
          (bvand fieldMask (lowBits 32 (Loc rS)))
    -- let res = ite (bveq check (LitBV 32 0x1)) newCR (undefinedBV 32)
    let res = newCR
    defLoc cr res

{- TODO: Generating errors when running scripts/genbase.sh, needs to be resolved
  defineOpcodeWithIP "MFOCRF" $ do
    comment "Move From One Condition Register Field (XFX-form)"
    rT <- param "rT" gprc naturalBV
    crbit <- param "FXM" crbitm (EBV 8)
    input crbit
    input rT
    input cr

    -- let check = bvpopcnt (zext' 32 (Loc crbit))
    -- let fieldIndex = bvclz (zext' 32 (Loc crbit))
    let fieldIndex = zext' 32 (Loc crbit)
    let fieldBitStart = bvmul fieldIndex (LitBV 32 0x4)
    let fieldBitEnd = bvadd fieldBitStart (LitBV 32 0x3)
    let fieldMask = mask 32 fieldBitStart fieldBitEnd
    let rTMask = bvnot $ mask 64 fieldBitStart fieldBitEnd
    let newRT = bvor
          (zext $ bvand fieldMask (Loc cr))
          (bvand rTMask (undefinedBV 64))
    -- let res = ite (bveq check (LitBV 32 0x1)) newRT (undefinedBV 64)
    let res = newRT
    defLoc rT res
-}
