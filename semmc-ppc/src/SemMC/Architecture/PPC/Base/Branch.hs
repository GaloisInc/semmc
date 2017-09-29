{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Branch (
  manualBranch
  ) where

import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

manualBranch :: (?bitSize :: BitSize) => SemM 'Top ()
manualBranch = do
  defineOpcode "B" $ do
    comment "Branch (I-form, AA=0, LK=0)"
    target <- iform directbrtarget
    input ip
    let off = sext (concat (Loc target) (LitBV 2 0x0))
    defLoc ip (bvadd (Loc ip) off)
  defineOpcode "BL" $ do
    comment "Branch (I-form, AA=0, LK=1)"
    target <- iform directbrtarget
    input ip
    let off = sext (concat (Loc target) (LitBV 2 0x0))
    defLoc ip (bvadd (Loc ip) off)
    defLoc lnk (bvadd (Loc ip) (naturalLitBV 0x4))
  defineOpcode "BA" $ do
    comment "Branch (I-form, AA=1, LK=0)"
    target <- iform absdirectbrtarget
    defLoc ip (sext (concat (Loc target) (LitBV 2 0x0)))
  defineOpcode "BLA" $ do
    comment "Branch (I-form, AA=1, LK=1)"
    target <- iform absdirectbrtarget
    defLoc ip (sext (concat (Loc target) (LitBV 2 0x0)))
    defLoc lnk (bvadd (Loc ip) (naturalLitBV 0x4))
  -- Note, this isn't a real instruction.  It is a special form of BCLR unconditional.
  defineOpcode "BLR" $ do
    input lnk
    defLoc ip (Loc lnk)
