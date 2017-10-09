{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Branch (
  manualBranch
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

manualBranch :: (?bitSize :: BitSize) => SemM 'Top ()
manualBranch = do
  defineOpcode "B" $ do
    comment "Branch (I-form, AA=0, LK=0)"
    target <- iform directbrtarget
    branchUnconditional Relative NoLink (Loc target)
  defineOpcode "BL" $ do
    comment "Branch (I-form, AA=0, LK=1)"
    target <- iform calltarget
    branchUnconditional Relative Link (Loc target)
  defineOpcode "BA" $ do
    comment "Branch (I-form, AA=1, LK=0)"
    target <- iform absdirectbrtarget
    branchUnconditional Absolute NoLink (Loc target)
  defineOpcode "BLA" $ do
    comment "Branch (I-form, AA=1, LK=1)"
    target <- iform abscalltarget
    branchUnconditional Absolute Link (Loc target)
  -- Note, this isn't a real instruction.  It is a special form of BCLR unconditional.
  defineOpcode "BLR" $ do
    input lnk
    defLoc ip (Loc lnk)

data AA = Absolute | Relative
  deriving (Eq, Show)

data LK = Link | NoLink
  deriving (Eq, Show)

branchUnconditional :: (?bitSize :: BitSize)
                    => AA
                    -> LK
                    -> Expr 'TBV
                    -> SemM 'Def ()
branchUnconditional aa lk target = do
  when (aa == Relative) $ do
    input ip
  let target' = if aa == Absolute
                then sext (concat target (LitBV 2 0x0))
                else bvadd (Loc ip) (sext (concat target (LitBV 2 0x0)))
  defLoc ip target'
  -- The IP is still an input even with an absolute jump if we are setting the
  -- link register, which depends on the current IP.
  when (lk == Link) $ do
    input ip
    defLoc lnk (bvadd (Loc ip) (naturalLitBV 0x4))

branchConditional :: (?bitSize :: BitSize)
                  => AA
                  -> LK
                  -> Expr 'TBV
                  -> Expr 'TBV
                  -> Expr 'TBV
                  -> SemM 'Def ()
branchConditional aa lk bo bi target = do
  let xtarget = sext (concat target (LitBV 2 0x0))
  -- The IP is still an input even with an absolute jump if we are setting the
  -- link register, which depends on the current IP.
  when (lk == Link) $ do
    input ip
    defLoc lnk (bvadd (Loc ip) (naturalLitBV 0x4))
