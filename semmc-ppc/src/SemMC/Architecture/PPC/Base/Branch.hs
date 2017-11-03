{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Branch (
  manualBranch
  ) where

import Prelude hiding ( concat )
import Data.Bits
import Data.Word.Indexed ( W )
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

  defineOpcode "BLR" $ do
    comment "BLR : BCLR (XL-form, LK=0)"
    comment "This is a specialized unconditional BCLR"
    branchConditionalLNK NoLink 0b00101 (LitBV 5 0x0)

  defineOpcode "BLRL" $ do
    comment "BLRL : BCLR (XL-form, LK=1)"
    comment "This is a specialized unconditional BCLR"
    branchConditionalLNK Link 0b00101 (LitBV 5 0x0)

  defineOpcode "BCLR" $ do
    comment "BCLR (XL-form, LK=0)"
    crbit <- param "bi" crbitrc (EBV 5)
    branchConditionalLNK NoLink 0b01100 (Loc crbit)

  defineOpcode "BCLRL" $ do
    comment "BCLR (XL-form, LK=1)"
    crbit <- param "bi" crbitrc (EBV 5)
    branchConditionalLNK Link 0b01100 (Loc crbit)

  defineOpcode "BCTR" $ do
    comment "BCTR : BCCTR (XL-form, LK=1)"
    comment "This is a specialized unconditional BCCTR"
    -- Note that this is unconditional (per the BO field), so the BI field is
    -- unused and passed as zero
    branchConditionalCTR NoLink 0b00101 (LitBV 5 0x0)

  defineOpcode "BCTRL" $ do
    comment "BCTR : BCCTR (XL-form, LK=0)"
    comment "This is a specialized unconditional BCCTR"
    branchConditionalCTR Link 0b00101 (LitBV 5 0x0)

  defineOpcode "BCCTR" $ do
    comment "BCCTR (XL-form, LK=0)"
    comment "This variant has BO=01100, branch when the CR bit is 1"
    crbit <- param "bi" crbitrc (EBV 5)
    branchConditionalCTR NoLink 0b01100 (Loc crbit)

  defineOpcode "BCCTRL" $ do
    comment "BCCTR (XL-form, LK=1)"
    comment "This variant has BO=01100, branch when the CR bit is 1"
    crbit <- param "bi" crbitrc (EBV 5)
    branchConditionalCTR Link 0b01100 (Loc crbit)

  defineOpcode "BC" $ do
    comment "BC (B-form, AA=0, LK=0)"
    comment "This form is actually equivalent to BT, which has the BO field=01100"
    target <- param "target" condbrtarget (EBV 14)
    crbit <- param "bi" crbitrc (EBV 5)
    branchConditional Relative NoLink 0b00110 (Loc crbit) (Loc target)

  defineOpcode "BCL" $ do
    comment "BC (B-form, AA=0, LK=1)"
    comment "This form is actually equivalent to BT, which has the BO field=01100"
    target <- param "target" condbrtarget (EBV 14)
    crbit <- param "bi" crbitrc (EBV 5)
    branchConditional Relative Link 0b00110 (Loc crbit) (Loc target)

  defineOpcode "BDZ" $ do
    target <- param "target" condbrtarget (EBV 14)
    -- The BO_0 bit is set, so the CR is ignored -- we pass in bit 0 for the CR
    -- to just have something
    branchConditional Relative NoLink 0b01001 (LitBV 5 0x0) (Loc target)

  defineOpcode "BDZL" $ do
    target <- param "target" condbrtarget (EBV 14)
    branchConditional Relative Link 0b01001 (LitBV 5 0x0) (Loc target)

  defineOpcode "BDNZ" $ do
    target <- param "target" condbrtarget (EBV 14)
    branchConditional Relative NoLink 0b00001 (LitBV 5 0x0) (Loc target)

  defineOpcode "BDNZL" $ do
    target <- param "target" condbrtarget (EBV 14)
    branchConditional Relative Link 0b00001 (LitBV 5 0x0) (Loc target)

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

-- | Conditional branch to the CTR register
branchConditionalCTR :: (?bitSize :: BitSize)
                     => LK
                     -> W 5 -- ^ The 5 bit BO field (fixed by our instructions)
                     -> Expr 'TBV -- ^ The Crbitrc (unused for unconditional branches)
                     -> SemM 'Def ()
branchConditionalCTR lk bo bi = do
  input ctr
  input ip
  when (not (testBit bo 0)) $ input cr

  let target = concat (highBits (bitSizeValue ?bitSize - 2) (Loc ctr)) (LitBV 2 0x0)
  let nextInsn = (bvadd (Loc ip) (naturalLitBV 0x4))
  defLoc ip (ite (cond_ok bo bi) target nextInsn)

  -- NOTE: Setting the LNK register is independent of whether or not the branch
  -- was taken
  when (lk == Link) $ do
    defLoc lnk nextInsn

-- | Conditional branch through the LNK register
branchConditionalLNK :: (?bitSize :: BitSize)
                     => LK
                     -> W 5 -- ^ The 5 bit BO field
                     -> Expr 'TBV
                     -> SemM 'Def ()
branchConditionalLNK lk bo bi = do
  input lnk
  input ip
  when (not (testBit bo 0)) $ input cr

  let nextInsn = bvadd (Loc ip) (naturalLitBV 0x4)
  let target = concat (highBits (bitSizeValue ?bitSize - 2) (Loc lnk)) (LitBV 2 0x0)

  defLoc ip (ite (andp (cond_ok bo bi) (ctr_ok bo)) target nextInsn)

  when (lk == Link) $ do
    defLoc lnk nextInsn

-- | Logic for implementing the conditional branch instruction.
--
-- See Note [Branch Conditional] for details
branchConditional :: (?bitSize :: BitSize)
                  => AA
                  -> LK
                  -> W 5
                  -- ^ The 5 bit BO field, which is not actually an argument to
                  -- our instructions and is thus not an 'Expr'
                  -> Expr 'TBV
                  -> Expr 'TBV
                  -> SemM 'Def ()
branchConditional aa lk bo bi target = do
  input ctr
  when (not (testBit bo 0)) $ input cr
  when (lk == Link || aa == Relative) $ input ip

  -- If bit 2 of BO is set, we decrement the CTR.  We do this in a let binding
  -- so that we can re-use the correct value later on in some expressions.
  let isCtrDec = not (testBit bo 2)
  let newCtr = if isCtrDec then bvsub (Loc ctr) (naturalLitBV 0x1) else Loc ctr
  when isCtrDec $ do
    defLoc ctr newCtr

  let xtarget = sext (concat target (LitBV 2 0x0))
  let nextInsn = bvadd (Loc ip) (naturalLitBV 0x4)
  let brEA = if aa == Absolute
             then xtarget
             else bvadd xtarget (Loc ip)
  defLoc ip (ite (andp (cond_ok bo bi) (ctr_ok bo)) brEA nextInsn)

  -- The IP is still an input even with an absolute jump if we are setting the
  -- link register, which depends on the current IP.
  when (lk == Link) $ defLoc lnk nextInsn

truePred :: Expr 'TBool
truePred = LitBool True

falsePred :: Expr 'TBool
falsePred = LitBool False

cond_ok :: W 5 -> Expr 'TBV -> Expr 'TBool
cond_ok bo bi =
  if testBit bo 0
  -- If BO_0 is set, the cond is always true and this is unconditional (modulo
  -- the CTR check)
  then truePred
  -- Otherwise, we have to check the CR field (the BI'th bit of the CR).  The CR
  -- is always 32 bits, and BI is wide enough to address any of them.
  else if testBit bo 1
       then bveq (extractDynamic 1 bi bi (Loc cr)) (LitBV 1 0x1)
       else bveq (extractDynamic 1 bi bi (Loc cr)) (LitBV 1 0x0)

ctr_ok :: (?bitSize :: BitSize) => W 5 -> Expr 'TBool
ctr_ok bo =
   if testBit bo 2
   -- If bit 2 is set, we don't check the CTR at all, so ctr_ok is trivially true
   then truePred
   -- Otherwise, we compute (CTR /= 0 ^ BO_3).  Note that BO_3 is
   -- statically known, so we don't generate an XOR -- we compute it
   -- in SemM.  If @CTR /= 0@ is treated as an opaque predicate P
   -- and @P ^ BO_3@ is opaque predicate Q:
   else if testBit bo 3
        then xorp ctr_ne_zero truePred
        else xorp ctr_ne_zero falsePred
  where
    ctr_ne_zero = bveq (Loc ctr) (naturalLitBV 0x0)

{- Note [Branch Conditional]

There are some minor discrepancies between our representation (based on the
tablegen data) and what is in the PowerPC manual.  It isn't incorrect, but the
mapping between the two is less than obvious.

First, the ~BC~ instruction in the manual has an additional BO field that our
~BC~ instruction lacks.  This is because our representation has a separate
instruction for each value of the BO field.  For example, BDNZ corresponds to
~BC~ with BO=1a00t, where a and t are used as hints to the branch predictor.
Our ~BC~ instruction corresponds to the case where BO=011at (branch if CR_BI=1).
That seems to correspond to the instruction that is sometimes called ~BT~
(Branch if True).  Note that we don't have all of the variants available,
presumably because clang never generates them.  We may have to fill those gaps
in the disassembler by hand.

| Our Insn | Mnemonic | BO    |
|----------+----------+-------|
| BC       | BT       | 011at |
| BDNZ     | BDNZ     | 1a00t |
| BDZ      | BDZ      | 1a01t |


There are corresponding variants to branch through the CTR or LR instead of to a
known offset.

-}
