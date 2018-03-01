-- | Pseudocode definitions of expansion of immediate constants from
-- F4.2.4 (page F4-2473) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ExpandImm
    ( armExpandImm
    , armExpandImmC
    , armExpandImmC'
    )
    where

import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
import SemMC.DSL


-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473)  This version expects to be called with
-- the Mod_imm ARM operand.
armExpandImm :: Location 'TBV -> Expr 'TBV
armExpandImm imm12 =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
    -- n.b. carry_in does not affect the imm32 result, safe to use literal 0
    in "armExpandImm" =: (fst $ expandimm_c val rot (LitBV 1 0))

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473) with carry.  This version expects to be called with
-- the Mod_imm ARM operand.
armExpandImmC :: Location 'TBV -> Expr 'TBV -> (Expr 'TBV, Expr 'TBV)
armExpandImmC imm12 carry_in =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
    in expandimm_c val rot carry_in

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473) with carry.  This version expects to be called with
-- a simple 12-bit value.
armExpandImmC' :: Expr 'TBV -> Expr 'TBV -> (Expr 'TBV, Expr 'TBV)
armExpandImmC' imm12 carry_in = -- KWQ: common code with armExpandImm and use ShiftC
    let val = extract 7 0 imm12
        rot = extract 11 8 imm12
    in expandimm_c val rot carry_in

expandimm_c :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV -> (Expr 'TBV, Expr 'TBV)
expandimm_c val rot carry_in =
    -- expects val to be 8 bits, rot to be 4 bits, carry_in to be 1 bit
    let rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
        fullres = shiftC (zext val) srtROR rotv carry_in
        cout = extract 32 32 fullres
        imm32 = extract 31 0 fullres
    in (imm32, cout)
