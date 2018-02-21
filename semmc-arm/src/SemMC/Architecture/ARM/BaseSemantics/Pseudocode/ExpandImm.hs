-- | Pseudocode definitions of expansion of immediate constants from
-- F4.2.4 (page F4-2473) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ExpandImm
    ( armExpandImm
    , armExpandImmC
    )
    where

import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
import SemMC.DSL


-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473)
armExpandImm :: Location 'TBV -> Expr 'TBV
armExpandImm imm12 =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
        -- Determine value per Table F4-6 (F4.2.4, F4-2472)
        val32 = zext val
        rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
        rval32 = ite (bveq rotv (naturalLitBV 0)) val32 (ror rotv val32)
    in "armExpandImm" =: rval32

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473) with carry
armExpandImmC :: Location 'TBV -> Expr 'TBV -> (Expr 'TBV, Expr 'TBV)
armExpandImmC imm12 carry_in =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
        -- Determine value per Table F4-6 (F4.2.4, F4-2472)
        val32 = zext val
        rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
        rval32 = ite (bveq rotv (naturalLitBV 0)) val32 (ror rotv val32)
        msb = extract 31 31 rval32 -- return the MSB as the carry_out
        carry_out =
          ite (bveq rotv (naturalLitBV 0)) carry_in msb
    in (rval32, carry_out)
