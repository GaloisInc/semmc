-- | Pseudocode definitions of expansion of immediate constants from
-- F4.2.4 (page F4-2473) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ExpandImm
    ( armExpandImm
    , armExpandImmC
    , armExpandImmC'
    , thumbExpandImm
    )
    where

import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
import SemMC.DSL

import Prelude hiding (concat)

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473)  This version expects to be called with
-- the Mod_imm ARM operand.
armExpandImm :: Location 'TPackedOperand
             -> SemARM 'Def (Expr 'TBV)
armExpandImm imm12 = do
    let val = modImm_imm imm12
        rot = modImm_rot imm12
    -- n.b. carry_in does not affect the imm32 result, safe to use literal 0
    (ans, _) <- expandimm_c val rot (LitBV 1 0)
    return $ "armExpandImm" =: ans

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473) with carry.  This version expects to be called with
-- the Mod_imm ARM operand.
armExpandImmC :: Location 'TPackedOperand -> Expr 'TBV
              -> SemARM 'Def (Expr 'TBV, Expr 'TBV)
armExpandImmC imm12 carry_in =
    let val = modImm_imm imm12
        rot = modImm_rot imm12
    in expandimm_c val rot carry_in

-- | Expand/rotate ModImm value to corresponding 32-bit immediate
-- value (F4-2473) with carry.  This version expects to be called with
-- a simple 12-bit value.
armExpandImmC' :: Expr 'TBV -> Expr 'TBV
               -> SemARM 'Def (Expr 'TBV, Expr 'TBV)
armExpandImmC' imm12 carry_in = -- KWQ: common code with armExpandImm and use ShiftC
    let val = extract 7 0 imm12
        rot = extract 11 8 imm12
    in expandimm_c val rot carry_in

expandimm_c :: Expr 'TBV -> Expr 'TBV -> Expr 'TBV
            -> SemARM 'Def (Expr 'TBV, Expr 'TBV)
expandimm_c val rot carry_in = do
    -- expects val to be 8 bits, rot to be 4 bits, carry_in to be 1 bit
    let rotv = bvshl (naturalLitBV 1) $ zext rot -- multiply by 2
    fullres <- shiftC (zext val) srtROR rotv carry_in
    let cout = extract 32 32 fullres
        imm32 = extract 31 0 fullres
    return (imm32, cout)

-- | Expand/rotate T2_So_Imm value to corresponding 32-bit immediate value
-- (AppxG-4948)  This version expects to be called with the T2_So_Imm ARM operand.
thumbExpandImm :: Location 'TPackedOperand -> Expr 'TBV
thumbExpandImm t2_so_imm =
  let imm12 = t2SoImm_imm t2_so_imm
  in "thumbExpandImm" =: (fst $ thumbexpandimm_c imm12 (LitBV 1 0))

thumbexpandimm_c :: Expr 'TBV -> Expr 'TBV -> (Expr 'TBV, Expr 'TBV)
thumbexpandimm_c imm12 carry_in =
  let sel1 = extract 11 10 imm12
      sel2 = extract  9  8 imm12
      top5 = extract 11  7 imm12
      val  = extract  7  0 imm12
      val' = extract  6  0 imm12

      unrotated_value = zext (LitBV 1 0b1 `concat` val')

      rorC'      = rorC unrotated_value (zext top5)
      carry_out' = extract 32 32 rorC'
      imm32'     = extract 31 0 rorC'

      carry_out = ite (bveq sel1 (LitBV 2 0b00)) carry_in carry_out'
      imm32     = ite (bveq sel1 (LitBV 2 0b00))
        ( cases
          [ ( bveq sel2 (LitBV 2 0b00), zext val )
          , ( bveq sel2 (LitBV 2 0b01),
              ite (bveq val (LitBV 8 0))
              ( unpredictable imm32' ) -- BGS: FIXME, is this right?
              ( LitBV 8 0 `concat` val `concat` LitBV 8 0 `concat` val) )
          , ( bveq sel2 (LitBV 2 0b10),
              ite (bveq val (LitBV 8 0))
              ( unpredictable imm32' )
              ( val `concat` LitBV 8 0 `concat` val `concat` LitBV 8 0) ) ]
          ( ite (bveq val (LitBV 8 0))
            ( unpredictable imm32')
            ( val `concat` val `concat` val `concat` val ) ) )
        imm32'
  in (imm32, carry_out)
