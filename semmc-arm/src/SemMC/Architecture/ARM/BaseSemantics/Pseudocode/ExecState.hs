-- | Pseudocode definitions of execution state management from E1.2.5
-- (page E1-2298) of the ARMv8 Architecture Reference Manual.

{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ExecState
    ( instrSetState
    , selectInstrSet
    , selectInstrSet'
    )
    where

import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.DSL
import SemMC.Util ( fromJust' )


-- | The processor mode can be determined by examining the ISETSTATE
-- execution state register, which is embedded in the CPSR (as 'J' at
-- bit 24 and 'T' at bit 5).  The following can be used to read the
-- current processor mode.
--
-- This is not normally used however because the current instruction
-- set (arch subtype) is set in the DSL state (see 'SemM_ARMData').
-- [If this function becomes useful, add the ArchSubtype to the
-- ExprTag in the DSL definition to return the value directly instead
-- of returning the string form.]
instrSetState :: Location 'TBV -> Expr 'TString
instrSetState cpsReg =
    let cpsr_j = testBitDynamic32 (LitBV 32 24) (Loc cpsReg)
        cpsr_t = testBitDynamic32 (LitBV 32 5) (Loc cpsReg)
        isT32 = andp cpsr_t (notp cpsr_j)
        isA32 = andp (notp cpsr_t) (notp cpsr_j)
        -- isJazelle = andp (notp cpsr_t) cpsr_j
        isT32EE = andp cpsr_t cpsr_j
        toRet = LitString . show
    in ite isA32 (toRet InstrSet_A32)
           (ite isT32 (toRet InstrSet_T32)
            (ite isT32EE (toRet InstrSet_T32EE) (toRet InstrSet_Jazelle)))


-- | Update the CPSR to set to either the A32 or T32 target instruction set as
-- indicated by the expression (E1.2.3, E1-2300)
selectInstrSet :: Expr 'TBool -> Expr 'TBool -> SemARM 'Def ()
selectInstrSet isEnabled toT32 = do
    setT32 <- cpsrT32
    setA32 <- cpsrA32
    updateCPSR (\cpsrReg -> ite isEnabled
                           (ite toT32 (setT32 cpsrReg) (setA32 cpsrReg))
                           cpsrReg)


-- | Update the CPSR to set to the known concrete target instruction set.
-- (E1.2.3, E1-2300)
selectInstrSet' :: ArchSubtype -> SemARM 'Def ()
selectInstrSet' tgtarch =
    case tgtarch of
      InstrSet_A32 -> updateCPSR =<< cpsrA32
      InstrSet_T32 -> updateCPSR =<< cpsrT32
      InstrSet_T32EE -> updateCPSR =<< cpsrT32EE
      InstrSet_Jazelle -> updateCPSR =<< cpsrJazelle


cpsrA32, cpsrT32, cpsrT32EE, cpsrJazelle :: SemARM 'Def (Expr 'TBV -> Expr 'TBV)
cpsrA32 = do
    curarch <- (subArch . fromJust' "cpsrA32") <$> getArchData
    if curarch == InstrSet_A32
    then return id
    else if curarch == InstrSet_T32EE
         then error "Invalid INSTRSET change T32EE->A32"
         else return (("SetA32Mode" =:) . (cpsr_jt $ arch_jt InstrSet_A32))

cpsrT32 = do
    curarch <- (subArch . fromJust' "cpsrT32") <$> getArchData
    if curarch == InstrSet_T32
    then return id
    else return (("SetT32Mode" =:) . (cpsr_jt $ arch_jt InstrSet_T32))

cpsrT32EE = do
    curarch <- (subArch . fromJust' "cpsrT32EE") <$> getArchData
    if curarch == InstrSet_T32EE
    then return id
    else if curarch == InstrSet_A32
         then error "Invalid INSTRSET change A32->T32EE"
         else return (("SetT32EEMode" =:) . (cpsr_jt $ arch_jt InstrSet_T32))

cpsrJazelle = error "Jazelle instruction set not currently supported"

cpsr_jt :: (Int, Int) -> (Expr 'TBV -> Expr 'TBV)
cpsr_jt (j, t) =
    let updJ = if j == 1 then bvset [24] else bvclr [24]
        updT = if t == 1 then bvset [5] else bvclr [5]
    in updJ . updT

arch_jt :: ArchSubtype -> (Int, Int)
arch_jt tgtarch =
    case tgtarch of
      InstrSet_A32 -> (0, 0)
      InstrSet_T32 -> (0, 1)
      InstrSet_T32EE -> (1, 1)
      InstrSet_Jazelle -> (1, 0)
