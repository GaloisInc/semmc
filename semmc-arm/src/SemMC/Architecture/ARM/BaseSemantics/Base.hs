{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Base
    where

import Data.Semigroup
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


-- | The ARM processor has 4 processing modes.  Current semantics
-- support is only defined for A32 and T32, but the others are defined
-- here for completeness.
data ArchSubtype = InstrSet_A32 | InstrSet_T32 | InstrSet_Jazelle | InstrSet_T32EE
                 deriving (Eq, Show)


-- | The 'fArchData' field of the current DSL state is used to contain
-- values used in the Semantics definitions of opcodes.

data SemM_ARMData = SemM_ARMData
    { subArch :: ArchSubtype
      -- ^ specifies the current instruction set being semantically
      -- defined.  This can be done because the Dismantle ARM Opcodes
      -- (A32) are distinct from the Dismantle Thumb Opcodes (T32), so
      -- the current instruction set is deterministically known.  By
      -- encoding it into the DSL state, additional validation and
      -- optimizations can be performed.

    , condPassed :: Expr 'TBool
      -- ^ stores the ConditionPassed expression for the current
      -- Opcode to be used for enabling defLoc updates.

    , cpsrUpdates :: Expr 'TBV -> Expr 'TBV
      -- ^ stores the various expressions that update the CPSR value
      -- for an opcode.  There may be multiple distinct updates to the
      -- CPSR (e.g. condition values, ISETSTATE, etc.), so this helps
      -- to accumulate those updates so that they can be expressed in
      -- a single defLoc update of the CPSR.

    , pcUpdate :: ArchSubtype -> Expr 'TBV
      -- ^ stores the expression used to update the PC after execution
      -- of an opcode.  By default, this simply increments the PC to
      -- the next instruction, but branches and other PC-modifying
      -- opcodes can effect different PC results.  This operation is
      -- executed at the *end* of the opcode DSL definition, so the
      -- ArchSubtype passed to it is the *result* of any changes made
      -- by the Opcode.
    }


newARMData :: SemM_ARMData
newARMData = SemM_ARMData
             { subArch = InstrSet_Jazelle -- not supported: force error if not updated to actual
             , condPassed = LitBool True
             , cpsrUpdates = id
             , pcUpdate = nextInstruction
             }
    where nextInstruction sub = case sub of
                                  InstrSet_A32 -> bvadd (Loc pc) (naturalLitBV 4)
                                  InstrSet_T32 -> bvadd (Loc pc) (naturalLitBV 2)
                                  _ -> error $ "Execution PC update not currently supported\
                                              \ for this arch subtype: " <> show sub


type SemARM t a = SemMD t SemM_ARMData a

-- Note: all ARM documentation references are to:
--   ARM Architecture Reference Manual
--      ARMv8, for ARMv8-A architecture profile
--         Beta (ARM DDI 0487A.a (ID090413)
--         Copyright 2013  (release 04 Sep 2013)
