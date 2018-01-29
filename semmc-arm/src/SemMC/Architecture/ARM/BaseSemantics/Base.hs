{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Base
    where

import Data.Bits
import Data.Word
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

    }


newARMData :: SemM_ARMData
newARMData = SemM_ARMData
             { subArch = InstrSet_Jazelle -- not supported: force error if not updated to actual
             , condPassed = LitBool False
             , cpsrUpdates = id
             }


type SemARM t a = SemMD t SemM_ARMData a

-- | All ARM registers are 32-bits wide for both A32 and T32.
naturalBitSize :: Int
naturalBitSize = 32

-- | A zero value of the full register width
naturalZero :: Word32
naturalZero = zeroBits

-- | A literal value bitvector of the full register width
naturalLitBV :: Integer -> Expr 'TBV
naturalLitBV n = LitBV naturalBitSize n

-- | A value as a bitvector of the full register width
naturalBV :: ExprType 'TBV
naturalBV = EBV naturalBitSize


-- Note: all ARM documentation references are to:
--   ARM Architecture Reference Manual
--      ARMv8, for ARMv8-A architecture profile
--         Beta (ARM DDI 0487A.a (ID090413)
--         Copyright 2013  (release 04 Sep 2013)
