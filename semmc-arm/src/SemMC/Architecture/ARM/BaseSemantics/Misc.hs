{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module SemMC.Architecture.ARM.BaseSemantics.Misc
    ( miscSemantics
    )
    where

import           Data.Parameterized.Context
import qualified Dismantle.Thumb as T
import           Prelude hiding ( concat, pred )
import           SemMC.Architecture.ARM.BaseSemantics.Base
import           SemMC.Architecture.ARM.BaseSemantics.Helpers
import           SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import           SemMC.DSL


miscSemantics :: SemARM 'Top ()
miscSemantics = do

  defineT32Opcode T.TSVC (Empty
                         :> ParamDef "imm" imm0_255 (EBV 8)
                         )
                      $ \_imm -> do
    comment "Supervisor Call (F7.1.241, F7-2926)"
    comment "All SVC calls are handled by an architecture-specific\
            \ instruction implementation at higher levels of semantic\
            \ evaluation (e.g. Macaw) because the semantics are\
            \ outside of the scope of individual instruction effects\
            \ on machine state."

  defineT32Opcode T.THINT (Empty
                          :> ParamDef "imm" imm0_15 (EBV 4)
                          )
                      $ \_imm -> do
    comment "Hints (F3.2.5, F3-2440)"
    comment "All hints are handled by an architecture-specific instruction\
            \ implementation at higher levels of semantic evaluation\
            \ (e.g. Macaw) because the semantics are outside of the scope\
            \ of individual instruction effects on machine state."
    -- imm is only 4 bits and is therefore assumed to be OpA .. verify
    -- as more of these are seen.
    comment "imm == 0: NOP hint (F7.1.124, F7-2734)"
    comment "          Does nothing.  Useful for instruction alignment purposes."
    comment "imm == 1: YIELD hint (F7.1.291, F7-3026)"
    comment "          Indicates an advantageous processor yield point, no semantic content."
            -- Calls Hint_Yield() (AppxG-5047); no definition
    comment "imm == 2: WFE (Wait For Event) hint (F7-3022)"
    comment "          Enter low-power state until one of a particular events occurs, including SEV events."
            -- Calls EventRegistered() (AppxG-5047); no definition
            -- Calls ClearEventRegister() (AppxG-5047); no definition
            -- Calls AArch32.WFxTrap(EL1, True) (AppxG-4943); definition not implemented here
            -- Calls WaitForEvent();  (AppxG-5047); no definition
    comment "imm == 3: WFI (Wait For Interrupt) hint (F7-3024)"
    comment "          Enter low-power state until one of a number of asynchronous events occurs."
            -- InterruptPending(); no definition
            -- Calls WaitForInterrupt() (AppxG-5047); no definition
    comment "imm == 4: SEV (Send Event) hint (F7-2804)"
    comment "          Signal an event to all PEs in the multiprocessor system."
            -- Calls SendEvent() (AppxG-5047); no definition
    comment "imm == 5: SEVL (Send Event Local) hint (F7-2805)"
    comment "          Signal an event locally, but not necessarily to all PEs in the multiprocessor system."
            -- Calls SendEvent() (AppxG-5047); no definition
