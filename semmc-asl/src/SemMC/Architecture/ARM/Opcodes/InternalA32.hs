-- | This module is necessary to get around the TH staging restriction, since we
-- need to refer to 'allOpcodes' from another TH call.

{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes.InternalA32
    ( a32Opcodes
    , a32OpcodeInfo
    )
    where

import           Data.Parameterized.Some ( Some(..), mapSome )
import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.Tablegen.TH.Capture as DT


a32OpcodeInfo :: [Some (DT.CaptureInfo (A32.Opcode A32.Operand))]
a32OpcodeInfo = $(DT.captureInfo (const True) ''A32.Opcode)

-- | All opcodes known for the architecture
a32Opcodes :: [Some (A32.Opcode A32.Operand)]
a32Opcodes = map (mapSome DT.capturedOpcode) a32OpcodeInfo
