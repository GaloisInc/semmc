-- | This module is necessary to get around the TH staging restriction, since we
-- need to refer to 'allOpcodes' from another TH call.

{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes.InternalT32
    ( t32Opcodes
    , t32OpcodeInfo
    )
    where

import           Data.Parameterized.Some ( Some(..), mapSome )
import qualified Dismantle.Thumb as T32
import qualified Dismantle.Tablegen.TH.Capture as DT


t32OpcodeInfo :: [Some (DT.CaptureInfo (T32.Opcode T32.Operand))]
t32OpcodeInfo = $(DT.captureInfo (const True) ''T32.Opcode)

-- | All opcodes known for the architecture
t32Opcodes :: [Some (T32.Opcode T32.Operand)]
t32Opcodes = map (mapSome DT.capturedOpcode) t32OpcodeInfo
