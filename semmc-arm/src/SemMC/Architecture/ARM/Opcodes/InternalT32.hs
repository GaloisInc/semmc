-- | This module is necessary to get around the TH staging restriction, since we
-- need to refer to 'allOpcodes' from another TH call.

{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes.InternalT32
    ( allT32Opcodes
    , allT32OpcodeInfo
    )
    where

import           Data.Parameterized.Some ( Some(..), mapSome )
import qualified Dismantle.Thumb as T32
import qualified Dismantle.Tablegen.TH.Capture as DT


allT32OpcodeInfo :: [Some (DT.CaptureInfo (T32.Opcode T32.Operand))]
allT32OpcodeInfo = $(DT.captureInfo (const True) ''T32.Opcode)

-- | All opcodes known for the architecture
allT32Opcodes :: [Some (T32.Opcode T32.Operand)]
allT32Opcodes = map (mapSome DT.capturedOpcode) allT32OpcodeInfo
