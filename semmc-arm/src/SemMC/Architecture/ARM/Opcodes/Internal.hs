-- | This module is necessary to get around the TH staging restriction, since we
-- need to refer to 'allOpcodes' from another TH call.

{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes.Internal
    ( allOpcodes
    , allOpcodeInfo
    )
    where

import           Data.Parameterized.Some ( Some(..), mapSome )
import qualified Dismantle.ARM as ARM
import qualified Dismantle.Tablegen.TH.Capture as DT


allOpcodeInfo :: [Some (DT.CaptureInfo (ARM.Opcode ARM.Operand))]
allOpcodeInfo = $(DT.captureInfo (const True) ''ARM.Opcode)

-- | All opcodes known for the architecture
allOpcodes :: [Some (ARM.Opcode ARM.Operand)]
allOpcodes = map (mapSome DT.capturedOpcode) allOpcodeInfo
