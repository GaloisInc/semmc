-- | This module is necessary to get around the TH staging restriction, since we
-- need to refer to 'allOpcodes' from another TH call.

{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes.InternalA32
    ( allA32Opcodes
    , allA32OpcodeInfo
    )
    where

import           Data.Parameterized.Some ( Some(..), mapSome )
import qualified Dismantle.ARM as A32
import qualified Dismantle.Tablegen.TH.Capture as DT


allA32OpcodeInfo :: [Some (DT.CaptureInfo (A32.Opcode A32.Operand))]
allA32OpcodeInfo = $(DT.captureInfo (const True) ''A32.Opcode)

-- | All opcodes known for the architecture
allA32Opcodes :: [Some (A32.Opcode A32.Operand)]
allA32Opcodes = map (mapSome DT.capturedOpcode) allA32OpcodeInfo
