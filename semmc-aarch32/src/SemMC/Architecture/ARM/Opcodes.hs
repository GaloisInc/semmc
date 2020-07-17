{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes
    ( allA32OpcodeInfo
    , allA32Opcodes
    , a32Opcodes
    , allT32OpcodeInfo
    , allT32Opcodes
    , t32Opcodes
    , ASL.loadSemantics
    , ASL.ASLSemantics(..)
    , ASL.ASLSemanticsOpts(..)
    )
    where

import           Data.Parameterized.Some

import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.T32 as T32

import qualified Dismantle.Tablegen.TH.Capture as DT

import           SemMC.Architecture.ARM.Combined
import           SemMC.Architecture.ARM.Opcodes.InternalA32 ( a32Opcodes, a32OpcodeInfo )
import           SemMC.Architecture.ARM.Opcodes.InternalT32 ( t32Opcodes, t32OpcodeInfo )
import qualified SemMC.Architecture.ARM.ASL as ASL


allA32OpcodeInfo :: [Some (DT.CaptureInfo (ARMOpcode ARMOperand))]
allA32OpcodeInfo = map (mapSome intoARMOpcode) a32OpcodeInfo
    where intoARMOpcode :: DT.CaptureInfo (A32.Opcode A32.Operand) a ->
                           DT.CaptureInfo (ARMOpcode ARMOperand) a
          intoARMOpcode ci = ci { DT.capturedOpcode = A32Opcode (DT.capturedOpcode ci) }

-- | All opcodes known for the architecture
allA32Opcodes :: [Some (ARMOpcode ARMOperand)]
allA32Opcodes = map (mapSome A32Opcode) a32Opcodes

-- ----------------------------------------------------------------------

allT32OpcodeInfo :: [Some (DT.CaptureInfo (ARMOpcode ARMOperand))]
allT32OpcodeInfo = map (mapSome intoARMOpcode) t32OpcodeInfo
    where intoARMOpcode :: DT.CaptureInfo (T32.Opcode T32.Operand) a ->
                           DT.CaptureInfo (ARMOpcode ARMOperand) a
          intoARMOpcode ci = ci { DT.capturedOpcode = T32Opcode (DT.capturedOpcode ci) }

-- | All opcodes known for the architecture
allT32Opcodes :: [Some (ARMOpcode ARMOperand)]
allT32Opcodes = map (mapSome T32Opcode) t32Opcodes

