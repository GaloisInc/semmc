{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes
    ( allA32Semantics
    , a32Semantics
    , allA32OpcodeInfo
    , allA32Opcodes
    , a32Opcodes
    , a32DefinedFunctions
    , allT32Semantics
    , t32Semantics
    , allT32OpcodeInfo
    , allT32Opcodes
    , t32Opcodes
    , t32DefinedFunctions
    )
    where

import qualified Data.ByteString as BS
import           Data.Parameterized.Some

import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.T32 as T32

import qualified Dismantle.Tablegen.TH.Capture as DT

import           SemMC.Architecture.ARM.Combined
import           SemMC.Architecture.ARM.Opcodes.InternalA32 ( a32Opcodes, a32OpcodeInfo )
import           SemMC.Architecture.ARM.Opcodes.InternalT32 ( t32Opcodes, t32OpcodeInfo )
import qualified SemMC.TH as STH
import           System.FilePath ( (<.>) )

-- | Every A32 opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
a32Semantics :: [(Some (A32.Opcode A32.Operand), BS.ByteString)]
a32Semantics = $(STH.attachSemantics (\(Some x) -> show x <.> "sem") a32Opcodes [ "data/sem" ])

allA32Semantics :: [(Some (ARMOpcode ARMOperand), BS.ByteString)]
allA32Semantics = fmap aconv a32Semantics
    where aconv :: (Some (A32.Opcode A32.Operand), BS.ByteString) -> (Some (ARMOpcode ARMOperand), BS.ByteString)
          aconv (o,b) = (mapSome A32Opcode o, b)

allA32OpcodeInfo :: [Some (DT.CaptureInfo (ARMOpcode ARMOperand))]
allA32OpcodeInfo = map (mapSome intoARMOpcode) a32OpcodeInfo
    where intoARMOpcode :: DT.CaptureInfo (A32.Opcode A32.Operand) a ->
                           DT.CaptureInfo (ARMOpcode ARMOperand) a
          intoARMOpcode ci = ci { DT.capturedOpcode = A32Opcode (DT.capturedOpcode ci) }

-- | All opcodes known for the architecture
allA32Opcodes :: [Some (ARMOpcode ARMOperand)]
allA32Opcodes = map (mapSome A32Opcode) a32Opcodes

a32DefinedFunctions :: [(String, BS.ByteString)]
a32DefinedFunctions = $(STH.attachDefinedFunctions [ "data/sem" ])

-- ----------------------------------------------------------------------

-- | Every T32 opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
t32Semantics :: [(Some (T32.Opcode T32.Operand), BS.ByteString)]
t32Semantics = $(STH.attachSemantics (\(Some x) -> show x <.> "sem") t32Opcodes [ "data/sem" ])


allT32Semantics :: [(Some (ARMOpcode ARMOperand), BS.ByteString)]
allT32Semantics = fmap tconv t32Semantics
    where tconv :: (Some (T32.Opcode T32.Operand), BS.ByteString) -> (Some (ARMOpcode ARMOperand), BS.ByteString)
          tconv (o,b) = (mapSome T32Opcode o, b)

allT32OpcodeInfo :: [Some (DT.CaptureInfo (ARMOpcode ARMOperand))]
allT32OpcodeInfo = map (mapSome intoARMOpcode) t32OpcodeInfo
    where intoARMOpcode :: DT.CaptureInfo (T32.Opcode T32.Operand) a ->
                           DT.CaptureInfo (ARMOpcode ARMOperand) a
          intoARMOpcode ci = ci { DT.capturedOpcode = T32Opcode (DT.capturedOpcode ci) }

-- | All opcodes known for the architecture
allT32Opcodes :: [Some (ARMOpcode ARMOperand)]
allT32Opcodes = map (mapSome T32Opcode) t32Opcodes

t32DefinedFunctions :: [(String, BS.ByteString)]
t32DefinedFunctions = $(STH.attachDefinedFunctions [ "data/sem" ])
