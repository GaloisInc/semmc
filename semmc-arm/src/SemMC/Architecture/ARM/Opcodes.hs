{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes
    ( allA32Semantics
    , allA32OpcodeInfo
    , allA32Opcodes
    , allT32Semantics
    , allT32OpcodeInfo
    , allT32Opcodes
    )
    where

import qualified Data.ByteString as BS
import           Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.ARM as A32
import qualified Dismantle.Thumb as T32
import           SemMC.Architecture.ARM.Opcodes.InternalA32 ( allA32Opcodes, allA32OpcodeInfo )
import           SemMC.Architecture.ARM.Opcodes.InternalT32 ( allT32Opcodes, allT32OpcodeInfo )
import qualified SemMC.TH as STH
import           System.FilePath ( (<.>) )


-- | Every A32 opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
allA32Semantics :: [(Some (A32.Opcode A32.Operand), BS.ByteString)]
allA32Semantics = $(STH.attachSemantics (\(Some x) -> show x <.> "sem") allA32Opcodes [ "data/sem" ])


-- | Every T32 opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
allT32Semantics :: [(Some (T32.Opcode T32.Operand), BS.ByteString)]
allT32Semantics = $(STH.attachSemantics (\(Some x) -> show x <.> "sem") allT32Opcodes [ "data/sem" ])
