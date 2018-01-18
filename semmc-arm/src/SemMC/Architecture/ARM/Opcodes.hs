{-# LANGUAGE TemplateHaskell #-}

module SemMC.Architecture.ARM.Opcodes
    ( allSemantics
    , allOpcodeInfo
    , allOpcodes
    )
    where

import qualified Data.ByteString as BS
import           Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.ARM as ARM
import           SemMC.Architecture.ARM.Opcodes.Internal ( allOpcodes, allOpcodeInfo )
import qualified SemMC.TH as STH
import           System.FilePath ( (<.>) )


-- | Every opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
allSemantics :: [(Some (ARM.Opcode ARM.Operand), BS.ByteString)]
allSemantics = $(STH.attachSemantics (\(Some x) -> show x <.> "sem") allOpcodes [ "data/base"
                                                                               , "data/manual"
                                                                               , "data/learned"
                                                                               ])
