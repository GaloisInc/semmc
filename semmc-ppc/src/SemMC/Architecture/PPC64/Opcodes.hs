{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Architecture.PPC64.Opcodes (
  baseOpcodes,
  pseudoOpcodes,
  allOpcodes,
  allOpcodeInfo,
  allSemantics,
  allDefinedFunctions
  ) where

import qualified Data.ByteString as BS
import           System.FilePath ( (<.>) )

import           Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH.Capture as DT
import qualified SemMC.Architecture.Pseudo as AP
import qualified SemMC.TH as STH

import           SemMC.Architecture.PPC64 ( PPC, PseudoOpcode )
import           SemMC.Architecture.PPC64.Opcodes.Internal ( allOpcodes, allOpcodeInfo )

-- | The base set of opcodes for learning
--
-- The base set is the set of all opcodes that have on-disk base definitions
-- available (determined at compile time).
baseOpcodes :: [Some (PPC.Opcode PPC.Operand)]
baseOpcodes = map fst $(STH.attachSemantics (\(Some x) -> show x <.> "sem") allOpcodes ["data/64/base"])

-- | Defined functions used by the semantics
allDefinedFunctions :: [(String, BS.ByteString)]
allDefinedFunctions = $(STH.attachDefinedFunctions [ "data/64/base"
                                                   , "data/64/manual"
                                                   , "data/64/learned" ])

-- | Every opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
allSemantics :: [(Some (PPC.Opcode PPC.Operand), BS.ByteString)]
allSemantics = $(STH.attachSemantics (\(Some x) -> show x <.> "sem") allOpcodes [ "data/64/base"
                                                                                          , "data/64/manual"
                                                                                          , "data/64/learned"
                                                                                          ])

-- | Pseudo-opcodes used for learning; these are not part of 'allOpcodes'
-- because they are not real opcodes
pseudoOpcodes :: [Some (AP.Pseudo PPC PPC.Operand)]
pseudoOpcodes = $(DT.captureDictionaries (const True) ''PseudoOpcode)
