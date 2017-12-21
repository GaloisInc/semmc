{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Architecture.PPC32.Opcodes (
  baseOpcodes,
  pseudoOpcodes,
  allOpcodes,
  allOpcodeInfo,
  allSemantics
  ) where

import qualified Data.ByteString as BS
import           System.FilePath ( (<.>) )

import           Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH.Capture as DT
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.TH as STH

import           SemMC.Architecture.PPC32 ( PPC, PseudoOpcode )
import           SemMC.Architecture.PPC32.Opcodes.Internal ( allOpcodes, allOpcodeInfo )

-- | The base set of opcodes for learning
--
-- The base set is the set of all opcodes that have on-disk base definitions
-- available (determined at compile time).
baseOpcodes :: [Some (PPC.Opcode PPC.Operand)]
baseOpcodes = map fst $(STH.attachSemantics (\(Some x) -> show x <.> "sem") allOpcodes ["data/32/base"])

-- | Every opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
allSemantics :: [(Some (PPC.Opcode PPC.Operand), BS.ByteString)]
allSemantics = $(STH.attachSemantics (\(Some x) -> show x <.> "sem") allOpcodes [ "data/32/base"
                                                                         , "data/32/manual"
                                                                         , "data/32/learned"
                                                                         ])

-- | Pseudo-opcodes used for learning; these are not part of 'allOpcodes'
-- because they are not real opcodes
pseudoOpcodes :: [Some (P.Pseudo PPC PPC.Operand)]
pseudoOpcodes = $(DT.captureDictionaries (const True) ''PseudoOpcode)
