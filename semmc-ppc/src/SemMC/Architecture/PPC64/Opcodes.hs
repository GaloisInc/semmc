{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Architecture.PPC64.Opcodes (
  baseOpcodes,
  pseudoOpcodes,
  allOpcodes,
  allSemantics
  ) where

import qualified Data.ByteString as BS
import           System.FilePath ( (<.>) )

import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH as DT
import qualified SemMC.Formula as F
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.TH as STH

import           SemMC.Architecture.PPC64 ( PPC, PseudoOpcode )
import           SemMC.Architecture.PPC64.Opcodes.Internal ( BuildAndUnfold, allOpcodes )

-- | The base set of opcodes for learning
--
-- The base set is the set of all opcodes that have on-disk base definitions
-- available (determined at compile time).
baseOpcodes :: [Some (Witness BuildAndUnfold (PPC.Opcode PPC.Operand))]
baseOpcodes = map fst $(STH.attachSemantics (\(Some (Witness x)) -> show x <.> "sem") allOpcodes ["data/64/base"])

-- | Every opcode with a defined semantics (either from the base set, the
-- learned set, or manually defined)
allSemantics :: [(Some (Witness BuildAndUnfold (PPC.Opcode PPC.Operand)), BS.ByteString)]
allSemantics = $(STH.attachSemantics (\(Some (Witness x)) -> show x <.> "sem") allOpcodes [ "data/64/base"
                                                                                          , "data/64/manual"
                                                                                          , "data/64/learned"
                                                                                          ])

-- | Pseudo-opcodes used for learning; these are not part of 'allOpcodes'
-- because they are not real opcodes
pseudoOpcodes :: [Some (Witness (F.BuildOperandList PPC) (P.Pseudo PPC PPC.Operand))]
pseudoOpcodes = $(DT.captureDictionaries (const True) ''PseudoOpcode)
