{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module OpcodeLists (
  allOpcodes32,
  pseudoOps32,
  allOpcodes64,
  pseudoOps64
  ) where

import           Data.Parameterized.Some ( Some(..) )

import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH.Capture as DT

import qualified SemMC.Architecture.Pseudo as AP

import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC64 as PPC64

allOpcodes32 :: [Some (PPC.Opcode PPC.Operand)]
allOpcodes32 = [Some PPC.ADD4, Some PPC.ADD4o]

pseudoOps32 :: [Some ((AP.Pseudo PPC32.PPC) PPC.Operand)]
pseudoOps32 = $(DT.captureDictionaries (const True) ''PPC32.PseudoOpcode)

allOpcodes64 :: [Some (PPC.Opcode PPC.Operand)]
allOpcodes64 = [Some PPC.OR, Some PPC.ORI]

pseudoOps64 :: [Some ((AP.Pseudo PPC64.PPC) PPC.Operand)]
pseudoOps64 = $(DT.captureDictionaries (const True) ''PPC64.PseudoOpcode)
