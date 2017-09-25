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
import qualified Data.Parameterized.Unfold as U
import           Data.Parameterized.Witness ( Witness(..) )

import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH as DT

import qualified SemMC.Formula as F
import qualified SemMC.Stochastic.Pseudo as P

import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC64 as PPC64

import qualified Util as U

class (F.BuildOperandList PPC32.PPC sh, F.ConvertShape sh, U.UnfoldShape sh) => BuildAndUnfold32 sh
instance (F.BuildOperandList PPC32.PPC sh, F.ConvertShape sh, U.UnfoldShape sh) => BuildAndUnfold32 sh

class (F.BuildOperandList PPC64.PPC sh, F.ConvertShape sh, U.UnfoldShape sh) => BuildAndUnfold64 sh
instance (F.BuildOperandList PPC64.PPC sh, F.ConvertShape sh, U.UnfoldShape sh) => BuildAndUnfold64 sh

allOpcodes32 :: [Some (Witness BuildAndUnfold32 (PPC.Opcode PPC.Operand))]
allOpcodes32 = [Some (Witness PPC.OR), Some (Witness PPC.ORI)]
  -- $(DT.captureDictionaries U.matchConstructor ''PPC.Opcode)

pseudoOps32 :: [Some (Witness (F.BuildOperandList PPC32.PPC) ((P.Pseudo PPC32.PPC) PPC.Operand))]
pseudoOps32 = $(DT.captureDictionaries (const True) ''PPC32.PseudoOpcode)

allOpcodes64 :: [Some (Witness BuildAndUnfold64 (PPC.Opcode PPC.Operand))]
allOpcodes64 = [Some (Witness PPC.OR), Some (Witness PPC.ORI)]
  -- $(DT.captureDictionaries U.matchConstructor ''PPC.Opcode)

pseudoOps64 :: [Some (Witness (F.BuildOperandList PPC64.PPC) ((P.Pseudo PPC64.PPC) PPC.Operand))]
pseudoOps64 = $(DT.captureDictionaries (const True) ''PPC64.PseudoOpcode)
