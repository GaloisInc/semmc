{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module OpcodeLists (
  allOpcodes,
  pseudoOps
  ) where

import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Unfold as U
import           Data.Parameterized.Witness ( Witness(..) )

import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH as DT

import qualified SemMC.Architecture.PPC as PPC
import qualified SemMC.Formula.Parser as F
import qualified SemMC.Stochastic.Pseudo as P

import qualified Util as U

class (F.BuildOperandList PPC.PPC sh, U.UnfoldShape sh) => BuildAndUnfold sh
instance (F.BuildOperandList PPC.PPC sh, U.UnfoldShape sh) => BuildAndUnfold sh

allOpcodes :: [Some (Witness BuildAndUnfold (PPC.Opcode PPC.Operand))]
allOpcodes = $(DT.captureDictionaries U.matchConstructor ''PPC.Opcode)

pseudoOps :: [Some (Witness (F.BuildOperandList PPC.PPC) ((P.Pseudo PPC.PPC) PPC.Operand))]
pseudoOps = undefined


