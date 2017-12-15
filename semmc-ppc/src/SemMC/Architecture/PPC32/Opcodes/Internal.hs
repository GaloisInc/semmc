-- | This module is necessary to get around the TH staging restriction, since we
-- need to refer to 'allOpcodes' from another TH call.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Architecture.PPC32.Opcodes.Internal (
  BuildAndUnfold,
  allOpcodes,
  allOpcodeInfo
  ) where

import           Data.Parameterized.Some ( Some(..), mapSome )
import qualified Data.Parameterized.Unfold as U
import           Data.Parameterized.Witness ( Witness(..) )
import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH.Capture as DT

class ({-F.BuildOperandList PPC sh, F.ConvertShape sh,-} U.UnfoldShape sh) => BuildAndUnfold sh
instance ({-F.BuildOperandList PPC sh, F.ConvertShape sh,-} U.UnfoldShape sh) => BuildAndUnfold sh


allOpcodeInfo :: [Some (DT.CaptureInfo BuildAndUnfold (PPC.Opcode PPC.Operand))]
allOpcodeInfo = $(DT.captureInfo (const True) ''PPC.Opcode)

-- | All opcodes known for the architecture
allOpcodes :: [Some (Witness BuildAndUnfold (PPC.Opcode PPC.Operand))]
allOpcodes = map (mapSome DT.capturedOpcode) allOpcodeInfo

