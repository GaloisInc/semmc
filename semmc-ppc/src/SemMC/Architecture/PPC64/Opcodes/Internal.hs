-- | This module is necessary to get around the TH staging restriction, since we
-- need to refer to 'allOpcodes' from another TH call.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Architecture.PPC64.Opcodes.Internal (
  allOpcodes,
  allOpcodeInfo
  ) where

import           Data.Parameterized.Some ( Some(..), mapSome )
import qualified Dismantle.PPC as PPC
import qualified Dismantle.Tablegen.TH.Capture as DT

allOpcodeInfo :: [Some (DT.CaptureInfo (PPC.Opcode PPC.Operand))]
allOpcodeInfo = $(DT.captureInfo (const True) ''PPC.Opcode)

-- | All opcodes known for the architecture
allOpcodes :: [Some (PPC.Opcode PPC.Operand)]
allOpcodes = map (mapSome DT.capturedOpcode) allOpcodeInfo

