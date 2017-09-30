{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Memory (
  manualMemory
  ) where

import Prelude hiding ( concat )
import Control.Monad ( when )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

manualMemory :: (?bitSize :: BitSize) => SemM 'Top ()
manualMemory = do
  defineOpcode "LBZ" $ do
    comment "Load Byte and Zero (D-form)"
    loadAndZero 1
  defineOpcode "LHZ" $ do
    comment "Load Halfword and Zero (D-form)"
    loadAndZero 2
  defineOpcode "LWZ" $ do
    comment "Load Word and Zero (D-form)"
    loadAndZero 4

  -- when (?bitSize == Size64) $ do
  --   defineOpcode "LD" $ do
  --     -- Note that this is not the same as the L*Z forms above; the displacement
  --     -- from the base is shorter (it can be due to alignment).
  --     comment "Load Doubleword (DS-form)"

-- | Define a load and zero of the given number of bytes.
loadAndZero :: (?bitSize :: BitSize) => Int -> SemM 'Def ()
loadAndZero nBytes = do
  rT <- param "rT" gprc naturalBV
  memref <- param "memref" memri EMemRef
  input memref
  input memory
  let rA = memriReg (Loc memref)
  let disp = memriOffset (Loc memref)
  let b = ite (isR0 rA) (naturalLitBV 0x0) rA
  let ea = bvadd b (sext disp)
  defLoc rT (zext (readMem (Loc memory) ea nBytes))
