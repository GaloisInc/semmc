module SemMC.Symbolic (
  Sym
  ) where

import qualified Lang.Crucible.Backend.Online as CRU

-- | Symbolic something?
type Sym t solver fs = CRU.OnlineBackend t solver fs
