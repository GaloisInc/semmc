module SemMC.Symbolic (
  Sym
  ) where

import qualified Lang.Crucible.Backend.Simple as CRU

-- | Symbolic something?
type Sym t = CRU.SimpleBackend t
