module SemMC.Symbolic (
  Sym
  ) where

import qualified Lang.Crucible.Solver.SimpleBackend as CRU

-- | Symbolic something?
type Sym t = CRU.SimpleBackend t
