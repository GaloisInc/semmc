module SemMC.Symbolic (
  Sym, Backend, SemMCW4State(..)
  ) where

import qualified What4.Expr.Builder as W4
import qualified Lang.Crucible.Backend.Online as CRU

data SemMCW4State t = SemMCW4State

type Sym t fs = W4.ExprBuilder t SemMCW4State fs
type Backend solver t fs = CRU.OnlineBackend solver t SemMCW4State fs
