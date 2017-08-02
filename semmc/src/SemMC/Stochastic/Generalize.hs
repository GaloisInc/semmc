module SemMC.Stochastic.Generalize ( generalize ) where


import qualified Data.Parameterized.Map as MapF

import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym )
import SemMC.Stochastic.Monad

generalize :: Syn t arch (MapF.MapF (SynthOpcode arch) (F.ParameterizedFormula (Sym t) arch))
generalize = do
  _ <- undefined
  askFormulas
