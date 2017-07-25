module SemMC.Stochastic.Generalize ( generalize ) where


import qualified Data.Parameterized.Map as MapF

import SemMC.Architecture ( Opcode, Operand )
import qualified SemMC.Formula.Formula as F
import SemMC.Stochastic.Monad

generalize :: Syn t arch (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t) arch))
generalize = do
  _ <- undefined
  askFormulas
