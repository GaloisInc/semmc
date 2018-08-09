module SemMC.Stochastic.Generalize ( generalize ) where


import qualified Data.Parameterized.Map as MapF

import SemMC.Architecture ( Opcode, Operand )
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym )
import SemMC.Stochastic.Monad

-- | Attempt to generalize over different immediate sizes.
--
-- This is really only important for x86(_64).  For other architectures,
-- operands are always the same size.  In our eventual representation of
-- x86(_64) instructions, it may be the case that immediates with different
-- sizes/extension properties will be represented by different opcodes.
--
-- For now, this function is a no-op and just returns all of the learned
-- formulas.
generalize :: Syn t solver fs arch (MapF.MapF (Opcode arch (Operand arch)) (F.ParameterizedFormula (Sym t solver fs) arch))
generalize = askFormulas
