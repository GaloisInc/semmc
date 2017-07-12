module SemMC.Stochastic.Synthesize ( synthesize ) where

import SemMC.Architecture ( Instruction )
import SemMC.Stochastic.Monad

-- | Attempt to stochastically find a program in terms of the base set that has
-- the same semantics as the given instruction.
--
-- Can fail due to timeouts.
synthesize :: Instruction arch
           -> Syn t arch (Maybe [Instruction arch])
synthesize = undefined
