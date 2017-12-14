{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module SemMC.Stochastic.Constraints (
  SynC
  ) where

import qualified Data.EnumF as E
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.HasRepr as H
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.SymbolRepr as SR

import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as CS
import qualified SemMC.Stochastic.Pseudo as P

-- | Synthesis constraints.
type SynC arch = ( P.OrdF (A.Opcode arch (A.Operand arch))
                 , P.OrdF (A.Operand arch)
                 , P.ShowF (A.Operand arch)
                 , P.ShowF (A.Opcode arch (A.Operand arch))
                 , D.ArbitraryOperand (A.Operand arch)
                 , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
                 , D.ArbitraryOperands (P.Pseudo arch) (A.Operand arch)
                 , E.EnumF (A.Opcode arch (A.Operand arch))
                 , H.HasRepr (A.Opcode arch (A.Operand arch)) (SL.List SR.SymbolRepr)
                 , H.HasRepr (P.Pseudo arch (A.Operand arch)) (SL.List SR.SymbolRepr)
                 , CS.ConcreteArchitecture arch
                 , P.ArchitectureWithPseudo arch
                 )
