{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Registers
    where

import SemMC.DSL
import SemMC.Architecture.ARM.BaseSemantics.Base


pc :: Location 'TBV
pc = LiteralLoc Literal { lName = "PC"
                        , lExprType = naturalBV
                        }
