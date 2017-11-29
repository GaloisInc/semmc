{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Core.Registers (
  ip,
  lnk,
  ctr,
  cr,
  fpscr,
  xer,
  memory
  ) where

import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core.BitSize

-- Registers

ip :: (?bitSize :: BitSize) => Location 'TBV
ip = LiteralLoc Literal { lName = "IP"
                        , lExprType = naturalBV
                        }

lnk :: (?bitSize :: BitSize) => Location 'TBV
lnk = LiteralLoc Literal { lName = "LNK"
                         , lExprType = naturalBV
                         }

ctr :: (?bitSize :: BitSize) => Location 'TBV
ctr = LiteralLoc Literal { lName = "CTR"
                         , lExprType = naturalBV
                         }

-- | The CR is always 32 bits
cr :: Location 'TBV
cr = LiteralLoc Literal { lName = "CR"
                        , lExprType = EBV 32
                        }

-- | The FPSCR is always 32 bits
fpscr :: Location 'TBV
fpscr = LiteralLoc Literal { lName = "FPSCR"
                           , lExprType = EBV 32
                           }

xer :: (?bitSize :: BitSize) => Location 'TBV
xer = LiteralLoc Literal { lName = "XER"
                         , lExprType = naturalBV
                         }

memory :: Location 'TMemory
memory = LiteralLoc Literal { lName = "Mem"
                            , lExprType = EMemory
                            }
