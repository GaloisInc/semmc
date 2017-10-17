{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Special (
  baseSpecial
  ) where

import Prelude hiding ( concat )
import SemMC.DSL
import SemMC.Architecture.PPC.Base.Core

baseSpecial :: (?bitSize :: BitSize) => SemM 'Top ()
baseSpecial = do
  defineOpcodeWithIP "MFCR" $ do
    rT <- param "rT" gprc naturalBV
    input cr
    defLoc rT (zext (Loc cr))
