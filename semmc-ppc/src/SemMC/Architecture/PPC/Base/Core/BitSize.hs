{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module SemMC.Architecture.PPC.Base.Core.BitSize (
  BitSize(..),
  bitSizeValue,
  naturalBV
  ) where

import SemMC.DSL

data BitSize = Size32
             | Size64
             deriving (Eq, Show, Read)

bitSizeValue :: BitSize -> Int
bitSizeValue Size32 = 32
bitSizeValue Size64 = 64

naturalBV :: (?bitSize :: BitSize) => ExprTypeRepr 'TBV
naturalBV = EBV (bitSizeValue ?bitSize)
