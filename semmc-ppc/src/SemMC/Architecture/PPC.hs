{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module SemMC.Architecture.PPC ( AnyPPC, Variant, V32, V64 ) where

data AnyPPC (v :: Variant)

data Variant = V32 | V64

type V32 = 'V32
type V64 = 'V64
