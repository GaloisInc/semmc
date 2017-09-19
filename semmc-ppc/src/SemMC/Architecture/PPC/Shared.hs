{-# LANGUAGE DataKinds #-}
-- | Definitions common to PPC32 and PPC64
module SemMC.Architecture.PPC.Shared (
  repr32,
  repr64,
  repr128
  ) where

import Lang.Crucible.BaseTypes ( NatRepr, knownNat )

repr32 :: NatRepr 32
repr32 = knownNat

repr64 :: NatRepr 64
repr64 = knownNat

repr128 :: NatRepr 128
repr128 = knownNat
