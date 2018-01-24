{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.ARM.UF
    ( uninterpretedFunctions
    )
    where

import Data.Parameterized.Context
import Data.Parameterized.Some ( Some(..) )
import GHC.TypeLits
import Lang.Crucible.BaseTypes
import SemMC.Architecture.ARM.Location


uninterpretedFunctions :: forall proxy arm. (KnownNat (ArchRegWidth arm), 1 <= ArchRegWidth arm) =>
                         proxy arm
                       -> [(String, Some (Assignment BaseTypeRepr), Some BaseTypeRepr)]
uninterpretedFunctions _ =
  [ ("arm.is_r15",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("arm.imm12_reg", -- reference to register by register number from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth arm))))
  , ("arm.imm12_imm", -- reference to immediate value from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth arm))))
  , ("arm.imm12_add", -- reference to U flag bit from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  ]
