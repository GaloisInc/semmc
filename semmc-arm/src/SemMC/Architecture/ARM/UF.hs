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

  , ("arm.conditionPassed",  -- can execute instr?  inputs are Pred and CCR
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 4
                                                         ::> BaseBVType 4)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))

  , ("test_bit_dynamic",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32
                                                         ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))

    -- A32 Operands

  , ("a32.imm12_reg", -- reference to register by register number from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth arm))))
  , ("a32.imm12_off", -- reference to immediate offset value from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))
  , ("a32.imm12_add", -- reference to U flag bit from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))

  , ("a32.am2offset_imm_add", -- reference to U flag bit from an am2offset_imm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("a32.am2offset_imm_imm", -- reference to immediate offset value from an am2offset_imm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))

  , ("a32.modimm_imm", -- reference to octet immediate value from a modimm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))
  , ("a32.modimm_rot", -- reference to 4-bit rotation value a modimm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 4)))

  , ("a32.soregreg_type", -- Extract the two bit shift type
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 2)))
  , ("a32.soregreg_reg1", -- Extract the register containing the shift amount
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth arm))))
  , ("a32.soregreg_reg2", -- Extract the register being shifted
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth arm))))

  , ("a32.soregimm_type", -- Extract the two bit shift type
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 2)))
  , ("a32.soregimm_reg", -- Extract the register being shifted
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth arm))))
  , ("a32.soregimm_imm", -- Extract the shift amount
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 5)))

    -- T32 Operands

  , ("t32.blxtarget_S", -- operand ThumbBlxTarget S bit reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 1)))

  , ("t32.blxtarget_imm10H", -- operand ThumbBlxTarget immediate, upper half reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 10)))

  , ("t32.blxtarget_imm10L", -- operand ThumbBlxTarget immediate, lower half reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 10)))

  , ("t32.blxtarget_J1", -- operand ThumbBlxTarget J1 bit reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 1)))

  , ("t32.blxtarget_J2", -- operand ThumbBlxTarget J1 bit reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 1)))

    -- Standard memory accesses

  , ("read_mem.8",
     Some (knownRepr :: Assignment BaseTypeRepr
                       (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8)
                                 ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))
  , ("read_mem.16",
     Some (knownRepr :: Assignment BaseTypeRepr
                       (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8)
                                 ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))
  , ("read_mem.32",
     Some (knownRepr :: Assignment BaseTypeRepr
                       (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8)
                                 ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("read_mem.64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("read_mem.128",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 128)))
  , ("write_mem.8",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth arm) ::> BaseBVType 8)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8))))
  , ("write_mem.16",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth arm) ::> BaseBVType 16)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8))))
  , ("write_mem.32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth arm) ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8))))
  , ("write_mem.64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth arm) ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8))))
  , ("write_mem.128",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth arm) ::> BaseBVType 128)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8))))

  ]
