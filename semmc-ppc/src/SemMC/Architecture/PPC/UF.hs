{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.UF (
  uninterpretedFunctions
  ) where

import           GHC.TypeLits
import           Data.Parameterized.Context ( Assignment
                                            , EmptyCtx
                                            , SingleCtx
                                            , (::>))
import           Data.Parameterized.Some ( Some(..) )
import           Lang.Crucible.BaseTypes

import           SemMC.Architecture.PPC.Location

uninterpretedFunctions :: forall proxy ppc
                        . (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
                       => proxy ppc
                       -> [(String, Some (Assignment BaseTypeRepr), Some BaseTypeRepr)]
uninterpretedFunctions _ =
  [ ("fp.add64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64 ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.add32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.sub64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64 ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.sub32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.mul64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64 ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.mul32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.div64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64 ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.div32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.muladd64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64 ::> BaseBVType 64 ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.muladd32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32 ::> BaseBVType 32 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.negate64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.negate32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  -- Round a double precision floating point value to single precision
  , ("fp.round_single",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  -- Convert a single precision floating point value to double precision
  , ("fp.single_to_double",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.abs",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.is_qnan32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("fp.is_qnan64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("fp.is_snan32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("fp.is_snan64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("fp.lt",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64 ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("ppc.is_r0",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("ppc.memri_reg",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("ppc.memri_offset",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))
  , ("ppc.memrix_reg",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("ppc.memrix_offset",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 14)))
  , ("ppc.memrr_base",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("ppc.memrr_offset",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("ppc.fp1",
     Some (knownRepr :: Assignment BaseTypeRepr ( EmptyCtx
                                                  ::> BaseStructType EmptyCtx
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 160)))
  , ("ppc.fp2",
     Some (knownRepr :: Assignment BaseTypeRepr ( EmptyCtx
                                                  ::> BaseStructType EmptyCtx
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 160)))
  , ("ppc.fp3",
     Some (knownRepr :: Assignment BaseTypeRepr ( EmptyCtx
                                                  ::> BaseStructType EmptyCtx
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 160)))
  , ("ppc.vec1",
     Some (knownRepr :: Assignment BaseTypeRepr ( EmptyCtx
                                                  ::> BaseStructType EmptyCtx
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 160)))
  , ("ppc.vec2",
     Some (knownRepr :: Assignment BaseTypeRepr ( EmptyCtx
                                                  ::> BaseStructType EmptyCtx
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 160)))
  , ("ppc.vec3",
     Some (knownRepr :: Assignment BaseTypeRepr ( EmptyCtx
                                                  ::> BaseStructType EmptyCtx
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 128
                                                  ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 160)))
  , ("read_mem.8",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))
  , ("read_mem.16",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))
  , ("read_mem.32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("read_mem.64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("read_mem.128",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 128)))
  , ("write_mem.8",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc) ::> BaseBVType 8)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("write_mem.16",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc) ::> BaseBVType 16)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("write_mem.32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc) ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("write_mem.64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc) ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("write_mem.128",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) ::> BaseBVType (ArchRegWidth ppc) ::> BaseBVType 128)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("test_bit_dynamic",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("clz.32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("clz.64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("popcnt.32",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("popcnt.64",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  ]
