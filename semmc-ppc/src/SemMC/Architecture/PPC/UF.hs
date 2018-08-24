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
import           What4.BaseTypes

import           SemMC.Architecture.PPC.Location

uninterpretedFunctions :: forall proxy ppc
                        . (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
                       => proxy ppc
                       -> [(String, Some (Assignment BaseTypeRepr), Some BaseTypeRepr)]
uninterpretedFunctions _ =
  [ ("fp.double_to_single",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseFloatType Prec64)),
     Some (knownRepr :: BaseTypeRepr (BaseFloatType Prec32)))
  , ("fp.tern_op_fpscr",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseStructType EmptyCtx ::> BaseBVType 128 ::> BaseBVType 128 ::> BaseBVType 128 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.bin_op_fpscr",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseStructType EmptyCtx ::> BaseBVType 128 ::> BaseBVType 128 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.un_op_fpscr",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseStructType EmptyCtx ::> BaseBVType 128 ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
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
