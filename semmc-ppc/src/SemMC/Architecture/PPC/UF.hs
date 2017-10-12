{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.UF (
  uninterpretedFunctions
  ) where

import           GHC.TypeLits
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import           Lang.Crucible.BaseTypes

import           SemMC.Architecture.PPC.Location

uninterpretedFunctions :: forall proxy ppc
                        . (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
                       => proxy ppc
                       -> [(String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)]
uninterpretedFunctions _ =
  [ ("fp.add64",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.add32",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 32 Ctx.::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("fp.sub64",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 64 Ctx.::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("fp.sub32",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType 2 Ctx.::> BaseBVType 32 Ctx.::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("ppc.is_r0",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("ppc.memri_reg",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("ppc.memri_offset",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))
  , ("ppc.memrix_reg",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("ppc.memrix_offset",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 14)))
  , ("ppc.memrr_base",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("ppc.memrr_offset",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (ArchRegWidth ppc))))
  , ("read_mem.8",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))
  , ("read_mem.16",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))
  , ("read_mem.32",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))
  , ("read_mem.64",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 64)))
  , ("write_mem.8",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc) Ctx.::> BaseBVType 8)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("write_mem.16",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc) Ctx.::> BaseBVType 16)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("write_mem.32",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc) Ctx.::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  , ("write_mem.64",
     Some (knownRepr :: Ctx.Assignment BaseTypeRepr (Ctx.EmptyCtx Ctx.::> BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8) Ctx.::> BaseBVType (ArchRegWidth ppc) Ctx.::> BaseBVType 64)),
     Some (knownRepr :: BaseTypeRepr (BaseArrayType (Ctx.SingleCtx (BaseBVType (ArchRegWidth ppc))) (BaseBVType 8))))
  ]
