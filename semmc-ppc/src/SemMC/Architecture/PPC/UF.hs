{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module SemMC.Architecture.PPC.UF (
  uninterpretedFunctions,
  ) where

import           GHC.TypeLits
import           Data.Parameterized.Context ( Ctx
                                            , EmptyCtx
                                            , SingleCtx
                                            , (::>)
                                            , type (<+>)
                                            )
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some

import           What4.BaseTypes
import qualified What4.Interface as W
import qualified What4.Expr.Builder as Expr

import           SemMC.Architecture (UninterpFn(..), mkUninterpFn, AccessData(..))
import           SemMC.Architecture.PPC.Location

                             
type family DuplicateCtx (n :: Nat) (x :: k) :: Ctx k where
  DuplicateCtx 0 x = EmptyCtx
  DuplicateCtx n x = (DuplicateCtx (n-1) x) ::> x

uninterpretedFunctions :: forall proxy ppc
                        . (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
                       => proxy ppc
                       -> [Some (UninterpFn ppc)]
uninterpretedFunctions _ = fpUninterpFns @ppc 
                        ++ ppcUninterpFns @ppc
                        ++ readMemUninterpFns @ppc
                        ++ writeMemUninterpFns @ppc
                        ++ clzUninterpFns @ppc
                        ++ popcntUninterpFns @ppc


fpUninterpFns :: forall ppc. (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
              => [Some (UninterpFn ppc)]
fpUninterpFns =
  [ mkUninterpFn @(SingleCtx (BaseFloatType Prec64)) 
                 @(BaseFloatType Prec32) 
                 "fp.double_to_single"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 3 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 24)
                 "fp.tern_op_fpscr"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 2 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 24)
                 "fp.bin_op_fpscr"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 1 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 24)
                  "fp.un_op_fpscr"
                 (\_ -> [])
  ]


ppcUninterpFns :: forall ppc. (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
              => [Some (UninterpFn ppc)]
ppcUninterpFns = [
    mkUninterpFn @(SingleCtx (BaseBVType (ArchRegWidth ppc)))
                 @BaseBoolType
                 "ppc.is_r0"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (ArchRegWidth ppc)))
                 @(BaseBVType (ArchRegWidth ppc))
                 "ppc.memri_reg"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (ArchRegWidth ppc)))
                 @(BaseBVType 16)
                 "ppc.memri_offset"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (ArchRegWidth ppc)))
                 @(BaseBVType (ArchRegWidth ppc))
                 "ppc.memrix_reg"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (ArchRegWidth ppc)))
                 @(BaseBVType 14)
                 "ppc.memrix_offset"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (ArchRegWidth ppc)))
                 @(BaseBVType (ArchRegWidth ppc))
                 "ppc.memrr_base"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (ArchRegWidth ppc)))
                 @(BaseBVType (ArchRegWidth ppc))
                 "ppc.memrr_offset"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 1 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 160)
                 "ppc.fp1"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 2 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 160)
                 "ppc.fp2"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 3 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 160)
                 "ppc.fp3"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 1 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 160)
                 "ppc.vec1"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 2 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 160)
                 "ppc.vec2"
                 (\_ -> [])
  , mkUninterpFn @(    SingleCtx (BaseStructType EmptyCtx) 
                   <+> DuplicateCtx 3 (BaseBVType 128) 
                   <+> SingleCtx (BaseBVType 32))
                 @(BaseBVType 160)
                 "ppc.vec3"
                 (\_ -> [])
  ]


-- | The basic type of indices into the array
type BaseIdxType ppc = BaseBVType (ArchRegWidth ppc)
-- | The type of the memory array
type BaseMemType ppc = BaseArrayType (SingleCtx (BaseIdxType ppc)) (BaseBVType 8) 

readMemUninterpFns :: forall ppc. (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
                   => [Some (UninterpFn ppc)]
readMemUninterpFns = [
    mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc)
                 @(BaseBVType 8)
                 "read_mem.8"
                 (\ (_ Ctx.:> _ Ctx.:> idx) -> [ReadData idx])
  , mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc)
                 @(BaseBVType 16)
                 "read_mem.16"
                 (\ (_ Ctx.:> _ Ctx.:> idx) -> [ReadData idx])
  , mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc)
                 @(BaseBVType 32)
                 "read_mem.32"
                 (\ (_ Ctx.:> _ Ctx.:> idx) -> [ReadData idx])
  , mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc)
                 @(BaseBVType 64)
                 "read_mem.64"
                 (\ (_ Ctx.:> _ Ctx.:> idx) -> [ReadData idx])
  , mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc)
                 @(BaseBVType 128)
                 "read_mem.128"
                 (\ (_ Ctx.:> _ Ctx.:> idx) -> [ReadData idx])
  ]

mkUninterpFnWriteMem :: forall ppc (n :: Nat).
                   (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc, KnownNat n, 1 <= n)
                => String -> Some (UninterpFn ppc)
mkUninterpFnWriteMem name = 
    mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc ::> BaseBVType n)
                 @(BaseMemType ppc)
                 name
                 $ \(_ Ctx.:> _ Ctx.:> idx Ctx.:> val) -> [WriteData idx val]

writeMemUninterpFns :: forall ppc. (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
              => [Some (UninterpFn ppc)]
writeMemUninterpFns = [ mkUninterpFnWriteMem @ppc @8   "write_mem.8"
                      , mkUninterpFnWriteMem @ppc @16  "write_mem.16"
                      , mkUninterpFnWriteMem @ppc @32  "write_mem.32"
                      , mkUninterpFnWriteMem @ppc @64  "write_mem.64"
                      , mkUninterpFnWriteMem @ppc @128 "write_mem.128"]

clzUninterpFns :: forall ppc. (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
               => [Some (UninterpFn ppc)]
clzUninterpFns = [
    mkUninterpFn @(SingleCtx (BaseBVType 32)) 
                 @(BaseBVType 32)
                 "clz.32"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType 64))
                 @(BaseBVType 64)
                 "clz.64"
                 (\_ -> [])
  ]


popcntUninterpFns :: forall ppc. (KnownNat (ArchRegWidth ppc), 1 <= ArchRegWidth ppc)
                     => [Some (UninterpFn ppc)]
popcntUninterpFns = [
    mkUninterpFn @(SingleCtx (BaseBVType 32)) 
                 @(BaseBVType 32)
                 "popcnt.32"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType 64))
                 @(BaseBVType 64)
                 "popcnt.64"
                 (\_ -> [])
  ]

