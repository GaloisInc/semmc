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
  mkUninterpFnWriteMem,
  mkUninterpFnReadMem,
  ) where

import           GHC.TypeLits
import           Data.Proxy
import           Data.Parameterized.Context ( Ctx
                                            , EmptyCtx
                                            , SingleCtx
                                            , (::>)
                                            , type (<+>)
                                            )
import qualified Data.Parameterized.Context as Ctx

import           What4.BaseTypes

import           SemMC.Architecture (UninterpFn(..), mkUninterpFn, AccessData(..))
import qualified SemMC.Architecture as A

                             
type family DuplicateCtx (n :: Nat) (x :: k) :: Ctx k where
  DuplicateCtx 0 x = EmptyCtx
  DuplicateCtx n x = (DuplicateCtx (n-1) x) ::> x

uninterpretedFunctions :: forall proxy ppc
                        . (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
                       => proxy ppc
                       -> [UninterpFn ppc]
uninterpretedFunctions _ = fpUninterpFns @ppc
                        ++ ppcUninterpFns @ppc
                        ++ (mkUninterpFnReadMem  @ppc <$> [8,16,32,64,128])
                        ++ (mkUninterpFnWriteMem @ppc <$> [8,16,32,64,128])
                        ++ clzUninterpFns @ppc
                        ++ popcntUninterpFns @ppc


fpUninterpFns :: forall ppc. (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
              => [UninterpFn ppc]
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


ppcUninterpFns :: forall ppc. (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
              => [UninterpFn ppc]
ppcUninterpFns = [
    mkUninterpFn @(SingleCtx (BaseBVType (A.RegWidth ppc)))
                 @BaseBoolType
                 "ppc.is_r0"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (A.RegWidth ppc)))
                 @(BaseBVType (A.RegWidth ppc))
                 "ppc.memri_reg"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (A.RegWidth ppc)))
                 @(BaseBVType 16)
                 "ppc.memri_offset"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (A.RegWidth ppc)))
                 @(BaseBVType (A.RegWidth ppc))
                 "ppc.memrix_reg"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (A.RegWidth ppc)))
                 @(BaseBVType 14)
                 "ppc.memrix_offset"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (A.RegWidth ppc)))
                 @(BaseBVType (A.RegWidth ppc))
                 "ppc.memrr_base"
                 (\_ -> [])
  , mkUninterpFn @(SingleCtx (BaseBVType (A.RegWidth ppc)))
                 @(BaseBVType (A.RegWidth ppc))
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
type BaseIdxType ppc = BaseBVType (A.RegWidth ppc)
-- | The type of the memory array
type BaseMemType ppc = BaseArrayType (SingleCtx (BaseIdxType ppc)) (BaseBVType 8) 

{-
readMemUninterpFns :: forall ppc. (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
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
-}

mkUninterpFnReadMem :: forall ppc. (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
                    => Integer -> UninterpFn ppc
mkUninterpFnReadMem n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                      , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc)
                 @(BaseBVType n)
                 ("read_mem." ++ show n)
                 (\(_ Ctx.:> _ Ctx.:> idx) -> [ReadData idx])
mkUninterpFnReadMem n | otherwise = error $ "Cannot construct read_mem." ++ show n

mkUninterpFnWriteMem :: forall ppc.
                   (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
                => Integer -> UninterpFn ppc
mkUninterpFnWriteMem n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                       , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = mkUninterpFn @(EmptyCtx ::> BaseMemType ppc ::> BaseIdxType ppc ::> BaseBVType n)
                 @(BaseMemType ppc)
                 ("write_mem." ++ show n)
                 $ \(_ Ctx.:> _ Ctx.:> idx Ctx.:> val) -> [WriteData idx val]
mkUninterpFnWriteMem n | otherwise = error $ "Cannot construct write_mem." ++ show n


clzUninterpFns :: forall ppc. (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
               => [UninterpFn ppc]
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


popcntUninterpFns :: forall ppc. (KnownNat (A.RegWidth ppc), 1 <= A.RegWidth ppc)
                     => [UninterpFn ppc]
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

