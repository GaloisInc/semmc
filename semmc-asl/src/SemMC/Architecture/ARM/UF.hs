{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}


module SemMC.Architecture.ARM.UF
    ( uninterpretedFunctions
    , mkReadMemUF
    , mkWriteMemUF
    )
    where

import           Data.Parameterized.WithRepr
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some
import           Control.Applicative ( Const(..) )
import qualified Data.Text as T
import           Data.Maybe ( catMaybes )

import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.Context
import GHC.TypeLits
import qualified SemMC.Architecture as A
import qualified Language.ASL.Globals as G

import What4.BaseTypes
import Data.Proxy

-- | The basic type of indices into the array
type BaseIdxType arch = BaseBVType (A.RegWidth arch)
-- | The type of the memory array
type BaseMemType arch = BaseArrayType (SingleCtx (BaseIdxType arch)) (BaseBVType 8)

uninterpretedFunctions :: forall proxy arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm) =>
                         proxy arm
                       -> [A.UninterpFn arm]
uninterpretedFunctions _ =
  [ A.mkUninterpFn @(EmptyCtx)
                   @(BaseBoolType)
                   ("UNDEFINED_boolean")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx)
                   @(BaseIntegerType)
                   ("UNDEFINED_integer")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx Ctx.::> G.GlobalsType "GPRS" Ctx.::> BaseBVType 4)
                   @(BaseBVType 32)
                   ("gpr_get")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx Ctx.::> G.GlobalsType "SIMDS" Ctx.::> BaseBVType 8)
                   @(BaseBVType 128)
                   ("simd_get")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx Ctx.::> G.GlobalsType "GPRS" Ctx.::> BaseBVType 4 Ctx.::> BaseBVType 32)
                   @(G.GlobalsType "GPRS")
                   ("gpr_set")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx Ctx.::> G.GlobalsType "SIMDS" Ctx.::> BaseBVType 8 Ctx.::> BaseBVType 128)
                   @(G.GlobalsType "SIMDS")
                   ("simd_set")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx)
                   @(G.GlobalsType "GPRS")
                   ("init_gprs")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx)
                   @(G.GlobalsType "SIMDS")
                   ("init_simds")
                   (\_ -> [])
  ]
  ++ (mkUndefBVUF  <$> ([1..32] ++ [40,48,52,64,65,128,160,256]))
  ++ (mkAssertBVUF <$> [1..65])
  ++ (mkWriteMemUF <$> [8,16,32,64])
  ++ (mkReadMemUF <$> [8,16,32,64])
  ++ (catMaybes $ FC.toListFC mkGlobalUF G.untrackedGlobals)

-- Standard signatures for "UNDEFINED" functions
type UFArgs = EmptyCtx
type UFRet t = t

mkUndefBVUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
            => Integer
            -> A.UninterpFn arm
mkUndefBVUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                      , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(UFArgs)
                   @(UFRet (BaseBVType n))
                   ("UNDEFINED_bitvector_" ++ show n)
                   (\_ -> [])
mkUndefBVUF n | otherwise = error $ "Cannot construct UNDEFINED_bitvector_0N_" ++ show n

mkAssertBVUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
             => Integer
             -> A.UninterpFn arm
mkAssertBVUF n
  | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
  , Just NR.LeqProof <- NR.isPosNat (knownNat @n)
  = A.mkUninterpFn @(EmptyCtx ::> BaseBoolType ::> BaseBVType n) @(BaseBVType n) ("assertBV_" ++ show n) (\_ -> [])


mkGlobalUF :: G.Global tp -> Maybe (A.UninterpFn arm)
mkGlobalUF gb =
  let
    name = "INIT_GLOBAL_" ++ (T.unpack $ G.gbName gb)
  in case G.gbType gb of
    BaseBVRepr (nr :: NatRepr n) -> withKnownNat nr $ Just $ A.mkUninterpFn @(EmptyCtx) @(BaseBVType n) name (\_ -> [])
    BaseIntegerRepr -> Just $ A.mkUninterpFn @EmptyCtx @BaseIntegerType name (\_ -> [])
    BaseBoolRepr -> Just $ A.mkUninterpFn @EmptyCtx @BaseBoolType name (\_ -> [])
    BaseArrayRepr (Empty :> BaseIntegerRepr) (BaseBVRepr nr) |
      Just Refl <- testEquality nr (knownNat @64) ->
      Just $ A.mkUninterpFn @EmptyCtx @(BaseArrayType (EmptyCtx ::> BaseIntegerType) (BaseBVType 64)) name (\_ -> [])
    BaseArrayRepr (Empty :> BaseBVRepr _) _ -> Nothing
    x -> error $ "Unexpected globals type: " ++ show x

mkReadMemUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
            => Integer
            -> A.UninterpFn arm
mkReadMemUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                      , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(EmptyCtx ::> BaseMemType arm ::> BaseIdxType arm)
                   @(BaseBVType n)
                   ("read_mem_" ++ show n)
                   (\(_ :> _ :> idx) -> [A.ReadData idx])
mkReadMemUF n | otherwise = error $ "Cannot construct read_mem." ++ show n

mkWriteMemUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
             => Integer
             -> A.UninterpFn arm
mkWriteMemUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
               , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(EmptyCtx ::> BaseMemType arm ::> BaseIdxType arm ::> BaseBVType n)
                   @(BaseMemType arm)
                   ("write_mem_" ++ show n)
                   $ \(_ :> _ :> idx :> val) -> [A.WriteData idx val]
mkWriteMemUF n | otherwise = error $ "Cannot construct write_mem." ++ show n
