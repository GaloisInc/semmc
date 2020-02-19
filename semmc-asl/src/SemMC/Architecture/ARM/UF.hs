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
import           Data.Parameterized.Some
import           Control.Applicative ( Const(..) )

import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.Context
import GHC.TypeLits
import qualified SemMC.Architecture as A
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
  [ A.mkUninterpFn @(UFArgs)
                   @(UFRet BaseBoolType)
                   ("UNDEFINED_boolean_0")
                   (\_ -> [])
  , A.mkUninterpFn @(UFArgs)
                   @(UFRet BaseIntegerType)
                   ("UNDEFINED_integer_0")
                   (\_ -> [])  
  ]
  ++ (mkUndefBVUF  <$> ([1..32] ++ [40,48,52,64,128,160,256]))
  ++ (mkWriteMemUF <$> [8,16,32,64])
  ++ (mkReadMemUF <$> [8,16,32,64])

-- Standard signatures for "UNDEFINED" functions
type UFArgs = EmptyCtx ::> (BaseStructType (EmptyCtx ::> BaseBoolType))
type UFRet t =
  BaseStructType (EmptyCtx ::> (BaseStructType (EmptyCtx ::> BaseBoolType)) ::> (BaseStructType (EmptyCtx ::> t)))

mkUndefBVUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
            => Integer
            -> A.UninterpFn arm
mkUndefBVUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                      , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(UFArgs)
                   @(UFRet (BaseBVType n))
                   ("UNDEFINED_bitvector_0N_" ++ show n)
                   (\_ -> [])
mkUndefBVUF n | otherwise = error $ "Cannot construct UNDEFINED_bitvector_0N_" ++ show n

mkReadMemUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
            => Integer
            -> A.UninterpFn arm
mkReadMemUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                      , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(EmptyCtx ::> BaseMemType arm ::> BaseIdxType arm)
                   @(BaseBVType n)
                   ("read_mem." ++ show n)
                   (\(_ :> _ :> idx) -> [A.ReadData idx])
mkReadMemUF n | otherwise = error $ "Cannot construct read_mem." ++ show n

mkWriteMemUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
             => Integer
             -> A.UninterpFn arm
mkWriteMemUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
               , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(EmptyCtx ::> BaseMemType arm ::> BaseIdxType arm ::> BaseBVType n)
                   @(BaseMemType arm)
                   ("write_mem." ++ show n)
                   $ \(_ :> _ :> idx :> val) -> [A.WriteData idx val]
mkWriteMemUF n | otherwise = error $ "Cannot construct write_mem." ++ show n
