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
--type BaseMemType arch = BaseArrayType (SingleCtx (BaseIdxType arch)) (BaseBVType 8)
type BaseMemType arch = G.MemoryBaseType

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
  , A.mkUninterpFn @(EmptyCtx)
                   @(G.GlobalsType "__Memory")
                   ("init_memory")
                   (\_ -> [])
  , A.mkUninterpFn @(EmptyCtx ::> BaseBVType 64 ::> BaseBVType 32 ::> BaseBoolType)
                   @(BaseBVType 32)
                   ("fpToFixedJS")
                   (\_ -> [])
  ]
  ++ (mkUndefBVUF  <$> ([1..32] ++ [40,48,52,64,65,128,160,256]))
  ++ (mkAssertBVUF <$> [1..65])
  ++ (mkWriteMemUF <$> [8,16,32,64])
  ++ (mkReadMemUF <$> [8,16,32,64])
  ++ (catMaybes $ FC.toListFC mkGlobalUF G.untrackedGlobals)
  -- uninterpreted floating point operations
  ++ bvUnOp "unsignedRSqrtEstimate" empty [16, 32]
  ++ bvBinOp "fpAdd" fpcrArg [16,32,64]
  ++ bvRelOp "fpCompareUN" fpcrArg [16,32,64]
  ++ bvBinOp "fpMin" fpcrArg [16,32,64]
  ++ bvUnOp "fpProcess" empty [16, 32, 64]
  ++ bvRelOp "fpCompareNE" fpcrArg [16,32,64]
  ++ bvRelOp "fpCompareGT" fpcrArg [8,16,32,64]
  ++ bvBinOp "fpSub" fpcrArg [16,32,64]
  ++ mkAllBV2 "fpToFixed" (\nr mr -> (Some (empty :> BaseBVRepr nr :> BaseBVRepr (NR.knownNat @32) :> BaseBoolRepr :> BaseBVRepr (NR.knownNat @32) :> BaseBVRepr (NR.knownNat @3)), Some (BaseBVRepr mr))) [16, 32, 64] [16, 32, 64]
  ++ mkAllBV2 "fixedToFP" (\mr nr -> (Some (empty :> BaseBVRepr mr :> BaseBVRepr (NR.knownNat @32) :> BaseBoolRepr :> BaseBVRepr (NR.knownNat @32) :> BaseBVRepr (NR.knownNat @3)), Some (BaseBVRepr nr))) [16, 32, 64] [16, 32, 64]
  ++ bvUnOp "fpRecpX" fpcrArg [16, 32, 64]
  ++ bvBinOp "fpMul" fpcrArg [16,32,64]
  ++ bvBinOp "fpRecipStep" empty [16,32]
  ++ bvBinOp "fpMulAddH" fpcrArg [32,64]
  ++ bvBinOp "fpMinNum" fpcrArg [16,32,64]
  ++ bvBinOp "fpMax" fpcrArg [16,32,64]
  ++ bvBinOp "fpMaxNum" fpcrArg [16,32,64]
  ++ bvUnOp "fpScale" fpcrArg [16,32,64]
  ++ bvUnOp "fpRoundIntN" (empty :> BaseBVRepr (NR.knownNat @32) :> BaseBVRepr (NR.knownNat @3) :> BaseBVRepr (NR.knownNat @32)) [16,32,64]
  ++ mkAllBV1 "fpCompare" (\nr -> (Some (empty :> BaseBVRepr nr :> BaseBVRepr nr :> BaseBVRepr (NR.knownNat @32)), Some (BaseBVRepr (NR.knownNat @4)))) [16,32,64]
  ++ bvRelOp "fpCompareGE" fpcrArg [8,16,32,64]
  ++ bvRelOp "fprSqrtStepFused" empty [16,32,64]
  ++ bvRelOp "fpCompareEQ" fpcrArg [16,32,64]
  ++ bvUnOp "fpRecipEstimate" fpcrArg [16,32,64]
  ++ bvUnOp "fpSqrt" fpcrArg [16,32,64]
  ++ mkAllBV2 "fpConvert" (\nr mr -> (Some (empty :> BaseBVRepr nr :> BaseBVRepr (NR.knownNat @32) :> BaseBVRepr (NR.knownNat @3)), Some (BaseBVRepr mr))) [16, 32, 64] [16, 32, 64]
  ++ bvBinOp "fpDiv" fpcrArg [16,32,64]
  ++ bvUnOp "fpSqrtEstimate" fpcrArg [16,32,64]
  ++ bvBinOp "fpTrigSMul" fpcrArg [16,32,64]
  ++ bvBinOp "fpHalvedSub" fpcrArg [16,32,64]
  ++ bvBinOp "fprSqrtStep" empty [16,32]
  ++ mkAllBV1 "fpMulAdd" (\nr -> (Some (empty :> BaseBVRepr nr :> BaseBVRepr nr :> BaseBVRepr nr :> BaseBVRepr (NR.knownNat @32)), Some (BaseBVRepr nr))) [16,32,64]
  ++ bvBinOp "fpRecipStepFused" empty [16,32,64]
  ++ bvBinOp "fpMulX" fpcrArg [16,32,64]
  ++ bvUnOp "fpRoundInt" (empty :> BaseBVRepr (NR.knownNat @32) :> BaseBVRepr (NR.knownNat @3) :> BaseBoolRepr) [16,32,64]
  
  
-- Standard signatures for "UNDEFINED" functions
type UFArgs = EmptyCtx
type UFRet t = t

fpcrArg :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)
fpcrArg = knownRepr
            
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

bvBinOp :: forall arm args. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
        => String
        -> Assignment BaseTypeRepr args
        -> [Integer]
        -> [A.UninterpFn arm]
bvBinOp name args sizes =
  mkAllBV1 name (\nr -> (Some ((empty :> BaseBVRepr nr :> BaseBVRepr nr) <++> args), Some (BaseBVRepr nr))) sizes

bvRelOp :: forall arm args. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
        => String
        -> Assignment BaseTypeRepr args
        -> [Integer]
        -> [A.UninterpFn arm]
bvRelOp name args sizes =
  mkAllBV1 name (\nr -> (Some ((empty :> BaseBVRepr nr :> BaseBVRepr nr) <++> args), Some BaseBoolRepr)) sizes

bvUnOp :: forall arm args. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
       => String
       -> Assignment BaseTypeRepr args
       -> [Integer]
       -> [A.UninterpFn arm]
bvUnOp name args sizes =
  mkAllBV1 name (\nr -> (Some ((empty :> BaseBVRepr nr) <++> args), Some (BaseBVRepr nr))) sizes

mkAllBV1 :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
         => String
         -> (forall n m. 1 <= n => NR.NatRepr n -> (Some (Assignment BaseTypeRepr), Some BaseTypeRepr))
         -> [Integer]
         -> [A.UninterpFn arm]
mkAllBV1 name mksig sizes = [ mkBV1UF name mksig sz | sz <- sizes ]

mkAllBV2 :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
         => String
         -> (forall n m. 1 <= n => 1 <= m => NR.NatRepr n -> NR.NatRepr m -> (Some (Assignment BaseTypeRepr), Some BaseTypeRepr))
         -> [Integer]
         -> [Integer]
         -> [A.UninterpFn arm]
mkAllBV2 name mksig sizes1 sizes2 = [ mkBV2UF name mksig sz1 sz2  | sz1 <- sizes1, sz2 <- sizes2 ]


mkBV1UF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
       => String
       -> (forall n. 1 <= n => NR.NatRepr n -> (Some (Assignment BaseTypeRepr), Some BaseTypeRepr))
       -> Integer
       -> A.UninterpFn arm
mkBV1UF name mksig bvsz
  | Just (Some nr) <- NR.someNat bvsz
  , Just NR.LeqProof <- NR.isPosNat nr
  , (Some args, Some ret) <- mksig nr
  = A.MkUninterpFn (name ++ "_" ++ show bvsz) args ret (\_ -> [])

mkBV2UF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
       => String
       -> (forall n m. 1 <= n => 1 <= m => NR.NatRepr n -> NR.NatRepr m -> (Some (Assignment BaseTypeRepr), Some BaseTypeRepr))
       -> Integer
       -> Integer 
       -> A.UninterpFn arm
mkBV2UF name mksig bvsz1 bvsz2
  | Just (Some bvszRep1) <- NR.someNat bvsz1
  , Just NR.LeqProof <- NR.isPosNat bvszRep1
  , Just (Some bvszRep2) <- NR.someNat bvsz2
  , Just NR.LeqProof <- NR.isPosNat bvszRep2
  , (Some args, Some ret) <- mksig bvszRep1 bvszRep2
  = A.MkUninterpFn (name ++ "_" ++ show bvsz1 ++ "_" ++ show bvsz2) args ret (\_ -> [])

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
