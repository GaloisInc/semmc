{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SemMC.Architecture.ARM.Location
    ( Location(..)
    , PSTATE(..)
    , ArchRegWidth
    , ArchRepr(..)
    , A32
    , T32
    )
    where

import           Control.Applicative ( (<|>) )
import           Data.Maybe ( fromMaybe, catMaybes, maybeToList )

import GHC.TypeNats
import           Data.Typeable
import           Data.Proxy
import qualified Data.Set.NonEmpty
import           Data.Parameterized.Classes
import           Data.Parameterized.Some
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Ctx
import           Data.Parameterized.Context ( pattern (:>), pattern Empty,  pattern EmptyCtx, pattern (::>) )
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Pair ( Pair )
import qualified Data.Parameterized.Pair as Pair
import           Data.Parameterized.Map ( MapF )
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.TH.GADT
import           Data.EnumF ( EnumF )
import qualified Data.EnumF as EnumF
import qualified Data.Text as T
import           Data.Semigroup
import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.T32 as T32
import           GHC.TypeLits
import qualified Data.Word.Indexed as W
import qualified Data.Word.Indexed
import           What4.BaseTypes
import qualified What4.Interface as WI
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Architecture as A
import qualified What4.BaseTypes as WT
import           GHC.TypeNats as TypeNats
import qualified Lang.Crucible.Types as CT
import           Prelude

data A32


data T32

type family ArchRegWidth arch :: Nat

class ArchRepr arch where
  regWidthRepr :: proxy arch -> NatRepr (ArchRegWidth arch)

-- ----------------------------------------------------------------------

-- note: R13 = SP, R14 = LR, and R15 = usually PC
--
-- SP and LR are just referenced as R13 and R14 respectively so that
-- their values do not have to be synchronously maintained, but since
-- R15 is sometimes not the PC value, it is separately managed.

data Location arm :: BaseType -> * where
  LocGPR :: W.W 4 -> Location arm (BaseBVType (ArchRegWidth arm))
  LocPC :: Location arm (BaseBVType (ArchRegWidth arm))
  LocPSTATE :: (1 WT.<= n, KnownNat n) => PSTATE n -> Location arm (BaseBVType n)
  LocMem :: Location arm (BaseArrayType (SingleCtx (BaseBVType (ArchRegWidth arm))) (BaseBVType 8))

locationBitWidthRepr :: forall n arm
                      . ArchRepr arm
                     => 1 WT.<= (ArchRegWidth arm)
                     => Location arm (BaseBVType n)
                     -> WT.NatRepr n
locationBitWidthRepr loc = case loc of
  LocGPR _ -> regWidthRepr (Proxy :: Proxy arm)
  LocPSTATE _ -> CT.knownNat
  LocPC -> regWidthRepr (Proxy :: Proxy arm)

data SomePSTATE where
  SomePSTATE :: (1 WT.<= n, KnownNat n) => PSTATE n -> SomePSTATE

somePSTATEToSomeLoc :: SomePSTATE -> Some (Location arm)
somePSTATEToSomeLoc (SomePSTATE pstate) = Some $ LocPSTATE pstate

allPSTATEs :: [SomePSTATE]
allPSTATEs =
  [ SomePSTATE PSTATE_BTYPE
  , SomePSTATE PSTATE_IT
  , SomePSTATE PSTATE_EL
  , SomePSTATE PSTATE_GE
  , SomePSTATE PSTATE_M ]
  ++
  (map SomePSTATE $
  [ PSTATE_N, PSTATE_C, PSTATE_V, PSTATE_D
  , PSTATE_A, PSTATE_I, PSTATE_F, PSTATE_PAN
  , PSTATE_UAO, PSTATE_DIT, PSTATE_TCO, PSTATE_SS
  , PSTATE_IL, PSTATE_nRW, PSTATE_SP
  , PSTATE_Q, PSTATE_SSBS, PSTATE_J, PSTATE_T, PSTATE_E
  ])


data PSTATE (n :: TypeNats.Nat) where
  PSTATE_N :: PSTATE 1
  PSTATE_Z :: PSTATE 1
  PSTATE_C :: PSTATE 1
  PSTATE_V :: PSTATE 1
  PSTATE_D :: PSTATE 1
  PSTATE_A :: PSTATE 1
  PSTATE_I :: PSTATE 1
  PSTATE_F :: PSTATE 1
  PSTATE_PAN :: PSTATE 1
  PSTATE_UAO :: PSTATE 1
  PSTATE_DIT :: PSTATE 1
  PSTATE_TCO :: PSTATE 1
  PSTATE_BTYPE :: PSTATE 2
  PSTATE_SS :: PSTATE 1
  PSTATE_IL :: PSTATE 1
  PSTATE_EL :: PSTATE 2
  PSTATE_nRW :: PSTATE 1
  PSTATE_SP :: PSTATE 1
  PSTATE_Q :: PSTATE 1
  PSTATE_GE :: PSTATE 4
  PSTATE_SSBS :: PSTATE 1
  PSTATE_IT :: PSTATE 8
  PSTATE_J :: PSTATE 1
  PSTATE_T :: PSTATE 1
  PSTATE_E :: PSTATE 1
  PSTATE_M :: PSTATE 5


deriving instance Ord (PSTATE n)
deriving instance Eq (PSTATE n)
deriving instance Show (PSTATE n)

instance Show (Location arm tp) where
  show (LocGPR gpr) = case gpr of
                        10 -> "sl"
                        11 -> "fp"
                        12 -> "ip"
                        13 -> "sp"
                        14 -> "lr"
                        15 -> "pc"
                        _ -> "r" <> show gpr
  show LocPC = "PC"
  show LocMem = "Mem"
  show (LocPSTATE ps) = show ps

instance ShowF (Location arm)


type family BitWidth (t :: Symbol) :: Nat where
  BitWidth "Bv1"  = 1
  BitWidth "Bv2"  = 2
  BitWidth "Bv3"  = 3
  BitWidth "Bv4"  = 4
  BitWidth "Bv5"  = 5
  BitWidth "Bv6"  = 6
  BitWidth "Bv7"  = 7
  BitWidth "Bv8"  = 8
  BitWidth "Bv9"  = 9
  BitWidth "Bv10" = 10
  BitWidth "Bv11" = 11
  BitWidth "Bv12" = 12
  BitWidth "Bv13" = 13
  BitWidth "Bv14" = 14
  BitWidth "Bv15" = 15
  BitWidth "Bv16" = 16
  BitWidth "Bv17" = 17
  BitWidth "Bv18" = 18
  BitWidth "Bv19" = 19
  BitWidth "Bv20" = 20
  BitWidth "Bv21" = 21
  BitWidth "Bv22" = 22
  BitWidth "Bv23" = 23
  BitWidth "Bv24" = 24
  BitWidth "R" = 32

data SymbolBitWidth s where
  SymbolBitWidth :: (1 WT.<= n, n ~ BitWidth s) => WT.NatRepr n -> SymbolBitWidth s

symbolToBitWidth' :: forall s
                   . CT.SymbolRepr s
                  -> SymbolBitWidth s
symbolToBitWidth' s = case CT.symbolRepr s of
  "Bv1"  -> mkRepr @"Bv1"  knownNat
  "Bv2"  -> mkRepr @"Bv2"  knownNat
  "Bv3"  -> mkRepr @"Bv3"  knownNat
  "Bv4"  -> mkRepr @"Bv4"  knownNat
  "Bv5"  -> mkRepr @"Bv5"  knownNat
  "Bv6"  -> mkRepr @"Bv6"  knownNat
  "Bv7"  -> mkRepr @"Bv7"  knownNat
  "Bv8"  -> mkRepr @"Bv8"  knownNat
  "Bv9"  -> mkRepr @"Bv9"  knownNat
  "Bv10" -> mkRepr @"Bv10" knownNat
  "Bv11" -> mkRepr @"Bv11" knownNat
  "Bv12" -> mkRepr @"Bv12" knownNat
  "Bv13" -> mkRepr @"Bv13" knownNat
  "Bv14" -> mkRepr @"Bv14" knownNat
  "Bv15" -> mkRepr @"Bv15" knownNat
  "Bv16" -> mkRepr @"Bv16" knownNat
  "Bv17" -> mkRepr @"Bv17" knownNat
  "Bv18" -> mkRepr @"Bv18" knownNat
  "Bv19" -> mkRepr @"Bv19" knownNat
  "Bv20" -> mkRepr @"Bv20" knownNat
  "Bv21" -> mkRepr @"Bv21" knownNat
  "Bv22" -> mkRepr @"Bv22" knownNat
  "Bv23" -> mkRepr @"Bv23" knownNat
  "Bv24" -> mkRepr @"Bv24" knownNat
  "R" -> mkRepr @"R" knownNat
  where
    mkRepr :: forall s'
            . (CT.KnownSymbol s', 1 WT.<= (BitWidth s'))
           => WT.NatRepr (BitWidth s')
           -> SymbolBitWidth s
    mkRepr nrepr = case testEquality s (CT.knownSymbol :: CT.SymbolRepr s') of
      Just Refl -> SymbolBitWidth $ nrepr
      _ -> error "Unexpected symbol"

symbolToBitWidth :: forall s. CT.SymbolRepr s -> WT.NatRepr (BitWidth s)
symbolToBitWidth s = case symbolToBitWidth' s of SymbolBitWidth nrepr -> nrepr

data BitWidthSymbol n where
  BitWidthSymbol :: (1 WT.<= n, n ~ BitWidth s) => CT.SymbolRepr s -> BitWidthSymbol n

-- For immediate arguments
bitWidthToSymbol' :: forall n
                   . WT.NatRepr n
                  -> BitWidthSymbol n
bitWidthToSymbol' nr = case WT.intValue nr of
  1  -> mkRepr @"Bv1"  knownNat
  2  -> mkRepr @"Bv2"  knownNat
  3  -> mkRepr @"Bv3"  knownNat
  4  -> mkRepr @"Bv4"  knownNat
  5  -> mkRepr @"Bv5"  knownNat
  6  -> mkRepr @"Bv6"  knownNat
  7  -> mkRepr @"Bv7"  knownNat
  8  -> mkRepr @"Bv8"  knownNat
  9  -> mkRepr @"Bv9"  knownNat
  10 -> mkRepr @"Bv10" knownNat
  11 -> mkRepr @"Bv11" knownNat
  12 -> mkRepr @"Bv12" knownNat
  13 -> mkRepr @"Bv13" knownNat
  14 -> mkRepr @"Bv14" knownNat
  15 -> mkRepr @"Bv15" knownNat
  16 -> mkRepr @"Bv16" knownNat
  17 -> mkRepr @"Bv17" knownNat
  18 -> mkRepr @"Bv18" knownNat
  19 -> mkRepr @"Bv19" knownNat
  20 -> mkRepr @"Bv20" knownNat
  21 -> mkRepr @"Bv21" knownNat
  22 -> mkRepr @"Bv22" knownNat
  23 -> mkRepr @"Bv23" knownNat
  24 -> mkRepr @"Bv24" knownNat
  where
    mkRepr :: forall s'
            . (1 WT.<= (BitWidth s'), CT.KnownSymbol s')
           => WT.NatRepr (BitWidth s')
           -> BitWidthSymbol n
    mkRepr nrepr = case testEquality nrepr nr of
      Just Refl -> BitWidthSymbol $ (CT.knownSymbol :: CT.SymbolRepr s')
      _ -> error "Unexpected symbol"

bitWidthToSymbol :: forall n. WT.NatRepr n -> Some (CT.SymbolRepr)
bitWidthToSymbol nr = case bitWidthToSymbol' nr of BitWidthSymbol srepr -> Some srepr

type instance A.OperandType A32 s = WT.BaseBVType (BitWidth s)

instance A.IsOperandTypeRepr A32 where
  type OperandTypeRepr A32 = CT.SymbolRepr
  operandTypeReprSymbol _ = T.unpack . CT.symbolRepr

type instance ArchRegWidth A32 = 32

instance ArchRepr A32 where
  regWidthRepr _ = WT.knownNat @32

$(return [])

instance TestEquality (PSTATE) where
  testEquality = $(structuralTypeEquality [t|PSTATE|] [])

instance TestEquality (Location arm) where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [(TypeApp (ConType [t|PSTATE|]) AnyType, [|testEquality|])
                   ])

instance OrdF (PSTATE) where
  compareF = $(structuralTypeOrd [t|PSTATE|] [])

instance OrdF (Location arm) where
  compareF = $(structuralTypeOrd [t|Location|]
               [(TypeApp (ConType [t|PSTATE|]) AnyType, [|compareF|])
               ])

type instance L.Location A32 = Location A32

instance A.IsLocation (Location A32) where
  readLocation _ = Nothing
  locationType p = case p of
    LocGPR _ -> knownRepr
    LocPC -> knownRepr
    LocPSTATE _ -> knownRepr
    LocMem -> knownRepr
  defaultLocationExpr sym p = let
    wRepr = regWidthRepr (Proxy :: Proxy A32)
    in case p of
    LocGPR _ -> WI.bvLit sym wRepr 0
    LocPC -> WI.bvLit sym wRepr 0
    LocPSTATE _ -> WI.bvLit sym (locationBitWidthRepr p) 0
    LocMem -> do
      zeroMem <- WI.bvLit sym (WT.knownNat @8) 0
      WI.constantArray sym (Ctx.singleton $ BaseBVRepr wRepr) zeroMem

  allLocations =
    A.registerizationLocations
    ++ map somePSTATEToSomeLoc allPSTATEs
    ++ [Some LocPC, Some LocMem]
  registerizationLocations = map (Some . LocGPR . fromIntegral) [(0 :: Int) .. 15]
  isMemoryLocation p = case p of
    LocMem -> True
    _ -> False

data Operand (tp :: Symbol)
  where
    Reg :: W.W 4 -> Operand "R"
    Imm :: CT.KnownSymbol s => !(Data.Word.Indexed.W (BitWidth s)) -> Operand s

deriving instance Show (Operand p)

instance ShowF (Operand) where
  showF p = show p

data Opcode (o :: Symbol -> *) (sh :: [Symbol])

deriving instance Eq (Opcode Operand p)
deriving instance Ord (Opcode Operand p)
deriving instance Show (Opcode Operand p)

instance A.IsOperand Operand where

type instance A.Operand A32 = Operand

type instance A.Opcode A32 = Opcode

instance A.IsOpcode Opcode where

instance ShowF (Opcode Operand) where
  showF _ = ""

instance EnumF (Opcode Operand) where
  enumF _ = 0
  congruentF p = Data.Set.NonEmpty.singleton p

instance TestEquality (Opcode Operand) where
  testEquality _ _ = Nothing

instance OrdF (Opcode Operand) where
  compareF p1 p2 = case testEquality p1 p2 of
    Just Refl -> EQF
    _ -> LTF

instance A.Architecture A32 where
  data TaggedExpr A32 sym s = TaggedExpr { unTaggedExpr :: A.AllocatedOperand A32 sym s }

  unTagged (TaggedExpr a) = case a of
    A.ValueOperand expr -> Just expr
    A.LocationOperand _ expr -> Just expr
    _ -> Nothing

  taggedOperand (TaggedExpr a) = a

  uninterpretedFunctions proxy = [] -- Needs to be generated statically by Dismantle

  allocateSymExprsForOperand _ sym f (operand :: Operand s) = case operand of
    Reg idx -> do
      let loc = LocGPR idx
      idxBV <- WI.bvLit sym (knownNat @4) (toInteger idx)
      registerBV <- f loc --  32 bits
      finalBV <- WI.bvConcat sym registerBV idxBV
      return $ TaggedExpr $ A.LocationOperand loc registerBV
    Imm w -> do
      let srepr = (CT.knownSymbol :: CT.SymbolRepr s)
      SymbolBitWidth nrepr <- return $ symbolToBitWidth' srepr
      expr <- withKnownNat nrepr (WI.bvLit sym nrepr (toInteger w))
      return $ TaggedExpr $ A.ValueOperand $ expr

  operandToLocation _ operand = case operand of
    Reg idx -> Just $ LocGPR idx
    _ -> Nothing

  locationFuncInterpretation _ = []

  shapeReprToTypeRepr _ (optype :: CT.SymbolRepr s) =
    case symbolToBitWidth' optype of SymbolBitWidth nr -> CT.BaseBVRepr nr