{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC.Location (
  Location(..),
  parseLocation
  ) where

import qualified Data.Parameterized.Ctx as Ctx
import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Data.Parameterized.Some
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import           Text.PrettyPrint.HughesPJClass ( pPrint )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture as A

data Location :: BaseType -> * where
  LocGPR :: PPC.GPR -> Location (BaseBVType 32)
  LocIP :: Location (BaseBVType 32)
  LocMSR :: Location (BaseBVType 32)
  LocCTR :: Location (BaseBVType 32)
  LocLNK :: Location (BaseBVType 32)
  LocXER :: Location (BaseBVType 32)
  LocCR :: Location (BaseBVType 32)
  LocVSR :: PPC.VSReg -> Location (BaseBVType 128)
  LocFPSCR :: Location (BaseBVType 64)
  LocMem :: Location (BaseArrayType (Ctx.SingleCtx (BaseBVType 32)) (BaseBVType 8))

instance Show (Location tp) where
  show (LocGPR gpr) = show (pPrint gpr)
  show LocIP = "IP"
  show LocMSR = "MSR"
  show LocCTR = "CTR"
  show LocLNK = "LNK"
  show LocXER = "XER"
  show LocCR = "CR"
  show (LocVSR vsr) = show (pPrint vsr)
  show LocFPSCR = "FPSCR"
  show LocMem = "Mem"
instance ShowF Location

$(return [])

fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
fakeTestEq x y = if x == y
                 then Just Refl
                 else Nothing

instance TestEquality Location where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [ (ConType [t|PPC.GPR|], [|fakeTestEq|])
                   , (ConType [t|PPC.VSReg|], [|fakeTestEq|])
                   ]
                  )

fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
fakeCompareF x y = fromOrdering (compare x y)

instance OrdF Location where
  compareF = $(structuralTypeOrd [t|Location|]
               [ (ConType [t|PPC.GPR|], [|fakeCompareF|])
               , (ConType [t|PPC.VSReg|], [|fakeCompareF|])
               ]
              )

instance A.IsLocation Location where
  readLocation = P.parseMaybe parseLocation

  locationType (LocGPR _) = knownRepr
  locationType LocIP = knownRepr
  locationType LocMSR = knownRepr
  locationType LocCTR = knownRepr
  locationType LocLNK = knownRepr
  locationType LocXER = knownRepr
  locationType LocCR = knownRepr
  locationType (LocVSR _) = knownRepr
  locationType LocFPSCR = knownRepr
  locationType LocMem = knownRepr

  defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocIP = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMSR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCTR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocLNK = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocXER = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym (LocVSR _) = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocFPSCR = S.bvLit sym knownNat 0
  defaultLocationExpr sym LocMem =
    S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0

  allLocations = concat
    [ map (Some . LocGPR . PPC.GPR) [0..31]
    , map (Some . LocVSR . PPC.VSReg) [0..63]
    , [ Some LocIP
      , Some LocMSR
      , Some LocCTR
      , Some LocLNK
      , Some LocXER
      , Some LocCR
      , Some LocFPSCR
      , Some LocMem
      ]
    ]

type Parser = P.Parsec String String

tryOne :: [Parser a] -> Parser a
tryOne = P.choice . map P.try

parseLocation :: Parser (Some Location)
parseLocation = do
  c <- P.lookAhead (P.anyChar)
  case c of
    'I' -> Some LocIP <$ P.string "IP"
    'X' -> Some LocXER <$ P.string "XER"
    'L' -> Some LocLNK <$ P.string "LNK"
    'r' -> parsePrefixedRegister (Some . LocGPR . PPC.GPR) 'r'
    'x' -> parsePrefixedRegister (Some . LocVSR . PPC.VSReg) 'x'
    'C' -> tryOne [ Some LocCTR <$ P.string "CTR"
                  , Some LocCR <$ P.string "CR"
                  ]
    'M' -> tryOne [ Some LocMSR <$ P.string "MSR"
                  , Some LocMem <$ P.string "Mem"
                  ]
    'F' -> Some LocFPSCR <$ P.string "FPSCR"
    _ -> fail ("Unexpected location prefix character: " ++ (c :[]))

parsePrefixedRegister :: (Integral a, Show a) => (a -> b) -> Char -> Parser b
parsePrefixedRegister f c = do
  _ <- P.char c
  n <- P.decimal
  case n >= 0 && n <= 31 of
    True -> return (f n)
    False -> fail ("Register number out of range: " ++ show n)
