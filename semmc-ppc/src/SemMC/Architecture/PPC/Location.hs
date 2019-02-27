{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module SemMC.Architecture.PPC.Location (
  Location(..),
--  ArchRegWidth,
  ArchRepr(..)
--  parseLocation
  ) where

import qualified Data.Parameterized.Ctx as Ctx
import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Text.PrettyPrint.HughesPJClass ( pPrint )

import           What4.BaseTypes

import qualified Dismantle.PPC as PPC
import qualified SemMC.Architecture as A

class ArchRepr arch where
  regWidthRepr :: proxy arch -> NatRepr (A.RegWidth arch)

data Location ppc :: BaseType -> * where
  -- NOTE: If you add new constructors here, you also need to update:
  --   * the PPC state serializer / deserializer functions
  --   * the remote-runner.c program's PPC32 state struct definition
  LocGPR :: PPC.GPR -> Location ppc (BaseBVType (A.RegWidth ppc))
  LocIP :: Location ppc (BaseBVType (A.RegWidth ppc))
  LocMSR :: Location ppc (BaseBVType 32)
  LocCTR :: Location ppc (BaseBVType (A.RegWidth ppc))
  LocLNK :: Location ppc (BaseBVType (A.RegWidth ppc))
  LocXER :: Location ppc (BaseBVType (A.RegWidth ppc))
  LocCR :: Location ppc (BaseBVType 32)
  LocVSR :: PPC.VSReg -> Location ppc (BaseBVType 128)
  LocFPSCR :: Location ppc (BaseBVType 32)
  LocVSCR :: Location ppc (BaseBVType 32)
  LocMem :: Location ppc (BaseArrayType (Ctx.SingleCtx (BaseBVType (A.RegWidth ppc))) (BaseBVType 8))

instance Show (Location ppc tp) where
  show (LocGPR gpr) = show (pPrint gpr)
  show LocIP = "IP"
  show LocMSR = "MSR"
  show LocCTR = "CTR"
  show LocLNK = "LNK"
  show LocXER = "XER"
  show LocCR = "CR"
  show (LocVSR vsr) = show (pPrint vsr)
  show LocFPSCR = "FPSCR"
  show LocVSCR = "VSCR"
  show LocMem = "Mem"

instance ShowF (Location ppc)


$(return [])

fakeTestEq :: (Eq a) => a -> a -> Maybe (a :~: a)
fakeTestEq x y = if x == y
                 then Just Refl
                 else Nothing

instance TestEquality (Location ppc) where
  testEquality = $(structuralTypeEquality [t|Location|]
                   [ (ConType [t|PPC.GPR|], [|fakeTestEq|])
                   , (ConType [t|PPC.VSReg|], [|fakeTestEq|])
                   ]
                  )

fakeCompareF :: (Ord a) => a -> a -> OrderingF a a
fakeCompareF x y = fromOrdering (compare x y)

instance OrdF (Location ppc) where
  compareF = $(structuralTypeOrd [t|Location|]
               [ (ConType [t|PPC.GPR|], [|fakeCompareF|])
               , (ConType [t|PPC.VSReg|], [|fakeCompareF|])
               ]
              )

-- instance A.IsLocation (Location ppc) where
--   readLocation = P.parseMaybe parseLocation

--   locationType (LocGPR _) = knownRepr
--   locationType LocIP = knownRepr
--   locationType LocMSR = knownRepr
--   locationType LocCTR = knownRepr
--   locationType LocLNK = knownRepr
--   locationType LocXER = knownRepr
--   locationType LocCR = knownRepr
--   locationType (LocVSR _) = knownRepr
--   locationType LocFPSCR = knownRepr
--   locationType LocMem = knownRepr

--   defaultLocationExpr sym (LocGPR _) = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocIP = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocMSR = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocCTR = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocLNK = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocXER = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocCR = S.bvLit sym knownNat 0
--   defaultLocationExpr sym (LocVSR _) = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocFPSCR = S.bvLit sym knownNat 0
--   defaultLocationExpr sym LocMem =
--     S.constantArray sym knownRepr =<< S.bvLit sym knownNat 0

--   allLocations = concat
--     [ map (Some . LocGPR . PPC.GPR) [0..31]
--     , map (Some . LocVSR . PPC.VSReg) [0..63]
--     , [ Some LocIP
--       , Some LocMSR
--       , Some LocCTR
--       , Some LocLNK
--       , Some LocXER
--       , Some LocCR
--       , Some LocFPSCR
--       , Some LocMem
--       ]
--     ]

--   registerizationLocations = map (Some . LocGPR . PPC.GPR) (0 : [3..10])

-- parseLocation :: PPCS.Parser (Some (Location ppc))
-- parseLocation = do
--   c <- P.lookAhead (P.anyChar)
--   case c of
--     'I' -> Some LocIP <$ P.string "IP"
--     'X' -> Some LocXER <$ P.string "XER"
--     'L' -> Some LocLNK <$ P.string "LNK"
--     'r' -> PPCS.parsePrefixedRegister (Some . LocGPR . PPC.GPR) 'r'
--     'x' -> PPCS.parsePrefixedRegister (Some . LocVSR . PPC.VSReg) 'x'
--     'C' -> PPCS.tryOne [ Some LocCTR <$ P.string "CTR"
--                        , Some LocCR <$ P.string "CR"
--                        ]
--     'M' -> PPCS.tryOne [ Some LocMSR <$ P.string "MSR"
--                        , Some LocMem <$ P.string "Mem"
--                        ]
--     'F' -> Some LocFPSCR <$ P.string "FPSCR"
--     _ -> fail ("Unexpected location prefix character: " ++ (c :[]))

