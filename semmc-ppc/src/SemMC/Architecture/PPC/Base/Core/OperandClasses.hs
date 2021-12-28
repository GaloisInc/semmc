{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module SemMC.Architecture.PPC.Base.Core.OperandClasses (
  SymToExprTagWrapper,
    -- * PPC Types
  gprc,
  gprc_nor0,
  fprc,
  vrrc,
  vsrc,
  crrc,
  crbitrc,
  crbitm,
  s5imm,
  s16imm,
  s16imm64,
  s17imm,
  u2imm,
  u4imm,
  u5imm,
  u6imm,
  u16imm,
  u16imm64,
  memrix,
  memri,
  memrr,
  directbrtarget,
  absdirectbrtarget,
  condbrtarget,
  calltarget,
  abscalltarget
  ) where

import           GHC.TypeLits ( Symbol )
import           Data.Kind ( Type )
import qualified Data.Type.List as TL

import           SemMC.DSL

-- PPC Types

gprc :: String
gprc = "Gprc"

gprc_nor0 :: String
gprc_nor0 = "Gprc_nor0"

fprc :: String
fprc = "Fprc"

vrrc :: String
vrrc = "Vrrc"

vsrc :: String
vsrc = "Vsrc"

absdirectbrtarget :: String
absdirectbrtarget = "Absdirectbrtarget"

directbrtarget :: String
directbrtarget = "Directbrtarget"

abscalltarget :: String
abscalltarget = "Abscalltarget"

calltarget :: String
calltarget = "Calltarget"

condbrtarget :: String
condbrtarget = "Condbrtarget"

crrc :: String
crrc = "Crrc"

crbitrc :: String
crbitrc = "Crbitrc"

crbitm :: String
crbitm = "Crbitm"

s5imm :: String
s5imm = "S5imm"

s16imm :: String
s16imm = "S16imm"

s17imm :: String
s17imm = "S17imm"

s16imm64 :: String
s16imm64 = "S16imm64"

u2imm :: String
u2imm = "U2imm"

u4imm :: String
u4imm = "U4imm"

u5imm :: String
u5imm = "U5imm"

u6imm ::  String
u6imm = "U6imm"

u16imm :: String
u16imm = "U16imm"

u16imm64 :: String
u16imm64 = "U16imm64"

memrix :: String
memrix = "Memrix"

memri :: String
memri = "Memri"

memrr :: String
memrr = "Memrr"

data SymToExprTagWrapper :: TL.TyFun k1 k2 -> Type
type instance TL.Apply SymToExprTagWrapper x = SymToExprTag x
type family SymToExprTag (sym :: Symbol) :: ExprTag where
  SymToExprTag "Gprc" = 'TBV
  SymToExprTag "S16imm" = 'TBV
  SymToExprTag "U16imm" = 'TBV
  SymToExprTag "Vrrc" = 'TBV
