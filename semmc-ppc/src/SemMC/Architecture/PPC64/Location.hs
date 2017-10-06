{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Architecture.PPC64.Location (
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
import           Text.PrettyPrint.HughesPJClass ( pPrint )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.PPC as PPC

-- import qualified SemMC.Architecture as A

-- import qualified SemMC.Architecture.PPC.Shared as PPCS
