{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Set as Set
import           Data.Type.Equality
import           GHC.TypeNats as TypeNats
import           LocationsTests
import           ParameterTests
import           ParamFormulaTests
import           Numeric.Natural
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F
import qualified SemMC.Formula.Env as FE
import           Test.Tasty
import           TestArch
import           TestArchPropGen
import           What4.BaseTypes
import qualified What4.Expr.Builder as SB

import           Prelude

main :: IO ()
main = defaultMain $ testGroup "Storable Semantics" $
       locationTests <> parameterTests <> parameterizedFormulaTests
