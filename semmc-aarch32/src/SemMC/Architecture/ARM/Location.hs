{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture.ARM.Location
    ( Location(..)
    , ArchRepr(..)
    , locMem
    , locPC
    , locRepr
    )
    where

import           Data.Kind ( Type )
import qualified Data.Text as T
import qualified Data.Parameterized.SymbolRepr as SR
import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import qualified SemMC.Architecture as A
import           What4.BaseTypes
import qualified What4.Interface as WI

import qualified Language.ASL.Globals as ASL

import           Prelude

class ArchRepr arch where
  regWidthRepr :: proxy arch -> NatRepr (A.RegWidth arch)

-- ----------------------------------------------------------------------

-- | A location is simply an index int o the globals
data Location arm :: BaseType -> Type where
  Location :: ASL.GlobalRef s -> Location arm (ASL.GlobalsType s)

locPC :: Location arm (BaseBVType 32)
locPC = Location $ ASL.knownGlobalRef @"_PC"

locMem :: Location arm ASL.MemoryBaseType
locMem = Location $ ASL.knownGlobalRef @"__Memory"

locRepr :: Location arm tp -> WI.BaseTypeRepr tp
locRepr (Location gr) = ASL.globalRefRepr gr

instance Show (Location arm tp) where
  show (Location gr) = T.unpack $ SR.symbolRepr $ ASL.globalRefSymbol gr

instance ShowF (Location arm) 

instance TestEquality (Location arm) where
  testEquality (Location gr1) (Location gr2) = do
    Refl <- testEquality gr1 gr2
    return Refl

instance OrdF (Location arm) where
  compareF (Location gr1) (Location gr2) = case compareF gr1 gr2 of
    LTF -> LTF
    GTF -> GTF
    EQF -> EQF
