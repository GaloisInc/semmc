{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module SemMC.Fuzzer.Filters
( InstFilter
, InstState
, rejectionSampleMaybe
, FilterParser(..)
) where

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.View as A
import           SemMC.Architecture.Value
import qualified Data.Word.Indexed as W

import qualified SemMC.Architecture.A32 as A32
import qualified SemMC.Architecture.A32.Location as A32Loc
import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC64 as PPC64

import qualified Dismantle.ARM as ARMDis
import qualified Dismantle.ARM.Operands as ARMDisOprs
import qualified Data.Parameterized.List as L
import qualified Dismantle.Arbitrary as DA
import Dismantle.Instruction
import qualified Data.Parameterized.Map as MapF
import Data.Word(Word32)
import Text.Read(readMaybe)

type InstState arch = (GenericInstruction (A.Opcode arch) (A.Operand arch), A.ConcreteState arch)
type InstFilter arch = InstState arch -> Maybe (InstState arch)

rejectionSampleMaybe :: DA.Gen -> (a -> Maybe b) -> (DA.Gen -> IO a) -> IO b
rejectionSampleMaybe gen f c = do
  a <- f <$> c gen
  case a of
    Nothing -> rejectionSampleMaybe gen f c
    Just b  -> return b

class FilterParser arch where
    readFilter :: proxy arch -> String -> Maybe (InstFilter arch)

instance FilterParser A32.A32 where
    readFilter _ = a32parseInstFilter

instance FilterParser PPC32.PPC where
    readFilter _ = const Nothing

instance FilterParser PPC64.PPC where
    readFilter _ = const Nothing

a32parseInstFilter :: String -> Maybe (InstFilter A32.A32)
a32parseInstFilter s = a32buildFilter <$> readMaybe s

data A32Filter =
    A32ProhibitGPRs [Word32]
    | A32SetCPSR Word32
    deriving(Read, Show)

a32buildFilter :: A32Filter -> InstFilter A32.A32
a32buildFilter = \case
    A32ProhibitGPRs excluded -> \(inst, rst) ->
        if a32RejectGpr excluded inst
            then Just (inst, rst)
            else Nothing
    A32SetCPSR cpsr -> \(inst, rst) -> Just $ (inst, a32SetCPSR cpsr rst)

a32SetCPSR :: Word32 -> A.ConcreteState A32.A32 -> A.ConcreteState A32.A32
a32SetCPSR cpsr st = MapF.insert A32Loc.LocCPSR newCpsr st
    where newCpsr = ValueBV (W.w (toInteger cpsr))

a32RejectGpr :: [Word32] -> GenericInstruction (A.Opcode A32.A32) (A.Operand A32.A32) -> Bool
a32RejectGpr lst (Instruction _ ops) = go ops
    where
        isSpecial n = any (==n) (ARMDisOprs.gpr <$> lst)
        go :: L.List (A.Operand A32.A32) a -> Bool
        go os = case os of
            L.Nil                             -> True
            o L.:< os' -> case o of
                ARMDis.GPR gpr | isSpecial gpr -> False
                _                              -> go os'