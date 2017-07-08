{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module TestPPC where

import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import qualified Data.Set as Set

import Lang.Crucible.BaseTypes
import Lang.Crucible.Solver.SimpleBackend
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.PPC as DPPC

import SemMC.Architecture
import SemMC.Architecture.PPC ( PPC )
import qualified SemMC.Architecture.PPC as PPC
import SemMC.Formula
import SemMC.Formula.Parser
import SemMC.Synthesis.Template
import SemMC.Synthesis
import SemMC.Util

moveFive :: S.SimpleBuilder t st -> PPC.Location (BaseBVType 32) -> IO (Formula (S.SimpleBuilder t st) PPC)
moveFive sym loc = do
  five <- S.bvLit sym knownNat 5
  -- locVar <- S.freshBoundVar sym (makeSymbol (show loc)) (locationType loc)
  return $ Formula { formUses = Set.empty
                   , formParamVars = MapF.empty
                   , formDefs = MapF.insert loc five
                              $ MapF.empty
                   }

doThing :: IO ()
doThing = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  baseSet <- PPC.loadBaseSet sym
  moveFiveIntoGPR2 <- moveFive sym (PPC.GPR (DPPC.GPR 2))
  -- print . take 5 =<< templatedInstructions sym baseSet
  print =<< mcSynth sym baseSet moveFiveIntoGPR2
