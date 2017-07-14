{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module TestPPC where

import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import qualified Data.Set as Set

import Lang.Crucible.BaseTypes
import Lang.Crucible.Solver.SimpleBackend
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.Interface as S

import qualified Dismantle.PPC as PPC

import SemMC.Architecture
import SemMC.Architecture.PPC ( PPC )
import qualified SemMC.Architecture.PPC as PPC
import SemMC.Formula
import SemMC.Formula.Parser
import SemMC.Synthesis.Template
import SemMC.Synthesis
import SemMC.Util

type MemoryType n = BaseArrayType (Ctx.SingleCtx (BaseBVType n)) (BaseBVType 8)

moveFive :: S.SimpleBuilder t st -> PPC.Location (BaseBVType 32) -> IO (Formula (S.SimpleBuilder t st) PPC)
moveFive sym loc = do
  five <- S.bvLit sym knownNat 5
  -- locVar <- S.freshBoundVar sym (makeSymbol (show loc)) (locationType loc)
  return $ Formula { formUses = Set.empty
                   , formParamVars = MapF.empty
                   , formDefs = MapF.insert loc five
                              $ MapF.empty
                   }

memFormula :: S.SimpleBuilder t st -> PPC.Location (BaseBVType 32) -> IO (Formula (S.SimpleBuilder t st) PPC)
memFormula sym loc = do
  locVar <- S.freshBoundVar sym (makeSymbol (show loc)) (locationType loc)
  memVar <- S.freshBoundVar sym (makeSymbol (show PPC.LocMem)) (locationType PPC.LocMem)
  lowBits <- S.bvSelect sym (knownNat @0) (knownNat @8) (S.varExpr sym locVar)
  fortytwo <- S.bvLit sym knownNat 42
  addr <- S.bvAdd sym (S.varExpr sym locVar) fortytwo
  newMem <- S.arrayUpdate sym (S.varExpr sym memVar) (Ctx.empty Ctx.%> addr) lowBits
  return $ Formula { formUses = Set.fromList [ Some loc, Some PPC.LocMem ]
                   , formParamVars = MapF.insert loc locVar
                                   $ MapF.insert PPC.LocMem memVar
                                   $ MapF.empty
                   , formDefs = MapF.insert PPC.LocMem newMem
                              $ MapF.empty
                   }

doThing :: IO ()
doThing = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  baseSet <- PPC.loadBaseSet sym
  foo <- memFormula sym (PPC.LocGPR (PPC.GPR 5))
  print foo
  print =<< mcSynth sym baseSet foo
  -- moveFiveIntoGPR2 <- moveFive sym (PPC.LocGPR (PPC.GPR 2))
  -- print . take 5 =<< templatedInstructions sym baseSet
  -- print =<< mcSynth sym baseSet moveFiveIntoGPR2
