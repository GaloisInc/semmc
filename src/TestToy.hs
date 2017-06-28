{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module TestToy where

import qualified Data.Set as Set
import Data.Monoid
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Classes
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some

import Lang.Crucible.Solver.SimpleBackend
import qualified Lang.Crucible.Solver.Interface as S

import SemMC.Architecture
import SemMC.Formula
import SemMC.Formula.Parser
import SemMC.ToyExample
import SemMC.Synthesis.Template
import SemMC.Synthesis.Cegis
import SemMC.Synthesis
import SemMC.Util

readBinOp :: forall t. SimpleBackend t -> FilePath -> IO (Either String (ParameterizedFormula (SimpleBackend t) (TemplatedArch Toy) '["R32", "R32"]))
readBinOp sym fp = readFormulaFromFile sym ("data/toy/base/" <> fp)

readBinOp' :: forall t. SimpleBackend t -> FilePath -> IO (Either String (ParameterizedFormula (SimpleBackend t) (TemplatedArch Toy) '["R32", "I32"]))
readBinOp' sym fp = readFormulaFromFile sym ("data/toy/base/" <> fp)

doThing :: IO ()
doThing = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOp sym "AddRr.sem"
  Right sub <- readBinOp sym "SubRr.sem"
  Right movi <- readBinOp' sym "MovRi.sem"
  let opcodes = MapF.insert (Witness AddRr) add
              $ MapF.insert (Witness SubRr) sub
              $ MapF.insert (Witness MovRi) movi
              $ MapF.empty
  -- print =<< instantiateFormula sym pf (R32 Reg1 :> R32 Reg3 :> Nil)
  templated <- templatedInstructions sym opcodes
  print $ templated !! 50
  -- (f1 : f2 : _) <- templatizeFormula' sym pf
  -- print =<< sequenceFormulas sym (tfFormula f1) (tfFormula f2)

-- Formula for moving 2*r1 into r2. The assembly I have in mind:
--
-- > SubRr r2, r2
-- > AddRr r2, r1
-- > AddRr r2, r1
--
fooFormula :: (ShowF (S.SymExpr sym)) => (S.IsSymInterface sym, S.IsExprBuilder sym) => sym -> IO (Formula sym Toy)
fooFormula sym = do
  reg1 <- S.freshBoundVar sym (makeSymbol (show Reg1)) (locationType (RegLoc Reg1))
  twoLit <- S.bvLit sym (knownNat :: NatRepr 32) 2
  reg1TimesTwo <- S.bvMul sym twoLit (S.varExpr sym reg1)
  reg2Def <- S.bvAdd sym reg1TimesTwo twoLit
  return $ Formula { formUses = Set.singleton (Some (RegLoc Reg1)) -- Do we really need uses? Should just be the keys of vars.
                   , formParamVars = MapF.insert (RegLoc Reg1) reg1
                                   $ MapF.empty
                   , formDefs = MapF.insert (RegLoc Reg2) reg2Def
                              $ MapF.empty
                   }

doThing2 :: IO ()
doThing2 = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right add <- readBinOp sym "AddRr.sem"
  Right sub <- readBinOp sym "SubRr.sem"
  Right movi <- readBinOp' sym "MovRi.sem"
  let opcodes = MapF.insert (Witness AddRr) add
              $ MapF.insert (Witness SubRr) sub
              $ MapF.insert (Witness MovRi) movi
              $ MapF.empty
  target <- fooFormula sym

  zero <- S.bvLit sym (knownNat :: NatRepr 32) 0
  -- one <- S.bvLit sym (knownNat :: NatRepr 32) 1
  -- ten <- S.bvLit sym (knownNat :: NatRepr 32) 10

  let testInputs = [ MapF.insert (RegLoc Reg1) zero MapF.empty
                   -- , MapF.insert Reg1 one MapF.empty
                   -- , MapF.insert Reg1 ten MapF.empty
                   ]
  tests <- mapM (\input -> (input,) <$> evalFormula sym target input) testInputs
  print =<< synthesizeFormula sym opcodes target tests
