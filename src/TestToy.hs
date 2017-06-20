{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module TestToy where

import Data.Parameterized.Nonce
import Data.Parameterized.Some
import Lang.Crucible.Solver.SimpleBackend

import Dismantle.Instruction ( OperandList(..) )

import SemMC.Formula
import SemMC.Formula.Parser
import SemMC.ToyExample
import SemMC.Instantiate

readBinOp :: forall t. SimpleBackend t -> IO (Either String (ParameterizedFormula (SimpleBackend t) Toy '["R32", "R32"]))
readBinOp sym = readFormulaFromFile sym "data/toy/base/AddRr.sem"

doThing :: IO ()
doThing = do
  Some r <- newIONonceGenerator
  sym <- newSimpleBackend r
  Right pf <- readBinOp sym
  print =<< instantiateFormula sym pf (R32 Reg1 :> R32 Reg3 :> Nil)
