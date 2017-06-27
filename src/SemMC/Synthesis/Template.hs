{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module SemMC.Synthesis.Template
  ( TemplatedArch
  , TemplatedFormula
  , OpcodeGoodShape(..)
  , InstructionWTFormula(..)
  , tfOperandList
  , tfOperandExprs
  , tfFormula
  , templatizeFormula
  , templatizeFormula'
  , templatedInstructions
  , recoverOperands
  , unTemplate
  ) where

import           Control.Monad ( join )
import           Data.EnumF
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Proxy ( Proxy(..) )
import qualified Data.ShowF as SF
import           Data.Typeable
import           GHC.TypeLits ( KnownSymbol, sameSymbol, Symbol, symbolVal )
import           Unsafe.Coerce ( unsafeCoerce )

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBackend as S
import           Lang.Crucible.Solver.SimpleBackend.GroundEval
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import           Lang.Crucible.Solver.Symbol ( userSymbol, SolverSymbol )

import           Dismantle.Instruction ( OperandList(..) )

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Formula.Instantiate

-- type family IsReg (arch :: *) (operand :: Symbol) :: Bool

data TemplatedOperand (arch :: *) (s :: Symbol) where
  Concrete :: Operand arch s -> TemplatedOperand arch s
  Abstract :: (KnownSymbol s) => BaseTypeRepr (OperandType arch s) -> TemplatedOperand arch s
-- deriving instance (Show (Operand arch s)) => Show (TemplatedOperand arch s)
-- deriving instance (Eq (Operand arch s)) => Eq (TemplatedOperand arch s)
-- deriving instance (Ord (Operand arch s)) => Ord (TemplatedOperand arch s)

instance (ShowF (Operand arch)) => ShowF (TemplatedOperand arch) where
  showF (Concrete op) = "Concrete (" ++ showF op ++ ")"
  showF (Abstract _) = "Abstract"

instance (ShowF (Operand arch)) => SF.ShowF (TemplatedOperand arch) where
  showF = showF

instance (TestEquality (Operand arch)) => TestEquality (TemplatedOperand arch) where
  Concrete op1 `testEquality` Concrete op2 = (\Refl -> Refl) <$> op1 `testEquality` op2
  (Abstract _ :: TemplatedOperand arch s1) `testEquality` (Abstract _ :: TemplatedOperand arch s2) =
    (\Refl -> Refl) <$> (Proxy :: Proxy s1) `sameSymbol` (Proxy :: Proxy s2)
  _            `testEquality` _            = Nothing

instance (IsOperand (Operand arch)) => IsOperand (TemplatedOperand arch) where
  -- allO = undefined

-- newtype TemplatedArch (arch :: *) = TemplatedArch { unTemplatedArch :: arch }
data TemplatedArch (arch :: *)

type instance Operand (TemplatedArch arch) = TemplatedOperand arch
type instance Opcode (TemplatedArch arch) = Opcode arch
type instance OperandType (TemplatedArch arch) s = OperandType arch s
type instance Location (TemplatedArch arch) = Location arch

makeSymbol :: String -> SolverSymbol
makeSymbol name = case userSymbol name of
                    Right symbol -> symbol
                    Left _ -> error "tried to create symbol with bad name"

instance (OrdF (Opcode arch (TemplatedOperand arch)),
          SF.ShowF (Opcode arch (TemplatedOperand arch)),
          EnumF (Opcode arch (TemplatedOperand arch)),
          Architecture arch,
          Typeable arch)
       => Architecture (TemplatedArch arch) where
  operandValue _ sym newVars (Concrete op) = operandValue (Proxy :: Proxy arch) sym newVars op
  operandValue _ sym _ (Abstract tpRepr :: TemplatedOperand arch s) =
    S.freshConstant sym (makeSymbol (symbolVal (Proxy :: Proxy s))) tpRepr

  operandToLocation _ (Concrete op) = operandToLocation (Proxy :: Proxy arch) op
  operandToLocation _ (Abstract _)  = Nothing

  -- valueToOperand?

-- TODO: need to store the variables used for the immediate spots.
data TemplatedFormula sym arch sh =
  TemplatedFormula { tfOperandList :: OperandList (TemplatedOperand arch) sh
                   , tfOperandExprs :: OperandList (WrappedExpr sym (TemplatedArch arch)) sh
                   , tfFormula :: Formula sym (TemplatedArch arch)
                   }
deriving instance (SF.ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => Show (TemplatedFormula sym arch sh)

instance (SF.ShowF (TemplatedOperand arch), ShowF (S.SymExpr sym), ShowF (S.BoundVar sym), ShowF (Location arch)) => ShowF (TemplatedFormula sym arch) where
  showF = show

-- class MakeTemplatedOpLists' (isreg :: Bool) arch sh where
--   makeTemplatedOpLists' :: Proxy isreg -> [OperandList (TemplatedOperand arch) sh]

-- instance MakeTemplatedOpLists' isreg arch '[] where
--   makeTemplatedOpLists' _ = [Nil]

-- instance forall arch s sh (next :: Bool). (Architecture arch, IsReg arch s ~ True, KnownSymbol s, MakeTemplatedOpLists' next arch sh
--                                           ) => MakeTemplatedOpLists' True arch (s ': sh) where
--   makeTemplatedOpLists' _ = []
--   -- makeTemplatedOpLists' _ =
--   --   makeTemplatedOpLists' (Proxy :: Proxy next) >>= \operands ->
--   --     fmap (\op -> Concrete op :> operands) (allPossible (Proxy :: Proxy (Operand arch s)))

-- instance (Architecture arch, KnownRepr BaseTypeRepr (OperandType arch s)-- , MakeTemplatedOpLists' next arch sh
--          ) => MakeTemplatedOpLists' False arch (s ': sh) where
--   makeTemplatedOpLists' _ = []
--   -- makeTemplatedOpLists' _ = (Abstract (knownRepr :: BaseTypeRepr (OperandType arch s)) :>) <$> makeTemplatedOpLists' (Proxy :: Proxy next)

-- class MakeTemplatedOpLists arch sh where
--   makeTemplatedOpLists :: [OperandList (TemplatedOperand arch) sh]

-- -- The False here in the constraint is arbitrary.
-- instance (MakeTemplatedOpLists' False arch '[]) => MakeTemplatedOpLists arch '[] where
--   makeTemplatedOpLists = makeTemplatedOpLists' (Proxy :: Proxy False)

-- instance (MakeTemplatedOpLists' (IsReg arch s) arch (s ': sh)) => MakeTemplatedOpLists arch (s ': sh) where
--   makeTemplatedOpLists = makeTemplatedOpLists' (Proxy :: Proxy (IsReg arch s))

class KnownBool (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance KnownBool 'False where
  boolVal _ = False

instance KnownBool 'True where
  boolVal _ = True

class MakeTemplatedOpLists arch sh where
  makeTemplatedOpLists :: [OperandList (TemplatedOperand arch) sh]

instance MakeTemplatedOpLists arch '[] where
  makeTemplatedOpLists = [Nil]

instance (Architecture arch, KnownSymbol s, IsSpecificOperand (Operand arch) s, KnownBool (IsReg arch s), KnownRepr BaseTypeRepr (OperandType arch s), MakeTemplatedOpLists arch sh) => MakeTemplatedOpLists arch (s ': sh) where
  makeTemplatedOpLists =
    -- We're in the List monad.
    makeTemplatedOpLists >>= \opList ->
      case boolVal (Proxy :: Proxy (IsReg arch s)) of
        True -> map (\op -> Concrete op :> opList) allOperandValues
        False -> return $ Abstract (knownRepr :: BaseTypeRepr (OperandType arch s)) :> opList

unTemplate :: ParameterizedFormula sym (TemplatedArch arch) sh
           -> ParameterizedFormula sym arch sh
unTemplate = unsafeCoerce

-- TODO: Can we get rid of these constraints in some nice way?
templatizeFormula :: forall t st arch sh.
                     (Architecture arch,
                      OrdF (Opcode arch (TemplatedOperand arch)),
                      SF.ShowF (Opcode arch (TemplatedOperand arch)),
                      EnumF (Opcode arch (TemplatedOperand arch)),
                      Typeable arch,
                      MakeTemplatedOpLists arch sh)
                  => S.SimpleBuilder t st
                  -> ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch) sh
                  -> [IO (TemplatedFormula (S.SimpleBuilder t st) arch sh)]
templatizeFormula sym pf = map (\ol -> uncurry (TemplatedFormula ol) <$> instantiateFormula sym pf ol) makeTemplatedOpLists

templatizeFormula' :: forall t st arch sh.
                      (Architecture arch,
                       OrdF (Opcode arch (TemplatedOperand arch)),
                       SF.ShowF (Opcode arch (TemplatedOperand arch)),
                       EnumF (Opcode arch (TemplatedOperand arch)),
                       Typeable arch,
                       MakeTemplatedOpLists arch sh)
                   => S.SimpleBuilder t st
                   -> ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch) sh
                   -> IO [TemplatedFormula (S.SimpleBuilder t st) arch sh]
templatizeFormula' sym = sequence . templatizeFormula sym

data OpcodeGoodShape (opcode :: (Symbol -> *) -> [Symbol] -> *) (operand :: Symbol -> *) (arch :: *) (sh :: [Symbol]) where
  OpcodeGoodShape :: (RecoverOperands arch sh, MakeTemplatedOpLists arch sh) => opcode operand sh -> OpcodeGoodShape opcode operand arch sh

instance (TestEquality (opcode operand)) => TestEquality (OpcodeGoodShape opcode operand arch) where
  OpcodeGoodShape op1 `testEquality` OpcodeGoodShape op2 =
    (\Refl -> Refl) <$> op1 `testEquality` op2

instance (OrdF (opcode operand)) => OrdF (OpcodeGoodShape opcode operand arch) where
  OpcodeGoodShape op1 `compareF` OpcodeGoodShape op2 =
    case op1 `compareF` op2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

class RecoverOperands (arch :: *) (sh :: [Symbol]) where
  recoverOperands :: GroundEvalFn t
                  -> OperandList (TemplatedOperand arch) sh
                  -> OperandList (WrappedExpr (S.SimpleBackend t) (TemplatedArch arch)) sh
                  -> IO (OperandList (Operand arch) sh)

instance RecoverOperands arch '[] where
  recoverOperands _ Nil Nil = return Nil

instance (Architecture arch, RecoverOperands arch sh)
  => RecoverOperands arch (s ': sh) where
  -- => RecoverOperands arch (Cons s sh) where
  recoverOperands evalFn (Concrete op :> restOps) (_ :> restExprs) =
    (op :>) <$> recoverOperands evalFn restOps restExprs
  recoverOperands evalFn (Abstract _ :> restOps) (WrappedExpr expr :> restExprs) =
    (:>) <$> (valueToOperand (Proxy :: Proxy arch) <$> evalGroundElt (groundEval evalFn) expr)
         <*> recoverOperands evalFn restOps restExprs

data InstructionWTFormula sym arch where
  InstructionWTFormula :: forall sym arch sh.
                          (MakeTemplatedOpLists arch sh, RecoverOperands arch sh)
                       => (Opcode arch) (Operand arch) sh
                       -> TemplatedFormula sym arch sh
                       -> InstructionWTFormula sym arch

instance (ShowF ((Opcode arch) (Operand arch)), ShowF (TemplatedFormula sym arch)) => Show (InstructionWTFormula sym arch) where
  -- show (InstructionWTFormula op tf) = "InstructionWTFormula " ++ showF op ++ " (" ++ showF tf ++ ")"
  show (InstructionWTFormula op tf) = "InstructionWTFormula " ++ showF op ++ " (" ++ showF tf ++ ")"

-- XXX: ...what should I name this?
foo :: forall a. [a] -> [[a]]
foo xs = join [foo' i | i <- [1..]]
  where foo' :: Int -> [[a]]
        foo' 1 = map pure xs
        foo' n = [x : xs' | x <- xs, xs' <- foo' (n-1)]

-- This is an infinite stream, thus why the IO is on the *inside* of the list.
templatedInstructions :: forall t st arch.
                         (Architecture arch,
                          OrdF (Opcode arch (TemplatedOperand arch)),
                          SF.ShowF (Opcode arch (TemplatedOperand arch)),
                          EnumF (Opcode arch (TemplatedOperand arch)),
                          Typeable arch)
                      => S.SimpleBuilder t st
                      -- -> BaseSet (S.SimpleBuilder t st) (TemplatedArch arch)
                      -> MapF.MapF (OpcodeGoodShape (Opcode arch) (Operand arch) arch) (ParameterizedFormula (S.SimpleBuilder t st) (TemplatedArch arch))
                      -> IO [[InstructionWTFormula (S.SimpleBuilder t st) arch]]
templatedInstructions sym m = foo . join <$> mapM f (MapF.toList m)
  where f (MapF.Pair (OpcodeGoodShape op) pf) = fmap (map (InstructionWTFormula op)) (templatizeFormula' sym pf)
