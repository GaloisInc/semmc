{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module SemMC.Architecture (
  Architecture(..),
  Location,
  IsLocation(..),
  FunctionInterpretation(..),
  Instruction,
  Operand,
  IsOperand,
  Opcode,
  IsOpcode,
  OperandType
  ) where

import           Data.EnumF
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.SymbolRepr as SR
import           Data.Typeable ( Typeable )
import           GHC.TypeLits ( Symbol )
import qualified Language.Haskell.TH as TH

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

import           SemMC.Architecture.Internal
import           SemMC.Architecture.Location
import           SemMC.Formula.Formula ( LocationFuncInterp )
import           SemMC.Formula.Eval ( Evaluator )

-- | An architecture is the top-level interface for specifying a semantics
-- implementation. It has specific operands, opcodes, and state variables.
class (IsOperand (Operand arch),
       IsOpcode (Opcode arch),
       IsLocation (Location arch),
       Show (Instruction arch),
       ShowF (Operand arch),
       Typeable arch,
       ShowF (Operand arch),
       OrdF (Opcode arch (Operand arch)),
       ShowF (Opcode arch (Operand arch)),
       EnumF (Opcode arch (Operand arch)))
      => Architecture arch where
  -- | Tagged expression type for this architecture.
  --
  -- This is a bit of a hack to add extra metadata needed for the templating stuff.
  data TaggedExpr arch sym :: Symbol -> *

  -- | Untag a tagged expression.
  unTagged :: TaggedExpr arch sym s -> S.SymExpr sym (OperandType arch s)

  -- | The uninterpreted functions referred to by this architecture
  uninterpretedFunctions :: proxy arch -> [(String, Some (Ctx.Assignment BaseTypeRepr), Some BaseTypeRepr)]

  -- | Map an operand to a Crucible expression, given a mapping from each state
  -- variable to a Crucible variable.
  operandValue :: forall proxy sym s.
                  (S.IsSymInterface sym,
                   S.IsExprBuilder sym)
               => proxy arch
               -> sym
               -> (forall tp. Location arch tp -> IO (S.SymExpr sym tp))
               -> Operand arch s
               -> IO (TaggedExpr arch sym s)

  -- | Map an operand to a specific state variable, if possible.
  operandToLocation :: forall proxy s.
                       proxy arch
                    -> Operand arch s
                    -> Maybe (Location arch (OperandType arch s))

  -- | Functions used to simplify defined locations in parameterized formulas
  -- that are defined as functions of an input parameter into a concrete
  -- location
  locationFuncInterpretation :: proxy arch -> [(String, FunctionInterpretation t arch)]

  shapeReprToTypeRepr :: proxy arch -> SR.SymbolRepr s -> BaseTypeRepr (OperandType arch s)

data FunctionInterpretation t arch =
  FunctionInterpretation { locationInterp :: LocationFuncInterp arch
                         -- ^ The function interpretation to apply to functions
                         -- appearing in location definition contexts (i.e., the
                         -- 'F.Parameter' function type).
                         , exprInterpName :: TH.Name
                         -- ^ The (template haskell) 'TH.Name' of the function
                         -- to apply statically during formula translation (at
                         -- the value level) to eliminate an uninterpreted
                         -- function appearing in a semantics expression.
                         , exprInterp :: Evaluator t
                         -- ^ The evaluator to apply to uninterpreted functions
                         -- during formula instantiation (in Formula.Instantiate)
                         }

