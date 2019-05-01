{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.ASL.Signature (
    FunctionSignature(..)
  , ProcedureSignature(..)
  , SomeSignature(..)
  , LabeledValue(..)
  , projectValue
  , BaseGlobalVar(..)
  ) where

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT

-- | The signature describes the inputs and outputs of an ASL function or procedure
--
-- Procedures have side effects, while functions are side-effect free and return a single value
-- (which may be a tuple).
--
-- Top-level code sequences (like the @instExecute@ field of an instruction) have a trivial type
-- signature with no inputs (just global refs) and a set of outputs that is the union of all of the
-- locations touched by that function.
data FunctionSignature init ret tp =
  FunctionSignature { funcSigRepr :: WT.BaseTypeRepr tp
                    , funcArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                    , funcGlobalReprs :: Some (Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr))
                    }
  deriving (Show)

data LabeledValue a b tp = LabeledValue a (b tp)

projectValue :: LabeledValue a b tp -> b tp
projectValue (LabeledValue _ v) = v

instance FC.FunctorFC (LabeledValue a) where
  fmapFC f (LabeledValue a b) = LabeledValue a (f b)

instance FC.FoldableFC (LabeledValue a) where
  foldrFC f s (LabeledValue _ b) = f b s

instance FC.TraversableFC (LabeledValue a) where
  traverseFC f (LabeledValue a b) = LabeledValue a <$> f b

instance (Show a, ShowF b) => ShowF (LabeledValue a b) where
  showF (LabeledValue l v) = concat [ "LabeledValue ", show l, " ", showF v ]

instance (Show a, ShowF b) => Show (LabeledValue a b tp) where
  show (LabeledValue l v) = concat [ "LabeledValue ", show l, " ", showF v ]

newtype BaseGlobalVar tp = BaseGlobalVar { unBaseVar :: CCG.GlobalVar (CT.BaseToType tp) }
  deriving (Show)

instance ShowF BaseGlobalVar

-- init are the non-global args
-- regs are the list of global registers
-- ret is the return type (actually a whole reg state, and basically congruent to regs)
-- bts is the list of extra return values - I think it will likely always be empty
--
-- Note that the actual args below (psArgReprs) has the full register state appended (as a struct)
data ProcedureSignature (init :: Ctx.Ctx CT.CrucibleType)
                        (regs :: Ctx.Ctx WT.BaseType)
                        (ret :: CT.CrucibleType) =
  ProcedureSignature { psRegsRepr :: WT.BaseTypeRepr (WT.BaseStructType regs)
                       -- ^ The type of the register file
                       --
                       -- Note that this will include state that isn't exactly a machine register,
                       -- but is CPU state that we track globally and need to thread through
                       -- procedure calls.  This is also the return type of the procedure
                       , psSigRepr :: CT.TypeRepr ret
                       -- ^ The return value of the procedure (in Crucible types).
                       --
                       -- Note that, morally, ret ~ regs, but we can't really write that.
                       , psArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                       -- ^ The full repr for the arguments to the procedure
                       --
                       -- The arguments are the stated arguments (the @init@ type, which is the
                       -- list of explicit arguments), as well as a struct containing all of the
                       -- register values at the time the procedure is called, passed as a struct
                       -- in the last argument position.
                       }
  deriving (Show)

data SomeSignature regs where
  SomeFunctionSignature :: (ret ~ CT.BaseToType tp) => FunctionSignature init ret tp -> SomeSignature regs
  SomeProcedureSignature :: ProcedureSignature init regs ret
                         -> SomeSignature regs

deriving instance Show (SomeSignature regs)
