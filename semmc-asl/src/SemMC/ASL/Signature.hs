{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-|

This module contains types that describe signatures of ASL functions and
procedures. Procedures have side effects, while functions are side-effect free and
return a single value (which may be a tuple).

Top-level code sequences (like the @instExecute@ field of an instruction) have a trivial type
signature with no inputs (just global refs) and a set of outputs that is the union of all of the
locations touched by that function.

-}
module SemMC.ASL.Signature (
    FunctionSignature(..)
  , ProcedureSignature(..)
  , SomeSignature(..)
  , LabeledValue(..)
  , projectValue
  , projectLabel
  , BaseGlobalVar(..)
  ) where

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT

-- | A 'FunctionSignature' describes the inputs and output of an ASL function.
--
-- An ASL function is side-effect free and returns a single value (which may be a
-- tuple). It takes as input a set of arguments, 'funcArgReprs', and a set of global
-- refs, 'funcGlobalReprs'.
data FunctionSignature globals init tp =
  FunctionSignature { funcName :: T.Text
                    , funcSigRepr :: WT.BaseTypeRepr tp
                    , funcArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                    , funcGlobalReprs :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
                    }
  deriving (Show)

data LabeledValue a b tp = LabeledValue a (b tp)

projectValue :: LabeledValue a b tp -> b tp
projectValue (LabeledValue _ v) = v

projectLabel :: LabeledValue a b tp -> a
projectLabel (LabeledValue l _) = l

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

-- | A 'ProcedureSignature' captures the signature of an ASL procedure.
--
-- init are the non-global args
-- regs are the list of global registers
-- ret is the return type (actually a whole reg state, and basically congruent to regs)
--
-- Note that the actual args below (psArgReprs) has the full register state appended (as a struct)
--
-- NOTE: We currently assume that each procedure can access all globals.  We
-- will want to improve on this at some point.
data ProcedureSignature (globals :: Ctx.Ctx WT.BaseType)
                        (init :: Ctx.Ctx CT.CrucibleType)
                        (ret :: CT.CrucibleType) =
  ProcedureSignature { psName :: T.Text
                     , psRegsRepr :: WT.BaseTypeRepr (WT.BaseStructType globals)
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
                       , psGlobalReprs :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
                       -- ^ The globals possibly accessed by this procedure.
                       --
                       -- For now, we can always make it the full set of
                       -- globals; later, we can find a tighter bound.
                       }
  deriving (Show)

data SomeSignature where
  SomeFunctionSignature :: FunctionSignature globals init tp -> SomeSignature
  SomeProcedureSignature :: ProcedureSignature globals init ret
                         -> SomeSignature

deriving instance Show SomeSignature
