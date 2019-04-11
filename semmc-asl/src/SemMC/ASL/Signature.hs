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
                    , funcGlobalReprs :: Some (Ctx.Assignment (LabeledValue T.Text CT.TypeRepr))
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

data ProcedureSignature init ret bts =
  ProcedureSignature { procSigBaseRepr :: WT.BaseTypeRepr (WT.BaseStructType bts)
                      -- ^ The return type (in terms of base types) of the procedure
                      , procSigRepr :: CT.TypeRepr ret
                      -- ^ The return type (in terms of Crucible types) of the procedure
                      , procSigGlobals :: Ctx.Assignment BaseGlobalVar bts
                      -- ^ The globals written to by the procedure - note that the type parameter is
                      -- the same as the return type repr by design, as we need to convert the
                      -- global writes into a struct return type
                      , procSigAssignedBase :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) bts
                      -- ^ The variables written to by the procedure, used to compute to footprint
                      -- of the function
                      , procSigAssigned :: Some (Ctx.Assignment (LabeledValue T.Text CT.TypeRepr))
                      -- ^ The names and types of all of the globals assigned to by this procedure
                      -- (including transitive assignments).  Note that these are Crucible types,
                      -- and that we can't easily relate this type to @bts@
                      , procSigArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                      -- ^ The types (and names) of the arguments to the procedure
                      --
                      -- Note that this has a different type compared to the footprint, and that we
                      -- don't need base types to talk about the parameters.
                      }
  deriving (Show)

instance ShowF (ProcedureSignature init ret)

data SomeSignature where
  SomeFunctionSignature :: (ret ~ CT.BaseToType tp) => FunctionSignature init ret tp -> SomeSignature
  SomeProcedureSignature :: ProcedureSignature init rep tps -> SomeSignature

deriving instance Show SomeSignature
