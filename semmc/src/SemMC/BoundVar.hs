{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.BoundVar (
  BoundVar(..)
  ) where

import           Data.Kind
import           GHC.TypeLits ( Symbol )
import           Data.Parameterized.Classes
import qualified What4.Interface as S
import qualified Unsafe.Coerce as U

import           SemMC.Architecture.Internal ( OperandType )

newtype BoundVar (sym :: Type) (arch :: Type) (op :: Symbol) =
  BoundVar { unBoundVar :: S.BoundVar sym (OperandType arch op) }
deriving instance (Eq (S.BoundVar sym (OperandType arch op))) => Eq (BoundVar sym arch op)
deriving instance (Ord (S.BoundVar sym (OperandType arch op))) => Ord (BoundVar sym arch op)

instance (ShowF (S.BoundVar sym)) => Show (BoundVar sym arch op) where
  show (BoundVar var) = showF var

instance (ShowF (S.BoundVar sym)) => ShowF (BoundVar sym arch)

instance (TestEquality (S.BoundVar sym)) => TestEquality (BoundVar sym arch) where
  testEquality (BoundVar bv1) (BoundVar bv2) = do
    Refl <- testEquality bv1 bv2
    -- The OperandType type function is not injective, so knowing that
    -- @OperandType arch op a ~ OperandType arch op b@ doesn't automatically let
    -- us know that @a ~ b@.  However, we know that to be true due to the
    -- semantics of bound variables.  We unsafe coerce the proof here.
    return (U.unsafeCoerce Refl)

instance (OrdF (S.BoundVar sym)) => OrdF (BoundVar sym arch) where
  compareF b1@(BoundVar bv1) b2@(BoundVar bv2) =
    case compareF bv1 bv2 of
      LTF -> LTF
      GTF -> GTF
      EQF | Just Refl <- testEquality b1 b2 -> EQF
          | otherwise -> error "Impossible"
