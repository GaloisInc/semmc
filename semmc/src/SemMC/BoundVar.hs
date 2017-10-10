{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.BoundVar (
  BoundVar(..)
  ) where

import           GHC.TypeLits ( Symbol )
import           Data.Parameterized.Classes
import qualified Lang.Crucible.Solver.Interface as S

import           SemMC.Architecture.Internal ( OperandType )

newtype BoundVar (sym :: *) (arch :: *) (op :: Symbol) =
  BoundVar { unBoundVar :: S.BoundVar sym (OperandType arch op) }
deriving instance (Eq (S.BoundVar sym (OperandType arch op))) => Eq (BoundVar sym arch op)
deriving instance (Ord (S.BoundVar sym (OperandType arch op))) => Ord (BoundVar sym arch op)

instance (ShowF (S.BoundVar sym)) => Show (BoundVar sym arch op) where
  show (BoundVar var) = showF var

instance (ShowF (S.BoundVar sym)) => ShowF (BoundVar sym arch)

