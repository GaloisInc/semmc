{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Data.Parameterized.Seq
  ( SeqF(..)
  , singleton
  , (><)
  ) where

import           Data.Kind
import qualified Data.Sequence as Seq

newtype SeqF (a :: k -> Type) (tp :: k) = SeqF { unSeqF :: Seq.Seq (a tp) }

singleton :: a tp -> SeqF a tp
singleton = SeqF . Seq.singleton

(><) :: SeqF a tp -> SeqF a tp -> SeqF a tp
SeqF s1 >< SeqF s2 = SeqF (s1 Seq.>< s2)

infixr 5 ><
