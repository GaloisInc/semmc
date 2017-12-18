{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Data.Parameterized.Unfold (
  unfoldShape
  ) where

import           Control.Monad.Catch ( MonadThrow )
import qualified Data.Parameterized.List as SL

unfoldShape :: (MonadThrow m)
            => SL.List r sh
            -> (a -> m (b '[]))
            -> (forall tp tps . (sh ~ (tp ': tps)) => r tp -> SL.List r tps -> a -> m (b sh))
            -> a
            -> m (b sh)
unfoldShape rep0 nil elt a =
  case rep0 of
    SL.Nil -> nil a
    rep SL.:< reps -> elt rep reps a
