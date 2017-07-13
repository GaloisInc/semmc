{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Data.Parameterized.Unfold (
  UnfoldShape(..),
  RecShape
  ) where

import Control.Monad.Catch ( MonadThrow )
import Data.Proxy ( Proxy(..) )

-- | The constraints required for unfolding shapes recursively
type RecShape tp tps' tps = (UnfoldShape tps', tps ~ (tp ': tps'))

-- | Based on a shape (a type-level list of elements of some kind @k@), unfold a
-- plain value (of kind '*') to produce a value that has the required shape.
--
-- The entire process is in 'E.MonadThrow' to allow for reporting detailed
-- failures.  It can easily be instantiated with 'Maybe', 'Either', or 'IO'.
class UnfoldShape (tps :: [k]) where
  unfoldShape :: (MonadThrow m)
              => (a -> m (b '[]))
              -- ^ Produce the value associated with the empty list (e.g., an HList nil)
              -> (forall proxy1 proxy2 tp tps' . (RecShape tp tps' tps) => proxy1 tp -> proxy2 tps' -> a -> m (b tps))
              -- ^ Recursively process a smaller part of the shape and then
              -- augment the resulting value with the current element (almost
              -- certainly requires a recursive call to 'unfoldShape')
              -> a
              -- ^ The seed of the unfold
              -> m (b tps)

instance UnfoldShape '[] where
  unfoldShape nil _elt a = nil a

instance (UnfoldShape tps) => UnfoldShape (tp ': tps) where
  unfoldShape _nil elt a = elt (Proxy :: Proxy tp) (Proxy :: Proxy tps) a

{-

data Free (a :: *) (b :: k) = Free { unFree :: a }

buildOperandList :: (UnfoldShape tps) => SC.SExpr Atom -> Maybe (D.OperandList (Free Int) tps)
buildOperandList = unfoldShape nilOp eltOp

nilOp :: SC.SExpr t -> Maybe (D.OperandList a '[])
nilOp SC.SNil = Just D.Nil
nilOp _ = Nothing

eltOp :: (RecShape tp tps' tps)
      => Proxy tp
      -> Proxy tps'
      -> SC.SExpr Atom
      -> Maybe (D.OperandList (Free Int) tps)
eltOp _ _ SC.SNil = Nothing
eltOp _ _ (SC.SAtom _) = Nothing
eltOp _ _ (SC.SCons _s rest) = do
  rest' <- unfoldShape nilOp eltOp rest
  return ((Free 1) D.:> rest')

-}
