{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities related to Hedgehog testing that are not supplied by
-- Hedgehog itself.
--
-- Primarily at this point:
--  * a MonadMask instance for PropertyT.
--  * non-identical subterm argument support

module HedgehogUtil where

import qualified Control.Monad.Catch as E
import           Control.Monad.Trans.Class ( lift )
import           Hedgehog.Internal.Gen
import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Tree

-- Hedgehog provides subtermM, subtermM2, and subtermM3, which can be
-- used for recursively reproducing a structure from combinations of
-- _itself_ (once, twice, or thrice), but it does not support the use
-- of constructions from *other* types.  These are provided here.
--
-- NOTE: currently these are extremely simplistic and unsound because
-- there is no controlled shrinking provided so there is no guarantee
-- that they will converge during shrinking.

-- very simplisic alternative; no good shrinking characteristics
subtermM' :: MonadGen m => m a -> (a -> m d) -> m d
subtermM' gx f = do
  x <- gx
  f x

-- very simplisic alternative; no good shrinking characteristics
subtermM2' :: MonadGen m => m a -> m b -> (a -> b -> m d) -> m d
subtermM2' gx gy f = do
  x <- gx
  y <- gy
  f x y

-- very simplisic alternative; no good shrinking characteristics
subtermM3' :: MonadGen m => m a -> m b -> m c -> (a -> b -> c -> m d) -> m d
subtermM3' gx gy gz f = do
  x <- gx
  y <- gy
  z <- gz
  f x y z


----------------------------------------------------------------------

instance E.MonadMask m => E.MonadMask (TreeT m) where
  mask a = TreeT $ E.mask $ \u -> runTreeT (a $ mapTreeT u)

  uninterruptibleMask a =
    TreeT $ E.uninterruptibleMask $ \u -> runTreeT (a $ mapTreeT u)

  generalBracket acquire release use =
    lift $ E.generalBracket
              (treeAcquire acquire)
              (treeRelease release)
              (treeUse use)

treeAcquire :: TreeT m a -> m (NodeT m a)
treeAcquire acquire = runTreeT acquire

treeRelease :: Monad m =>
               (a -> E.ExitCase b -> TreeT m c)
            -> NodeT m a
            -> E.ExitCase b
            -> m c
treeRelease release resource exitCase =
  nodeValue <$> runTreeT (release (nodeValue resource) exitCase)

treeUse :: Monad m => (a -> TreeT m b) -> NodeT m a -> m b
treeUse use resource =
  nodeValue <$> runTreeT (use (nodeValue resource))

instance E.MonadMask m => E.MonadMask (GenT m) where
  mask a = GenT $ \sz -> \seed -> E.mask $ \u -> unGenT (a $ q u) sz seed
    where q u o = GenT $ \sz' -> \seed' -> u $ unGenT o sz' seed'

  uninterruptibleMask a =
    GenT $ \sz -> \seed ->
                    E.uninterruptibleMask $ \u ->
                                              unGenT (a $ q u) sz seed
    where q u o = GenT $ \sz' -> \seed' -> u $ unGenT o sz' seed'

  generalBracket acquire release use = GenT $ \sz -> \seed ->
    E.generalBracket
    (unGenT acquire sz seed)
    (\resource exitCase -> unGenT (release resource exitCase) sz seed)
    (\resource -> unGenT (use resource) sz seed)


instance E.MonadMask m => E.MonadMask (TestT m) where
  mask a = TestT $ E.mask $ \u -> unTest (a $ q u)
    where q u = TestT . u . unTest

  uninterruptibleMask a =
    TestT $ E.uninterruptibleMask $ \u -> unTest (a $ q u)
    where q u = TestT . u . unTest

  generalBracket acquire release use = TestT $
    E.generalBracket
    (unTest acquire)
    (\resource exitCase -> unTest (release resource exitCase))
    (\resource -> unTest (use resource))


instance E.MonadMask m => E.MonadMask (PropertyT m) where
  mask a = PropertyT $ E.mask $ \u -> unPropertyT (a $ q u)
    where q u = PropertyT . u . unPropertyT

  uninterruptibleMask a =
    PropertyT $ E.uninterruptibleMask $ \u -> unPropertyT (a $ q u)
    where q u = PropertyT . u . unPropertyT

  generalBracket acquire release use = PropertyT $
    E.generalBracket
    (unPropertyT acquire)
    (\resource exitCase -> unPropertyT (release resource exitCase))
    (\resource -> unPropertyT (use resource))
