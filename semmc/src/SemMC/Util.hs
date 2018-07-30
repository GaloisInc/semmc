{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module SemMC.Util
  ( -- * Misc
    groundValToExpr
  , makeSymbol
  , mapFReverse
  , sequenceMaybes
  , allBoundVars
  , extractUsedLocs
  , mapFMapBothM
  , filterMapF
  , fromJust'
    -- * Async
  , asyncLinked
  , withAsyncLinked
    -- * Reexports
  , module SemMC.Log
  ) where

import           Control.Monad.ST ( runST )
import qualified Data.HashTable.Class as H
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Set as Set
import           Text.Printf ( printf )

import qualified UnliftIO as U
import qualified Control.Exception as E

import           What4.BaseTypes
import qualified What4.Expr.Builder as B
import qualified What4.Expr.GroundEval as GE
import qualified What4.Interface as S
import           What4.Symbol ( SolverSymbol, userSymbol )
import qualified What4.Utils.Hashable as Hash

import           SemMC.Log

----------------------------------------------------------------
-- * Async

-- | Fork an async action that is linked to the parent thread, but can
-- be safely 'U.cancel'd without also killing the parent thread.
--
-- Note that if your async doesn't return unit, then you probably want
-- to 'U.wait' for it instead, which eliminates the need for linking
-- it. Also, if you plan to cancel the async near where you fork it,
-- then 'withAsyncLinked' is a better choice than using this function
-- and subsequently canceling, since it ensures cancellation.
--
-- See https://github.com/simonmar/async/issues/25 for a perhaps more
-- robust, but also harder to use version of this. The linked version
-- is harder to use because it requires a special version of @cancel@.
asyncLinked :: (U.MonadUnliftIO m) => m () -> m (U.Async ())
asyncLinked action = do
  -- We use 'U.mask' to avoid a race condition between starting the
  -- async and running @action@. Without 'U.mask' here, an async
  -- exception (e.g. via 'U.cancel') could arrive after
  -- @handleUnliftIO@ starts to run but before @action@ starts.
  U.mask $ \restore -> do
  a <- U.async $ handleUnliftIO (\E.ThreadKilled -> return ()) (restore action)
  restore $ do
  U.link a
  return a

-- | A version of 'U.withAsync' that safely links the child. See
-- 'asyncLinked'.
withAsyncLinked :: (U.MonadUnliftIO m) => m () -> (U.Async () -> m a) -> m a
withAsyncLinked child parent = do
  U.mask $ \restore -> do
  U.withAsync (handleUnliftIO (\E.ThreadKilled -> return ()) $ restore child) $ \a -> restore $ do
  U.link a
  parent a

-- A 'U.MonadUnliftIO' version of 'Control.Exception.handle'.
--
-- The 'U.handle' doesn't catch async exceptions, because the
-- @unliftio@ library uses the @safe-execeptions@ library, not
-- @base@, for it exception handling primitives. This is very
-- confusing if you're not expecting it!
handleUnliftIO :: (U.MonadUnliftIO m, U.Exception e)
               => (e -> m a) -> m a -> m a
handleUnliftIO h a = U.withUnliftIO $ \u ->
  E.handle (U.unliftIO u . h) (U.unliftIO u a)

----------------------------------------------------------------
-- * Uncategorized

-- | Try converting any 'String' into a 'SolverSymbol'. If it is an invalid
-- symbol, then error.
makeSymbol :: String -> SolverSymbol
makeSymbol name = case userSymbol sanitizedName of
                    Right symbol -> symbol
                    Left _ -> error $ printf "tried to create symbol with bad name: %s (%s)"
                                             name sanitizedName
  where
    sanitizedName = map (\c -> case c of ' ' -> '_'; '.' -> '_'; _ -> c) name

-- | Convert a 'GroundValue' (a primitive type that represents the given
-- Crucible type) back into a symbolic expression, just as a literal.
groundValToExpr :: forall sym tp.
                   (S.IsExprBuilder sym)
                => sym
                -> BaseTypeRepr tp
                -> GE.GroundValue tp
                -> IO (S.SymExpr sym tp)
groundValToExpr sym BaseBoolRepr True = return (S.truePred sym)
groundValToExpr sym BaseBoolRepr False = return (S.falsePred sym)
groundValToExpr sym (BaseBVRepr w) val = S.bvLit sym w val
groundValToExpr sym BaseNatRepr val = S.natLit sym val
groundValToExpr sym BaseIntegerRepr val = S.intLit sym val
groundValToExpr sym BaseRealRepr val = S.realLit sym val
groundValToExpr sym BaseComplexRepr val = S.mkComplexLit sym val
groundValToExpr sym (BaseArrayRepr idxTp elemTp) (GE.ArrayConcrete base m) = do
  base' <- groundValToExpr sym elemTp base
  entries <- Hash.mkMap <$> traverse (groundValToExpr sym elemTp) m
  S.arrayFromMap sym idxTp entries base'
groundValToExpr _ (BaseArrayRepr _ _) (GE.ArrayMapping _) = error "groundValToExpr: ArrayMapping not handled"
groundValToExpr _ (BaseStructRepr _) _ = error "groundValToExpr: struct type isn't handled yet"

-- | Reverse a MapF, so that the old keys are the new values and the old values
-- are the new keys.
mapFReverse :: (OrdF value) => MapF.MapF key value -> MapF.MapF value key
mapFReverse = MapF.foldrWithKey (flip MapF.insert) MapF.empty

-- | Run the monadic actions in order, returning the first 'Just' value.
sequenceMaybes :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
sequenceMaybes [] = return Nothing
sequenceMaybes (x : xs) = x >>= maybe (sequenceMaybes xs) (return . Just)

-- | Find all the bound variables in a symbolic expression.
allBoundVars :: B.Expr t tp -> Set.Set (Some (B.ExprBoundVar t))
allBoundVars e = runST (B.boundVars e >>= H.foldM f Set.empty)
  where f s (_, v) = return (Set.union s v)

-- | Given a map from location to bound variable, return all of the locations
-- that are actually used in an expression (along with their corresponding
-- variables).
extractUsedLocs :: forall t loc tp
                 . (OrdF loc)
                => MapF.MapF loc (B.ExprBoundVar t)
                -> B.Expr t tp
                -> MapF.MapF loc (B.ExprBoundVar t)
extractUsedLocs locMapping expr = MapF.mapMaybe keepIfNeeded locMapping
  where
    keepIfNeeded :: forall tp' . B.ExprBoundVar t tp' -> Maybe (B.ExprBoundVar t tp')
    keepIfNeeded bv' =
      case Set.member (Some bv') bvs of
        False -> Nothing
        True -> Just bv'
    bvs = allBoundVars expr

-- | Monadically map both keys and values of a 'MapF.MapF'.
mapFMapBothM :: forall k1 v1 k2 v2 m.
                (OrdF k2, Monad m)
             => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp))
             -> MapF.MapF k1 v1
             -> m (MapF.MapF k2 v2)
mapFMapBothM f = MapF.foldrWithKey f' (return MapF.empty)
  where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
        f' k v wrappedM = do
          (k', v') <- f k v
          m <- wrappedM
          return $ MapF.insert k' v' m

-- | Filter the elements of a 'MapF.MapF'.
filterMapF :: forall k v. (OrdF k) => (forall tp. k tp -> v tp -> Bool) -> MapF.MapF k v -> MapF.MapF k v
filterMapF f = MapF.foldrWithKey go MapF.empty
  where go :: forall tp. k tp -> v tp -> MapF.MapF k v -> MapF.MapF k v
        go key value m
          | f key value = MapF.insert key value m
          | otherwise   = m

-- | Traceback-friendly fromJust alternative.
fromJust' :: (HasCallStack) => String -> Maybe a -> a
fromJust' label x =
    let msg = "fromJust': got Nothing (" ++ label ++ ")"
    in fromMaybe (error msg) x
