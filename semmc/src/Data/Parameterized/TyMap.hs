{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


-- | Type-level maps
module Data.Parameterized.TyMap (
  TyFun,
  Apply,
  Map,
  applyMapList,
  MapContext,
  applyMapContext
  ) where

import Prelude hiding (reverse)
import           Data.Kind
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as SL
import           Data.Proxy ( Proxy(..) )

data TyFun :: k1 -> k2 -> Type
type family Apply (f :: TyFun k1 k2 -> Type) (x :: k1) :: k2


type family Map (f :: TyFun k1 k2 -> Type) (xs :: [k1]) :: [k2] where
  Map f '[] = '[]
  Map f (x ': xs) = Apply f x ': Map f xs

-- | Apply a type-modifying 'Map' to each element of an 'SL.List' 
applyMapList :: forall k1 k2 (f :: TyFun k1 k2 -> Type) (xs :: [k1])
                        (g :: k2 -> Type) (h :: k1 -> Type)
               . Proxy f -> (forall (x :: k1). h x -> g (Apply f x))
              -> SL.List h xs
              -> SL.List g (Map f xs)
applyMapList _ _ SL.Nil = SL.Nil
applyMapList p1 f (x SL.:< xs) = f x SL.:< (applyMapList p1 f xs)

type family MapContext (f :: TyFun k1 k2 -> Type) (xs :: Ctx.Ctx k1) :: Ctx.Ctx k2 where
  MapContext f Ctx.EmptyCtx = Ctx.EmptyCtx
  MapContext f (xs Ctx.::> x) = MapContext f xs Ctx.::> Apply f x


-- | Apply a type-modifying 'MapContext' to each element of an 'Ctx.Assignment' 
applyMapContext :: forall k1 k2 (f :: TyFun k1 k2 -> Type) (xs :: Ctx.Ctx k1)
                        (g :: k2 -> Type) (h :: k1 -> Type)
               . Proxy f -> (forall (x :: k1). h x -> g (Apply f x))
              -> Ctx.Assignment h xs
              -> Ctx.Assignment g (MapContext f xs)
applyMapContext p1 f asn = case Ctx.viewAssign asn of
  Ctx.AssignEmpty -> Ctx.empty
  Ctx.AssignExtend asn' x -> applyMapContext p1 f asn' Ctx.:> f x
