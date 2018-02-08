{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.List (
  TyFun,
  Apply,
  Reverse,
  Map,
  ToContext,
  SameShape
  ) where

import qualified Data.Parameterized.Context as Ctx

data TyFun :: k1 -> k2 -> *
type family Apply (f :: TyFun k1 k2 -> *) (x :: k1) :: k2
type family ReverseAcc xs acc where
  ReverseAcc '[] acc = acc
  ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

type family Reverse xs where
  Reverse xs = ReverseAcc xs '[]

type family Map (f :: TyFun k1 k2 -> *) (xs :: [k1]) :: [k2] where
  Map f '[] = '[]
  Map f (x ': xs) = Apply f x ': Map f xs

type family ToContext (lst :: [k]) :: Ctx.Ctx k where
  ToContext '[] = Ctx.EmptyCtx
  ToContext (a ': rest) = ToContext rest Ctx.::> a

type family SameShape args sh where
  SameShape args sh = args ~ ToContext (Reverse sh)
