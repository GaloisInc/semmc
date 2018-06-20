{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.List (
  TyFun,
  Apply,
  Reverse,
  Map,
  mapFromMapped,
  ToContext,
  SameShape,
  toAssignment
  ) where

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as SL
import           Data.Proxy ( Proxy(..) )

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

-- Apply a function to each element of an 'SL.List' whose shape is an
-- application of 'Map'. Tricky because doing case analysis on such a list does
-- not provide much, since GHC does not infer from, say, @Map f xs ~ '[]@ that
-- @xs ~ '[]@.
mapFromMapped :: forall (f :: TyFun k1 k2 -> *) (xs :: [k1])
                        (g :: k2 -> *) (h :: k1 -> *) w
               . Proxy f -> (forall (x :: k1). g (Apply f x) -> h x)
              -> SL.List w xs -- ^ Needed to do case analysis on @xs@
              -> SL.List g (Map f xs) -> SL.List h xs
mapFromMapped _ _ SL.Nil SL.Nil = SL.Nil
mapFromMapped p1 f (_ SL.:< repr') (a SL.:< rest) =
  f a SL.:< mapFromMapped p1 f repr' rest

type family ToContext (lst :: [k]) :: Ctx.Ctx k where
  ToContext '[] = Ctx.EmptyCtx
  ToContext (a ': rest) = ToContext rest Ctx.::> a

type family SameShape args sh where
  SameShape args sh = args ~ ToContext (Reverse sh)

toAssignment :: SL.List f lst -> Ctx.Assignment f (ToContext lst)
toAssignment SL.Nil = Ctx.empty
toAssignment (a SL.:< rest) = toAssignment rest `Ctx.extend` a
