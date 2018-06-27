{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.List (
  FunctionOver,
  applyFunctionOver,
  TyFun,
  Apply,
  Reverse,
  ReverseAcc,
  reverse,
  reverseAcc,
  Map,
  mapFromMapped,
  ToContext,
  ToContextFwd,
  SameShape,
  toAssignment,
  toAssignmentFwd
  ) where

import Prelude hiding (reverse)

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as SL
import           Data.Proxy ( Proxy(..) )

type family FunctionOver f xs y where
  FunctionOver f '[] y = y
  FunctionOver f (x ': xs) y = f x -> FunctionOver f xs y

applyFunctionOver :: FunctionOver f xs y -> SL.List f xs -> y
applyFunctionOver f SL.Nil = f
applyFunctionOver f (x SL.:< xs) = applyFunctionOver (f x) xs

data TyFun :: k1 -> k2 -> *
type family Apply (f :: TyFun k1 k2 -> *) (x :: k1) :: k2
type family ReverseAcc xs acc where
  ReverseAcc '[] acc = acc
  ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

type family Reverse xs where
  Reverse xs = ReverseAcc xs '[]

reverseAcc :: SL.List f xs -> SL.List f ys -> SL.List f (ReverseAcc xs ys)
reverseAcc SL.Nil ys = ys
reverseAcc (x SL.:< xs) ys = reverseAcc xs (x SL.:< ys)

reverse :: SL.List f xs -> SL.List f (Reverse xs)
reverse xs = reverseAcc xs SL.Nil

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

type ToContextFwd (lst :: [k]) = ToContext (Reverse lst)

type family SameShape args sh where
  SameShape args sh = args ~ ToContextFwd sh

toAssignment :: SL.List f lst -> Ctx.Assignment f (ToContext lst)
toAssignment SL.Nil = Ctx.empty
toAssignment (a SL.:< rest) = toAssignment rest `Ctx.extend` a

toAssignmentFwd :: SL.List f lst -> Ctx.Assignment f (ToContextFwd lst)
toAssignmentFwd lst = toAssignment (reverse lst)
