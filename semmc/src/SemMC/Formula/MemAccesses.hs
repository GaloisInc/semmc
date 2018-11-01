{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module SemMC.Formula.MemAccesses
  ( liveMem
  , liveMem'
  , liveMemInExpr
  , liveMemSymbolic
  , liveMemConst
  , partitionEithers
  , partitionLocs
  ) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Either as Either
import           Data.Foldable
import           Data.Maybe (listToMaybe)

import           Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Classes as P
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC

import qualified What4.Interface as S
import qualified What4.Expr as WE
import qualified What4.Expr.Builder as WB
import qualified What4.Protocol.Online as WPO
import qualified What4.Symbol as WS
import qualified What4.Concrete as WC

import           SemMC.Architecture
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula.Formula


partitionLocs :: forall arch
               . Architecture arch
              => Set.Set (Some (L.Location arch))
              -> ( Set.Set (Some (L.Location arch))
                 , Maybe (L.MemLoc (L.Location arch)))
partitionLocs locs = 
    let memLocs = Set.filter (\(Some l) -> L.isMemLoc l) locs
        memLocList = (\(Some l) -> L.toMemLoc l) <$> Set.toList memLocs
    in (locs `Set.difference` memLocs, listToMaybe memLocList)


-- | Returns a list of memory locations (as bit vectors) that are accessed or
-- written to in a formula, along with a list of symbolic expressions
liveMem :: Architecture arch
        => Formula (WE.ExprBuilder t st fs) arch
        -> (Set.Set Integer, Set.Set (Some (WE.Expr t)))
liveMem form = partitionEithers $ liveMem' form

liveMem' :: Architecture arch
         => Formula (WE.ExprBuilder t st fs) arch
         -> Set.Set (Either Integer (Some (WE.Expr t)))
liveMem' form = case readLocation "Mem" of
                  Just (Some mem) -> -- maybe [] (Set.toList . (liveMemInExpr form)) 
                                     --          (MapF.lookup mem (formDefs form))
                                     case MapF.lookup mem (formDefs form) of
                                       Nothing -> Set.empty
                                       Just e  -> liveMemInExpr form e
                  Nothing         -> error "Cannot construct the location 'Mem'"


partitionEithers :: (Ord a,Ord b) => Set.Set (Either a b) -> (Set.Set a, Set.Set b)
partitionEithers = foldr (Either.either (\a (sA,sB) -> (Set.insert a sA,sB)) 
                                        (\b (sA,sB) -> (sA,Set.insert b sB))) 
                         (Set.empty,Set.empty)
                     

liveMemConst :: Architecture arch
             => Formula (WE.ExprBuilder t st fs) arch
             -> [Integer]
liveMemConst = Set.toList . fst . liveMem 


-- | Coerce an integer into a memory address
mkAddress :: S.IsExprBuilder sym
          => sym -> L.MemLoc loc -> Integer -> IO (Some (S.SymExpr sym))
mkAddress sym (L.MemLoc w _) i = do e <- S.bvLit sym w i
                                    return (Some e)
mkAddresses :: S.IsExprBuilder sym
          => sym -> L.MemLoc loc -> Set.Set Integer -> IO (Set.Set (Some (S.SymExpr sym)))
mkAddresses = undefined

liveMemSymbolic :: (Architecture arch)
                => Formula (WE.ExprBuilder t st fs) arch
                -> Set.Set (Some (WE.Expr t))
liveMemSymbolic = undefined

liveMemInExprs :: Architecture arch
               => Formula (WE.ExprBuilder t st fs) arch
               -> Ctx.Assignment (WE.Expr t) idx
               -> Set.Set (Either Integer (Some (WE.Expr t)))
liveMemInExprs form idx = foldMapFC (liveMemInExpr form) idx


liveMemInExpr :: Architecture arch
              => Formula (WE.ExprBuilder t st fs) arch
              -> WE.Expr t a
              -> Set.Set (Either Integer (Some (WE.Expr t)))
-- liveMemInExpr _ (WE.BVExpr _ i _)     = Set.singleton i
-- liveMemInExpr f (WE.SemiRingLiteral WE.SemiRingNat n _)  = Set.singleton $ toInteger n
-- liveMemInExpr f (WE.SemiRingLiteral WE.SemiRingInt i _)  = Set.singleton i
-- liveMemInExpr f (WE.SemiRingLiteral WE.SemiRingReal _ _) = Set.empty
-- liveMemInExpr f (WE.StringExpr s _)   = Set.empty
liveMemInExpr f (WE.AppExpr a)        = liveMemInApp f $ WE.appExprApp a 
liveMemInExpr f (WE.NonceAppExpr a)   = liveMemInNonceApp f $ WE.nonceExprApp a
liveMemInExpr _ _                     = Set.empty

liveMemInApp :: Architecture arch
              => Formula (WE.ExprBuilder t st fs) arch
              -> WE.App (WE.Expr t) a
              -> Set.Set (Either Integer (Some (WE.Expr t)))
liveMemInApp _ WE.TrueBool = Set.empty
liveMemInApp _ WE.FalseBool = Set.empty
liveMemInApp f (WE.NotBool e) = liveMemInExpr f e
liveMemInApp f (WE.AndBool e1 e2) = liveMemInExpr f e1 `Set.union` liveMemInExpr f e2
liveMemInApp f (WE.XorBool e1 e2) = liveMemInExpr f e1 `Set.union` liveMemInExpr f e2
liveMemInApp f (WE.IteBool e e1 e2) = liveMemInExpr f e 
                          `Set.union` liveMemInExpr f e1 
                          `Set.union` liveMemInExpr f e2
liveMemInApp f (WE.BVAdd _ e1 e2) = liveMemInExpr f e1 `Set.union` liveMemInExpr f e2
liveMemInApp f (WE.BVConcat _ e1 e2) = liveMemInExpr f e1 `Set.union` liveMemInExpr f e2
liveMemInApp f (WE.BVSext _ e) = liveMemInExpr f e
liveMemInApp f (WE.ConstantArray _ _ e) = liveMemInExpr f e
liveMemInApp _ e = error $ "Case " ++ show e ++ " not covered in liveMemInApp"
-- TODO: expand liveMemInApp to more cases, possibly with lenses?


exprSymFnToUninterpFn :: forall arch t args ret.
                         Architecture arch 
                      => WE.ExprSymFn t args ret -> Maybe (UninterpFn arch '(args,ret))
exprSymFnToUninterpFn f = 
  case WE.symFnInfo f of
    WE.UninterpFnInfo args ret -> do
      let name = Text.unpack . WS.solverSymbolAsText $ WE.symFnName f 
      Some f'@(MkUninterpFn _ args' ret' _) <- getUninterpFn @arch name
      case (P.testEquality args args', P.testEquality ret ret') of 
        (Just P.Refl, Just P.Refl) -> return f'
        (_, _)                     -> Nothing
    _ -> Nothing



liveMemInNonceApp :: forall arch t st fs a. 
                 Architecture arch
              => Formula (WE.ExprBuilder t st fs) arch
              -> WE.NonceApp t (WE.Expr t) a
              -> Set.Set (Either Integer (Some (WE.Expr t)))
-- when we get to an uninterpreted function, we should check whether the live
-- registers they ask for are constants; if they are, add that constant to the
-- result set. If not, construct an abstract formula from the result;
-- > Old_Mem => New_Mem
-- where 
-- > Old_Mem = WhatWeKnow /\ i≠anyKnownLoc => Mem[i]=0
-- and
-- > New_Mem = WhatWeKnow /\ i ≠ anyKnownLoc => updatedMem[i]=0
liveMemInNonceApp form (WE.FnApp f args) =
    case exprSymFnToUninterpFn @arch f of
      Just (MkUninterpFn name _ _ liveness) ->
        let exprs = liveness args
            maybeConsts = decideConst <$> exprs
        in Set.fromList maybeConsts `Set.union` liveMemInExprs form args
      Nothing -> Set.empty

  where
    decideConst :: Some (WE.Expr t) -> Either Integer (Some (WE.Expr t))
    decideConst (Some e) = case S.asConcrete e of
                              Just (WC.ConcreteBV _ i) -> Left i
                              Just (WC.ConcreteInteger i) -> Left i
                              Just (WC.ConcreteNat n) -> Left (toInteger n)
                              _ -> Right (Some e)
liveMemInNonceApp _ _ = undefined

