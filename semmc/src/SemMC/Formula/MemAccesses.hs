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
  (-- liveMem
--  , liveMem'
   liveMemInExpr
  , liveMem
  , liveMemAddresses
  , liveMemConst
  , liveMemMap
--  , partitionEithers
  , partitionLocs
  , someArrayLookup
  , someIsEq
  ) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Either as Either
import qualified Data.Map as Map
import           Data.Foldable
import           Data.Maybe (listToMaybe, catMaybes)
import           GHC.TypeNats (KnownNat)

import           Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.NatRepr as P
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

-- | Given a formula @F@, construct a map @i ↦ lookup (F(mem)) i@ for each @i@ in
-- the live memory addresses of @F@
liveMemMap :: forall arch t st fs sym.
              (Architecture arch, sym ~ WE.ExprBuilder t st fs)
           => Formula sym arch
           -> Map.Map (Some (WE.Expr t)) (Some (WE.Expr t))
liveMemMap f = foldMap accessMap $ liveMem f 
  where
    accessMap :: AccessData sym -> Map.Map (Some (WE.Expr t)) (Some (WE.Expr t))
    accessMap (ReadData _)    = Map.empty
    accessMap (WriteData e i) = Map.singleton (Some e) (Some i)

  

-- | @someArrayLookup sym (Some arr) (Some i)@ results in @Some <$>
-- S.arrayLookup arr i@ as long as @arr@ and @i@ have the correct type;
-- otherwise throws an error
someArrayLookup :: S.IsExprBuilder sym
                => sym 
                -> Some (S.SymExpr sym)
                -> Some (S.SymExpr sym)
                -> IO (Some (S.SymExpr sym))
someArrayLookup sym (Some arr) (Some i)
  | S.BaseArrayRepr (Ctx.Empty Ctx.:> tp) _ <- S.exprType arr -- does the array expression have the right shape?
  , Just S.Refl <- S.testEquality tp (S.exprType i) -- does the argument type match the indices of the array?
  = Some <$> S.arrayLookup sym arr (Ctx.Empty Ctx.:> i)
  | otherwise = error "Could not construct an array lookup because arguments have the wrong type"

someIsEq :: (S.IsExprBuilder sym, P.ShowF (S.SymExpr sym))
         => sym
         -> Some (S.SymExpr sym)
         -> Some (S.SymExpr sym)
         -> IO (S.Pred sym)
someIsEq sym (Some x) (Some y) 
  | Just S.Refl <- S.testEquality (S.exprType x) (S.exprType y) = S.isEq sym x y
  | otherwise = error $ "Could not construct an equality predicate because its arguments have the wrong type\n" 
                     ++ "Expression 1: " ++ P.showF x ++ "\nHas type " ++ show (S.exprType x)
                     ++ "\nExpression 2: " ++ P.showF y ++ "\nHas type " ++ show (S.exprType y)


-- | Returns the set of memory addresses that are accessed by the formula
liveMem :: (Architecture arch)
        => Formula (WE.ExprBuilder t st fs) arch
        -> Set.Set (AccessData (WE.ExprBuilder t st fs))
liveMem f = foldMapF (liveMemInExpr f) (formDefs f)

liveMemAddresses :: forall arch t st fs. 
                    Architecture arch
                 => Formula (WE.ExprBuilder t st fs) arch
                 -> Set.Set (Some (WE.Expr t))
liveMemAddresses = Set.map accessAddr . liveMem

{-
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



liveMemInExprs :: Architecture arch
               => Formula (WE.ExprBuilder t st fs) arch
               -> Ctx.Assignment (WE.Expr t) idx
               -> Set.Set (Some (WE.Expr t))
liveMemInExprs form idx = foldMapFC (liveMemInExpr form) idx
-}

liveMemConst :: forall arch t st fs.
                Architecture arch
             => Formula (WE.ExprBuilder t st fs) arch
             -> [Integer]
liveMemConst f = catMaybes $ exprToInt <$> (Set.toList $ liveMemAddresses f)
  where
    exprToInt :: Some (WE.Expr t) -> Maybe Integer
    exprToInt (Some e) | Just (WC.ConcreteBV _ i)    <- S.asConcrete e = Just i
    exprToInt (Some e) | Just (WC.ConcreteInteger i) <- S.asConcrete e = Just i
    exprToInt _        | otherwise = Nothing


liveMemInExpr :: Architecture arch
              => Formula (WE.ExprBuilder t st fs) arch
              -> WE.Expr t a
              -> Set.Set (AccessData (WE.ExprBuilder t st fs))
liveMemInExpr f (WE.AppExpr a)        = foldMapFC (liveMemInExpr f) $ WE.appExprApp a 
liveMemInExpr f (WE.NonceAppExpr a)   = liveMemInNonceApp f $ WE.nonceExprApp a
liveMemInExpr _ _                     = Set.empty

{-
liveMemInApp :: Architecture arch
              => Formula (WE.ExprBuilder t st fs) arch
              -> WE.App (WE.Expr t) a
              -> Set.Set (Some (WE.Expr t))
liveMemInApp f app = foldMapFC (liveMemInExpr f) app
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
-}


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
              -> Set.Set (AccessData (WE.ExprBuilder t st fs))
liveMemInNonceApp form (WE.FnApp f args) =
    case exprSymFnToUninterpFn @arch f of
      Just (MkUninterpFn _ _ _ liveness) ->
        let exprs = liveness args
        -- Add the live expressions to the set
        -- Also recurse along arguments
        in Set.fromList exprs `Set.union` foldMapFC (liveMemInExpr form) args
      Nothing -> foldMapFC (liveMemInExpr form) args
liveMemInNonceApp form app = foldMapFC (liveMemInExpr form) app

