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

module SemMC.Synthesis.Cegis.MemAccesses
  ( liveMemInExpr
  , liveMem
  , liveMemAddresses
  , liveMemConst
  , liveMemMap
  , partitionLocs
  , nonMemIPLocs
  , someArrayLookup
  , someIsEq
  , exprSymFnToUninterpFn
  , isReadMem
  , isWriteMem
  ) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Maybe (listToMaybe, catMaybes)

import           Data.Parameterized.Some
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Classes as P
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC

import qualified What4.Interface as S
import qualified What4.Expr as WE
import qualified What4.Expr.Builder as WB
import qualified What4.Symbol as WS
import qualified What4.Concrete as WC

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula.Formula


partitionLocs :: forall arch
               . A.Architecture arch
              => Set.Set (Some (L.Location arch))
              -> ( Set.Set (Some (L.Location arch))
                 , Maybe (L.MemLoc (L.Location arch)))
partitionLocs locs = 
    let memLocs = Set.filter (\(Some l) -> L.isMemLoc l) locs
        memLocList = (\(Some l) -> L.toMemLoc l) <$> Set.toList memLocs
    in (locs `Set.difference` memLocs, listToMaybe memLocList)

nonMemIPLocs :: forall arch.
                A.Architecture arch
             => Set.Set (Some (L.Location arch))
             -> Set.Set (Some (L.Location arch))
nonMemIPLocs locs = Set.filter (\(Some l) -> not (L.isIP l) && not (L.isMemLoc l)) locs

-- | Given a formula @F@, construct a map @i ↦ lookup (F(mem)) i@ for each @i@ in
-- the live memory addresses of @F@
liveMemMap :: forall arch t st fs sym.
              (A.Architecture arch, sym ~ WE.ExprBuilder t st fs)
           => Formula sym arch
           -> Set.Set (A.AccessData sym arch)
liveMemMap f = Set.filter isWrite $ liveMem f
  where
    isWrite :: A.AccessData sym arch -> Bool
    isWrite (A.WriteData _ _) = True
    isWrite _ = False

  

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
liveMem :: (A.Architecture arch)
        => Formula (WE.ExprBuilder t st fs) arch
        -> Set.Set (A.AccessData (WE.ExprBuilder t st fs) arch)
liveMem f = foldMapF liveMemInExpr (formDefs f)

liveMemAddresses :: forall arch t st fs. 
                    A.Architecture arch
                 => Formula (WE.ExprBuilder t st fs) arch
                 -> Set.Set (WE.Expr t (S.BaseBVType (A.RegWidth arch)))
liveMemAddresses = Set.map A.accessAddr . liveMem

liveMemConst :: forall arch t st fs.
                A.Architecture arch
             => Formula (WE.ExprBuilder t st fs) arch
             -> [Integer]
liveMemConst f = catMaybes $ exprToInt <$> (Set.toList $ liveMemAddresses f)
  where
    exprToInt :: WE.Expr t (S.BaseBVType (A.RegWidth arch)) -> Maybe Integer
    exprToInt e | Just (WC.ConcreteBV _ i)    <- S.asConcrete e = Just i
    exprToInt _ | otherwise = Nothing


liveMemInExpr :: forall arch t st fs a.
                 A.Architecture arch
              => WE.Expr t a
              -> Set.Set (A.AccessData (WE.ExprBuilder t st fs) arch)
liveMemInExpr e
  | Just S.Refl <- S.testEquality (S.exprType e) (A.memTypeRepr @arch)
  , Just (mem', wd@(A.WriteData i v)) <- isWriteMem @arch @(WE.ExprBuilder t st fs) e
  = Set.insert wd $ liveMemInExpr mem' `Set.union` liveMemInExpr i `Set.union` liveMemInExpr v

liveMemInExpr e
  | S.BaseBVRepr _ <- S.exprType e
  , Just (mem', rd@(A.ReadData i)) <- isReadMem @arch e
  = Set.insert rd $ liveMemInExpr mem' `Set.union` liveMemInExpr i

liveMemInExpr (WE.AppExpr a)        = foldMapFC liveMemInExpr $ WE.appExprApp a
liveMemInExpr (WE.NonceAppExpr a)   = foldMapFC liveMemInExpr $ WE.nonceExprApp a
liveMemInExpr _                     = Set.empty

exprSymFnToUninterpFn :: forall arch t args ret.
                         A.Architecture arch 
                      => WE.ExprSymFn t args ret -> Maybe (A.UninterpFn arch)
exprSymFnToUninterpFn f =
  case WE.symFnInfo f of
    WE.UninterpFnInfo args ret -> do
      let name = Text.unpack . WS.solverSymbolAsText $ WE.symFnName f 
      f'@(A.MkUninterpFn _ args' ret' _) <- A.getUninterpFn @arch name
      case (P.testEquality args args', P.testEquality ret ret') of 
        (Just P.Refl, Just P.Refl) -> return f'
        (_, _)                     -> Nothing
    _ -> Nothing


-- | Given an expression representing memory, returns 'Just (mem, WriteData idx
-- v)' if the original expression is equal to 'write_mem_x mem idx v' for some
-- x-length bit vector 'v'.
isWriteMem :: forall arch sym t st fs.
              ( sym ~ WE.ExprBuilder t st fs
              , A.Architecture arch
              )
           => S.SymExpr sym (A.MemType arch)
           -> Maybe ( S.SymExpr sym (A.MemType arch)
                    , A.AccessData sym arch)
isWriteMem memExpr@(WE.NonceAppExpr a)
  | WE.FnApp f (Ctx.Empty Ctx.:> mem Ctx.:> i Ctx.:> v) <- WE.nonceExprApp a
  , Just uf <- exprSymFnToUninterpFn @arch f
  , S.BaseBVRepr w <- S.exprType v
  , S.BaseBVRepr iSize <- S.exprType i
  , A.uninterpFnName uf == A.uninterpFnName (A.writeMemUF @arch (S.natValue w))
  , Just S.Refl <- S.testEquality (S.exprType mem) (S.exprType memExpr)
  , Just S.Refl <- S.testEquality (S.exprType i) (S.BaseBVRepr iSize)
  , Just S.Refl <- S.testEquality (iSize) (S.knownNat @(A.RegWidth arch))
  = Just (mem, A.WriteData i v)
isWriteMem memExpr
  | Just (WE.UpdateArray (S.BaseBVRepr w) _ mem (Ctx.Empty Ctx.:> i) v) <- WB.asApp memExpr
  , Just S.Refl <- S.testEquality w (S.knownNat @8)
  = Just (mem, A.WriteData i v)
isWriteMem _ = Nothing

-- | Returns 'Just (mem, ReadData idx)' if the original expression is equal to 'read_mem_w'
isReadMem :: forall arch sym t st fs w.
           ( sym ~ WE.ExprBuilder t st fs
           , A.Architecture arch
           )
         => S.SymBV sym w
         -> Maybe (S.SymExpr sym (A.MemType arch)
                  , A.AccessData sym arch)
isReadMem e@(WE.NonceAppExpr a)
  | WE.FnApp f (Ctx.Empty Ctx.:> mem Ctx.:> i) <- WE.nonceExprApp a
  , Just uf <- exprSymFnToUninterpFn @arch f
  , A.uninterpFnName uf == A.uninterpFnName (A.readMemUF @arch (S.natValue $ S.bvWidth e))
  , Just S.Refl <- S.testEquality (S.exprType i) (S.BaseBVRepr (S.knownNat @(A.RegWidth arch)))
  , Just S.Refl <- S.testEquality (S.exprType mem) (A.memTypeRepr @arch)
  = Just (mem, A.ReadData i)
isReadMem e
  | Just (WE.SelectArray _ mem (Ctx.Empty Ctx.:> i)) <- WB.asApp e
  , Just S.Refl <- S.testEquality (S.bvWidth e) (S.knownNat @8)
  , Just S.Refl <- S.testEquality (S.exprType i) (S.BaseBVRepr (S.knownNat @(A.RegWidth arch)))
  = Just (mem, A.ReadData i)
isReadMem _ | otherwise = Nothing
