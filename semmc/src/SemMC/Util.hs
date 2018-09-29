{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
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
  , withRounding
  , roundingModeToBits
    -- * Async
  , asyncLinked
  , withAsyncLinked
    -- * Reexports
  , module SemMC.Log
  ) where

import           Control.Monad.ST ( runST )
import           Control.Monad (foldM)
import qualified Data.HashTable.Class as H
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Context
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr (knownNat)
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Set as Set
import           Text.Printf ( printf )

import           Data.Foldable
import qualified Data.Map as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Ctx
import           Data.Parameterized.Utils.Endian (Endian(..))
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TraversableFC
import qualified Data.Parameterized.Vector as Vector


import qualified UnliftIO as U
import qualified Control.Exception as E

import           What4.BaseTypes
import qualified What4.Expr.Builder as B
import qualified What4.Expr.GroundEval as GE
import qualified What4.Interface as S
import           What4.Symbol ( SolverSymbol, userSymbol, emptySymbol )
import qualified What4.Utils.Hashable as Hash
import qualified What4.Concrete as W

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
                   (S.IsSymExprBuilder sym)
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
groundValToExpr sym (BaseFloatRepr fpp@(FloatingPointPrecisionRepr eb sb)) val
  | LeqProof <- leqTrans (LeqProof @1 @2) (leqProof (knownNat @2) eb)
  , LeqProof <- leqTrans (LeqProof @1 @2) (leqProof (knownNat @2) sb)
  , LeqProof <- leqAddPos eb sb
  = S.floatFromBinary sym fpp =<< S.bvLit sym (addNat eb sb) val
groundValToExpr sym BaseComplexRepr val = S.mkComplexLit sym val
groundValToExpr sym (BaseArrayRepr idxTp elemTp) (GE.ArrayConcrete base m) = do
  base' <- groundValToExpr sym elemTp base
  entries <- Hash.mkMap <$> traverse (groundValToExpr sym elemTp) m
  S.arrayFromMap sym idxTp entries base'

-- Groundvaltoexpr _ (BaseArrayRepr _ _) (GE.ArrayMapping _) = error "groundValToExpr: ArrayMapping not handled"
groundValToExpr sym (BaseArrayRepr idxs r) (GE.ArrayMapping f) = do
    -- Construct a default value to populate the array
    defaultValue <- f $ defaultInput idxs
    defaultExpr  <- groundValToExpr sym r defaultValue
    let defaultConcrete = groundToConcrete r defaultValue

    let indexVals = allGroundAssign idxs
        indexLits = fmap (mkIndex idxs) indexVals
        indexConcrete = fmap (fmapFC indexToConcrete) indexLits

    resultVals <- mapM f indexVals
    let resultConcrete = groundToConcrete r <$> resultVals

    let arrayMap' = Map.fromList $ zip indexLits resultConcrete
        concreteMap = W.ConcreteArray idxs defaultConcrete (Map.mapKeys (fmapFC indexToConcrete) arrayMap')

    arr <- S.concreteToSym sym concreteMap

    case S.asConcrete arr of
      Just (W.ConcreteArray _ _ _) -> putStrLn "Created a concrete array"
      Nothing                      -> putStrLn "Created a symbolic array"
    return arr



--    sFun <- inlineDefineFun' sym (makeSymbol "arr") idxs (arrayMappingToExpr sym idxs r f)
--    S.arrayFromFn sym sFun

  where
    defaultInput :: forall idx. Assignment BaseTypeRepr idx -> Assignment GE.GroundValueWrapper idx
    defaultInput Empty = Empty
    defaultInput (idxs' :> r') = defaultInput idxs' :> GE.GVW (GE.defaultValueForType r')


    mkIndex :: Assignment BaseTypeRepr idx'
            -> Assignment GE.GroundValueWrapper idx'
            -> Assignment S.IndexLit idx'
    mkIndex Empty Empty = Empty
    mkIndex (idx' :> BaseBVRepr n) (vals :> GE.GVW i) = mkIndex idx' vals :> S.BVIndexLit n i
    mkIndex (idx' :> BaseNatRepr)  (vals :> GE.GVW i) = mkIndex idx' vals :> S.NatIndexLit i
    mkIndex _                      _                  = error "Error creating index literal into an array: unsupported types"


{-
    listToArray :: Assignment BaseTypeRepr idx'
                -> [(Assignment S.IndexLit idx',S.SymExpr sym xs)]
                -> IO (S.SymArray sym idx' xs)
    listToArray _ [] = error "Cannot construct an empty array"
    listToArray Empty _ = error "Cannot construct an empty array"
    listToArray idx@(_ :> _) [(i,e)] = S.constantArray sym idx e
-}


groundValToExpr _ (BaseStructRepr _) _ = error "groundValToExpr: struct type isn't handled yet"
groundValToExpr _ BaseStringRepr     _ = error "groundValToExpr: string base types are not supported yet"


showGroundValue :: BaseTypeRepr b -> GE.GroundValue b -> String
showGroundValue BaseBoolRepr b = show b
showGroundValue BaseNatRepr n = show n
showGroundValue BaseIntegerRepr i = show i
showGroundValue BaseRealRepr r = show r
showGroundValue (BaseBVRepr w) i = show i
showGroundValue (BaseFloatRepr fpp) f = show f
showGroundValue BaseComplexRepr i = show i
showGroundValue BaseStringRepr s = show s
showGroundValue (BaseArrayRepr idx b) i = "No show instance for BaseArrayRepr"
showGroundValue (BaseStructRepr ctx) i = "No show instance for BaseStructType"

showGroundValues :: Assignment BaseTypeRepr idx -> Assignment GE.GroundValueWrapper idx -> String
showGroundValues Empty Empty = "Empty"
showGroundValues (rs :> r) (bs :> GE.GVW b) = showGroundValues rs bs ++ " :> " ++ showGroundValue r b


arrayMappingToExpr :: forall sym idx b.
                      S.IsSymExprBuilder sym
                   => sym
                   -> Assignment BaseTypeRepr idx
                   -> BaseTypeRepr b
                   -> (Assignment GE.GroundValueWrapper idx -> IO (GE.GroundValue b))
                   -> Assignment (S.SymExpr sym) idx -> IO (S.SymExpr sym b)
arrayMappingToExpr sym idxRepr bRepr f idx = arrayMappingToExpr' (allGroundAssign idxRepr)
  where
    arrayMappingToExpr' :: [Assignment GE.GroundValueWrapper idx] -> IO (S.SymExpr sym b)
    arrayMappingToExpr' []  = error "No candidate ground values found when converting ArrayMapping"
    arrayMappingToExpr' [g] = arrayAsExp g
    arrayMappingToExpr' (g : gs) = do eq <- isEqAssignment idxRepr idx g
                                      ifthenelse eq (arrayAsExp g)
                                                    (arrayMappingToExpr' gs)

    arrayAsExp :: Assignment GE.GroundValueWrapper idx -> IO (S.SymExpr sym b)
    arrayAsExp g = do -- putStrLn $ "Looking up " ++ showGroundValues idxRepr g
                      v <- f g
                      -- putStrLn $ "Got base value " ++ showGroundValue bRepr v
                      e <- groundValToExpr sym bRepr v
                      -- putStrLn $ "Got expression "
                      return e

    isEqAssignment :: forall idx'. 
                      Assignment BaseTypeRepr idx'
                   -> Assignment (S.SymExpr sym) idx'
                   -> Assignment GE.GroundValueWrapper idx'
                   -> IO (S.Pred sym)
    isEqAssignment Empty Empty Empty = return $ S.truePred sym
    isEqAssignment (rs :> r) (es :> e) (gs :> GE.GVW g) = do 
      bs <- isEqAssignment rs es gs
      e' <- groundValToExpr sym r g
      b  <- S.isEq sym e e'
      S.andPred sym bs b

    -- The What4 interface does not allow lazy evaluation of monadic results in
    -- an if/then/else statement unless the condition (in this case eq) is a
    -- constant.
    ifthenelse :: S.Pred sym -> IO (S.SymExpr sym b) -> IO (S.SymExpr sym b)
               -> IO (S.SymExpr sym b)
    ifthenelse b tIO fIO = case S.asConstantPred b of
      Just True  -> tIO
      Just False -> fIO
      Nothing    -> do t' <- tIO
                       f' <- fIO
                       S.baseTypeIte sym b t' f'




allGroundAssign :: Assignment BaseTypeRepr idx -> [Assignment GE.GroundValueWrapper idx]
allGroundAssign Empty       = [Empty]
allGroundAssign (idx :> r) = do vs <- allGroundAssign idx
                                v  <- GE.GVW <$> allGroundVals r
                                return $ vs :> v

allConcreteFC :: Assignment BaseTypeRepr idx -> [Assignment W.ConcreteVal idx]
allConcreteFC Empty       = [Empty]
allConcreteFC (idx :> r) = do vs <- allConcreteFC idx
                              v  <- groundToConcrete r <$> allGroundVals r
                              return $ vs :> v

groundToConcrete :: BaseTypeRepr b -> GE.GroundValue b -> W.ConcreteVal b
groundToConcrete BaseBoolRepr b = W.ConcreteBool b
groundToConcrete (BaseBVRepr n) b = W.ConcreteBV n b
groundToConcrete BaseNatRepr n = W.ConcreteNat n
groundToConcrete BaseIntegerRepr n = W.ConcreteInteger n
groundToConcrete BaseRealRepr r = W.ConcreteReal r
groundToConcrete (BaseFloatRepr f) x = error "Error converting a float to a concrete value"
groundToConcrete BaseStringRepr s = W.ConcreteString s
groundToConcrete BaseComplexRepr c = W.ConcreteComplex c
groundToConcrete (BaseStructRepr ctx) vs = W.ConcreteStruct $ groundToConcreteFC ctx vs
groundToConcrete (BaseArrayRepr ctx b) (GE.ArrayConcrete v map) =
    W.ConcreteArray ctx (groundToConcrete b v) $ Map.mapKeys (fmapFC indexToConcrete) $ 
                                                 Map.map (groundToConcrete b) map

indexToConcrete :: S.IndexLit b -> W.ConcreteVal b
indexToConcrete (S.NatIndexLit n) = W.ConcreteNat n
indexToConcrete (S.BVIndexLit n i) = W.ConcreteBV n i

groundToConcreteFC :: Assignment BaseTypeRepr ctx 
                   -> Assignment GE.GroundValueWrapper ctx 
                   -> Assignment W.ConcreteVal ctx
groundToConcreteFC = undefined


allGroundVals :: BaseTypeRepr b -> [GE.GroundValue b]
allGroundVals BaseBoolRepr = [True,False]
allGroundVals (BaseBVRepr w) = [0..(natValue w-1)]
allGroundVals r              = error $ "Cannot produce ground values for the infinite type" ++ show r



-- | Support for converting symbolic expressions to ground values. 
exprToGroundVal :: forall sym tp.
                   S.IsSymExprBuilder sym
                => BaseTypeRepr tp
                -> S.SymExpr sym tp
                -> Maybe (GE.GroundValue tp)
exprToGroundVal BaseBoolRepr                 e = S.asConstantPred e
exprToGroundVal (BaseBVRepr w)               e = S.asSignedBV e
exprToGroundVal BaseNatRepr                  e = S.asNat e 
exprToGroundVal BaseIntegerRepr              e = S.asInteger e
exprToGroundVal BaseRealRepr                 e = Nothing
exprToGroundVal (BaseFloatRepr fpp)          e = Nothing
exprToGroundVal BaseComplexRepr              e = S.asComplex e
exprToGroundVal (BaseArrayRepr idxTp elemTp) e = Nothing
exprToGroundVal (BaseStructRepr r)           e = do
    v <- S.asStruct e
    traverseFC (\e' -> GE.GVW <$> exprToGroundVal @sym (S.exprType e') e') v
exprToGroundVal BaseStringRepr               e = S.asString e

-- | Given a list of symbolic expressions, map 'exprToGroundVal' over them.
mapAssignment :: forall sym ctx.
                 (S.IsSymExprBuilder sym)
              => sym
              -> Assignment BaseTypeRepr ctx
              -> Assignment (S.SymExpr sym) ctx
              -> IO (Assignment GE.GroundValueWrapper ctx)
mapAssignment _ _ Empty = return Empty
mapAssignment sym (reps :> r) (assn :> e) = do 
    assn' <- mapAssignment sym reps assn
    v <- maybe (myError)
               return 
               (GE.GVW <$> exprToGroundVal @sym r e)
    return $ assn' :> v
  where
    myError = error $ "Could not extract ground value from symbolic expression of type" 
                      ++ show r

inlineDefineFun' :: S.IsSymExprBuilder sym
                 => sym
                 -> SolverSymbol
                 -> Assignment BaseTypeRepr args
                 -> (Assignment (S.SymExpr sym) args 
                     -> IO (S.SymExpr sym ret))
                 -> IO (S.SymFn sym args ret)
inlineDefineFun' sym nm tps f = do
    -- Create bound variables for function
    vars <- traverseFC (S.freshBoundVar sym emptySymbol) tps
    -- Call operation on expressions created from variables
    r <- f (fmapFC (S.varExpr sym) vars)
    -- Define function
    S.definedFn sym nm vars r (\_ -> False)



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

withRounding
  :: forall sym tp
   . S.IsExprBuilder sym
  => sym
  -> S.SymBV sym 2
  -> (S.RoundingMode -> IO (S.SymExpr sym tp))
  -> IO (S.SymExpr sym tp)
withRounding sym r action = do
  cRNE <- roundingCond S.RNE
  cRTZ <- roundingCond S.RTZ
  cRTP <- roundingCond S.RTP
  S.iteM S.baseTypeIte sym cRNE
    (action S.RNE) $
    S.iteM S.baseTypeIte sym cRTZ
      (action S.RTZ) $
      S.iteM S.baseTypeIte sym cRTP (action S.RTP) (action S.RTN)
 where
  roundingCond :: S.RoundingMode -> IO (S.Pred sym)
  roundingCond rm =
    S.bvEq sym r =<< S.bvLit sym knownNat (roundingModeToBits rm)

roundingModeToBits :: S.RoundingMode -> Integer
roundingModeToBits = \case
  S.RNE -> 0
  S.RTZ -> 1
  S.RTP -> 2
  S.RTN -> 3
  S.RNA -> error $ "unsupported rounding mode: " ++ show S.RNA
