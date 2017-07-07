{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Synthesis.DivideAndConquer
  ( truncateFormula
  , splits
  , enumerateSplits
  , divideAndConquer
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Maybe ( runMaybeT )
import           Data.Maybe ( fromJust, mapMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import qualified Data.Set as Set
import           Data.Typeable

import qualified Lang.Crucible.Solver.SimpleBuilder as S
import qualified Lang.Crucible.Solver.SimpleBackend as S

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Synthesis.Core
import           SemMC.Synthesis.Template
import           SemMC.Util

truncateFormula :: forall t st arch.
                   (OrdF (Location arch))
                => Formula (S.SimpleBuilder t st) arch
                -> Set.Set (Some (Location arch))
                -> Formula (S.SimpleBuilder t st) arch
truncateFormula form keepLocs =
  let filterDef :: Location arch tp
                -> S.Elt t tp
                -> MapF.MapF (Location arch) (S.Elt t)
                -> MapF.MapF (Location arch) (S.Elt t)
      filterDef loc expr
        | Set.member (Some loc) keepLocs = MapF.insert loc expr
        | otherwise = id
      newDefs = MapF.foldrWithKey filterDef MapF.empty (formDefs form)
      newParamVars = foldrF (MapF.union . extractUsedLocs (formParamVars form)) MapF.empty newDefs
      newUses = Set.fromList $ mapFKeys newParamVars
  in Formula { formUses = newUses
             , formParamVars = newParamVars
             , formDefs = newDefs
             }

makeSplit :: (OrdF (Location arch))
          => Formula (S.SimpleBuilder t st) arch
          -> (Set.Set (Some (Location arch)), Set.Set (Some (Location arch)))
          -> Maybe (Formula (S.SimpleBuilder t st) arch, Formula (S.SimpleBuilder t st) arch)
makeSplit form (locs1, locs2)
  | Set.null locs1 || Set.null locs2 = Nothing
  | otherwise = let form1 = truncateFormula form locs1
                    form2 = truncateFormula form locs2
                in if Set.null (Set.fromList (mapFKeys (formDefs form1)) `Set.intersection` formUses form2)
                   then Just (form1, form2)
                   else Nothing

splits :: (Ord a) => [a] -> [(Set.Set a, Set.Set a)]
splits [] = [(Set.empty, Set.empty)]
splits (x:xs) = [ s' | (left, right) <- splits xs
                     , s' <- [(Set.insert x left, right), (left, Set.insert x right)]]

enumerateSplits :: (OrdF (Location arch))
                => Formula (S.SimpleBuilder t st) arch
                -> [(Formula (S.SimpleBuilder t st) arch, Formula (S.SimpleBuilder t st) arch)]
enumerateSplits form = mapMaybe (makeSplit form)
                     $ splits (mapFKeys (formDefs form))

divideAndConquer :: (Architecture arch,
                     Architecture (TemplatedArch arch),
                     Typeable arch)
                 => SynthesisParams (S.SimpleBackend t) arch
                 -> Formula (S.SimpleBackend t) arch
                 -- ^ Formula to synthesize.
                 -> IO (Maybe [Instruction arch])
divideAndConquer params form =
  sequenceMaybes (map trySplit (enumerateSplits form))
    >>= maybe (synthesizeFormula params form) (return . Just)
  where trySplit (form1, form2) = runMaybeT $ do
          Just insns1 <- liftIO $ divideAndConquer params form1
          Just insns2 <- liftIO $ divideAndConquer params form2
          return (insns1 ++ insns2)
