{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemMC.Synthesis.DivideAndConquer
  ( truncateFormula
  , splits
  , enumerateSplits
  , divideAndConquer
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Maybe ( runMaybeT )
import           Data.Maybe ( mapMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import qualified Data.Set as Set
import           Data.Typeable

import qualified What4.Expr as WE
import qualified What4.Protocol.Online as WPO
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.LLVM.MemModel as LLVM

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Synthesis.Core
import           SemMC.Synthesis.Template
import           SemMC.Util

truncateFormula :: forall sym t st fs arch.
                   (OrdF (Location arch), sym ~ WE.ExprBuilder t st fs)
                => Formula sym arch
                -> Set.Set (Some (Location arch))
                -> Formula sym arch
truncateFormula form keepLocs =
  let filterDef :: Location arch tp
                -> WE.Expr t tp
                -> MapF.MapF (Location arch) (WE.Expr t)
                -> MapF.MapF (Location arch) (WE.Expr t)
      filterDef loc expr
        | Set.member (Some loc) keepLocs = MapF.insert loc expr
        | otherwise = id
      newDefs = MapF.foldrWithKey filterDef MapF.empty (formDefs form)
      newParamVars = foldrF (MapF.union . extractUsedLocs (formParamVars form)) MapF.empty newDefs
  in Formula { formParamVars = newParamVars
             , formDefs = newDefs
             }

makeSplit :: (OrdF (Location arch), sym ~ WE.ExprBuilder t st fs)
          => Formula sym arch
          -> (Set.Set (Some (Location arch)), Set.Set (Some (Location arch)))
          -> Maybe (Formula sym arch, Formula sym arch)
makeSplit form (locs1, locs2)
  | Set.null locs1 || Set.null locs2 = Nothing
  | otherwise = let form1 = truncateFormula form locs1
                    form2 = truncateFormula form locs2
                in if Set.null (formOutputs form1 `Set.intersection` formInputs form2)
                   then Just (form1, form2)
                   else Nothing

splits :: (Ord a) => [a] -> [(Set.Set a, Set.Set a)]
splits [] = [(Set.empty, Set.empty)]
splits (x:xs) = [ s' | (left, right) <- splits xs
                     , s' <- [(Set.insert x left, right), (left, Set.insert x right)]]

enumerateSplits :: (OrdF (Location arch), sym ~ WE.ExprBuilder t st fs)
                => Formula sym arch
                -> [(Formula sym arch, Formula sym arch)]
enumerateSplits form = mapMaybe (makeSplit form)
                     $ splits (MapF.keys (formDefs form))

divideAndConquer :: (Architecture arch,
                     TemplatableOperand arch,
                     ArchRepr arch,
                     Architecture (TemplatedArch arch),
                     Typeable arch,
                     sym ~ WE.ExprBuilder t st fs,
                     CB.IsSymInterface sym,
                     WPO.OnlineSolver solver,
                     ?memOpts :: LLVM.MemOptions
                     )
                 => CBO.OnlineBackend solver t st fs
                 -> SynthesisParams sym arch
                 -> Formula sym arch
                 -- ^ Formula to synthesize.
                 -> IO (Maybe [Instruction arch])
divideAndConquer bak params form =
  sequenceMaybes (map trySplit (enumerateSplits form))
    >>= maybe (synthesizeFormula bak params form) (return . Just)
  where trySplit (form1, form2) = runMaybeT $ do
          Just insns1 <- liftIO $ divideAndConquer bak params form1
          Just insns2 <- liftIO $ divideAndConquer bak params form2
          return (insns1 ++ insns2)
