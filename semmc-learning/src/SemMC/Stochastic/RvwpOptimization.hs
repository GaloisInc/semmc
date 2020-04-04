{-# LANGUAGE RecordWildCards #-}
-- | Description: Right-value-wrong-place (rvwp) optimization support.
--
-- This module implements the generic parts of the
-- right-value-wrong-place (rvwp) optimization. Each arch needs to
-- implement the class 'RvwpOptimization' in order for the the rvwp
-- optimization to be applied.
module SemMC.Stochastic.RvwpOptimization where

import           Text.Printf

import qualified SemMC.Architecture.Pseudo as AP
import qualified SemMC.Architecture.View as V
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Util as U

----------------------------------------------------------------
-- * Generic part of arch-specific interface to rvwp optimization

-- | Attempt to fix right values in the wrong places by moving them to
-- the right place.
--
-- Assumes that the rvwp optimization applies; the input semantic
-- views are assumed to be the output of
-- 'checkIfRvwpOptimizationApplies'.
fixRvwps :: (U.HasLogCfg, U.HasCallStack, AP.RvwpOptimization arch)
         => [V.SemanticView arch]
         -> Maybe [AP.SynthInstruction arch]
fixRvwps outMasks = case outMasks of
  [] -> error "fixRvwps: numRvwps == 0 so there's nothing to fix."
  [mask] -> do
    case mask of
      V.SemanticView{..} -> do
        let dst = semvView
        let src = head semvCongruentViews
        AP.rvwpMov dst src
  -- Only implementing the simpler case of a single rv in the wp to
  -- start. The general case is more complicated to implement, and
  -- probably requires a swap instruction instead of 'rvwpMov' (if we
  -- don't assume a scratch register).
  _ -> let msg = printf "fixRvwps: numRvwps > 1 is not supported; numRvwps = %i."
                 (length outMasks)
       in U.logTrace U.Error msg $ error msg

----------------------------------------------------------------
-- * Bookkeeping for rvwp optimization

-- | Right value/wrong place (rvwp) info.
--
-- We compute these separately for each test and then combine them
-- using 'combineDeltas'. When deltas are combined, the fields below
-- become cumulative instead of per test.
data RvwpDelta arch = RvwpDelta
  { rdRvwpPlaces :: [Bool]
    -- ^ Booleans indicating whether the corresponding (by position)
    -- congruent view had the right value in the wrong place on the
    -- test.
  , rdWeight :: Double
    -- ^ Weight of candidate on the test.
  } deriving Show

-- | Combine the per test deltas for each out mask into a cumulative
-- delta for each out mask.
combineDeltas :: [[RvwpDelta arch]] -> [RvwpDelta arch]
-- Note that in the @[[RvwpDelta arch]]@ input the outer list is per
-- test and the inner list is per out mask. So, we combine the outer
-- list by combining its inner lists pointwise.
combineDeltas = foldr1 (zipWith combineRvwp)
  where
    r1 `combineRvwp` r2 = RvwpDelta
      { rdWeight = rdWeight r1 + rdWeight r2
      , rdRvwpPlaces = zipWith (&&) (rdRvwpPlaces r1) (rdRvwpPlaces r2) }

-- | Check if the rvwp optimization applies.
--
-- Returns 'Nothing' if the optimization does not apply. When the
-- optimization does apply, returns @Just svs@ for @svs@ the
-- @SemanticView@s for the locations whose right value occurs in the
-- wrong place. The returned semantic views have their
-- @semvCongruentViews@ fields filtered to only include the locations
-- where the right value occurred.
--
-- The argument is suposed to be the result of 'combineDeltas' on all
-- the delta lists for all the tests.
checkIfRvwpOptimizationApplies ::
  [V.SemanticView arch] -> [RvwpDelta arch] -> Maybe [V.SemanticView arch]
checkIfRvwpOptimizationApplies semViews rvwps =
  if rvwpOptimizationApplies
  then Just rvwpSemanticViewsFiltered
  else Nothing
  where
    rvwpOptimizationApplies = allRightValuesAvailable &&
                              someRightValueInTheWrongPlace
    allRightValuesAvailable = or [ rdWeight == 0 || or rdRvwpPlaces
                                 | RvwpDelta{..} <- rvwps ]
    someRightValueInTheWrongPlace = length rvwpSemanticViews > 0
    -- Checking for non-zero weight is only sound when
    -- 'allRightValuesAvailable' is true.
    rvwpSemanticViews = [ (s, r) | (s, r) <- zip semViews rvwps
                                 , rdWeight r /= 0 ]
    filterCongruentViews V.SemanticView{..} rvwp =
      let filteredCongruentViews =
            [ view | (view, isRvwpPlace) <-
                     zip semvCongruentViews (rdRvwpPlaces rvwp)
                   , isRvwpPlace ]
      in V.SemanticView
         { V.semvView = semvView
         , V.semvCongruentViews = filteredCongruentViews
         , V.semvDiff = semvDiff }
    rvwpSemanticViewsFiltered =
      map (uncurry filterCongruentViews) rvwpSemanticViews
