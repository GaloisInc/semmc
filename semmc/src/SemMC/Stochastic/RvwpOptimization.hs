{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
-- | Description: Right-value-wrong-place (rvwp) optimization support.
--
-- This module implements the generic parts of the
-- right-value-wrong-place (rvwp) optimization. Each arch needs to
-- implement the class 'RvwpOptimization' in order for the the rvwp
-- optimization to be applied.
module SemMC.Stochastic.RvwpOptimization where

import           Text.Printf

import qualified SemMC.Architecture.View as V
import qualified SemMC.Stochastic.Pseudo as P
import qualified SemMC.Util as U

----------------------------------------------------------------
-- * Generic part of arch-specific interface to rvwp optimization

class RvwpOptimization arch where
  -- | The @rvwpMov dst src@ returns an instruction sequence that
  -- moves @src@ to @dst@, if available. This allows us to fix a
  -- candidate with a single right value in the wrong place.
  rvwpMov :: V.View arch n -> V.View arch n -> Maybe [P.SynthInstruction arch]
  -- If we add support for fixing multiple rvs in the wps, then we'll
  -- want to have
  {-
  rvwpSwap :: V.View arch n -> V.View arch n -> Maybe [SynthInstruction arch]
  -}
  -- that does an in place swap, e.g. via xor if the underlying arch
  -- doesn't support it directly.

-- | Attempt to fix right values in the wrong places by moving them to
-- the right place.
--
-- Assumes that the rvwp optimization applies
fixRvwps :: (U.HasLogCfg, U.HasCallStack, RvwpOptimization arch)
         => Int -> [V.SemanticView arch] -> [RvwpDelta arch]
         -> Maybe [P.SynthInstruction arch]
fixRvwps numRvwps outMasks rvwps
  | numRvwps == 0 = error "fixRvwps: numRvwps == 0 so there's nothing to fix."
  -- Only implementing the simpler case of a single rv in the wp to
  -- start. The general case is more complicated to implement, and
  -- probably requires a swap instruction instead of 'rvwpMov' (if we
  -- don't assume a scratch register).
  | numRvwps > 1  =
    let msg = printf "fixRvpws: numRvwps > 1 is not supported; numRvwps = %i." numRvwps
    in U.logTrace U.Error msg $ error msg
  | otherwise = do
      let (mask, rvwp) = head [ (mask', rvwp')
                              | mask' <- outMasks
                              | rvwp' <- rvwps, rdWeight rvwp' /= 0 ]
      case mask of
        V.SemanticView{..} -> do
          let dst = semvView
          let src = head [ view
                         | view <- semvCongruentViews
                         | isRvwpPlace <- rdRvwpPlaces rvwp
                         , isRvwpPlace ]
          rvwpMov dst src

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
  }

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

-- | Check if the unimplemented rvwp optimization applies.
--
-- Returns 'Nothing' if the optimization does not apply. When the
-- optimization does apply, returns @Just k@ for @k@ the number of
-- values that occur in the wrong places.
--
-- The argument is suposed to be the result of 'combineDeltas' on all
-- the delta lists for all the tests.
checkIfRvwpOptimizationApplies :: [RvwpDelta arch] -> Maybe Int
checkIfRvwpOptimizationApplies rvwps =
  if rvwpOptimizationApplies
  then Just numRvwps
  else Nothing
  where
    rvwpOptimizationApplies = allRightValuesAvailable &&
                              someRightValueInTheWrongPlace
    allRightValuesAvailable = or [ rdWeight == 0 || or rdRvwpPlaces
                                 | RvwpDelta{..} <- rvwps ]
    someRightValueInTheWrongPlace = numRvwps > 0
    -- Checking for non-zero weight is only sound when
    -- 'allRightValuesAvailable' is true.
    numRvwps = length [ () | r <- rvwps , rdWeight r /= 0 ]
