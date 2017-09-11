{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Constraints (
  weakenConstraints
  ) where

import qualified Data.Constraint as C
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Witness ( Witness(..) )

-- | Given a list of values with a more specific constraint, weaken the
-- constraint to a less specific constraint (using a witness implication
-- provided as an argument).
weakenConstraints :: (forall sh . c1 sh C.:- c2 sh)
                  -> [Some (Witness c1 a)]
                  -> [Some (Witness c2 a)]
weakenConstraints impl wvals =
  [ Some (Witness a) C.\\ impl @sh
  | Some ((Witness a) :: Witness c a sh) <- wvals
  ]
