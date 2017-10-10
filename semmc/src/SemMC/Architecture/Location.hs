{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module SemMC.Architecture.Location (
  Location,
  IsLocation(..)
  ) where

import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S

-- | Methods we want on state variables.
class (OrdF a, TestEquality a, ShowF a) => IsLocation a where
  -- | Try parsing a human representation of a state variable like "r8" into its
  -- representation (and its type).
  readLocation :: String -> Maybe (Some a)
  -- | Given a state variable, return a representation of its type matching its
  -- parameter.
  locationType :: a tp -> BaseTypeRepr tp
  -- | Default value for this location. Typically something like 0.
  defaultLocationExpr :: (S.IsExprBuilder sym) => sym -> a tp -> IO (S.SymExpr sym tp)
  -- | All the locations!
  allLocations :: [Some a]
  -- | Locations that are acceptable to use for registerization.
  --
  -- This isn't just allLocations because vector registers and control registers
  -- are not valid.  Also, on some architectures r0 is special and not allowable
  -- for holding data.
  registerizationLocations :: [Some a]

-- | Represents the different registers, flags, and (eventually) memory a given
-- architecture has.
type family Location (arch :: *) :: BaseType -> *
