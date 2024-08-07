{-# LANGUAGE TemplateHaskell #-}
module SemMC.Stochastic.Panic (
    P.panic
  , Component(..)
  ) where

import qualified Panic as P

data Component = SemMCLearning
  deriving (Show)

instance P.PanicComponent Component where
  panicComponentName = show
  panicComponentIssues _ = "https://github.com/GaloisInc/semmc/issues"
  panicComponentRevision = $(P.useGitRevision)
