{-# LANGUAGE DataKinds #-}
-- | The definitions of the base and manual sets of formulas
--
-- This is the set of definitions shared between PPC32 and PPC64
--
-- base and manual are specified separately so that we can store them in
-- different places (manual definitions are not included in the base set, since
-- we don't want to or cannot learn from them).
module SemMC.Architecture.PPC.Base (
  base,
  manual
  ) where

import SemMC.DSL

gprc :: String
gprc = "Gprc"

base :: [(String, Definition)]
base = runSem $ do
  defineOpcode "ADD4" $ do
    rT <- param "rT" gprc
    rA <- param "rA" gprc
    rB <- param "rB" gprc
    input rA
    input rB
    defLoc (ParamLoc rT) (bvadd (Param rA) (Param rB))
  return ()

manual :: [(String, Definition)]
manual = runSem $ do
  return ()
