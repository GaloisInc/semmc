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

lnk :: String
lnk = "LNK"

ctr :: String
ctr = "CTR"

xoform3 :: SemM 'Def (Parameter, Parameter, Parameter)
xoform3 = do
  rT <- param "rT" gprc
  rA <- param "rA" gprc
  rB <- param "rB" gprc
  input rA
  input rB
  return (rT, rA, rB)

xform3 :: SemM 'Def (Parameter, Parameter, Parameter)
xform3 = do
  rA <- param "rA" gprc
  rS <- param "rS" gprc
  rB <- param "rB" gprc
  input rS
  input rB
  return (rA, rS, rB)

base :: [(String, Definition)]
base = runSem $ do
  defineOpcode "ADD4" $ do
    (rT, rA, rB) <- xoform3
    defLoc (ParamLoc rT) (bvadd (Param rA) (Param rB))
  defineOpcode "SUBF" $ do
    (rT, rA, rB) <- xoform3
    defLoc (ParamLoc rT) (bvsub (Param rB) (Param rA))
  defineOpcode "XOR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvxor (Param rS) (Param rB))
  defineOpcode "OR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvor (Param rS) (Param rB))
  defineOpcode "AND" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvand (Param rS) (Param rB))
  defineOpcode "NAND" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvand (Param rS) (Param rB)))
  defineOpcode "NOR" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvor (Param rS) (Param rB)))
  defineOpcode "EQV" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvnot (bvxor (Param rS) (Param rB)))
  defineOpcode "ANDC" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvand (Param rS) (bvnot (Param rB)))
  defineOpcode "ORC" $ do
    (rA, rS, rB) <- xform3
    defLoc (ParamLoc rA) (bvor (Param rS) (bvnot (Param rB)))
  return ()

manual :: [(String, Definition)]
manual = runSem $ do
  defineOpcode "MTLR" $ do
    rA <- param "rA" gprc
    input rA
    defLoc (LiteralLoc lnk) (Param rA)
  defineOpcode "MFLR" $ do
    rA <- param "rA" gprc
    defLoc (ParamLoc rA) (Loc lnk)
  defineOpcode "MTCTR" $ do
    rA <- param "rA" gprc
    input rA
    defLoc (LiteralLoc ctr) (Param rA)
  defineOpcode "MFCTR" $ do
    rA <- param "rA" gprc
    defLoc (ParamLoc rA) (Loc ctr)
  return ()
