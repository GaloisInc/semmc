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

-- Types

gprc :: String
gprc = "Gprc"

memrix :: String
memrix = "Memrix"

-- Registers

lnk :: String
lnk = "LNK"

ctr :: String
ctr = "CTR"

memory :: String
memory = "Mem"

-- Form helpers

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

-- Defs

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
  defineOpcode "SLD" $ do
    comment "Shift Left Doubleword (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 6 (Param rB)
    defLoc (ParamLoc rA)  (bvshl (Param rS) n)
  defineOpcode "SLW" $ do
    comment "Shift Left Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 5 (Param rB)
    defLoc (ParamLoc rA) (maskHigh32 (bvshl (Param rS) n))
  defineOpcode "SRD" $ do
    comment "Shift Right Doubleword (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 6 (Param rB)
    defLoc (ParamLoc rA) (bvlshr (Param rS) n)
  defineOpcode "SRW" $ do
    comment "Shift Right Word (X-form)"
    (rA, rS, rB) <- xform3
    let n = lowBits64 5 (Param rB)
    defLoc (ParamLoc rA) (maskHigh32 (bvlshr (Param rS) n))
  return ()

-- | Extract the @n@ low bits of a 64 bit register.
--
-- This is parameterized so that we can easily adjust the index numbering if we
-- have to in order to interface with crucible/macaw.  The bit numbering in PPC
-- is somewhat odd compared to other architectures.
lowBits64 :: Int -> Expr -> Expr
lowBits64 n = extract 63 (63 - n + 1)

-- | Mask out the high 32 bits of a 64 bit bitvector.
--
-- Again, this is factored out so that we can easily adjust the bit indexing if
-- necessary.
maskHigh32 :: Expr -> Expr
maskHigh32 = bvand (LitBV 64 0xFFFF0000)

manual :: Int -> [(String, Definition)]
manual bitSize = runSem $ do
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
  -- defineOpcode "LD" do
  --   rT <- param "rT" gprc
  --   memRef <- param "memRef" memrix
  --   let rA = memrixReg (Param memRef)
  --   let ds = memrixOffset (Param memRef)
  --   let b = ite (isR0 rA) (LitBV bitSize 0) rA
  --   let ea = bvadd b (sext bitSize 16 (concat ds (LitBV 2 0)))
  --   defLoc (ParamLoc rT) (Loc memory)
  return ()

-- | Smart sign extend (extend to the full word width, which is a parameter)
sext :: Int -> Int -> Expr -> Expr
sext fullWidth valWidth =
  signExtend (fullWidth - valWidth)

-- | Extract the base register from a memrix field
memrixReg :: Expr -> Expr
memrixReg = uf "memrix_reg" . (:[])

-- | Extract the offset (DS field) of a memrix memory access
memrixOffset :: Expr -> Expr
memrixOffset = uf "memrix_offset" . (:[])

-- | An uninterpreted function that tests if the argument is zero
isR0 :: Expr -> Expr
isR0 = uf "is_r0" . (:[])
