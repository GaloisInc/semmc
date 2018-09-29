{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Architecture.PPC.Base.FP (
    floatingPoint
  , floatingPointLoads
  , floatingPointStores
  , floatingPointCompare
  -- * Primitives
  , fpBinaryDoubleToSingle
  , fpBinarySingleToDouble
  ) where

import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat )
import Data.Parameterized.Some ( Some(..) )

import qualified What4.Interface as S

import SemMC.DSL
import SemMC.Util
import SemMC.Architecture.PPC.Base.Core


withUnRegs
  :: (Location 'TBV -> Location 'TBV -> SemM 'Def ()) -> SemM 'Def ()
withUnRegs action = do
  frT <- param "frT" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  input frB
  action frT frB

withUnRegsFPSCR
  :: String
  -> (Location 'TBV -> Location 'TBV -> SemM 'Def ())
  -> SemM 'Def ()
withUnRegsFPSCR name action = withUnRegs $ \frT frB -> do
  input fpscr
  action frT frB
  setFPSCR $ uf
    (EBV 24)
    "fp.un_op_fpscr"
    [ Some (LitString name)
    , Some (Loc frB)
    , Some (Loc fpscr)
    ]

withBinRegs
  :: (Location 'TBV -> Location 'TBV -> Location 'TBV -> SemM 'Def ())
  -> SemM 'Def ()
withBinRegs action = do
  -- NOTE!!!
  -- Dismantle argument order for binary operations is: frT, frB, frA
  frT <- param "frT" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  frA <- param "frA" fprc (EBV 128)
  input frA
  input frB
  action frT frA frB

withBinRegsFPSCR
  :: String
  -> (Location 'TBV -> Location 'TBV -> Location 'TBV -> SemM 'Def ())
  -> SemM 'Def ()
withBinRegsFPSCR name action = withBinRegs $ \frT frA frB -> do
  input fpscr
  action frT frA frB
  setFPSCR $ uf
    (EBV 24)
    "fp.bin_op_fpscr"
    [ Some (LitString name)
    , Some (Loc frA)
    , Some (Loc frB)
    , Some (Loc fpscr)
    ]

withTernRegs
  :: (  Location 'TBV
     -> Location 'TBV
     -> Location 'TBV
     -> Location 'TBV
     -> SemM 'Def ()
     )
  -> SemM 'Def ()
withTernRegs action = do
  -- NOTE!!!
  -- Dismantle argument order for ternary operations is: frT, frC, frB, frA
  frT <- param "frT" fprc (EBV 128)
  frB <- param "frB" fprc (EBV 128)
  frC <- param "frC" fprc (EBV 128)
  frA <- param "frA" fprc (EBV 128)
  input frA
  input frC
  input frB
  action frT frA frC frB

withTernRegsFPSCR
  :: String
  -> (  Location 'TBV
     -> Location 'TBV
     -> Location 'TBV
     -> Location 'TBV
     -> SemM 'Def ()
     )
  -> SemM 'Def ()
withTernRegsFPSCR name action = withTernRegs $ \frT frA frC frB -> do
  input fpscr
  action frT frA frC frB
  setFPSCR $ uf
    (EBV 24)
    "fp.tern_op_fpscr"
    [ Some (LitString name)
    , Some (Loc frA)
    , Some (Loc frB)
    , Some (Loc frC)
    , Some (Loc fpscr)
    ]

setFPSCR :: Expr 'TBV -> SemM 'Def ()
setFPSCR state = defLoc fpscr $ concat state $ extract 31 24 $ Loc fpscr

roundingMode :: Expr 'TBV
roundingMode = extract 31 30 $ Loc fpscr

fpArithWoFPSCRUnOp :: (Expr 'TDouble -> Expr 'TDouble) -> SemM 'Def ()
fpArithWoFPSCRUnOp op = withUnRegs $ \frT frB -> do
  let res = op (decodeDouble $ Loc frB)
  defLoc frT $ encodeDouble res

fpArithWoFPSCRUnOpS :: (Expr 'TFloat -> Expr 'TFloat) -> SemM 'Def ()
fpArithWoFPSCRUnOpS op = withUnRegs $ \frT frB -> do
  let res = op (decodeSingle $ Loc frB)
  defLoc frT $ encodeSingle res

fpArithUnOp
  :: String -> (Expr 'TBV ->Expr 'TDouble -> Expr 'TDouble) -> SemM 'Def ()
fpArithUnOp name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op roundingMode (decodeDouble $ Loc frB)
  defLoc frT $ encodeDouble res

fpArithUnOpS
  :: String -> (Expr 'TBV ->Expr 'TFloat -> Expr 'TFloat) -> SemM 'Def ()
fpArithUnOpS name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op roundingMode (decodeSingle $ Loc frB)
  defLoc frT $ encodeSingle res

fpArithBinOp
  :: String
  -> (Expr 'TBV -> Expr 'TDouble -> Expr 'TDouble -> Expr 'TDouble)
  -> SemM 'Def ()
fpArithBinOp name op = withBinRegsFPSCR name $ \frT frA frB -> do
  let res = op roundingMode
               (decodeDouble $ Loc frA)
               (decodeDouble $ Loc frB)
  defLoc frT $ encodeDouble res

fpArithBinOpS
  :: String
  -> (Expr 'TBV -> Expr 'TFloat -> Expr 'TFloat -> Expr 'TFloat)
  -> SemM 'Def ()
fpArithBinOpS name op = withBinRegsFPSCR name $ \frT frA frB -> do
  let res = op roundingMode
               (decodeSingle $ Loc frA)
               (decodeSingle $ Loc frB)
  defLoc frT $ encodeSingle res

fpArithTernOp
  :: String
  -> (  Expr 'TBV
     -> Expr 'TDouble
     -> Expr 'TDouble
     -> Expr 'TDouble
     -> Expr 'TDouble
     )
  -> SemM 'Def ()
fpArithTernOp name op = withTernRegsFPSCR name $ \frT frA frC frB -> do
  let res = op roundingMode
               (decodeDouble $ Loc frA)
               (decodeDouble $ Loc frC)
               (decodeDouble $ Loc frB)
  defLoc frT $ encodeDouble res

fpArithTernOpS
  :: String
  -> (  Expr 'TBV
     -> Expr 'TFloat
     -> Expr 'TFloat
     -> Expr 'TFloat
     -> Expr 'TFloat
     )
  -> SemM 'Def ()
fpArithTernOpS name op = withTernRegsFPSCR name $ \frT frA frB frC -> do
  let res = op roundingMode
               (decodeSingle $ Loc frA)
               (decodeSingle $ Loc frC)
               (decodeSingle $ Loc frB)
  defLoc frT $ encodeSingle res

fpConvDoubleToBV64Op
  :: String -> (Expr 'TBV -> Expr 'TDouble -> Expr 'TBV) -> SemM 'Def ()
fpConvDoubleToBV64Op name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op roundingMode (decodeDouble $ Loc frB)
  defLoc frT $ extendDouble res

fpConvDoubleToBV64RTZOp
  :: String -> (Expr 'TBV -> Expr 'TDouble -> Expr 'TBV) -> SemM 'Def ()
fpConvDoubleToBV64RTZOp name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op (LitBV 2 $ roundingModeToBits S.RTZ) (decodeDouble $ Loc frB)
  defLoc frT $ extendDouble res

fpConvDoubleToBV32Op
  :: String -> (Expr 'TBV -> Expr 'TDouble -> Expr 'TBV) -> SemM 'Def ()
fpConvDoubleToBV32Op name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op roundingMode (decodeDouble $ Loc frB)
  defLoc frT $ extendDouble $ concat (undefinedBV 32) res

fpConvDoubleToBV32RTZOp
  :: String -> (Expr 'TBV -> Expr 'TDouble -> Expr 'TBV) -> SemM 'Def ()
fpConvDoubleToBV32RTZOp name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op (LitBV 2 $ roundingModeToBits S.RTZ) (decodeDouble $ Loc frB)
  defLoc frT $ extendDouble $ concat (undefinedBV 32) res

fpConvBV64ToDoubleOp
  :: String -> (Expr 'TBV -> Expr 'TBV -> Expr 'TDouble) -> SemM 'Def ()
fpConvBV64ToDoubleOp name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op roundingMode (extractDouble $ Loc frB)
  defLoc frT $ encodeDouble res

fpConvBV64ToSingleOp
  :: String -> (Expr 'TBV -> Expr 'TBV -> Expr 'TFloat) -> SemM 'Def ()
fpConvBV64ToSingleOp name op = withUnRegsFPSCR name $ \frT frB -> do
  let res = op roundingMode (extractDouble $ Loc frB)
  defLoc frT $ encodeSingle res

fpRoundToIntegerOp :: String -> S.RoundingMode -> SemM 'Def ()
fpRoundToIntegerOp name rm = withUnRegsFPSCR name $ \frT frB -> do
  let res = frti (LitBV 2 $ roundingModeToBits rm) $ decodeDouble $ Loc frB
  defLoc frT $ encodeDouble res

fpRoundToIntegerOpS :: String -> S.RoundingMode -> SemM 'Def ()
fpRoundToIntegerOpS name rm = withUnRegsFPSCR name $ \frT frB -> do
  let res = frtis (LitBV 2 $ roundingModeToBits rm) $ decodeSingle $ Loc frB
  defLoc frT $ encodeSingle res

fp1op :: String -> SemM 'Def ()
fp1op name = do
  (frT, frB) <- xform2f
  input fpscr
  let res = ppcvec1 name (Loc frB) (Loc fpscr)
  defLoc frT (highBits' 128 res)
  defLoc fpscr (lowBits' 32 res)

fp2op :: String -> SemM 'Def ()
fp2op name = do
  (frT, frA, frB) <- aform
  input fpscr
  let res = ppcvec2 name (Loc frA) (Loc frB) (Loc fpscr)
  defLoc frT (highBits' 128 res)
  defLoc fpscr (lowBits' 32 res)

fp3op :: String -> SemM 'Def ()
fp3op name = do
  (frT, frA, frB, frC) <- aform4
  input fpscr
  let res = ppcvec3 name (Loc frA) (Loc frB) (Loc frC) (Loc fpscr)
  defLoc frT (highBits' 128 res)
  defLoc fpscr (lowBits' 32 res)

-- | Extract the double-precision part of a vector register
extractDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extractDouble = highBits128 64

-- | Extend a double-precision value out to 128 bits
extendDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
extendDouble x = concat x (LitBV 64 0x0)

decodeDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TDouble
decodeDouble = fpBinaryToDouble . extractDouble

decodeSingle :: (HasCallStack) => Expr 'TBV -> Expr 'TFloat
decodeSingle = fpDoubleToSingle . decodeDouble

encodeDouble :: (HasCallStack) => Expr 'TDouble -> Expr 'TBV
encodeDouble = extendDouble . fpDoubleToBinary

encodeSingle :: (HasCallStack) => Expr 'TFloat -> Expr 'TBV
encodeSingle = encodeDouble . fpSingleToDouble

fpBinaryDoubleToSingle :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
fpBinaryDoubleToSingle = fpSingleToBinary . fpDoubleToSingle . fpBinaryToDouble

fpBinarySingleToDouble :: (HasCallStack) => Expr 'TBV -> Expr 'TBV
fpBinarySingleToDouble = fpDoubleToBinary . fpSingleToDouble . fpBinaryToSingle


-- | Floating point comparison definitions
fcbits
  :: (HasCallStack, ?bitSize::BitSize)
  => (Expr tp -> Expr tp -> Expr 'TBool)
  -- ^ @<@ check function
  -> (Expr tp -> Expr tp -> Expr 'TBool)
  -- ^ @=@ check function
  -> (Expr tp -> Expr 'TBool)
  -- ^ @nan@ check comparison
  -> Expr tp
  -- ^ The first operand
  -> Expr tp
  -- ^ The second operand
  -> Expr 'TBV
fcbits flt feq fnan opa opb =
  concat (bvPredToBit $ flt opa opb) $
    concat (bvPredToBit $ flt opb opa) $
      concat (bvPredToBit $ feq opa opb) $
      bvPredToBit $ orp (fnan opa) (fnan opb)

floatingPointCompare :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPointCompare = do
  -- For some reason, Dismantle disassembles the FCMPU instruction in two
  -- variants. There really is no difference between the two.

  -- FIXME:
  -- Here, we are either setting or unsetting the FPCC and VXSNAN fields (either 0 or
  -- 1), but we are not unsetting the FX field if VXSNAN gets set to 0. I'm not sure
  -- if this is the correct behavior; something to look into.

  defineOpcodeWithIP "FCMPUD" $ do
    comment "Floating Compare Unordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frB <- param "frB" fprc (EBV 128)
    frA <- param "frA" fprc (EBV 128)
    input bf
    input frA
    input frB
    input cr
    input fpscr
    let c = fcbits
              fltd
              feqd
              fnand
              (decodeDouble $ Loc frA)
              (decodeDouble $ Loc frB)
    defLoc cr $ updateCRField (Loc bf) c
    defLoc fpscr $ bvUpdate (Loc fpscr) 16 c

  defineOpcodeWithIP "FCMPUS" $ do
    comment "Floating Compare Unordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frB <- param "frB" fprc (EBV 128)
    frA <- param "frA" fprc (EBV 128)
    input frA
    input frB
    input cr
    input fpscr
    let c = fcbits
              flts
              feqs
              fnans
              (decodeSingle $ Loc frA)
              (decodeSingle $ Loc frB)
    defLoc cr $ updateCRField (Loc bf) c
    defLoc fpscr $ bvUpdate (Loc fpscr) 16 c

  defineOpcodeWithIP "FCMPOD" $ do
    comment "Floating Compare Ordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frA <- param "frA" fprc (EBV 128)
    frB <- param "frB" fprc (EBV 128)
    input bf
    input frA
    input frB
    input cr
    input fpscr
    let c = fcbits
              fltd
              feqd
              fnand
              (decodeDouble $ Loc frA)
              (decodeDouble $ Loc frB)
    defLoc cr $ updateCRField (Loc bf) c
    let vxvc = bvPredToBit $ orp
                (fnand $ decodeDouble $ Loc frA)
                (fnand $ decodeDouble $ Loc frB)
    defLoc fpscr $ bvUpdate (bvUpdate (Loc fpscr) 16 c) 12 vxvc

  defineOpcodeWithIP "FCMPOS" $ do
    comment "Floating Compare Ordered (X-form)"
    bf  <- param "bf" crrc (EBV 3)
    frA <- param "frA" fprc (EBV 128)
    frB <- param "frB" fprc (EBV 128)
    input frA
    input frB
    input cr
    input fpscr
    let c = fcbits
              flts
              feqs
              fnans
              (decodeSingle $ Loc frA)
              (decodeSingle $ Loc frB)
    defLoc cr $ updateCRField (Loc bf) c
    let vxvc = bvPredToBit $ orp
                (fnans $ decodeSingle $ Loc frA)
                (fnans $ decodeSingle $ Loc frB)
    defLoc fpscr $ bvUpdate (bvUpdate (Loc fpscr) 16 c) 12 vxvc

  -- FIXME: CR is left undefined here
  defineOpcodeWithIP "MFFS" $ do
    comment "Move From FPSCR (X-form, RC=0)"
    frT <- param "FRT" fprc vectorBV
    input fpscr
    defLoc frT (concat (Loc fpscr) (undefinedBV 96))
    forkDefinition "MFFSo" $ do
      comment "Move From FPSCR (X-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MCRFS" $ do
    comment "Move to Condition Register from FPSCR (X-form)"
    _bf <- param "BF" crrc (EBV 3)
    _bfa <- param "BFA" crrc (EBV 3)
    defLoc cr (undefinedBV 32)
    defLoc fpscr (undefinedBV 32)

  defineOpcodeWithIP "MTFSFI" $ do
    comment "Move to FPSCR Field Immediate (X-form, RC=0)"
    _bf <- param "BF" crrc (EBV 3)
    _u <- param "U" "I32imm" (EBV 4)
    _w <- param "W" "I32imm" (EBV 1)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSFIo" $ do
      comment "Move to FPSCR Field Immediate (X-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MTFSF" $ do
    comment "Move to FPSCR Fields (XFL-form, RC=0)"
    _flm <- param "FLM" "I32imm" (EBV 8)
    _l <- param "L" "I32imm" (EBV 1)
    _frB <- param "frB" fprc vectorBV
    _w <- param "W" "I32imm" (EBV 1)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSFo" $ do
      comment "Move to FPSCR Fields (XFL-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MTFSB0" $ do
    comment "Move to FPSCR Bit 0 (X-form, RC=0)"
    _bt <- param "BT" u5imm (EBV 5)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSB0o" $ do
      comment "Move to FPSCR Bit 0 (X-form, RC=1)"
      defLoc cr (undefinedBV 32)

  defineOpcodeWithIP "MTFSB1" $ do
    comment "Move to FPSCR Bit 1 (X-form, RC=0)"
    _bt <- param "BT" u5imm (EBV 5)
    defLoc fpscr (undefinedBV 32)
    forkDefinition "MTFSB1o" $ do
      comment "Move to FPSCR Bit 1 (X-form, RC=1)"
      defLoc cr (undefinedBV 32)


-- | Floating point operation definitions
--
-- FIXME: None of these are defining the status or control registers yet
floatingPoint :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPoint = do
  defineOpcodeWithIP "FADD" $ do
    comment "Floating Add (A-form)"
    fpArithBinOp "FADD" fadd

  defineOpcodeWithIP "FADDS" $ do
    comment "Floating Add Single (A-form)"
    fpArithBinOpS "FADDS" fadds

  defineOpcodeWithIP "FSUB" $ do
    comment "Floating Subtract (A-form)"
    fpArithBinOp "FSUB" fsub

  defineOpcodeWithIP "FSUBS" $ do
    comment "Floating Subtract Single (A-form)"
    fpArithBinOpS "FSUBS" fsubs

  defineOpcodeWithIP "FMUL" $ do
    comment "Floating Multiply (A-form)"
    fpArithBinOp "FMUL" fmul

  defineOpcodeWithIP "FMULS" $ do
    comment "Floating Multiply Single (A-form)"
    fpArithBinOpS "FMULS" fmuls

  defineOpcodeWithIP "FDIV" $ do
    comment "Floating Divide (A-form)"
    fpArithBinOp "FDIV" fdiv

  defineOpcodeWithIP "FDIVS" $ do
    comment "Floating Divide Single (A-form)"
    fpArithBinOpS "FDIVS" fdivs

  defineOpcodeWithIP "FSQRT" $ do
    comment "Floating Square Root (A-form)"
    fpArithUnOp "FSQRT" fsqrt

  defineOpcodeWithIP "FSQRTS" $ do
    comment "Floating Square Root Single (A-form)"
    fpArithUnOpS "FSQRTS" fsqrts

  defineOpcodeWithIP "FRE" $ do
    comment "Floating Reciprocal Estimate (A-form)"
    fp1op "FRE"

  defineOpcodeWithIP "FRES" $ do
    comment "Floating Reciprocal Estimate Single (A-form)"
    fp1op "FRES"

  defineOpcodeWithIP "FRSQRTE" $ do
    comment "Floating Reciprocal Square Root Estimate (A-form)"
    fp1op "FRSQRTE"

  defineOpcodeWithIP "FRSQRTES" $ do
    comment "Floating Reciprocal Square Root Estimate Single (A-form)"
    fp1op "FRSQRTES"

  defineOpcodeWithIP "FSELD" $ do
    comment "Floating-Point Select (A-form)"
    withTernRegs $ \frT frA frC frB ->
      defLoc frT $
        ite (fled (fpBinaryToDouble $ LitBV 64 0) (decodeDouble $ Loc frA))
            (Loc frC)
            (Loc frB)

  defineOpcodeWithIP "FSELS" $ do
    comment "Floating-Point Select Single (A-form)"
    withTernRegs $ \frT frA frC frB ->
      defLoc frT $
        ite (fles (fpBinaryToSingle $ LitBV 32 0) (decodeSingle $ Loc frA))
            (Loc frC)
            (Loc frB)

  defineOpcodeWithIP "FMADD" $ do
    comment "Floating Multiply-Add (A-form)"
    fpArithTernOp "FMADD" ffma

  defineOpcodeWithIP "FMADDS" $ do
    comment "Floating Multiply-Add Single (A-form)"
    fpArithTernOpS "FMADDS" ffmas

  -- NOTE: This functions were previously defined in terms of lower-level operations
  -- like negation and multiply-add, but our new encoding just pushes the opcode
  -- through for consistency.
  defineOpcodeWithIP "FMSUB" $ do
    comment "Floating Multiply-Subtract (A-form)"
    fpArithTernOp "FMSUB" $ \r x y z -> ffma r x y $ fnegd z

  defineOpcodeWithIP "FMSUBS" $ do
    comment "Floating Multiply-Subtract Single (A-form)"
    fpArithTernOpS "FMSUBS" $ \r x y z -> ffmas r x y $ fnegs z

  defineOpcodeWithIP "FNMADD" $ do
    comment "Floating Negative Multiply-Add (A-form)"
    fpArithTernOp "FNMADD" $ \r x y z -> fnegd $ ffma r x y  z

  defineOpcodeWithIP "FNMADDS" $ do
    comment "Floating Negative Multiply-Add Single (A-form)"
    fpArithTernOpS "FNMADDS" $ \r x y z -> fnegs $ ffmas r x y z

  defineOpcodeWithIP "FNMSUB" $ do
    comment "Floating Negative Multiply-Subtract (A-form)"
    fpArithTernOp "FNMSUB" $ \r x y z -> fnegd $ ffma r x y $ fnegd z

  defineOpcodeWithIP "FNMSUBS" $ do
    comment "Floating Negative Multiply-Subtract Single (A-form)"
    fpArithTernOpS "FNMSUBS" $ \r x y z -> fnegs $ ffmas r x y $ fnegs z

  defineOpcodeWithIP "FRSP" $ do
    comment "Floating Round to Single-Precision (X-form)"
    withUnRegsFPSCR "FRSP" $ \frT frB ->
      defLoc frT $
        encodeSingle $ frsp roundingMode $ decodeDouble $ Loc frB

  defineOpcodeWithIP "FCTID" $ do
    comment "Floating Point Convert to Integer Doubleword (X-form)"
    fpConvDoubleToBV64Op "FCTID" fctid

  defineOpcodeWithIP "FCTIDZ" $ do
    comment "Floating Point Convert to Integer Doubleword with Round Towards Zero (X-form)"
    fpConvDoubleToBV64RTZOp "FCTIDZ" fctid

  defineOpcodeWithIP "FCTIDU" $ do
    comment "Floating Point Convert to Integer Doubleword Unsigned (X-form)"
    fpConvDoubleToBV64Op "FCTIDU" fctidu

  defineOpcodeWithIP "FCTIDUZ" $ do
    comment "Floating Point Convert to Integer Doubleword Unsigned with Round Towards Zero (X-form)"
    fpConvDoubleToBV64RTZOp "FCTIDUZ" fctidu

  defineOpcodeWithIP "FCTIW" $ do
    comment "Floating Point Convert to Integer Word (X-form)"
    fpConvDoubleToBV32Op "FCTIW" fctiw

  defineOpcodeWithIP "FCTIWZ" $ do
    comment "Floating Point Convert to Integer Word with Round Towards Zero (X-form)"
    fpConvDoubleToBV32RTZOp "FCTIWZ" fctiw

  defineOpcodeWithIP "FCTIWU" $ do
    comment "Floating Point Convert to Integer Word Unsigned (X-form)"
    fpConvDoubleToBV32Op "FCTIWU" fctiwu

  defineOpcodeWithIP "FCTIWUZ" $ do
    comment "Floating Point Convert to Integer Word Unsigned with Round Towards Zero (X-form)"
    fpConvDoubleToBV32RTZOp "FCTIWUZ" fctiwu

  defineOpcodeWithIP "FCFID" $ do
    comment "Floating Point Convert from Integer Doubleword (X-form)"
    fpConvBV64ToDoubleOp "FCFID" fcfid

  defineOpcodeWithIP "FCFIDU" $ do
    comment "Floating Point Convert from Integer Doubleword Unsigned (X-form)"
    fpConvBV64ToDoubleOp "FCFIDU" fcfidu

  defineOpcodeWithIP "FCFIDS" $ do
    comment "Floating Point Convert from Integer Doubleword Single (X-form)"
    fpConvBV64ToSingleOp "FCFIDS" fcfids

  defineOpcodeWithIP "FCFIDUS" $ do
    comment "Floating Point Convert from Integer Doubleword Unsigned Single (X-form)"
    fpConvBV64ToSingleOp "FCFIDUS" fcfidus

  defineOpcodeWithIP "FRIND" $ do
    comment "Floating Round to Integer Nearest (X-form)"
    fpRoundToIntegerOp "FRIND" S.RNE

  defineOpcodeWithIP "FRINS" $ do
    comment "Floating Round to Integer Nearest Single (X-form)"
    fpRoundToIntegerOpS "FRINS" S.RNE

  defineOpcodeWithIP "FRIPD" $ do
    comment "Floating Round to Integer Plus (X-form)"
    fpRoundToIntegerOp "FRIPD" S.RTP

  defineOpcodeWithIP "FRIPS" $ do
    comment "Floating Round to Integer Plus Single (X-form)"
    fpRoundToIntegerOpS "FRIPS" S.RTP

  defineOpcodeWithIP "FRIZD" $ do
    comment "Floating Round to Integer Toward Zero (X-form)"
    fpRoundToIntegerOp "FRIZD" S.RTZ

  defineOpcodeWithIP "FRIZS" $ do
    comment "Floating Round to Integer Toward Zero Single (X-form)"
    fpRoundToIntegerOpS "FRIZS" S.RTZ

  defineOpcodeWithIP "FRIMD" $ do
    comment "Floating Round to Integer Minus (X-form)"
    fpRoundToIntegerOp "FRIMD" S.RTN

  defineOpcodeWithIP "FRIMS" $ do
    comment "Floating Round to Integer Minus Single (X-form)"
    fpRoundToIntegerOpS "FRIMS" S.RTN

  defineOpcodeWithIP "FMR" $ do
    comment "Floating Move Register (X-form)"
    withUnRegs $ \frT frB ->
      defLoc frT $ Loc frB

  defineOpcodeWithIP "FNEGD" $ do
    comment "Floating Negate (X-form)"
    comment "There is no single-precision form of this because"
    comment "the sign bit is always in the same place (MSB)"
    fpArithWoFPSCRUnOp fnegd

  defineOpcodeWithIP "FNEGS" $ do
    comment "Floating Negate (X-form)"
    comment "There is no single-precision form of this because"
    comment "the sign bit is always in the same place (MSB)"
    fpArithWoFPSCRUnOpS fnegs

  -- See Note [FABS]
  defineOpcodeWithIP "FABSD" $ do
    comment "Floating Absolute Value (X-form)"
    fpArithWoFPSCRUnOp fabsd

  defineOpcodeWithIP "FABSS" $ do
    comment "Floating Absolute Value (X-form)"
    fpArithWoFPSCRUnOpS fabss

  defineOpcodeWithIP "FNABSD" $ do
    comment "Floating Negative Absolute Value (X-form)"
    fpArithWoFPSCRUnOp $ fnegd . fabsd

  defineOpcodeWithIP "FNABSS" $ do
    comment "Floating Negative Absolute Value (X-form)"
    fpArithWoFPSCRUnOpS $ fnegs . fabss

  defineOpcodeWithIP "FCPSGND" $ do
    comment "Floating Copy Sign (X-form)"
    fp2op "FCPSGND"

  defineOpcodeWithIP "FCPSGNS" $ do
    comment "Floating Copy Sign Single (X-form)"
    fp2op "FCPSGNS"


-- | Define a load and double conversion of a single floating-point (D-form)
loadFloat :: (?bitSize :: BitSize)
          => Int
          -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
          -> SemM 'Def ()
loadFloat nBytes convert = do
  frT <- param "frT" fprc (EBV 128)
  memref <- param "memref" memri EMemRef
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext disp)
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))

loadFloatWithUpdate :: (?bitSize :: BitSize)
                   => Int
                   -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                   -> SemM 'Def ()
loadFloatWithUpdate nBytes convert = do
  frT <- param "frT" fprc (EBV 128)
  memref <- param "memref" memri EMemRef
  input memory
  input memref
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let ea = bvadd (Loc rA) (sext disp)
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))
  defLoc rA ea

loadFloatIndexed :: (?bitSize :: BitSize)
                 => Int
                 -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                 -> SemM 'Def ()
loadFloatIndexed nBytes convert = do
  frT <- param "rT" fprc (EBV 128)
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b rB
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))

loadFloatWithUpdateIndexed :: (?bitSize :: BitSize)
                          => Int
                          -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                          -> SemM 'Def ()
loadFloatWithUpdateIndexed nBytes convert = do
  frT <- param "frT" fprc (EBV 128)
  memref <- param "memref" memrr EMemRef
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let ea = bvadd (Loc rA) rB
  defLoc frT (extendDouble (convert (readMem (Loc memory) ea nBytes)))
  defLoc rA ea

floatingPointLoads :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPointLoads = do
  defineOpcodeWithIP "LFS" $ do
    comment "Load Floating-Point Single (D-form)"
    loadFloat 4 fpBinarySingleToDouble
  defineOpcodeWithIP "LFSX" $ do
    comment "Load Floating-Point Single Indexed (X-form)"
    loadFloatIndexed 4 fpBinarySingleToDouble
  defineOpcodeWithIP "LFSU" $ do
    comment "Load Floating-Point Single with Update (D-form)"
    loadFloatWithUpdate 4 fpBinarySingleToDouble
  defineOpcodeWithIP "LFSUX" $ do
    comment "Load Floating-Point Single with Update Indexed (X-form)"
    loadFloatWithUpdateIndexed 4 fpBinarySingleToDouble
  defineOpcodeWithIP "LFD" $ do
    comment "Load Floating-Point Double (D-form)"
    loadFloat 8 id
  defineOpcodeWithIP "LFDX" $ do
    comment "Load Floating-Point Double Indexed (X-form)"
    loadFloatIndexed 8 id
  defineOpcodeWithIP "LFDU" $ do
    comment "Load Floating-Point Double with Update (D-form)"
    loadFloatWithUpdate 8 id
  defineOpcodeWithIP "LFDUX" $ do
    comment "Load Floating-Point Single with Update Indexed (X-form)"
    loadFloatWithUpdateIndexed 8 id

  defineOpcodeWithIP "LFIWAX" $ do
    comment "Load Floating-Point as Integer Word Algebraic Indexed (X-form)"
    loadFloatIndexed 4 (sext' 64)

  defineOpcodeWithIP "LFIWZX" $ do
    comment "Load Floating-Point as Integer Word Zero Indexed (X-form)"
    loadFloatIndexed 4 (zext' 64)
  return ()


storeFloat :: (?bitSize :: BitSize)
           => Int
           -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
           -> SemM 'Def ()
storeFloat nBytes convert = do
  memref <- param "memref" memri EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b (sext disp)
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))

storeFloatWithUpdate :: (?bitSize :: BitSize)
                     => Int
                     -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                     -> SemM 'Def ()
storeFloatWithUpdate nBytes convert = do
  memref <- param "memref" memri EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memriReg memref
  let disp = memriOffset 16 (Loc memref)
  let ea = bvadd (Loc rA) (sext disp)
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))
  defLoc rA ea

storeFloatIndexed :: (?bitSize :: BitSize)
                  => Int
                  -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                  -> SemM 'Def ()
storeFloatIndexed nBytes convert = do
  memref <- param "memref" memrr EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let b = ite (isR0 (Loc rA)) (naturalLitBV 0x0) (Loc rA)
  let ea = bvadd b rB
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))

storeFloatWithUpdateIndexed :: (?bitSize :: BitSize)
                            => Int
                            -> ((?bitSize :: BitSize) => Expr 'TBV -> Expr 'TBV)
                            -> SemM 'Def ()
storeFloatWithUpdateIndexed nBytes convert = do
  memref <- param "memref" memrr EMemRef
  frS <- param "frS" fprc (EBV 128)
  input frS
  input memref
  input memory
  let rA = memrrBaseReg memref
  let rB = memrrOffsetReg (Loc memref)
  let ea = bvadd (Loc rA) rB
  defLoc memory (storeMem (Loc memory) ea nBytes (convert (extractDouble (Loc frS))))
  defLoc rA ea

floatingPointStores :: (?bitSize :: BitSize) => SemM 'Top ()
floatingPointStores = do
  defineOpcodeWithIP "STFS" $ do
    comment "Store Floating-Point Single (D-form)"
    storeFloat 4 fpBinaryDoubleToSingle
  defineOpcodeWithIP "STFSU" $ do
    comment "Store Floating-Point Single with Update (D-form)"
    storeFloatWithUpdate 4 fpBinaryDoubleToSingle
  defineOpcodeWithIP "STFSX" $ do
    comment "Store Floating-Point Single Indexed (X-form)"
    storeFloatIndexed 4 fpBinaryDoubleToSingle
  defineOpcodeWithIP "STFSUX" $ do
    comment "Store Floating-Point Single with Update Indexed (X-form)"
    storeFloatWithUpdateIndexed 4 fpBinaryDoubleToSingle
  defineOpcodeWithIP "STFD" $ do
    comment "Store Floating-Point Double (D-form)"
    storeFloat 8 id
  defineOpcodeWithIP "STFDU" $ do
    comment "Store Floating-Point Double with Update (D-form)"
    storeFloatWithUpdate 8 id
  defineOpcodeWithIP "STFDX" $ do
    comment "Store Floating-Point Double Indexed (X-form)"
    storeFloatIndexed 8 id
  defineOpcodeWithIP "STFDUX" $ do
    comment "Store Floating-Point Double with Update Indexed (X-form)"
    storeFloatWithUpdateIndexed 8 id

  defineOpcodeWithIP "STFIWX" $ do
    comment "Store Floating-Point as Integer Word Indexed (X-form)"
    storeFloatIndexed 4 (lowBits' 32)
  return ()


{- Note [FABS and FNEG]

There is actually only one FABS instruction on PPC: the 64 bit FABS.  The
operation happens to have the same effect on single and double precision values,
so only one instruction is necessary.

The LLVM tablegen data includes a single and double precision version,
presumably to simplify code generation.  We specify semantics here for both to
mirror LLVM.

The same is true of FNEG

-}
