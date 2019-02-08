{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import           Data.Bits
import           Data.Proxy
import qualified Numeric as Num
import           Data.Char(intToDigit)
import           Control.Monad(forM_, when)

import qualified SemMC.Formula as F
import qualified What4.BaseTypes as W4B
import qualified What4.Interface as W4I
import qualified What4.Expr.Builder as W4SB
import           Data.Type.List ( toAssignmentFwd, ToContext, ReverseAcc )
import qualified Data.Parameterized.NatRepr as NatRepr
import qualified Data.Parameterized.Nonce as Nonce
import           Data.Parameterized.Some(Some(..))
import           Data.Parameterized.List(List(..))
import qualified Data.Parameterized.List as L
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Classes( (:~:)(..), testEquality, knownRepr )
import qualified Lang.Crucible.Backend.Simple as SB
import qualified SemMC.Architecture.ARM.Opcodes as ARM
import qualified SemMC.Log as Log

import qualified SemMC.Architecture.A32 as A32

import           Test.Tasty
import           Test.Tasty.HUnit

type DefaultSym s = SB.SimpleBackend s (SB.Flags SB.FloatIEEE)

main :: IO ()
main =
  withSym $ \sym -> do
    lib <- mkLib sym
    defaultMain (tests sym lib)

tests :: DefaultSym s
      -> F.Library (DefaultSym s)
      -> TestTree
tests sym lib =
  testGroup "Pseudocode Tests"
    [ predTests sym lib
    ]

predTests :: DefaultSym s
          -> F.Library (DefaultSym s)
          -> TestTree
predTests sym lib =
    testGroup "Predication"
      [ tc (pb 0 0 0 0) nzcvZ         -- EQ
      , tc (pb 0 0 0 1) (not . nzcvZ) -- NE
      , tc (pb 0 0 1 0) nzcvC         -- CS
      , tc (pb 0 0 1 1) (not . nzcvC) -- CC
      , tc (pb 0 1 0 0) nzcvN         -- MI
      , tc (pb 0 1 0 1) (not . nzcvN) -- PL
      , tc (pb 0 1 1 0) nzcvV         -- VS
      , tc (pb 0 1 1 1) (not . nzcvV) -- VC
      , tc (pb 1 0 0 0) hi            -- HI
      , tc (pb 1 0 0 1) (not . hi)    -- LS
      , tc (pb 1 0 1 0) ge            -- GE
      , tc (pb 1 0 1 1) (not . ge)    -- LT
      , tc (pb 1 1 0 0) gt            -- GT
      , tc (pb 1 1 0 1) (not . gt)    -- LE
      , tc (pb 1 1 1 1) (const True)  -- AL
      , tc (pb 1 1 1 0) (const True)  -- AL
      ]
  where
    gt n = ge n && not (nzcvZ n)
    ge n = nzcvN n == nzcvV n
    hi n = nzcvC n && not (nzcvZ n)

    pb :: Integer -> Integer -> Integer -> Integer -> PredBits
    pb b3 b2 b1 b0 = PredBits (b3 /= 0) (b2 /= 0) (b1 /= 0) (b0 /= 0)

    showBin int =
      let str = (Num.showIntAtBase 2 intToDigit int) ""
      in replicate (4 - length str) '0' ++ str -- left pad

    tcName predb =
      "pred=" ++ showBin (predBitsAsInteger predb)

    -- showAll p ex = do
    --   putStrLn ""
    --   putStrLn ""
    --   forM_ allNZCV $ \n -> do
    --     res <- callTestCondition sym lib n p
    --     putStrLn ("    " ++ tcName p ++ " nzcv=" ++ displayNZCV n ++ " -> " ++ show res ++ " [ex " ++ show (ex n) ++ "]")

    tc p expected = testCase (tcName p) $ do
      -- showAll p expected
      forM_ allNZCV $ \n -> do
        let ex = expected n
        res <- callTestCondition sym lib n p
        assertBool ("unexpected result for NZCV " ++ displayNZCV n ++ ": " ++ show res) (ex == res)
        return ()

-- General infrastructure for function formulas
withSym :: (forall s. DefaultSym s -> IO a) -> IO a
withSym action = Nonce.withIONonceGenerator $ \nonceGen -> do
  sym <- SB.newSimpleBackend nonceGen
  W4SB.stopCaching sym
  action sym

mkLib :: DefaultSym s
      -> IO (F.Library (DefaultSym s))
mkLib sym = do
  cfg <- Log.mkNonLogCfg
  Log.withLogCfg cfg $
    F.loadLibrary proxy sym ARM.a32DefinedFunctions
  where
    proxy = Proxy @A32.A32

getLibFun :: String -> F.Library sym -> Some (F.FunctionFormula sym)
getLibFun nm lib =
  let flt (MapF.Pair (F.FunctionRef n _ _) _) = nm == n in
  case filter flt (MapF.toList lib) of
    []                -> error $ "could not find function named " ++ nm ++ " in library"
    (MapF.Pair _ f):_ -> Some f

callLibFun :: W4I.IsSymExprBuilder sym
           => sym
           -> Some (F.FunctionFormula sym)
           -> L.List (W4I.SymExpr sym) tps
           -> L.List (W4B.BaseTypeRepr) tps
           -> W4B.BaseTypeRepr tp
           -> IO (W4I.SymExpr sym tp)
callLibFun sym (Some frm) exprs types retRepr =
  case frm of
    F.FunctionFormula { F.ffArgTypes = argTs
                      , F.ffArgVars = _
                      , F.ffRetType = retT
                      , F.ffDef = def
                      } ->
                        case types `testEquality` argTs of
                          Nothing -> fail $ "function formula arg types don't match [expecting: " <> (show types) <> " got: " <> (show argTs) <> "]"
                          Just Refl -> do
                            let exprsA = toAssignmentFwd exprs
                            _res <- W4I.applySymFn sym def exprsA
                            case retT `testEquality` retRepr of
                              Just Refl -> do
                                return _res
                              Nothing   -> fail "function formula return types don't match!"

-- Specific interesting formula functions

data NZCV = NZCV { nzcvN :: Bool
                 , nzcvZ :: Bool
                 , nzcvC :: Bool
                 , nzcvV :: Bool
                 }

allNZCV :: [NZCV]
allNZCV = do
    n <- tf
    z <- tf
    c <- tf
    v <- tf
    return (NZCV n c z v)
  where tf = [True, False]

displayNZCV :: NZCV -> String
displayNZCV (NZCV n z c v) = show $ zipWith display labels bits
    where
      display l vl = l ++ "=" ++ vl
      bits = show <$> [n,z,c,v]
      labels = ["n", "z", "c", "v"]

data PredBits = PredBits Bool Bool Bool Bool

nzcvBits :: NZCV -> Integer
nzcvBits nzcv = foldl (.|.) 0 masks
  where
    nzcvBit (n, k) = if k nzcv then bit n else 0
    bitDesc = [(31, nzcvN), (30, nzcvZ), (29, nzcvC), (28, nzcvV)]
    masks = nzcvBit <$> bitDesc

predBitsAsInteger :: PredBits -> Integer
predBitsAsInteger (PredBits b3 b2 b1 b0) = bitsToInt [b3, b2, b1, b0]

bitsToInt :: [Bool] -> Integer
bitsToInt bits = foldl (.|.) 0 masks
  where
    bitVal b k = if b then bit k else 0
    masks = zipWith bitVal (reverse bits) [0..]

callTestCondition :: DefaultSym s
                  -> F.Library (DefaultSym s)
                  -> NZCV
                  -> PredBits
                  -> IO Bool
callTestCondition sym lib nzcv predb = do
    let fun = getLibFun "testCondition" lib
        predInt = predBitsAsInteger predb

    predBv <- W4I.bvLit sym (NatRepr.knownNat @4)  predInt
    cpsrBv <- W4I.bvLit sym (NatRepr.knownNat @32) (nzcvBits nzcv)

    let args = predBv :< cpsrBv :< Nil
        types = knownRepr :< knownRepr :< Nil
        ret = W4I.BaseBoolRepr

    result <- callLibFun sym fun args types ret

    case W4I.asConstantPred result of
      Just b -> return b
      Nothing -> fail "expecting bool result from callTestCondition"
