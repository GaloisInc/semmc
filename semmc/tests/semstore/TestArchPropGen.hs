{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestArchPropGen
where

import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.BitVector.Sized as BVS
import qualified Data.Foldable as F
import           Data.Int ( Int64 )
import           Data.Kind ( Type )
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe ( catMaybes )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.List ( List( (:<) ) )
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Pair ( Pair(..) )
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableFC
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )
import           Hedgehog
import qualified Hedgehog.Gen as HG
import           Hedgehog.Range
import           HedgehogUtil
import qualified SemMC.Architecture as SA
import qualified SemMC.BoundVar as BV
import qualified SemMC.Formula.Env as FE
import qualified SemMC.Formula.Formula as F
import           SemMC.Util ( fromJust', makeSymbol )
import           TestArch
import           What4.BaseTypes
import qualified What4.Expr.Builder as WE
import qualified What4.Interface as WI
import           What4.Symbol ( systemSymbol )


-- | The Trace provides user information on the generated input
type Trace = String


----------------------------------------------------------------------
-- Location Generators

genIntLocation :: Monad m => GenT m (TestLocation BaseIntegerType)
genIntLocation = TestIntLoc <$>
                 HG.frequency [ (10, return 0)
                              , (10, return 1)
                              , (80, fromInteger . toEnum . fromEnum <$>
                                     HG.integral (linearBounded :: Range Int64))
                              ]
                 -- Ensures that 0 and 1 are present in any reasonably-sized distribution

genBoxLocation :: Monad m => GenT m (TestLocation (BaseBVType 32))
genBoxLocation = TestBoxLoc <$> HG.element [0..3]

----------------------------------------------------------------------
-- Function.Parameter Generators


genIntParameter :: Monad m => GenT m (F.Parameter TestGenArch sh BaseIntegerType)
genIntParameter = HG.choice
                  [
                    -- , F.OperandParameter :: BaseTypeRepr (A.OperandType arch s) -> PL.Index sh s -> Parameter arch sh (A.OperandType arch s)
                    F.LiteralParameter <$> genIntLocation
                    -- , FunctionParameter :: String
                    -- -- The name of the uninterpreted function
                    -- -> WrappedOperand arch sh s
                    -- -- The operand we are calling the function on (this is a newtype so
                    -- -- we don't need an extra typerepr)
                    -- -> BaseTypeRepr tp
                    -- -- The typerepr for the return type of the function
                    -- -> Parameter arch sh tp
                  ]

genBoxParameter :: Monad m => GenT m (F.Parameter TestGenArch sh (BaseBVType 32))
genBoxParameter = HG.choice -- KWQ: more of theses!
                  [
                    -- , F.OperandParameter :: BaseTypeRepr (A.OperandType arch s) -> PL.Index sh s -> Parameter arch sh (A.OperandType arch s)
                    F.LiteralParameter <$> genBoxLocation
                    -- , FunctionParameter :: String
                    -- -- The name of the uninterpreted function
                    -- -> WrappedOperand arch sh s
                    -- -- The operand we are calling the function on (this is a newtype so
                    -- -- we don't need an extra typerepr)
                    -- -> BaseTypeRepr tp
                    -- -- The typerepr for the return type of the function
                    -- -> Parameter arch sh tp
                  ]

genSomeParameter :: Monad m => GenT m (Some (F.Parameter TestGenArch sh))
genSomeParameter =
  HG.choice
  [
    Some <$> genBoxParameter
    -- , Some <$> genNatParameter  -- not supported for formula printing
    -- , Some <$> genIntParameter  -- not supported for formula printing
  ]


----------------------------------------------------------------------
-- SolverSymbol Generators

-- | Generates a solver symbol (e.g. the name of a bound var).  The
-- 'forPrinting' argument is true if this will be used for generating
-- Formula's that are printed (serialized); the serialization process
-- only supports user symbols, not system symbols (in part because a
-- system symbol has an exclamation point which is not a valid
-- s-expression symbol name).
genSolverSymbol :: Monad m => Bool -> GenT m WI.SolverSymbol
genSolverSymbol forPrinting =
  HG.choice $ catMaybes
  [ Just genUserSymbol
  , if forPrinting then Nothing else Just genSystemSymbol
  -- , return WI.emptySymbol   -- can generate eqns with this, but the eqns are invalid!
  ]
  where userSymPrefix = "user__"
        systemSymPrefix = "sys__"
        genUSym = HG.string (linear 1 32) $
                  HG.frequency [ (90, HG.alphaNum)
                               , (10, return '_')
                               ]
        genSSym s = let sp = zip3 (L.inits s) (repeat "!") (L.tails s)
                        join3 (a,b,c) = a <> b <> c
                        s' = map join3 $ tail sp
                    in HG.element s'
        genUserSymbol = (WI.userSymbol . (<>) userSymPrefix <$> genUSym) >>= \case
          -- alphaNum and _, but must not match a known Yices/smtlib keyword
          Left _ -> genUserSymbol  -- could repeat forever...
          Right s -> return s
        genSystemSymbol = do s <- genUSym
                             -- Like a userSymbol but must contain a !
                             -- and systemSymbol throws an error if
                             -- not happy.  Let genUSymbol+userSymbol
                             -- eliminate keywords, and genSSym
                             -- ensures that there is at least one !
                             -- to prevent failure.
                             case WI.userSymbol s of
                               Left _ -> genSystemSymbol -- could repeat forever...
                               Right _ -> systemSymbol . (<>) systemSymPrefix <$> genSSym s

----------------------------------------------------------------------

genBoundIntVar :: ( Monad m
                  , MonadIO m
                  , WI.IsSymExprBuilder sym
                  ) =>
                  sym
               -> Maybe String
               -> GenT m (WI.BoundVar sym BaseIntegerType)
genBoundIntVar sym mbName = do
  s <- bvSymName mbName
  liftIO $ WI.freshBoundVar sym s BaseIntegerRepr

genBoundBV32Var :: ( Monad m
                   , MonadIO m
                   , WI.IsSymExprBuilder sym
                   ) =>
                   sym
                -> Maybe String
                -> GenT m (WI.BoundVar sym (BaseBVType 32))
genBoundBV32Var sym mbName = do
  s <- bvSymName mbName
  liftIO $ WI.freshBoundVar sym s (BaseBVRepr knownNat)

bvSymName :: Monad m => Maybe String -> GenT m WI.SolverSymbol
bvSymName = \case
    Nothing -> genSolverSymbol True
    Just n ->
           case WI.userSymbol n of
             Left e -> error $ "invalid genBoundBV32Var name '" <> n <> "': " <> show e
             Right us -> return us

type TestBoundVar sym = BV.BoundVar sym TestGenArch

-- | OperandPair is a helper to contain both of the parameterized
-- TestBoundVar and Operand values.
data OperandPair sym x = OpP (TestGenOperand x) (TestBoundVar sym x)

operandName :: ( WI.BoundVar sym ~ WE.ExprBoundVar t) =>
               OperandPair sym x -> String
operandName (OpP _ bv) = show $ WE.bvarName $ BV.unBoundVar bv

genBoundVar_IntegerArgFoo
  :: (Monad m, MonadIO m, WI.IsSymExprBuilder sym)
  => sym
  -> GenT m (OperandPair sym "Foo")
genBoundVar_IntegerArgFoo sym =
  OpP FooArg . BV.BoundVar <$> genBoundIntVar sym Nothing

genBoundVar_BV32ArgBar :: Monad m => MonadIO m =>
                          WI.IsSymExprBuilder sym =>
                          sym -> GenT m (OperandPair sym "Bar")
genBoundVar_BV32ArgBar sym = do
  OpP BarArg . BV.BoundVar <$> genBoundBV32Var sym Nothing

genBoundVar_BV32ArgBox :: Monad m => MonadIO m =>
                          WI.IsSymExprBuilder sym =>
                          sym -> GenT m (OperandPair sym "Box")
genBoundVar_BV32ArgBox sym = do
  n <- HG.element [0..3]
  OpP (BoxArg n) . BV.BoundVar <$> genBoundBV32Var sym Nothing

----------------------------------------------------------------------
-- What4.Interface.SymExpr generators

-- First, some helpers for expressions with arguments and tracing
t1 :: String -> String -> String
t1 n a = n <> "(" <> a <> ")"
t2 :: String -> String -> String -> String
t2 n a b = n <> "(" <> a <> ", " <> b <> ")"
t3 :: String -> String -> String -> String -> String
t3 n c a b = n <> "(" <> c <> "; " <> a <> ", " <> b <> ")"
fn1 :: MonadIO m => t1 -> String -> (t1 -> t2 -> IO b) -> (Trace, t2) -> m (Trace, b)
fn1 sym n i = \(l,t) -> do e <- liftIO $ i sym t
                           return (t1 n l, e)
fn2 :: MonadIO m => t1 -> String -> (t1 -> t2 -> t3 -> IO b)
    -> (Trace, t2) -> (Trace, t3) -> m (Trace, b)
fn2 sym n i = \(l,t) -> \(m,u) -> do e <- liftIO $ i sym t u
                                     return (t2 n l m, e)
fn3 :: MonadIO m => t1 -> String -> (t1 -> t2 -> t3 -> t4 -> IO b)
    -> (Trace, t2) -> (Trace, t3) -> (Trace, t4) -> m (Trace, b)
fn3 sym n i = \(l,t) -> \(m,u) -> \(o,v) -> do e <- liftIO $ i sym t u v
                                               return (t3 n l m o, e)


genIntSymExpr :: ( MonadIO m
                 , WI.IsExprBuilder sym
                 ) =>
                 sym -> GenT m (Trace, WI.SymExpr sym BaseIntegerType)
genIntSymExpr sym = do expr <- liftIO $ WI.intLit sym 9
                       return ("(9::int)", expr)


genBoolSymExpr :: ( MonadIO m
                  , WI.IsExprBuilder sym
                  , WI.IsSymExprBuilder sym
                  , SA.Location arch ~ TestLocation
                  ) =>
                  sym
               -> Set.Set (Some (F.Parameter arch sh))
               -> PL.List (BV.BoundVar sym arch) sh
               -> MapF.MapF TestLocation (WI.BoundVar sym)
               -> GenT m (Trace, WI.SymExpr sym BaseBoolType)
genBoolSymExpr sym params opvars litvars =
  do let nonrecursive =
           [
             return ("true", WI.truePred sym)
           , return ("false", WI.falsePred sym)
           ]

     HG.small $ HG.recursive HG.choice nonrecursive
       [ -- recursive
         HG.subtermM
         (genBoolSymExpr sym params opvars litvars)
         (fn1 sym "notPred" WI.notPred)

       , subtermM'
         (genBV32SymExpr sym params opvars litvars)
         (fn1 sym "bvIsNeg" WI.bvIsNeg)

       , HG.subtermM2
         (genBoolSymExpr sym params opvars litvars)
         (genBoolSymExpr sym params opvars litvars)
         (fn2 sym "andPred" WI.andPred)

       , HG.subtermM2
         (genBoolSymExpr sym params opvars litvars)
         (genBoolSymExpr sym params opvars litvars)
         (fn2 sym "orPred" WI.orPred)

       , HG.subtermM2
         (genBoolSymExpr sym params opvars litvars)
         (genBoolSymExpr sym params opvars litvars)
         (fn2 sym "xorPred" WI.xorPred)

       , subtermM2'
         (genBV32SymExpr sym params opvars litvars)
         (genBV32SymExpr sym params opvars litvars)
         (fn2 sym "bvEq" WI.bvEq)

       , subtermM2'
         (genBV32SymExpr sym params opvars litvars)
         (genBV32SymExpr sym params opvars litvars)
         (fn2 sym "bvUlt" WI.bvUlt)

       , subtermM2'
         (genBV32SymExpr sym params opvars litvars)
         (genBV32SymExpr sym params opvars litvars)
         (fn2 sym "bvSlt" WI.bvSlt)

       -- unhandled App in Printer.hs:232
       -- , subtermM2'
       --   ((,) "nat" <$> HG.element [(0::Natural) .. 31 {- BV32 valid bits -}])
       --   (genBV32SymExpr sym params opvars litvars)
       --   (fn2 sym "testBitBV" WI.testBitBV)

       , subtermM3'
         (genBoolSymExpr sym params opvars litvars)
         (genBoolSymExpr sym params opvars litvars)
         (genBoolSymExpr sym params opvars litvars)
         (fn3 sym "itePred" WI.itePred)

       -- n.b. the following Interface expressions are not provided
       -- here because they are combinations of existing expressions:
       --   bvNe, bvUle, bvUge bvUgt bvSgt bvSle bvSge bvIsNonzero

       ]


genBV32SymExpr :: ( MonadIO m
                  , WI.IsExprBuilder sym
                  , WI.IsSymExprBuilder sym
                  , SA.IsLocation (SA.Location arch)
                  , SA.Location arch ~ TestLocation
                  ) =>
                  sym
               -> Set.Set (Some (F.Parameter arch sh))
               -> PL.List (BV.BoundVar sym arch) sh
               -> MapF.MapF TestLocation (WI.BoundVar sym)
               -> GenT m (Trace, WI.SymExpr sym (BaseBVType 32))
genBV32SymExpr sym params opvars litvars = do
  let optional = catMaybes
                 [
                   -- get one of the possible variables (params) that
                   -- can be used as sources in the defs, which are
                   -- declared already as freshBoundVars.
                   case F.length params of
                     0 -> Nothing
                     _ -> Just $ do p <- HG.element (Set.toList params)
                                    e <- paramExprBV32 sym opvars litvars p
                                    return ("param'" <> show p <> "'::bv32", e)
                 ]
      nonrecursive =
        [ -- non-recursive
          do i <- toInteger <$> HG.int32 linearBounded
             expr <- liftIO $ WI.bvLit sym knownNat (BVS.mkBV knownNat i)
             return ("bvLit32=" <> show i, expr)
        ] <> optional

  HG.small $ HG.recursive HG.choice nonrecursive
    [ -- recursive
      HG.subtermM
      (genBV32SymExpr sym params opvars litvars)
      (fn1 sym "bvNeg" WI.bvNeg)

    , HG.subtermM
      (genBV32SymExpr sym params opvars litvars)
      (fn1 sym "bvNotBits" WI.bvNotBits)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvAdd" WI.bvAdd)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvSub" WI.bvSub)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvMul" WI.bvMul)

    -- , HG.subtermM2
    --   (genBV32SymExpr sym params opvars litvars)
    --   (genBV32SymExpr sym params opvars litvars)
    --   (fn2 sym "bvUdiv" WI.bvUdiv)

    -- , HG.subtermM2
    --   (genBV32SymExpr sym params opvars litvars)
    --   (genBV32SymExpr sym params opvars litvars)
    --   (fn2 sym "bvUrem" WI.bvUrem)

    -- , HG.subtermM2
    --   (genBV32SymExpr sym params opvars litvars)
    --   (genBV32SymExpr sym params opvars litvars)
    --   (fn2 sym "bvSdiv" WI.bvSdiv)

    -- , HG.subtermM2
    --   (genBV32SymExpr sym params opvars litvars)
    --   (genBV32SymExpr sym params opvars litvars)
    --   (fn2 sym "bvSrem" WI.bvSrem)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvAndBits" WI.bvAndBits)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvOrBits" WI.bvOrBits)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvXorBits" WI.bvXorBits)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvShl" WI.bvShl)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvLshr" WI.bvLshr)

    , HG.subtermM2
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn2 sym "bvAshr" WI.bvAshr)

    -- unhandled App in Printer.hs:232
    -- , HG.subtermM
    --   (genBV32SymExpr sym params opvars litvars)
    --   (fn1 sym "bvPopcount" WI.bvPopcount)

    -- unhandled App in Printer.hs:232
    -- , HG.subtermM
    --   (genBV32SymExpr sym params opvars litvars)
    --   (fn1 sym "bvCountLeadingZeros" WI.bvCountLeadingZeros)

    -- unhandled App in Printer.hs:232
    -- , HG.subtermM
    --   (genBV32SymExpr sym params opvars litvars)
    --   (fn1 sym "bvCountTrailingZeros" WI.bvCountTrailingZeros)

    -- TODO: bvZext, bvSext, bvTrunc(bvSelect) operations

    , subtermM3'
      (genBoolSymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (genBV32SymExpr sym params opvars litvars)
      (fn3 sym "bvIte" WI.bvIte)
    ]

-- | Generate a symbolic expression for a parameter
--
-- Note that while parameters may be either bitvectors or integers, we always
-- return a bitvector symbolic expression.  To do this, we generate a value of
-- the actual expected type and convert it to a bitvector.
--
-- Question: why? What is this code actually doing?
paramExprBV32 :: ( MonadIO m
                 , WI.IsSymExprBuilder sym
                 , SA.IsLocation (SA.Location arch)
                 , SA.Location arch ~ TestLocation
                 ) =>
                 sym
              -> PL.List (BV.BoundVar sym arch) sh
              -> MapF.MapF TestLocation (WI.BoundVar sym)
              -> Some (F.Parameter arch sh)
              -> m (WI.SymBV sym 32)
paramExprBV32 sym opvars litvars (Some param) =
  case F.paramType param of
    (BaseBVRepr w) ->
      case testEquality w (knownNat :: NatRepr 32) of
        Just Refl -> return $
          case param of
            F.OperandParameter _ idx -> WI.varExpr sym $ BV.unBoundVar $ opvars PL.!! idx
            F.LiteralParameter loc -> WI.varExpr sym $ fromJust' "paramExprBV32.BVRepr32.lookup" $ MapF.lookup loc litvars
        Nothing -> error $ "paramExprBV32 unsupported BVRepr size: " <> show w
    BaseIntegerRepr ->
      case param of
        F.OperandParameter _ idx -> liftIO $ do
          let i = WI.varExpr sym (BV.unBoundVar (opvars PL.!! idx))
          WI.integerToBV sym i (knownNat @32)
        F.LiteralParameter loc -> liftIO $ do
          let i = WI.varExpr sym (fromJust' "paramExprBV32.IntegerRerp.lookup" (MapF.lookup loc litvars))
          WI.integerToBV sym i (knownNat @32)



----------------------------------------------------------------------
-- Formula.ParameterizedFormula generators

  -- KWQ: proxy sym?
genParameterizedFormula :: forall sh sym m t .  -- reordered args to allow TypeApplication of sh first
                           ( Monad m
                           , MonadIO m
                           , WI.IsSymExprBuilder sym
                           , MkOperands (GenT m) sym (PL.List (OperandPair sym)) sh
                           , WI.BoundVar sym ~ WE.ExprBoundVar t
                           ) =>
                           sym
                        -> TestGenOpcode TestGenOperand sh
                        -> GenT m (F.ParameterizedFormula sym TestGenArch (sh :: [Symbol])
                                  , List TestGenOperand sh
                                  , Trace
                                  )
genParameterizedFormula sym _opcode = do
  -- creates operands for 'sh'
  ops <- mkOperand sym
  let operandVars = fmapFC (\(OpP _ bv) -> bv) ops
      actualOperands = fmapFC (\(OpP o _) -> o) ops

  -- Operands could be inputs, outputs, or both, and the same operand
  -- can appear multiple times in the list with different roles in
  -- each.  Examples:
  --
  --     MOVL R0, R1
  --     MOVL (R0)+, R1
  --     ADDL R1, R0, R1
  --
  -- Each actual Operand has a unique name (handled by mkOperand
  -- above) for referencing (e.g. the ADDL might be "ADDL RBase,
  -- RAddend, ROut" but the type of all three is a "GeneralRegister".
  -- A particular instantiation of an operand will have specific
  -- Location bindings for each operand (e.g. RBase and ROut location
  -- is R1, RAddend location is R0"

  -- params are all the parameters useable as inputs for the various
  -- defs that will be generated.  These are the input operands
  -- (OperandParameter) plus any known locations (LiteralParameter).
  -- An operand takes precendence over a location if they match.
  -- There may be locations that are not in the operands.
  locParams <- HG.list (linear 0 10) genSomeParameter
  inputOrOutput <- sequence $ replicate (lengthFC operandVars) HG.bool
  let inpParams = snd $ PL.ifoldr possibleInput (inputOrOutput, []) operandVars
  let params = Set.fromList inpParams <>
               Set.fromList locParams  -- assumes left biasing for <>

  -- Any location (LiteralParameter) in either the operands or the
  -- params.  Output-only locations are not present here.
  literalVars <- locationsForLiteralParams sym params
  (trace, defs) <-
    if F.length params == 0
    then return ("<no params>", MapF.empty)
    else let genExpr = intdefexpr $
                       bv32defexpr $
                       error "unsupported parameter type in generator"
             anElem = do Some p <- HG.element $ F.toList params
                         -- keys should be a (sub-)Set from params
                         genExpr sym p params operandVars literalVars
             joinTraces trc1 trc2 = trc1 <> "  || " <> trc2
         in do g <- HG.list (linear 0 10) anElem
               return ( foldr joinTraces "" $ fst <$> g
                      , MapF.fromList $ snd <$> g
                      )
  return
    ( F.ParameterizedFormula
      { F.pfUses = params              -- Set.Set (Some (Parameter arch sh))
      , F.pfOperandVars = operandVars  -- PL.List (BV.BoundVar sym arch) sh
      , F.pfLiteralVars = literalVars  -- MapF.MapF (L.Location arch) (WI.BoundVar sym)
      , F.pfDefs = defs                -- MapF.MapF (Parameter arch sh) (WI.SymExpr sym)
      }
    , actualOperands
    , trace
    )


possibleInput :: ( WI.IsSymExprBuilder sym
                 , WI.IsExprBuilder sym
                 , WI.BoundVar sym ~ WE.ExprBoundVar t
                 ) =>
                 PL.Index sh tp
              -> BV.BoundVar sym TestGenArch tp
              -> ([Bool], [Some (F.Parameter TestGenArch sh)])
              -> ([Bool], [Some (F.Parameter TestGenArch sh)])
possibleInput idx opBV (isInput:r, inps) =
  let inpParam = F.OperandParameter (WE.bvarType $ BV.unBoundVar opBV) idx
  in if isInput
     then (r, Some inpParam : inps)
     else (r, inps)
possibleInput _ _ a = a


locationsForLiteralParams :: ( F.Foldable t
                             , MonadIO m
                             , WI.IsSymExprBuilder sym
                             ) =>
                             sym
                          -> t (Some (F.Parameter TestGenArch sh))
                          -> GenT m (MapF.MapF TestLocation (WI.BoundVar sym))
locationsForLiteralParams sym params = MapF.fromList <$> F.foldrM appendLitVarPair [] params
  where appendLitVarPair (Some p@(F.LiteralParameter loc)) rs = do
          let nm = Just $ show loc
          e <- case F.paramType p of
                 BaseIntegerRepr -> Pair loc <$> genBoundIntVar sym nm
                 (BaseBVRepr w) ->
                   case testEquality w (knownNat :: NatRepr 32) of
                     Just Refl -> Pair loc <$> genBoundBV32Var sym nm
                     Nothing -> error $ "lFLP unsupported BVRepr size: " <> show w
                 BaseBoolRepr -> error "lFLP unimplemented BaseBoolRepr"
                 BaseRealRepr -> error "lFLP unimplemented BaseRealRepr"
                 (BaseFloatRepr _) -> error "lFLP unimplemented BaseFloatRepr"
                 BaseStringRepr _ -> error "lFLP unimplemented BaseStringRepr"
                 BaseComplexRepr -> error "lFLP unimplemented BaseComplexRepr"
                 (BaseStructRepr _) -> error "lFLP unimplemented BaseStructRepr"
                 (BaseArrayRepr _ _) -> error "lFLP unimplemented BaseArrayRepr"
          atEnd <- HG.bool
          return $ if atEnd then rs <> [e] else e : rs
        appendLitVarPair _ rs = return rs


type GenDefParam      = F.Parameter TestGenArch
type GenDefRes sym sh = (Trace, Pair (GenDefParam sh) (WI.SymExpr sym))
type GenDefDirect m sym arch sh tp =
                  (sym -> GenDefParam sh tp
                       -> Set.Set (Some (F.Parameter arch sh))
                       -> PL.List (BV.BoundVar sym arch) sh
                       -> MapF.MapF TestLocation (WI.BoundVar sym)
                       -> GenT m (GenDefRes sym sh))
type GenDefFunc = forall m sym arch sh tp .
                  ( MonadIO m
                  , WI.IsExprBuilder sym
                  , WI.IsSymExprBuilder sym
                  , SA.IsLocation (SA.Location arch)
                  , SA.Location arch ~ TestLocation
                  ) =>
                  GenDefDirect m sym arch sh tp
               -> GenDefDirect m sym arch sh tp

intdefexpr :: GenDefFunc
intdefexpr next sym p params opvars litvars =
  case testEquality (F.paramType p) BaseIntegerRepr of
    Just Refl -> fmap (Pair p) <$> genIntSymExpr sym
    Nothing -> next sym p params opvars litvars

bv32defexpr :: GenDefFunc
bv32defexpr next sym p params opvars litvars =
  let aBV32 = BaseBVRepr knownNat :: BaseTypeRepr (BaseBVType 32)
  in case testEquality (F.paramType p) aBV32 of
       Just Refl -> fmap (Pair p) <$> genBV32SymExpr sym params opvars litvars
       Nothing -> next sym p params opvars litvars


--------------------------------------------------------------------------------
-- Helpers to generate the operandVars based on the caller-specified
-- ParameterizedFormula 'sh'


class Monad m => MkOperands m sym (f :: k -> Type) (ctx :: k) where
  mkOperand :: sym -> m (f ctx)

instance Monad m => MkOperands m sym (PL.List (OperandPair sym)) '[] where
  mkOperand _ = return PL.Nil

instance ( Monad m
         , MkOperands m sym (OperandPair sym) o
         , MkOperands m sym (PL.List (OperandPair sym)) os
         , WI.BoundVar sym ~ WE.ExprBoundVar t
         ) =>
         MkOperands m sym (PL.List (OperandPair sym)) (o ': os) where
  mkOperand sym =
    do opr <- mkOperand sym
       let oprNames = foldrFC ((:) . operandName) [] opr
       -- operand boundvar symbol names must be unique within a
       -- list. Retry a limited number of times to get a name for the
       -- current element that doesn't match any name already
       -- obtained; the limit is high enough that uniqueness should
       -- always be provided via Hedgehog string generation, but low
       -- enough to ensure termination.
       let attempts = 100 :: Int
       opl <- mkUniqueOperand attempts oprNames
       return $ opl :< opr
    where
      mkUniqueOperand 0 _ =
        error "too many attempts to create a unique operand!"
      mkUniqueOperand n names =
        do o <- mkOperand sym
           let oName = operandName o
           if oName `elem` names
             then mkUniqueOperand (n-1) names
             else return o

instance ( Monad m
         , MonadIO m
         , WI.IsSymExprBuilder sym
         ) =>
         MkOperands (GenT m) sym (OperandPair sym) "Foo" where
  mkOperand = genBoundVar_IntegerArgFoo

instance ( Monad m
         , MonadIO m
         , WI.IsSymExprBuilder sym
         ) =>
         MkOperands (GenT m) sym (OperandPair sym) "Bar" where
  mkOperand = genBoundVar_BV32ArgBar

instance ( Monad m
         , MonadIO m
         , WI.IsSymExprBuilder sym
         ) =>
         MkOperands (GenT m) sym (OperandPair sym) "Box" where
  mkOperand = genBoundVar_BV32ArgBox


----------------------------------------------------------------------
-- Formula Environment sample

testFormulaEnv :: ( MonadIO m
                  , WI.IsSymExprBuilder sym
                  ) =>
                  sym -> m (FE.FormulaEnv sym TestGenArch)
testFormulaEnv sym = do
  tst_isBox3 <- liftIO $ testUF sym
                "tst.isBox3"
                (Some (Ctx.singleton (BaseBVRepr (knownNat :: NatRepr 32))))
                (Some BaseBoolRepr)
  tst_smash <- liftIO $ testUF sym
               "tst.smash"
               (Some (Ctx.Empty
                      Ctx.:> (BaseBVRepr (knownNat :: NatRepr 32))
                      Ctx.:> (BaseBVRepr (knownNat :: NatRepr 32))))
               (Some (BaseBVRepr (knownNat :: NatRepr 32)))
  envUndef <- liftIO $ WI.freshConstant sym (makeSymbol "undefined_bit") knownRepr
  return $ FE.FormulaEnv
    {
      FE.envFunctions = Map.fromList [ tst_isBox3, tst_smash ]
    , FE.envUndefinedBit = envUndef
    }

testUF :: (WI.IsSymExprBuilder sym) =>
          sym
       -> String
       -> Some (Ctx.Assignment BaseTypeRepr)
       -> Some BaseTypeRepr
       -> IO (String, (FE.SomeSome (WI.SymFn sym), Some BaseTypeRepr))
testUF sym name (Some args) retRep@(Some ret) = do
  uf <- FE.SomeSome <$> WI.freshTotalUninterpFn sym (makeSymbol ("uf." ++ name)) args ret
  return (("uf." ++ name), (uf, retRep))
