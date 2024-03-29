{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TestUtils where

import           Control.Monad ( forM_, when )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Parameterized.Classes
import           Data.Parameterized.List ( List( (:<) ) )
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableFC
import qualified Data.String as String
import           GHC.TypeLits ( Symbol )
import           Hedgehog
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified SemMC.Architecture as SA
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Equivalence
import qualified SemMC.Formula.Formula as SF
import           SemMC.Formula.Instantiate ( instantiateFormula )
import           SemMC.Formula.Parser ( literalVarPrefix
                                      , operandVarPrefix )
import           System.Directory
import           Test.Tasty ( TestName, TestTree )
import           Test.Tasty.Hedgehog ( testPropertyNamed )
import           TestArch
import qualified What4.Expr.Builder as WE
import qualified What4.Interface as WI
import qualified What4.Protocol.Online as WPO

import           Prelude


debugFile :: FilePath
debugFile = "semstore.log"

debugReset :: IO ()
debugReset = do e <- doesFileExist debugFile
                when e $ removeFile debugFile

debugOut, alwaysPrint :: MonadIO m => String -> m ()
debugOut msg = liftIO $ do appendFile debugFile (msg <> "\n")
                           -- alwaysPrint  -- comment this out to disable printing
                           return ()
alwaysPrint = liftIO . putStrLn


----------------------------------------------------------------------
-- Formulas

-- | Compare two ParameterizedFormulas using a simple backend (which
-- provides limited comparison capability).  The expectation is that
-- the second formula is related to the first (e.g. via a round trip
-- through SemMC.Formula.Printer+SemMC.Formula.Parser) and so they are
-- similar modulo small elements like alpha-renaming and nonce values.
--
-- The ncycles argument specifies the number of round-trips (and
-- therefore the number of adjustments that may need to be accounted
-- for).

compareParameterizedFormulasSimply
  :: ( MonadIO m
     , MonadTest m
     , TestEquality (SA.Location arch)
     , ShowF (SA.Location arch)
     , ShowF (WI.BoundVar sym)
     , WI.BoundVar sym ~ WE.ExprBoundVar t
     , SA.Location arch ~ TestLocation
     ) =>
     sym
  -> Integer
  -> SF.ParameterizedFormula sym arch sh
  -> SF.ParameterizedFormula sym arch sh
  -> m ()
compareParameterizedFormulasSimply sym ncycles origFormula resultFormula =
  do on (===) SF.pfUses origFormula resultFormula

     compareOperandLists sym ncycles
       (SF.pfOperandVars origFormula)
       (SF.pfOperandVars resultFormula)

     compareLiteralVarMaps sym
       (SF.pfLiteralVars origFormula)
       (SF.pfLiteralVars resultFormula)

     on (===) (MapF.size . SF.pfDefs) origFormula resultFormula
     on (===) (MapF.keys . SF.pfDefs) origFormula resultFormula


-- | Compare two ParameterizedFormulas using a symbolic backend to
-- prove formula equivalence.  The expectation is that the second
-- formula is related to the first (e.g. via a round trip through
-- SemMC.Formula.Printer+SemMC.Formula.Parser) and so they are similar
-- modulo small elements like alpha-renaming and nonce values.
--
-- The ncycles argument specifies the number of round-trips (and
-- therefore the number of adjustments that may need to be accounted
-- for).
--
-- This comparison is an extension of the
-- 'compareParameterizedFormulasSimply' with additional functionality.

compareParameterizedFormulasSymbolically
  :: ( MonadIO m
     , MonadTest m
     , TestEquality (SA.Location arch)
     , ShowF (SA.Location arch)
     , ShowF (WI.BoundVar sym)
     , WI.BoundVar sym ~ WE.ExprBoundVar t
     , SA.Architecture arch
     , SA.Location arch ~ TestLocation
     , sym ~ WE.ExprBuilder t st fs
     , CB.IsSymInterface sym
     , WPO.OnlineSolver solver
     ) =>
     CBO.OnlineBackend solver t st fs
  -> PL.List (SA.Operand arch) sh
  -> Integer
  -> SF.ParameterizedFormula sym arch sh
  -> SF.ParameterizedFormula sym arch sh
  -> m ()
compareParameterizedFormulasSymbolically bak operands ncycles origFormula resultFormula =
  do let sym = CB.backendGetSym bak

     on (===) SF.pfUses origFormula resultFormula

     compareOperandLists sym ncycles
       (SF.pfOperandVars origFormula)
       (SF.pfOperandVars resultFormula)

     compareLiteralVarMaps sym
       (SF.pfLiteralVars origFormula)
       (SF.pfLiteralVars resultFormula)

     on (===) (MapF.size . SF.pfDefs) origFormula resultFormula
     on (===) (MapF.keys . SF.pfDefs) origFormula resultFormula
     (_te1, f1) <- liftIO $ instantiateFormula bak origFormula operands
     (_te2, f2) <- liftIO $ instantiateFormula bak resultFormula operands
     -- NOTE: The test architecture doesn't even support memory, so we don't
     -- need to specify any memory locations to test here.  If we do need to
     -- check that, we'll have go carefully set up memory to make the test
     -- possible.
     equiv <- liftIO $ formulasEquivSym bak [] f1 f2
     case equiv of
       Equivalent -> success
       DifferentBehavior _ -> failure
       Timeout -> failure


----------------------------------------------------------------------
-- Operands

-- | Compare two Parameterized Lists of Operands.  The expectation is
-- that these are similar and the comparison is the input to and
-- results from (for example) a round-trip serialization.  Since it's
-- explicitly not possible to compare different Nonce environments,
-- the same Nonce must be used for all operations and thus the
-- comparison is permissive on items like the nonce-ID based on the
-- above expectation.
--
-- The ncycle argument is the number of cycles that have been
-- performed.  This is needed in case each cycle performs a measured
-- modification to the Formula (e.g. the Formula Parser adds "op_"
-- each time it reads a pfOperandVar).
compareOperandLists :: ( MonadIO m
                       , MonadTest m
                       , ShowF (WI.BoundVar sym)
                       , WI.BoundVar sym ~ WE.ExprBoundVar t
                       ) =>
                       sym
                    -> Integer
                    -> PL.List (BV.BoundVar sym arch) sh
                    -> PL.List (BV.BoundVar sym arch) sh
                    -> m ()
compareOperandLists sym ncycle origOprnds resultOprnds = do
  on (===) lengthFC origOprnds resultOprnds
  compareEachOperand sym ncycle origOprnds resultOprnds

-- | Iterates through the PL.List elements pairwise, comparing each
-- pair of elements for "equality".
compareEachOperand :: ( MonadIO m
                      , MonadTest m
                      , WI.BoundVar sym ~ WE.ExprBoundVar t
                      ) =>
                      sym
                   -> Integer
                   -> PL.List (BV.BoundVar sym arch) sh
                   -> PL.List (BV.BoundVar sym arch) sh
                   -> m ()
compareEachOperand _ _ PL.Nil PL.Nil = return ()
compareEachOperand sym ncycle (l :< ls) (r :< rs) = do
  compareOperands sym ncycle l r
  compareEachOperand sym ncycle ls rs


-- | Compares a specific Operand to an Operand that is expected to be
-- "equivalent" to the original.  As noted above, the equivalence may
-- be relative a round-trip of serialization/deserialization, so some
-- aspects (e.g. Nonce and program location) are ignored by this
-- comparison.
--
-- A raw comparison without accomodation for the above expectation
-- would yield something like:
--
--    Failure: ?op_user__a@4:n != ?op_op_user__a@5:n
--
compareOperands :: ( MonadIO m
                   , MonadTest m
                   , Eq (WI.BoundVar sym (SA.OperandType arch op))
                   , ShowF (WI.BoundVar sym)
                   , WI.BoundVar sym ~ WE.ExprBoundVar t
                   ) =>
                   sym
                -> Integer
                -> BV.BoundVar sym arch (op :: Symbol)
                -> BV.BoundVar sym arch (op :: Symbol)
                -> m ()
compareOperands = compareSemMCBVars (Just operandVarPrefix)


----------------------------------------------------------------------
-- BoundVars

compareSemMCBVars :: ( MonadIO m
                     , MonadTest m
                     , Eq (WI.BoundVar sym (SA.OperandType arch op))
                     , ShowF (WI.BoundVar sym)
                     , WI.BoundVar sym ~ WE.ExprBoundVar t
                     ) =>
                     Maybe String
                  -> sym
                  -> Integer
                  -> BV.BoundVar sym arch (op :: Symbol)
                  -> BV.BoundVar sym arch (op :: Symbol)
                  -> m ()
compareSemMCBVars mbPrefix sym ncycles origOprnd resultOprnd =
  compareBVars mbPrefix sym ncycles
  (BV.unBoundVar origOprnd)
  (BV.unBoundVar resultOprnd)

compareBVars :: ( MonadIO m
                , MonadTest m
                , WI.BoundVar sym ~ WE.ExprBoundVar t
                ) =>
                Maybe String
             -> sym
             -> Integer
             -> WI.BoundVar sym ty
             -> WI.BoundVar sym ty
             -> m ()
compareBVars mbPrefix _ ncycles origOprnd resultOprnd = do
  -- debugPrint $ "bvId=" <> show (WE.bvarId origOprnd)
  -- debugPrint $ "bvLoc=" <> show (WE.bvarLoc origOprnd)
  -- debugPrint $ "bvName=" <> show (WE.bvarName origOprnd)
  -- debugPrint $ "bvType=" <> show (WE.bvarType origOprnd)
  -- If the resultOprnd is supplied via a Formula Parse, the Parse
  -- operation will add an 'operandVarPrefix' (e.g. "op_") prefix to
  -- the parsed names to indicate they occurred in the operand
  -- location.  Allow that here if it is present.
  let orignm = bvName origOprnd
      rsltnm = bvName resultOprnd
      bvName = WE.bvarName
      rsltnm_str = show rsltnm
      newnm = WI.userSymbol $ dropPfx ncycles mbPrefix rsltnm_str
      dropPfx _ Nothing s = s
      dropPfx 0 _ s = s
      dropPfx n (Just p) s = let s' = if p `L.isPrefixOf` s
                                      then drop (length p) s
                                      else s
                             in dropPfx (n-1) mbPrefix s'
  if (orignm == rsltnm)
    then orignm === rsltnm
    else do nm' <- evalEither newnm
            orignm === nm'
  on compareBaseTypes WE.bvarType origOprnd resultOprnd
  -- cannot compare bvarId: generated via Nonce
  -- cannot compare bvarKind: not exported from What4.Expr.Builder
  -- cannot compare bvarLoc: dependent on sym state
  success


----------------------------------------------------------------------
-- LiteralVars

compareLiteralVarMaps :: ( MonadIO m
                         , MonadTest m
                         , WI.BoundVar sym ~ WE.ExprBoundVar t
                         ) =>
                         sym
                      -> MapF.MapF TestLocation (WI.BoundVar sym)
                      -> MapF.MapF TestLocation (WI.BoundVar sym)
                      -> m ()
compareLiteralVarMaps sym origMap resultMap = do
  MapF.size origMap === MapF.size resultMap
  MapF.keys origMap === MapF.keys resultMap
  forM_ (MapF.keys origMap) $ \(Some k) -> do
    let origBV = MapF.lookup k origMap
        resultBV = MapF.lookup k resultMap
    let o = case origBV of
              Just o' -> o'
              Nothing -> error "compareLiteralVarMaps: impossible"
    let r = case resultBV of
              Just r' -> r'
              Nothing -> error $ "compareLiteralVarMaps: origMap contains "
                              ++ show k ++ ", but resultMap does not"
    compareBVars (Just literalVarPrefix) sym 1 o r


----------------------------------------------------------------------
-- BaseTypes

-- | Verifies that two BaseTypes are equal
compareBaseTypes :: ( MonadIO m
                    , MonadTest m
                    ) =>
                    WI.BaseTypeRepr t1 -> WI.BaseTypeRepr t2 -> m ()
compareBaseTypes ty1 ty2 =
  case testEquality ty1 ty2 of
    Just Refl -> success
    Nothing -> show ty1 === show ty2 -- fails, but reveals types


----------------------------------------------------------------------
-- BaseTypes


-- | Create a 'T.TestTree' from a Hedgehog 'Property'.
--
-- Note that @tasty-hedgehog@'s version of 'testProperty' has been deprecated
-- in favor of 'testPropertyNamed', whose second argument is intended to
-- represent the name of a top-level 'Property' value to run in the event that
-- the test fails. See https://github.com/qfpl/tasty-hedgehog/pull/42.
--
-- That being said, @semmc@ currently does not define any of the
-- properties that it tests as top-level values. In the
-- meantime, we avoid incurring deprecation warnings by defining our own
-- version of 'testProperty'. The downside to this workaround is that if a
-- property fails, the error message it will produce will likely suggest
-- running ill-formed Haskell code, so users will have to use context clues to
-- determine how to /actually/ reproduce the error.
testProperty :: TestName -> Property -> TestTree
testProperty name = testPropertyNamed name (String.fromString name)
