{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TestUtils where

import           Control.Monad.IO.Class ( MonadIO )
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Parameterized.Classes
import           Data.Parameterized.List ( List( (:<) ) )
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.TraversableFC
import           GHC.TypeLits ( Symbol )
import           Hedgehog
import qualified SemMC.Architecture as SA
import qualified SemMC.BoundVar as BV
import           SemMC.Formula.Parser ( operandVarPrefix )
import qualified What4.Expr.Builder as WE
import qualified What4.Interface as WI

import           Prelude


debugPrint :: Monad m => msg -> m ()
-- debugPrint = liftIO $ putStrLn  -- add liftIO to Control.Monad.IO.Class import
debugPrint _ = return ()


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
                       , Eq (WI.BoundVar sym (SA.OperandType arch op))
                       , ShowF (WI.BoundVar sym)
                       , WI.BoundVar sym ~ WE.ExprBoundVar t
                       ) =>
                       Integer
                    -> SL.List (BV.BoundVar sym arch) (op : r)
                    -> SL.List (BV.BoundVar sym arch) (op : r)
                    -> m ()
compareOperandLists ncycle origOprnds resultOprnds = do
  on (===) lengthFC origOprnds resultOprnds
  compareEachOperand ncycle origOprnds resultOprnds

-- | Iterates through the SL.List elements pairwise, comparing each
-- pair of elements for "equality".
compareEachOperand :: ( MonadIO m
                      , MonadTest m
                      , WI.BoundVar sym ~ WE.ExprBoundVar t
                      ) =>
                      Integer
                   -> SL.List (BV.BoundVar sym arch) sh
                   -> SL.List (BV.BoundVar sym arch) sh
                   -> m ()
compareEachOperand _ SL.Nil SL.Nil = return ()
compareEachOperand ncycle (l :< ls) (r :< rs) = do
  compareOperands ncycle l r
  compareEachOperand ncycle ls rs


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
                   Integer
                -> BV.BoundVar sym arch (op :: Symbol)
                -> BV.BoundVar sym arch (op :: Symbol)
                -> m ()
compareOperands ncycles origOprnd resultOprnd = do
  debugPrint $ "bvId=" <> show (WE.bvarId $ BV.unBoundVar origOprnd)
  debugPrint $ "bvLoc=" <> show (WE.bvarLoc $ BV.unBoundVar origOprnd)
  debugPrint $ "bvName=" <> show (WE.bvarName $ BV.unBoundVar origOprnd)
  debugPrint $ "bvType=" <> show (WE.bvarType $ BV.unBoundVar origOprnd)
  -- If the resultOprnd is supplied via a Formula Parse, the Parse
  -- operation will add an 'operandVarPrefix' (e.g. "op_") prefix to
  -- the parsed names to indicate they occurred in the operand
  -- location.  Allow that here if it is present.
  let orignm = bvName origOprnd
      rsltnm = bvName resultOprnd
      bvName = WE.bvarName . BV.unBoundVar
      rsltnm_str = show rsltnm
      newnm = WI.userSymbol $ dropPfx ncycles operandVarPrefix rsltnm_str
      dropPfx n p s = if n == 0 then s
                      else let s' = if p `L.isPrefixOf` s
                                    then drop (length p) s
                                    else s
                           in dropPfx (n-1) p s'
  if (orignm == rsltnm)
    then orignm === rsltnm
    else do nm' <- evalEither newnm
            orignm === nm'
  on compareBaseTypes (WE.bvarType . BV.unBoundVar) origOprnd resultOprnd
  -- cannot compare bvarId: generated via Nonce
  -- cannot compare bvarKind: not exported from What4.Expr.Builder
  -- cannot compare bvarLoc: dependent on sym state
  success


-- | Verifies that two BaseTypes are equal
compareBaseTypes :: ( MonadIO m
                    , MonadTest m
                    ) =>
                    WI.BaseTypeRepr t1 -> WI.BaseTypeRepr t2 -> m ()
compareBaseTypes ty1 ty2 =
  case testEquality ty1 ty2 of
    Just Refl -> success
    Nothing -> show ty1 === show ty2 -- fails, but reveals types
