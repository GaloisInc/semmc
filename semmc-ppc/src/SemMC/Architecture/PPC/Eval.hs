{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluators for location functions in formula definitions (e.g., memri_reg)
module SemMC.Architecture.PPC.Eval (
  interpMemriReg,
  interpMemriRegExtractor,
  interpMemriOffsetExtractor,
  interpMemriOffset,
  interpMemrixReg,
  interpMemrixRegExtractor,
  interpMemrixOffsetExtractor,
  interpMemrixOffset,
  interpMemrrBase,
  interpMemrrOffset,
  interpMemrrBaseExtractor,
  interpMemrrOffsetExtractor,
  interpIsR0,
  ) where

import           Data.Int ( Int16 )
import qualified Data.Int.Indexed as I
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as SL
import           What4.BaseTypes
import qualified What4.Expr as S

import qualified Dismantle.PPC as PPC

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L
import qualified SemMC.Formula as F

import           SemMC.Architecture.PPC.OperandComponents ( OperandComponents(..) )
import           SemMC.Architecture.PPC.Location

interpMemriReg :: forall sh s ppc tp sym t st fs
                . ( L.IsLocation (Location ppc)
                  , L.Location ppc ~ Location ppc
                  , sym ~ S.ExprBuilder t st fs
                  , A.OperandComponents ppc sym ~ OperandComponents ppc sym
                  )
               => SL.List (A.AllocatedOperand ppc sym) sh
               -> F.WrappedOperand ppc sh s
               -> BaseTypeRepr tp
               -> Maybe (L.Location ppc tp)
interpMemriReg operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    A.CompoundOperand (OCMemri loc _ _)
      | Just Refl <- testEquality rep (L.locationType loc) -> Just loc
      | otherwise -> error ("Invalid return type for location function 'memri_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemriRegExtractor :: PPC.MemRI -> Maybe PPC.GPR
interpMemriRegExtractor (PPC.MemRI mgpr _) = mgpr

interpMemriOffsetExtractor :: PPC.MemRI -> Int16
interpMemriOffsetExtractor (PPC.MemRI _ off) = off

interpMemrixReg :: forall sh s ppc tp sym t st fs
                 . ( L.IsLocation (Location ppc)
                   , L.Location ppc ~ Location ppc
                   , sym ~ S.ExprBuilder t st fs
                   , A.OperandComponents ppc sym ~ OperandComponents ppc sym
                   )
                => SL.List (A.AllocatedOperand ppc sym) sh
                -> F.WrappedOperand ppc sh s
                -> BaseTypeRepr tp
                -> Maybe (L.Location ppc tp)
interpMemrixReg operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    A.CompoundOperand (OCMemrix loc _ _)
      | Just Refl <- testEquality rep (L.locationType loc) -> Just loc
      | otherwise -> error ("Invalid return type for location function 'memrix_reg' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrixOffset :: forall sh s ppc tp sym
                 . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
                => SL.List (A.AllocatedOperand ppc sym) sh
                -> F.WrappedOperand ppc sh s
                -> BaseTypeRepr tp
                -> Maybe (L.Location ppc tp)
interpMemrixOffset _ _ _ = Nothing

interpMemriOffset :: forall sh s ppc tp sym
                 . (L.IsLocation (Location ppc), L.Location ppc ~ Location ppc)
                => SL.List (A.AllocatedOperand ppc sym) sh
                -> F.WrappedOperand ppc sh s
                -> BaseTypeRepr tp
                -> Maybe (L.Location ppc tp)
interpMemriOffset _ _ _ = Nothing

interpMemrixRegExtractor :: PPC.MemRIX -> Maybe PPC.GPR
interpMemrixRegExtractor (PPC.MemRIX mgpr _) = mgpr

interpMemrixOffsetExtractor :: PPC.MemRIX -> I.I 14
interpMemrixOffsetExtractor (PPC.MemRIX _ off) = off

interpMemrrBase :: forall sh s ppc tp t st fs sym
                . ( L.IsLocation (Location ppc)
                  , L.Location ppc ~ Location ppc
                  , sym ~ S.ExprBuilder t st fs
                  , A.OperandComponents ppc sym ~ OperandComponents ppc sym
                  )
               => SL.List (A.AllocatedOperand ppc sym) sh
               -> F.WrappedOperand ppc sh s
               -> BaseTypeRepr tp
               -> Maybe (L.Location ppc tp)
interpMemrrBase operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    A.CompoundOperand (OCMemrr base _ _ _)
      | Just Refl <- testEquality rep (L.locationType base) -> Just base
      | otherwise -> error ("Invalid return type for location function 'memrr_base' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrrOffset :: forall sh s ppc tp t st fs sym
                   . ( L.IsLocation (Location ppc)
                     , L.Location ppc ~ Location ppc
                     , sym ~ S.ExprBuilder t st fs
                     , A.OperandComponents ppc sym ~ OperandComponents ppc sym
                     )
                  => SL.List (A.AllocatedOperand ppc sym) sh
                  -> F.WrappedOperand ppc sh s
                  -> BaseTypeRepr tp
                  -> Maybe (L.Location ppc tp)
interpMemrrOffset operands (F.WrappedOperand _orep ix) rep =
  case operands SL.!! ix of
    A.CompoundOperand (OCMemrr _ _ offset _)
      | Just Refl <- testEquality rep (L.locationType offset) -> Just offset
      | otherwise -> error ("Invalid return type for location function 'memrr_offset' at index " ++ show ix)
    _ -> error ("Invalid operand type at index " ++ show ix)

interpMemrrBaseExtractor :: PPC.MemRR -> Maybe PPC.GPR
interpMemrrBaseExtractor (PPC.MemRR mgpr _) = mgpr

interpMemrrOffsetExtractor :: PPC.MemRR -> PPC.GPR
interpMemrrOffsetExtractor (PPC.MemRR _ gpr) = gpr

class InterpIsR0 a where
  interpIsR0 :: a -> Bool

instance InterpIsR0 PPC.GPR where
  interpIsR0 (PPC.GPR rnum) = rnum == 0

instance InterpIsR0 (Maybe PPC.GPR) where
  interpIsR0 mr =
    case mr of
      Nothing -> True
      Just r -> interpIsR0 r
