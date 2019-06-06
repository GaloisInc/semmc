{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestArchPropGen
where

import           Control.Monad.IO.Class
import           Data.Int ( Int64 )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as SL
import           Data.Parameterized.Nonce ( newIONonceGenerator )
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           GHC.TypeLits ( Symbol )
import           Hedgehog
import qualified Hedgehog.Gen as HG
import           Hedgehog.Range
import           Numeric.Natural
import qualified SemMC.Architecture as SA
import qualified SemMC.BoundVar as BV
import qualified SemMC.Formula.Formula as F
import           TestArch
import           What4.BaseTypes
import qualified What4.Expr.Builder as WB
import qualified What4.Interface as WI


genNat :: Monad m => GenT m Natural
genNat = HG.frequency [ (5, return 0)
                      , (5, return 1)
                      , (90, toEnum . abs <$> HG.int (linearBounded :: Range Int))
                      ]
         -- Ensures that 0 and 1 are present in any reasonably-sized distribution

----------------------------------------------------------------------
-- Location Generators

genNatLocation :: Monad m => GenT m (TestLocation BaseNatType)
genNatLocation = TestNatLoc <$> genNat

genIntLocation :: Monad m => GenT m (TestLocation BaseIntegerType)
genIntLocation = TestIntLoc <$>
                 HG.frequency [ (5, return 0)
                              , (5, return 1)
                              , (90, fromInteger . toEnum . fromEnum <$>
                                     HG.integral (linearBounded :: Range Int64))
                              ]
                 -- Ensures that 0 and 1 are present in any reasonably-sized distribution

----------------------------------------------------------------------
-- Function.Parameter Generators

genNatParameter :: Monad m => GenT m (F.Parameter TestGenArch sh BaseNatType)
genNatParameter = HG.choice
                  [
                    -- , F.OperandParameter :: BaseTypeRepr (A.OperandType arch s) -> SL.Index sh s -> Parameter arch sh (A.OperandType arch s)
                    F.LiteralParameter <$> genNatLocation
                    -- , FunctionParameter :: String
                    -- -- The name of the uninterpreted function
                    -- -> WrappedOperand arch sh s
                    -- -- The operand we are calling the function on (this is a newtype so
                    -- -- we don't need an extra typerepr)
                    -- -> BaseTypeRepr tp
                    -- -- The typerepr for the return type of the function
                    -- -> Parameter arch sh tp
                  ]

genIntParameter :: Monad m => GenT m (F.Parameter TestGenArch sh BaseIntegerType)
genIntParameter = HG.choice
                  [
                    -- , F.OperandParameter :: BaseTypeRepr (A.OperandType arch s) -> SL.Index sh s -> Parameter arch sh (A.OperandType arch s)
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

genSomeParameter :: Monad m => GenT m (Some (F.Parameter TestGenArch sh))
genSomeParameter =
  HG.choice
  [ Some <$> genNatParameter
  , Some <$> genIntParameter
  ]


----------------------------------------------------------------------

-- data TestSymLocation :: BaseType -> Type where
--   TestSymLocation :: TestSymBackend
data TestSymExpr (ty :: BaseType) = TestSymExpr
  deriving Show

instance ShowF TestSymExpr

data TestBoundVar (th :: BaseType) = TestBoundVar
  deriving Show

instance ShowF TestBoundVar

data TestSymbolicBackend = TestSymbolicBackend
 deriving Show

type instance WI.SymExpr TestSymbolicBackend = TestSymExpr
type instance WI.BoundVar TestSymbolicBackend = TestBoundVar

type TestOperand = BV.BoundVar TestSymbolicBackend TestGenArch


----------------------------------------------------------------------
-- What4.Interface.BoundVar generators

  -- KWQ: proxy sym?
genBoundNatVar :: Monad m => GenT m (WI.BoundVar TestSymbolicBackend BaseNatType)
genBoundNatVar = return TestBoundVar

genBoundIntVar :: Monad m => GenT m (WI.BoundVar TestSymbolicBackend BaseIntegerType)
genBoundIntVar = return TestBoundVar

genBoundVar_NatArgFoo :: Monad m => GenT m (BV.BoundVar TestSymbolicBackend TestGenArch "NatArg:Foo")
genBoundVar_NatArgFoo = BV.BoundVar <$> genBoundNatVar

----------------------------------------------------------------------
-- Formula.ParameterizedFormula generators

  -- KWQ: proxy sym?
genParameterizedFormula :: forall sh sym m .  -- reordered args to allow TypeApplication of sh first
                           ( Monad m
                           , TestSymbolicBackend ~ sym
                           , MkOperands (GenT m) (SL.List TestOperand) sh
                           ) =>
                           sym
                        -> GenT m (F.ParameterizedFormula sym TestGenArch (sh :: [Symbol]))
genParameterizedFormula _ = do
  params <- Set.fromList <$> HG.list (linear 0 10) genSomeParameter
  operandVars <- mkOperand -- SL.List (BV.BoundVar sym arch) sh
  return F.ParameterizedFormula { F.pfUses = params
                                , F.pfOperandVars = operandVars
                                , F.pfLiteralVars = undefined
                                , F.pfDefs = undefined
                                }


--------------------------------------------------------------------------------
-- Helpers to generate the operandVars based on the caller-specified
-- ParameterizedFormula 'sh'

class Monad m => MkOperands m (f :: k -> *) (ctx :: k) where
  mkOperand :: m (f ctx)

instance (Monad m) => MkOperands m (SL.List TestOperand) '[] where
  mkOperand = return SL.Nil
instance (Monad m, MkOperands m TestOperand o, MkOperands m (SL.List TestOperand) os) =>
  MkOperands m (SL.List TestOperand) (o ': os) where
  mkOperand = do opl <- mkOperand
                 opr <- mkOperand
                 return $ opl SL.:< opr

instance (Monad m) => MkOperands (GenT m) TestOperand "NatArg:Foo" where
  mkOperand = genBoundVar_NatArgFoo
