{-# LANGUAGE FlexibleInstances #-}

module TestArchPropGen
where

import           Data.Int ( Int64 )
import           Hedgehog
import qualified Hedgehog.Gen as HG
import           Hedgehog.Range
import           Numeric.Natural
import qualified SemMC.Formula.Formula as F
import           TestArch
import           What4.BaseTypes


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

-- instance Arbitrary (F.Parameter TestGenArch sh BaseNatType) where
--   arbitrary = oneof [ undefined -- return $ F.OperandParameter oprepr1 undefined
--                     , F.LiteralParameter <$> arbitrary
--                     -- , return $ F.FunctionParameter undefined undefined oprepr2
--                     ]
--               where oprepr1 = undefined
--                     oprepr2 = undefined
--                     -- loc :: TestLocation ty
--                     -- loc = arbitrary

-- instance Arbitrary (F.Parameter TestGenArch sh BaseIntegerType) where
--   arbitrary = oneof [ undefined -- return $ F.OperandParameter oprepr1 undefined
--                     , F.LiteralParameter <$> arbitrary
--                     -- , return $ F.FunctionParameter undefined undefined oprepr2
--                     ]
--               where oprepr1 = undefined
--                     oprepr2 = undefined
--                     -- loc :: TestLocation ty
--                     -- loc = arbitrary

----------------------------------------------------------------------
-- Formula.ParameterizedFormula generators

genParameterizedFormula :: Monad m => GenT m (F.ParameterizedFormula sym TestGenArch sh)
genParameterizedFormula =
  HG.element
  [
    -- TBD!
  ]
