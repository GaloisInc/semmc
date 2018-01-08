{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module SemMC.DSL.Internal (
  ExprTag(..),
  ExprType(..),
  Parameter(..),
  Location(..),
  Literal(..),
  Expr(..)
  ) where

import           Control.Monad ( guard )
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TH.GADT ( structuralTypeEquality )

data ExprTag = TBool
              | TBV
              | TInt
              | TFloat
              | TDouble
              | TMemory
              | TMemRef
              | TString

data ExprType tp where
  -- | A type of bitvectors of a fixed width
  EBV :: Int -> ExprType 'TBV
  EInt :: ExprType 'TInt
  EFloat :: ExprType 'TFloat
  EDouble :: ExprType 'TDouble
  EBool :: ExprType 'TBool
  EMemory :: ExprType 'TMemory
  EMemRef :: ExprType 'TMemRef
  EString :: ExprType 'TString

deriving instance Eq (ExprType tp)
deriving instance Show (ExprType tp)

instance ShowF ExprType

$(return [])

instance TestEquality ExprType where
  testEquality = $(structuralTypeEquality [t| ExprType |] [])

-- | A parameter and its type
--
-- The type is a string corresponding to an operand type from the architecture
-- (e.g., Gprc), rather than an 'ExprType'.
data Parameter tp = Parameter { pName :: String
                              , pType :: String
                              , pExprType :: ExprType tp
                              }
               deriving (Show)

instance ShowF Parameter

$(return [])

instance TestEquality Parameter where
  testEquality p1 p2 = do
    guard (pName p1 == pName p2)
    guard (pType p1 == pType p2)
    Refl <- testEquality (pExprType p1) (pExprType p2)
    return Refl

data Literal tp = Literal { lName :: String
                          , lExprType :: ExprType tp
                          }
               deriving (Show)

instance ShowF Literal

instance TestEquality Literal where
  testEquality l1 l2 = do
    guard (lName l1 == lName l2)
    Refl <- testEquality (lExprType l1) (lExprType l2)
    return Refl

data Location tp where
  ParamLoc :: Parameter tp -> Location tp
  LiteralLoc :: Literal tp -> Location tp
  LocationFunc :: ExprType tp -> String -> Location tp' -> Location tp

deriving instance Show (Location tp)
instance ShowF Location

instance TestEquality Location where
  testEquality l1 l2 =
    case l1 of
      ParamLoc p1 ->
        case l2 of
          ParamLoc p2 -> do
            Refl <- testEquality p1 p2
            return Refl
          _ -> Nothing
      LiteralLoc ll1 ->
        case l2 of
          LiteralLoc ll2 -> do
            Refl <- testEquality ll1 ll2
            return Refl
          _ -> Nothing
      LocationFunc e1 s1 ll1 ->
        case l2 of
          LocationFunc e2 s2 ll2 -> do
            guard (s1 == s2)
            Refl <- testEquality e1 e2
            Refl <- testEquality ll1 ll2
            return Refl
          _ -> Nothing

-- | An expression representing an SMT formula.  It can reference parameters
--
-- Note that there are some GADT type tags -- unlike crucible, we never need to
-- recover those.  They are only there to guard term construction.
data Expr (tp :: ExprTag) where
  LitBool :: Bool -> Expr 'TBool
  LitBV :: Int -> Integer -> Expr 'TBV
  LitInt :: Integer -> Expr 'TInt
  LitString :: String -> Expr 'TString
  Loc :: Location tp -> Expr tp
  -- | Built-in operations (e.g., bitvector ops)
  Builtin :: ExprType tp -> String -> [Some Expr] -> Expr tp
  -- | Functions provided by theory backends that are called with the underscore
  -- syntax in smt (e.g., extract and extend)
  TheoryFunc :: ExprType tp -> String -> [Some Expr] -> [Some Expr] -> Expr tp
  -- | User-defined uninterpreted functions called with the @call@ SMTLib
  -- primitive
  UninterpretedFunc :: ExprType tp -> String -> [Some Expr] -> Expr tp

deriving instance Show (Expr tp)
instance ShowF Expr

-- $(return [])

-- instance TestEquality Expr where
--   testEquality = $(structuralTypeEquality [t| Expr |] [])

