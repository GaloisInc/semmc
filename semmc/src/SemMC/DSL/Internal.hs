{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module SemMC.DSL.Internal
    ( ExprTag(..)
    , ExprType(..)
    , Parameter(..)
    , Location(..)
    , Literal(..)
    , Expr(..)
    , litEq
  ) where

import Control.Monad ( guard )
import Data.Parameterized.Classes
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.TH.GADT ( structuralTypeEquality )


data ExprTag = TBool
              | TBV
              | TInt
              | TFloat
              | TDouble
              | TMemory
              | TMemRef
              | TString
              | TPackedOperand

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
  EPackedOperand :: String -> ExprType 'TPackedOperand

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
  -- | The MemoryLoc specifies a location in memory.  These are always
  -- accessed via a read_mem or write_mem uninterpreted function which
  -- has an associated address within the memory region.  This address
  -- is usually computed, but some opcodes can write to multiple
  -- memory locations (e.g. PUSH can write multiple registers to
  -- multiple stack locations), but defLoc has protection against
  -- writing the same location multiple times (thereby creating
  -- indeterminism about which write takes precedence).  To support
  -- that, the memory locations can be declared with a local
  -- identifier (usually the word offset, e.g. MemoryLoc 0, MemoryLoc
  -- 4, etc.).  When defLoc compares, all of these refer to "Mem", but
  -- the local identifier must be unique to ensure each location is
  -- written only once.
  MemoryLoc :: Integer -> Location 'TMemory
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
      MemoryLoc addroff1 ->
          case l2 of
            MemoryLoc addroff2 -> do
                guard (addroff1 == addroff2)
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
  -- | Assign an advisory name to a sub-expression.  This can be used
  -- (for example) to guide let-binding for output S-expression forms
  -- of this expression.
  NamedSubExpr :: String -> Expr tp -> Expr tp
  -- | Reference to a packed operand; these are unique types that can
  -- only be unpacked into more useable expression values by
  -- uninterpreted function(s).
  PackedOperand :: String -> Expr 'TPackedOperand

deriving instance Show (Expr tp)
instance ShowF Expr


-- | If both terms are literals, this can provide an evaluation of
-- their equality, otherwise it returns Nothing to indicate that a
-- literal comparison cannot be made (deferring to an actual
-- evaluation).
litEq :: Expr tp -> Expr tp -> Maybe (Expr 'TBool)
litEq (LitBool x) (LitBool y) = Just $ LitBool $ x == y
litEq (LitBV w x) (LitBV v y) = if w == v
                                then Just $ LitBool $ x == y
                                else Nothing -- error indicated elsewhere
litEq (LitInt x) (LitInt y) = Just $ LitBool $ x == y
litEq (LitString x) (LitString y) = Just $ LitBool $ x == y
litEq (NamedSubExpr _ x) y = litEq x y
litEq x (NamedSubExpr _ y) = litEq x y
litEq _ _ = Nothing


-- $(return [])

-- instance TestEquality Expr where
--   testEquality = $(structuralTypeEquality [t| Expr |] [])

