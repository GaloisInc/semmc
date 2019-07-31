{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module SemMC.ASL.Exceptions (
  TranslationException(..)
  ) where

import qualified Control.Exception as X
import qualified Data.Text as T
import qualified Data.Parameterized.Context as Ctx
import qualified Lang.Crucible.Types as CT

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Signature

data TranslationException = forall ret . NoReturnInFunction (SomeSignature ret)
                          | forall tp . InvalidReturnType (CT.TypeRepr tp)
                          | forall tp1 tp2 .  UnexpectedExprType (Maybe AS.Expr) (CT.TypeRepr tp1) (CT.TypeRepr tp2)
                          -- ^ Expression, actual type, expected type
                          | UnsupportedExpr AS.Expr
                          | InvalidZeroLengthBitvector
                          | forall tp1 tp2 . UnexpectedBitvectorLength (CT.TypeRepr tp1) (CT.TypeRepr tp2)
                          | forall tp . ExpectedBVType AS.Expr (CT.TypeRepr tp)
                          | forall tp . ExpectedStructType (Maybe AS.Expr) (CT.TypeRepr tp)
                          | forall tp . UnsupportedComparisonType AS.Expr (CT.TypeRepr tp)
                          | UnboundName T.Text
                          | LocalAlreadyDefined T.Text
                          | UnsupportedBinaryOperator AS.BinOp
                          | EmptySetElementList AS.Expr
                          | MalformedConditionalExpression AS.Expr
                          | forall tp . ExpectedBaseTypeRepr (CT.TypeRepr tp)
                          | forall tp . ExpectedBaseType AS.Expr (CT.TypeRepr tp)
                          | forall tp . ExpectedBaseTypeArgument T.Text (CT.TypeRepr tp)
                          | InvalidFunctionName T.Text
                          | MissingFunctionDefinition T.Text
                          | ExpectedFunctionSignature T.Text
                          | ExpectedProcedureSignature T.Text
                          | forall tps . InvalidArgumentTypes T.Text (Ctx.Assignment CT.TypeRepr tps)
                          | forall tp1 tp2 . UnexpectedProcedureReturn (CT.TypeRepr tp1) (CT.TypeRepr tp2)
                          | MissingGlobal T.Text
                          | forall tp . UnexpectedGlobalType T.Text (CT.TypeRepr tp)
                          | UnexpectedType AS.QualifiedIdentifier
                          | InvalidSliceRange Integer Integer
                          | forall tp . InvalidSlice Integer Integer (CT.TypeRepr tp)

deriving instance Show TranslationException

instance X.Exception TranslationException
