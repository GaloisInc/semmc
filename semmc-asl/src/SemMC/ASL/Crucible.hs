{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | Convert fragments of ASL code into Crucible CFGs
module SemMC.ASL.Crucible (
    functionToCrucible
  , procedureToCrucible
  , FunctionSignature
  , funcSigRepr
  , funcArgReprs
  , funcGlobalReprs
  , ProcedureSignature
  , procSigRepr
  , procArgReprs
  , procGlobalReprs
  , SomeSignature(..)
  , computeDefinitionSignature
  , computeInstructionSignature
  , Callable
  , asCallable
  , LabeledValue(..)
  , BaseGlobalVar(..)
  , Overrides(..)
  -- * Exceptions
  , TranslationException(..)
  ) where

import           Control.Applicative ( (<|>) )
import qualified Control.Exception as X
import           Control.Monad.ST ( stToIO )
import qualified Control.Monad.State.Class as MS
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Expr as CCE
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.CFG.SSAConversion as CCS
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Types as CT
import           Unsafe.Coerce ( unsafeCoerce )
import qualified What4.BaseTypes as WT
import qualified What4.ProgramLoc as WP

import qualified Language.ASL.Syntax as AS

-- | The signature describes the inputs and outputs of an ASL function or procedure
--
-- Procedures have side effects, while functions are side-effect free and return a single value
-- (which may be a tuple).
--
-- Top-level code sequences (like the @instExecute@ field of an instruction) have a trivial type
-- signature with no inputs (just global refs) and a set of outputs that is the union of all of the
-- locations touched by that function.
data FunctionSignature sym init ret tp =
  FunctionSignature { funcSigRepr :: WT.BaseTypeRepr tp
                    , funcArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                    , funcGlobalReprs :: Some (Ctx.Assignment (LabeledValue T.Text CT.TypeRepr))
                    }
  deriving (Show)

data LabeledValue a b tp = LabeledValue a (b tp)

instance FC.FunctorFC (LabeledValue a) where
  fmapFC f (LabeledValue a b) = LabeledValue a (f b)

instance FC.FoldableFC (LabeledValue a) where
  foldrFC f s (LabeledValue _ b) = f b s

instance FC.TraversableFC (LabeledValue a) where
  traverseFC f (LabeledValue a b) = LabeledValue a <$> f b

instance (Show a, ShowF b) => ShowF (LabeledValue a b) where
  showF (LabeledValue l v) = concat [ "LabeledValue ", show l, " ", showF v ]

instance (Show a, ShowF b) => Show (LabeledValue a b tp) where
  show (LabeledValue l v) = concat [ "LabeledValue ", show l, " ", showF v ]

newtype BaseGlobalVar tp = BaseGlobalVar (CCG.GlobalVar (CT.BaseToType tp))
  deriving (Show)

instance ShowF BaseGlobalVar

-- | Like 'FunctionSignature', except with a different return value
--
-- The return here is actually a list of global variables updated by the function (both direct and
-- indirect updates)
data ProcedureSignature sym init ret tps =
  ProcedureSignature { procSigRepr :: Ctx.Assignment BaseGlobalVar tps
                     , procArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                     , procGlobalReprs :: Some (Ctx.Assignment (LabeledValue T.Text CT.TypeRepr))
                     }
  deriving (Show)

instance ShowF (ProcedureSignature sym init ret)

data SomeSignature sym where
  SomeFunctionSignature :: FunctionSignature sym init ret tp -> SomeSignature sym
  SomeProcedureSignature :: ProcedureSignature sym init rep tps -> SomeSignature sym

deriving instance Show (SomeSignature sym)

data Callable = Callable { callableName :: AS.QualifiedIdentifier
                         , callableArgs :: [AS.SymbolDecl]
                         , callableRets :: [AS.Type]
                         , callableStmts :: [AS.Stmt]
                         }

asCallable :: AS.Definition -> Maybe Callable
asCallable def =
  case def of
    AS.DefCallable { AS.callableName = name
                   , AS.callableArgs = args
                   , AS.callableRets = rets
                   , AS.callableStmts = stmts
                   } ->
      Just Callable { callableName = name
                    , callableArgs = args
                    , callableRets = rets
                    , callableStmts = stmts
                    }
    _ -> Nothing

-- | Compute the signature for a definition
--
-- FIXME: This may need to take all of the signatures of called functions to compute its own
-- signature (since they might be procedures updating state that isn't obvious)
computeDefinitionSignature :: [(String, SomeSignature sym)] -> Callable -> IO (SomeSignature sym)
computeDefinitionSignature = undefined

computeInstructionSignature :: [(String, SomeSignature sym)] -> [AS.Stmt] -> IO (SomeSignature sym)
computeInstructionSignature = undefined

functionToCrucible :: (ret ~ CT.BaseToType tp)
                   => Overrides
                   -> FunctionSignature sym init ret tp
                   -> CFH.FnHandle init ret
                   -> [AS.Stmt]
                   -> IO (CCC.SomeCFG () init ret)
functionToCrucible ov sig hdl stmts = do
  let pos = WP.InternalPos
  (CCG.SomeCFG cfg0, _) <- stToIO $ CCG.defineFunction pos hdl (funcDef ov sig stmts)
  return (CCS.toSSA cfg0)

funcDef :: (ret ~ CT.BaseToType tp)
        => Overrides
        -> FunctionSignature sym init ret tp
        -> [AS.Stmt]
        -> Ctx.Assignment (CCG.Atom s) init
        -> (TranslationState ret s, CCG.Generator () h s (TranslationState ret) ret (CCG.Expr () s ret))
funcDef ov sig stmts args = (initialState sig args, defineFunction ov sig stmts args)

initialState :: forall sym init ret tp s
              . FunctionSignature sym init ret tp
             -> Ctx.Assignment (CCG.Atom s) init
             -> TranslationState ret s
initialState sig args = TranslationState m1 Map.empty (error "globals")
  where
    m1 = Ctx.forIndex (Ctx.size args) addArgumentAtom Map.empty
    addArgumentAtom :: forall tp0
                     . Map.Map T.Text (Some (CCG.Atom s))
                    -> Ctx.Index init tp0
                    -> Map.Map T.Text (Some (CCG.Atom s))
    addArgumentAtom m idx =
      let atom = args Ctx.! idx
          LabeledValue argName _ = funcArgReprs sig Ctx.! idx
      in Map.insert argName (Some atom) m

newtype TypedName tp = TypedName T.Text

instance TestEquality TypedName where
  testEquality (TypedName t1) (TypedName t2)
    | t1 == t2 = Just (unsafeCoerce Refl)
    | otherwise = Nothing

instance OrdF TypedName where
  compareF (TypedName t1) (TypedName t2) = unsafeCoerce (fromOrdering (compare t1 t2))

-- Will track the mapping from (ASL) identifiers to Crucible Atoms
data TranslationState ret s =
  TranslationState { tsArgAtoms :: Map.Map T.Text (Some (CCG.Atom s))
                   -- ^ Atoms corresponding to function/procedure inputs.  We assume that these are
                   -- immutable and allocated before we start executing.
                   , tsVarRefs :: Map.Map T.Text (Some (CCG.Reg s))
                   -- ^ Local registers containing values; these are created on first use
                   , tsGlobals :: Map.Map T.Text (Some CCG.GlobalVar)
                   -- ^ Global variables corresponding to machine state (e.g., machine registers).
                   -- These are allocated before we start executing based on the list of
                   -- transitively-referenced globals in the signature.
                   }

data ExprConstructor h s ret where
  ExprConstructor :: a tp
                  -> (a tp -> CCG.Generator () h s (TranslationState ret) ret (CCG.Expr () s tp))
                  -> ExprConstructor h s ret

lookupVarRef :: forall h s ret
              . T.Text
             -> CCG.Generator () h s (TranslationState ret) ret (Some (CCG.Expr () s))
lookupVarRef name = do
  ts <- MS.get
  let err = X.throw (UnboundName name)
  case fromMaybe err (lookupArg ts <|> lookupRef ts <|> lookupGlobal ts) of
    ExprConstructor e con -> Some <$> con e
  where
    lookupArg ts = do
      Some e <- Map.lookup name (tsArgAtoms ts)
      return (ExprConstructor (CCG.AtomExpr e) return)
    lookupRef ts = do
      Some r <- Map.lookup name (tsVarRefs ts)
      return (ExprConstructor r CCG.readReg)
    lookupGlobal ts = do
      Some g <- Map.lookup name (tsGlobals ts)
      return (ExprConstructor g CCG.readGlobal)

data Overrides =
  Overrides { overrideStmt :: forall h s ret . AS.Stmt -> Maybe (CCG.Generator () h s (TranslationState ret) ret ())
            , overrideExpr :: forall h s ret . AS.Expr -> Maybe (CCG.Generator () h s (TranslationState ret) ret (Some (CCG.Atom s)))
            }

defineFunction :: (ret ~ CT.BaseToType tp)
               => Overrides
               -> FunctionSignature sym init ret tp
               -> [AS.Stmt]
               -> Ctx.Assignment (CCG.Atom s) init
               -> CCG.Generator () h s (TranslationState ret) ret (CCG.Expr () s ret)
defineFunction ov sig stmts args = do
  mapM_ (translateStatement ov (CT.baseToType (funcSigRepr sig))) stmts
  -- Note: we shouldn't actually get here, as we should have called returnFromFunction while
  -- translating.
  X.throw (NoReturnInFunction (SomeFunctionSignature sig))

translateStatement :: Overrides -> CT.TypeRepr ret -> AS.Stmt -> CCG.Generator () h s (TranslationState ret) ret ()
translateStatement ov rep stmt
  | Just so <- overrideStmt ov stmt = so
  | otherwise =
    case stmt of
      AS.StmtReturn Nothing
        | Just Refl <- testEquality rep CT.UnitRepr -> CCG.returnFromFunction (CCG.App CCE.EmptyApp)
        | otherwise -> X.throw (InvalidReturnType CT.UnitRepr)
      AS.StmtReturn (Just expr) -> do
        Some a <- translateExpr ov expr
        Refl <- assertAtomType expr rep a
        CCG.returnFromFunction (CCG.AtomExpr a)
      AS.StmtIf clauses melse -> translateIf ov rep clauses melse

translateIf :: Overrides
            -> CT.TypeRepr ret
            -> [(AS.Expr, [AS.Stmt])]
            -> Maybe [AS.Stmt]
            -> CCG.Generator () h s (TranslationState ret) ret ()
translateIf ov rep clauses melse =
  case clauses of
    [] -> mapM_ (translateStatement ov rep) (fromMaybe [] melse)
    (cond, body) : rest -> do
      Some condAtom <- translateExpr ov cond
      Refl <- assertAtomType cond CT.BoolRepr condAtom
      let genThen = mapM_ (translateStatement ov rep) body
      let genElse = translateIf ov rep rest melse
      CCG.ifte_ (CCG.AtomExpr condAtom) genThen genElse

assertAtomType :: AS.Expr
               -- ^ Expression that was translated
               -> CT.TypeRepr tp1
               -- ^ Expected type
               -> CCG.Atom s tp2
               -- ^ Translation (which contains the actual type)
               -> CCG.Generator () h s (TranslationState ret) ret (tp1 :~: tp2)
assertAtomType expr expectedRepr atom =
  case testEquality expectedRepr (CCG.typeOfAtom atom) of
    Nothing -> X.throw (UnexpectedExprType expr (CCG.typeOfAtom atom) expectedRepr)
    Just Refl -> return Refl

-- | Translate an ASL expression into an Atom (which is a reference to an immutable value)
--
-- Atoms may be written to registers, which are mutable locals
translateExpr :: Overrides -> AS.Expr -> CCG.Generator () h s (TranslationState ret) ret (Some (CCG.Atom s))
translateExpr ov expr
  | Just eo <- overrideExpr ov expr = eo
  | otherwise =
    case expr of
      AS.ExprLitInt i -> Some <$> CCG.mkAtom (CCG.App (CCE.IntLit i))
      AS.ExprLitBin bits -> do
        let nBits = length bits
        case NR.mkNatRepr (fromIntegral nBits) of
          Some nr
            | Just NR.LeqProof <- NR.testLeq (NR.knownNat @1) nr ->
              Some <$> CCG.mkAtom (CCG.App (CCE.BVLit nr (bitsToInteger bits)))
            | otherwise -> X.throw InvalidZeroLengthBitvector
      AS.ExprVarRef (AS.QualifiedIdentifier _ ident) -> do
        Some e <- lookupVarRef ident
        Some <$> CCG.mkAtom e
      AS.ExprLitReal {} -> X.throw (UnsupportedExpr expr)
      AS.ExprLitString {} -> X.throw (UnsupportedExpr expr)

bitsToInteger :: [Bool] -> Integer
bitsToInteger = undefined

procedureToCrucible :: ProcedureSignature sym init ret tps -> [AS.Stmt] -> IO (CCC.SomeCFG () init ret)
procedureToCrucible = undefined

data TranslationException = forall sym . NoReturnInFunction (SomeSignature sym)
                          | forall tp . InvalidReturnType (CT.TypeRepr tp)
                          | forall tp1 tp2 .  UnexpectedExprType AS.Expr (CT.TypeRepr tp1) (CT.TypeRepr tp2)
                          -- ^ Expression, actual type, expected type
                          | UnsupportedExpr AS.Expr
                          | InvalidZeroLengthBitvector
                          | forall tp1 tp2 . UnexpectedBitvectorLength (CT.TypeRepr tp1) (CT.TypeRepr tp2)
                          | UnboundName T.Text

deriving instance Show TranslationException

instance X.Exception TranslationException
