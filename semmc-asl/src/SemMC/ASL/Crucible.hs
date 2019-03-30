{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  -- * Exceptions
  , TranslationException(..)
  ) where

import qualified Control.Exception as X
import           Control.Monad.ST ( stToIO )
import qualified Control.Monad.State.Class as MS
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Expr as CCE
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.CFG.SSAConversion as CCS
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT
import qualified What4.ProgramLoc as WP
import           Unsafe.Coerce ( unsafeCoerce )

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
                   => FunctionSignature sym init ret tp
                   -> CFH.FnHandle init ret
                   -> [AS.Stmt]
                   -> IO (CCC.SomeCFG () init ret)
functionToCrucible sig hdl stmts = do
  let pos = WP.InternalPos
  (CCG.SomeCFG cfg0, _) <- stToIO $ CCG.defineFunction pos hdl (funcDef sig stmts)
  return (CCS.toSSA cfg0)

funcDef :: (ret ~ CT.BaseToType tp)
        => FunctionSignature sym init ret tp
        -> [AS.Stmt]
        -> Ctx.Assignment (CCG.Atom s) init
        -> (TranslationState ret s, CCG.Generator () h s (TranslationState ret) ret (CCG.Expr () s ret))
funcDef sig stmts args = (initialState sig args, defineFunction sig stmts args)

initialState :: forall sym init ret tp s
              . FunctionSignature sym init ret tp
             -> Ctx.Assignment (CCG.Atom s) init
             -> TranslationState ret s
initialState sig args = TranslationState m1 MapF.empty (error "globals")
  where
    m1 = Ctx.forIndex (Ctx.size args) addArgumentAtom MapF.empty
    addArgumentAtom :: forall tp0
                     . MapF.MapF TypedName (CCG.Atom s)
                    -> Ctx.Index init tp0
                    -> MapF.MapF TypedName (CCG.Atom s)
    addArgumentAtom m idx =
      let atom = args Ctx.! idx
          LabeledValue argName _ = funcArgReprs sig Ctx.! idx
      in MapF.insert (TypedName argName) atom m

newtype TypedName tp = TypedName T.Text

instance TestEquality TypedName where
  testEquality (TypedName t1) (TypedName t2)
    | t1 == t2 = Just (unsafeCoerce Refl)
    | otherwise = Nothing

instance OrdF TypedName where
  compareF (TypedName t1) (TypedName t2) = unsafeCoerce (fromOrdering (compare t1 t2))

-- Will track the mapping from (ASL) identifiers to Crucible Atoms
data TranslationState ret s =
  TranslationState { tsArgAtoms :: MapF.MapF TypedName (CCG.Atom s)
                   -- ^ Atoms corresponding to function/procedure inputs.  We assume that these are
                   -- immutable and allocated before we start executing.
                   , tsVarRefs :: MapF.MapF TypedName (CCG.Reg s)
                   -- ^ Local registers containing values; these are created on first use
                   , tsGlobals :: MapF.MapF TypedName CCG.GlobalVar
                   -- ^ Global variables corresponding to machine state (e.g., machine registers).
                   -- These are allocated before we start executing based on the list of
                   -- transitively-referenced globals in the signature.
                   }

lookupVarRef :: T.Text -> CCG.Generator () h s (TranslationState ret) ret (CCG.Expr () s tp)
lookupVarRef = undefined

defineFunction :: (ret ~ CT.BaseToType tp)
               => FunctionSignature sym init ret tp
               -> [AS.Stmt]
               -> Ctx.Assignment (CCG.Atom s) init
               -> CCG.Generator () h s (TranslationState ret) ret (CCG.Expr () s ret)
defineFunction sig stmts args = do
  mapM_ (translateStatement (CT.baseToType (funcSigRepr sig))) stmts
  -- Note: we shouldn't actually get here, as we should have called returnFromFunction while
  -- translating.
  X.throw (NoReturnInFunction (SomeFunctionSignature sig))

translateStatement :: CT.TypeRepr ret -> AS.Stmt -> CCG.Generator () h s (TranslationState ret) ret ()
translateStatement rep stmt =
  case stmt of
    AS.StmtReturn Nothing
      | Just Refl <- testEquality rep CT.UnitRepr -> CCG.returnFromFunction (CCG.App CCE.EmptyApp)
      | otherwise -> X.throw (InvalidReturnType CT.UnitRepr)
    AS.StmtReturn (Just expr) -> do
      a <- translateExpr rep expr
      CCG.returnFromFunction (CCG.AtomExpr a)

-- | Translate an ASL expression into an Atom (which is a reference to an immutable value)
--
-- Atoms may be written to registers, which are mutable locals
translateExpr :: CT.TypeRepr tp -> AS.Expr -> CCG.Generator () h s (TranslationState ret) ret (CCG.Atom s tp)
translateExpr rep expr =
  case expr of
    AS.ExprLitInt i
      | Just Refl <- testEquality rep CT.IntegerRepr -> CCG.mkAtom (CCG.App (CCE.IntLit i))
      | otherwise -> X.throw (UnexpectedExprType rep expr)
    AS.ExprLitBin bits -> do
      let nBits = length bits
      case NR.mkNatRepr (fromIntegral nBits) of
        Some nr ->
          case NR.isZeroOrGT1 nr of
            Left _ -> X.throw InvalidZeroLengthBitvector
            Right NR.LeqProof -> do
              let bvrep = CT.BVRepr nr
              if | Just Refl <- testEquality rep bvrep ->
                   CCG.mkAtom (CCG.App (CCE.BVLit nr (bitsToInteger bits)))
                 | otherwise -> X.throw (UnexpectedBitvectorLength rep bvrep)

    AS.ExprLitReal {} -> X.throw (UnsupportedExpr expr)

bitsToInteger :: [Bool] -> Integer
bitsToInteger = undefined

procedureToCrucible :: ProcedureSignature sym init ret tps -> [AS.Stmt] -> IO (CCC.SomeCFG () init ret)
procedureToCrucible = undefined

data TranslationException = forall sym . NoReturnInFunction (SomeSignature sym)
                          | forall tp . InvalidReturnType (CT.TypeRepr tp)
                          | forall tp .  UnexpectedExprType (CT.TypeRepr tp) AS.Expr
                          | UnsupportedExpr AS.Expr
                          | InvalidZeroLengthBitvector
                          | forall tp1 tp2 . UnexpectedBitvectorLength (CT.TypeRepr tp1) (CT.TypeRepr tp2)

deriving instance Show TranslationException

instance X.Exception TranslationException
