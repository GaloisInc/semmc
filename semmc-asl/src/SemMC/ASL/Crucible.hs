{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
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
  , psArgReprs
  , SomeSignature(..)
  , computeDefinitionSignature
  , computeInstructionSignature
  , Callable
  , asCallable
  , LabeledValue(..)
  , BaseGlobalVar(..)
  , Overrides(..)
  -- * Syntax extension
  , ASLExt
  , ASLArch(..)
  , ASLApp(..)
  , ASLStmt
  , aslExtImpl
  -- * Exceptions
  , TranslationException(..)
  ) where

import qualified Control.Exception as X
import           Control.Monad.ST ( stToIO )
import qualified Control.Monad.State.Strict as MS
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
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
import qualified What4.ProgramLoc as WP

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Extension ( ASLExt, ASLArch(..), ASLApp(..), ASLStmt, aslExtImpl )
import           SemMC.ASL.Exceptions ( TranslationException(..) )
import           SemMC.ASL.Signature
import           SemMC.ASL.Translation ( TranslationState(..), Overrides(..), translateStatement, assignmentFromList )

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
computeDefinitionSignature :: [(String, SomeSignature regs)] -> Callable -> IO (SomeSignature regs)
computeDefinitionSignature = undefined

computeInstructionSignature :: [(String, SomeSignature regs)] -> [AS.Stmt] -> IO (SomeSignature regs)
computeInstructionSignature = undefined

functionToCrucible :: (ret ~ CT.BaseToType tp, ASLArch arch)
                   => Overrides arch regs
                   -> FunctionSignature init ret tp
                   -> CFH.FnHandle init ret
                   -> [AS.Stmt]
                   -> IO (CCC.SomeCFG (ASLExt arch) init ret)
functionToCrucible ov sig hdl stmts = do
  let pos = WP.InternalPos
  (CCG.SomeCFG cfg0, _) <- stToIO $ CCG.defineFunction pos hdl (funcDef ov sig stmts)
  return (CCS.toSSA cfg0)

funcDef :: (ret ~ CT.BaseToType tp, ASLArch arch)
        => Overrides arch regs
        -> FunctionSignature init ret tp
        -> [AS.Stmt]
        -> Ctx.Assignment (CCG.Atom s) init
        -> (TranslationState arch regs ret s, CCG.Generator (ASLExt arch) h s (TranslationState arch regs ret) ret (CCG.Expr (ASLExt arch) s ret))
funcDef ov sig stmts args = (funcInitialState sig args, defineFunction ov sig stmts args)

funcInitialState :: forall init ret tp s regs arch
                  . FunctionSignature init ret tp
                 -> Ctx.Assignment (CCG.Atom s) init
                 -> TranslationState arch regs ret s
funcInitialState sig args =
  TranslationState m1 Map.empty (error "globals") (error "undefined") (error "unpredictable") (error "sigs") (error "globalctx")
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


defineFunction :: forall ret tp init h s regs arch
                . (ret ~ CT.BaseToType tp, ASLArch arch)
               => Overrides arch regs
               -> FunctionSignature init ret tp
               -> [AS.Stmt]
               -> Ctx.Assignment (CCG.Atom s) init
               -> CCG.Generator (ASLExt arch) h s (TranslationState arch regs ret) ret (CCG.Expr (ASLExt arch) s ret)
defineFunction ov sig stmts args = do
  -- FIXME: Put args into the environment as locals (that can be read from)
  mapM_ (translateStatement ov (CT.baseToType (funcSigRepr sig))) stmts
  -- Note: we shouldn't actually get here, as we should have called returnFromFunction while
  -- translating.
  X.throw (NoReturnInFunction (SomeFunctionSignature sig))


-- FIXME: Make a wrapper to capture the constructed CFG and all of the global
-- variables used in the definition, as they will also be required to simulate
-- it (and we don't want to duplicate globals).
procedureToCrucible :: (ASLArch arch)
                    => Overrides arch regs
                    -> ProcedureSignature init regs ret
                    -> CFH.FnHandle init ret
                    -> [AS.Stmt]
                    -> IO (CCC.SomeCFG (ASLExt arch) init ret)
procedureToCrucible ov sig hdl stmts = do
  let pos = WP.InternalPos
  (CCG.SomeCFG cfg0, _) <- stToIO $ CCG.defineFunction pos hdl (procDef ov sig stmts)
  return (CCS.toSSA cfg0)

procDef :: (ASLArch arch)
        => Overrides arch regs
        -> ProcedureSignature init regs ret
        -> [AS.Stmt]
        -> Ctx.Assignment (CCG.Atom s) init
        -> (TranslationState arch regs ret s, CCG.Generator (ASLExt arch) h s (TranslationState arch regs ret) ret (CCG.Expr (ASLExt arch) s ret))
procDef ov sig stmts args =
  (procInitialState sig args, defineProcedure ov sig stmts args)

procInitialState :: ProcedureSignature init regs ret
                 -> Ctx.Assignment (CCG.Atom s) init
                 -> TranslationState arch regs ret s
procInitialState = undefined

defineProcedure :: (ASLArch arch)
                => Overrides arch regs
                -> ProcedureSignature init regs ret
                -> [AS.Stmt]
                -> Ctx.Assignment (CCG.Atom s) init
                -> CCG.Generator (ASLExt arch) h s (TranslationState arch regs ret) ret (CCG.Expr (ASLExt arch) s ret)
defineProcedure ov sig stmts args = do
  -- FIXME: Initialize arguments with args
  mapM_ (translateStatement ov (psSigRepr sig)) stmts
  -- Read all of the globals in the signature to produce a struct expr
  baseGlobals <- MS.gets tsGlobalCtx
  someVals <- mapM readBaseGlobal (FC.toListFC Some baseGlobals) -- (procSigGlobals sig))
  case assignmentFromList (Some Ctx.empty) someVals of
    Some vals -> do
      let reprs = FC.fmapFC CCG.exprType vals
      retAtom <- CCG.mkAtom (CCG.App (CCE.MkStruct reprs vals))
      if | Just Refl <- testEquality (CCG.typeOfAtom retAtom) (psSigRepr sig) ->
           return (CCG.AtomExpr retAtom)
         | otherwise -> X.throw (UnexpectedProcedureReturn (psSigRepr sig) (CCG.typeOfAtom retAtom))

readBaseGlobal :: (CCE.IsSyntaxExtension ext)
               => Some BaseGlobalVar
               -> CCG.Generator ext h s (TranslationState arch regs ret) ret (Some (CCG.Expr ext s))
readBaseGlobal (Some (BaseGlobalVar gv)) = Some <$> CCG.readGlobal gv

{- Note [Call Translation]

There are two types of calls in ASL: functions and procedures.

Functions are simple and are already purely functional, and thus need no additional support.

Procedures are more complicated, as they return no values but instead update processor state through
side effects.  Our challenge in this code is to turn these imperative procedures into pure
functions.  The strategy will be to arrange it so that, in addition to its natural set of
parameters, each procedure takes an entire machine state as a BaseStruct.  It will also return an
entire BaseStruct register state.

At procedure initialization time, the procedure will copy all of its input machine state into a set
of locals (Crucible refs).  Before calling a procedure, the caller takes a snapshot of the current
machine state (from the refs) to construct the BaseStruct to pass to the callee.  After a procedure
call returns, the caller will assign the contents of the register state back to its locals (refs).

Question: do we need any additional components to the return value of procedures?  Anything that
isn't a global is local, and local modifications can't be reflected to callers.

For the semantics of an *instruction*, we'll set up the global state as globals.

-}
