{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
-- | Convert fragments of ASL code into Crucible CFGs
module SemMC.ASL.Crucible (
    functionToCrucible
  , Function(..)
  , FunctionSignature
  , funcRetRepr
  , funcArgReprs
  , funcGlobalReadReprs
  , funcGlobalWriteReprs
  , SomeFunctionSignature(..)
  , LabeledValue(..)
  , BaseGlobalVar(..)
  , Overrides(..)
  -- * Preprocessing
  -- , computeDefinitionSignature
  -- , computeInstructionSignature
  -- , collectUserTypes
  , UserType
  , Definitions(..)
  -- * Syntax extension
  , ASLExt
  , ASLApp(..)
  , ASLStmt
  , aslExtImpl
  -- * Exceptions
  , TranslationException(..)

  ) where

import           Control.Monad ( when )
import qualified Control.Exception as X
import           Control.Monad.ST ( stToIO, RealWorld, ST )
import qualified Data.Map as Map
import qualified Data.STRef as STRef
import qualified Data.Bimap as BM
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Nonce ( newSTNonceGenerator )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Data.List as List
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Expr as CCE
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.CFG.SSAConversion as CCS
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT
import qualified What4.FunctionName as WFN
import qualified What4.ProgramLoc as WP

import qualified Language.ASL.Syntax as AS

import           SemMC.ASL.Extension ( ASLExt, ASLApp(..), ASLStmt(..), aslExtImpl )
import           SemMC.ASL.Exceptions ( TranslationException(..), LoggedTranslationException(..) )
import           SemMC.ASL.Signature
import           SemMC.ASL.Translation ( UserType(..), TranslationState(..), Overrides(..), Definitions(..), InnerGenerator, translateStatement, overrides, addExtendedTypeData, throwTrace, unliftGenerator)
import qualified SemMC.ASL.SyntaxTraverse as TR
import           SemMC.ASL.Types
import           SemMC.ASL.StaticExpr
import qualified Control.Monad.State.Class as MS

import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Extension as CCExt

import System.IO.Unsafe -- FIXME: For debugging

data Function arch globalReads globalWrites init tps =
   Function { funcSig :: FunctionSignature globalReads globalWrites init tps
            , funcCFG :: CCC.SomeCFG (ASLExt arch) (ToCrucTypes init) (FuncReturn globalWrites tps)
            , funcGlobalReads :: Ctx.Assignment BaseGlobalVar globalReads
            , funcDepends :: Map.Map T.Text StaticValues
            }

-- | This type alias is a constraint relating the 'globals' (base types) to the
-- actual return type in terms of Crucible types
--
-- The constraint is simple but a bit annoying to write
type ReturnsGlobals ret globalWrites tps = (ret ~ FuncReturn globalWrites tps)

-- | Translate an ASL function (signature plus statements) into a Crucible function
--
-- We bundle up the signature, CFG, and allocated globals.  We need to keep the
-- globals around for re-use during simulation.
--
-- The overall strategy is to allocate a Crucible global variable for each part
-- of the CPU state (i.e., machine register) that could be read or written by
-- the procedure.  We'll use symbolic simulation to determine the effect of the
-- procedure on each register.
--
-- Every function takes its natural argument list plus one extra argument: the
-- register file (a struct of all of the register values).  When the procedure
-- starts, we'll copy all of the values from the register struct into the globals.
--
-- NOTE: The signature computation MUST account for the UNPREDICTABLE and
-- UNDEFINED globals.  They may be accessed during the translation and must be
-- available in the 'TranslationState'
functionToCrucible :: forall arch globalReads globalWrites init tps ret
                     . (ReturnsGlobals ret globalWrites tps)
                    => Definitions arch
                    -> FunctionSignature globalReads globalWrites init tps
                    -> CFH.HandleAllocator
                    -> [AS.Stmt]
                    -> Integer -- ^ Logging level
                    -> IO (Function arch globalReads globalWrites init tps)
functionToCrucible defs sig hdlAlloc stmts logLvl = do
  let argReprs = toCrucTypes $ FC.fmapFC (projectValue) (funcArgReprs sig)
  let retRepr = funcSigRepr sig
  hdl <- CFH.mkHandle' hdlAlloc (WFN.functionNameFromText (funcName sig)) argReprs retRepr
  globalReads <- FC.traverseFC allocateGlobal (funcGlobalReadReprs sig)
  let pos = WP.InternalPos
  (CCG.SomeCFG cfg0, depends) <- (do
    (result, log) <- stToIO $ defineCCGFunction pos hdl
      (\refs -> funcDef defs sig refs globalReads stmts logLvl)
    when (logLvl >= 5) $ printLog $ log
    return result)
    `X.catch` (\(LoggedTranslationException log e) -> printLog log >> X.throw e)
  return $
       Function { funcSig = sig
                , funcCFG = CCS.toSSA cfg0
                , funcGlobalReads = globalReads
                , funcDepends = depends
                }
  where
    allocateGlobal :: forall tp . LabeledValue T.Text WT.BaseTypeRepr tp -> IO (BaseGlobalVar tp)
    allocateGlobal (LabeledValue name rep) =
      BaseGlobalVar <$> CCG.freshGlobalVar hdlAlloc name (CT.baseToType rep)

printLog :: [T.Text] -> IO ()
printLog [] = return ()
printLog log = putStrLn (T.unpack $ T.unlines $ List.reverse $ log)

defineCCGFunction :: CCExt.IsSyntaxExtension ext
               => WP.Position
               -> CFH.FnHandle init ret
               -> ((STRef.STRef h (Map.Map T.Text StaticValues), STRef.STRef h [T.Text]) ->
                    CCG.FunctionDef ext t init ret (ST h))
               -> ST h ((CCG.SomeCFG ext init ret, Map.Map T.Text StaticValues), [T.Text])
defineCCGFunction p h f = do
    ng <- newSTNonceGenerator
    funDepRef <- STRef.newSTRef Map.empty
    logRef <- STRef.newSTRef []
    (cfg, _) <- CCG.defineFunction p ng h (f (funDepRef, logRef))
    log <- STRef.readSTRef logRef
    funDeps <- STRef.readSTRef funDepRef
    return ((cfg, funDeps), log)

funcDef :: (ReturnsGlobals ret globalWrites tps)
        => Definitions arch
        -> FunctionSignature globalReads globalWrites init tps
        -> (STRef.STRef h (Map.Map T.Text StaticValues), STRef.STRef h [T.Text])
        -> Ctx.Assignment BaseGlobalVar globalReads
        -> [AS.Stmt]
        -> Integer -- ^ Logging level
        -> Ctx.Assignment (CCG.Atom s) (ToCrucTypes init)
        -> (TranslationState h ret s, InnerGenerator h s arch ret (CCG.Expr (ASLExt arch) s ret))
funcDef defs sig hdls globalReads stmts logLvl args =
  (funcInitialState defs sig hdls logLvl globalReads args, defineFunction overrides sig globalReads stmts args)

funcInitialState :: forall init globalReads globalWrites tps h s arch ret
                  . (ReturnsGlobals ret globalWrites tps)
                 => Definitions arch
                 -> FunctionSignature globalReads globalWrites init tps
                 -> (STRef.STRef h (Map.Map T.Text StaticValues), STRef.STRef h [T.Text])
                 -> Integer -- ^ Logging level
                 -> Ctx.Assignment BaseGlobalVar globalReads
                 -> Ctx.Assignment (CCG.Atom s) (ToCrucTypes init)
                 -> TranslationState h ret s
funcInitialState defs sig (funDepRef, logRef) logLvl globalReads args =
  TranslationState { tsArgAtoms = Ctx.forIndex (Ctx.size args) addArgument Map.empty
                   , tsVarRefs = Map.empty
                   , tsExtendedTypes = defExtendedTypes defs
                   , tsGlobals = FC.foldrFC addGlobal Map.empty globalReads
                   , tsConsts = defConsts defs
                   , tsEnums = defEnums defs
                   , tsFunctionSigs = fst <$> defSignatures defs
                   , tsUserTypes = defTypes defs
                   , tsHandle = funDepRef
                   , tsStaticValues = funcStaticVals sig
                   , tsSig = SomeFunctionSignature sig
                   , tsLogHandle = (logRef, logLvl, 0)
                   }
  where
    addArgument :: forall tp
                 . Map.Map T.Text (Some (CCG.Atom s))
                -> Ctx.Index (ToCrucTypes init) tp
                -> Map.Map T.Text (Some (CCG.Atom s))
    addArgument m idx =
      let
        argReprs = FC.fmapFC projectValue (funcArgReprs sig)
        inArgReprs = FC.fmapFC (CCG.typeOfAtom) args
        bidx = toBaseIndex argReprs inArgReprs idx
      in
        Map.insert (projectFunctionName (funcArgReprs sig Ctx.! bidx)) (Some (args Ctx.! idx)) m
    addGlobal (BaseGlobalVar gv) m =
      Map.insert (CCG.globalName gv) (Some gv) m

projectFunctionName :: LabeledValue FunctionArg a tp -> T.Text
projectFunctionName (LabeledValue (FunctionArg nm _ _) _) = nm

defineFunction :: (ReturnsGlobals ret globalWrites tps)
               => Overrides arch
               -> FunctionSignature globalReads globalWrites init tps
               -> Ctx.Assignment BaseGlobalVar globalReads
               -> [AS.Stmt]
               -> Ctx.Assignment (CCG.Atom s) (ToCrucTypes init)
               -> InnerGenerator h s arch ret (CCG.Expr (ASLExt arch) s ret)
defineFunction ov sig baseGlobals stmts _args = do
  unliftGenerator $ FC.forFC_ (funcArgReprs sig) (\(LabeledValue (FunctionArg nm t _) _) -> addExtendedTypeData nm t)
  unliftGenerator $ mapM_ (translateStatement ov) stmts
  case funcRetRepr sig of
    Ctx.Empty -> unliftGenerator $ translateStatement ov (AS.StmtReturn Nothing)
    _ -> return ()
  let errmsg = "Function " <> funcName sig <> " does not return."
  errStr <- CCG.mkAtom (CCG.App (CCE.TextLit errmsg))
  CCG.reportError (CCG.AtomExpr errStr)

{- Note [Call Translation]

Functions may return both values or update processor state through side effects.
Our challenge in this code is to turn these imperative procedures into pure
functions.  The strategy will be to arrange it so that, in addition to its
natural set of parameters, each function takes an entire machine state as a
BaseStruct.  It will also return an entire BaseStruct register state.

At function initialization time, the function will copy all of its input
machine state into a set of locals (Crucible or globals).  Before calling a
function, the caller takes a snapshot of the current machine state (from the
refs) to construct the BaseStruct to pass to the callee.  After a function call
returns, the caller will assign the contents of the register state back to its
locals (refs).

Question: do we need any additional components to the return value of
procedures?  Anything that isn't a global is local, and local modifications
can't be reflected to callers.

Note that we have an additional unusual constraint: we need to represent calls
in any context as uninterpreted functions, since we don't want to eagerly expand
definitions of functions.  Doing so produces an enormous code explosion that we
can't handle.  Crucible can support uninterpreted functions via what4; however,
they aren't exactly first class.  Uninterpreted functions can only take as
arguments and return base types.  Crucible doesn't have great support for
working with base types.

Beyond the normal machine registers, we introduce two extra state variables:
- Undefined
- Unpredictable

Each is a boolean that starts as False and is switched to True if an instruction
has undefined or unpredictable behavior, respectively.

-}
