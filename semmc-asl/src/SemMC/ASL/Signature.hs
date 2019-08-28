{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-|

This module contains types that describe signatures of ASL functions and
procedures. Procedures have side effects, while functions are side-effect free and
return a single value (which may be a tuple).

Top-level code sequences (like the @instExecute@ field of an instruction) have a trivial type
signature with no inputs (just global refs) and a set of outputs that is the union of all of the
locations touched by that function.

-}
module SemMC.ASL.Signature (
    FunctionSignature(..)
  , DependentFunctionSignature(..)
  , ProcedureSignature(..)
  , procSigRepr
  , SomeSignature(..)
  , someSigRepr
  , someSigName
  , SomeDFS(..)
  , DependentTypeRepr(..)
  , BaseGlobalVar(..)
  , SimpleFunctionSignature(..)
  , SimpleProcedureSignature(..)
  , SomeSimpleSignature(..)
  ) where

import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Generator as CCG
import qualified Lang.Crucible.Types as CT
import qualified What4.BaseTypes as WT
import           SemMC.ASL.Types
import qualified Language.ASL.Syntax as AS

-- | A 'FunctionSignature' describes the inputs and output of an ASL function.
--
-- An ASL function is side-effect free and returns a single value (which may be a
-- tuple). It takes as input a set of arguments, 'funcArgReprs', and a set of global
-- refs, 'funcGlobalReprs'.
data FunctionSignature globals init tp =
  FunctionSignature { funcName :: T.Text
                    -- ^ The name of the function
                    , funcSigRepr :: WT.BaseTypeRepr tp
                    -- ^ The return type of the function
                    , funcArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                    -- ^ The types of the natural arguments of the function
                    , funcGlobalReprs :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
                    -- ^ The globals referenced by the function; NOTE that we
                    -- assume that globals are read-only in functions
                    , funcTypeEnvir :: TypeEnvir
                    -- ^ The typing environment used to monomorphize this function
                    }
  deriving (Show)

data DependentFunctionSignature globals init tp =
  DependentFunctionSignature
  { pFuncName :: T.Text
  -- ^ The name of the function
  , pFuncSigRepr :: DependentTypeRepr init tp
  -- ^ The return type of the function (might reference an argument
  -- type)
  , pFuncArgReprs :: Ctx.Assignment (LabeledValue T.Text (DependentTypeRepr init)) init
  -- ^ The types of the natural arguments of the function
  , pFuncGlobalReprs :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
  -- ^ The globals referenced by the function; NOTE that we assume
  -- that globals are read-only in functions
  }
  deriving (Show)

data DependentTypeRepr init tp where
  DependentBaseRepr :: WT.BaseTypeRepr tp -> DependentTypeRepr init tp
  DependentBVRepr :: Ctx.Index init tp -> DependentTypeRepr init tp

deriving instance Show (DependentTypeRepr init tp)

instance ShowF (DependentTypeRepr init)



newtype BaseGlobalVar tp = BaseGlobalVar { unBaseVar :: CCG.GlobalVar (CT.BaseToType tp) }
  deriving (Show)

instance ShowF BaseGlobalVar

-- | A 'ProcedureSignature' captures the signature of an ASL procedure.
--
-- init are the non-global args
-- regs are the list of global registers
-- ret is the return type (actually a whole reg state, and basically congruent to regs)
data ProcedureSignature (globals :: Ctx.Ctx WT.BaseType)
                        (init :: Ctx.Ctx CT.CrucibleType) =
  ProcedureSignature { procName :: T.Text
                       --  The name of the procedure
                     , procArgReprs :: Ctx.Assignment (LabeledValue T.Text CT.TypeRepr) init
                       -- ^ The full repr for the natural arguments to the procedure
                       --
                       -- The arguments are the stated arguments (the @init@ type, which is the
                       -- list of explicit arguments)
                     , procGlobalReprs :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
                       -- ^ The globals possibly accessed by this procedure.
                       --
                       -- For now, we can always make it the full set of
                       -- globals; later, we can find a tighter bound.
                     , procTypeEnvir :: TypeEnvir
                       -- ^ The typing environment used to monomorphize this function
                       }
  deriving (Show)

-- | Compute the return type (repr) of a procedure in Crucible types (lifted from base types)
procSigRepr :: ProcedureSignature globals init -> CT.TypeRepr (CT.SymbolicStructType globals)
procSigRepr sig = CT.baseToType (WT.BaseStructRepr (FC.fmapFC projectValue (procGlobalReprs sig)))

data SomeSignature ret where
  SomeFunctionSignature :: FunctionSignature globals init tp -> SomeSignature (CT.BaseToType tp)
  SomeProcedureSignature :: ProcedureSignature globals init -> SomeSignature (CT.SymbolicStructType globals)

someSigRepr :: SomeSignature ret -> CT.TypeRepr ret
someSigRepr (SomeFunctionSignature fSig) = CT.baseToType (funcSigRepr fSig)
someSigRepr (SomeProcedureSignature pSig) = procSigRepr pSig

someSigName :: SomeSignature ret -> T.Text
someSigName (SomeFunctionSignature fSig) = funcName fSig
someSigName (SomeProcedureSignature pSig) = procName pSig

data SomeDFS where
  SomeDFS :: DependentFunctionSignature globals init tp -> SomeDFS

deriving instance Show (SomeSignature ret)

instance ShowF SomeSignature



-- | A 'SimpleFunctionSignature' describes the inputs and output of an ASL function.
-- 
-- Similar to a FunctionSignature but retains the syntactic types in order to
-- potentially represent multiple monomorphic variants
data SimpleFunctionSignature globals =
  SimpleFunctionSignature { sfuncName :: T.Text
                           -- ^ The name of the function
                           , sfuncRet :: [AS.Type]
                           -- ^ The return type of the function
                           , sfuncArgs :: [AS.SymbolDecl]
                           -- ^ The types of the natural arguments of the function
                           , sfuncGlobalReprs :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
                           -- ^ The globals referenced by the function; NOTE that we
                           -- assume that globals are read-only in functions
                           }
  deriving (Show)

-- | A 'ProcedureSignature' captures the signature of an ASL procedure.
--
-- init are the non-global args
-- regs are the list of global registers
-- ret is the return type (actually a whole reg state, and basically congruent to regs)
data SimpleProcedureSignature (globals :: Ctx.Ctx WT.BaseType) =
  SimpleProcedureSignature { sprocName :: T.Text
                       --  The name of the procedure
                           , sprocArgs :: [AS.SymbolDecl]
                             -- ^ The natural arguments to the procedure
                           , sprocGlobalReprs :: Ctx.Assignment (LabeledValue T.Text WT.BaseTypeRepr) globals
                             -- ^ The globals possibly accessed by this procedure.
                             --
                             -- For now, we can always make it the full set of
                             -- globals; later, we can find a tighter bound.
                             }
  deriving (Show)


data SomeSimpleSignature where
  SomeSimpleFunctionSignature :: SimpleFunctionSignature globals -> SomeSimpleSignature
  SomeSimpleProcedureSignature :: SimpleProcedureSignature globals -> SomeSimpleSignature


deriving instance Show (SomeSimpleSignature)
