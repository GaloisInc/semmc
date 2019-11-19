{-# LANGUAGE RankNTypes, TypeApplications, ScopedTypeVariables, TypeFamilies,
  DataKinds, AllowAmbiguousTypes #-}

module SemMC.Synthesis.Cegis.EvalFormula
  ( LocEval
  , mkEvalLoc
  , mkMemEvalLoc
  , lookupInState
  , evalExpression
  , evalFormula
  , evalFormula'
  , evalFormulaMem
  , evalFormulaMem'
  , condenseInstructions
  ) where

import           Control.Monad.IO.Class ( liftIO )

import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.TraversableF as TF

import qualified Lang.Crucible.Backend as CB
import qualified What4.Interface as S
import qualified What4.Expr as WE

import           SemMC.Formula
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L

import qualified SemMC.Synthesis.Template as Temp
import qualified SemMC.Synthesis.Cegis.ReadWriteEval as RW
import qualified SemMC.Synthesis.Cegis.Types as T

    
-- A data structure representing functions from locations to expressions
data LocEval loc expr =
  LocEval { evalLoc :: forall (tp :: S.BaseType). loc tp -> IO (expr tp) }

-- | Convert an ArchState (a finite map from locations to expressions) into a
-- total function that maps locations @l@ to their default value
-- for that location.
mkEvalLoc :: forall arch sym.
                (S.IsExprBuilder sym, A.Architecture arch)
             => sym
             -> L.ArchState arch (S.SymExpr sym)
             -> LocEval (L.Location arch) (S.SymExpr sym)
mkEvalLoc sym st = LocEval $ lookupInState sym st

-- | Look up the given location in the given machine state. If the location is
-- 'Mem', return the mem-expression. Otherwise, if the location is not found, return the
-- default value for that location.
mkMemEvalLoc :: forall arch sym.
                    (S.IsExprBuilder sym, A.Architecture arch)
                 => sym
                 -> T.LocExprs sym (L.Location arch)
                 -> S.SymExpr sym (A.MemType arch)
                 -> LocEval (L.Location arch) (S.SymExpr sym)
mkMemEvalLoc sym st memExpr = LocEval $ \loc ->
  case (L.isMemoryLocation loc, S.testEquality (L.locationType loc) (A.memTypeRepr @arch)) of
    (True, Just S.Refl) -> return memExpr
    (_, _) -> evalLoc (mkEvalLoc @arch sym st) loc

-- | Look up the given location in the given machine state. If the location is
-- not found, return the default value for that location.
lookupInState :: forall sym loc.
                 (S.IsExprBuilder sym,
                  L.IsLocation loc)
              => sym
              -> T.LocExprs sym loc
              -> forall tp. loc tp -> IO (S.SymExpr sym tp)
lookupInState sym st loc =
    case MapF.lookup loc st of
      Just e  -> return e
      Nothing -> L.defaultLocationExpr sym loc


-- | Evaluate an expression, substituting in the location values given in the
-- machine state.
evalExpression :: forall arch t st fs tp.
                  A.Architecture arch
               => WE.ExprBuilder t st fs
               -> MapF.MapF (L.Location arch) (WE.ExprBoundVar t)
               -> T.LocExprs (WE.ExprBuilder t st fs) (L.Location arch)
               -> WE.Expr t tp
               -> IO (WE.Expr t tp)
evalExpression sym vars state = evalExpression' sym vars (mkEvalLoc @arch sym state)


evalExpression' :: (L.IsLocation loc)
               => WE.ExprBuilder t st fs
               -> MapF.MapF loc (WE.ExprBoundVar t)
               -> LocEval loc (WE.Expr t)
               -> WE.Expr t tp
               -> IO (WE.Expr t tp)
evalExpression' sym vars (LocEval el) e = replaceLitVars sym el vars e

-- | Evaluate the formula, replacing locations @l@ with @el l@.
evalFormula' :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                , A.Architecture arch
                )
             => sym
             -> Formula sym arch
             -> LocEval (L.Location arch) (WE.Expr t)
             -> IO (Formula sym arch)
evalFormula' sym (Formula vars defs) el = do
  defs' <- TF.traverseF (evalExpression' sym vars el) defs
  return (Formula vars defs')


-- | instantiate all calls to read_mem in the formula
simplifyReadMem' :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                , A.Architecture arch
                )
             => sym
             -> Formula sym arch
             -> LocEval (L.Location arch) (WE.Expr t)
             -> IO (Formula sym arch)
simplifyReadMem' sym f@(Formula vars defs) (LocEval el) = do
  defs' <- TF.traverseF (RW.instantiateReadMem sym f el) defs
  return (Formula vars defs')

-- | Evaluate the formula, replacing locations @l@ with @el l@, and
-- simplifying occurrences of @read_mem@ in the formula.
evalFormulaMem' :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                , A.Architecture arch
                )
             => sym
             -> Formula sym arch
             -> LocEval (L.Location arch) (WE.Expr t)
             -> IO (Formula sym arch)
evalFormulaMem' sym f el = do
  f' <- evalFormula' sym f el
  simplifyReadMem' sym f' el


-- | Evaluate the formula with respect to @st@, replacing location @Mem@ with
-- the underlying mem expression of the monad, and replacing other locations @l@
-- with the default value for that location. In addition, simplify occurrences of
-- @read_mem_x@ in the formula.
evalFormulaMem :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
                , A.Architecture arch
                )
             => Formula sym arch
             -> L.ArchState arch (WE.Expr t)
             -> T.Cegis sym arch (Formula sym arch)
evalFormulaMem f st = do
  sym <- T.askSym
  memExpr <- T.askMemExpr
  let locEval = mkMemEvalLoc @arch sym st memExpr
  liftIO $ evalFormulaMem' sym f locEval

-- | Evaluate the formula with respect to @st@, replacing locations @l@
-- with the default value for that location.
evalFormula :: forall arch t st fs sym.
             ( sym ~ WE.ExprBuilder t st fs, CB.IsSymInterface sym
             , A.Architecture arch
             )
             => Formula sym arch
             -> L.ArchState arch (WE.Expr t)
             -> T.Cegis sym arch (Formula sym arch)
evalFormula f st = do
  sym <- T.askSym
  let locEval = mkEvalLoc @arch sym st
  liftIO $ evalFormula' sym f locEval


-- | Build a formula corresponding to the semantics of the given concrete instruction.
instantiateFormula' :: A.Architecture arch
                    => T.TemplatableInstruction arch
                    -> T.Cegis (WE.ExprBuilder t st fs) arch (Formula (WE.ExprBuilder t st fs) arch)
instantiateFormula' (T.TemplatableInstruction op oplist) = do
  sym <- T.askSym
  semantics <- T.askSemantics
  case MapF.lookup op semantics of
    Nothing -> error ("Missing semantics for opcode " ++ MapF.showF op)
    Just x  -> do
      (_,f) <- liftIO $ instantiateFormula sym (Temp.unTemplate x) oplist
      return f

-- | Condense a series of instructions in sequential execution into one formula
-- describing the semantics of the block.
condenseInstructions :: A.Architecture arch
                     => [T.TemplatableInstruction arch]
                     -> T.Cegis (WE.ExprBuilder t st fs) arch (Formula (WE.ExprBuilder t st fs) arch)
condenseInstructions insns = do
  sym <- T.askSym
  insnFormulas <- traverse instantiateFormula' insns
  liftIO $ condenseFormulas sym insnFormulas
