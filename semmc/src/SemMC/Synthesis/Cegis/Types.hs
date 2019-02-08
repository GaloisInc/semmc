{-# LANGUAGE RankNTypes, TypeApplications, ScopedTypeVariables, KindSignatures,
  ViewPatterns, GADTs, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module SemMC.Synthesis.Cegis.Types
  ( -- * Cegis types
    CegisParams(..)
  , mkCegisParams
  , CegisState(..)
  , mkCegisState
  , Cegis(..)
  , runCegis
  , askSym
  , askSemantics
  , askTarget
  , askTests
  , askCheck
  , askMemExpr
  -- * Templatable Instructions
  , TemplatableInstruction(..)
  , templInsnToDism
  , tryExtractingConcrete
  -- * Concrete Tests
  , ConcreteTest(..)
  , LocExprs
  , concreteTestEq
  , isExistingTest
  ) where

import           Data.Kind (Type)
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans.Reader ( ReaderT(..), reader )
import           Control.Monad.Trans.State  (StateT(..), evalStateT, get)
import           Control.Monad.Trans.Class  (lift)
import           Data.Maybe ( isJust )

import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Classes as P
import qualified Data.Parameterized.HasRepr as HR

import qualified What4.Interface as S
import qualified What4.Expr as WE
import qualified What4.Symbol as WS
import qualified What4.Expr.GroundEval as GE
import qualified What4.SatResult as SAT

import qualified Dismantle.Instruction as D

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as L

import           SemMC.Formula
import qualified SemMC.Synthesis.Template as T


-- | Parameters given to call cegis.
data CegisParams sym arch =
  CegisParams { cpSym :: sym
              -- ^ The symbolic expression builder.
              , cpSemantics :: T.TemplatedSemantics sym arch
              -- ^ The base set of opcode semantics.
              , cpTarget :: Formula sym arch
              -- ^ The target formula we're trying to match.
              , cpUFEnv :: FormulaEnv sym arch
              -- ^ The uninterpreted functions in scope
              , cpMem :: S.SymExpr sym (A.MemType arch)
              -- ^ A symbolic expression representing the memory in all tests
              }

-- | Construct parameters for Cegis
mkCegisParams :: (S.IsSymExprBuilder sym, A.Architecture arch)
              => sym 
              -> T.TemplatedSemantics sym arch
              -- ^ The base set of opcode semantics
              -> Formula sym arch
              -- ^ The target formula we're trying to match
              -> FormulaEnv sym arch
              -- ^ The uninterpreted functions in scope
              -> IO (CegisParams sym arch)
mkCegisParams sym sem target ufEnv = case S.userSymbol "Mem" of
    Left  err       -> error (show $ WS.ppSolverSymbolError err)
    Right memSymbol -> do
      memExpr <- S.freshConstant sym memSymbol S.knownRepr
      return $ CegisParams { cpSym = sym
                           , cpSemantics = sem
                           , cpTarget = target
                           , cpUFEnv = ufEnv
                           , cpMem   = memExpr
                           }

mkCegisState :: (S.IsSymExprBuilder sym, A.Architecture arch)
             => sym
             -> CegisState sym arch
mkCegisState sym = CegisState { csTests = []
                             , csCheck = S.truePred sym
                             }



data CegisState sym arch =
  CegisState { csTests :: [ConcreteTest sym arch]
             -- ^ The tests so far
             , csCheck :: S.Pred sym
             -- ^ A predicate represnting the validity of the current tests
             }
newtype Cegis sym arch a = Cegis (ReaderT (CegisParams sym arch) (StateT (CegisState sym arch) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runCegis :: CegisParams sym arch -> CegisState sym arch -> Cegis sym arch a -> IO a
runCegis params st (Cegis op) = evalStateT (runReaderT op params) st

askSym :: Cegis sym arch sym
askSym = Cegis $ reader cpSym

askSemantics :: Cegis sym arch (T.TemplatedSemantics sym arch)
askSemantics = Cegis $ reader cpSemantics

askTarget :: Cegis sym arch (Formula sym arch)
askTarget = Cegis $ reader cpTarget

askTests :: Cegis sym arch [ConcreteTest sym arch]
askTests = Cegis . lift $ csTests <$> get

askCheck :: Cegis sym arch (S.Pred sym)
askCheck = Cegis . lift $ csCheck <$> get

askMemExpr :: Cegis sym arch (S.SymExpr sym (A.MemType arch))
askMemExpr = Cegis $ reader cpMem


-- ** Templatable instructions

-- | This is exactly a Dismantle 'Instruction', just with the dictionary of
-- constraints of a templatable opcode available.
data TemplatableInstruction (arch :: Type) where
  TemplatableInstruction :: A.Opcode arch (A.Operand arch) sh
                         -> SL.List (A.Operand arch) sh
                         -> TemplatableInstruction arch

instance (MapF.ShowF (A.Operand arch), MapF.ShowF (A.Opcode arch (A.Operand arch)))
      => Show (TemplatableInstruction arch) where
  show (TemplatableInstruction opcode operand) = MapF.showF opcode ++ " " ++ show operand


-- | Disregard the constraints.
templInsnToDism :: TemplatableInstruction arch -> A.Instruction arch
templInsnToDism (TemplatableInstruction op oplist) = D.Instruction op oplist



-- | Given a concrete model from the SMT solver, extract concrete instructions
-- from the templated instructions, so that all of the initially templated
-- operands are filled in concretely.
extractConcreteInstructions :: (A.ArchRepr arch)
                            => GE.GroundEvalFn t
                            -> [T.TemplatedInstructionFormula (WE.ExprBuilder t st fs) arch]
                            -> IO [TemplatableInstruction arch]
extractConcreteInstructions (GE.GroundEvalFn evalFn) = mapM f
  where f (T.TemplatedInstructionFormula (T.TemplatedInstruction op _ _) tf) =
          TemplatableInstruction op <$> T.recoverOperands (HR.typeRepr op) evalFn (T.tfOperandExprs tf)

-- | Meant to be used as the callback in a check SAT operation. If the result is
-- Sat, it pulls out concrete instructions corresponding to the SAT model.
-- Otherwise, it returns Nothing.
tryExtractingConcrete :: (A.ArchRepr arch)
                      => [T.TemplatedInstructionFormula (WE.ExprBuilder t st fs) arch]
                      -> SAT.SatResult (GE.GroundEvalFn t) a
                      -> IO (Maybe [TemplatableInstruction arch])
tryExtractingConcrete insns (SAT.Sat evalFn) = Just <$> extractConcreteInstructions evalFn insns
tryExtractingConcrete _ SAT.Unsat{} = return Nothing
tryExtractingConcrete _ SAT.Unknown = fail "got Unknown when checking sat-ness"



-- ** Tests

-- | Note that ArchState arch (S.SymExpr sym) = LocExprs sym (Location arch)
type LocExprs sym loc = MapF.MapF loc (S.SymExpr sym)

-- | Concrete input and output states of a formula. There's nothing in the types
-- that prevents the values from being symbolic, but please don't let them be!
data ConcreteTest sym arch =
  ConcreteTest { testInput  :: LocExprs sym (L.Location arch)
                , testOutput :: LocExprs sym (L.Location arch)
                , memInput   :: [A.AccessData sym arch]
                , memOutput  :: [A.AccessData sym arch]
                }

instance (A.Architecture arch, P.ShowF (S.SymExpr sym))
      => Show (ConcreteTest sym arch)
  where
    show test = "⟨\t" ++ show (testInput test)
                ++ "\n|||\t" ++ show (testOutput test)
                ++ "\n|||\t" ++ show (memInput test) 
                ++ "\n|||\t" ++ show (memOutput test) ++ "\n⟩"

-- | The equality of concrete tests only relies on their input
concreteTestEq :: forall sym arch.
                  (S.IsExprBuilder sym, A.Architecture arch)
               => ConcreteTest sym arch -> ConcreteTest sym arch -> Bool
concreteTestEq test1 test2 =
    concreteLocExprsEq (testInput test1) (testInput test2)
    && and (zipWith concreteAccessDataEq (memInput test1) (memInput test2))
  where
    concreteLocExprsEq :: S.TestEquality loc => LocExprs sym loc -> LocExprs sym loc -> Bool
    concreteLocExprsEq (MapF.toList -> l1) (MapF.toList -> l2) =
        length l1 == length l2
        && and (zipWith (\(MapF.Pair loc1 v1) (MapF.Pair loc2 v2) ->
                            isJust (S.testEquality loc1 loc2) && concreteSymExprEq v1 v2)
                l1 l2)

    concreteAccessDataEq :: A.AccessData sym arch -> A.AccessData sym arch -> Bool
    concreteAccessDataEq (A.ReadData idx1) (A.ReadData idx2) = concreteSymExprEq idx1 idx2
    concreteAccessDataEq (A.WriteData idx1 v1) (A.WriteData idx2 v2) =
        concreteSymExprEq idx1 idx2 && concreteSymExprEq v1 v2
    concreteAccessDataEq _ _ = False

    concreteSymExprEq :: S.SymExpr sym tp1 -> S.SymExpr sym tp2 -> Bool
    concreteSymExprEq e1 e2 | Just c1 <- S.asConcrete e1, Just c2 <- S.asConcrete e2 =
        isJust (S.testEquality c1 c2)
                            | otherwise = False

isExistingTest :: (A.Architecture arch, S.IsExprBuilder sym)
               => ConcreteTest sym arch
               -> Cegis sym arch Bool
isExistingTest test = do
  tests <- askTests
  return . and $ not . concreteTestEq test <$> tests
