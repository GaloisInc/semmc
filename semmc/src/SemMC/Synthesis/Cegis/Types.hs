{-# LANGUAGE RankNTypes, InstanceSigs, TypeApplications, ScopedTypeVariables,
  KindSignatures, ViewPatterns, GADTs, FlexibleContexts, UndecidableInstances,
  GeneralizedNewtypeDeriving #-}

module SemMC.Synthesis.Cegis.Types
  ( -- * Cegis types
    CegisParams(..)
  , mkCegisParams
  , CegisState(..)
  , emptyCegisState
  , Cegis(..)
  , runCegis
  , HasSym(..)
  , askSemantics
  , askTarget
  , askTests
  , askCheck
  , putCheck
  , HasMemExpr(..)
  -- * Templatable Instructions
  , TemplatableInstruction(..)
  , templInsnToDism
  , extractConcreteInstructions
--  , tryExtractingConcrete
  , tryExtractingConcreteWithParamsCheck
  -- * Concrete Tests
  , ConcreteTest(..)
  , LocExprs
  , andPred
  ) where

import           Data.Kind (Type)
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans.Reader ( ReaderT(..), reader)
import           Control.Monad.Trans.State  (StateT(..), evalStateT, get, put)
import           Control.Monad.Trans.Class  (lift)
import           Data.Proxy (Proxy(..))
import           Control.Monad (join)
import           Data.Foldable (toList, foldrM)

import           Data.Parameterized.Some (Some(..))
import qualified Data.Parameterized.TraversableFC as FC
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

import qualified SemMC.Util as U
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
              , cpMem :: S.SymExpr sym (A.MemType arch)
              -- ^ A symbolic expression representing the memory, to be used in
              -- concrete test checks
              }

class HasSym m where
  askSym :: m sym arch sym
class HasMemExpr m where
  askMemExpr :: m sym arch (S.SymExpr sym (A.MemType arch))

-- | Construct parameters for Cegis
mkCegisParams :: (S.IsSymExprBuilder sym, A.Architecture arch)
              => sym 
              -> T.TemplatedSemantics sym arch
              -- ^ The base set of opcode semantics
              -> Formula sym arch
              -- ^ The target formula we're trying to match
              -> IO (CegisParams sym arch)
mkCegisParams sym sem target = case S.userSymbol "Mem" of
    Left  err       -> error (show $ WS.ppSolverSymbolError err)
    Right memSymbol -> do
      memExpr <- S.freshConstant sym memSymbol S.knownRepr
      return $ CegisParams { cpSym = sym
                           , cpSemantics = sem
                           , cpTarget = target
                           , cpMem   = memExpr
                           }

emptyCegisState :: (S.IsSymExprBuilder sym, A.Architecture arch)
             => sym
             -> CegisState sym arch
emptyCegisState sym = CegisState { csTests = []
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

instance HasSym Cegis where
  askSym = Cegis $ reader cpSym

askSemantics :: Cegis sym arch (T.TemplatedSemantics sym arch)
askSemantics = Cegis $ reader cpSemantics

askTarget :: Cegis sym arch (Formula sym arch)
askTarget = Cegis $ reader cpTarget

askTests :: Cegis sym arch [ConcreteTest sym arch]
askTests = Cegis . lift $ csTests <$> get

askCheck :: Cegis sym arch (S.Pred sym)
askCheck = Cegis . lift $ csCheck <$> get

putCheck :: S.Pred sym -> Cegis sym arch ()
putCheck check = Cegis . lift $ do
  st <- get
  put st{csCheck = check}

instance HasMemExpr Cegis where
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
-- Sat, it pulls out concrete instructions corresponding to the SAT model, and
-- also constructs a predicate saying: for all immediate parameters @i@ occurring
-- in the templated instructions, @i <> model(i)@. This check is used in future
-- satisfiability checks to ensure we never generate duplicate models.
tryExtractingConcreteWithParamsCheck :: forall arch t st fs a sym.
                                  ( sym ~ WE.ExprBuilder t st fs
                                  , A.ArchRepr arch, A.Architecture arch, A.Architecture (T.TemplatedArch arch))
                               => sym
                               -> [T.TemplatedInstructionFormula sym arch]
                               -> SAT.SatResult (GE.GroundEvalFn t) a
                               -> IO (Maybe ([TemplatableInstruction arch], S.Pred sym))
tryExtractingConcreteWithParamsCheck _ _ SAT.Unsat{} = return Nothing
tryExtractingConcreteWithParamsCheck _ _ SAT.Unknown = return Nothing
tryExtractingConcreteWithParamsCheck sym tInsns (SAT.Sat model) = do
  cInsns <- extractConcreteInstructions model tInsns
  let params = join $ mapM tInsnToParams tInsns
  paramsCheck <- liftIO $ andPred sym params $ \(Some e) -> do
    gv <- WE.groundEval @t model e
    v <- U.groundValToExpr sym [] (S.exprType e) gv
    S.notPred sym =<< S.isEq sym e v
  return (Just (cInsns, paramsCheck))
  where
    tInsnToParams :: T.TemplatedInstructionFormula sym arch
                  -> [Some (S.SymExpr sym)]
    tInsnToParams (T.TemplatedInstructionFormula _ tf) =
      join $ FC.toListFC tExprToImm (T.tfOperandExprs tf)

    tExprToImm :: A.TaggedExpr (T.TemplatedArch arch) sym x
               -> [Some (WE.Expr t)]
    tExprToImm tExpr = toList $ A.taggedExprImmediate (Proxy @sym) tExpr


-- ** Tests

-- | Note that ArchState arch (S.SymExpr sym) = LocExprs sym (Location arch)
type LocExprs sym loc = MapF.MapF loc (S.SymExpr sym)

-- | Concrete input and output states of a formula. There's nothing in the types
-- that prevents the values from being symbolic, but please don't let them be!
-- The @memInput@ and @memOutput@ fields record the input and output state of
-- memory, respectively, as a list of writes.
--
-- That is, a concrete test makes the assertion that if registers r_i map to
-- concrete values c_i as specified by the @testInput@, and if the memory
-- initially has the writes given by @memInput@, then the output will see
-- registers having values indicated by @testOutput@, and memory will perform
-- the writes specified by @memOutput@.
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

-- | Take the conjunction of (f a) for each a in some foldable data structure
andPred :: forall t sym m a. (Foldable t, S.IsExprBuilder sym, MonadIO m)
        => sym -> t a -> (a -> m (S.Pred sym)) -> m (S.Pred sym)
andPred sym ls f = foldrM go (S.truePred sym) ls
  where
    go :: a -> S.Pred sym -> m (S.Pred sym)
    go accA accP = f accA >>= liftIO . (S.andPred sym accP)
