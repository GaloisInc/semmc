{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module SemMC.Synthesis.Cegis
  ( evalFormula
  , cegis
  ) where

import           Control.Monad.IO.Class ( MonadIO(..) )
import           Data.Foldable
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.TraversableF
import           System.IO ( stderr )

import           Lang.Crucible.BaseTypes
import           Lang.Crucible.Config (initialConfig, setConfigValue)
import           Lang.Crucible.Solver.Adapter
import qualified Lang.Crucible.Solver.Interface as S
import           Lang.Crucible.Solver.SatResult
import qualified Lang.Crucible.Solver.SimpleBackend as S
import           Lang.Crucible.Solver.SimpleBackend.Z3
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import           Lang.Crucible.Utils.MonadVerbosity (withVerbosity)

import           SemMC.Architecture
import           SemMC.Formula

foldlMWithKey :: forall k a b m. (Monad m) => (forall s. b -> k s -> a s -> m b) -> b -> MapF.MapF k a -> m b
foldlMWithKey f z0 m = MapF.foldrWithKey f' return m z0
  where f' :: forall s. k s -> a s -> (b -> m b) -> b -> m b
        f' k x c z = f z k x >>= c

buildLitAssignment :: forall sym loc.
                      sym
                   -> (forall tp. loc tp -> IO (S.SymExpr sym tp))
                   -> MapF.MapF loc (S.BoundVar sym)
                   -> IO (MapF.Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym)))
buildLitAssignment _ exprLookup = foldlMWithKey f (MapF.Pair Ctx.empty Ctx.empty)
  where f :: forall (tp :: BaseType).
             MapF.Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym))
          -> loc tp
          -> S.BoundVar sym tp
          -> IO (MapF.Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym)))
        f (MapF.Pair varAssn exprAssn) loc var =
          fmap (\expr -> MapF.Pair (Ctx.extend varAssn var) (Ctx.extend exprAssn expr))
               (exprLookup loc)

-- TODO: This was mostly ripped from SemMC.Formula.Instantiate, and we should
-- generalize that rather than copy-and-pasting here.
replaceLitVars :: forall loc t st tp.
                  (OrdF loc)
               => S.SimpleBuilder t st
               -> (forall tp'. loc tp' -> IO (S.Elt t tp'))
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> S.Elt t tp
               -> IO (S.Elt t tp)
replaceLitVars sym newExprs oldVars expr =
  buildLitAssignment sym newExprs oldVars >>=
    \(MapF.Pair varAssn exprAssn) -> S.evalBoundVars sym expr varAssn exprAssn

type MachineState sym loc = MapF.MapF loc (S.SymExpr sym)

lookupInState :: forall sym loc tp.
                 (S.IsExprBuilder sym,
                  IsLocation loc)
              => sym
              -> MachineState sym loc
              -> loc tp
              -> IO (S.SymExpr sym tp)
lookupInState sym st loc = maybe (defaultLocationExpr sym loc) return $ MapF.lookup loc st

evalFormula :: (Architecture arch)
            => S.SimpleBuilder t st
            -> Formula (S.SimpleBuilder t st) arch
            -> ArchState (S.SimpleBuilder t st) arch
            -> IO (ArchState (S.SimpleBuilder t st) arch)
evalFormula sym (Formula _ vars defs) input =
  traverseF (replaceLitVars sym (lookupInState sym input) vars) defs

-- XXX: make sure that all locations are actually checked correctly, i.e., in
-- the case where there are different locations in the state map than in the
-- defs map
buildEquality' :: (IsLocation loc)
               => S.SimpleBuilder t st
               -> (MachineState (S.SimpleBuilder t st) loc, MachineState (S.SimpleBuilder t st) loc)
               -> MapF.MapF loc (S.SimpleBoundVar t)
               -> loc tp
               -> S.Elt t tp
               -> IO (S.BoolElt t)
buildEquality' sym (input, output) vars outputLoc expr = do
  actuallyIs <- replaceLitVars sym (lookupInState sym input) vars expr
  let shouldBe = maybe (error "outputLoc wasn't in output state") id $ MapF.lookup outputLoc output
  S.isEq sym actuallyIs shouldBe

buildEquality :: (Architecture arch)
              => S.SimpleBuilder t st
              -> (ArchState (S.SimpleBuilder t st) arch, ArchState (S.SimpleBuilder t st) arch)
              -> Formula (S.SimpleBuilder t st) arch
              -> IO (S.BoolElt t)
buildEquality sym test (Formula uses vars defs) = foldlMWithKey f (S.truePred sym) defs
  where f pred loc e = putStrLn (showF loc) >> print e >> (S.andPred sym pred =<< buildEquality' sym test vars loc e)

cegis :: (Architecture arch)
      => S.SimpleBackend t
      -> Formula (S.SimpleBackend t) arch
      -> [(ArchState (S.SimpleBackend t) arch, ArchState (S.SimpleBackend t) arch)]
      -> Formula (S.SimpleBackend t) arch
      -> IO (Maybe [Instruction arch])
cegis sym target tests trial = do
  -- initial dumb thing: return Just [] if all the tests are satisfiable
  pred <- foldrM (\test pred -> S.andPred sym pred =<< buildEquality sym test trial) (S.truePred sym) tests

  print pred

  result <- withVerbosity stderr 1 $ do
    cfg <- liftIO $ initialConfig 1 z3Options
    setConfigValue z3Path cfg "/usr/local/bin/z3"
    liftIO $ solver_adapter_check_sat z3Adapter sym cfg (const . const $ return ()) pred return

  putStr "will this be sat? ..."

  case result of
    Sat _ -> putStrLn "yup!" >> return (Just [])
    Unsat -> putStrLn "nope." >> return Nothing
    Unknown -> fail "Got Unknown result when checking sat-ness"
