{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Synthesis.Cegis
  ( evalFormula
  , ConcreteTest
  , initTest
  , CegisParams(..)
  , CegisResult(..)
  , cegis
  ) where

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Reader ( ReaderT(..), reader )
import           Data.Foldable
import           Data.Maybe ( fromJust, listToMaybe )
import qualified Data.Map as Map
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.HasRepr as HR
import           Data.Parameterized.Some ( Some(..), viewSome, mapSome, traverseSome )
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Classes as P
import           GHC.TypeNats (KnownNat)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Proxy (Proxy(..))
import           Debug.Trace (trace)

import qualified Lang.Crucible.Backend.Online as CBO
import qualified What4.Interface as S
import           What4.SatResult
import           What4.Expr.GroundEval
import qualified What4.Expr as WE
import qualified What4.Expr.Builder as WB
import qualified What4.Protocol.Online as WPO
import qualified What4.Symbol as WS

import           Dismantle.Instruction

import           SemMC.Architecture
import qualified SemMC.Architecture.Location as L
import           SemMC.Formula
import           SemMC.Synthesis.Template
import           SemMC.Formula.MemAccesses
import           SemMC.Formula.Env

-- | This is exactly a Dismantle 'Instruction', just with the dictionary of
-- constraints of a templatable opcode available.
data TemplatableInstruction (arch :: *) where
  TemplatableInstruction :: Opcode arch (Operand arch) sh
                         -> SL.List (Operand arch) sh
                         -> TemplatableInstruction arch

instance (MapF.ShowF (Operand arch), MapF.ShowF (Opcode arch (Operand arch)))
      => Show (TemplatableInstruction arch) where
  show (TemplatableInstruction opcode operand) = MapF.showF opcode ++ " " ++ show operand


-- | Disregard the constraints.
templInsnToDism :: TemplatableInstruction arch -> Instruction arch
templInsnToDism (TemplatableInstruction op oplist) = Instruction op oplist

-- | Note that ArchState arch (S.SymExpr sym) = LocExprs sym (Location arch)
type LocExprs sym loc = MapF.MapF loc (S.SymExpr sym)

-- | Look up the given location in the given machine state. If the location is
-- not found, return the default value for that location.
lookupInState :: forall sym loc tp.
                 (S.IsExprBuilder sym,
                  IsLocation loc)
              => sym
              -> LocExprs sym loc
              -> loc tp
              -> IO (S.SymExpr sym tp)
lookupInState sym st loc =
  maybe (defaultLocationExpr sym loc) return $ MapF.lookup loc st

-- | Parameters given to call cegis.
data CegisParams sym arch =
  CegisParams { cpSym :: sym
              -- ^ The symbolic expression builder.
              , cpSemantics :: TemplatedSemantics sym arch
              -- ^ The base set of opcode semantics.
              , cpTarget :: Formula sym arch
              -- ^ The target formula we're trying to match.
              , cpUFEnv :: FormulaEnv sym arch
              -- ^ The uninterpreted functions in scope
              }

type Cegis sym arch = ReaderT (CegisParams sym arch) IO

askSym :: Cegis sym arch sym
askSym = reader cpSym

askUF :: Cegis sym arch (FormulaEnv sym arch)
askUF = reader cpUFEnv

askSemantics :: Cegis sym arch (TemplatedSemantics sym arch)
askSemantics = reader cpSemantics

askTarget :: Cegis sym arch (Formula sym arch)
askTarget = reader cpTarget

-- | Evaluate an expression, substituting in the location values given in the
-- machine state.
evalExpression' :: (IsLocation loc)
               => WE.ExprBuilder t st fs
               -> MapF.MapF loc (WE.ExprBoundVar t)
               -> LocExprs (WE.ExprBuilder t st fs) loc
               -> WE.Expr t tp
               -> IO (WE.Expr t tp)
evalExpression' sym vars state expr = 
  replaceLitVars sym (lookupInState sym state) vars expr

evalExpression :: (IsLocation loc)
               => MapF.MapF loc (WE.ExprBoundVar t)
               -> LocExprs (WE.ExprBuilder t st fs) loc
               -> WE.Expr t tp
               -> Cegis (WE.ExprBuilder t st fs) arch (WE.Expr t tp)
evalExpression vars state expr = do
  sym <- askSym
  liftIO $ evalExpression' sym vars state expr


-- | Evaluate a whole formula, substituting in the location values given in the
-- machine state, returning the transformed machine state.
evalFormula :: (Architecture arch)
            => Formula (WE.ExprBuilder t st fs) arch
            -> L.ArchState arch (WE.Expr t)
            -> Cegis (WE.ExprBuilder t st fs) arch (L.ArchState arch (WE.Expr t))
evalFormula (Formula vars defs) input = do
  traverseF (evalExpression vars input) defs

evalFormula' :: (Architecture arch)
             => WE.ExprBuilder t st fs
             -> Formula (WE.ExprBuilder t st fs) arch
             -> L.ArchState arch (WE.Expr t)
             -> IO (L.ArchState arch (WE.Expr t))
evalFormula' sym (Formula vars defs) input = traverseF (evalExpression' sym vars input) defs


-- | Concrete input and output states of a formula. There's nothing in the types
-- that prevents the values from being symbolic, but please don't let them be!
data ConcreteTest' sym loc =
  ConcreteTest' { testInput  :: LocExprs sym loc
                , testOutput :: LocExprs sym loc
                , memInput   :: Map.Map (Some (S.SymExpr sym)) (Some (S.SymExpr sym))
                , memOutput  :: Map.Map (Some (S.SymExpr sym)) (Some (S.SymExpr sym))
                }

-- nonMemTestInput :: P.OrdF loc
--                => L.MemLoc loc -> ConcreteTest' sym loc -> LocExprs sym loc
--nonMemTestInput (L.MemLoc _ mem) test = MapF.delete mem $ testInput test

type ConcreteTest sym arch = ConcreteTest' sym (Location arch)
instance (MapF.ShowF loc, MapF.ShowF (S.SymExpr sym)) 
      => Show (ConcreteTest' sym loc) 
  where
    show test = "⟨\t" ++ show (testInput test)
                ++ "\n|||\t" ++ show (testOutput test) 
                ++ "\n|||\t" ++ show (memInput test) 
                ++ "\n|||\t" ++ show (memOutput test) ++ "\n⟩"

-- Returns a 'LocExprs' data structure that maps all locations to a default value.
defaultLocExprs :: forall loc sym. 
                ( L.IsLocation loc 
                , MapF.OrdF loc 
                , S.IsExprBuilder sym
                )
               => sym
               -> IO (LocExprs sym loc) -- MapF.MapF loc (S.SymExpr sym)
defaultLocExprs sym = do locExprList <- mapM pairDefault (L.allLocations @loc)
                         return $ MapF.fromList locExprList
  where
    pairDefault :: Some loc -> IO (MapF.Pair loc (S.SymExpr sym))
    pairDefault (Some l) = MapF.Pair l <$> L.defaultLocationExpr sym l


-- | Given a formula and a counterexample provided from the solver, construct
-- the concrete test illustrated by the counterexample.
--
-- TODO: adapt this to deal with the case when the architecture has several memory locations (e.g. A32)
mkTest :: forall sym arch t st fs.
          (Architecture arch, sym ~ WE.ExprBuilder t st fs)
       => sym
       -> Formula sym arch
       -> L.ArchState arch (S.SymExpr sym)
       -> IO (ConcreteTest sym arch)
mkTest sym targetFormula ctrExample 
  | [L.MemLoc _ mem] <- memLocation @(L.Location arch) = do
    -- putStrLn $ "Constructing test from " ++ show ctrExample
    -- the testInput is exactly the counter example for non-memory locations
    let testInput' = MapF.delete mem ctrExample

    -- substitute the non-memory locations from the test into the target
    -- formula. For non-memory locations, this gives us the testOutput.
    ctrExampleOut <- evalFormula' sym targetFormula testInput'
    let testOutput' = MapF.delete mem ctrExampleOut

    -- to construct the memInput/memOutput, we need to find all memory locations that
    -- occur in the input/output counterexamples respectively and record the values they map to.
    let inputAddrMap  = liveMemMap $ Formula @sym @arch (formParamVars targetFormula) ctrExample
        outputAddrMap = liveMemMap $ Formula @sym @arch (formParamVars targetFormula) ctrExampleOut

    -- putStrLn $ "Test: " ++ show (ConcreteTest' @sym testInput' testOutput' inputAddrMap outputAddrMap)
    return $ ConcreteTest' testInput' testOutput' inputAddrMap outputAddrMap
mkTest _ _ _ | otherwise = error "Cannot make test for this architecture"

-- | Construct an initial test from concrete values
initTest :: (L.IsLocation (Location arch), Architecture arch)
         => WE.ExprBuilder t st fs
         -> Formula (WE.ExprBuilder t st fs) arch
         -> IO (ConcreteTest (WE.ExprBuilder t st fs) arch)
initTest sym f = do locExprs <- defaultLocExprs sym
                    mkTest sym f locExprs

{-
-- | Given a formula and a concrete test, replace all non-memory locations in
-- the formula with their bindings from the test
substituteNonMem :: forall sym t st fs arch.
                    (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                 => sym
                 -> Formula sym arch
                 -> ConcreteTest' sym (L.Location arch)
                 -> IO (Formula sym arch)
substituteNonMem sym f test = Formula (formParamVars f) <$> traverseF go (formDefs f)
  where
    go :: S.SymExpr sym s -> IO (S.SymExpr sym s)
    go e = evalExpression' sym (formParamVars f) (testInput test) e
-}


-- | Build an equality expression for the given location, under the given
-- concrete test, of the form 
-- > e[i ↦ testInput(i)] = testOutput(l)]
-- What this means is all of the machine state variables have
-- been filled in, and the resulting expression is set equal to the known "real"
-- output value.
--
-- For example (simplified): if the ConcreteTest is
-- @
--   ConcreteTest { testInput = { r2 -> 5, r3 -> 7 }
--                , testOutput = { r2 -> 5, r3 -> 10 }
--                }
-- @
-- , the location is @r3@, and the expression is @Just (3*r2 + imm5)@, this
-- function will generate the expression @3*5 + imm5 = 10@. If the expression
-- were instead @Nothing@, this would generate @7 = 10@.
buildEqualityLocation :: (IsLocation loc, P.ShowF loc)
                      => WE.ExprBuilder t st fs
                      -> ConcreteTest' (WE.ExprBuilder t st fs) loc
                      -> MapF.MapF loc (WE.ExprBoundVar t)
                      -- ^ The bound variables representing the input values for
                      -- each location.
                      -> loc tp
                      -- ^ The location for which we're building the equality.
                      -> Maybe (WE.Expr t tp)
                      -- ^ If 'Just', the symbolic representation of the new
                      -- definition of this location. If 'Nothing', then assume
                      -- the identity transformation.
                      -> IO (WE.Expr t tp, WE.Expr t tp)
buildEqualityLocation sym test vars outputLoc expr = do
  actuallyIs <- case expr of
                  Just expr' -> evalExpression' sym vars (testInput test) expr'
                  -- If this location isn't present in the definitions of the
                  -- candidate formula, then its original value is preserved.
                  Nothing -> liftIO $ lookupInState sym (testInput test) outputLoc
  shouldBe <- liftIO $ lookupInState sym (testOutput test) outputLoc
--  liftIO . putStrLn $ "Built equation for location " ++ P.showF outputLoc 
--  liftIO . putStrLn $ "FOR test " ++ show test
--  liftIO . putStrLn $ "WITH expression " ++ show expr
--  liftIO . putStrLn $ "ACTUALLY IS:\t" ++ show actuallyIs
--  liftIO . putStrLn $ "SHOULD BE:\t" ++ show shouldBe
  return (actuallyIs, shouldBe)


-- | Build a conjuction of the equality expressions for all registers given by
-- the input list of locations.
buildEqualityLocations :: forall arch t st fs
                       . (Architecture arch)
                       => WE.ExprBuilder t st fs
                       -> Formula (WE.ExprBuilder t st fs) arch
                       -> ConcreteTest (WE.ExprBuilder t st fs) arch
                       -> Set.Set (Some (Location arch))
                       -> IO (WE.BoolExpr t)
buildEqualityLocations sym (Formula vars defs) test locs = do
  let addEquality :: WE.BoolExpr t -> Some (Location arch) -> IO (WE.BoolExpr t)
      addEquality soFar (Some loc) = do
        let locDef = MapF.lookup loc defs
        (actuallyIs,shouldBe) <- buildEqualityLocation sym test vars loc locDef
        locEquality <- liftIO $ S.isEq sym actuallyIs shouldBe
        liftIO $ S.andPred sym soFar locEquality
  foldlM addEquality (S.truePred sym) locs

{-

-- | Build a conjuction of the equality expressions for all the
-- indices/locations in memory touched by the input formula
buildEqualityMem :: forall arch t st fs
                 . (Architecture arch)
                 => Formula (WE.ExprBuilder t st fs) arch
                 -> ConcreteTest (WE.ExprBuilder t st fs) arch
                 -> Maybe (L.MemLoc (L.Location arch))
                 -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
buildEqualityMem _ _ Nothing = do sym <- askSym
                                  return $ S.truePred sym
buildEqualityMem f@(Formula vars defs) test (Just (L.MemLoc w mem)) = do
  sym <- askSym
  let locDef = MapF.lookup mem defs
  (actualMem,targetMem) <- buildEqualityLocation test vars mem locDef
  liftIO . putStrLn $ "ACTUALLY IS:\t" ++ show actualMem
  liftIO . putStrLn $ "SHOULD BE:\t" ++ show targetMem
  let indices = liveMemInExpr f actualMem `Set.union` liveMemInExpr f targetMem
  liftIO . putStrLn $ "INDICES: " ++ show indices
  let addEquality :: WE.BoolExpr t -> Integer -> IO (WE.BoolExpr t)
      addEquality soFar i = do
        iExp        <- S.bvLit sym w i
        actualMem_i <- S.arrayLookup sym actualMem (Ctx.Empty Ctx.:> iExp)
        targetMem_i <- S.arrayLookup sym targetMem (Ctx.Empty Ctx.:> iExp)
        liftIO . putStrLn $ "ACTUALLY IS[" ++ show i ++ "]:\t" ++ show actualMem_i
        liftIO . putStrLn $ "SHOULD BE[" ++ show i ++ "]:\t" ++ show targetMem_i
        locEquality <- S.isEq sym actualMem_i targetMem_i
--        liftIO . putStrLn $ "actually=should:\t" ++ show locEquality
        S.andPred sym soFar locEquality
  liftIO $ foldlM addEquality (S.truePred sym) indices
-}
buildEqualityMem :: forall arch t st fs sym
                 . (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                 => sym
                 -> Formula sym arch
                 -> ConcreteTest sym arch
                 -> Maybe (L.MemLoc (L.Location arch))
                 -> IO (WE.BoolExpr t)
buildEqualityMem sym _ _ Nothing = return $ S.truePred sym
buildEqualityMem sym f@(Formula vars defs) test (Just (L.MemLoc w mem)) = do
  let locDef = MapF.lookup mem defs
--  putStrLn $ "\n\nTEST: " ++ show test
--  putStrLn $ "Formula: " ++ show f
--  putStrLn $ "LocDef: " ++ show locDef
  (actualMem,targetMem) <- buildEqualityLocation sym test vars mem locDef
--  putStrLn $ "LocDef: " ++ show locDef
  (actualMem,targetMem) <- buildEqualityLocation sym test vars mem locDef
--  putStrLn $ "ACTUALLY IS:\t" ++ show actualMem
--  putStrLn $ "SHOULD BE:\t" ++ show targetMem

  res <- S.isEq sym actualMem targetMem
--  putStrLn $ "ACTUALLY IS = SHOULD BE: " ++ show res
  return res
--  WB.sbMakeExpr sym $ WE.ArrayEq actualMem targetMem

  where
    toBVExpr :: S.NatRepr w 
             -> Some (S.SymExpr sym) 
             -> IO (S.SymExpr sym (S.BaseBVType w))
    toBVExpr w0 (Some e) = case S.exprType e of
      S.BaseBVRepr w' -> case S.compareNat w0 w' of 
                           S.NatEQ -> return e
                           _       -> error $ "Memory index " ++ show e ++ " is not a bit vector of size " ++ show w0
      _ -> error $ "Memory index " ++ show e ++ " is not a bit vector of size " ++ show w0





partitionLocs' :: forall arch t st fs
                . Architecture arch
               => Formula (WE.ExprBuilder t st fs) arch
               -> ConcreteTest (WE.ExprBuilder t st fs) arch
               -> ( Set.Set (Some (Location arch))
                 , Maybe (L.MemLoc (L.Location arch)))
partitionLocs' (Formula _ defs) test =
    let allOutputLocs = Set.fromList (MapF.keys (testOutput test)) `Set.union`
                         Set.fromList (MapF.keys defs)
    in partitionLocs @arch allOutputLocs

{-
-- | Given a formula and a concrete test, return (1) the set of non-memory
-- locations touched by the formula and test; (2) the location referring to
-- memory
--
-- Unfortunately, the xs type is escaping its scope.
partitionLocs :: forall arch t st fs
               . Architecture arch
              => Formula (WE.ExprBuilder t st fs) arch
              -> ConcreteTest (WE.ExprBuilder t st fs) arch
              -> ( Set.Set (Some (Location arch))
                 , Maybe (Some (MemLoc arch)))
partitionLocs (Formula _ defs) test =
    let allOutputLocs = Set.fromList (MapF.keys (testOutput test)) `Set.union`
                        Set.fromList (MapF.keys defs)
        (memLocs, nonMemLocs) = Set.partition (\(Some loc) -> isMemoryLocation loc) allOutputLocs
        mem = Set.lookupMin memLocs -- memLocs should either be empty or a singleton set, so this should be deterministic
    in (nonMemLocs, mapSome toMemLoc <$> mem)
  where 
    toMemLoc :: Location arch tp -> MemLoc arch tp
    toMemLoc loc 
      | S.BaseArrayRepr (Ctx.Empty Ctx.:> S.BaseBVRepr w) _ <- locationType loc
        = MemLoc w loc
      | otherwise = error "The type of the memory Location in this architecture is unsupported"
-}

{-
data MemLoc arch ty where
  MemLoc :: 1 S.<= w
         => S.NatRepr w
         -> Location arch (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType w)) xs)
         -> MemLoc arch (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType w)) xs)
instance P.ShowF (Location arch) => Show (MemLoc arch ty) where 
  show (MemLoc _ l) = P.showF l
-}

-- | Build a conjuction of the equality expressions for /all/ locations in
-- either the outputs of the 'ConcreteTest' or the definitions of the 'Formula'.
-- That is, builds an equality expression of the form
-- > f.l1 (test_in.l1) = test_out.l1 /\ ... /\ (f.lm (test_in.lm) = test_out.lm)
-- for all locations li possibly touched by f or the test.
buildEqualityMachine :: forall arch t st fs
                      . (Architecture arch)
                     => WE.ExprBuilder t st fs
                     -> Formula (WE.ExprBuilder t st fs) arch
                     -> ConcreteTest (WE.ExprBuilder t st fs) arch
                     -> IO (WE.BoolExpr t)
buildEqualityMachine sym f test = do
  let (nonMemLocs, mem) = partitionLocs' f test
  nonMemPred <- buildEqualityLocations sym f test nonMemLocs
--  liftIO . putStrLn $ "Non-mem: " ++ show nonMemPred
  memPred <- buildEqualityMem sym f test mem
--  liftIO . putStrLn $ "Mem: " ++ show memPred
  liftIO $ S.andPred sym nonMemPred memPred

simplifyWithTest :: forall arch t st fs sym.
                         (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                      => Formula sym arch
                      -- ^ the target formula
                      -> ConcreteTest sym arch
                      -- ^ a single test case
                      -> Cegis sym arch (S.Pred sym)
simplifyWithTest f test = do
    nonMemPred <- simplifyWithTestNonMem f test
    memPred <- simplifyWithTestMem (formMem f) f test
    liftIO . putStrLn $ "Non-mem: " ++ show nonMemPred
    liftIO . putStrLn $ "Mem: " ++ show memPred
    sym <- askSym
    liftIO $ S.andPred sym nonMemPred memPred

formMem :: Architecture arch
        => Formula sym arch 
        -> Maybe (L.MemLoc (L.Location arch))
formMem f | any (\(Some l) -> L.isMemLoc l) (MapF.keys $ formParamVars f) = listToMaybe L.memLocation
formMem _ | otherwise = Nothing


-- | Substitute test input (for non-memory locations) into the target formula,
-- producing a new formula f' such that the only free variables in f' are Mem
-- and any uninstantiated immediates to be generated. We then construct a
-- predicate of the form 
-- @\forall l <> Mem, f'(l) = testOutput(l)@
simplifyWithTestNonMem :: forall arch t st fs sym.
                         (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                      => Formula sym arch
                      -- ^ the target formula
                      -> ConcreteTest sym arch
                      -- ^ a single test case
                      -> Cegis sym arch (S.Pred sym)
simplifyWithTestNonMem trialFormula test = do
  sym <- askSym
  defs' <- liftIO $ evalFormula' sym trialFormula (testInput test)
  let nonMemLocs = fst . partitionLocs @arch $ formInputs  trialFormula `Set.union` formOutputs trialFormula
  liftIO $ andPred sym nonMemLocs $ \(Some l) -> do
    (actuallyIs,shouldBe) <- buildEqualityLocation sym test vars l (MapF.lookup l defs')
    S.isEq sym actuallyIs shouldBe
  where
    vars = formParamVars trialFormula

-- | Meant to be used as the recurser to a fold to accumulate several predicates
andPred' :: S.IsExprBuilder sym
        => sym -> (a -> IO (S.Pred sym)) -> a -> S.Pred sym -> IO (S.Pred sym)
andPred' sym f a accum = f a >>= S.andPred sym accum

-- | Take the conjunction of (f a) for each a in some foldable data structure
andPred :: (Foldable m, S.IsExprBuilder sym)
        => sym -> m a -> (a -> IO (S.Pred sym)) -> IO (S.Pred sym)
andPred sym a f = foldrM (andPred' sym f) (S.truePred sym) a


-- | For each memory address i touched by the formula, constrain the input and
-- output state of memory with respect to that data
simplifyWithTestMem :: forall arch t st fs sym.
                         (Architecture arch, sym ~ WE.ExprBuilder t st fs)
                      => Maybe (L.MemLoc (L.Location arch))
                      -> Formula sym arch
                      -- ^ the target formula
                      -> ConcreteTest sym arch
                      -- ^ a single test case
                      -> Cegis sym arch (S.Pred sym)
simplifyWithTestMem Nothing _ _ = askSym >>= liftIO . return . S.truePred
simplifyWithTestMem (Just (L.MemLoc _ mem)) (Formula vars defs) test = do
  -- FIXME: we do this both here and in simplifyWithTestNonMem
--  putStrLn $ "Trial formula: " ++ show trialFormula
--  putStrLn $ "In simplify with test for " ++ show test
  
  sym <- askSym
  env <- askUF

  -- We only want to instantiate the non-mem variables in the formula
  let vars' = MapF.delete mem vars
  defs' <- liftIO $ evalFormula' sym (Formula @sym @arch vars' defs) (testInput test)
--  putStrLn $ "Evaluating formula on input tests " ++ show defs'
  let addrs = liveMemAddresses @arch $ Formula vars' defs'
--  putStrLn $ "Live addresses: " ++ show addrs

  let memExpr  = S.varExpr sym $ fromJust $ MapF.lookup mem vars
  let memExpr' = fromJust $ MapF.lookup mem defs'
  liftIO . putStrLn $ "Mem type: " ++ show (S.exprType memExpr)

  liftIO $ andPred sym addrs $ \i -> do
    preState  <- constrainMem @arch sym env (Some memExpr)  i (memInput test)
    postState <- constrainMem @arch sym env (Some memExpr') i (memOutput test)
    S.andPred sym preState postState


-- | constrain a state with respect to a particular map from addresses in memory
-- to values. In particular, 'constrainMem mem i addressMap' produces a predicate 
--
-- '(forall (j,v) in addressMap, mem(j)=v) 
--   /\ ((forall j in dom(addressMap), i <> j) -> (forall v in rng(addressMap), mem(i) <> v))
constrainMem :: forall arch t st fs sym.
                (Architecture arch, sym ~ WE.ExprBuilder t st fs)
             => sym
             -> FormulaEnv sym arch
             -> Some (S.SymExpr sym) 
             -- ^ An expression corresponding to memory
             -> Some (S.SymExpr sym)
             -- ^ A memory address touched by the formula
             -> Map.Map (Some (S.SymExpr sym)) (Some (S.SymExpr sym))
             -- ^ A mapping from concrete memory addresses to concrete values
             -> IO (S.Pred sym)
constrainMem sym env mem i addrMap = do 
  -- Produce a predicate @constrPos && (neq_i -> neq_v)@
  putStrLn $ "ConstrainMem at index " ++ show i
  putStrLn $ "with array mapping " ++ show addrMap
  putStrLn $ "with memory " ++ show mem
  constrPos <- andPred sym (Map.toList addrMap) $ \(j,Some v) -> 
    -- need to know the size of 'v' to know how many bits to read starting from i
    case S.exprType v of
      S.BaseBVRepr w -> do mem_j <- someReadMem sym env w mem j
                           S.isEq sym mem_j v
      _ -> error "Could not constrain memory because arguments have the wrong type"
  neq_i     <- andPred sym (Map.toList addrMap) $ \(j, _) -> S.notPred sym =<< someIsEq sym i j
  neq_v     <- andPred sym (Map.toList addrMap) $ \(_, Some v) ->
    -- need to know the size of 'v' to know how many bits to read starting from i
    case S.exprType v of
      S.BaseBVRepr w -> do mem_i <- someReadMem sym env w mem i
                           S.notPred sym =<< S.isEq sym mem_i v
      _ -> error "Could not constrain memory because arguments have the wrong type"
  S.andPred sym constrPos =<< S.impliesPred sym neq_i neq_v


someReadMem :: forall arch sym w.
               (Architecture arch, S.IsSymExprBuilder sym, 1 S.<= w)
            => sym
            -> FormulaEnv sym arch
            -> S.NatRepr w
            -- ^ the number of bits to read                
            -> Some (S.SymExpr sym)
            -- ^ an expression representing memory
            -> Some (S.SymExpr sym)
            -- ^ an expression representing the location in memory to read
            -> IO (S.SymExpr sym (S.BaseBVType w))
someReadMem sym env w arr i = 
  let readMemF = lookupUF env (readMemUF @arch $ S.natValue w)
  in someApplySymFn sym readMemF arr i (S.BaseBVRepr w)

{-
  | S.BaseArrayRepr (Ctx.Empty Ctx.:> arg) _ <- S.exprType arr
  , Just S.Refl <- S.testEquality arg (S.exprType i)
  , SomeSome f <- lookupUF env (readMemUF @arch $ S.natValue w)
  , Ctx.Empty Ctx.:> mem Ctx.:> arg' <- S.fnArgTypes f
  , S.BaseBVRepr w' <- S.fnReturnType f
  , Just S.Refl <- S.testEquality arg' arg
  , Just S.Refl <- S.testEquality mem (S.exprType arr)
  , Just S.Refl <- S.testEquality w w'
-}
--  = S.applySymFn sym f (Ctx.empty Ctx.:> arr Ctx.:> i)
{-
someReadMem _ _ w (Some arr) (Some i) 
  | otherwise = error $ "Could not construct a readMem because arguments have the wrong type"
                      ++ "\nw: " ++ show w
                      ++ "\nArray: " ++ show (S.exprType arr)
                      ++ "\ni: " ++ show (S.exprType i)
-}

someApplySymFn :: S.IsSymExprBuilder sym
               => sym
               -> SomeSome (S.SymFn sym)
               -- ^ the symbolic function to apply
               -> Some (S.SymExpr sym)
               -- ^ an expression representing memory
               -> Some (S.SymExpr sym)
               -- ^ an expression representing the location in memory to read
               -> S.BaseTypeRepr res
               -- ^ the target result type of the function
               -> IO (S.SymExpr sym res)
someApplySymFn sym (SomeSome f) (Some arr) (Some i) res 
  | Ctx.Empty Ctx.:> arrType Ctx.:> iType <- S.fnArgTypes f
  , Just S.Refl <- S.testEquality arrType (S.exprType arr)
  , Just S.Refl <- S.testEquality iType (S.exprType i)
  , Just S.Refl <- S.testEquality (S.fnReturnType f) res
  = S.applySymFn sym f (Ctx.empty Ctx.:> arr Ctx.:> i)
  | otherwise = error $ "Could not construct a readMem because arguments have the wrong type"


lookupUF :: FormulaEnv sym arch 
         -> UninterpFn arch 
         -> SomeSome (S.SymFn sym)
lookupUF env (MkUninterpFn name _ _ _)
  = case Map.lookup ("uf." ++ name) (envFunctions env) of
      Just (f0, _) -> f0
      Nothing      -> error $ "Could not find uninterpreted function in FormulaEnv\n"
                             ++ Map.foldMapWithKey go (envFunctions env)
  where
    go name (SomeSome _, _) = name ++ "\n"


trivialParameterizedFormula :: Architecture arch
                            => Formula sym arch -> ParameterizedFormula sym arch '[]
trivialParameterizedFormula (Formula vars defs) = 
    ParameterizedFormula Set.empty SL.Nil vars (mapFKeys LiteralParameter defs)

mapFKeys :: P.OrdF keys'
         => (forall tp. keys tp -> keys' tp) 
         -> MapF.MapF keys res 
         -> MapF.MapF keys' res
mapFKeys f m = MapF.foldrWithKey (\k -> MapF.insert (f k)) MapF.empty m


readMemBigEvaluator' :: forall sym byte w i.
                        (S.IsExprBuilder sym, 1 S.<= byte, 1 S.<= w, 1 S.<= i)
                     => sym
                     -> S.NatRepr byte
                     -> S.NatRepr w
                     -> S.SymExpr sym (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType i)) (S.BaseBVType byte))
                     -> S.SymExpr sym (S.BaseBVType i)
                     -> IO (S.SymExpr sym (S.BaseBVType w))
readMemBigEvaluator' sym byte w mem i = 
  case S.compareNat w byte of
    -- if the number of bits to read is equal to the number of bits stored in
    -- one block of memory, then just lookup the result in memory
    S.NatEQ -> S.arrayLookup sym mem (Ctx.Empty Ctx.:> i)

    -- if the number of bits we need to read is greater than the number of
    -- bits stored in a single block of memory, then look up a single block
    -- and then recurse
    S.NatGT w_minus_byte_minus1 
      -- we need a proof that 1 <= (w-byte-1)+1
      | S.LeqProof <- myLeqPf w_minus_byte_minus1 -> do
        let w_minus_byte = S.addNat w_minus_byte_minus1 (S.knownNat @1)
        let S.BaseBVRepr iRepr = S.exprType i

        b <- S.arrayLookup sym mem (Ctx.Empty Ctx.:> i)
        one <- S.bvLit sym iRepr 1
        i' <- S.bvAdd sym i one
        bs <- readMemBigEvaluator' sym byte w_minus_byte mem i'
        S.bvConcat sym b bs

        -- if the number of bits we need to read is less than the number of bits
        -- stored in a single block of memory, then we have asked for some
        -- number of bits that does not divide evenly and throw an error
    S.NatLT _ -> error $ "read_mem cannot be evaluated: "
                      ++ "the number of bits requested is not a mulitple of the dimension of the array"

  where
    myLeqPf :: S.NatRepr n -> S.LeqProof 1 (n S.+ 1)
    myLeqPf n | S.Refl <- S.plusComm n (S.knownNat @1) = S.leqAdd (S.leqRefl (S.knownNat @1)) n

-- | The evaluator for reading bits from memory (big endian representation)
--
-- read_mem does is not an operand, so we throw an error if 'sh' is not 'Nil'
readMemBigEvaluator :: 
                    WE.ExprBuilder t st fs
                 -> ParameterizedFormula (WE.ExprBuilder t st fs) arch sh
                 -> SL.List (AllocatedOperand arch (WE.ExprBuilder t st fs)) sh
                 -- ^ We expect this list to be empty
                 -> Ctx.Assignment (WB.Expr t) u
                 -- ^ The input to read_mem, in this case an array corresponding
                 -- to memory along with the index into the array at which to start reading
                 -> (forall ltp . Location arch ltp -> IO (WB.Expr t ltp))
                 -> S.BaseTypeRepr tp
                 -- ^ The result type, a bit-vector of size w
                 -> IO (WB.Expr t tp)
readMemBigEvaluator sym _f SL.Nil (Ctx.Empty Ctx.:> mem Ctx.:> i) evalLoc (S.BaseBVRepr w) 
    | Just (byte, iType, S.Refl, S.LeqProof, S.LeqProof, S.Refl) <- checkArgs (S.exprType mem) (S.exprType i) w
    = readMemBigEvaluator' sym byte w mem i
  where
    checkArgs :: S.BaseTypeRepr mem
              -> S.BaseTypeRepr i
              -> S.NatRepr w
              -> Maybe (S.NatRepr byte, S.NatRepr i'
                       , i S.:~: S.BaseBVType i'
                       , S.LeqProof 1 i'
                       , S.LeqProof 1 byte
                       , mem S.:~: S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType i')) (S.BaseBVType byte))
    checkArgs = undefined
readMemBigEvaluator _ _ _ _ _ _ = error "read_mem called with incorrect arguments and cannot be evaluated"

-- | Build an equality of the form
-- > f(test1_in) = test1_out /\ f(test2_in) = test2_out /\ ... /\ f(testn_in) = testn_out
-- where f(test_in) = test_out is an "equality machine" as in 'buildEqualityMachine'.
buildEqualityTests :: forall arch t st fs
                    . (Architecture arch)
                   => Formula (WE.ExprBuilder t st fs) arch
                   -> [ConcreteTest (WE.ExprBuilder t st fs) arch]
                   -> Cegis (WE.ExprBuilder t st fs) arch (WE.BoolExpr t)
buildEqualityTests form tests = do
  sym <- askSym
  let andTest test soFar = do test1 <- simplifyWithTest form test
                              liftIO $ S.andPred sym soFar test1
  foldrM andTest (S.truePred sym) tests


-- | Given a concrete model from the SMT solver, extract concrete instructions
-- from the templated instructions, so that all of the initially templated
-- operands are filled in concretely.
extractConcreteInstructions :: (ArchRepr arch)
                            => GroundEvalFn t
                            -> [TemplatedInstructionFormula (WE.ExprBuilder t st fs) arch]
                            -> IO [TemplatableInstruction arch]
extractConcreteInstructions (GroundEvalFn evalFn) = mapM f
  where f (TemplatedInstructionFormula (TemplatedInstruction op _ _) tf) =
          TemplatableInstruction op <$> recoverOperands (HR.typeRepr op) evalFn (tfOperandExprs tf)

-- | Meant to be used as the callback in a check SAT operation. If the result is
-- Sat, it pulls out concrete instructions corresponding to the SAT model.
-- Otherwise, it returns Nothing.
tryExtractingConcrete :: (ArchRepr arch)
                      => [TemplatedInstructionFormula (WE.ExprBuilder t st fs) arch]
                      -> SatResult (GroundEvalFn t)
                      -> IO (Maybe [TemplatableInstruction arch])
tryExtractingConcrete insns (Sat evalFn) = Just <$> extractConcreteInstructions evalFn insns
tryExtractingConcrete _ Unsat = return Nothing
tryExtractingConcrete _ Unknown = fail "got Unknown when checking sat-ness"

-- | Build a formula for the given concrete instruction.
instantiateFormula' :: (Architecture arch
                       -- , WE.IsBoolSolver (WE.ExprBuilder t st)
                       )
                    => TemplatableInstruction arch
                    -> Cegis (WE.ExprBuilder t st fs) arch (Formula (WE.ExprBuilder t st fs) arch)
instantiateFormula' (TemplatableInstruction op oplist) = do
  sym <- askSym
  semantics <- askSemantics
  let pf = unTemplate . fromJust $ MapF.lookup op semantics
  liftIO (snd <$> instantiateFormula sym pf oplist)

-- | Condense a series of instructions in sequential execution into one formula.
condenseInstructions :: (Architecture arch
                       -- , WE.IsBoolSolver (WE.ExprBuilder t st)
                       )
                     => [TemplatableInstruction arch]
                     -> Cegis (WE.ExprBuilder t st fs) arch (Formula (WE.ExprBuilder t st fs) arch)
condenseInstructions insns = do
  sym <- askSym
  insnFormulas <- traverse instantiateFormula' insns
  liftIO $ condenseFormulas sym insnFormulas

data CegisResult sym arch = CegisUnmatchable [ConcreteTest sym arch]
                          -- ^ There is no way to make the target and the
                          -- candidate do the same thing. This is proven by the
                          -- set of tests.
                          | CegisEquivalent [Instruction arch]
                          -- ^ This series of instructions, an instantiated form
                          -- of the candidate instructions given, has the same
                          -- behavior as the target formula.

cegis' :: (Architecture arch, ArchRepr arch, WPO.OnlineSolver t solver)
       => [TemplatedInstructionFormula (CBO.OnlineBackend t solver fs) arch]
       -- ^ The trial instructions.
       -> Formula (CBO.OnlineBackend t solver fs) arch
       -- ^ A formula representing the sequence of trial instructions.
       -> [ConcreteTest (CBO.OnlineBackend t solver fs) arch]
       -- ^ All the tests we have so far.
       -> Cegis (CBO.OnlineBackend t solver fs) arch (CegisResult (CBO.OnlineBackend t solver fs) arch)
cegis' trial trialFormula tests = do
--   liftIO . putStrLn $ "Number of tests: " ++ show (length tests)
--   liftIO . putStrLn $ "\n\nTESTS:\n\t" ++ show tests
--  liftIO . putStrLn $ "\n\nTRIAL FORMULA:\n" ++ show trialFormula
  sym <- askSym
  -- Is this candidate satisfiable for the concrete tests we have so far? At
  -- this point, the machine state is concrete, but the immediate values of the
  -- instructions are symbolic. If the candidate is satisfiable for the tests,
  -- the SAT solver will give us values for the templated immediates in order to
  -- make the tests pass.
  check <- buildEqualityTests trialFormula tests
  insns <- liftIO $ checkSat sym check (tryExtractingConcrete trial)

  case insns of
    Just insns' -> do
      -- For the concrete immediate values that the solver just gave us, are the
      -- target formula and the concrete candidate instructions equivalent for
      -- all symbolic machine states?
      liftIO . putStrLn $ "TRIAL INSTRUCTIONS:\n\t" ++ show insns'
      filledInFormula <- condenseInstructions insns'
      targetFormula <- askTarget
      equiv <- liftIO $ formulasEquivSym sym 
                                         (liveMemConst filledInFormula) 
                                         targetFormula 
                                         filledInFormula
      case equiv of
        -- FIXME: Is the correct behavior in a timeout to give up on this
        -- branch?
        Timeout -> do
          liftIO . putStrLn $ "Timeout"
          return (CegisUnmatchable tests)
        Equivalent -> do
          liftIO . putStrLn $ "Equivalent"
          return . CegisEquivalent $ map templInsnToDism insns'
        DifferentBehavior ctrExample -> do
          newTest <- liftIO $ mkTest sym targetFormula ctrExample
          liftIO . putStrLn $ "=============Added counterexample:=============== \n" ++ show newTest
          cegis' trial trialFormula (newTest : tests)
    Nothing -> return (CegisUnmatchable tests)


cegis :: (Architecture arch, ArchRepr arch, WPO.OnlineSolver t solver)
      => CegisParams (CBO.OnlineBackend t solver fs) arch
      -- ^ Parameters not specific to the candidate. See 'CegisParams' for
      -- details.
      -> [ConcreteTest (CBO.OnlineBackend t solver fs) arch]
      -- ^ The tests we know so far for the target formula.
      -> [TemplatedInstructionFormula (CBO.OnlineBackend t solver fs) arch]
      -- ^ The candidate program.
      -> IO (CegisResult (CBO.OnlineBackend t solver fs) arch)
cegis params tests trial = do
  trialFormula <- condenseFormulas (cpSym params) (map tifFormula trial)
  runReaderT (cegis' trial trialFormula tests) params


