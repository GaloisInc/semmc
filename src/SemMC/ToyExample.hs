{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Description: A toy example architecture.
--
-- A toy example architecture and end-to-end semantic synthesis
-- example. The goal is to get something simple working, that
-- abstracts away as many practical details as possible, and teases
-- out the necessary difficulties.
--
-- Q: how to handle undefined values? Some instructions may have
-- non-deterministic effects on some parts of the machine state -- the
-- stratified synthesis paper mentions this -- and we may want to
-- capture this in the semantics (then there are at least three
-- possibilities for how an instruction interacts with a bit of
-- machine state: 1) modifies it in a deterministic way; 2) modifies
-- it in an undefined way; 3) does not modify it).
--
-- TODO:
--
-- - [ ] generate random instructions.
--
-- - [ ] specify what locations each instruction mutates.
--
-- - [ ] implement a metric (probably number of bits different) on
--   machine states, and on machine states restricted to the mutation
--   set of a specific instruction.
--
-- - [ ] synthesize programs and check equality on concrete inputs
--   using 'evalProg'.
--
-- - [ ] check equality using formal SMT semantics, after generating
--   candidates using concrete inputs and 'evalProg'.
--
-- - [ ] add flags.
module SemMC.ToyExample where

import           Data.EnumF ( congruentF, EnumF, enumF )
import           Data.Map ( Map )
import qualified Data.Map as M
import           Data.Maybe ( fromJust )
import qualified Data.Parameterized.Classes as ParamClasses
import           Data.Parameterized.Classes hiding ( ShowF, showF )
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Set as Set
import           Data.ShowF ( ShowF, showF )
import           Data.Word ( Word32 )
import           GHC.TypeLits ( Symbol )

import           Dismantle.Instruction ( OperandList(Nil,(:>)) )
import qualified Dismantle.Instruction as D
-- import qualified Dismantle.Instruction.Random as D

import           Lang.Crucible.BaseTypes
import qualified Lang.Crucible.Solver.Interface as S
import qualified Lang.Crucible.Solver.SimpleBuilder as S
import           Lang.Crucible.Solver.Symbol ( SolverSymbol, userSymbol )

import qualified SemMC.Architecture as A
import           SemMC.Formula

data Reg :: BaseType -> * where
  Reg1 :: Reg (BaseBVType 32)
  Reg2 :: Reg (BaseBVType 32)
  Reg3 :: Reg (BaseBVType 32)
deriving instance Show (Reg tp)
deriving instance Eq (Reg tp)
deriving instance Ord (Reg tp)

instance ParamClasses.ShowF Reg where
  showF = show

instance ShowF Reg where
  showF = show

instance OrdF Reg where
  Reg1 `compareF` Reg1 = EQF
  Reg1 `compareF` Reg2 = LTF
  Reg1 `compareF` Reg3 = LTF
  Reg2 `compareF` Reg1 = GTF
  Reg2 `compareF` Reg2 = EQF
  Reg2 `compareF` Reg3 = LTF
  Reg3 `compareF` Reg1 = GTF
  Reg3 `compareF` Reg2 = GTF
  Reg3 `compareF` Reg3 = EQF

instance TestEquality Reg where
  Reg1 `testEquality` Reg1 = Just Refl
  Reg2 `testEquality` Reg2 = Just Refl
  Reg3 `testEquality` Reg3 = Just Refl
  _     `testEquality`     _ = Nothing

instance A.IsStateVar Reg where
  readStateVar "r1" = Just $ A.StateVarDesc (knownRepr :: BaseTypeRepr (BaseBVType 32)) Reg1
  readStateVar "r2" = Just $ A.StateVarDesc (knownRepr :: BaseTypeRepr (BaseBVType 32)) Reg2
  readStateVar "r3" = Just $ A.StateVarDesc (knownRepr :: BaseTypeRepr (BaseBVType 32)) Reg3
  readStateVar    _ = Nothing

  stateVarType Reg1 = knownRepr :: BaseTypeRepr (BaseBVType 32)
  stateVarType Reg2 = knownRepr :: BaseTypeRepr (BaseBVType 32)
  stateVarType Reg3 = knownRepr :: BaseTypeRepr (BaseBVType 32)

  allStateVars = [Some Reg1, Some Reg2, Some Reg3]

-- | An operand, indexed so that we can compute the type of its
-- values, and describe the shape of instructions that can take it as
-- an argument.
data Operand :: Symbol -> * where
  R32 :: Reg (BaseBVType 32) -> Operand "R32"
  -- | The use of @Value "I32"@ here seems a little inconsistent with
  -- 'Reg' as the argument to 'R32' above: the 'Reg' stands for a
  -- register name, and which bits in that register are being referred
  -- to is determined by the 'R32' constructor (e.g. we could have a
  -- @R16l@ and @R16h@ for the low and high 16 bits, respectively). In
  -- the analogy with 'R32', might it then make more sense to simply
  -- have 'I32' take an @Imm@ argument?
  I32 :: Value "I32" -> Operand "I32"
deriving instance Show (Operand a)
deriving instance Eq (Operand a)
deriving instance Ord (Operand a)

instance ShowF Operand where
  showF (R32 reg) = "R32 " ++ show reg
  showF (I32 val) = "R32 " ++ show val

instance TestEquality Operand where
  testEquality (R32 r1) (R32 r2)
    | r1 == r2 = Just Refl
    | otherwise = Nothing
  testEquality (I32 i1) (I32 i2)
    | i1 == i2 = Just Refl
    | otherwise = Nothing
  testEquality _ _ = Nothing

instance OrdF Operand where
  compareF (R32 _) (I32 _) = LTF
  compareF (R32 r1) (R32 r2) = fromOrdering $ compare r1 r2
  compareF (I32 _) (R32 _) = GTF
  compareF (I32 i1) (I32 i2) = fromOrdering $ compare i1 i2

-- Q: Why index by the 'Operand' type, if it's always the same?
--
-- A: Because the 'D.GenericInstruction' expects its first argument to
-- be indexed in this way.
--
-- Q: So, why is 'D.GenericInstruction' expect an indexed first arg?
data Opcode (o :: Symbol -> *) (sh :: [Symbol]) where
  -- Three instructions for my simplest example.
  AddRr :: Opcode Operand '["R32","R32"]
  SubRr :: Opcode Operand '["R32","R32"]
  NegR  :: Opcode Operand '["R32"]
{-
  -- A few more to add once the first example is working.
  AddRi :: Opcode Operand '["R32","I32"]
  SubRi :: Opcode Operand '["R32","I32"]
  SetRi :: Opcode Operand '["R32","I32"]
-}
deriving instance Show (Opcode o sh)
deriving instance Eq (Opcode o sh)
deriving instance Ord (Opcode o sh)

instance ShowF (Opcode o) where
  showF = show

instance EnumF (Opcode o) where
  enumF AddRr = 0
  enumF SubRr = 1
  enumF NegR = 2

  congruentF AddRr = Set.fromList [AddRr, SubRr]
  congruentF SubRr = Set.fromList [AddRr, SubRr]
  congruentF NegR = Set.fromList [NegR]

instance TestEquality (Opcode o) where
  testEquality AddRr AddRr = Just Refl
  testEquality SubRr SubRr = Just Refl
  testEquality  NegR  NegR = Just Refl
  testEquality     _     _ = Nothing

instance OrdF (Opcode o) where
  AddRr `compareF` AddRr = EQF
  AddRr `compareF` SubRr = LTF
  AddRr `compareF`  NegR = LTF
  SubRr `compareF` AddRr = GTF
  SubRr `compareF` SubRr = EQF
  SubRr `compareF`  NegR = LTF
  NegR  `compareF` AddRr = GTF
  NegR  `compareF` SubRr = GTF
  NegR  `compareF`  NegR = EQF

type Instruction = D.GenericInstruction Opcode Operand

-- | Registers, flags, and memory.
data MachineState = MachineState
  { msRegs :: !(Map (Reg (BaseBVType 32)) (Value "R32"))
--  , msFlags :: *
--  , msMem :: *
  }

initialMachineState :: MachineState
initialMachineState = MachineState
  { msRegs = M.fromList [ (Reg1, 0)
                        , (Reg2, 0)
                        , (Reg3, 0)
                        ]
  }

-- | The value type of a symbol, e.g. @"R32"@ registers are 32 bits.
type family Value (s :: Symbol) :: *
type instance Value "R32" = Word32
type instance Value "I32" = Word32

-- | Get the value of an operand in a machine state.
getOperand :: MachineState -> Operand s -> Value s
getOperand ms (R32 r) = msRegs ms M.! r
getOperand _ (I32 x) = x

-- | Update the value of an operand in a machine state.
putOperand :: MachineState -> Operand s -> Value s -> MachineState
putOperand ms (R32 r) x = ms { msRegs = M.insert r x $ msRegs ms }
putOperand _ I32{} _ = error "putOperand: putting an immediate does not make sense!"

-- | Evaluate an instruction, updating the machine state.
evalInstruction :: MachineState -> Instruction -> MachineState
evalInstruction ms (D.Instruction op args) = case (op, args) of
  (AddRr, r1 :> r2 :> Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (x1 + x2)
  (SubRr, r1 :> r2 :> Nil) ->
    let x1 = getOperand ms r1
        x2 = getOperand ms r2
    in putOperand ms r1 (x1 - x2)
  (NegR, r1 :> Nil) ->
    let x1 = getOperand ms r1
    in putOperand ms r1 (- x1)

evalProg :: MachineState -> [Instruction] -> MachineState
evalProg ms is = foldl evalInstruction ms is

data Toy = Toy

type instance A.Operand Toy = Operand
type instance A.Opcode Toy = Opcode

type instance A.OperandType Toy "R32" = BaseBVType 32
type instance A.OperandType Toy "I32" = BaseBVType 32

type instance A.StateVar Toy = Reg

instance A.Architecture Toy where
  operandValue = const operandValue
  operandToStateVar = const operandToStateVar

-- makeSymbol :: String -> SolverSymbol
-- makeSymbol name = case userSymbol name of
--                     Right symbol -> symbol
--                     Left _ -> error "tried to create symbol with bad name"

-- -- !!! Should go in a typeclass
operandValue :: forall sym s.
                (S.IsSymInterface sym)
             => sym
             -> MapF.MapF (A.StateVar Toy) (S.BoundVar sym)
             -> Operand s
             -> IO (S.SymExpr sym (A.OperandType Toy s))
operandValue sym m (R32 reg) = return $ S.varExpr sym (fromJust (MapF.lookup reg m))
operandValue sym _ (I32 imm) = S.bvLit sym (knownNat :: NatRepr 32) (toInteger imm)

-- foldlMWithKey :: forall k a b m. (Monad m) => (forall s. b -> k s -> a s -> m b) -> b -> MapF.MapF k a -> m b
-- foldlMWithKey f z0 m = MapF.foldrWithKey f' return m z0
--   where f' :: forall s. k s -> a s -> (b -> m b) -> b -> m b
--         f' k x c z = f z k x >>= c
--     -- go z' Tip = z'
--     -- go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l

-- replaceOpVars :: forall t st sh tp.
--                  S.SimpleBuilder t st
--               -- ^ Symbolic expression builder
--               -> MapF.MapF (A.StateVar Toy) (S.BoundVar (S.SimpleBuilder t st))
--               -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
--               -> OperandList (A.BoundVar (S.SimpleBuilder t st) Toy) sh
--               -- ^ List of variables corresponding to each operand
--               -> OperandList Operand sh
--               -- ^ List of operand values corresponding to each operand
--               -> S.SymExpr (S.SimpleBuilder t st) tp
--               -- ^ Expression to do the replace in
--               -> IO (S.SymExpr (S.SimpleBuilder t st) tp)
-- replaceOpVars   _ _                          Nil               Nil expr = return expr
-- replaceOpVars sym m ((A.BoundVar var) :> varsRest) (val :> valsRest) expr = do
--   -- Would it be faster to do this all in one replace? Probably, but for now we
--   -- do it like this.
--   val' <- operandValue sym m val
--   replaced <- S.evalBoundVars sym expr (Ctx.extend Ctx.empty var) (Ctx.extend Ctx.empty val')
--   replaceOpVars sym m varsRest valsRest replaced

-- replaceLitVars :: forall t st tp.
--                   S.SimpleBuilder t st
--                -> MapF.MapF (A.StateVar Toy) (S.BoundVar (S.SimpleBuilder t st))
--                -> MapF.MapF (A.StateVar Toy) (S.BoundVar (S.SimpleBuilder t st))
--                -> S.SymExpr (S.SimpleBuilder t st) tp
--                -> IO (S.SymExpr (S.SimpleBuilder t st) tp)
-- replaceLitVars sym newVars oldVars expr0 = foldlMWithKey f expr0 oldVars
--   where f :: forall tp'. S.Elt t tp -> A.StateVar Toy tp' -> S.SimpleBoundVar t tp' -> IO (S.Elt t tp)
--         f expr k oldVar = let newExpr = S.varExpr sym . fromJust $ MapF.lookup k newVars
--                           in S.evalBoundVars sym expr (Ctx.extend Ctx.empty oldVar) (Ctx.extend Ctx.empty newExpr)

-- mapFMapMBoth :: forall k1 v1 k2 v2 m. (OrdF k2, Monad m) => (forall tp. k1 tp -> v1 tp -> m (k2 tp, v2 tp)) -> MapF.MapF k1 v1 -> m (MapF.MapF k2 v2)
-- mapFMapMBoth f = MapF.foldrWithKey f' (return MapF.empty)
--   where f' :: forall tp. k1 tp -> v1 tp -> m (MapF.MapF k2 v2) -> m (MapF.MapF k2 v2)
--         f' k v wrappedM = do
--           (k', v') <- f k v
--           m <- wrappedM
--           return $ MapF.insert k' v' m

-- -- !!! Should go in a typeclass
operandToStateVar :: forall s. Operand s -> Maybe (A.StateVar Toy (A.OperandType Toy s))
operandToStateVar (R32 reg) = Just reg
operandToStateVar (I32 _) = Nothing

-- paramToStateVar :: forall sh tp. OperandList Operand sh -> Parameter Toy sh tp -> Maybe (A.StateVar Toy tp)
-- paramToStateVar opVals (Operand _ idx) = operandToStateVar $ indexOpList opVals idx
-- paramToStateVar _ (Literal _ var) = Just var

-- instantiateFormula :: forall t st sh.
--                       S.SimpleBuilder t st
--                    -> ParameterizedFormula (S.SimpleBuilder t st) Toy sh
--                    -> OperandList Operand sh
--                    -> IO (Formula (S.SimpleBuilder t st) Toy)
-- instantiateFormula
--   sym
--   (ParameterizedFormula { pfUses = uses
--                         , pfOperandVars = opVars
--                         , pfLiteralVars = litVars
--                         , pfDefs = defs
--                         })
--   opVals = do
--     -- First, make variables. For now, create variables for *all* possible state
--     -- parts. We can change this later, but for now, it's simple.
--     -- newLitVarPairs <- mapM (\v -> Pair v <$> S.freshBoundVar sym (makeSymbol (showF v)) (A.stateVarType v))
--     --                        [Reg'1, Reg'2, Reg'3]
--     newLitVars <- MapF.fromKeysM (\v -> S.freshBoundVar sym (makeSymbol (showF v)) (A.stateVarType v))
--                                  [Some Reg1, Some Reg2, Some Reg3]

--     let mapDef :: forall tp. Parameter Toy sh tp -> S.Elt t tp -> IO (A.StateVar Toy tp, S.Elt t tp)
--         mapDef p e = case paramToStateVar opVals p of
--           Just var -> (var,) <$> (replaceLitVars sym newLitVars litVars =<< replaceOpVars sym newLitVars opVars opVals e)
--           Nothing -> error "XXX: handle this error case more gracefully"
--     -- opVarsReplaced <- MapF.map (replaceOpVars newLitVars opVars opVals)
--     newDefs <- mapFMapMBoth mapDef defs

--     let mapParam (Some param) = maybe Set.empty (Set.singleton . Some) $ paramToStateVar opVals param
--         newUses = foldMap mapParam uses

--     return $ Formula { formUses = newUses
--                      , formParamVars = newLitVars
--                      , formDefs = newDefs
--                      }
-- type family ListToCtx (args :: [k]) :: Ctx k where
--   ListToCtx '[]       = EmptyCtx
--   ListToCtx (x ': xs) = (EmptyCtx ::> x) <+> (ListToCtx xs)

-- data ParameterizedFormula sym c o =
--   ParameterizedFormula { tfOperands :: Ctx.Assignment o (ListToCtx )
--                        , tfUses :: [Parameter]
--                        , tfParamExprs :: Map.Map Parameter (Some (S.SymExpr sym))
--                        , tfDefs :: Map.Map Parameter (Some (S.SymExpr sym))
--                        }
-- varToExpr :: (S.IsSymInterface sym) => sym -> Some (S.BoundVar sym) -> Some (S.SymExpr sym)
-- varToExpr sym (Some var) = Some $ S.varExpr sym var

-- filterMapSet :: (Ord k2) => (k1 -> Maybe k2) -> Set.Set k1 -> Set.Set k2
-- filterMapSet f = foldr go Set.empty
--   where go v accum = case f v of
--           Just v' -> Set.insert v' accum
--           Nothing -> accum

-- renameOps :: [(T.Text, T.Text)] -> D.OperandList Operand sh -> Set.Set FormulaVar
-- renameOps [] Nil = Set.empty
-- renameOps ((name, _) : restNames) (op :> restOps) =
--   case op of
--     R32 reg -> Set.insert (FormulaVar $ T.pack $ show reg) $ renameOps restNames restOps
--     I32 _ -> renameOps restNames restOps
-- renameOps _ _ = error "renameOps: mismatching length"

-- mapKeysAndValues :: forall k1 k2 a b. (Ord k2) => (k1 -> a -> (k2, b)) -> M.Map k1 a -> M.Map k2 b
-- mapKeysAndValues f = M.foldrWithKey go M.empty
--   where go :: k1 -> a -> M.Map k2 b -> M.Map k2 b
--         go oldKey oldVal = uncurry M.insert (f oldKey oldVal)

-- instantiateFormula :: forall t st. S.SimpleBuilder t st -> ParameterizedFormula (S.SimpleBuilder t st) -> Instruction -> IO (Formula (S.SimpleBuilder t st))
-- instantiateFormula
--   sym
--   (ParameterizedFormula { tfOperands = operands
--                         , tfUses = oldUses
--                         , tfParamVars = oldParamVars
--                         , tfDefs = oldDefs })
--   (D.Instruction _ oplist) = do
--   when (length operands /= D.operandListLength oplist)
--     (throwIO "length of operands in ParameterizedFormula doesn't match that in Instruction")

--   opExprs <- operandExprs sym oplist
--   -- let paramExprs = varToExpr sym <$> oldParamVars

--   assignments <- case buildAssignments sym oldParamVars opExprs operands oplist of
--     Just assgns -> return assgns
--     Nothing -> throwIO "buildAssignments failed"

--   newDefs <- traverse (evalBoundVars' sym assignments) oldDefs

--   let useTrans (Operand _) = Nothing
--       useTrans (Literal lit) = Just $ FormulaVar lit
--       newUses = Set.union (renameOps operands oplist) (filterMapSet useTrans oldUses)
--       paramVarMap :: Parameter -> Some (S.SimpleBoundVar t) -> (FormulaVar, Some (S.SimpleBoundVar t))
--       paramVarMap (Operand op) _ =
--         let idx = maybe (error "op not in the list?") id $ elemIndex op (map fst operands)
--         in (undefined, maybe (error "idx out of range of assignments") id $ indexAssignments assignments idx)
--       paramVarMap (Literal lit) var = (FormulaVar lit, var)
--       newParamVars = mapKeysAndValues paramVarMap oldParamVars
--   return $ Formula { formUses = newUses
--                    , formParamVars = newParamVars
--                    , formDefs = newDefs }


-- -- convertOperand :: (S.IsExprBuilder sym,
-- --                    S.IsSymInterface sym)
-- --                => [(T.Text, T.Text)]
-- --                -> IO (forall s. Operand s -> Maybe (S.SymExpr sym (OperandType s)))
-- -- convertOperand [] = return (const Nothing)
-- -- convertOperand (())

-- fromRight :: Either l r -> r
-- fromRight (Right y) = y
-- fromRight (Left _) = error "fromRight called on Left value"

-- mkOperand :: (S.IsExprBuilder sym,
--               S.IsSymInterface sym)
--           => sym
--           -> Operand s
--           -> IO (S.SymExpr sym (OperandType s))
-- mkOperand sym (R32 reg) = S.freshConstant sym (fromRight $ userSymbol (show reg)) (knownRepr :: BaseTypeRepr (BaseBVType 32))
-- mkOperand sym (I32 imm) = S.bvLit sym (knownNat :: NatRepr 32) (toInteger imm)

-- -- This is a Map so that if two operands are the same register, they map to the
-- -- same symbolic variable.
-- operandExprs :: (S.IsExprBuilder sym,
--                  S.IsSymInterface sym)
--              => sym
--              -> D.OperandList Operand sh
--              -> IO (M.Map (Some Operand) (Some (S.SymExpr sym)))
-- operandExprs sym D.Nil = return M.empty
-- operandExprs sym (val D.:> rest) = M.insert (Some val) <$> (Some <$> mkOperand sym val) <*> operandExprs sym rest

-- data BoundVarAssigns sym where
--   BoundVarAssigns :: forall sym args.
--                      Ctx.Assignment (S.BoundVar sym) args
--                   -> Ctx.Assignment (S.SymExpr sym) args
--                   -> BoundVarAssigns sym

-- indexAssignments :: BoundVarAssigns sym -> Int -> Maybe (Some (S.SymExpr sym))
-- indexAssignments (BoundVarAssigns _ exprs) i =
--   let index = Ctx.intIndex i (Ctx.size exprs)
--   in Some . idxLookup id exprs <$> index

-- evalBoundVars' :: S.SimpleBuilder t st
--                -> BoundVarAssigns (S.SimpleBuilder t st)
--                -> Some (S.Elt t)
--                -> IO (Some (S.Elt t))
-- evalBoundVars' sym (BoundVarAssigns vars exprs) (Some expr) = Some <$> S.evalBoundVars sym expr vars exprs

-- buildAssignments :: (S.IsSymInterface sym)
--                  => sym
--                  -> M.Map Parameter (Some (S.BoundVar sym))
--                  -> M.Map (Some Operand) (Some (S.SymExpr sym))
--                  -> [(T.Text, T.Text)]
--                  -> D.OperandList Operand sh
--                  -> Maybe (BoundVarAssigns sym)
-- buildAssignments sym _ _ [] Nil = Just $ BoundVarAssigns Ctx.empty Ctx.empty
-- buildAssignments sym paramMap opMap ((name, _) : paramRest) (val :> opsRest) = do
--   Some bvar <- M.lookup (Operand name) paramMap
--   Some opExpr <- M.lookup (Some val) opMap
--   Refl <- testEquality (S.exprType (S.varExpr sym bvar)) (S.exprType opExpr)
--   BoundVarAssigns vars exprs <- buildAssignments sym paramMap opMap paramRest opsRest
--   return $ BoundVarAssigns (Ctx.extend vars bvar) (Ctx.extend exprs opExpr)
-- buildAssignments _ _ _ _ _ = Nothing

-- -- instantiateOperands :: (S.IsExprBuilder sym,
-- --                         S.IsSymInterface sym)
-- --                     => M.Map (Some Operand) (Some (S.SymExpr sym))
-- --                     -> [(T.Text, T.Text)]
-- --                     -> D.OperandList Operand sh
-- --                     -> IO (Maybe [(T.Text, Some (S.SymExpr sym))])
-- -- instantiateOperands _ [] D.Nil = return $ Just []
-- -- instantiateOperands m ((name, _ty) : xs) (val D.:> ys) = undefined
  
--   -- how to check ty?


-- instance A.Architecture Opcode Operand Toy where
--   -- type Operand Toy = Operand
--   -- type Opcode Toy = Opcode
--   -- type MachineState Toy = MachineState
--   -- type OperandType Toy "R32" = BaseBVType 32
--   -- type OperandType Toy "I32" = BaseBVType 32

--   showInsn _ = undefined
--   instantiateFormula _ = undefined

-- -- TODO: also need a notion of flags.
-- --
-- -- If all flags are one bit, then perhaps we can do away with the
-- -- indexing.
-- --
-- -- - [ ] set flags in operational semantics.
-- {-
-- data Flag :: G.Symbol -> * where
--   Flag

-- getFlag :: Flag s -> MachineState -> Value s
-- getFlag = undefined
-- -}
