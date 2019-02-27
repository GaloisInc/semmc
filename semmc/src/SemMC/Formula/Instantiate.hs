{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Formula.Instantiate
  ( instantiateFormula
  , copyFormula
  , sequenceFormulas
  , condenseFormulas
  , replaceVars
  , replaceLitVars
  , paramToLocation
  ) where

import           Data.Foldable                      ( foldlM, foldrM )
import           Data.Kind
import           Data.Maybe                         ( isNothing )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context         as Ctx
import           Data.Parameterized.Ctx
import qualified Data.Parameterized.List            as SL
import qualified Data.Parameterized.Map             as MapF
import           Data.Parameterized.Pair            ( Pair(..) )
import           Data.Parameterized.Some            ( Some(..) )
import           Data.Parameterized.TraversableF
import qualified Data.Parameterized.TraversableFC   as FC
import           GHC.TypeLits                       ( Symbol )
import           Text.Printf                        ( printf )
import           Data.Proxy                         ( Proxy(..) )

import           What4.BaseTypes
import qualified What4.Interface     as S
import qualified What4.Expr.Builder as S

import qualified SemMC.Architecture                 as A
import qualified SemMC.BoundVar                     as BV
import qualified SemMC.Formula.Eval                 as FE
import           SemMC.Formula.Formula
import qualified SemMC.Util                         as U

-- I got tired of typing this.
type SB t st fs = S.ExprBuilder t st fs

-- | Convert a type-level list of operands to a Crucible-style context of
-- operand types. This reverses it, but we don't care about that for our use
-- case (and not doing so would be harder).
type family ShapeCtx (arch :: Type) (sh :: [Symbol]) :: Ctx BaseType where
  ShapeCtx _    '[] = EmptyCtx
  ShapeCtx arch (s ': sh) = ShapeCtx arch sh '::> A.OperandType arch s

data OperandAssignment sym arch sh =
  OperandAssignment { opAssnTaggedExprs :: SL.List (A.TaggedExpr arch sym) sh
                    -- ^ The raw values obtained by calling 'operandValue' on
                    -- each of the operands.
                    , opAssnVars :: SomeVarAssignment sym
                    }
type SomeVarAssignment sym = Pair (Ctx.Assignment (S.BoundVar sym)) (Ctx.Assignment (S.SymExpr sym))


-- | For a given pair of bound variables and operands, build up:
-- 1. 'TaggedExpr's corresponding to each operand.
-- 2. a 'Ctx.Assignment' form of the bound variables (for use in substitution)
-- 3. a 'Ctx.Assignment' of the created expressions corresponding to each
--    operand
buildOpAssignment :: forall sym arch sh.
                  (A.Architecture arch,
                   S.IsSymExprBuilder sym)
                => sym
                -- ^ Symbolic expression builder
                -> (forall tp'. A.Location arch tp' -> IO (S.SymExpr sym tp'))
                -- ^ Lookup for expression variables from a part of state name ("r2", "memory", etc.)
                -> SL.List (BV.BoundVar sym arch) sh
                -- ^ List of variables corresponding to each operand
                -> SL.List (A.Operand arch) sh
                -- ^ List of operand values corresponding to each operand
                -> IO (OperandAssignment sym arch sh)
buildOpAssignment _ _ SL.Nil SL.Nil = return (OperandAssignment SL.Nil (Pair Ctx.empty Ctx.empty))
buildOpAssignment sym newVars ((BV.BoundVar var) SL.:< varsRest) (val SL.:< valsRest) = do
  taggedExpr <- A.allocateSymExprsForOperand (Proxy @arch) sym newVars val
  rest <- buildOpAssignment sym newVars varsRest valsRest
  let oa' = case A.unTagged taggedExpr of
        Nothing ->
          OperandAssignment { opAssnTaggedExprs = taggedExpr SL.:< opAssnTaggedExprs rest
                            , opAssnVars = opAssnVars rest
                            }
        Just symExpr ->
          case opAssnVars rest of
            Pair varAssn exprAssn ->
              OperandAssignment { opAssnTaggedExprs = taggedExpr SL.:< opAssnTaggedExprs rest
                                , opAssnVars = Pair (varAssn Ctx.:> var) (exprAssn Ctx.:> symExpr)
                                }
  return oa'


-- | Given
-- 1. a generator for expressions given locations
-- 2. a mapping of all locations used to their corresponding bound variables
-- this function builds an assignment of bound variables to expressions to use
-- as a substitution.
buildLitAssignment :: forall m proxy sym loc
                    . (Monad m)
                   => proxy sym
                   -> (forall tp. loc tp -> m (S.SymExpr sym tp))
                   -> MapF.MapF loc (S.BoundVar sym)
                   -> m (SomeVarAssignment sym)
buildLitAssignment _ exprLookup = foldlM f (MapF.Pair Ctx.empty Ctx.empty) . MapF.toList
  where f (Pair varAssn exprAssn) (Pair loc var) =
          fmap (\expr -> Pair (varAssn Ctx.:> var) (exprAssn Ctx.:> expr))
               (exprLookup loc)

-- | Replace all the variables in the given 'SomeVarAssignment' with their
-- corresponding expressions, in the given expression.
replaceVars :: forall t st fs tp
             . SB t st fs
            -> SomeVarAssignment (SB t st fs)
            -> S.Expr t tp
            -> IO (S.Expr t tp)
replaceVars sym (Pair varAssn exprAssn) expr =
  S.evalBoundVars sym expr varAssn exprAssn

-- | Given a generator for expressions given machine locations and all used
-- machine locations, this replaces bound variables with their corresponding
-- expressions in the given top-level expression.
replaceLitVars :: forall loc t st fs tp.
                  (OrdF loc)
               => SB t st fs
               -> (forall tp'. loc tp' -> IO (S.Expr t tp'))
               -> MapF.MapF loc (S.ExprBoundVar t)
               -> S.Expr t tp
               -> IO (S.Expr t tp)
replaceLitVars sym newExprs oldVars expr = do
  assn <- buildLitAssignment (Proxy @(SB t st fs)) newExprs oldVars
  replaceVars sym assn expr

-- | Get the corresponding location of a parameter, if it actually corresponds
-- to one.
paramToLocation :: forall arch sh tp t st fs
                 . (A.Architecture arch)
                => SL.List (A.AllocatedOperand arch (S.ExprBuilder t st fs)) sh
                -> Parameter arch sh tp
                -> Maybe (A.Location arch tp)
paramToLocation opVals op@(OperandParameter _ idx) =
  case opVals SL.!! idx of
    A.LocationOperand l _ -> Just l
    A.ValueOperand {} -> error ("Unexpected ValueOperand while extracting location for " ++ show op)
    A.CompoundOperand {} -> error ("Unexpected CompoundOperand while extracting location for " ++ show op)
paramToLocation _ (LiteralParameter loc) = Just loc
paramToLocation opVals (FunctionParameter fnName wo rep) =
  case fnName `lookup` A.locationFuncInterpretation (Proxy @arch) of
    Nothing -> error ("No function interpretation for " ++ show fnName)
    Just interp ->
      case A.locationInterp interp of
        LocationFuncInterp fn -> fn opVals wo rep

-- | Create a concrete 'Formula' from the given 'ParameterizedFormula' and
-- operand list. The first result is the list of created 'TaggedExpr's for each
-- operand that are used within the returned formula. Will instantiate the
-- functions given by 'SemMC.Architecture.locationFuncInterpretation'
instantiateFormula :: forall arch t st fs sh.
                      ( A.Architecture arch
                      , S.IsSymExprBuilder (SB t st fs))
                   => SB t st fs
                   -> ParameterizedFormula (SB t st fs) arch sh
                   -> SL.List (A.Operand arch) sh
                   -> IO (SL.List (A.TaggedExpr arch (SB t st fs)) sh, Formula (SB t st fs) arch)
instantiateFormula sym pf opVals 
    = instantiateFormula' sym pf opVals (A.locationFuncInterpretation (Proxy @ arch))

-- | Create a concrete 'Formula' from the given 'ParameterizedFormula' and
-- operand list. The first result is the list of created 'TaggedExpr's for each
-- operand that are used within the returned formula. 
instantiateFormula' :: forall arch t st fs sh.
                      ( A.Architecture arch
                      , S.IsSymExprBuilder (SB t st fs))
                   => SB t st fs
                   -> ParameterizedFormula (SB t st fs) arch sh
                   -> SL.List (A.Operand arch) sh
                   -> [(String, A.FunctionInterpretation t st fs arch)]
                   -- ^ The interpretations of functions that may occur in the
                   -- parameterized formula to be instantiated
                   -> IO (SL.List (A.TaggedExpr arch (SB t st fs)) sh, Formula (SB t st fs) arch)
instantiateFormula'
  sym
  pf@(ParameterizedFormula { pfOperandVars = opVars
                           , pfLiteralVars = litVars
                           , pfDefs = defs
                           })
  opVals 
  functions = do
    let addLitVar (Some loc) m = do
          bVar <- S.freshBoundVar sym (U.makeSymbol (showF loc)) (A.locationType loc)
          return (MapF.insert loc bVar m)
    newLitVars <- foldrM addLitVar MapF.empty A.allLocations
    let newLitExprLookup :: A.Location arch tp -> IO (S.Expr t tp)
        -- 'newLitVars' has all locations in it, so this 'fromJust' is total.
        newLitExprLookup loc =
            (return . S.varExpr sym . U.fromJust' ("newLitExprLookup: " ++ showF loc) . flip MapF.lookup newLitVars) loc

    OperandAssignment { opAssnTaggedExprs = opTaggedExprs
                      , opAssnVars = Pair opVarsAssn opExprsAssn
                      } <- buildOpAssignment sym newLitExprLookup opVars opVals
    let allocOpers :: SL.List (A.AllocatedOperand arch (SB t st fs)) sh
        allocOpers = FC.fmapFC A.taggedOperand opTaggedExprs
    let rewrite :: forall tp . S.Expr t tp -> IO (S.Expr t tp)
        rewrite = FE.evaluateFunctions sym pf allocOpers newLitExprLookup (fmap A.exprInterp <$> functions)
    -- Here, the formula rewriter walks over the formula AST and replaces calls
    -- to functions that we know something about with concrete values.  Most
    -- often, these functions are predicates testing something about operands
    -- (e.g., is the operand a specific register) or splitting apart a compound
    -- operand (e.g., some memory references are a combination of a register and
    -- an offset).
    --
    -- For normal instructions, this is simple: look at the operand(s) of the
    -- function and evaluate them statically.
    --
    -- For templated instructions, this is more complicated.  For templated
    -- instructions, the register portions of compound operands are all static
    -- (as part of the template), but the literals are *symbolic* variables
    -- (which cannot be stored in the original operand type).  Moreover, the
    -- variables standing in for the constant portions have not been allocated
    -- yet, as that happens in `buildOpAssignment`.
    --
    -- Additionally, the representation in `buildOpAssignment` doesn't seem to
    -- be very useful: it generates symbolic expressions that combine the
    -- register and offset automatically (using addition or other bitvector
    -- operations).  I think it hasn't ended up mattering, as the variables
    -- allocated for the compound operands are actually never used (since the
    -- rewriting pass eliminates direct references to them).  Noting especially
    -- that the extractors for the current set of architectures allocate fresh
    -- solver expressions (as bvLits) for extracted constants held in compound
    -- operands.
    --
    -- It seems like (solver) values must be allocated *before* rewriting, and
    -- there should not be any solver values for composite operands.  Instead,
    -- there should be solver values allocated for each *component* of each
    -- operand.  We need to make sure that e.g., an offset contained in a
    -- compound operand has only a single variable allocated for it while
    -- instantiating a templated instruction (so that all references are to the
    -- same value).
    --
    -- Maybe this pre-processing pass to allocate all of the necessary
    -- variables/values could eliminate the need to return the partial map from
    -- the evaluator - we can just make sure that everything is pre-allocated.
    -- This extra structure would have to be passed in to the evaluators.  It
    -- should be able to have the same type for both the templated case and the
    -- non-templated case, as the values in the map would just be SymExprs.  It
    -- will probably require an arch-specific data type to key the map (to
    -- represent "lenses" into the compound operands).  This system should
    -- probably subsume operandValues (and extend buildOpAssignment)
    defs' <- traverseF rewrite defs
    -- ok, so rewrite is not working in Mem??

    -- After rewriting, it should be the case that all references to the bound
    -- variables corresponding to compound operands (for which we don't have
    -- valid expressions) should be gone.
    --
    -- We could change 'opAssnBareExprs' so that simple operands for which
    -- substitution makes sense are present in the assignment (with some kind of
    -- MaybeF wrapper), while operands with no simple SymExpr (i.e., compound
    -- operands) are not present, and we can raise an error if we try to do a
    -- substitution for one.
    let instantiateDefn :: forall tp. Parameter arch sh tp -> S.Expr t tp -> IO (A.Location arch tp, S.Expr t tp)
        instantiateDefn definingParam definition = do
          definingLoc <- case paramToLocation allocOpers definingParam of
            Just loc -> return loc
            Nothing -> fail $ printf "parameter %s is not a valid location" (show definingParam)
          opVarsReplaced <- S.evalBoundVars sym definition opVarsAssn opExprsAssn
          litVarsReplaced <-
            replaceLitVars sym newLitExprLookup litVars opVarsReplaced
          return (definingLoc, litVarsReplaced)

    newDefs <- U.mapFMapBothM instantiateDefn defs'

    -- 'newLitVars' has variables for /all/ of the machine locations. Here we
    -- extract only the ones that are actually used.
    let newActualLitVars = foldrF (MapF.union . U.extractUsedLocs newLitVars) MapF.empty newDefs

    -- TODO: Should we filter out definitions that are syntactically identity
    -- functions?

    return $ (opTaggedExprs, Formula { formParamVars = newActualLitVars
                                     , formDefs = newDefs
                                     })

-- | Create a new formula with the same semantics, but with fresh bound vars.
copyFormula :: forall t st fs arch.
               (A.IsLocation (A.Location arch), U.HasCallStack)
            => SB t st fs
            -> Formula (SB t st fs) arch
            -> IO (Formula (SB t st fs) arch)
copyFormula sym (Formula { formParamVars = vars, formDefs = defs}) = do
  let mkVar :: forall tp. A.Location arch tp -> IO (S.ExprBoundVar t tp)
      mkVar loc = S.freshBoundVar sym (U.makeSymbol (showF loc)) (A.locationType loc)
  newVars <- MapF.traverseWithKey (const . mkVar) vars
  let lookupNewVar :: forall tp. A.Location arch tp -> S.Expr t tp
      lookupNewVar = S.varExpr sym . U.fromJust' "copyFormula" . flip MapF.lookup newVars
  assn <- buildLitAssignment (Proxy @(SB t st fs)) (return . lookupNewVar) vars
  newDefs <- traverseF (replaceVars sym assn) defs
  return $ Formula { formParamVars = newVars
                   , formDefs = newDefs
                   }

-- | Combine two formulas in sequential execution
sequenceFormulas :: forall t st fs arch.
                    (A.Architecture arch)
                 => SB t st fs
                 -> Formula (SB t st fs) arch
                 -> Formula (SB t st fs) arch
                 -> IO (Formula (SB t st fs) arch)
sequenceFormulas sym form1 form2 = do
  -- First, copy them, just to be safe. This might not be necessary for one of
  -- them, but I'll keep it like this for now.
  Formula { formParamVars = vars1
          , formDefs = defs1
          } <- copyFormula sym form1
  Formula { formParamVars = vars2
          , formDefs = defs2
          } <- copyFormula sym form2

  let varReplace :: forall tp. A.Location arch tp -> S.Expr t tp
      varReplace loc
        -- If this location is defined in the first formula, use the new
        -- definition.
        | Just expr <- MapF.lookup loc defs1 = expr
        -- If this location isn't defined in the first formula, but is used by
        -- it, use the first formula's variable.
        | Just newVar <- MapF.lookup loc vars1 = S.varExpr sym newVar
        -- Otherwise, use the original variable.
        | otherwise = S.varExpr sym $ U.fromJust' "sequenceFormulas" $ MapF.lookup loc vars2
  assn <- buildLitAssignment (Proxy @(SB t st fs)) (return . varReplace) vars2
  newDefs2 <- traverseF (replaceVars sym assn) defs2

  let newDefs = MapF.union newDefs2 defs1
      -- The new vars are all the vars from the first, plus the vars from the
      -- second that are neither required in the first nor defined in the first.
      notWrittenVars2 = U.filterMapF (\k _ -> isNothing $ MapF.lookup k defs1) vars2
      newVars = MapF.union vars1 notWrittenVars2

  return $ Formula { formParamVars = newVars
                   , formDefs = newDefs
                   }

condenseFormulas :: forall f t st fs arch.
                    ( A.Architecture arch
                    , Foldable f
                    )
                 => SB t st fs
                 -> f (Formula (SB t st fs) arch)
                 -> IO (Formula (SB t st fs) arch)
condenseFormulas sym = foldrM (sequenceFormulas sym) emptyFormula
