{-# LANGUAGE RankNTypes, TypeOperators, TypeApplications, DataKinds, 
TypeFamilies, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Evaluators for readMem and writeMem uninterpreted functions
module SemMC.Synthesis.Cegis.ReadWriteEval
  ( instantiateReadMem
  ) where

import qualified Data.Set as Set

import           Data.Parameterized.Classes (OrdF(..), TestEquality(..))
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as L
import qualified Data.Parameterized.Map as MapF

import qualified Lang.Crucible.Backend as B
import qualified What4.Interface as S
import qualified What4.Expr as WE

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as AL
import qualified SemMC.Formula.Formula as F
import qualified SemMC.Formula.Eval as E

import qualified SemMC.Synthesis.Cegis.LLVMMem as LLVM


-- | Instantiate occurrences of 'read_mem' that occur in an expression
instantiateReadMem :: forall arch t st fs sym tp.
                   ( sym ~ WE.ExprBuilder t st fs, B.IsSymInterface sym
                   , A.Architecture arch
                   )
                   => sym
                   -> F.Formula sym arch
                   -> (forall tp'. AL.Location arch tp' -> IO (WE.Expr t tp'))
                   -> S.SymExpr sym tp
                   -> IO (S.SymExpr sym tp)
instantiateReadMem sym f evalLoc e =
    E.evaluateFunctions sym
                        (trivialParameterizedFormula f)
                        L.Nil
                        evalLoc
                        (readMemInterp <$> sizes)
                        e
  where
    sizes = [8,16,32,64,128]



trivialParameterizedFormula :: forall sym arch.
                               A.Architecture arch
                            => F.Formula sym arch 
                            -> F.ParameterizedFormula sym arch '[]
trivialParameterizedFormula (F.Formula vars defs) = 
    F.ParameterizedFormula Set.empty L.Nil vars (mapFKeys F.LiteralParameter defs)

mapFKeys :: forall keys keys' res.
            OrdF keys'
         => (forall (tp :: S.BaseType). keys tp -> keys' tp) 
         -> MapF.MapF keys res 
         -> MapF.MapF keys' res
mapFKeys f m = MapF.foldrWithKey (\k -> MapF.insert (f k)) MapF.empty m

readMemInterp :: forall arch t st fs sym.
                ( sym ~ WE.ExprBuilder t st fs, B.IsSymInterface sym
                , A.Architecture arch
                )
              => Integer
              -> (String, A.Evaluator arch t st fs)
readMemInterp n = ( A.createSymbolicName (A.readMemUF @arch n)
                  , A.Evaluator readMemEvaluator)

-- read_mem is not an operand, so we throw an error if 'sh' is not 'Nil'
readMemEvaluator :: forall arch sym t st fs sh u tp.
                       ( sym ~ WE.ExprBuilder t st fs, B.IsSymInterface sym
                       , A.Architecture arch
                       )
                 => sym
                 -> F.ParameterizedFormula sym arch sh
                 -> L.List (A.AllocatedOperand arch sym) sh
                 -- ^ We expect this list to be empty
                 -> Ctx.Assignment (WE.Expr t) u
                 -- ^ The input to read_mem, in this case an array corresponding
                 -- to memory along with the index into the array at which to start reading
                 -> (forall ltp . A.Location arch ltp -> IO (WE.Expr t ltp))
                 -> S.BaseTypeRepr tp
                 -- ^ The result type, a bit-vector of size w
                 -> IO (WE.Expr t tp)
readMemEvaluator sym _f L.Nil (Ctx.Empty Ctx.:> mem Ctx.:> i) evalLoc (S.BaseBVRepr w)
  | Just S.Refl <- S.testEquality (S.exprType mem) (A.memTypeRepr @arch)
  , S.BaseBVRepr iType <- S.exprType i
  , S.NatEQ <- S.compareNat iType (S.knownNat @(A.RegWidth arch))
  , S.NonZeroNat <- S.isZeroNat iType
    = readMemEvaluatorTotal @arch sym evalLoc w mem i
readMemEvaluator _ _ _ _ _ _ = error "read_mem called with incorrect arguments and cannot be evaluated"




-- | Interpretes a 'readMem' as a function over well-typed symbolic expressions
-- as a sequence of reads of primitive memory
readMemEvaluatorTotal :: forall arch sym w t st fs.
                       ( sym ~ WE.ExprBuilder t st fs, B.IsSymInterface sym
                       , A.Architecture arch
                       , 1 S.<= w
                       )
                     => sym
                     -- ^ The expression builder
                     -> (forall ltp . A.Location arch ltp -> IO (WE.Expr t ltp))
                     -> S.NatRepr w
                     -- ^ The number of bits total to read. Will throw an error
                     -- if 'byte' does not evenly divide 'w'
                     -> S.SymExpr sym (A.MemType arch)
                     -- ^ The memory expression to evaluate
                     -> S.SymExpr sym (S.BaseBVType (A.RegWidth arch))
                     -- ^ The index at which to start reading bits
                     -> IO (S.SymExpr sym (S.BaseBVType w))
readMemEvaluatorTotal sym evalLoc w memExpr i
  | [AL.MemLoc w' memLoc] <- AL.memLocation @(AL.Location arch)
  , Just S.Refl <- testEquality w' (S.knownNat @(A.RegWidth arch)) = do
    startingMem <- evalLoc memLoc
    LLVM.withMem @arch sym startingMem $ do
      LLVM.instantiateMemOps memExpr
      LLVM.readMem w i
  | otherwise = error "Could not find memory location for this architecture"
