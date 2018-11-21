{-# LANGUAGE RankNTypes, TypeOperators, TypeApplications, DataKinds, 
TypeFamilies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Evaluators for readMem and writeMem uninterpreted functions
module SemMC.Formula.ReadWriteEval
  ( memOpInterpretation
  , readMemEvaluator
  , writeMemEvaluator
  ) where

import Text.Printf ( printf )

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.List as L
import           Data.Parameterized.Some (Some(..))

import qualified What4.Interface as S
import qualified What4.Expr as WE

import qualified SemMC.Architecture as A
import qualified SemMC.Formula.Formula as F

import qualified SemMC.DSL as DSL

{-
-- | Read from the pseudo-location "Memory"
--
-- should go in 'SemMC.DSL', which can then be used by architecture-specific
-- core/semantics?
readMemSemantics :: DSL.Expr DSL.TMemory
                 -- ^ The memory
                 -> DSL.Expr DSL.TBV
                 -- ^ The effective address to load
                 -> Int
                 -- ^ The number of bytes
                 -> DSL.Expr DSL.TBV
readMemSemantics mem ea nBytes =
  DSL.uf (DSL.EBV (8 * nBytes)) funcName [Some mem, Some ea]
  where
    funcName :: String
    funcName = printf "read_mem.%d" (nBytes * 8)
-}


-- | Interpretation of readMemUF and writeMemUF
memOpInterpretation :: A.Architecture arch => proxy arch -> [(String, A.Evaluator arch t st fs)]
memOpInterpretation proxy = (readMemInterp end <$> sizes) ++ (writeMemInterp end <$> sizes)
  where
    end   = A.archEndianness proxy
    sizes = [8,16,32,64,128]

readMemInterp :: forall arch t st fs. 
                 A.Architecture arch
              => A.Endianness ->  Integer -> (String, A.Evaluator arch t st fs)
readMemInterp end n = let f = A.readMemUF @arch n
                      in (A.createSymbolicName (A.uninterpFnName f), readMemEvaluator end)

writeMemInterp :: forall arch t st fs. 
                  A.Architecture arch
               => A.Endianness ->  Integer -> (String, A.Evaluator arch t st fs)
writeMemInterp end n = let f = A.writeMemUF @arch n
                       in (A.createSymbolicName (A.uninterpFnName f), writeMemEvaluator end)


-- | The evaluator for reading bits from memory
readMemEvaluator :: A.Endianness -> A.Evaluator arch t st fs
readMemEvaluator endianness = A.Evaluator (readMemEvaluator' endianness)

-- read_mem is not an operand, so we throw an error if 'sh' is not 'Nil'
readMemEvaluator' :: 
                    A.Endianness
                 -> WE.ExprBuilder t st fs
                 -> F.ParameterizedFormula (WE.ExprBuilder t st fs) arch sh
                 -> L.List (A.AllocatedOperand arch (WE.ExprBuilder t st fs)) sh
                 -- ^ We expect this list to be empty
                 -> Ctx.Assignment (WE.Expr t) u
                 -- ^ The input to read_mem, in this case an array corresponding
                 -- to memory along with the index into the array at which to start reading
                 -> (forall ltp . A.Location arch ltp -> IO (WE.Expr t ltp))
                 -> S.BaseTypeRepr tp
                 -- ^ The result type, a bit-vector of size w
                 -> IO (WE.Expr t tp)
readMemEvaluator' endianness sym _f L.Nil (Ctx.Empty Ctx.:> mem Ctx.:> i) _ (S.BaseBVRepr w) 
  | S.BaseArrayRepr (Ctx.Empty Ctx.:> S.BaseBVRepr iType') (S.BaseBVRepr byte) <- S.exprType mem
  , S.BaseBVRepr iType <- S.exprType i
  , S.NatEQ <- S.compareNat iType iType'
  , S.NonZeroNat <- S.isZeroNat iType
  , S.NonZeroNat <- S.isZeroNat byte
    = readMemEvaluatorTotal sym endianness byte w mem i
readMemEvaluator' _ _ _ _ _ _ _ = error "read_mem called with incorrect arguments and cannot be evaluated"

-- | Interpretes a 'readMem' as a function over well-typed symbolic expressions
readMemEvaluatorTotal :: forall sym byte w i.
                        (S.IsExprBuilder sym, 1 S.<= byte, 1 S.<= w, 1 S.<= i)
                     => sym
                     -- ^ The expression builder
                     -> A.Endianness
                     -- ^ The endianness of the architecture
                     -> S.NatRepr byte
                     -- ^ The number of bits in a single register in the array; often a byte
                     -> S.NatRepr w
                     -- ^ The number of bits total to read. Will throw an error
                     -- if 'byte' does not evenly divide 'w'
                     -> S.SymExpr sym (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType i)) (S.BaseBVType byte))
                     -- ^ An expression representing memory
                     -> S.SymExpr sym (S.BaseBVType i)
                     -- ^ The index at which to start reading bits
                     -> IO (S.SymExpr sym (S.BaseBVType w))
readMemEvaluatorTotal sym endianness byte w mem i = 
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
        bs <- readMemEvaluatorTotal sym endianness byte w_minus_byte mem i'
        bvConcatEndian endianness b bs

    -- if the number of bits we need to read is less than the number of bits
    -- stored in a single block of memory, then we have asked for some
    -- number of bits that does not divide evenly and throw an error
    S.NatLT _ -> error $ "read_mem cannot be evaluated: "
                      ++ "the number of bits requested is not a mulitple of the dimension of the array"

  where
    myLeqPf :: S.NatRepr n -> S.LeqProof 1 (n S.+ 1)
    myLeqPf n | S.Refl <- S.plusComm n (S.knownNat @1) = S.leqAdd (S.leqRefl (S.knownNat @1)) n

    -- with a big endian representation, we concatinate bit vectors in the same
    -- direction as they are alid out in memory; with little endian, the opposite
    bvConcatEndian :: (1 S.<= u, 1 S.<= v)
                   => A.Endianness -> S.SymBV sym u -> S.SymBV sym v  -> IO (S.SymBV sym (u S.+ v))
    bvConcatEndian A.BigEndian    u v = S.bvConcat sym u v
    bvConcatEndian A.LittleEndian u v 
      | S.Refl <- S.plusComm (S.bvWidth u) (S.bvWidth v) = S.bvConcat sym v u


-- | The evaluator for writing bits from memory
writeMemEvaluator :: A.Endianness -> A.Evaluator arch t st fs
writeMemEvaluator endianness = A.Evaluator (writeMemEvaluator' endianness)

-- write_mem is not an operand, so we throw an error if 'sh' is not 'Nil'
writeMemEvaluator' :: 
                    A.Endianness
                 -> WE.ExprBuilder t st fs
                 -> F.ParameterizedFormula (WE.ExprBuilder t st fs) arch sh
                 -> L.List (A.AllocatedOperand arch (WE.ExprBuilder t st fs)) sh
                 -- ^ We expect this list to be empty
                 -> Ctx.Assignment (WE.Expr t) u
                 -- ^ The input to write_mem, in this case an array
                 -- corresponding to memory along, the index into the array at
                 -- which to start writing, and the value to write
                 -> (forall ltp . A.Location arch ltp -> IO (WE.Expr t ltp))
                 -> S.BaseTypeRepr tp
                 -- ^ The result type, matching the memory
                 -> IO (WE.Expr t tp)
writeMemEvaluator' endianness sym _f L.Nil (Ctx.Empty Ctx.:> mem Ctx.:> i Ctx.:> v) _ memType
  | S.BaseArrayRepr (Ctx.Empty Ctx.:> S.BaseBVRepr iType') (S.BaseBVRepr byte) <- S.exprType mem
  , S.BaseBVRepr iType <- S.exprType i
  , S.NatEQ <- S.compareNat iType iType'
  , S.NonZeroNat <- S.isZeroNat iType
  , S.NonZeroNat <- S.isZeroNat byte
  , Just S.Refl <- S.testEquality (S.exprType mem) memType
  , S.BaseBVRepr _ <- S.exprType v
    = writeMemEvaluatorTotal sym endianness byte mem i v
writeMemEvaluator' _ _ _ _ _ _ _ = error "write_mem called with incorrect arguments and cannot be evaluated"

-- | Interpretes a 'writeMem' as a function over well-typed symbolic expressions
writeMemEvaluatorTotal :: forall sym byte w i.
                        (S.IsExprBuilder sym, 1 S.<= byte, 1 S.<= w, 1 S.<= i)
                     => sym
                     -- ^ The expression builder
                     -> A.Endianness
                     -- ^ The endianness of the architecture
                     -> S.NatRepr byte
                     -- ^ The number of bits in a single register in the array; often a byte
                     -> S.SymExpr sym (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType i)) (S.BaseBVType byte))
                     -- ^ An expression representing memory
                     -> S.SymExpr sym (S.BaseBVType i)
                     -- ^ The index at which to start writing data bits
                     -> S.SymExpr sym (S.BaseBVType w)
                     -- ^ The value to write to memory
                     -> IO (S.SymExpr sym (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType i)) 
                                                           (S.BaseBVType byte)))
writeMemEvaluatorTotal sym endianness byte mem i v =
  case S.compareNat (S.bvWidth v) byte of
    -- if the number of bits to write is equal to the number of bits stored in
    -- one block of memory, then just write the result in memory
    S.NatEQ -> S.arrayUpdate sym mem (Ctx.Empty Ctx.:> i) v

    -- if the number of bits we need to write is greater than the number of
    -- bits stored in a single block of memory, then write a single block
    -- and then recurse
    S.NatGT w_minus_byte_minus1 
      -- we need a proof that 1 <= (w-byte-1)+1
      | S.LeqProof <- myLeqPf w_minus_byte_minus1 -> do
        let w_minus_byte = S.addNat w_minus_byte_minus1 (S.knownNat @1)
        (blockHere, blockThere) <- bvSelectEndian endianness w_minus_byte v
        mem' <- S.arrayUpdate sym mem (Ctx.Empty Ctx.:> i) blockHere
        one <- S.bvLit sym (S.bvWidth i) 1
        i' <- S.bvAdd sym i one
        writeMemEvaluatorTotal sym endianness byte mem' i' blockThere

    -- if the number of bits we need to write is less than the number of bits
    -- stored in a single block of memory, then we have asked for some
    -- number of bits that does not divide evenly and throw an error
    S.NatLT _ -> error $ "write_mem cannot be evaluated: "
                      ++ "the number of bits requested is not a mulitple of the dimension of the array"

  where

    myLeqPf :: S.NatRepr n -> S.LeqProof 1 (n S.+ 1)
    myLeqPf n | S.Refl <- S.plusComm n (S.knownNat @1) = S.leqAdd (S.leqRefl (S.knownNat @1)) n


    -- with a big endian representation, we write the most significant bits first
    bvSelectEndian :: 1 S.<= rest
                   => A.Endianness 
                   -> S.NatRepr rest
                   -> S.SymBV sym (byte S.+ rest)
                   -> IO (S.SymBV sym byte, S.SymBV sym rest)
    bvSelectEndian A.LittleEndian rest result 
      -- need a proof that @byte <= byte + rest@
      | S.LeqProof <- S.addIsLeq byte rest = do
        -- take 'byte' nuber of bits starting from index 0 (the least signficant bit)
        leastSig <- S.bvSelect sym (S.knownNat @0) byte result
        mostSig <- S.bvSelect sym byte rest result
        return (leastSig, mostSig)
    bvSelectEndian A.BigEndian rest result
      -- need a proof that @rest <= byte + rest@
      | S.LeqProof <- S.addPrefixIsLeq byte rest 
      -- and a proof that @byte+rest = rest+byte@
      , S.Refl <- S.plusComm byte rest = do
        -- take 'byte' nuber of bits starting from index 'rest'
        mostSig  <- S.bvSelect sym rest byte result
        leastSig <- S.bvSelect sym (S.knownNat @0) rest result
        return (mostSig, leastSig)
