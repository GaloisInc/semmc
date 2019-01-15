{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies, GeneralizedNewtypeDeriving,
  PatternSynonyms, TypeApplications, ScopedTypeVariables, RankNTypes,
  AllowAmbiguousTypes, FlexibleContexts #-}

module SemMC.Formula.LLVMMem
  ( MemData(..)
  , MemM(..)
  , withMem
  , askSym
  , askImpl
  , askBase
  , askDefault
  , readMem
  , writeMem
  , LLVM.MemImpl
  , LLVM.HasPtrWidth
--  , saveImpl
  , instantiateMemOpsLLVM
  ) where

import           Data.String (fromString)
import           Data.Proxy (Proxy(..))
import           Control.Monad.State
import           Data.Maybe (fromJust)
import           GHC.TypeLits (KnownNat)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen ( (<+>) )

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as N
import qualified Lang.Crucible.Backend as B
import qualified Lang.Crucible.Types as T

import qualified What4.Interface as S
import qualified What4.Expr.Builder as WE
import qualified What4.ProgramLoc as WPL
import qualified What4.FunctionName as WFN

import qualified Lang.Crucible.LLVM.MemModel as LLVM
-- import qualified Lang.Crucible.LLVM.MemModel.Generic as LLVMG
import qualified Lang.Crucible.LLVM.Bytes as LLVM
import qualified Lang.Crucible.LLVM.DataLayout as LLVM

import qualified SemMC.Architecture as A
import qualified SemMC.Formula.MemAccesses as MA
import qualified SemMC.Formula.ReadWriteEval as RW

data MemData sym arch = MemData { memSym :: sym
                                , memImpl :: LLVM.MemImpl sym
                                , memBase :: LLVM.LLVMPtr sym (A.RegWidth arch)
                                , memAlignment :: LLVM.Alignment
                                , defaultValue :: S.SymBV sym 8
                                }
newtype MemM sym arch a = MemM {runMemM :: StateT (MemData sym arch) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Start an LLVM memory model instance consisting of a single block of memory
-- of size 2^w where w is the width of registers in the architecture. All values
-- in memory are initialized to the values of an uninterpreted symbolic array. 
withMem :: forall arch sym a.
           (A.Architecture arch, B.IsSymInterface sym)
        => sym
        -> (LLVM.HasPtrWidth (A.RegWidth arch) => MemM sym arch a)
        -> IO a
withMem sym op = do
  let w = A.regWidth @arch in LLVM.withPtrWidth w $ do

    -- 1) Construct an LLVM memory implelementation with no blocks
    initMem <- LLVM.emptyMem (A.archEndianForm (Proxy @arch))

    let alignment = LLVM.noAlignment

    -- 2) Allocate a block of uninitialized memory
    print $ PP.text "Initializing memory of unbounded size"
    (base,mem) <- LLVM.doMallocUnbounded sym LLVM.GlobalAlloc LLVM.Mutable "Mem" 
                                initMem alignment

    print $ PP.text "Base value: " <+> LLVM.ppPtr base

    -- 3) Create a symbolic array to store in memory
--    let Right memSymbol = S.userSymbol "Mem"
--    let idxRepr = S.BaseArrayRepr (Ctx.empty Ctx.:> S.BaseBVRepr w) (S.BaseBVRepr (S.knownNat @8))
--    uninterpMem <- S.freshConstant sym memSymbol idxRepr
--    mem' <- LLVM.doArrayStore sym mem base alignment uninterpMem wExpr

    let Right dSymbol = S.userSymbol "d"
    d <- S.freshConstant sym dSymbol (S.BaseBVRepr (S.knownNat @8))
    uninterpMem <- S.constantArray sym (Ctx.empty Ctx.:> S.BaseBVRepr w) d
    mem' <- LLVM.doArrayStoreUnbounded sym mem base alignment uninterpMem

    putStrLn $ "Creating uninterpreted memory: " ++ show (LLVM.ppMem mem')

    
    -- 4) Execute the operation with these starting conditions
    evalStateT (runMemM op) (MemData sym mem' base alignment d)

-- Do the first operation but then restore the memImpl from the initial state
saveImpl :: MemM sym w a
         -> MemM sym w a
saveImpl op = do
  origImpl <- askImpl
  a <- op
  putImpl origImpl
  return a

askSym :: MemM sym w sym
askSym = MemM $ memSym <$> get

askImpl :: MemM sym w (LLVM.MemImpl sym)
askImpl = MemM $ memImpl <$> get

askBase :: MemM sym arch (LLVM.LLVMPtr sym (A.RegWidth arch))
askBase = MemM $ memBase <$> get

askAlignment :: MemM sym arch (LLVM.Alignment)
askAlignment = MemM $ memAlignment <$> get

putImpl :: LLVM.MemImpl sym -> MemM sym w ()
putImpl m = MemM $ do 
  memData <- get
  put $ memData{memImpl = m}

askDefault :: MemM sym w (S.SymBV sym 8)
askDefault = MemM $ do
  memData <- get
  return $ defaultValue memData

-- Input: a bit vector representing the offset from the base ptr for memory
mkPtr :: forall arch sym.
         (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym)
      => S.SymBV sym (A.RegWidth arch)
      -> MemM sym arch (LLVM.LLVMPtr sym (A.RegWidth arch))
mkPtr offset = do
  sym <- askSym
  basePtr <- askBase
  let w = LLVM.ptrWidth basePtr
  liftIO $ LLVM.ptrAdd sym w basePtr offset

--  LLVM.LLVMPointer _ base <- askBase
--  liftIO $ S.bvAdd sym base offset >>= LLVM.llvmPointer_bv sym

-- | Read 'bytes' number of bits starting from location i in memory
readMem :: (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym, 1 S.<= bytes)
        => S.NatRepr bytes
        -- ^ The number of bytes to read
        -> S.SymBV sym (A.RegWidth arch)
        -- ^ The address in memory at which to read
        -> MemM sym arch (S.SymBV sym bytes)
readMem bytes offset = do
  sym <- askSym
  ptr <- mkPtr offset
  mem <- askImpl
  liftIO . print $ PP.text "Attempting to read pointer " <+> LLVM.ppPtr ptr
               <+> PP.text " from mem " <+> LLVM.ppMem mem
  alignment <- askAlignment
  v <- liftIO $ LLVM.doLoad sym mem ptr (bvType bytes) (T.BVRepr bytes) alignment
  liftIO . print $ PP.text "Successfully read value " <+> S.printSymExpr v
  return v

bvType :: S.NatRepr bytes -> LLVM.StorageType
bvType x = LLVM.bitvectorType . LLVM.bitsToBytes $ S.natValue x

writeMem :: (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym, 1 S.<= bytes)
         => S.SymBV sym (A.RegWidth arch)
         -- ^ The address in memory at which to write
         -> S.SymBV sym bytes
         -- ^ The value to write
         -> MemM sym arch ()
writeMem offset v = do
  sym <- askSym
  ptr <- mkPtr offset
  mem <- askImpl
  liftIO . print $ PP.text "Attempting to write value " <+> S.printSymExpr v
  liftIO . print $ PP.text "to pointer " <+> LLVM.ppPtr ptr
  liftIO . print $ PP.text "to MemImpl " <+> LLVM.ppMem mem
  let vType = bvType (S.bvWidth v)
--  v' <- symBVToVal v
  align <- askAlignment

  -- instead of using 'storeRaw' here, we want to do the same operation but
  -- without checking that the index is in the bounds, since we don't care about
  -- that.
  -- mem' <- liftIO $ myStore sym mem ptr vType align v'
  mem' <- liftIO $ LLVM.doStore sym mem ptr (T.BVRepr (S.bvWidth v)) vType align v

  liftIO . print $ PP.text "Successfully wrote value: " <+> LLVM.ppMem mem'
  putImpl mem'


{-
-- | Store an LLVM value in memory. Does not assert that the pointer is valid
-- and points to a mutable memory region.
myStore :: (B.IsSymInterface sym, LLVM.HasPtrWidth wptr)
  => sym
  -> LLVM.MemImpl sym
  -> LLVM.LLVMPtr sym wptr {- ^ pointer to store into -}
  -> LLVM.StorageType      {- ^ type of value to store -}
  -> LLVM.Alignment
  -> LLVM.LLVMVal sym      {- ^ value to store -}
  -> IO (LLVM.MemImpl sym)
myStore sym mem ptr valType alignment val = do
    (p, heap') <- LLVMG.writeMem sym LLVM.PtrWidth ptr valType alignment val (LLVM.memImplHeap mem)
    let reason = B.AssumptionReason (WPL.mkProgramLoc (WFN.functionNameFromText (fromString "NoFunction")) WPL.InternalPos) 
                                    "Presume index is within the bounds"
    B.addAssumption sym (B.LabeledPred p reason)
--    let errMsg = ptrMessage "Invalid memory store." ptr valType
--    assert sym p (AssertFailureSimError errMsg)
    return mem{ LLVM.memImplHeap = heap' }
-}



---------------------------------------


-- | Interpret a symbolic array as a sequence of operations on the memory.
--
-- We assume that the expression is an array built up from call to
-- 'arrayUpdate' and 'write_mem'. If the memory is constructed in a different
-- way, that array will just be written directly to the memory model, not taking
-- recursive structure into account.
--
-- TODO: add support for 'arrayIte' and 'arrayFromMap'?
instantiateMemOpsLLVM :: forall sym t st fs arch.
                         ( sym ~ WE.ExprBuilder t st fs
                         , A.Architecture arch
                         , B.IsSymInterface sym)
                      => Maybe (S.SymArray sym (Ctx.SingleCtx (S.BaseBVType (A.RegWidth arch))) (S.BaseBVType 8))
                      -- ^ A symbolic expression representing memory
                      -> MemM sym arch ()
instantiateMemOpsLLVM (Just e) = LLVM.withPtrWidth (S.knownNat @(A.RegWidth arch)) $ 
                                     instantiateLLVMExpr e
instantiateMemOpsLLVM Nothing  = return () -- or error?


isUpdateArray :: forall arch t st fs sym bytes.
              ( sym ~ WE.ExprBuilder t st fs
              , A.Architecture arch
              )
           => S.SymArray sym (Ctx.SingleCtx (S.BaseBVType (A.RegWidth arch))) (S.BaseBVType bytes)
           -> Maybe ( S.SymArray sym (Ctx.SingleCtx (S.BaseBVType (A.RegWidth arch))) (S.BaseBVType bytes) 
                    , A.AccessData sym arch)
isUpdateArray (WE.AppExpr a)
  | WE.UpdateArray (S.BaseBVRepr _) (Ctx.Empty Ctx.:> S.BaseBVRepr regWidth) 
                   mem (Ctx.Empty Ctx.:> idx) val <- WE.appExprApp a 
  , Just S.Refl <- S.testEquality regWidth (S.knownNat @(A.RegWidth arch)) = 
    Just (mem, A.WriteData idx val)
isUpdateArray _ | otherwise = Nothing



instantiateLLVMExpr :: forall arch sym t st fs.
                       (sym ~ WE.ExprBuilder t st fs, B.IsSymInterface sym
                       , A.Architecture arch, LLVM.HasPtrWidth (A.RegWidth arch)
                       )
                    => S.SymArray sym (Ctx.SingleCtx (S.BaseBVType (A.RegWidth arch))) (S.BaseBVType 8)
                    -> MemM sym arch ()
instantiateLLVMExpr mem
  | Just (mem', A.WriteData idx val) <- isUpdateArray @arch mem = do
  instantiateLLVMExpr mem'
  writeMem idx val
instantiateLLVMExpr mem 
  | Just (mem', A.WriteData idx val) <- RW.isSomeWriteMem @arch mem = do
  instantiateLLVMExpr mem'
  writeMem idx val
instantiateLLVMExpr _ | otherwise = return ()

{-
  -- Write the memory expression to the underlying memory model
  sym <- askSym
  impl <- askImpl
  ptr <- askBase
  alignment <- askAlignment
  let width = S.knownNat @(A.RegWidth arch)
  lenExpr <- liftIO $ S.bvLit sym width (2^(S.natValue width)-1)
  impl' <- liftIO $ LLVM.doArrayStore sym impl ptr alignment mem lenExpr 
  putImpl impl'
-}
