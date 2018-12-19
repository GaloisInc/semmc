{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies, GeneralizedNewtypeDeriving,
  PatternSynonyms, TypeApplications, ScopedTypeVariables, RankNTypes,
  AllowAmbiguousTypes #-}

module SemMC.Formula.LLVMMem
  ( MemM(..)
  , withMem
  , askSym
  , askImpl
  , askBase
  , readMem
  , writeMem
  , LLVM.MemImpl
  , LLVM.HasPtrWidth
  , saveImpl
  ) where

import           Data.Proxy (Proxy(..))
import           Control.Monad.State
import           Data.Maybe (fromJust)

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as N
import qualified What4.Interface as S
import qualified Lang.Crucible.Backend as B

import qualified Lang.Crucible.LLVM.MemModel as LLVM
import qualified Lang.Crucible.LLVM.Bytes as LLVM
import qualified Lang.Crucible.LLVM.DataLayout as LLVM

import qualified SemMC.Architecture as A

data MemData sym arch = MemData { memSym :: sym
                                , memImpl :: LLVM.MemImpl sym
                                , memBase :: LLVM.LLVMPtr sym (A.RegWidth arch)
                                , memAlignment :: LLVM.Alignment
                                }
newtype MemM sym arch a = MemM {runMemM :: StateT (MemData sym arch) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | TODO: I want allocation to also allocate some 'default' values. But what is
-- the size of this default value?
withMem :: forall arch sym a.
           (A.Architecture arch, B.IsSymInterface sym)
        => sym
--        -> S.NatRepr (A.RegWidth arch)
         -- ^ the width of addresses in memory
        -> (LLVM.HasPtrWidth (A.RegWidth arch) => MemM sym arch a)
        -> IO a
withMem sym op = do
  let w = A.regWidth @arch in LLVM.withPtrWidth w $ do
    initMem <- LLVM.emptyMem (A.archEndianForm (Proxy @arch))
    wExpr   <- S.bvLit sym w (S.natValue w)
    -- trivial alignment; will produce a 'Just' if the number is a power of 2
    let alignment = fromJust $ LLVM.toAlignment $ LLVM.toBytes 1
    (base,mem) <- LLVM.doMalloc sym LLVM.GlobalAlloc LLVM.Mutable "Mem" 
                                initMem wExpr alignment
    evalStateT (runMemM op) (MemData sym mem base alignment)

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

--askLocMem :: MemM sym arch (A.Location arch (S.BaseArrayType (Ctx.SingleCtx (S.BaseBVType (A.RegWidth arch))) tp))
--askLocMem = undefined


putImpl :: LLVM.MemImpl sym -> MemM sym w ()
putImpl m = MemM $ do 
  memData <- get
  put $ memData{memImpl = m}

-- Input: a bit vector representing the offset from the base ptr for memory
mkPtr :: (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym)
      => S.SymBV sym (A.RegWidth arch)
      -> MemM sym arch (LLVM.LLVMPtr sym (A.RegWidth arch))
mkPtr offset = do
  sym <- askSym
  LLVM.LLVMPointer _ base <- askBase
  liftIO $ S.bvAdd sym base offset >>= LLVM.llvmPointer_bv sym

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
  v <- liftIO $ LLVM.loadRaw sym mem ptr (bvType bytes)
  valToSymBV bytes v

bvType :: S.NatRepr bytes -> LLVM.StorageType
bvType x = LLVM.bitvectorType . LLVM.toBytes $ S.natValue x

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
  let vType = bvType (S.bvWidth v)
  v' <- symBVToVal v
  align <- askAlignment
  mem' <- liftIO $ LLVM.storeRaw sym mem ptr vType align v'
  putImpl mem'


valToSymBV :: (S.IsExprBuilder sym, 1 S.<= bytes)
           => S.NatRepr bytes
           -> LLVM.LLVMVal sym
           -> MemM sym arch (S.SymBV sym bytes)
valToSymBV bytes (LLVM.LLVMValInt _ v) 
  | Just S.Refl <- S.testEquality bytes (S.bvWidth v)
  = return v
valToSymBV bytes (LLVM.LLVMValZero tp)
  | LLVM.Bitvector bytes' <- LLVM.storageTypeF tp
  , S.natValue bytes == LLVM.bytesToInteger bytes' = do 
    sym <- askSym
    liftIO $ S.bvLit sym bytes 0
valToSymBV _ _ = error "LLVM value is not a bit vector"

symBVToVal :: (S.IsExprBuilder sym, 1 S.<= bytes)
           => S.SymBV sym bytes
           -> MemM sym w (LLVM.LLVMVal sym)
symBVToVal v = do
  sym <- askSym
  vNat <- liftIO $ S.bvToNat sym v
  return $ LLVM.LLVMValInt vNat v
