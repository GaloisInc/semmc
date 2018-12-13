{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies, GeneralizedNewtypeDeriving,
  PatternSynonyms #-}

module SemMC.Formula.LLVMMem
  ( MemM(..)
  , withMem
  , askSym
  , askImpl
  , askBase
  , readMem
  , writeMem
  ) where

import           Control.Monad.State

import qualified What4.Interface as S
import qualified Lang.Crucible.Backend as B

import qualified Lang.Crucible.LLVM.MemModel as LLVM
import qualified Lang.Crucible.LLVM.Bytes as LLVM
import qualified Lang.Crucible.LLVM.DataLayout as LLVM


data MemData sym w = MemData { memSym :: sym
                             , memImpl :: LLVM.MemImpl sym
                             , memBase :: LLVM.LLVMPtr sym w
                             }
newtype MemM sym w a = MemM {runMemM :: StateT (MemData sym w) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

withMem :: (B.IsSymInterface sym, LLVM.HasPtrWidth w)
        => sym
        -> LLVM.EndianForm
        -> S.NatRepr w
        -> MemM sym w a
        -> IO a
withMem sym end w op = do
  initMem <- LLVM.emptyMem end
  wExpr   <- S.bvLit sym w (S.natValue w)
  (base,mem) <- LLVM.doMalloc sym LLVM.GlobalAlloc LLVM.Mutable "Mem" initMem wExpr
  evalStateT (runMemM op) (MemData sym mem base)

askSym :: MemM sym w sym
askSym = MemM $ memSym <$> get

askImpl :: MemM sym w (LLVM.MemImpl sym)
askImpl = MemM $ memImpl <$> get

askBase :: MemM sym w (LLVM.LLVMPtr sym w)
askBase = MemM $ memBase <$> get

putImpl :: LLVM.MemImpl sym -> MemM sym w ()
putImpl m = MemM $ do 
  memData <- get
  put $ memData{memImpl = m}

-- Input: a bit vector representing the offset from the base ptr for memory
mkPtr :: (B.IsSymInterface sym, LLVM.HasPtrWidth w)
      => S.SymBV sym w
      -> MemM sym w (LLVM.LLVMPtr sym w)
mkPtr offset = do
  sym <- askSym
  LLVM.LLVMPointer _ base <- askBase
  liftIO $ S.bvAdd sym base offset >>= LLVM.llvmPointer_bv sym

readMem :: (B.IsSymInterface sym, LLVM.HasPtrWidth w, 1 S.<= bytes)
        => S.NatRepr bytes
        -> S.SymBV sym w
        -> MemM sym w (S.SymBV sym bytes)
readMem bytes offset = do
  sym <- askSym
  ptr <- mkPtr offset
  mem <- askImpl
  v <- liftIO $ LLVM.loadRaw sym mem ptr (bvType bytes)
  valToSymBV bytes v

bvType :: S.NatRepr bytes -> LLVM.Type
bvType x = LLVM.bitvectorType . LLVM.toBytes $ S.natValue x

writeMem :: (B.IsSymInterface sym, LLVM.HasPtrWidth w, 1 S.<= bytes)
         => S.SymBV sym w
         -> S.SymBV sym bytes
         -> MemM sym w ()
writeMem offset v = do
  sym <- askSym
  ptr <- mkPtr offset
  mem <- askImpl
  let vType = bvType (S.bvWidth v)
  v' <- symBVToVal v
  mem' <- liftIO $ LLVM.storeRaw sym mem ptr vType v'
  putImpl mem'


valToSymBV :: (S.IsExprBuilder sym, 1 S.<= bytes)
           => S.NatRepr bytes
           -> LLVM.LLVMVal sym
           -> MemM sym w (S.SymBV sym bytes)
valToSymBV bytes (LLVM.LLVMValInt _ v) 
  | Just S.Refl <- S.testEquality bytes (S.bvWidth v)
  = return v
valToSymBV bytes (LLVM.LLVMValZero tp)
  | LLVM.Bitvector bytes' <- LLVM.typeF tp
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
