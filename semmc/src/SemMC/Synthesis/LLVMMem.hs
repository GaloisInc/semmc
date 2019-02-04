{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies, GeneralizedNewtypeDeriving,
  PatternSynonyms, TypeApplications, ScopedTypeVariables, RankNTypes,
  AllowAmbiguousTypes, FlexibleContexts #-}

module SemMC.Synthesis.LLVMMem
  ( MemData(..)
  , MemM(..)
  , withMem
  , withMem'
  , askSym
  , askImpl
  , askBase
  -- , askDefault
  , readMem
  , writeMem
  , doMemAccesses
  , LLVM.MemImpl
--  , LLVM.HasPtrWidth
--  , saveImpl
  , instantiateMemOpsLLVM
  , instantiateLLVMExpr
  ) where

import           Data.Proxy (Proxy(..))
import           Control.Monad.State
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen ( (<+>) )

import qualified Data.Parameterized.Context as Ctx
import qualified Lang.Crucible.Backend as B

import qualified What4.Interface as S
import qualified What4.Expr.Builder as WE
import qualified What4.Concrete as WC

import qualified Lang.Crucible.LLVM.MemModel as LLVM
import qualified Lang.Crucible.LLVM.DataLayout as LLVM
import qualified Lang.Crucible.LLVM.MemType as MemType

import qualified SemMC.Architecture as A
import qualified SemMC.Formula.MemAccesses as MA

data MemData sym arch = MemData { memSym :: sym
                                , memImpl :: LLVM.MemImpl sym
                                , memBase :: LLVM.LLVMPtr sym (A.RegWidth arch)
                                , memAlignment :: LLVM.Alignment
                                -- , defaultValue :: S.SymBV sym 8
                                }
newtype MemM sym arch a = MemM {runMemM :: StateT (MemData sym arch) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Start an LLVM memory model instance consisting of a single block of memory
-- of size 2^w where w is the width of registers in the architecture. All values
-- in memory are initialized to the values of an uninterpreted symbolic array. 
withMem' :: forall arch sym a.
           (A.Architecture arch, B.IsSymInterface sym)
        => sym
        -> S.SymExpr sym (A.MemType arch)
        -> (LLVM.HasPtrWidth (A.RegWidth arch) => MemM sym arch a)
        -> IO a
withMem' sym memExpr op = do
  let w = A.regWidth @arch in LLVM.withPtrWidth w $ do

    -- 1) Construct an LLVM memory implelementation with no blocks
    initMem <- LLVM.emptyMem (A.archEndianForm (Proxy @arch))

    let alignment = LLVM.noAlignment

    -- 2) Allocate a block of uninitialized memory
--    print $ PP.text "Initializing memory of unbounded size"
    (base,mem) <- LLVM.doMallocUnbounded sym LLVM.GlobalAlloc LLVM.Mutable "Mem" 
                                initMem alignment

--    print $ PP.text "Base value: " <+> LLVM.ppPtr base

    -- 3) Create a symbolic array to store in memory
--    let Right memSymbol = S.userSymbol "Mem"
--    let idxRepr = S.BaseArrayRepr (Ctx.empty Ctx.:> S.BaseBVRepr w) (S.BaseBVRepr (S.knownNat @8))
--    uninterpMem <- S.freshConstant sym memSymbol idxRepr
--    mem' <- LLVM.doArrayStore sym mem base alignment uninterpMem wExpr

--    let Right dSymbol = S.userSymbol "d"
--    d <- S.freshConstant sym dSymbol (S.BaseBVRepr (S.knownNat @8))
--    uninterpMem <- S.constantArray sym (Ctx.empty Ctx.:> S.BaseBVRepr w) d
    mem' <- LLVM.doArrayStoreUnbounded sym mem base alignment memExpr

--    print $ PP.text "Creating uninterpreted memory: " <+> LLVM.ppMem mem'

    
    -- 4) Execute the operation with these starting conditions
    evalStateT (runMemM op) (MemData sym mem' base alignment)


-- | Start an LLVM memory model instance consisting of a single block of memory
-- of size 2^w where w is the width of registers in the architecture. All values
-- in memory are initialized to a single uninterpreted constant, which is also returned.
withMem :: forall arch sym a.
           (A.Architecture arch, B.IsSymInterface sym)
        => sym
        -> (LLVM.HasPtrWidth (A.RegWidth arch) => S.SymBV sym 8 -> MemM sym arch a)
        -> IO a
withMem sym op = do
  let Right dSymbol = S.userSymbol "d"
  d <- S.freshConstant sym dSymbol (S.BaseBVRepr (S.knownNat @8))
  uninterpMem <- S.constantArray sym (Ctx.empty Ctx.:> S.BaseBVRepr S.knownNat) d
  withMem' @arch sym uninterpMem (op d)

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

-- | Read 'bits' number of bits starting from location i in memory
readMem :: (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym, 1 S.<= bits)
        => S.NatRepr bits
        -- ^ The number of bits to read
        -> S.SymBV sym (A.RegWidth arch)
        -- ^ The address in memory at which to read
        -> MemM sym arch (S.SymBV sym bits)
readMem bits offset = do
  sym <- askSym
  ptr <- mkPtr offset
  mem <- askImpl
--  liftIO . print $ PP.text "Attempting to read pointer " <+> LLVM.ppPtr ptr
--               <+> PP.text " from mem " <+> LLVM.ppMem mem
  alignment <- askAlignment
  sType <- bvType bits

  v <- liftIO $ LLVM.loadRaw sym mem ptr sType alignment

--  liftIO . print $ PP.text "Successfully read value " <+> PP.text (show v)

  return $ llvmValToSymBV bits v

-- | An LLVMVal represents a plain bitvector if it is an 'LLVMValInt' with block
-- number 0.
llvmValToSymBV :: S.IsExprBuilder sym
               => S.NatRepr bits
               -> LLVM.LLVMVal sym
               -> S.SymBV sym bits
llvmValToSymBV bits (LLVM.LLVMValInt blk val)
  | Just S.Refl <- S.testEquality bits (S.bvWidth val)
  , Just (WC.ConcreteNat 0) <- S.asConcrete blk = val
llvmValToSymBV _ _ = error "LLVMVal given is not a bitvector."


symBVToLLVMVal :: (1 S.<= bits, S.IsExprBuilder sym)
               => S.SymBV sym bits
               -> MemM sym arch (LLVM.LLVMVal sym)
symBVToLLVMVal b = do
  sym <- askSym
  zero <- liftIO $ S.natLit sym 0
  return $ LLVM.LLVMValInt zero b

bvType :: LLVM.HasPtrWidth (A.RegWidth arch)
       => S.NatRepr bits
       -> MemM sym arch LLVM.StorageType
bvType x = LLVM.toStorableType $ MemType.IntType (fromIntegral (S.natValue x))

writeMem :: forall arch sym bits.
            (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym, 1 S.<= bits)
         => S.SymBV sym (A.RegWidth arch)
         -- ^ The address in memory at which to write
         -> S.SymBV sym bits
         -- ^ The value to write
         -> MemM sym arch ()
writeMem offset v = do
  ptr <- mkPtr offset
  mem <- askImpl
--  liftIO . print $ PP.text "Attempting to write value " <+> S.printSymExpr v
--  liftIO . print $ PP.text "to pointer " <+> LLVM.ppPtr ptr
  align <- askAlignment

  sType <- bvType (S.bvWidth v)

  v' <- symBVToLLVMVal v

  sym <- askSym
  mem' <- liftIO $ LLVM.storeRaw sym mem ptr sType align v'

--  liftIO . print $ PP.text "Successfully wrote value"

  putImpl mem'

doMemAccesses :: (A.Architecture arch, B.IsSymInterface sym, LLVM.HasPtrWidth (A.RegWidth arch))
              => [A.AccessData sym arch]
              -> MemM sym arch ()
doMemAccesses [] = return ()
doMemAccesses (A.ReadData _ : ls) = doMemAccesses ls
doMemAccesses (A.WriteData i v : ls) = do
  writeMem i v
  doMemAccesses ls


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
                      => Maybe (S.SymExpr sym (A.MemType arch))
                      -- ^ A symbolic expression representing memory
                      -> MemM sym arch ()
instantiateMemOpsLLVM (Just e) = LLVM.withPtrWidth (S.knownNat @(A.RegWidth arch)) $ 
                                     instantiateLLVMExpr e
instantiateMemOpsLLVM Nothing  = return () -- or error?


isUpdateArray :: forall arch t st fs sym.
              ( sym ~ WE.ExprBuilder t st fs
              , A.Architecture arch
              )
           => S.SymExpr sym (A.MemType arch)
           -> Maybe ( S.SymExpr sym (A.MemType arch)
                    , A.AccessData sym arch)
isUpdateArray (WE.AppExpr a)
  | WE.UpdateArray (S.BaseBVRepr _) (Ctx.Empty Ctx.:> S.BaseBVRepr regWidth) 
                   mem (Ctx.Empty Ctx.:> idx) val <- WE.appExprApp a 
  , Just S.Refl <- S.testEquality regWidth (S.knownNat @(A.RegWidth arch)) = 
    Just (mem, A.WriteData idx val)
isUpdateArray _ | otherwise = Nothing



instantiateLLVMExpr :: forall arch sym t st fs.
                       (sym ~ WE.ExprBuilder t st fs, B.IsSymInterface sym
                       , A.Architecture arch
                       )
                    => S.SymExpr sym (A.MemType arch)
                    -> MemM sym arch ()
instantiateLLVMExpr mem
  | Just (mem', A.WriteData idx val) <- isUpdateArray @arch mem = 
  LLVM.withPtrWidth (S.knownNat @(A.RegWidth arch)) $ do
    instantiateLLVMExpr mem'
    writeMem idx val
instantiateLLVMExpr mem
  | Just (mem', A.WriteData idx val) <- MA.isWriteMem @arch mem = do
  LLVM.withPtrWidth (S.knownNat @(A.RegWidth arch)) $ do
    instantiateLLVMExpr mem'
    writeMem idx val
instantiateLLVMExpr _ | otherwise = return ()
