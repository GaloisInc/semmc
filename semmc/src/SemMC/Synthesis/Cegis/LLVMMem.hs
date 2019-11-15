{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies, GeneralizedNewtypeDeriving,
  PatternSynonyms, TypeApplications, ScopedTypeVariables, RankNTypes,
  AllowAmbiguousTypes, FlexibleContexts, ViewPatterns #-}

module SemMC.Synthesis.Cegis.LLVMMem
  ( -- * LLVM memory monad
    MemM
  , withMem
  , askImpl
  , askBase
  -- * Memory operations
  , readMem
  , readMemIO
  , readMemOF
  , writeMem
  , doWrites
  , instantiateMemOps
  ) where

import           Data.Proxy (Proxy(..))
import           Control.Monad.State
-- import qualified Text.PrettyPrint.ANSI.Leijen as PP
-- import           Text.PrettyPrint.ANSI.Leijen ( (<+>) )

import           Data.Parameterized.Some (Some(..))
import qualified Data.Parameterized.Context as Ctx
import qualified Lang.Crucible.Backend as B

import qualified What4.Interface as S
import qualified What4.Expr.Builder as WE
import qualified What4.Concrete as WC

import qualified Lang.Crucible.LLVM.MemModel as LLVM
import qualified Lang.Crucible.LLVM.DataLayout as LLVM
import qualified Lang.Crucible.LLVM.MemType as MemType

import qualified SemMC.Architecture as A
import qualified SemMC.Synthesis.Cegis.Types as T
import qualified SemMC.Synthesis.Cegis.MemAccesses as MA

data MemData sym arch = MemData { memSym :: sym
                                , memImpl :: LLVM.MemImpl sym
                                , memBase :: LLVM.LLVMPtr sym (A.RegWidth arch)
                                , memAlignment :: LLVM.Alignment
                                , memExpr :: S.SymExpr sym (A.MemType arch)
                                }

-- | This monad has an underlying 'LLMV.MemImpl' that holds the current state of
-- the memory, which is an unbounded array of bytes indexed by @RegWidth
-- arch@-bit bitvectors.
newtype MemM sym arch a = MemM {runMemM :: StateT (MemData sym arch) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance T.HasMemExpr MemM
  where
    askMemExpr = MemM $ memExpr <$> get

instance T.HasSym MemM
  where
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

-- | Start an LLVM memory model instance consisting of a single block of memory
-- of size 2^w where w is the width of registers in the architecture. All values
-- in memory are initialized to the values of an uninterpreted symbolic array. 
withMem :: forall arch sym a.
           (A.Architecture arch, B.IsSymInterface sym)
        => sym
        -> S.SymExpr sym (A.MemType arch)
        -> (LLVM.HasPtrWidth (A.RegWidth arch) => MemM sym arch a)
        -> IO a
withMem sym memExp op = do
  let w = A.regWidth @arch in LLVM.withPtrWidth w $ do

    -- 1) Construct an LLVM memory implelementation with no blocks
    initMem <- LLVM.emptyMem (A.archEndianForm (Proxy @arch))

    -- 2) Allocate a block of uninitialized memory
    (base,mem) <- LLVM.doMallocUnbounded sym LLVM.GlobalAlloc LLVM.Mutable "Mem" 
                                initMem LLVM.noAlignment

    -- 3) Write the initial memory expression array to the allocated block
    mem' <- LLVM.doArrayStoreUnbounded sym mem base LLVM.noAlignment memExp
    
    -- 4) Execute the operation with these starting conditions
    evalStateT (runMemM op) (MemData sym mem' base LLVM.noAlignment memExp)


-- Input: a bit vector representing the offset from the base ptr for memory
mkPtr :: forall arch sym.
         (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym)
      => S.SymBV sym (A.RegWidth arch)
      -> MemM sym arch (LLVM.LLVMPtr sym (A.RegWidth arch))
mkPtr offset = do
  sym <- T.askSym
  basePtr <- askBase
  let w = LLVM.ptrWidth basePtr
  liftIO $ LLVM.ptrAdd sym w basePtr offset

--  LLVM.LLVMPointer _ base <- askBase
--  liftIO $ S.bvAdd sym base offset >>= LLVM.llvmPointer_bv sym

-- | Read 'bits' number of bits starting from location i in memory
--
-- Precondition: offset+bitsToBytes(bits) <= A.RegWidth arch
readMemNoOF :: (LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym, 1 S.<= bits)
            => S.NatRepr bits
            -- ^ The number of bits to read
            -> S.SymBV sym (A.RegWidth arch)
            -- ^ The address in memory at which to read
            -> MemM sym arch (S.SymBV sym bits)
readMemNoOF bits offset = do
  sym <- T.askSym
  ptr <- mkPtr offset
  mem <- askImpl
  -- liftIO . print $ PP.text "Attempting to read " <+> PP.text (show bits) <+> PP.text " bits "
  --              <+> PP.text " from pointer " <+> LLVM.ppPtr ptr
  --              <+> PP.text " from mem " <+> LLVM.ppMem mem
  alignment <- askAlignment
  sType <- bvType bits

  pv <- liftIO $ LLVM.loadRaw sym mem ptr sType alignment
  v <- liftIO $ LLVM.assertSafe sym pv
  let v' = llvmValToSymBV bits v

  -- liftIO . print $ PP.text "Successfully read value " --  <+> S.printSymExpr v'

  return v'

checkOverflowITE :: forall bytes a.
                    S.NatRepr bytes
                 -- ^ The number of bytes to read
                 -> Integer
                 -- ^ The address in memory at which to read
                 -> Integer
                 -- ^ The size of the address space
                 -> (forall bytes1 bytes2.
                     (bytes ~ (bytes1 S.+ bytes2) , 1 S.<= bytes1, 1 S.<= bytes2)
                     => S.NatRepr bytes1 -> S.NatRepr bytes2 -> a)
                 -- ^ If overflow would have occurred, split @bytes@ into @bytes1@ and @bytes2@ such that
                 -- @off + bytes1 = maxsize@ and execute this continuation
                 -> a
                 -- ^ If overflow would not have occurred, execute this continuation
                 -> a
checkOverflowITE bytes off maxSize trueCont falseCont =
    if off + S.intValue bytes <= maxSize
    then falseCont
    else let bytes1 = maxSize - off
             bytes2 = S.intValue bytes - bytes1
         in runTrue (S.someNat bytes1) (S.someNat bytes2)
  where
    runTrue :: Maybe (Some S.NatRepr) -> Maybe (Some S.NatRepr) -> a
    runTrue (Just (Some bytes1)) (Just (Some bytes2))
          | S.NatEQ <- S.compareNat (bytes1 `S.addNat` bytes2) bytes
          , Just S.LeqProof <- S.testLeq (S.knownNat @1) bytes1
          , Just S.LeqProof <- S.testLeq (S.knownNat @1) bytes2
          = trueCont bytes1 bytes2
    runTrue _ _ = falseCont


-- | Read 'bits' number of bits starting from location i in memory
readMem :: forall arch sym bits.
           (A.Architecture arch, LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym, 1 S.<= bits)
        => S.NatRepr bits
        -- ^ The number of bits to read
        -> S.SymBV sym (A.RegWidth arch)
        -- ^ The address in memory at which to read
        -> MemM sym arch (S.SymBV sym bits)
readMem bits addr = readMemNoOF bits addr

readMemIO :: forall arch sym bits.
             (A.Architecture arch, B.IsSymInterface sym, 1 S.<= bits)
          => sym
          -> S.NatRepr bits
          -- ^ The number of bits to read
          -> S.SymBV sym (A.RegWidth arch)
          -- ^ The address in memory at which to read
          -> S.SymExpr sym (A.MemType arch)
          -- ^ The memory expression from which to read
          -> IO (S.SymBV sym bits)
readMemIO sym bits i mem = withMem @arch sym mem $ readMem bits i


-- | Read 'bits' number of bits starting from location i in memory, possibly overflowing the address space
readMemOF :: forall arch sym bits.
           (A.Architecture arch, LLVM.HasPtrWidth (A.RegWidth arch), B.IsSymInterface sym, 1 S.<= bits)
        => S.NatRepr bits
        -- ^ The number of bits to read
        -> S.SymBV sym (A.RegWidth arch)
        -- ^ The address in memory at which to read
        -> MemM sym arch (S.SymBV sym bits)
readMemOF bits addr = case S.asUnsignedBV addr of
  Nothing  -> readMemNoOF bits addr
  Just off -> S.withDivModNat bits (S.knownNat @8) $
    \bytes m -> -- bytes is the number of bytes corresponding to bits
      case S.isZeroNat m of
        S.NonZeroNat -> error "Cannot read a number of bits that is not a multiple of 8"
        S.ZeroNat ->
            -- bytes+0 = 0+bytes = bytes
            -- 8*bytes = bytes*8
            case S.mulComm bytes (S.knownNat @8) of
              S.Refl -> checkOverflowITE bytes off addrSize (splitReadMem bytes) (readMemNoOF bits addr)

  where
    -- bytes = fromIntegral $ B.bitsToBytes (S.intValue bits)
    toBits :: S.NatRepr bytes -> S.NatRepr (8 S.* bytes)
    toBits b = S.knownNat @8 `S.natMultiply` b
    addrSize :: Integer
    addrSize = 2^(S.intValue (S.knownNat @(A.RegWidth arch)))


    splitReadMem :: forall bytes bytes1 bytes2.
                    ((8 S.* bytes) ~ bits, bytes ~ (bytes1 S.+ bytes2), 1 S.<= bytes1, 1 S.<= bytes2)
                 => S.NatRepr bytes
                 -> S.NatRepr bytes1
                 -> S.NatRepr bytes2
                 -- -> MemM sym arch (S.SymBV sym ((8 S.* bytes1) S.+ (8 S.* bytes2)))
                 -> MemM sym arch (S.SymBV sym bits)
    splitReadMem _ bytes1 bytes2
      | S.LeqProof <- S.leqMulPos (S.knownNat @8) bytes1
      , S.LeqProof <- S.leqMulPos (S.knownNat @8) bytes2 = do
      sym <- T.askSym

      -- first read to the end of the address space
      xs <- readMemNoOF (toBits bytes1) addr
      -- then read from the beginning of the address space
      z <- liftIO $ S.bvLit sym (S.knownNat @(A.RegWidth arch)) 0
      ys <- readMemNoOF (toBits bytes2) z
      xys <- liftIO $ S.bvConcat sym xs ys
      -- 8*bytes1 + 8*bytes2 ~ 8*(bytes1 + bytes2)
      withAddMulDistribLeft (S.knownNat @8) bytes1 bytes2 $
        return xys

withAddMulDistribLeft :: S.NatRepr a
                      -> S.NatRepr b
                      -> S.NatRepr c
                      -> ((a S.* (b S.+ c)) ~ ((a S.* b) S.+ (a S.* c)) => d) -> d
withAddMulDistribLeft a b c d =
  case ( S.mulComm a (b `S.addNat` c)
       , S.mulComm a b
       , S.mulComm a c ) of
    (S.Refl, S.Refl, S.Refl) -> S.withAddMulDistribRight b c a d


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
  sym <- T.askSym
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
  -- liftIO . print $ PP.text "Attempting to write value " <+> S.printSymExpr v
  --              <+> PP.text "to pointer " <+> LLVM.ppPtr ptr
  align <- askAlignment

  sType <- bvType (S.bvWidth v)

  v' <- symBVToLLVMVal v

  sym <- T.askSym
  mem' <- liftIO $ LLVM.storeRaw sym mem ptr sType align v'

  -- liftIO . print $ PP.text "Successfully wrote value"
  putImpl mem'


-- | For each @WriteData@ that occurs in the list of 'A.AccessData', write that
-- value to the underlying 'LLVM.MemImpl'.
doWrites :: (A.Architecture arch, B.IsSymInterface sym, LLVM.HasPtrWidth (A.RegWidth arch))
              => [A.AccessData sym arch]
              -> MemM sym arch ()
doWrites [] = return ()
doWrites (A.ReadData _ : ls) = doWrites ls
doWrites (A.WriteData i v : ls) = do
  writeMem i v
  doWrites ls


--------------------------------------


-- | Interpret a symbolic array as a sequence of operations on the memory.
--
-- We assume that the expression is an array built up from call to
-- 'arrayUpdate' and 'write_mem'. If the memory is constructed in a different
-- way, that array will just be written directly to the memory model, not taking
-- recursive structure into account.
--
-- TODO: add support for 'arrayIte' and 'arrayFromMap'?
instantiateMemOps :: forall arch sym t st fs.
                       (sym ~ WE.ExprBuilder t st fs, B.IsSymInterface sym
                       , A.Architecture arch
                       )
                    => S.SymExpr sym (A.MemType arch)
                    -> MemM sym arch ()
instantiateMemOps mem
  | Just (mem', A.WriteData idx val) <- isUpdateArray @arch mem =
  LLVM.withPtrWidth (S.knownNat @(A.RegWidth arch)) $ do
    instantiateMemOps mem'
    writeMem idx val
instantiateMemOps mem
  | Just (mem', A.WriteData idx val) <- MA.isWriteMem @arch mem = do
  LLVM.withPtrWidth (S.knownNat @(A.RegWidth arch)) $ do
    instantiateMemOps mem'
    writeMem idx val
instantiateMemOps _ | otherwise = return ()


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
