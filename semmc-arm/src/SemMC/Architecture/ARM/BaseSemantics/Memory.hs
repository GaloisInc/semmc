{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module SemMC.Architecture.ARM.BaseSemantics.Memory
    ( manualMemory
    )
    where

import Data.Monoid
import Data.Parameterized.Some ( Some(..) )
import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


manualMemory :: SemM 'Top ()
manualMemory = do
  defineLoads
  defineStores

defineLoads :: SemM 'Top ()
defineLoads = do
  return ()

defineStores :: SemM 'Top ()
defineStores = do
  defineLinearOpcode "STR_PRE_IMM" $ do
    comment "Store Register, Pre-indexed (P=1, W=1), immediate  (A32)"
    comment "doc: F7.1.217, page F7-2880"
    comment "see also PUSH, F7.1.138, page F7-2760" -- TBD: if add && rN=SP && imm.imm=4 [A1 v.s. A2 form]"
    ok <- condPassed
    -- TBD: EncodingSpecificOperations()
    imm12 <- param "imm" addrmode_imm12_pre EMemRef
    rT <- param "gpr" gpr naturalBV
    input rT
    input imm12
    input memory
    let rN = imm12Reg imm12
        imm12arg = [Some $ Loc imm12]
        imm = imm12Imm imm12arg
        add = imm12Add imm12arg
        addr = ite add (bvadd (Loc rN) imm) (bvsub (Loc rN) imm)
        nBytes = 4
    -- TBD: next use PCStoreValue() of rT == r15 instead of (Loc rT)
    defMemWhen ok memory addr nBytes (Loc rT)
    -- TBD: if rN == r15 or rN == rT then UNPREDICTABLE
    defRegWhen ok rN addr


-- ----------------------------------------------------------------------



memory :: Location 'TMemory
memory = LiteralLoc Literal { lName = "Mem"
                            , lExprType = EMemory
                            }

-- | Read from the pseudo-location "Memory"
readMem :: Expr 'TMemory -- ^ The memory
        -> Expr 'TBV -- ^ The effective address to load
        -> Int -- ^ The number of bytes
        -> Expr 'TBV
readMem mem ea nBytes =
  uf (EBV (8 * nBytes)) funcName [Some mem, Some ea]
  where
    funcName :: String
    funcName = "read_mem." <> show (nBytes * 8)

-- | Define a write to memory; it takes a memory and returns a whole new memory.
storeMem :: Expr 'TMemory -- ^ The memory
         -> Expr 'TBV -- ^ The effective address to store at
         -> Int -- ^ The number of bytes to store
         -> Expr 'TBV -- ^ The bitvector value to store (size is checked)
         -> Expr 'TMemory
storeMem mem ea nBytes val
  | EBV w <- exprType val
  , w == nBytes * 8 =
    uf EMemory funcName [Some mem, Some ea, Some val]
  | otherwise = error ("Invalid byte count to store value " ++ show val)
  where
    funcName = "write_mem." <> show (nBytes * 8)


-- | Performs an assignment for a conditional Opcode when the target
-- is a memory location.
defMemWhen :: Expr 'TBool -- ^ is store enabled? (result of condPassed check)
           -> Location 'TMemory -- ^ The memory target
           -> Expr 'TBV -- ^ The effective address to store at
           -> Int -- ^ The number of bytes to store
           -> Expr 'TBV -- ^ The bitvector value to store (size is checked)
           -> SemM 'Def ()
defMemWhen isOK memloc addr nBytes expr =
    let origval = readMem (Loc memloc) addr nBytes
        updval = ite isOK expr origval
    in defLoc memloc (storeMem (Loc memloc) addr nBytes updval)

