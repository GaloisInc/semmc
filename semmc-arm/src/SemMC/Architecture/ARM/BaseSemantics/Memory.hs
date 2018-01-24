{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module SemMC.Architecture.ARM.BaseSemantics.Memory
    ( manualMemory
    )
    where

import Data.Monoid
import Data.Parameterized.Some ( Some(..) )
import Prelude hiding ( concat )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
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
  return ()


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

