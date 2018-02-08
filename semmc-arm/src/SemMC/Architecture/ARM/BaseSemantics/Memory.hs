{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module SemMC.Architecture.ARM.BaseSemantics.Memory
    ( manualMemory
    )
    where

import Data.Maybe
import Data.Parameterized.Context
import Data.Parameterized.Some ( Some(..) )
import Data.Semigroup
import Prelude hiding ( concat, pred )
import qualified Dismantle.ARM as A
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Helpers
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


manualMemory :: SemARM 'Top ()
manualMemory = do
  defineLoads
  defineStores

defineLoads :: SemARM 'Top ()
defineLoads = do
  return ()

defineStores :: SemARM 'Top ()
defineStores = do
  defineA32Opcode A.STR_PRE_IMM (Empty
                                :> ParamDef "predBits" pred (EBV 4)
                                :> ParamDef "imm" addrmode_imm12_pre EMemRef
                                :> ParamDef "gpr" gpr naturalBV
                                )
               $ \_ imm12 rT -> do
    comment "Store Register, Pre-indexed (P=1, W=1), immediate  (A32)"
    comment "doc: F7.1.217, page F7-2880"
    comment "see also PUSH, F7.1.138, page F7-2760" -- TBD: if add && rN=SP && imm.imm=4 [A1 v.s. A2 form]"
    input rT
    input imm12
    input memory
    let rN = imm12Reg imm12
        imm12arg = [Some $ Loc imm12]
        off = zext $ imm12Off imm12arg
        add = imm12Add imm12arg
        addr = ite add (bvadd (Loc rN) off) (bvsub (Loc rN) off)
        nBytes = 4
        sameRegs = sameLocation rT rN
    defMem memory addr nBytes (ite (isR15 rT) (Loc pc) (Loc rT))
    defReg rN (ite (orp (isR15 rN) sameRegs) (unpredictable addr) addr)


-- ----------------------------------------------------------------------



memory :: Location 'TMemory
memory = LiteralLoc Literal { lName = "Mem"
                            , lExprType = EMemory
                            }


-- | Base operation to semantically read from the pseudo-location
-- "Memory"
readMem :: Expr 'TMemory -- ^ The memory
        -> Expr 'TBV -- ^ The effective address to load
        -> Int -- ^ The number of bytes
        -> Expr 'TBV
readMem mem ea nBytes =
  uf (EBV (8 * nBytes)) funcName [Some mem, Some ea]
  where
    funcName :: String
    funcName = "read_mem." <> show (nBytes * 8)


-- | Base operation to semantically write to memory; it takes a memory
-- and returns a whole new memory.
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
defMem :: Location 'TMemory -- ^ The memory target
       -> Expr 'TBV -- ^ The effective address to store at
       -> Int -- ^ The number of bytes to store
       -> Expr 'TBV -- ^ The bitvector value to store (size is checked)
       -> SemARM 'Def ()
defMem memloc addr nBytes expr = do
    isOK <- (condPassed . fromJust) <$> getArchData
    let origval = readMem (Loc memloc) addr nBytes
        updval = ite isOK expr origval
    defLoc memloc (storeMem (Loc memloc) addr nBytes updval)


