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
import SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
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
  -- Note about STR_PRE_IMM vs STR_POST_IMM:
  -- for STR_PRE_IMM, the addrmode_imm12_pre bundle is holding three pieces of
  -- information: the register holding the target address, the immediate offset, and
  -- the add bit. for STR_POST_IMM, the am2offset_imm argument plays a similar role,
  -- except that one only holds the last two. The GPR holding the target address
  -- appears as the addr_offset_none argument. This is just a dumb quirk about the
  -- TableGen data -- the data is packaged differently for the two instruction
  -- variants in a way that doesn't really make much sense. But there's a lot about
  -- the tablegen data that doesn't really make much sense.
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
        sameRegs = sameLocation rT rN
        imm12arg = [Some $ Loc imm12]
        nBytes = 4
        off = zext $ imm12Off imm12arg
        add = imm12Add imm12arg
        addr = "addr" =: ite add (bvadd (Loc rN) off) (bvsub (Loc rN) off)
    defMem memory addr nBytes (ite (isR15 rT) (Loc pc) (Loc rT))
    defReg rN (ite (orp (isR15 rN) sameRegs) (unpredictable addr) addr)
  defineA32Opcode A.STR_POST_IMM (Empty
                                 :> ParamDef "predBits" pred (EBV 4)
                                 :> ParamDef "imm" am2offset_imm EMemRef
                                 :> ParamDef "off" addr_offset_none naturalBV
                                 :> ParamDef "gpr" gpr naturalBV
                                 )
    $ \_ imm12 off rT -> do
    comment "Store Register, Post-indexed (P=0, W=1), immediate  (A32)"
    comment "doc: F7.1.217, page F7-2880"
    input rT
    input imm12
    input off
    input memory
    let imm12arg = [Some $ Loc imm12]
        add = am2offset_immAdd imm12arg
        offset = zext $ am2offset_immImm imm12arg
        rN = off
        updated_addr = ite add (bvadd (Loc rN) offset) (bvsub (Loc rN) offset)
        nBytes = 4
        sameRegs = sameLocation rT rN
    defMem memory (Loc rN) nBytes (ite (isR15 rT) (Loc pc) (Loc rT))
    defReg rN (ite (orp (isR15 rN) sameRegs) (unpredictable updated_addr) updated_addr)
  defineA32Opcode A.STR_PRE_REG (Empty
                                :> ParamDef "predBits" pred (EBV 4)
                                :> ParamDef "ldst_so_reg" ldst_so_reg EMemRef -- ???
                                :> ParamDef "grp" gpr naturalBV
                                )
    $ \_ ldstSoReg rT -> do
    comment "Store Register, Pre-indexed (P=1, W=1), register (A32)"
    comment "doc: F7.1.218, page F7-2882"
    input rT
    input ldstSoReg
    input memory
    let ldstSoRegArg = [Some $ Loc ldstSoReg]

        add  = ldst_so_regAdd ldstSoRegArg
        rN   = ldst_so_regBaseRegister ldstSoReg
        rM   = ldst_so_regOffsetRegister ldstSoReg
        imm5 = ldst_so_regImmediate ldstSoRegArg
        st   = ldst_so_regShiftType ldstSoRegArg

        (_,_,c,_) = getNZCV

        (shift_t, shift_n) = splitImmShift (decodeImmShift st imm5)
        offset = shift (Loc rM) shift_t shift_n c

        nBytes = 4
        addr = ite add (bvadd (Loc rN) offset) (bvsub (Loc rN) offset)
    defMem memory addr nBytes (ite (isR15 rT) (Loc pc) (Loc rT))

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


