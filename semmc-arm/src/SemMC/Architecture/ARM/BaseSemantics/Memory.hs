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
import SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Registers
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
  defineA32Opcode A.LDR_POST_IMM (Empty
                                 :> ParamDef "gpr" gpr naturalBV
                                 :> ParamDef "predBits" pred (EBV 4)
                                 :> ParamDef "imm" am2offset_imm EMemRef
                                 :> ParamDef "off" addr_offset_none naturalBV
                                 )
                      $ \rT _ imm12 off -> do
    comment "Load Register, Post-indexed (P=0, W=0), immediate (A32), Encoding A1"
    comment "doc: F7.1.69, page F7-2636"
    input imm12
    input off
    let imm12arg = [Some $ Loc imm12]
        add = am2offset_immAdd imm12arg
        offset = zext $ am2offset_immImm imm12arg
        rN = off
        b'P = LitBV 1 0
        b'W = LitBV 1 0
        index =bveq b'P (LitBV 1 1)
        wback = orp (bveq b'P (LitBV 1 0)) (bveq b'W (LitBV 1 1))
    ldr rT add rN offset index wback

  defineA32Opcode A.LDRi12 (Empty
                           :> ParamDef "gpr" gpr naturalBV
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "imm12" addrmode_imm12 EMemRef
                           )
                      $ \rT _ imm12 -> do
    comment "Load Register, offset addressing (P=1, W=0, U=1), immediate (A32), Encoding A1"
    comment "doc: F7.1.69, page F7-2636"
    input imm12
    let rN = imm12Reg imm12
        offset = zext $ imm12Off $ [Some $ Loc imm12]
        add = imm12Add $ [Some $ Loc imm12]
        b'P = LitBV 1 1
        b'W = LitBV 1 0
        index =bveq b'P (LitBV 1 1)
        wback = orp (bveq b'P (LitBV 1 0)) (bveq b'W (LitBV 1 1))
    ldr rT add rN offset index wback

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
    input imm12
    let rN = imm12Reg imm12
        imm12arg = [Some $ Loc imm12]
        off = zext $ imm12Off imm12arg
        add = imm12Add imm12arg
    streg rT add off rN (LitBV 1 1) (LitBV 1 1)
  defineA32Opcode A.STR_POST_IMM (Empty
                                 :> ParamDef "predBits" pred (EBV 4)
                                 :> ParamDef "imm" am2offset_imm EMemRef
                                 :> ParamDef "off" addr_offset_none naturalBV
                                 :> ParamDef "gpr" gpr naturalBV
                                 )
    $ \_ imm12 off rT -> do
    comment "Store Register, Post-indexed (P=0, W=1), immediate  (A32)"
    comment "doc: F7.1.217, page F7-2880"
    input imm12
    input off
    let imm12arg = [Some $ Loc imm12]
        add = am2offset_immAdd imm12arg
        offset = zext $ am2offset_immImm imm12arg
        rN = off
    streg rT add offset rN (LitBV 1 0) (LitBV 1 1)
  defineA32Opcode A.STRi12 (Empty
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "imm12" addrmode_imm12 EMemRef
                           :> ParamDef "gpr" gpr naturalBV
                           )
                      $ \_ imm12 rT -> do
    comment "Store Register, offset addressing (P=1, W=0, U=1), immediate (A32), Encoding A1"
    comment "doc: F7.1.217, page F7-2880"
    input imm12
    let rN = imm12Reg imm12
        offset = zext $ imm12Off $ [Some $ Loc imm12]
        add = imm12Add $ [Some $ Loc imm12]
        b'P = LitBV 1 1
        b'W = LitBV 1 0
    streg rT add offset rN b'P b'W

  defineA32Opcode A.STR_PRE_REG (Empty
                                :> ParamDef "predBits" pred (EBV 4)
                                :> ParamDef "ldst_so_reg" ldst_so_reg EMemRef -- ???
                                :> ParamDef "gpr" gpr naturalBV
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


ldr :: Location 'TBV -> Expr 'TBool -> Location 'TBV
    -> Expr 'TBV -> Expr 'TBool -> Expr 'TBool
    -> SemARM 'Def ()
ldr rT add rN imm32 index wback = do
  input memory
  let updated_addr = "updAddr" =: ite add (bvadd (Loc rN) imm32) (bvsub (Loc rN) imm32)
      nBytes = 4
      addr = ite index updated_addr (Loc rN)
      result = readMem (Loc memory) addr nBytes
      alignedResult = ite (orp (tstBit 1 addr) (tstBit 0 addr)) (unpredictable result) result
      sameRegs = sameLocation rT rN
      isUnpredictable = "isUnpredictable" =: andp sameRegs wback
  loadWritePC (isR15 rT) alignedResult
  defReg rN (ite wback updated_addr (Loc rN))
  defReg rT (ite (isR15 rT)
                 (Loc rT)
                 (ite isUnpredictable (unpredictable result) result))

streg :: Location 'TBV
      -> Expr 'TBool -> Expr 'TBV -> Location 'TBV
      -> Expr 'TBV -> Expr 'TBV
    -> SemARM 'Def ()
streg rT add offset rN pbit wbit = do
  input memory
  input rT
  let index = bveq pbit (LitBV 1 1)
      wback = "wback" =: orp (bveq pbit (LitBV 1 0)) (bveq wbit (LitBV 1 1))
      offAddr = "offAddr" =: ite add (bvadd (Loc rN) offset) (bvsub (Loc rN) offset)
      addr = "addr" =: ite index offAddr (Loc rN)
      isUnpredictable = "isUnpredictable" =: (andp wback (orp (isR15 rN) (sameLocation rN rT)))
      nBytes = 4
      newMem = "wval" =: ite (isR15 rT) (Loc pc) (Loc rT)
      newRn = "rnUpd" =: ite wback offAddr (Loc rN)
  defMem memory addr nBytes (ite isUnpredictable (unpredictable newMem) newMem)
  defReg rN (ite isUnpredictable (unpredictable newRn) newRn)
