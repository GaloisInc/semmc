{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module SemMC.Architecture.ARM.BaseSemantics.Memory
    ( manualMemory
    )
    where

import           Data.Maybe
import           Data.Parameterized.Context
import           Data.Parameterized.Some ( Some(..) )
import           Data.Semigroup
import qualified Dismantle.ARM as A
import qualified Dismantle.Thumb as T
import           Prelude hiding ( concat, pred )
import           SemMC.Architecture.ARM.BaseSemantics.Base
import           SemMC.Architecture.ARM.BaseSemantics.Helpers
import           SemMC.Architecture.ARM.BaseSemantics.Natural
import           SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Arithmetic
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.Registers
import           SemMC.Architecture.ARM.BaseSemantics.Pseudocode.ShiftRotate
import           SemMC.Architecture.ARM.BaseSemantics.Registers
import           SemMC.DSL


manualMemory :: SemARM 'Top ()
manualMemory = do
  defineLoads
  defineStores

defineLoads :: SemARM 'Top ()
defineLoads = do
  defineA32Opcode A.LDR_POST_IMM (Empty
                                 :> ParamDef "gpr" gpr naturalBV
                                 :> ParamDef "predBits" pred (EBV 4)
                                 :> ParamDef "imm" am2offset_imm (EPackedOperand "Am2Offset_Imm")
                                 :> ParamDef "off" addr_offset_none naturalBV
                                 )
                      $ \rT _ imm12 off -> do
    comment "Load Register, Post-indexed (P=0, W=0), immediate (A32), Encoding A1"
    comment "doc: F7.1.69, page F7-2636"
    input imm12
    input off
    let add = am2offset_immAdd imm12
        offset = zext $ am2offset_immImm imm12
        rN = off
        b'P = LitBV 1 0
        b'W = LitBV 1 0
        index = bveq b'P (LitBV 1 1)
        wback = orp (bveq b'P (LitBV 1 0)) (bveq b'W (LitBV 1 1))
    ldri rT add rN offset index wback

  defineA32Opcode A.LDRi12 (Empty
                           :> ParamDef "gpr" gpr naturalBV
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "imm12" addrmode_imm12 (EPackedOperand "Imm12")
                           )
                      $ \rT _ imm12 -> do
    comment "Load Register, offset addressing (P=1, W=0, U=1), immediate (A32), Encoding A1"
    comment "doc: F7.1.69, page F7-2636"
    input imm12
    let rN = imm12Reg imm12
        offset = zext $ imm12Off imm12
        add = imm12Add imm12
        b'P = LitBV 1 1
        b'W = LitBV 1 0
        index = bveq b'P (LitBV 1 1)
        wback = orp (bveq b'P (LitBV 1 0)) (bveq b'W (LitBV 1 1))
    ldri rT add rN offset index wback

  defineT32Opcode T.TLDRi (Empty
                          :> ParamDef "gpr" tgpr naturalBV
                          :> ParamDef "addris" t_addrmode_is4 (EPackedOperand "T_AddrMode_IS4")
                          )
                      $ \rT addris4 -> do
    comment "Load Register immediate, Encoding T1 (F7.1.68, F7-2634)"
    input addris4
    let rN = addrmode_is4_reg addris4
        imm5 = addrmode_is4_imm addris4
        imm32 = zext $ concat imm5 (LitBV 2 0b00)
        index = LitBool True
        add = LitBool True
        wback = LitBool False
    ldri rT add rN imm32 index wback

  defineT32Opcode T.TLDRpci (Empty
                            :> ParamDef "gpr" tgpr naturalBV
                            :> ParamDef "addrpc" t_addrmode_pc (EPackedOperand "T_AddrMode_PC")
                            )
                      $ \rT addrpc -> do
    comment "Load Register literal, Encoding T1 (F7.1.70, F7-2638)"
    input addrpc
    let imm8 = t_addrmode_pc_val addrpc
        imm32 = zext $ concat imm8 (LitBV 2 0b00)
        add = LitBool True
        useimm32 = LitBool True
        isUnpredictable = LitBool False
        base = align (Loc pc) 4
    _ <- ldr rT add base imm32 useimm32 isUnpredictable
    return ()

  defineT32Opcode T.TPOP (Empty
                         :> ParamDef "registers" reglist (EPackedOperand "Reglist")
                         )
                      $ \regsarg -> do
    comment "Pop registers, Encoding T1 (F7.1.136, F7-2756)"
    input regsarg
    let rlist = register_list regsarg
        allowUnaligned = False
        isUnpred = "isUnpredictable" =:
                   (orp (bveq rlist (naturalLitBV 0))
                        (andp (bveq (extract 15 15 rlist) (LitBV 1 1))
                              (andp inITBlock (notp lastInITBlock))))
    popregs rlist allowUnaligned isUnpred


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
                                :> ParamDef "imm" addrmode_imm12_pre (EPackedOperand "Imm12")
                                :> ParamDef "gpr" gpr naturalBV
                                )
               $ \_ imm12 rT -> do
    comment "Store Register, Pre-indexed (P=1, W=1), immediate  (A32)"
    comment "doc: F7.1.217, page F7-2880"
    comment "see also PUSH, F7.1.138, page F7-2760" -- TBD: if add && rN=SP && imm.imm=4 [A1 v.s. A2 form]"
    input imm12
    let rN = imm12Reg imm12
        off = zext $ imm12Off imm12
        add = imm12Add imm12
    streg rT add off rN (LitBV 1 1) (LitBV 1 1)
  defineA32Opcode A.STR_POST_IMM (Empty
                                 :> ParamDef "predBits" pred (EBV 4)
                                 :> ParamDef "imm" am2offset_imm (EPackedOperand "Am2Offset_Imm")
                                 :> ParamDef "off" addr_offset_none naturalBV
                                 :> ParamDef "gpr" gpr naturalBV
                                 )
    $ \_ imm12 off rT -> do
    comment "Store Register, Post-indexed (P=0, W=1), immediate  (A32)"
    comment "doc: F7.1.217, page F7-2880"
    input imm12
    input off
    let add = am2offset_immAdd imm12
        offset = zext $ am2offset_immImm imm12
        rN = off
    streg rT add offset rN (LitBV 1 0) (LitBV 1 1)
  defineA32Opcode A.STRi12 (Empty
                           :> ParamDef "predBits" pred (EBV 4)
                           :> ParamDef "imm12" addrmode_imm12 (EPackedOperand "Imm12")
                           :> ParamDef "gpr" gpr naturalBV
                           )
                      $ \_ imm12 rT -> do
    comment "Store Register, offset addressing (P=1, W=0, U=1), immediate (A32), Encoding A1"
    comment "doc: F7.1.217, page F7-2880"
    input imm12
    let rN = imm12Reg imm12
        offset = zext $ imm12Off imm12
        add = imm12Add imm12
        b'P = LitBV 1 1
        b'W = LitBV 1 0
    streg rT add offset rN b'P b'W

  defineA32Opcode A.STR_PRE_REG (Empty
                                :> ParamDef "predBits" pred (EBV 4)
                                :> ParamDef "ldst_so_reg" ldst_so_reg (EPackedOperand "LdstSoReg")
                                :> ParamDef "gpr" gpr naturalBV
                                )
    $ \_ ldstSoReg rT -> do
    comment "Store Register, Pre-indexed (P=1, W=1), register (A32)"
    comment "doc: F7.1.218, page F7-2882"
    input rT
    input ldstSoReg
    input memory
    let add  = ldst_so_regAdd ldstSoReg
        rN   = ldst_so_regBaseRegister ldstSoReg
        rM   = ldst_so_regOffsetRegister ldstSoReg
        imm5 = ldst_so_regImmediate ldstSoReg
        st   = ldst_so_regShiftType ldstSoReg

        (_,_,c,_) = getNZCV

        (shift_t, shift_n) = splitImmShift (decodeImmShift st imm5)
        offset = shift (Loc rM) shift_t shift_n c

        nBytes = 4
        addr = ite add (bvadd (Loc rN) offset) (bvsub (Loc rN) offset)
    defMem memory addr nBytes (ite (isR15 rT) (Loc pc) (Loc rT))

  defineT32Opcode T.TSTRi (Empty
                          :> ParamDef "addris" t_addrmode_is4 (EPackedOperand "T_AddrMode_IS4")
                          :> ParamDef "gpr" tgpr naturalBV
                          )
                      $ \addris4 rT -> do
    comment "Store Register immediate, Encoding T1"
    comment "doc: F7.1.216, page F7-2878"
    input addris4
    let rN = addrmode_is4_reg addris4
        imm5 = addrmode_is4_imm addris4
        imm32 = zext $ concat imm5 (LitBV 2 0b00)
        index = LitBool True
        add = LitBool True
        wback = LitBool False
    stri rT add imm32 rN index wback (LitBool False)

  defineT32Opcode T.TSTRHi (Empty
                          :> ParamDef "addris" t_addrmode_is2 (EPackedOperand "T_AddrMode_IS2")
                          :> ParamDef "gpr" tgpr naturalBV
                          )
                      $ \addris2 rT -> do
    comment "Store Register HalfWord immediate, Encoding T1"
    comment "doc: F7.1.229, page F7-2904"
    input addris2
    let rN = addrmode_is2_reg addris2
        imm5 = addrmode_is2_imm addris2
        imm32 = zext $ concat imm5 (LitBV 1 0b0)
        index = LitBool True
        add = LitBool True
        wback = LitBool False
    -- n.b. stri has checks for if rT is 15; rT encoding here is a
    -- TGPR (0-7) and can never be 15.
    stri rT add imm32 rN index wback (LitBool False)

  defineT32Opcode T.TPUSH (Empty
                          :> ParamDef "registers" reglist (EPackedOperand "Reglist")
                          )
                  $ \regsarg -> do
    comment "Push register list, Encoding T1 (F7.1.137, F7-2760)"
    input regsarg
    let rlist = register_list regsarg
        allowUnaligned = False
        isUnpred = bveq rlist (naturalLitBV 0)
    pushregs rlist allowUnaligned isUnpred


-- ----------------------------------------------------------------------

memory :: Location 'TMemory
memory = LiteralLoc Literal { lName = "Mem"
                            , lExprType = EMemory
                            }

memory' :: Integer -> Location 'TMemory
memory' = MemoryLoc
              -- Literal { lName = "Mem:" <> show off
              --                    , lExprType = EMemory
              --                    }

-- n.b. The ARM documentation describes "MemA" v.s. "MemU" read and
-- write operations, used for accessing aligned v.s. unaligned
-- addresses, respectively.  The discussion of MemA/MemU is located on
-- (G.2, AppxG-4956) and essentially indicates that MemA accesses are
-- atomic and MemU accesses are "Normal" (i.e. non-atomic).  At
-- present, this distinction is not modelled in the instruction
-- semantics themselves.

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


ldri :: Location 'TBV -> Expr 'TBool -> Location 'TBV
    -> Expr 'TBV -> Expr 'TBool -> Expr 'TBool
    -> SemARM 'Def ()
ldri rT add rN imm32 index wback = do
  let sameRegs = sameLocation rT rN
      isUnpredictable = "isUnpredictable" =: andp sameRegs wback
  updated_addr <- ldr rT add (Loc rN) imm32 index isUnpredictable
  defReg rN (ite wback updated_addr (Loc rN))

ldr :: Location 'TBV -> Expr 'TBool -> Expr 'TBV
    -> Expr 'TBV -> Expr 'TBool -> Expr 'TBool
    -> SemARM 'Def (Expr 'TBV)
ldr rT add baseAddr imm32 index isUnpredictable = do
  input memory
  let updated_addr = "updAddr" =: ite add (bvadd baseAddr imm32) (bvsub baseAddr imm32)
      nBytes = 4
      addr = ite index updated_addr baseAddr
      result = readMem (Loc memory) addr nBytes
      alignedResult = ite (orp (tstBit 1 addr) (tstBit 0 addr)) (unpredictable result) result
  loadWritePC (isR15 rT) alignedResult
  defReg rT (ite (isR15 rT)
                 (Loc rT)
                 (ite isUnpredictable (unpredictable result) result))
  return updated_addr

streg :: Location 'TBV
      -> Expr 'TBool -> Expr 'TBV -> Location 'TBV
      -> Expr 'TBV -> Expr 'TBV
    -> SemARM 'Def ()
streg rT add offset rN pbit wbit = do
  let index = bveq pbit (LitBV 1 1)
      wback = "wback" =: orp (bveq pbit (LitBV 1 0)) (bveq wbit (LitBV 1 1))
      isUnpredictable = "isUnpredictable" =: (andp wback (orp (isR15 rN) (sameLocation rN rT)))
  stri rT add offset rN index wback isUnpredictable

stri :: Location 'TBV
      -> Expr 'TBool -> Expr 'TBV -> Location 'TBV
      -> Expr 'TBool -> Expr 'TBool -> Expr 'TBool
    -> SemARM 'Def ()
stri rT add offset rN index wback isUnpredictable = do
  input memory
  input rT
  let offAddr = "offAddr" =: ite add (bvadd (Loc rN) offset) (bvsub (Loc rN) offset)
      addr = "addr" =: ite index offAddr (Loc rN)
      nBytes = 4
      newMem = "wval" =: ite (isR15 rT) (Loc pc) (Loc rT)
      newRn = "rnUpd" =: ite wback offAddr (Loc rN)
  defMem memory addr nBytes (ite isUnpredictable (unpredictable newMem) newMem)
  defReg rN (ite isUnpredictable (unpredictable newRn) newRn)


-- | Implements the body of the POP opcode, defined (F7.1.136, F7-2757).
popregs :: Expr 'TBV -- ^ register list as 32-bit bitmask
         -> Bool -- ^ true if unaligned accesses allowed
         -> Expr 'TBool -- ^ true if unpredictable
         -> SemARM 'Def ()
popregs rlist allowUnaligned isUnpred =
    do input memory
       let nRegs = 15 :: Integer
           reg n = case n of
                     15 -> pc
                     _ -> LiteralLoc Literal { lName = "R" <> show n, lExprType = naturalBV }
       mapM_ (input . reg) $ filter ((/=) 15) [0..nRegs]  -- PC as input is declared by defineXOpcode

       let address = "newSP" =: bvadd (Loc sp) (bvmul (naturalLitBV 4) (bvpopcnt $ zext rlist))
           newSP = ite (isActive 13) (unpredictable address) address
           nBytes = 4
           stkOffAddr :: Expr 'TBV -> Expr 'TBV
           stkOffAddr stkoff = bvadd (Loc sp) (bvmul (naturalLitBV 4) stkoff)
           readAtOffset si = readMem (Loc $ memory' si) (stkOffAddr $ stkRegOff si) nBytes

           isActive rn = "isActive_R" <> show rn =: bvne (LitBV 16 0) (bvand (rMask rn) rlist)
           rMask rn = LitBV 16 (2^rn)
           cntReg rn = "cntReg_uptoR" <> show rn =: (bvpopcnt $ zext $ extract (fromInteger rn) 0 rlist)
           stkRegOff :: Integer -> Expr 'TBV
           stkRegOff rn = "stkOff_R" <> show rn =: (bvsub (cntReg rn) (naturalLitBV 1))

           setRegFromStack :: Integer -> SemARM 'Def ()
           setRegFromStack rn = let tgt = reg rn
                                    stkval = stackVal rn (Loc tgt)
                                in defReg tgt (ite isUnpred (unpredictable stkval) stkval)

           stackVal :: Integer -> Expr 'TBV -> Expr 'TBV
           stackVal rn origval = ("stkVal_R" <> show rn) =:
                                 ite (isActive rn) (readAtOffset rn) origval

           pcVal = let pcStkVal = "pcStkVal" =: readAtOffset 15
                       pcStkAddr = "pcStkAddr" =: (stkOffAddr $ stkRegOff 15)
                   in if allowUnaligned
                      then (ite (bveq (LitBV 2 0b00) (extract 1 0 pcStkAddr)) pcStkVal (unpredictable pcStkVal))
                      else pcStkVal

           notSPorPC n = not $ n `elem` [13, 15]

       mapM_ setRegFromStack $ filter notSPorPC [0..(nRegs-1)]  -- SP and PC below:
       loadWritePC (isActive 15) pcVal
       defLoc sp (ite isUnpred (unpredictable newSP) newSP)


-- | Implements the body of the PUSH opcode, defined (F7.1.138, F7-2761).
pushregs :: Expr 'TBV -- ^ register list as 32-bit bitmask
         -> Bool -- ^ true if unaligned accesses allowed
         -> Expr 'TBool -- ^ true if unpredictable
         -> SemARM 'Def ()
pushregs rlist _allowUnaligned isUnpred =
    do input memory
       let nRegs = 15 :: Integer
           reg n = case n of
                     15 -> pc
                     _ -> LiteralLoc Literal { lName = "R" <> show n, lExprType = naturalBV }
       mapM_ (input . reg) $ filter ((/=) 15) [0..nRegs]  -- PC as input is declared by defineXOpcode
       let regCnt = "regCnt" =: (extract 15 0 $ bvpopcnt $ zext rlist)
           address = "newSP" =: bvsub (Loc sp) (bvmul (naturalLitBV 4) (bvpopcnt $ zext rlist))
           nBytes = 4

           stackWrite :: Integer -> SemARM 'Def ()
           stackWrite si = let sAddr = stackPushAddr si
                               origval = readMem (Loc memory) sAddr nBytes
                           in defMem (memory' si) sAddr nBytes (stackVal si origval)
           stackPushAddr si = "stack_" <> show si =: bvsub (Loc sp) (naturalLitBV (4 * (1+si)))
           -- The `stackVal si` is the set of possible values that
           -- could be written to this stack location (offset from the
           -- original SP), based on the bitmask specified at runtime.
           -- For example, 'SP - (4 * (15+1))' can only hold R0, and
           -- only if the reglist bitmask has all of bits 0..15 set,
           -- whereas 'SP-4' could be written with any of R0..R15,
           -- whatever happens to be the highest bit set in the
           -- reglist bitmask.
           stackVal :: Integer -> Expr 'TBV -> Expr 'TBV
           stackVal si origval = ("pshval_SPminus" <> show si) =:
                                 let maxRegCandidate = (nRegs - si)
                                     target = bvsub regCnt (LitBV 16 si)
                                     regRange = [0 .. maxRegCandidate]
                                     regMask n = "regMask_SPminus" <> show si <> "_R" <> show n =:
                                                 (extract 15 0 $ bvpopcnt (zext $ extract (fromInteger n) 0 rlist))
                                     activeReg rn = "isActive_SPminus" <> show si <> "_R" <> show rn =:
                                                    andp (bvugt regCnt (LitBV 16 si)) (bveq (regMask rn) target)
                                     regVal n = case n of
                                                  15 -> pcStoreValue
                                                  13 -> ite (bveq rlist (LitBV 16 ((2^n) - 1)))
                                                            (Loc $ reg n)
                                                            (unpredictable $ Loc $ reg n)
                                                  _ -> Loc $ reg n
                                     tryReg rnum els = ite (activeReg rnum) (regVal rnum) els
                                 in foldr tryReg origval regRange
       mapM_ stackWrite [0..nRegs]
       defLoc sp (ite isUnpred (unpredictable address) address)
