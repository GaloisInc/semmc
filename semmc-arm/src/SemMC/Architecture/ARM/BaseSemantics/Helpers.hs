{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

module SemMC.Architecture.ARM.BaseSemantics.Helpers
    ( -- * Opcode definition assistance
      defineA32Opcode
    , defineA32OpcodeNoPred
    , defineA32Branch
    , defineT32Opcode
    , defineT32Branch
    -- * Arch subtype (Instruction Set) handling
    , instrSetState
    , selectInstrSet
    , selectInstrSet'
    -- CPSR (APSR) management
    , cpsrNZCV
    -- * PC management
    , updatePC
    , aluWritePC
    , loadWritePC
    , branchWritePC
    , bxWritePC
    -- * Result definition via instruction predicate control
    , defReg
    -- * Manipulation of bit values (BV)
    , zext, zext'
    , sext, sext'
    , bvset, bvclr, tstBit
    -- * Opcode unpacking
    , imm12Reg, imm12Imm, imm12Add
    , blxtgt_S, blxtgt_imm10H, blxtgt_imm10L, blxtgt_J1, blxtgt_J2
      -- * Miscellaneous common functionality
    , unpredictable
    , constrainUnpredictable
    , sameLocation
    , isR15
    )
    where

import Control.Monad ( unless )
import Data.Bits
import Data.Maybe
import Data.Parameterized.Classes
import Data.Parameterized.Some ( Some(..) )
import Data.Semigroup
import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat, pred )
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


-- | A wrapper around 'defineOpcode' that sets A32 mode and updates
-- the PC after the instruction executes (by 4 if A32 or 2 if T32).
-- Do not use for branches or any operation that might update R15/PC.
defineA32Opcode :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineA32Opcode = defineOpcode'A32 False

-- | An alternative version of 'defineA32Opcode' for opcodes which do
-- not have a Pred operand that controls the expression of the opcode.
defineA32OpcodeNoPred :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineA32OpcodeNoPred = defineOpcode'A32 True

-- | An alternative version of 'defineA32Opcode' for branch opcodes
-- which explicitly update the PC and therefore should not have the PC
-- automatically updated to the next instruction.
defineA32Branch :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineA32Branch = defineOpcode'A32 False

defineOpcode'A32 :: HasCallStack => Bool -> String -> SemARM 'Def () -> SemARM 'Top ()
defineOpcode'A32 noPred name def =
    if a32OpcodeNameLooksValid name
    then defineOpcode name $ do
      subarch InstrSet_A32
      input pc
      input cpsr
      unless (noPred) $ do
        predV <- param "predBits" pred (EBV 4)  -- CurrentCond() (F2.3.1, F2-2417)
        testForConditionPassed (Loc predV)
      def
      finalizeCPSR
      finalizePC
    else error $ "Opcode " <> name <> " does not look like a valid A32 ARM opcode"


-- | A wrapper around 'defineOpcode' that sets A32 mode and updates
-- the PC after the instruction executes (by 4 if A32 or 2 if T32).
-- Do not use for branches or any operation that might update R15/PC.
defineT32Opcode :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineT32Opcode = defineOpcode'T32

-- | An alternative form of defineT32Opcode that is used for Branch
-- opcodes; all PC updates are performed explicitly by the opcode
-- definition and not by this entry point.
defineT32Branch :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineT32Branch = defineOpcode'T32

defineOpcode'T32 :: HasCallStack => String -> SemARM 'Def () -> SemARM 'Top ()
defineOpcode'T32 name def =  --- inIT, notInIT, lastInIT
    if t32OpcodeNameLooksValid name
    then defineOpcode name $ do
      subarch InstrSet_T32
      input pc

      input cpsr
      let itstate_7_4 = extract 15 12 (Loc cpsr)
          itstate_3_2 = extract 11 10 (Loc cpsr)
          itstate_1_0 = extract 26 25 (Loc cpsr)
          itstate_3_0 = concat itstate_3_2 itstate_1_0
          -- CurrentCond() for T32 except T1 and T3 encodings of the Branch instruction (F2.3.1, F2-2417)
          predV = ite (bveq itstate_3_0 (LitBV 4 0b0000))
                  (ite (bveq itstate_7_4 (LitBV 4 0b0000))
                   (LitBV 4 0b1110)
                   (unpredictable (LitBV 4 0b1110))) -- ARM doc doesn't cover this case...
                  itstate_7_4
      testForConditionPassed predV

      def
      finalizeCPSR
      finalizePC
    else error $ "Opcode " <> name <> " does not look like a valid T32 ARM opcode"



a32OpcodeNameLooksValid :: String -> Bool
a32OpcodeNameLooksValid name =
    take 1 name /= "T" ||
    take 3 name == "TEQ" ||
    take 4 name == "TRAP" ||
    take 3 name == "TST"

t32OpcodeNameLooksValid :: String -> Bool
t32OpcodeNameLooksValid name = take 1 name == "T"


-- ----------------------------------------------------------------------

-- | As described for the 'SemM_ARMData' datatype, the DSL state is
-- used to store the current instruction set type (normally A32 or
-- T32).
subarch :: ArchSubtype -> SemARM t ()
subarch sa =
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ newARMData { subArch = sa }
                              Just ad -> Just $ ad { subArch = sa })


-- ----------------------------------------------------------------------
-- CPSR (APSR) management

-- | The CPSR may be updated in multiple places in the semantics for a
-- particular Opcode.  The DSL state is used to accumulate the set of
-- updates so that they can be expressed in a single value-setting
-- expression at the end of the opcode semantics definition.  This
-- function adds another expression updating the CPSR value.  There is
-- no protection against multiple updates to the same location, and
-- the effects of this are unpredictable.
updateCPSR :: (Expr 'TBV -> Expr 'TBV) -> SemARM t ()
updateCPSR updExp =
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ newARMData { cpsrUpdates = updExp }
                              Just ad -> Just $ ad { cpsrUpdates = updExp . (cpsrUpdates ad) })

-- | finalizeCPSR is called at the end of the opcode semantics
-- definition to write the new CPSR value based on the accumulated
-- expression.
finalizeCPSR :: SemARM 'Def ()
finalizeCPSR = do
    updExp <- (cpsrUpdates . fromJust) <$> getArchData
    defReg cpsr (updExp (Loc cpsr))


-- | The processor mode can be determined by examining the ISETSTATE
-- execution state register, which is embedded in the CPSR (as 'J' at
-- bit 24 and 'T' at bit 5).  The following can be used to read the
-- current processor mode.
--
-- This is not normally used however because the current instruction
-- set (arch subtype) is set in the DSL state (see 'SemM_ARMData').
-- [If this function becomes useful, add the ArchSubtype to the
-- ExprTag in the DSL definition to return the value directly instead
-- of returning the string form.]
instrSetState :: Location 'TBV -> Expr 'TString
instrSetState cpsReg =
    let cpsr_j = testBitDynamic (LitBV 32 24) (Loc cpsReg)
        cpsr_t = testBitDynamic (LitBV 32 5) (Loc cpsReg)
        isT32 = andp cpsr_t (notp cpsr_j)
        isA32 = andp (notp cpsr_t) (notp cpsr_j)
        -- isJazelle = andp (notp cpsr_t) cpsr_j
        isT32EE = andp cpsr_t cpsr_j
        toRet = LitString . show
    in ite isA32 (toRet InstrSet_A32)
           (ite isT32 (toRet InstrSet_T32)
            (ite isT32EE (toRet InstrSet_T32EE) (toRet InstrSet_Jazelle)))


-- | Update the CPSR to set to either the A32 or T32 target instruction set as
-- indicated by the expression (E1.2.3, E1-2300)
selectInstrSet :: Expr 'TBool -> Expr 'TBool -> SemARM 'Def ()
selectInstrSet isEnabled toT32 = do
    setT32 <- cpsrT32
    setA32 <- cpsrA32
    updateCPSR (\cpsrReg -> ite isEnabled
                           (ite toT32 (setT32 cpsrReg) (setA32 cpsrReg))
                           cpsrReg)


-- | Update the CPSR to set to the known concrete target instruction set.
-- (E1.2.3, E1-2300)
selectInstrSet' :: ArchSubtype -> SemARM 'Def ()
selectInstrSet' tgtarch =
    case tgtarch of
      InstrSet_A32 -> updateCPSR =<< cpsrA32
      InstrSet_T32 -> updateCPSR =<< cpsrT32
      InstrSet_T32EE -> updateCPSR =<< cpsrT32EE
      InstrSet_Jazelle -> updateCPSR =<< cpsrJazelle


cpsrA32, cpsrT32, cpsrT32EE, cpsrJazelle :: SemARM 'Def (Expr 'TBV -> Expr 'TBV)
cpsrA32 = do
    curarch <- (subArch . fromJust) <$> getArchData
    if curarch == InstrSet_A32
    then return id
    else if curarch == InstrSet_T32EE
         then error "Invalid INSTRSET change T32EE->A32"
         else return $ cpsr_jt $ arch_jt InstrSet_A32

cpsrT32 = do
    curarch <- (subArch . fromJust) <$> getArchData
    if curarch == InstrSet_T32
    then return id
    else return $ cpsr_jt $ arch_jt InstrSet_T32

cpsrT32EE = do
    curarch <- (subArch . fromJust) <$> getArchData
    if curarch == InstrSet_T32EE
    then return id
    else if curarch == InstrSet_A32
         then error "Invalid INSTRSET change A32->T32EE"
         else return $ cpsr_jt $ arch_jt InstrSet_T32

cpsrJazelle = error "Jazelle instruction set not currently supported"

cpsr_jt :: (Int, Int) -> (Expr 'TBV -> Expr 'TBV)
cpsr_jt (j, t) =
    let updJ = if j == 1 then bvset [24] else bvclr [24]
        updT = if t == 1 then bvset [5] else bvclr [5]
    in updJ . updT

arch_jt :: ArchSubtype -> (Int, Int)
arch_jt tgtarch =
    case tgtarch of
      InstrSet_A32 -> (0, 0)
      InstrSet_T32 -> (0, 1)
      InstrSet_T32EE -> (1, 1)
      InstrSet_Jazelle -> (1, 0)


-- Updates the N[31], Z[30], C[29], and V[28] condition code bits in
-- the CPSR (aka. the APSR)
cpsrNZCV :: HasCallStack => Expr 'TBool -> Expr 'TBV -> SemARM 'Def ()
cpsrNZCV isEnabled nzcv =
    let cpsr' r = concat nzcv (extract 27 0 r)
    in updateCPSR (\cpsrReg -> ite isEnabled (cpsr' cpsrReg) cpsrReg)



-- ----------------------------------------------------------------------
-- PC management

-- | The PC is normally updated to the next instruction, but it may be
-- explicitly modified by a branch or other PC-affecting opcode.  The
-- 'updatePC' function is used to store the update expression so that
-- the proper one can be expressed at the end of the instruction.
--
-- The PC update might not be certain at this point: it may only be
-- actual evaluation that can determine if the modified update should
-- be performed (e.g. isR15), so the new update is passed the old
-- update so that it can choose to use the original functionality or
-- its new functionality.
updatePC :: ((ArchSubtype -> Expr 'TBV) -> ArchSubtype -> Expr 'TBV) -> SemARM t ()
updatePC pcf =
    let mod_pcf m'ad = let ad = maybe newARMData id m'ad
                           oldUpd = pcUpdate ad
                           newUpd = pcf oldUpd
                       in Just $ ad { pcUpdate = newUpd }
    in modifyArchData mod_pcf


-- | Update the PC to the next instruction.  The PC always points to
-- the current instruction (plus an offset), so the update should be
-- done as the final step of the semantic modifications for each
-- instruction.  The mode is always known here, as initially set by
-- 'defineA32Opcode' or 'defineT32Opcode' and possibly updated during
-- instruction execution.
finalizePC :: HasCallStack => SemARM 'Def ()
finalizePC = do
  instrSet <- (subArch  . fromJust) <$> getArchData
  updExp   <- (pcUpdate . fromJust) <$> getArchData
  defLoc pc $ updExp instrSet


-- | ALUWritePC pseudocode.  This is usually for arithmetic operations
-- when they target R15/PC. (E1.2.3, E1-2297)
aluWritePC :: Expr 'TBool -> Expr 'TBV -> SemARM 'Def ()
aluWritePC tgtRegIsPC addr = do
    curarch <- (subArch . fromJust) <$> getArchData
    if curarch == InstrSet_A32
    then bxWritePC tgtRegIsPC addr
    else branchWritePC tgtRegIsPC addr


-- | LoadWritePC pseudocode.  (E1.2.3, E1-2297)
loadWritePC :: Expr 'TBV -> SemARM 'Def ()
loadWritePC = bxWritePC (LitBool True)


-- | BranchWritePC pseudocode.  (E1.2.3, E1-2296)
branchWritePC :: Expr 'TBool -> Expr 'TBV -> SemARM 'Def ()
branchWritePC tgtRegIsPC addr =
    let setAddr curarch = if curarch == InstrSet_A32
                          then bvclr [0,1] addr
                          else bvclr [1] addr
    in updatePC $ \old suba -> ite tgtRegIsPC (setAddr suba) (old suba)


-- | BxWritePC pseudocode  (E1.2.3, E1-2296)
bxWritePC :: Expr 'TBool -> Expr 'TBV -> SemARM 'Def ()
bxWritePC tgtRegIsPC addr =
    let toT32 = tstBit 0 addr
        setAddr curarch = case curarch of
                            InstrSet_T32EE -> error "TBD: bxWritePC for T32EE mode"
                            InstrSet_Jazelle -> error "TBD: bxWritePC for Jazelle mode"
                            _ -> ite toT32
                                    (bvclr [0] addr)
                                    (ite (andp (tstBit 1 addr) constrainUnpredictable)
                                         (bvclr [1] addr)
                                         addr)
    in do selectInstrSet tgtRegIsPC toT32
          updatePC $ \old suba -> ite tgtRegIsPC (setAddr suba) (old suba)


-- ----------------------------------------------------------------------

-- | Stores the expression that calculates the ConditionPassed result
-- (see the ARM documentation pseudocode) for the current instruction
-- so that subsequent defLoc operations can use this to determine if
-- they are enabled to effect the changes from the Opcode.
testForConditionPassed :: Expr 'TBV -> SemARM 'Def ()
testForConditionPassed instrPred = do
    -- assumes already: input cpsr
    let pstate_n = extract 31 31 (Loc cpsr)  -- (E1.2.4, E1-2297)
        pstate_z = extract 30 30 (Loc cpsr)
        pstate_c = extract 29 29 (Loc cpsr)
        pstate_v = extract 28 28 (Loc cpsr)
        cond_3_1 = extract 3 1 instrPred
        cond_0   = extract 0 0 instrPred
        isBitSet = bveq (LitBV 1 0b1)
        -- ConditionHolds (F2.3.1, F2-2417):
        result = ite (bveq cond_3_1 (LitBV 3 0b000)) (isBitSet pstate_z) -- EQ or NE
                 (ite (bveq cond_3_1 (LitBV 3 0b001)) (isBitSet pstate_c) -- CS or CC
                  (ite (bveq cond_3_1 (LitBV 3 0b010)) (isBitSet pstate_n) -- MI or PL
                   (ite (bveq cond_3_1 (LitBV 3 0b011)) (isBitSet pstate_v) -- VS or VC
                    (ite (bveq cond_3_1 (LitBV 3 0b100)) (andp (isBitSet pstate_c)
                                                               (notp $ isBitSet pstate_z)) -- HI or LS
                     (ite (bveq cond_3_1 (LitBV 3 0b101)) (bveq pstate_n pstate_v)  -- GE or LT
                      (ite (bveq cond_3_1 (LitBV 3 0b110)) (andp (bveq pstate_n pstate_v)
                                                                 (notp $ isBitSet pstate_z)) -- GT or LE
                       {- (bveq cond_3_1 (LitBV 3 0b111)) -} (LitBool True))))))) -- AL
        result' = ite (andp (isBitSet cond_0)
                            (bvne instrPred (LitBV 4 0b1111))) (notp result) result
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ newARMData { condPassed = result' }
                              Just ad -> Just $ ad { condPassed = result' })


-- | Performs an assignment for a conditional Opcode when the target
-- is a register location.
defReg :: Location a -> Expr a -> SemARM 'Def ()
defReg loc expr = do
  isOK <- (condPassed . fromJust) <$> getArchData
  defLoc loc $ ite isOK expr (Loc loc)


-- ----------------------------------------------------------------------

-- | Zero extension to the full native bit width of registers
zext :: Expr 'TBV -> Expr 'TBV
zext = zext' naturalBitSize


-- | Generalized zero extension to arbitrary width
zext' :: Int -> Expr 'TBV -> Expr 'TBV
zext' fullWidth e
  | extendBy == 0 = e
  | otherwise = zeroExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e

-- | Zero extension to the full native bit width of registers
sext :: Expr 'TBV -> Expr 'TBV
sext = sext' naturalBitSize


-- | Generalized zero extension to arbitrary width
sext' :: Int -> Expr 'TBV -> Expr 'TBV
sext' fullWidth e
  | extendBy == 0 = e
  | otherwise = signExtend extendBy e
  where
    extendBy = fullWidth - exprBVSize e


-- | Set bits, specifying the list of bit numbers to set (0-based)
bvset :: [Int] -> Expr 'TBV -> Expr 'TBV
bvset bitnums = bvor (naturalLitBV $ toInteger $ foldl setBit naturalZero bitnums)

-- | Clear bits, specifying the list of bit numbers to clear (0-based)
bvclr :: [Int] -> Expr 'TBV -> Expr 'TBV
bvclr bitnums = bvand (naturalLitBV $ toInteger $ complement $ foldl setBit naturalZero bitnums)

-- | Test if a specific bit is set
tstBit :: Int -> Expr 'TBV -> Expr 'TBool
tstBit n = bveq (LitBV 1 0) . extract n n


-- ----------------------------------------------------------------------
-- Opcode unpacking

-- | User defined function to place in the SemMC DST to get the value
-- contained in the register referenced by the addrmode_imm12_[pre].
-- During the Eval stage this will perform the actual extraction of the value.
imm12Reg :: Location 'TMemRef -> Location 'TBV
imm12Reg = locUF naturalBV "a32.imm12_reg"

-- | Returns the immediate value in the addrmode_imm12_[pre]
imm12Imm :: [Some Expr] -> Expr 'TBV
imm12Imm = uf naturalBV "a32.imm12_imm"

-- | Returns the addition flag in the addrmode_imm12_[pre]
imm12Add :: [Some Expr] -> Expr 'TBool
imm12Add = uf EBool "a32.imm12_add"

-- | Decoding for the ThumbBlxTarget type: S bit
blxtgt_S :: Location 'TBV -> Expr 'TBV
blxtgt_S = uf (EBV 1) "t32.blxtarget_S" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: Imm10H
blxtgt_imm10H :: Location 'TBV -> Expr 'TBV
blxtgt_imm10H = uf (EBV 10) "t32.blxtarget_imm10H" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: Imm10L
blxtgt_imm10L :: Location 'TBV -> Expr 'TBV
blxtgt_imm10L = uf (EBV 10) "t32.blxtarget_imm10L" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: J1
blxtgt_J1 :: Location 'TBV -> Expr 'TBV
blxtgt_J1 = uf (EBV 1) "t32.blxtarget_J1" . ((:[]) . Some) . Loc

-- | Decoding for the ThumbBlxTarget type: J2
blxtgt_J2 :: Location 'TBV -> Expr 'TBV
blxtgt_J2 = uf (EBV 1) "t32.blxtarget_J2" . ((:[]) . Some) . Loc


-- ----------------------------------------------------------------------

memriOffset :: Int
            -- ^ The number of bits of the offset
            -> Expr 'TMemRef
            -- ^ The memory ref expression
            -> Expr 'TBV
memriOffset osize = uf (EBV osize) "arm.memri_offset" . ((:[]) . Some)


-- | A wrapper around the two low bit extractors parameterized by bit size (it
-- selects the appropriate extractor based on architecture size)
lowBits :: Int -> Expr 'TBV -> Expr 'TBV
lowBits n e
  | n == 32 = e
  | otherwise = lowBits32 n e


lowBits64 :: Int -> Expr 'TBV -> Expr 'TBV
lowBits64 n = extract 63 (63 - n + 1)

lowBits32 :: Int -> Expr 'TBV -> Expr 'TBV
lowBits32 n = extract 31 (31 - n + 1)

lowBits128 :: Int -> Expr 'TBV -> Expr 'TBV
lowBits128 n = extract 127 (127 - n + 1)


-- ----------------------------------------------------------------------
-- Miscellaneous common functionality

-- | Can be used to flag unpredictable expressions.  For now, this
-- doesn't do much, but it is an abstraction point that allows future
-- focus and refinement on these areas.
unpredictable :: Expr a -> Expr a
unpredictable = id


-- | In some cases, the implementation of the chip is allowed to
-- decide if unpredictable behavior should be avoided.  For example,
-- in A32 mode, a target address with bit 1 set in BxWritePC can
-- either clear the bit automatically (avoiding unpredictable
-- behavior) or leave the bit set, causing a PC Alignment fault (or
-- other unpredictable behavior).   -- AppxG-5049
constrainUnpredictable :: Expr 'TBool
constrainUnpredictable = LitBool True


-- | Do both arguments refer to the same location?
sameLocation :: Location tp -> Location tp -> Expr 'TBool
sameLocation l = LitBool . maybe False (const True) . testEquality l


-- | Is the current register R15 (aka PC)?
isR15 :: Location 'TBV -> Expr 'TBool
isR15 = uf EBool "arm.is_r15" . ((:[]) . Some) . Loc
