{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Architecture.ARM.BaseSemantics.Helpers
    ( -- * Opcode definition assistance
      defineA32Opcode
    , defineT32Opcode
    , OpcodeParamDef(..)
    -- * CPSR (APSR) management
    , updateCPSR
    , cpsrNZCV
    , getNZCV
    -- * PC management
    , updatePC
    -- * Result definition via instruction predicate control
    , defReg
    -- * Manipulation of bit values (BV)
    , zext, zext'
    , sext, sext'
    , bvset, bvclr, tstBit
    -- * Opcode unpacking
    , imm12Reg, imm12Off, imm12Add
    , addrmode_is2_reg, addrmode_is2_imm
    , addrmode_is4_reg, addrmode_is4_imm
    , am2offset_immAdd, am2offset_immImm
    , ldst_so_regBaseRegister, ldst_so_regOffsetRegister
    , ldst_so_regAdd, ldst_so_regImmediate
    , ldst_so_regShiftType
    , modImm_imm, modImm_rot
    , register_list
    , soRegReg_type, soRegReg_reg1, soRegReg_reg2
    , soRegImm_imm, soRegImm_type, soRegImm_reg
    , t_addrmode_pc_val
    , t2SoImm_imm
    , t2SoReg_reg, t2SoReg_imm, t2SoReg_type
    , t32_imm_0_1020s4_val, t32_imm_0_508s4_val
    , blxtgt_S, blxtgt_imm10H, blxtgt_imm10L, blxtgt_J1, blxtgt_J2
      -- * Miscellaneous common functionality
    , unpredictable
    , constrainUnpredictable
    , sameLocation
    , isR15
    , anyp
    )
    where

import           Control.Monad ( when )
import           Data.Bits hiding (shift)
import           Data.List (isPrefixOf)
import           Data.Maybe
import           Data.Parameterized.Classes
import           Data.Parameterized.Context
import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.TraversableFC
import           Data.Semigroup hiding ( Arg )
import qualified Data.Type.List as TL
import qualified Dismantle.ARM as A
import qualified Dismantle.Thumb as T
import           GHC.Stack ( HasCallStack )
import           Prelude hiding ( concat, pred )
import           SemMC.Architecture.ARM.BaseSemantics.Base
import           SemMC.Architecture.ARM.BaseSemantics.Natural
import           SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import           SemMC.Architecture.ARM.BaseSemantics.Registers
import           SemMC.DSL


data OpcodeParamDef t = ParamDef String String (ExprTypeRepr t)

defineA32Opcode :: (CurryAssignmentClass args, TL.SameShape args (TL.Map SymToExprTagWrapper sh))
                => A.Opcode A.Operand sh
                -> Assignment OpcodeParamDef args
                -> CurryAssignment args Location (SemARM 'Def ())
                -> SemARM 'Top ()
defineA32Opcode opc defargs defbody =
    if a32OpcodeNameLooksValid name
    then defineOpcode name $ do
      subarch InstrSet_A32
      input pc
      input cpsr
      defineOp'Core defbody =<< traverseFC param' defargs
      finalizeCPSR
      finalizePC
    else error $ "Opcode " <> name <> " does not look like a valid A32 ARM opcode"
        where param' :: OpcodeParamDef t -> SemARM 'Def (Location t)
              param' (ParamDef pname ty ety) = do
                -- Convert all param descriptions to explicit
                -- instantiations of a Location
                p <- param pname ty ety
                -- Check for the *pred* param and use it to perform
                -- the comparison to the CPSR to see if this
                -- instruction should be executed (storing the result
                -- in the state monad for use by defMem and
                -- defReg). CurrentCond() (F2.3.1, F2-2417)
                when (ty == pred) $ case testEquality (exprType $ Loc p) (EBV 4) of
                                      Just Refl -> testForConditionPassed (Loc p)
                                      Nothing -> return ()
                return p

              name = show opc

defineOp'Core :: CurryAssignmentClass args =>
                 CurryAssignment args Location (SemARM 'Def ())
              -> Assignment Location args
              -> SemARM 'Def ()
defineOp'Core defbody deflocs =
    uncurryAssignment defbody deflocs


defineT32Opcode :: (HasCallStack, CurryAssignmentClass args, TL.SameShape args (TL.Map SymToExprTagWrapper sh)) =>
                   T.Opcode T.Operand sh
                -> Assignment OpcodeParamDef args
                -> CurryAssignment args Location (SemARM 'Def ())
                -> SemARM 'Top ()
defineT32Opcode opc defargs defbody =
    if t32OpcodeNameLooksValid name
    then defineOpcode name $ do
      subarch InstrSet_T32
      input pc
      input cpsr
      -- n.b. ISETSTATE is updated between instruction execution to
      -- the next state in the ITBLOCK (E1-2300).  SemMC is only
      -- concerned with the discrete semantics of individual opcodes;
      -- the ITSTATE updates form the machine state that must be
      -- maintained by the evaluator of these instructions.
      let predV = lf itStateLF (Loc cpsr :< Nil)
      testForConditionPassed predV
      defineOp'Core defbody =<< traverseFC param' defargs
      finalizeCPSR
      finalizePC
    else error $ "Opcode " <> name <> " does not look like a valid T32 ARM opcode"
        where param' :: OpcodeParamDef t -> SemARM 'Def (Location t)
              param' (ParamDef pname ty ety) = param pname ty ety

              name = show opc

itStateLF :: LibraryFunctionDef '( '[ 'TBV], 'TBV)
itStateLF = defineLibraryFunction "ITState" (Arg "cpsr" naturalBV :< Nil) $
  \cpsrValue ->
    let itstate_7_4 = extract 15 12 cpsrValue
        itstate_3_2 = extract 11 10 cpsrValue
        itstate_1_0 = extract 26 25 cpsrValue
        itstate_3_0 = concat itstate_3_2 itstate_1_0
        -- CurrentCond() for T32 except T1 and T3 encodings of the Branch instruction (F2.3.1, F2-2417)
    in ite (bveq itstate_3_0 (LitBV 4 0b0000))
       (ite (bveq itstate_7_4 (LitBV 4 0b0000))
        (LitBV 4 0b1110)
        (unpredictable (LitBV 4 0b1110))) -- ARM doc doesn't cover this case...
       itstate_7_4

a32OpcodeNameLooksValid :: String -> Bool
a32OpcodeNameLooksValid name =
    or [ not ("T" `isPrefixOf` name)
       , "TEQ" `isPrefixOf` name
       , "TRAP" `isPrefixOf` name
       , "TST" `isPrefixOf` name
       ]

t32OpcodeNameLooksValid :: String -> Bool
t32OpcodeNameLooksValid name = "T" `isPrefixOf` name


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


-- | Updates the N[31], Z[30], C[29], and V[28] condition code bits in
-- the CPSR (aka. the APSR)
cpsrNZCV :: HasCallStack => Expr 'TBool -> Expr 'TBV -> SemARM 'Def ()
cpsrNZCV isEnabled nzcv =
    let cpsr' r = concat nzcv (extract 27 0 r)
    in updateCPSR (\cpsrReg -> ite isEnabled (cpsr' cpsrReg) cpsrReg)

-- | Extracts the N[31], Z[30], C[29], and V[28] bits from the CPSR
-- (E1.2.4, E1-2297)
getNZCV :: HasCallStack => (Expr 'TBV, Expr 'TBV, Expr 'TBV, Expr 'TBV)
getNZCV = splitNZCV (Loc cpsr)

splitNZCV :: HasCallStack
          => Expr 'TBV
          -> (Expr 'TBV, Expr 'TBV, Expr 'TBV, Expr 'TBV)
splitNZCV bv = let n = extract 31 31 bv
                   z = extract 30 30 bv
                   c = extract 29 29 bv
                   v = extract 28 28 bv
               in (n, z, c, v)

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
updatePC :: (PCUpdateExpr -> PCUpdateExpr) -> SemARM t ()
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
  defLoc pc $ updExp instrSet (Loc pc)


-- ----------------------------------------------------------------------

testConditionLF :: LibraryFunctionDef '(['TBV, 'TBV], 'TBool)
testConditionLF =
  -- TODO Would be easier to use getNZCV inside the library function, but then
  -- the formula parser complains that CPSR wasn't declared as an input.
  -- Effectively, library functions must be pure functions of their arguments.
  defineLibraryFunction "testCondition"
    (Arg "instrPred" (EBV 4) :<
     Arg "cpsr" (EBV 32) :< Nil) $
  \instrPred cpsrValue ->
    let (n,z,c,v) = splitNZCV cpsrValue  -- (E1.2.4, E1-2297)
        cond_3_1 = extract 3 1 instrPred
        cond_0   = extract 0 0 instrPred
        isBitSet = bveq (LitBV 1 0b1)
        -- ConditionHolds (F2.3.1, F2-2417):
        result = "conditionMatch" =:
                 ite (bveq cond_3_1 (LitBV 3 0b000)) (isBitSet z) -- EQ or NE
                 (ite (bveq cond_3_1 (LitBV 3 0b001)) (isBitSet c) -- CS or CC
                  (ite (bveq cond_3_1 (LitBV 3 0b010)) (isBitSet n) -- MI or PL
                   (ite (bveq cond_3_1 (LitBV 3 0b011)) (isBitSet v) -- VS or VC
                    (ite (bveq cond_3_1 (LitBV 3 0b100)) (andp (isBitSet c)
                                                               (notp $ isBitSet z)) -- HI or LS
                     (ite (bveq cond_3_1 (LitBV 3 0b101)) (bveq n v)  -- GE or LT
                      (ite (bveq cond_3_1 (LitBV 3 0b110)) (andp (bveq n v)
                                                                 (notp $ isBitSet z)) -- GT or LE
                       {- (bveq cond_3_1 (LitBV 3 0b111)) -} (LitBool True))))))) -- AL
    in ite (andp (isBitSet cond_0) (bvne instrPred (LitBV 4 0b1111))) (notp result) result

-- | Stores the expression that calculates the ConditionPassed result
-- (see the ARM documentation pseudocode) for the current instruction
-- so that subsequent defLoc operations can use this to determine if
-- they are enabled to effect the changes from the Opcode.
--
-- This is the predication check.
testForConditionPassed :: Expr 'TBV -> SemARM 'Def ()
testForConditionPassed instrPred = do
    -- assumes already: input cpsr
    let result = lf testConditionLF (instrPred :< Loc cpsr :< Nil)
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ newARMData { condPassed = result }
                              Just ad -> Just $ ad { condPassed = result })


-- | Performs an assignment for a conditional Opcode when the target
-- is a register location.
defReg :: HasCallStack => Location a -> Expr a -> SemARM 'Def ()
defReg loc expr = do
  isOK <- (condPassed . fromJust) <$> getArchData
  defLoc loc $ ite isOK expr (Loc loc)


-- ----------------------------------------------------------------------
-- Manipulation of Bit Values (BV)

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
imm12Reg :: Location 'TPackedOperand -> Location 'TBV
imm12Reg = unpackLocUF "Imm12" naturalBV "a32.imm12_reg"

-- | Returns the immediate offset value in the addrmode_imm12_[pre]
imm12Off :: Location 'TPackedOperand -> Expr 'TBV
imm12Off = unpackUF "Imm12" (EBV 16) "a32.imm12_off"

-- | Returns the addition flag in the addrmode_imm12_[pre]
imm12Add :: Location 'TPackedOperand -> Expr 'TBool
imm12Add = unpackUF "Imm12" EBool "a32.imm12_add"

-- | Returns the addition flag in the am2offset_imm
am2offset_immAdd :: Location 'TPackedOperand -> Expr 'TBool
am2offset_immAdd = unpackUF "Am2Offset_Imm" EBool "a32.am2offset_imm_add"

-- | Returns the immediate flag in the am2offset_imm
am2offset_immImm :: Location 'TPackedOperand -> Expr 'TBV
am2offset_immImm = unpackUF "Am2Offset_Imm" (EBV 16) "a32.am2offset_imm_imm"

-- | Returns the base register in ldst_so_reg
ldst_so_regBaseRegister :: Location 'TPackedOperand -> Location 'TBV
ldst_so_regBaseRegister = unpackLocUF "LdstSoReg" naturalBV "a32.ldst_so_reg_base_register"

-- | Returns the offset register in ldst_so_reg
ldst_so_regOffsetRegister :: Location 'TPackedOperand -> Location 'TBV
ldst_so_regOffsetRegister = unpackLocUF "LdstSoReg" naturalBV "a32.ldst_so_reg_offset_register"

-- | Returns the addition flag in ldst_so_reg
ldst_so_regAdd :: Location 'TPackedOperand -> Expr 'TBool
ldst_so_regAdd = unpackUF "LdstSoReg" EBool "a32.ldst_so_reg_add"

-- | Returns the immediate offset value in ldst_so_reg
ldst_so_regImmediate :: Location 'TPackedOperand -> Expr 'TBV
ldst_so_regImmediate = unpackUF "LdstSoReg" (EBV 5) "a32.ldst_so_reg_immediate"

-- | Returns the shift type in ldst_so_reg
ldst_so_regShiftType :: Location 'TPackedOperand -> Expr 'TBV
ldst_so_regShiftType = unpackUF "LdstSoReg" (EBV 2) "a32.ldst_so_reg_shift_type"

-- | Decoding for ModImm immediate octet (ARMExpandImm(), (F4.2.4, F-2473)
modImm_imm :: Location 'TPackedOperand -> Expr 'TBV
modImm_imm = unpackUF "ModImm" (EBV 8) "a32.modimm_imm"

-- | Decoding for ModImm rotation 4 bits (ARMExpandImm(), (F4.2.4, F-2473)
modImm_rot :: Location 'TPackedOperand -> Expr 'TBV
modImm_rot = unpackUF "ModImm" (EBV 4) "a32.modimm_rot"

-- | Decoding for Reglist register list (see PUSH/POP)
register_list :: Location 'TPackedOperand -> Expr 'TBV
register_list = unpackUF "Reglist" (EBV 16) "t32.reglist"

-- | Extract the shift type from a so_reg_reg
soRegReg_type :: Location 'TPackedOperand -> Expr 'TBV
soRegReg_type = unpackUF "SoRegReg" (EBV 2) "a32.soregreg_type"

-- | Extract the register containing the shift amount from a so_reg_reg
soRegReg_reg1 :: Location 'TPackedOperand -> Location 'TBV
soRegReg_reg1 = unpackLocUF "SoRegReg" naturalBV "a32.soregreg_reg1"

-- | Extract the register containing the value to be shifted from a so_reg_reg
soRegReg_reg2 :: Location 'TPackedOperand -> Location 'TBV
soRegReg_reg2 = unpackLocUF "SoRegReg" naturalBV "a32.soregreg_reg2"

soRegImm_type :: Location 'TPackedOperand -> Expr 'TBV
soRegImm_type = unpackUF "SoRegImm" (EBV 2) "a32.soregimm_type"

soRegImm_reg :: Location 'TPackedOperand -> Location 'TBV
soRegImm_reg = unpackLocUF "SoRegImm" naturalBV "a32.soregimm_reg"

soRegImm_imm :: Location 'TPackedOperand -> Expr 'TBV
soRegImm_imm = unpackUF "SoRegImm" (EBV 5) "a32.soregimm_imm"

t2SoImm_imm :: Location 'TPackedOperand -> Expr 'TBV
t2SoImm_imm = unpackUF "T2_So_Imm" (EBV 12) "t32.t2soimm_imm"

t2SoReg_reg :: Location 'TPackedOperand -> Expr 'TBV
t2SoReg_reg = unpackUF "T2_So_Reg" naturalBV "t32.t2soreg_reg"

t2SoReg_type :: Location 'TPackedOperand -> Expr 'TBV
t2SoReg_type = unpackUF "T2_So_Reg" (EBV 2) "t32.t2soreg_type"

t2SoReg_imm :: Location 'TPackedOperand -> Expr 'TBV
t2SoReg_imm = unpackUF "T2_So_Reg" (EBV 5) "t32.t2soreg_imm"

t32_imm_0_1020s4_val :: Location 'TPackedOperand -> Expr 'TBV
t32_imm_0_1020s4_val = unpackUF "imm0_1020s4" (EBV 8) "t32.imm0_1020S4_imm"

t32_imm_0_508s4_val :: Location 'TPackedOperand -> Expr 'TBV
t32_imm_0_508s4_val = unpackUF "imm0_508s4" (EBV 8) "t32.imm0_508S4_imm"

addrmode_is2_imm :: Location 'TPackedOperand -> Expr 'TBV
addrmode_is2_imm = unpackUF "T_AddrMode_IS2" (EBV 5) "t32.addrmode_is2_imm"

addrmode_is2_reg :: Location 'TPackedOperand -> Location 'TBV
addrmode_is2_reg = unpackLocUF "T_AddrMode_IS2" naturalBV "t32.addrmode_is2_reg"

addrmode_is4_imm :: Location 'TPackedOperand -> Expr 'TBV
addrmode_is4_imm = unpackUF "T_AddrMode_IS4" (EBV 5) "t32.addrmode_is4_imm"

addrmode_is4_reg :: Location 'TPackedOperand -> Location 'TBV
addrmode_is4_reg = unpackLocUF "T_AddrMode_IS4" naturalBV "t32.addrmode_is4_reg"

-- | Decoding for Thumb T_addrmode_pc operand
t_addrmode_pc_val :: Location 'TPackedOperand -> Expr 'TBV
t_addrmode_pc_val = unpackUF "T_AddrMode_PC" (EBV 8) "t32.addrmode_pc"

-- | Decoding for the ThumbBlxTarget type: S bit
blxtgt_S :: Location 'TPackedOperand -> Expr 'TBV
blxtgt_S = unpackUF "ThumbBlxTarget" (EBV 1) "t32.blxtarget_S"

-- | Decoding for the ThumbBlxTarget type: Imm10H
blxtgt_imm10H :: Location 'TPackedOperand -> Expr 'TBV
blxtgt_imm10H = unpackUF "ThumbBlxTarget" (EBV 10) "t32.blxtarget_imm10H"

-- | Decoding for the ThumbBlxTarget type: Imm10L
blxtgt_imm10L :: Location 'TPackedOperand -> Expr 'TBV
blxtgt_imm10L = unpackUF "ThumbBlxTarget" (EBV 10) "t32.blxtarget_imm10L"

-- | Decoding for the ThumbBlxTarget type: J1
blxtgt_J1 :: Location 'TPackedOperand -> Expr 'TBV
blxtgt_J1 = unpackUF "ThumbBlxTarget" (EBV 1) "t32.blxtarget_J1"

-- | Decoding for the ThumbBlxTarget type: J2
blxtgt_J2 :: Location 'TPackedOperand -> Expr 'TBV
blxtgt_J2 = unpackUF "ThumbBlxTarget" (EBV 1) "t32.blxtarget_J2"


-- ----------------------------------------------------------------------


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

anyp :: [Expr 'TBool] -> Expr 'TBool
anyp [] = LitBool False
anyp (r : rs) = orp r (anyp rs)
