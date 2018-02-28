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
    , am2offset_immAdd, am2offset_immImm
    , ldst_so_regBaseRegister, ldst_so_regOffsetRegister
    , ldst_so_regAdd, ldst_so_regImmediate
    , ldst_so_regShiftType
    , modImm_imm, modImm_rot
    , soRegReg_type, soRegReg_reg1, soRegReg_reg2
    , soRegImm_imm, soRegImm_type, soRegImm_reg
    , blxtgt_S, blxtgt_imm10H, blxtgt_imm10L, blxtgt_J1, blxtgt_J2
      -- * Miscellaneous common functionality
    , unpredictable
    , constrainUnpredictable
    , sameLocation
    , isR15
    , anyp
    )
    where

import Control.Monad ( when )
import Data.Bits hiding (shift)
import Data.Maybe
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.TraversableFC
import Data.Semigroup
import qualified Data.Type.List as TL
import GHC.Stack ( HasCallStack )
import Prelude hiding ( concat, pred )
import qualified Dismantle.ARM as A
import qualified Dismantle.Thumb as T
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Natural
import SemMC.Architecture.ARM.BaseSemantics.OperandClasses
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


data OpcodeParamDef t = ParamDef String String (ExprType t)

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
      let itstate_7_4 = extract 15 12 (Loc cpsr)
          itstate_3_2 = extract 11 10 (Loc cpsr)
          itstate_1_0 = extract 26 25 (Loc cpsr)
          itstate_3_0 = concat itstate_3_2 itstate_1_0
          -- CurrentCond() for T32 except T1 and T3 encodings of the Branch instruction (F2.3.1, F2-2417)
          predV = "ITState" =:
                  ite (bveq itstate_3_0 (LitBV 4 0b0000))
                  (ite (bveq itstate_7_4 (LitBV 4 0b0000))
                   (LitBV 4 0b1110)
                   (unpredictable (LitBV 4 0b1110))) -- ARM doc doesn't cover this case...
                  itstate_7_4
      testForConditionPassed predV
      defineOp'Core defbody =<< traverseFC param' defargs
      finalizeCPSR
      finalizePC
    else error $ "Opcode " <> name <> " does not look like a valid T32 ARM opcode"
        where param' :: OpcodeParamDef t -> SemARM 'Def (Location t)
              param' (ParamDef pname ty ety) = param pname ty ety

              name = show opc

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


-- | Updates the N[31], Z[30], C[29], and V[28] condition code bits in
-- the CPSR (aka. the APSR)
cpsrNZCV :: HasCallStack => Expr 'TBool -> Expr 'TBV -> SemARM 'Def ()
cpsrNZCV isEnabled nzcv =
    let cpsr' r = concat nzcv (extract 27 0 r)
    in updateCPSR (\cpsrReg -> ite isEnabled (cpsr' cpsrReg) cpsrReg)

-- | Extracts the N[31], Z[30], C[29], and V[28] bits from the CPSR
-- (E1.2.4, E1-2297)
getNZCV :: HasCallStack => (Expr 'TBV, Expr 'TBV, Expr 'TBV, Expr 'TBV)
getNZCV = let n = extract 31 31 (Loc cpsr)
              z = extract 30 30 (Loc cpsr)
              c = extract 29 29 (Loc cpsr)
              v = extract 28 28 (Loc cpsr)
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

-- | Stores the expression that calculates the ConditionPassed result
-- (see the ARM documentation pseudocode) for the current instruction
-- so that subsequent defLoc operations can use this to determine if
-- they are enabled to effect the changes from the Opcode.
--
-- This is the predication check.
testForConditionPassed :: Expr 'TBV -> SemARM 'Def ()
testForConditionPassed instrPred = do
    -- assumes already: input cpsr
    let (n,z,c,v) = getNZCV  -- (E1.2.4, E1-2297)
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
        result' = "testCondition" =: ite (andp (isBitSet cond_0)
                                          (bvne instrPred (LitBV 4 0b1111))) (notp result) result
    modifyArchData (\m'ad -> case m'ad of
                              Nothing -> Just $ newARMData { condPassed = result' }
                              Just ad -> Just $ ad { condPassed = result' })


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
imm12Reg :: Location 'TMemRef -> Location 'TBV
imm12Reg = locUF naturalBV "a32.imm12_reg"

-- | Returns the immediate offset value in the addrmode_imm12_[pre]
imm12Off :: [Some Expr] -> Expr 'TBV
imm12Off = uf (EBV 16) "a32.imm12_off"

-- | Returns the addition flag in the addrmode_imm12_[pre]
imm12Add :: [Some Expr] -> Expr 'TBool
imm12Add = uf EBool "a32.imm12_add"

-- | Returns the addition flag in the am2offset_imm
am2offset_immAdd :: [Some Expr] -> Expr 'TBool
am2offset_immAdd = uf EBool "a32.am2offset_imm_add"

-- | Returns the immediate flag in the am2offset_imm
am2offset_immImm :: [Some Expr] -> Expr 'TBV
am2offset_immImm = uf (EBV 16) "a32.am2offset_imm_imm"

-- | Returns the base register in ldst_so_reg
ldst_so_regBaseRegister :: Location 'TMemRef -> Location 'TBV
ldst_so_regBaseRegister = locUF naturalBV "a32.ldst_so_reg_base_register"

-- | Returns the offset register in ldst_so_reg
ldst_so_regOffsetRegister :: Location 'TMemRef -> Location 'TBV
ldst_so_regOffsetRegister = locUF naturalBV "a32.ldst_so_reg_offset_register"

-- | Returns the addition flag in ldst_so_reg
ldst_so_regAdd :: [Some Expr] -> Expr 'TBool
ldst_so_regAdd = uf EBool "a32.ldst_so_reg_add"

-- | Returns the immediate offset value in ldst_so_reg
ldst_so_regImmediate :: [Some Expr] -> Expr 'TBV
ldst_so_regImmediate = uf (EBV 5) "a32.ldst_so_reg_immediate"

-- | Returns the shift type in ldst_so_reg
ldst_so_regShiftType :: [Some Expr] -> Expr 'TBV
ldst_so_regShiftType = uf (EBV 2) "a32.ldst_so_reg_shift_type"

-- | Decoding for ModImm immediate octet (ARMExpandImm(), (F4.2.4, F-2473)
modImm_imm :: Location 'TBV -> Expr 'TBV
modImm_imm = uf (EBV 8) "a32.modimm_imm" . ((:[]) . Some) . Loc

-- | Decoding for ModImm rotation 4 bits (ARMExpandImm(), (F4.2.4, F-2473)
modImm_rot :: Location 'TBV -> Expr 'TBV
modImm_rot = uf (EBV 4) "a32.modimm_rot" . ((:[]) . Some) . Loc

-- | Extract the shift type from a so_reg_reg
soRegReg_type :: Location 'TMemRef -> Expr 'TBV
soRegReg_type = uf (EBV 2) "a32.soregreg_type" . ((:[]) . Some) . Loc

-- | Extract the register containing the shift amount from a so_reg_reg
soRegReg_reg1 :: Location 'TMemRef -> Location 'TBV
soRegReg_reg1 = locUF naturalBV "a32.soregreg_reg1"

-- | Extract the register containing the value to be shifted from a so_reg_reg
soRegReg_reg2 :: Location 'TMemRef -> Location 'TBV
soRegReg_reg2 = locUF naturalBV "a32.soregreg_reg2"

soRegImm_type :: Location 'TMemRef -> Expr 'TBV
soRegImm_type = uf (EBV 2) "a32.soregimm_type" . ((:[]) . Some) . Loc

soRegImm_reg :: Location 'TMemRef -> Location 'TBV
soRegImm_reg = locUF naturalBV "a32.soregimm_reg"

soRegImm_imm :: Location 'TMemRef -> Expr 'TBV
soRegImm_imm = uf (EBV 5) "a32.soregimm_imm" . ((:[]) . Some) . Loc

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
