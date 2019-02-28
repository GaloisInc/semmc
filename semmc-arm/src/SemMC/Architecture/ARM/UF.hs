{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}


module SemMC.Architecture.ARM.UF
    ( uninterpretedFunctions
    , mkReadMemUF
    , mkWriteMemUF
    )
    where

import Data.Parameterized.Context
import GHC.TypeLits
import qualified SemMC.Architecture as A
import What4.BaseTypes
import Data.Proxy

-- | The basic type of indices into the array
type BaseIdxType arch = BaseBVType (A.RegWidth arch)
-- | The type of the memory array
type BaseMemType arch = BaseArrayType (SingleCtx (BaseIdxType arch)) (BaseBVType 8)

uninterpretedFunctions :: forall proxy arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm) =>
                         proxy arm
                       -> [A.UninterpFn arm]
uninterpretedFunctions _ =
  [ -- is_r15
    A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @BaseBoolType @arm
                   "arm.is_r15"
                   (\_ -> [])
  , -- conditionPassed; can execute instr? inputs are Pred and CCR
    A.mkUninterpFn @(EmptyCtx ::> BaseBVType 4 ::> BaseBVType 4)
                   @BaseBoolType
                   "arm.conditionPassed"
                   (\_ -> [])
  -- A32 operands

  -- imm12_reg: reference to register by register number from an addrmode_imm12_pre operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseIdxType arm)
                   "a32.imm12_reg"
                   (\_ -> [])
  -- imm12_off: reference to immediate offset value from an addrmode_imm12_pre operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 12)
                   "a32.imm12_off"
                   (\_ -> [])
  -- a32.imm12_add: reference to U flag bit from an addrmode_imm12_pre operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @BaseBoolType
                   "a32.imm12_add"
                   (\_ -> [])

  -- a32.am2offset_imm_add: reference to U flag bit from an am2offset_imm operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @BaseBoolType
                   "a32.am2offset_imm_add"
                   (\_ -> [])
  -- a32.am2offset_imm_imm -- reference to immediate offset value from an am2offset_imm operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 12)
                   "a32.am2offset_imm_imm"
                   (\_ -> [])

  -- a32.ldst_so_reg_base_register -- ref to base register from ldst_so_reg operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseIdxType arm)
                   "a32.ldst_so_reg_base_register"
                   (\_ -> [])
  -- a32.ldst_so_reg_offset_register -- ref to offset register from ldst_so_reg operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseIdxType arm)
                   "a32.ldst_so_reg_offset_register"
                   (\_ -> [])

  -- "a32.ldst_so_reg_add" -- ref to add/subtract flag from ldst_so_reg operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @BaseBoolType
                   "a32.ldst_so_reg_add"
                   (\_ -> [])
  -- "a32.ldst_so_reg_immediate" -- ref to immediate value from ldst_so_reg operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 5)
                   "a32.ldst_so_reg_immediate"
                   (\_ -> [])
  -- "a32.ldst_so_reg_shift_type" -- ref to shift type value from ldst_so_reg operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 2)
                   "a32.ldst_so_reg_shift_type"
                   (\_ -> [])

  -- "a32.modimm_imm" -- reference to octet immediate value from a modimm operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 8)
                   "a32.modimm_imm"
                   (\_ -> [])
  -- "a32.modimm_rot" -- reference to 4-bit rotation value a modimm operand
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 4)
                   "a32.modimm_rot"
                   (\_ -> [])

  -- "a32.soregreg_type" -- Extract the two bit shift type
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 2)
                   "a32.soregreg_type"
                   (\_ -> [])
  -- "a32.soregreg_reg1" -- Extract the register containing the shift amount
   , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                    @(BaseIdxType arm)
                    "a32.soregreg_reg1"
                    (\_ -> [])
  -- "a32.soregreg_reg2" -- Extract the register being shifted
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseIdxType arm)
                   "a32.soregreg_reg2"
                   (\_ -> [])
  -- "a32.soregimm_type" -- Extract the two bit shift type
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 2)
                   "a32.soregimm_type"
                   (\_ -> [])
  -- "a32.soregimm_reg" -- Extract the register being shifted
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseIdxType arm)
                   "a32.soregimm_reg"
                   (\_ -> [])
  -- "a32.soregimm_imm" -- Extract the shift amount
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 5)
                   "a32.soregimm_imm"
                   (\_ -> [])

    -- T32 Operands

  -- "t32.blxtarget_S" -- operand ThumbBlxTarget S bit reference
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 1)
                   "t32.blxtarget_S"
                   (\_ -> [])

  -- "t32.blxtarget_imm10H" -- operand ThumbBlxTarget immediate, upper half reference
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 10)
                   "t32.blxtarget_imm10H"
                   (\_ -> [])

  -- "t32.blxtarget_imm10L" -- operand ThumbBlxTarget immediate, lower half reference
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 10)
                   "t32.blxtarget_imm10L"
                   (\_ -> [])

  -- "t32.blxtarget_J1" -- operand ThumbBlxTarget J1 bit reference
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 1)
                   "t32.blxtarget_J1"
                   (\_ -> [])

  -- "t32.blxtarget_J2" -- operand ThumbBlxTarget J2 bit reference
  , A.mkUninterpFn @(SingleCtx (BaseIdxType arm))
                   @(BaseBVType 1)
                   "t32.blxtarget_J2"
                   (\_ -> [])

  -- "t32.imm0_1020S4_imm" -- Extract the immediate value
  , A.mkUninterpFn @(SingleCtx (BaseBVType 8))
                   @(BaseBVType 8)
                   "t32.imm0_1020S4_imm"
                   (\_ -> [])

  -- "t32.imm0_508S4_imm" -- Extract the immediate value
  , A.mkUninterpFn @(SingleCtx (BaseBVType 8))
                   @(BaseBVType 8)
                   "t32.imm0_508S4_imm"
                   (\_ -> [])

  -- "t32.reglist" -- Extract the register bitmask
  , A.mkUninterpFn @(SingleCtx (BaseBVType 16))
                   @(BaseBVType 16)
                   "t32.reglist"
                   (\_ -> [])


  -- "t32.t2soimm_imm" -- Extract the immediate value
  , A.mkUninterpFn @(SingleCtx (BaseBVType 16))
                   @(BaseBVType 12)
                   "t32.t2soimm_imm"
                   (\_ -> [])

  -- "t32.t2soreg_reg" -- Extract the register value
  , A.mkUninterpFn @(SingleCtx (BaseBVType 32))
                   @(BaseBVType 32)
                   "t32.t2soreg_reg"
                   (\_ -> [])

  -- "t32.t2soreg_imm" -- Extract the immediate value
  , A.mkUninterpFn @(SingleCtx (BaseBVType 32))
                   @(BaseBVType 5)
                   "t32.t2soreg_imm"
                   (\_ -> [])

  -- "t32.t2soreg_type" -- Extract the immediate value
  , A.mkUninterpFn @(SingleCtx (BaseBVType 32))
                   @(BaseBVType 2)
                   "t32.t2soreg_type"
                   (\_ -> [])

  -- "t32.addrmode_is2_imm"
  , A.mkUninterpFn @(SingleCtx (BaseBVType 32))
                   @(BaseBVType 5)
                   "t32.addrmode_is2_imm"
                   (\_ -> [])

  -- "t32.addrmode_is2_reg"
  , A.mkUninterpFn @(SingleCtx (BaseBVType 32))
                   @(BaseIdxType arm)
                   "t32.addrmode_is2_reg"
                   (\_ -> [])

  -- "t32.addrmode_is4_imm"
  , A.mkUninterpFn @(SingleCtx (BaseBVType 32))
                   @(BaseBVType 5)
                   "t32.addrmode_is4_imm"
                   (\_ -> [])

  -- "t32.addrmode_is4_reg"
  , A.mkUninterpFn @(SingleCtx (BaseBVType 32))
                   @(BaseIdxType arm)
                   "t32.addrmode_is4_reg"
                   (\_ -> [])

  -- "t32.addrmode_pc"
  , A.mkUninterpFn @(SingleCtx (BaseBVType 8))
                   @(BaseBVType 8)
                   "t32.addrmode_pc"
                   (\_ -> [])

  ]
  ++ (mkPopcntUF <$> [16,32])
  ++ (mkWriteMemUF <$> [8,16,32,64])
  ++ (mkReadMemUF <$> [8,16,32,64])

mkPopcntUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
           => Integer
           -> A.UninterpFn arm
mkPopcntUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                    , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(SingleCtx (BaseBVType n))
                   @(BaseBVType n)
                   ("popcnt." ++ show n)
                   (\_ -> [])
mkPopcntUF n | otherwise = error $ "Cannot construct popcnt." ++ show n

mkReadMemUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
            => Integer
            -> A.UninterpFn arm
mkReadMemUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
                      , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(EmptyCtx ::> BaseMemType arm ::> BaseIdxType arm)
                   @(BaseBVType n)
                   ("read_mem." ++ show n)
                   (\(_ :> _ :> idx) -> [A.ReadData idx])
mkReadMemUF n | otherwise = error $ "Cannot construct read_mem." ++ show n

mkWriteMemUF :: forall arm. (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm)
             => Integer
             -> A.UninterpFn arm
mkWriteMemUF n | Just (SomeNat (_ :: Proxy n)) <- someNatVal n
               , NatGT _ <- compareNat (knownNat @n) (knownNat @0)
  = A.mkUninterpFn @(EmptyCtx ::> BaseMemType arm ::> BaseIdxType arm ::> BaseBVType n)
                   @(BaseMemType arm)
                   ("write_mem." ++ show n)
                   $ \(_ :> _ :> idx :> val) -> [A.WriteData idx val]
mkWriteMemUF n | otherwise = error $ "Cannot construct write_mem." ++ show n



{-
  [ ("arm.is_r15",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))

  , ("arm.conditionPassed",  -- can execute instr?  inputs are Pred and CCR
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 4
                                                         ::> BaseBVType 4)),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))

    -- A32 Operands

  , ("a32.imm12_reg", -- reference to register by register number from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))
  , ("a32.imm12_off", -- reference to immediate offset value from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 12)))
  , ("a32.imm12_add", -- reference to U flag bit from an addrmode_imm12_pre operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))

  , ("a32.am2offset_imm_add", -- reference to U flag bit from an am2offset_imm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("a32.am2offset_imm_imm", -- reference to immediate offset value from an am2offset_imm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 12)))

  , ("a32.ldst_so_reg_base_register", -- ref to base register from ldst_so_reg operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))
  , ("a32.ldst_so_reg_offset_register", -- ref to offset register from ldst_so_reg operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))
  , ("a32.ldst_so_reg_add", -- ref to add/subtract flag from ldst_so_reg operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr BaseBoolType))
  , ("a32.ldst_so_reg_immediate", -- ref to immediate value from ldst_so_reg operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 5)))
  , ("a32.ldst_so_reg_shift_type", -- ref to shift type value from ldst_so_reg operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 2)))

  , ("a32.modimm_imm", -- reference to octet immediate value from a modimm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))
  , ("a32.modimm_rot", -- reference to 4-bit rotation value a modimm operand
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 4)))

  , ("a32.soregreg_type", -- Extract the two bit shift type
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 2)))
  , ("a32.soregreg_reg1", -- Extract the register containing the shift amount
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))
  , ("a32.soregreg_reg2", -- Extract the register being shifted
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))

  , ("a32.soregimm_type", -- Extract the two bit shift type
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 2)))
  , ("a32.soregimm_reg", -- Extract the register being shifted
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))
  , ("a32.soregimm_imm", -- Extract the shift amount
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 5)))

    -- T32 Operands

  , ("t32.blxtarget_S", -- operand ThumbBlxTarget S bit reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 1)))

  , ("t32.blxtarget_imm10H", -- operand ThumbBlxTarget immediate, upper half reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 10)))

  , ("t32.blxtarget_imm10L", -- operand ThumbBlxTarget immediate, lower half reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 10)))

  , ("t32.blxtarget_J1", -- operand ThumbBlxTarget J1 bit reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 1)))

  , ("t32.blxtarget_J2", -- operand ThumbBlxTarget J2 bit reference
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType (A.RegWidth arm))),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 1)))

  , ("t32.imm0_1020S4_imm", -- Extract the immediate value
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 8)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))

  , ("t32.imm0_508S4_imm", -- Extract the immediate value
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 8)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))

  , ("t32.reglist", -- Extract the register bitmask
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 16)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 16)))

  , ("t32.t2soimm_imm", -- Extract the immediate value
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 16)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 12)))

  , ("t32.t2soreg_reg", -- Extract the register value
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 32)))

  , ("t32.t2soreg_imm", -- Extract the immediate value
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 5)))

  , ("t32.t2soreg_type", -- Extract the immediate value
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 2)))

  , ("t32.addrmode_is2_imm",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 5)))

  , ("t32.addrmode_is2_reg",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))

  , ("t32.addrmode_is4_imm",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 5)))

  , ("t32.addrmode_is4_reg",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 32)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType (A.RegWidth arm))))

  , ("t32.addrmode_pc",
     Some (knownRepr :: Assignment BaseTypeRepr (EmptyCtx ::> BaseBVType 8)),
     Some (knownRepr :: BaseTypeRepr (BaseBVType 8)))

  ]
  ++ $(ufGen "popcnt" [16, 32])
  ++ $(ufRGen "read_mem" [8, 16, 32, 64])
  ++ $(ufWGen "write_mem" [8, 16, 32, 64])
-}
