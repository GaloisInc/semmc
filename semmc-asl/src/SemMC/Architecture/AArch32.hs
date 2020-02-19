{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module SemMC.Architecture.AArch32
    ( AArch32
    , MachineState(..)
    , Instruction
    , ARMOpcode(..)
    , ARMOperand(..)
    , machineStateToBS
    , machineStateFromBS
    , shapeReprType
    , module SemMC.Architecture.ARM.Combined  -- for the instances
    )
    where

import           Control.Monad ( replicateM )
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty ( NonEmpty(..), fromList )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Vector.Sized as V
import           Data.Word ( Word8, Word32 )
import qualified Data.Word.Indexed as W
import           GHC.TypeLits
import           Language.Haskell.TH hiding ( recover )

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Location as AL
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Formula as F
import qualified SemMC.Synthesis.Template as T
import qualified SemMC.Util as U

import           What4.BaseTypes
import qualified What4.Expr as WE
import qualified What4.Interface as S

import qualified Dismantle.ARM.A32 as A32
import qualified Dismantle.ARM.T32 as T32

import qualified Language.ASL.Globals as ASL

import qualified SemMC.Architecture.ARM.UF as UF
import           SemMC.Architecture.ARM.Combined -- A32 + T32
import           SemMC.Architecture.ARM.Location

data AArch32  -- arch type


-- ----------------------------------------------------------------------

data MachineState -- machinestate

type Parser = P.Parsec String String

type Instruction = LB.ByteString

machineStateToBS :: MachineState -> B.ByteString
machineStateToBS ms = error ""

machineStateFromBS :: B.ByteString -> Maybe MachineState
machineStateFromBS bs = error ""

getMachineState :: G.Get MachineState
getMachineState =  error ""

-- ----------------------------------------------------------------------

type instance A.Opcode   AArch32 = ARMOpcode
type instance A.Operand  AArch32 = ARMOperand
type instance A.Location AArch32 = Location AArch32

instance A.IsOpcode  ARMOpcode
instance A.IsOperand ARMOperand

type instance A.OperandType AArch32 "Bv1" = BaseBVType 1
type instance A.OperandType AArch32 "Bv2" = BaseBVType 2
type instance A.OperandType AArch32 "Bv3" = BaseBVType 3
type instance A.OperandType AArch32 "Bv4" = BaseBVType 4
type instance A.OperandType AArch32 "Bv5" = BaseBVType 5
type instance A.OperandType AArch32 "Bv6" = BaseBVType 6
type instance A.OperandType AArch32 "Bv7" = BaseBVType 7
type instance A.OperandType AArch32 "Bv8" = BaseBVType 8
type instance A.OperandType AArch32 "Bv9" = BaseBVType 9
type instance A.OperandType AArch32 "Bv10" = BaseBVType 10
type instance A.OperandType AArch32 "Bv11" = BaseBVType 11
type instance A.OperandType AArch32 "Bv12" = BaseBVType 12
type instance A.OperandType AArch32 "Bv13" = BaseBVType 13
type instance A.OperandType AArch32 "Bv14" = BaseBVType 14
type instance A.OperandType AArch32 "Bv15" = BaseBVType 15
type instance A.OperandType AArch32 "Bv16" = BaseBVType 16
type instance A.OperandType AArch32 "Bv17" = BaseBVType 17
type instance A.OperandType AArch32 "Bv18" = BaseBVType 18
type instance A.OperandType AArch32 "Bv19" = BaseBVType 19
type instance A.OperandType AArch32 "Bv20" = BaseBVType 20
type instance A.OperandType AArch32 "Bv21" = BaseBVType 21
type instance A.OperandType AArch32 "Bv22" = BaseBVType 22
type instance A.OperandType AArch32 "Bv23" = BaseBVType 23
type instance A.OperandType AArch32 "Bv24" = BaseBVType 24

-- Psuedo-operands that have an expected value, but can be otherwise ignored
type instance A.OperandType AArch32 "QuasiMask1" = BaseBVType 1
type instance A.OperandType AArch32 "QuasiMask2" = BaseBVType 2
type instance A.OperandType AArch32 "QuasiMask3" = BaseBVType 3
type instance A.OperandType AArch32 "QuasiMask4" = BaseBVType 4
type instance A.OperandType AArch32 "QuasiMask5" = BaseBVType 5
type instance A.OperandType AArch32 "QuasiMask6" = BaseBVType 6
type instance A.OperandType AArch32 "QuasiMask7" = BaseBVType 7
type instance A.OperandType AArch32 "QuasiMask8" = BaseBVType 8
type instance A.OperandType AArch32 "QuasiMask9" = BaseBVType 9
type instance A.OperandType AArch32 "QuasiMask10" = BaseBVType 10
type instance A.OperandType AArch32 "QuasiMask11" = BaseBVType 11
type instance A.OperandType AArch32 "QuasiMask12" = BaseBVType 12
type instance A.OperandType AArch32 "QuasiMask13" = BaseBVType 13
type instance A.OperandType AArch32 "QuasiMask14" = BaseBVType 14
type instance A.OperandType AArch32 "QuasiMask15" = BaseBVType 15
type instance A.OperandType AArch32 "QuasiMask16" = BaseBVType 16

instance A.IsOperandTypeRepr AArch32 where
    type OperandTypeRepr AArch32 = ARMOperandRepr
    operandTypeReprSymbol _ (A32OperandRepr o) = A32.operandReprString o
    operandTypeReprSymbol _ (T32OperandRepr o) = T32.operandReprString o


operandValue :: forall sym s.
                (S.IsSymExprBuilder sym,
                 S.IsExprBuilder sym)
             => sym
             -> (forall tp. Location AArch32 tp -> IO (S.SymExpr sym tp))
             -> ARMOperand s
             -> IO (A.TaggedExpr AArch32 sym s)
operandValue sym locLookup op = TaggedExpr <$> opV op
  where opV :: ARMOperand s -> IO (A.AllocatedOperand AArch32 sym s)
        opV (A32Operand o) = opVa o
        opV (T32Operand o) = opVt o

        opVa :: A32.Operand s -> IO (A.AllocatedOperand AArch32 sym s)
        opVa o = case o of
          A32.Bv1 v -> A.ValueOperand <$> S.bvLit sym knownNat (toInteger $ W.unW v)
          unhandled -> error $ "operandValue not implemented for " <> show unhandled

        opVt :: T32.Operand s -> IO (A.AllocatedOperand AArch32 sym s)
        opVt o = case o of
          T32.Bv1 v -> A.ValueOperand <$> S.bvLit sym knownNat (toInteger $ W.unW v)
          unhandled -> error $ "operandValue not implemented for " <> show unhandled
 
-- In the ASL-translated semantics, operands never correspond to locations
operandToLocation :: ARMOperand s -> Maybe (Location AArch32 (A.OperandType AArch32 s))
operandToLocation _ = Nothing

-- ----------------------------------------------------------------------

instance (KnownNat (A.RegWidth arm), 1 <= A.RegWidth arm) =>
         A.IsLocation (Location arm) where

  isMemoryLocation loc = Some loc == Some locMem

  isIP loc = Some loc == Some locPC

  readLocation = readLoc

  locationType = locRepr

  defaultLocationExpr sym loc = reprToDefault sym (locRepr loc)

  nonMemLocations = filter (\gr -> gr /= Some locMem) $ FC.toListFC (Some . Location) ASL.allGlobalRefs
  memLocation = [AL.toMemLoc locMem]

  registerizationLocations = []

readLoc :: String -> Maybe (Some (Location arm))
readLoc str = do
  Some gr <- ASL.lookupGlobalRef str
  return $ Some $ Location gr

reprToDefault :: sym -> S.BaseTypeRepr tp -> IO (S.SymExpr sym tp)
reprToDefault sym repr = error "reprToDefault"


  -- do
  -- c <- P.lookAhead (P.anySingle)
  -- case c of
  --   'C' -> Some LocCPSR <$ P.string "CPSR"
  --   'M' -> Some LocMem <$ P.string "Mem"
  --   'P' -> Some LocPC <$ P.string "PC"
  --   'R' -> do
  --     parsePrefixedRegister (Some . LocGPR) 'R'
  --   _ -> do
  --     P.failure (Just $ P.Tokens $ (c:|[])) (Set.fromList $ [ P.Label $ fromList "Location" ])

parsePrefixedRegister :: (Word32 -> b) -> Char -> Parser b
parsePrefixedRegister f c = error ""

 -- do
 --  _ <- P.char c
 --  n <- P.decimal
 --  case n >= 0 && n <= (numGPR-1) of
 --    True -> return (f n)
 --    False -> P.failure (Just $ P.Tokens $ fromList $ show n)
 --                      (Set.fromList $ [ P.Label $ fromList $ "Register number 0-" <> show (numGPR-1) ])

-- ----------------------------------------------------------------------

-- ShowF (A.Operand ARM)
--       ShowF (A.Opcode ARM (A.Operand ARM))
--             OrdF (A.Opcode ARM (A.Operand ARM))
--                  (Data.EnumF.EnumF (A.Opcode ARM (A.Operand ARM)))

type instance A.RegWidth AArch32 = 32


instance A.Architecture AArch32 where
    data TaggedExpr AArch32 sym s = TaggedExpr (A.AllocatedOperand AArch32 sym s)
    unTagged (TaggedExpr te) =
      case te of
        A.ValueOperand se -> Just se
        A.LocationOperand _ se -> Just se
        A.CompoundOperand {} -> Nothing
    taggedOperand (TaggedExpr e) = e
    allocateSymExprsForOperand _ = operandValue
    operandToLocation _ = operandToLocation
    uninterpretedFunctions = UF.uninterpretedFunctions
    readMemUF = A.uninterpFnName . UF.mkReadMemUF @AArch32
    writeMemUF = A.uninterpFnName . UF.mkWriteMemUF @AArch32
    locationFuncInterpretation _proxy = A.createSymbolicEntries locationFuncInterpretation
    shapeReprToTypeRepr _proxy = shapeReprType
    operandComponentsImmediate = operandComponentsImmediate
    -- FIXME: architecture endianness is configurable, not sure how to represent this
    archEndianForm _ = A.LittleEndian

type instance A.OperandComponents AArch32 sym = OperandComponents AArch32 sym

data OperandComponents arch sym (s :: Symbol) where
  OCBv :: forall arch sym n s. S.SymExpr sym (S.BaseBVType n) -> OperandComponents arch sym s
  
instance (S.IsExpr (S.SymExpr sym), ShowF (A.Location arch)) => Show (OperandComponents arch sym s) where
  show oc =
    case oc of
      OCBv e -> unwords [ "OCBv"
                        , show (S.printSymExpr e)
                        ]
                
operandComponentsImmediate :: proxy sym -> OperandComponents arch sym s -> Maybe (Some (S.SymExpr sym))
operandComponentsImmediate _ (OCBv e) = Just . Some $ e

noLocation :: PL.List (A.AllocatedOperand arch sym) sh
           -> F.WrappedOperand arch sh s
           -> BaseTypeRepr tp
           -> Maybe (Location arch tp)
noLocation _ _ _ = Nothing

locationFuncInterpretation :: [(String, A.FunctionInterpretation t st fs AArch32)]
locationFuncInterpretation = []
    -- [ ("arm.is_r15", A.FunctionInterpretation
    --                    { A.locationInterp = F.LocationFuncInterp noLocation
    --                    , A.exprInterpName = 'interpIsR15
    --                    })
    -- ]
                             

shapeReprType :: forall tp . ARMOperandRepr tp -> BaseTypeRepr (A.OperandType AArch32 tp)
shapeReprType orep =
    case orep of
      A32OperandRepr a32rep ->
          case a32rep of
            A32.Bv1Repr -> knownRepr
            A32.Bv2Repr -> knownRepr
            A32.Bv3Repr -> knownRepr
            A32.Bv4Repr -> knownRepr
            A32.Bv5Repr -> knownRepr
            A32.Bv6Repr -> knownRepr
            A32.Bv7Repr -> knownRepr
            A32.Bv8Repr -> knownRepr
            A32.Bv9Repr -> knownRepr
            A32.Bv10Repr -> knownRepr
            A32.Bv11Repr -> knownRepr
            A32.Bv12Repr -> knownRepr
            A32.Bv13Repr -> knownRepr
            A32.Bv14Repr -> knownRepr
            A32.Bv15Repr -> knownRepr
            A32.Bv16Repr -> knownRepr
            A32.Bv17Repr -> knownRepr
            A32.Bv18Repr -> knownRepr
            A32.Bv19Repr -> knownRepr
            A32.Bv20Repr -> knownRepr
            A32.Bv21Repr -> knownRepr
            A32.Bv22Repr -> knownRepr
            A32.Bv23Repr -> knownRepr
            A32.Bv24Repr -> knownRepr
            A32.QuasiMask1Repr -> knownRepr
            A32.QuasiMask2Repr -> knownRepr
            A32.QuasiMask3Repr -> knownRepr
            A32.QuasiMask4Repr -> knownRepr
            A32.QuasiMask5Repr -> knownRepr
            A32.QuasiMask6Repr -> knownRepr
            A32.QuasiMask7Repr -> knownRepr
            A32.QuasiMask8Repr -> knownRepr
            A32.QuasiMask9Repr -> knownRepr
            A32.QuasiMask10Repr -> knownRepr
            A32.QuasiMask11Repr -> knownRepr
            A32.QuasiMask12Repr -> knownRepr
            A32.QuasiMask13Repr -> knownRepr
            A32.QuasiMask14Repr -> knownRepr
            A32.QuasiMask15Repr -> knownRepr
            A32.QuasiMask16Repr -> knownRepr

      T32OperandRepr t32rep ->
          case t32rep of
            T32.Bv1Repr -> knownRepr
            T32.Bv2Repr -> knownRepr
            T32.Bv3Repr -> knownRepr
            T32.Bv4Repr -> knownRepr
            T32.Bv5Repr -> knownRepr
            T32.Bv6Repr -> knownRepr
            T32.Bv7Repr -> knownRepr
            T32.Bv8Repr -> knownRepr
            T32.Bv9Repr -> knownRepr
            T32.Bv10Repr -> knownRepr
            T32.Bv11Repr -> knownRepr
            T32.Bv12Repr -> knownRepr
            T32.Bv13Repr -> knownRepr
            T32.Bv14Repr -> knownRepr
            T32.Bv15Repr -> knownRepr
            T32.Bv16Repr -> knownRepr
            T32.Bv17Repr -> knownRepr
            T32.Bv18Repr -> knownRepr
            T32.Bv19Repr -> knownRepr
            T32.Bv20Repr -> knownRepr
            T32.Bv21Repr -> knownRepr
            T32.Bv22Repr -> knownRepr
            T32.Bv23Repr -> knownRepr
            T32.Bv24Repr -> knownRepr
            T32.QuasiMask1Repr -> knownRepr
            T32.QuasiMask2Repr -> knownRepr
            T32.QuasiMask3Repr -> knownRepr
            T32.QuasiMask4Repr -> knownRepr
            T32.QuasiMask5Repr -> knownRepr
            T32.QuasiMask6Repr -> knownRepr
            T32.QuasiMask7Repr -> knownRepr
            T32.QuasiMask8Repr -> knownRepr
            T32.QuasiMask9Repr -> knownRepr
            T32.QuasiMask10Repr -> knownRepr
            T32.QuasiMask11Repr -> knownRepr
            T32.QuasiMask12Repr -> knownRepr
            T32.QuasiMask13Repr -> knownRepr
            T32.QuasiMask14Repr -> knownRepr
            T32.QuasiMask15Repr -> knownRepr
            T32.QuasiMask16Repr -> knownRepr


-- ----------------------------------------------------------------------

-- data Signed = Signed | Unsigned deriving (Eq, Show)


-- type instance A.OperandComponents AArch32 sym = AOC.OperandComponents AArch32 sym

-- instance T.TemplatableOperand AArch32 where
--   opTemplates sr =
--       case sr of
--         (A32OperandRepr a) -> a32template a
--         (T32OperandRepr a) -> t32template a

-- a32template :: ARMDis.OperandRepr s -> [T.TemplatedOperand AArch32 s]
-- a32template a32sr =
--     case a32sr of
--       ARMDis.Addrmode_imm12Repr ->
--           mkTemplate <$> [0..numGPR-1]
--               where mkTemplate gprNum = T.TemplatedOperand Nothing
--                                         (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
--                                             :: T.TemplatedOperand AArch32 "Addrmode_imm12"
--                         where mkTemplate' :: forall sym
--                                            . (S.IsSymExprBuilder sym)
--                                           => sym
--                                           -> (forall tp . Location AArch32 tp -> IO (S.SymExpr sym tp))
--                                           -> IO (A.AllocatedOperand AArch32 sym "Addrmode_imm12",
--                                                  T.RecoverOperandFn sym (A.Operand AArch32 "Addrmode_imm12"))
--                               mkTemplate' sym locLookup = do
--                                 let gprN = ARMOperands.gpr $ fromIntegral gprNum
--                                 let loc = LocGPR gprNum
--                                 base <- locLookup loc
--                                 offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_off") knownRepr
--                                 addflag <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_add") knownRepr
--                                 let recover :: (forall tp . S.SymExpr sym tp -> IO (WE.GroundValue tp)) -> IO (A.Operand AArch32 "Addrmode_imm12")
--                                     recover evalFn = do
--                                       offsetVal <- fromInteger <$> evalFn offset
--                                       addflagVal <- fromInteger <$> evalFn addflag
--                                       return $ A32Operand $ ARMDis.Addrmode_imm12 $
--                                              ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
--                                 return ( A.CompoundOperand (AOC.OCAddrmodeImm12 loc base offset addflag)
--                                        , T.RecoverOperandFn recover
--                                        )
--       ARMDis.Addrmode_imm12_preRepr ->
--           mkTemplate <$> [0..numGPR-1]
--             where mkTemplate gprNum = T.TemplatedOperand Nothing
--                                       (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
--                                           :: T.TemplatedOperand AArch32 "Addrmode_imm12_pre"
--                     where mkTemplate' :: forall sym
--                                        . (S.IsSymExprBuilder sym)
--                                       => sym
--                                       -> (forall tp . Location AArch32 tp -> IO (S.SymExpr sym tp))
--                                       -> IO (A.AllocatedOperand AArch32 sym "Addrmode_imm12_pre",
--                                              T.RecoverOperandFn sym (A.Operand AArch32 "Addrmode_imm12_pre"))
--                           mkTemplate' sym locLookup = do
--                             let gprN = ARMOperands.gpr $ fromIntegral gprNum
--                             let loc = LocGPR gprNum
--                             base <- locLookup loc
--                             offset <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_off") knownRepr
--                             addflag :: S.SymExpr sym (BaseBVType 1)
--                                     <- S.freshConstant sym (U.makeSymbol "Addrmode_imm12_pre_add") knownRepr
--                             let recover :: (forall tp . S.SymExpr sym tp -> IO (WE.GroundValue tp)) -> IO (A.Operand AArch32 "Addrmode_imm12_pre")
--                                 recover evalFn = do
--                                   offsetVal <- fromInteger <$> evalFn offset
--                                   addflagVal <- fromInteger <$> evalFn addflag
--                                   return $ A32Operand $ ARMDis.Addrmode_imm12_pre $
--                                          ARMOperands.AddrModeImm12 gprN offsetVal addflagVal
--                             return ( A.CompoundOperand (AOC.OCAddrmodeImm12 loc base offset addflag)
--                                    , T.RecoverOperandFn recover
--                                    )
--       ARMDis.Arm_bl_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
--       ARMDis.Arm_blx_targetRepr -> error "opTemplate ARM_blx_targetRepr TBD"
--       ARMDis.Arm_br_targetRepr -> error "opTemplate ARM_br_targetRepr TBD"
--       ARMDis.Cc_outRepr -> error "opTemplate ARM_Cc_outRepr TBD"
--       ARMDis.GPRRepr -> concreteTemplatedOperand (A32Operand . ARMDis.GPR . ARMOperands.gpr . fromIntegral) LocGPR <$> [0..numGPR-1]
--       ARMDis.Mod_immRepr -> error "opTemplate ARM_Mod_immRepr TBD"
--       ARMDis.PredRepr -> [symbolicTemplatedOperand (Proxy @4) Unsigned "Pred"
--                           (A32Operand . ARMDis.Pred . ARMDis.mkPred . fromInteger)]
--       ARMDis.Shift_so_reg_immRepr -> error "opTemplate Shift_so_reg_immRepr TBD"
--       ARMDis.So_reg_immRepr -> error "opTemplate So_reg_immRepr TBD"
--       -- ARMDis.So_reg_regRepr ->
--       --     mkTemplate <$> [0..numGPR-1]
--       --       where mkTemplate gprNum = T.TemplatedOperand Nothing
--       --                                 (Set.singleton (Some (LocGPR gprNum))) mkTemplate'
--       --                                     :: T.TemplatedOperand ARM "So_reg_reg"
--       --               where mkTemplate' :: T.TemplatedOperandFn ARM "So_reg_reg"
--       --                     mkTemplate' sym locLookup = do
--       --                       let gprN = ARMOperands.gpr gprNum
--       --                       base <- A.unTagged <$> A.operandValue (Proxy @ARM) sym locLookup (ARMDis.GPR $ gprN)
--       --                       offset <- S.freshConstant sym (U.makeSymbol "So_reg_reg_shift") knownRepr
--       --                       expr <- S.bvAdd sym offset offset -- KWQ!
--       --                       let recover evalFn = do
--       --                             offsetVal <- fromInteger <$> evalFn offset
--       --                             return $ ARMDis.So_reg_reg $ ARMOperands.SoRegReg gprN gprN offsetVal
--       --                       return (expr, T.RecoverOperandFn recover)
--       ARMDis.UnpredictableRepr -> error "opTemplate ARM_UnpredictableRepr TBD... and are you sure?"

-- t32template :: ThumbDis.OperandRepr s -> [T.TemplatedOperand AArch32 s]
-- t32template t32sr =
--     case t32sr of
--       _ -> error "opTemplate T32 ?? TBD"



-- concreteTemplatedOperand :: forall arch s a.
--                             (A.Architecture arch)
--                          => (a -> A.Operand arch s)
--                          -> (a -> A.Location arch (A.OperandType arch s))
--                          -> a
--                          -> T.TemplatedOperand arch s
-- concreteTemplatedOperand op loc x =
--   T.TemplatedOperand { T.templOpLocation = Just (loc x)
--                      , T.templUsedLocations = Set.singleton (Some (loc x))
--                      , T.templOpFn = mkTemplate'
--                      }
--   where mkTemplate' :: T.TemplatedOperandFn arch s
--         mkTemplate' sym locLookup = do
--           ao <- A.taggedOperand <$> A.allocateSymExprsForOperand (Proxy @arch) sym locLookup (op x)
--           return (ao, T.RecoverOperandFn $ const (return (op x)))


-- symbolicTemplatedOperand :: forall arch s (bits :: Nat)
--                           . (A.OperandType arch s ~ BaseBVType bits,
--                              KnownNat bits,
--                              1 <= bits)
--                          => Proxy bits
--                          -> Signed
--                          -> String
--                          -> (Integer -> A.Operand arch s)
--                          -> T.TemplatedOperand arch s
-- symbolicTemplatedOperand Proxy _signed name constr =
--   T.TemplatedOperand { T.templOpLocation = Nothing
--                      , T.templUsedLocations = Set.empty
--                      , T.templOpFn = mkTemplate'
--                      }
--   where mkTemplate' :: T.TemplatedOperandFn arch s
--         mkTemplate' sym _ = do
--           v <- S.freshConstant sym (U.makeSymbol name) (knownRepr :: BaseTypeRepr (BaseBVType bits))
--           let recover evalFn = constr <$> evalFn v
--           return (A.ValueOperand v, T.RecoverOperandFn recover)
