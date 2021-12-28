{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module SemMC.Architecture.PPC.Pseudo (
  PseudoOpcode(..),
  ppcAssemblePseudo
  ) where

import           GHC.TypeLits ( Symbol )

import           Data.Kind ( Type )
import qualified Data.Word.Indexed as W
import           Data.Parameterized.Classes
import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.TH.GADT as TH

import qualified Dismantle.Instruction.Random as D
import qualified Dismantle.PPC as PPC
import qualified Dismantle.Instruction as D
import           Dismantle.PPC.Random ()

import qualified SemMC.Architecture as A

data PseudoOpcode :: (Symbol -> Type) -> [Symbol] -> Type where
  -- | @ReplaceByteGPR rA, n, rB@ replaces the @n@th byte of @rA@ with the low
  -- byte of @rB@.
  ReplaceByteGPR :: PseudoOpcode PPC.Operand '["Gprc", "U2imm", "Gprc"]
  -- | @ExtractByteGPR rA, rB, n@ extracts the @n@th byte of @rB@ into the low
  -- byte of @rA@, zero-extending it.
  ExtractByteGPR :: PseudoOpcode PPC.Operand '["Gprc", "Gprc", "U2imm"]
  -- | @ReplaceWordVR vrA, n, rB@ replaces the @n@th word of @vrA@ with the
  -- value of @rB@.
  ReplaceWordVR :: PseudoOpcode PPC.Operand '["Vrrc", "U2imm", "Gprc"]
  -- | @ExtractWordVR rA, vrB, n@ extracts the @n@th word of @vrB@ into @rA@.
  ExtractWordVR :: PseudoOpcode PPC.Operand '["Gprc", "Vrrc", "U2imm"]
  -- | A simple register to register move
  --
  -- This is useful, because the idiomatic way to do a MOV in PPC is @ADDI
  -- rDest, rSrc, 0@, which is difficult for the synthesis to come up with
  -- randomly.
  Move :: PseudoOpcode PPC.Operand '["Gprc",  "Gprc_nor0"]
  -- | Set the CR0 field based on the value in a register (the value is compared
  -- against zero and part of the XER is pulled into CR0).
  --
  -- The comparison is signed, as this pseudo-op is designed to learn the @.@
  -- variants of instructions which have an implicit signed comparison against
  -- zero that populates CR0.
  SetSignedCR0 :: PseudoOpcode PPC.Operand '["Gprc"]

deriving instance Show (PseudoOpcode op sh)

instance ShowF (PseudoOpcode op)

$(return [])

instance TestEquality (PseudoOpcode op) where
  testEquality = $(TH.structuralTypeEquality [t| PseudoOpcode |] [])

instance OrdF (PseudoOpcode op) where
  compareF = $(TH.structuralTypeOrd [t| PseudoOpcode |] [])

instance HasRepr (PseudoOpcode op) (SL.List PPC.OperandRepr) where
  typeRepr ReplaceByteGPR = knownRepr
  typeRepr ExtractByteGPR = knownRepr
  typeRepr ReplaceWordVR = knownRepr
  typeRepr ExtractWordVR = knownRepr
  typeRepr Move = knownRepr
  typeRepr SetSignedCR0 = knownRepr

instance D.ArbitraryOperands PseudoOpcode PPC.Operand where
  arbitraryOperands gen op = case op of
    ReplaceByteGPR -> D.arbitraryShapedList gen
    ExtractByteGPR -> D.arbitraryShapedList gen
    ReplaceWordVR  -> D.arbitraryShapedList gen
    ExtractWordVR  -> D.arbitraryShapedList gen
    Move           -> D.arbitraryShapedList gen
    SetSignedCR0   -> D.arbitraryShapedList gen

-- | An assembler for pseudo-instructions.
--
-- While it mentions @arch@, the type functions we use are the same for both
-- PPC32 and PPC64, so we can share this function.
ppcAssemblePseudo :: (A.Opcode arch ~ PPC.Opcode, A.Operand arch ~ PPC.Operand)
                  => proxy arch
                  -> PseudoOpcode op sh
                  -> SL.List op sh
                  -> [A.Instruction arch]
ppcAssemblePseudo _proxy opcode oplist =
  case opcode of
    ReplaceByteGPR ->
      case (oplist :: SL.List PPC.Operand '["Gprc", "U2imm", "Gprc"]) of
        (target SL.:< PPC.U2imm (W.unW -> n) SL.:< source SL.:< SL.Nil) ->
          let n' :: W.W 5 = fromIntegral n
          in [ D.Instruction PPC.RLWIMI ( target SL.:<
                                          PPC.U5imm (n' * 8 + 7) SL.:<
                                          PPC.U5imm (n' * 8) SL.:<
                                          PPC.U5imm (n' * 8) SL.:<
                                          source SL.:<
                                          SL.Nil
                                        )
             ]
    ExtractByteGPR ->
      case (oplist :: SL.List PPC.Operand '["Gprc", "Gprc", "U2imm"]) of
        (target SL.:< source SL.:< PPC.U2imm (W.unW -> n) SL.:< SL.Nil) ->
          let n' :: W.W 5 = fromIntegral n
          in [ D.Instruction PPC.RLWINM ( target SL.:<
                                          PPC.U5imm 31 SL.:<
                                          PPC.U5imm (0 - n') SL.:<
                                          PPC.U5imm (8 + n') SL.:<
                                          source SL.:<
                                          SL.Nil
                                        )
             ]
    ReplaceWordVR ->
      case (oplist :: SL.List PPC.Operand '["Vrrc", "U2imm", "Gprc"]) of
        (target SL.:< PPC.U2imm (W.unW -> n) SL.:< source SL.:< SL.Nil) ->
          -- Assumes there's a free chunk of memory pointed to by R31.
          let vrLocation = PPC.Memrr (PPC.MemRR Nothing (PPC.GPR 31))
              gprWriteLocation = PPC.Memri (PPC.MemRI (Just (PPC.GPR 31)) (fromIntegral (n * 4)))
          in [ -- First, store the current contents of the target into memory.
               D.Instruction PPC.STVX ( vrLocation SL.:<
                                        target SL.:<
                                        SL.Nil
                                      )
             , -- Next, write the GPR into the appropriate spot.
               D.Instruction PPC.STW ( gprWriteLocation SL.:<
                                       source SL.:<
                                       SL.Nil
                                     )
             , -- Finally, read the target back from memory.
               D.Instruction PPC.LVX ( target SL.:<
                                       vrLocation SL.:<
                                       SL.Nil
                                     )
             ]
    ExtractWordVR ->
      case (oplist :: SL.List PPC.Operand '["Gprc", "Vrrc", "U2imm"]) of
        (target SL.:< source SL.:< PPC.U2imm (W.unW -> n) SL.:< SL.Nil) ->
          -- Assumes there's a free chunk of memory pointed to by R31.
          let vrLocation = PPC.Memrr (PPC.MemRR Nothing (PPC.GPR 31))
              gprReadLocation = PPC.Memri (PPC.MemRI (Just (PPC.GPR 31)) (fromIntegral (n * 4)))
          in [ -- First, write the contents of the vector register into memory.
               D.Instruction PPC.STVX ( vrLocation SL.:<
                                        source SL.:<
                                        SL.Nil
                                      )
             , -- Then, read the GPR from an offset into that saved register.
               D.Instruction PPC.LWZ ( target SL.:<
                                       gprReadLocation SL.:<
                                       SL.Nil
                                     )
             ]
    Move ->
      case (oplist :: SL.List PPC.Operand '["Gprc", "Gprc_nor0"]) of
        (target SL.:< source SL.:< SL.Nil) ->
          [ D.Instruction PPC.ADDI ( target SL.:< PPC.S16imm 0 SL.:< source SL.:< SL.Nil ) ]

    SetSignedCR0 ->
      case (oplist :: SL.List PPC.Operand '["Gprc"]) of
        (source SL.:< SL.Nil) ->
          [ D.Instruction PPC.CMPDI ( PPC.Crrc (PPC.CRRC 0) SL.:< PPC.S16imm64 0 SL.:< source SL.:< SL.Nil ) ]
