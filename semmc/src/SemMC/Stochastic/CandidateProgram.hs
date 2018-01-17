{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Stochastic.CandidateProgram (
  CandidateProgram(..),
  programFormula,
  lookupCongruentOpcodes
  ) where

import           Control.Monad
import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import           Data.Proxy
import qualified Data.Sequence as Seq
import           Text.Printf

import qualified UnliftIO as U

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.List as SL

import qualified Dismantle.Instruction as D

import qualified Data.Parameterized.Seq as SeqF
import qualified SemMC.Architecture as A
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym )

import           SemMC.Stochastic.Monad
import           SemMC.Stochastic.Pseudo
                 ( ArchitectureWithPseudo(..)
                 , Pseudo
                 , SynthOpcode(..)
                 , SynthInstruction(..)
                 )
import qualified SemMC.Util as U

data CandidateProgram t arch =
  CandidateProgram { cpInstructions :: [SynthInstruction arch]
                   , cpFormula :: F.Formula (Sym t) arch
                   }

deriving instance (SynC arch) => Show (CandidateProgram t arch)

-- | Convert an instruction into a 'F.Formula'
instructionFormula :: forall arch t .
                      (SynC arch)
                   => Sym t
                   -> SynthInstruction arch
                   -> Syn t arch (F.Formula (Sym t) arch)
instructionFormula sym i = do
  case i of
    SynthInstruction sop operands -> do
      case sop of
        RealOpcode op -> realInstructionFormula Nothing op operands
        PseudoOpcode op -> pseudoInstructionFormula op operands
  where
    -- The @mop :: Maybe PseudoOpcode@ is used to improve the error
    -- message in the case where the real opcode arose from the
    -- assembly of a pseudo opcode.
    realInstructionFormula :: Maybe (Pseudo arch (A.Operand arch) sh')
                           -> A.Opcode arch (A.Operand arch) sh
                           -> SL.List (A.Operand arch) sh
                           -> Syn t arch (F.Formula (Sym t) arch)
    realInstructionFormula mop op operands = do
      mpf <- MapF.lookup op <$> askFormulas
      pf <- maybe (die mop op) return mpf
      (_, f) <- liftIO $ F.instantiateFormula sym pf operands
      return f

    pseudoInstructionFormula :: Pseudo arch (A.Operand arch) sh
                             -> SL.List (A.Operand arch) sh
                             -> Syn t arch (F.Formula (Sym t) arch)
    pseudoInstructionFormula op operands = do
      -- Some pseudo ops have handwritten generic (parameterized) .sem
      -- files on disk, so we try looking those up first. If there are
      -- no hand written generic semantics, then here we compute
      -- concrete semantics for the specific instantiation of the
      -- pseudo op, via the concrete semantics of the underlying real
      -- ops we get by assembling the pseudo. A better approach to
      -- handling these psuedo ops without handwritten generic
      -- semantics would be to /precompute/ the /generic/ semantics
      -- for these pseudo ops at start up. However, doing that would
      -- presumably require some refactoring of
      -- @Instantiate.instantiateInstruction@ to handle psuedo ops,
      -- which would in turn require having iorels for pseudo ops.
      mCachedFormula <- MapF.lookup op <$> askPseudoFormulas
      case mCachedFormula of
        Just pFormula -> do
          (_, f) <- liftIO $ F.instantiateFormula sym pFormula operands
          return f
        -- Compute the formula for the pseudo op from the formulas for the
        -- underlying real ops.
        Nothing -> do
          let realInstrs = assemblePseudo (Proxy @arch) op operands
          fs <- forM realInstrs $ \ (D.Instruction o os) ->
            realInstructionFormula (Just op) o os
          liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula fs

    die mop op = do
      let epilogue = case mop of
            Nothing -> ""
            Just pop -> printf ", in assembly of pseudo op = %s" (MapF.showF pop)
      let msg = printf "No formula for op = %s%s" (MapF.showF op) epilogue
      U.logM U.Error msg
      U.throwIO (userError msg)

-- | Convert a program into a formula
programFormula :: (SynC arch)
               => Sym t
               -> [SynthInstruction arch]
               -> Syn t arch (F.Formula (Sym t) arch)
programFormula sym insns = do
  fs <- mapM (instructionFormula sym) insns
  liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula fs

lookupCongruentOpcodes :: (HasRepr (A.Opcode arch (A.Operand arch)) (A.ShapeRepr arch),
                           HasRepr (Pseudo arch (A.Operand arch)) (A.ShapeRepr arch),
                           MapF.OrdF (A.OperandTypeRepr arch))
                       => SynthOpcode arch sh
                       -> Syn t arch (Seq.Seq (SynthOpcode arch sh))
lookupCongruentOpcodes op = maybe Seq.empty SeqF.unSeqF . MapF.lookup (typeRepr op) <$> askKnownCongruentOps

