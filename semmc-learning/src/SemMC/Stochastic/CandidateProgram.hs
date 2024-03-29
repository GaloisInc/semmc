{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Data.Parameterized.Seq as SeqF

import qualified Dismantle.Instruction as D

import qualified Lang.Crucible.Backend as CB
import           What4.Protocol.Online ( OnlineSolver )

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Pseudo as AP
import qualified SemMC.Formula as F
import           SemMC.Symbolic ( Sym, Backend )

import           SemMC.Stochastic.Monad
import qualified SemMC.Util as U

data CandidateProgram t fs arch =
  CandidateProgram { cpInstructions :: [AP.SynthInstruction arch]
                   , cpFormula :: F.Formula (Sym t fs) arch
                   }

deriving instance (SynC arch) => Show (CandidateProgram t fs arch)

-- | Convert an instruction into a 'F.Formula'
instructionFormula :: forall arch t solver fs .
                      (SynC arch, CB.IsSymInterface (Sym t fs), OnlineSolver solver)
                   => Backend solver t fs
                   -> AP.SynthInstruction arch
                   -> Syn solver t fs arch (F.Formula (Sym t fs) arch)
instructionFormula bak i = do
  case i of
    AP.SynthInstruction sop operands -> do
      case sop of
        AP.RealOpcode op -> realInstructionFormula Nothing op operands
        AP.PseudoOpcode op -> pseudoInstructionFormula op operands
  where
    sym = CB.backendGetSym bak

    -- The @mop :: Maybe PseudoOpcode@ is used to improve the error
    -- message in the case where the real opcode arose from the
    -- assembly of a pseudo opcode.
    realInstructionFormula :: Maybe (AP.Pseudo arch (A.Operand arch) sh')
                           -> A.Opcode arch (A.Operand arch) sh
                           -> SL.List (A.Operand arch) sh
                           -> Syn solver t fs arch (F.Formula (Sym t fs) arch)
    realInstructionFormula mop op operands = do
      mpf <- MapF.lookup op <$> askFormulas
      pf <- maybe (die mop op) return mpf
      (_, f) <- liftIO $ F.instantiateFormula bak pf operands
      return f

    pseudoInstructionFormula :: AP.Pseudo arch (A.Operand arch) sh
                             -> SL.List (A.Operand arch) sh
                             -> Syn solver t fs arch (F.Formula (Sym t fs) arch)
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
          (_, f) <- liftIO $ F.instantiateFormula bak pFormula operands
          return f
        -- Compute the formula for the pseudo op from the formulas for the
        -- underlying real ops.
        Nothing -> do
          let realInstrs = AP.assemblePseudo (Proxy @arch) op operands
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
programFormula :: (SynC arch, CB.IsSymInterface (Sym t fs), OnlineSolver solver)
               => Backend solver t fs
               -> [AP.SynthInstruction arch]
               -> Syn solver t fs arch (F.Formula (Sym t fs) arch)
programFormula bak insns = do
  let sym = CB.backendGetSym bak
  fs <- mapM (instructionFormula bak) insns
  liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula fs

lookupCongruentOpcodes :: (HasRepr (A.Opcode arch (A.Operand arch)) (A.ShapeRepr arch),
                           HasRepr (AP.Pseudo arch (A.Operand arch)) (A.ShapeRepr arch),
                           MapF.OrdF (A.OperandTypeRepr arch))
                       => AP.SynthOpcode arch sh
                       -> Syn solver t fs arch (Seq.Seq (AP.SynthOpcode arch sh))
lookupCongruentOpcodes op = maybe Seq.empty SeqF.unSeqF . MapF.lookup (typeRepr op) <$> askKnownCongruentOps

