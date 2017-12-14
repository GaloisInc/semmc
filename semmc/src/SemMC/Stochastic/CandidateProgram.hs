{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module SemMC.Stochastic.CandidateProgram (
  CandidateProgram(..),
  programFormula,
  lookupCongruentOpcodes,
  lookupFormula,
  instantiateFormula
  ) where

import           Control.Monad.Trans ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.HasRepr ( HasRepr(..) )
import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.SymbolRepr as SR

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

data CandidateProgram t arch =
  CandidateProgram { cpInstructions :: [SynthInstruction arch]
                   , cpFormula :: F.Formula (Sym t) arch
                   }

deriving instance (SynC arch) => Show (CandidateProgram t arch)

-- | Convert an instruction into a 'F.Formula'
instructionFormula :: (ArchitectureWithPseudo arch)
                   => Sym t
                   -> SynthInstruction arch
                   -> Syn t arch (F.Formula (Sym t) arch)
instructionFormula sym i = do
  case i of
    SynthInstruction op operands -> do
      Just pf <- lookupFormula op
      (_, f) <- liftIO $ F.instantiateFormula sym pf operands
      return f

-- | Convert a program into a formula
programFormula :: (ArchitectureWithPseudo arch)
               => Sym t
               -> [SynthInstruction arch]
               -> Syn t arch (F.Formula (Sym t) arch)
programFormula sym insns = do
  fs <- mapM (instructionFormula sym) insns
  liftIO $ F.foldlM (F.sequenceFormulas sym) F.emptyFormula fs


lookupFormula :: (ArchitectureWithPseudo arch)
              => SynthOpcode arch sh
              -> Syn t arch (Maybe (F.ParameterizedFormula (Sym t) arch sh))
lookupFormula (RealOpcode op) = MapF.lookup op <$> askFormulas
lookupFormula (PseudoOpcode pseudo) = MapF.lookup pseudo <$> askPseudoFormulas

lookupCongruentOpcodes :: (HasRepr (A.Opcode arch (A.Operand arch)) (SL.List SR.SymbolRepr),
                           HasRepr (Pseudo arch (A.Operand arch)) (SL.List SR.SymbolRepr))
                       => SynthOpcode arch sh
                       -> Syn t arch (Seq.Seq (SynthOpcode arch sh))
lookupCongruentOpcodes op = maybe Seq.empty SeqF.unSeqF . MapF.lookup (typeRepr op) <$> askKnownCongruentOps

instantiateFormula :: (ArchitectureWithPseudo arch)
                   => SynthInstruction arch
                   -> Syn t arch (Maybe (F.Formula (Sym t) arch))
instantiateFormula (SynthInstruction opcode oplist) = do
  mpf <- lookupFormula opcode
  case mpf of
    Nothing -> return Nothing
    Just pf -> do
      withSymBackend $ \sym -> do
        (_, f) <- liftIO $ F.instantiateFormula sym pf oplist
        return (Just f)
