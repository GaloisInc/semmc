{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module SemMC.Synthesis.Testing (
  synthesizeAndCheck,
  SynthResult(..)
  ) where

import qualified Data.Foldable as F
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Map as MapF
import qualified What4.Protocol.Online as WPO
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Backend as CB

import qualified SemMC.Architecture as A
import qualified SemMC.Formula as SF

import qualified SemMC.Synthesis as SS

data SynthResult = Equivalent
                 -- ^ The synthesized program is equivalent
                 | BadSemantics
                 -- ^ The synthesized program does not match the semantics of the input
                 | FailedSynthesis
                 -- ^ The synthesis failed to find a program
                 | MissingSemantics
                 -- ^ Semantics for an instruction were not found
                 | Timeout
                 deriving (Show, Eq)

synthesizeAndCheck :: forall proxy arch t solver fs
                    . ( A.Architecture arch
                      , A.Architecture (SS.TemplatedArch arch)
                      , A.ArchRepr arch
                      , WPO.OnlineSolver t solver
                      , SS.TemplatableOperand arch
                      , CB.IsSymInterface (CBO.OnlineBackend t solver fs)
                      )
                   => proxy arch
                   -> SS.SynthesisEnvironment (CBO.OnlineBackend t solver fs) arch
                   -> MapF.MapF (A.Opcode arch (A.Operand arch)) (SF.ParameterizedFormula (CBO.OnlineBackend t solver fs) arch)
                   -> (forall a . A.Instruction arch -> (forall sh . A.Opcode arch (A.Operand arch) sh -> PL.List (A.Operand arch) sh -> a) -> a)
                   -> [A.Instruction arch]
                   -> IO SynthResult
synthesizeAndCheck proxy env sem matchInsn p = do
  print $ "Synthesizing program from "
  putStrLn $ show p
  mfrm <- symbolicallySimulateProgram proxy (SS.synthSym env) sem matchInsn p
  case mfrm of
    Just frm -> do
      print $ "Obtained formula from program"
      putStrLn $ show frm
      mp <- SS.mcSynth env frm
      case mp of
        Nothing -> return FailedSynthesis
        Just p' -> do
          print $ "Obtained synthesized program "
          putStrLn $ show p'
          mfrm' <- symbolicallySimulateProgram proxy (SS.synthSym env) sem matchInsn p'
          case mfrm' of
            Nothing -> return MissingSemantics
            Just frm' -> do
              print $ "Obtained formula from new program"
              er <- SF.formulasEquivConcrete (SS.synthSym env) (SF.formStripIP frm) (SF.formStripIP frm')
              case er of
                SF.Equivalent -> return Equivalent
                SF.DifferentBehavior {} -> return BadSemantics
                SF.Timeout -> return Timeout
    Nothing -> return MissingSemantics

symbolicallySimulateProgram :: forall proxy t solver fs arch
                             . ( A.Architecture arch
                               , WPO.OnlineSolver t solver
                               )
                            => proxy arch
                            -> CBO.OnlineBackend t solver fs
                            -> MapF.MapF (A.Opcode arch (A.Operand arch)) (SF.ParameterizedFormula (CBO.OnlineBackend t solver fs) arch)
                            -> (forall a . A.Instruction arch -> (forall sh . A.Opcode arch (A.Operand arch) sh -> PL.List (A.Operand arch) sh -> a) -> a)
                            -> [A.Instruction arch]
                            -> IO (Maybe (SF.Formula (CBO.OnlineBackend t solver fs) arch))
symbolicallySimulateProgram _ sym sem matchInsn p = do
  mfs <- mapM toSemantics p
  case sequence mfs of
    Nothing -> return Nothing
    Just fs -> Just <$> F.foldrM (SF.sequenceFormulas sym) SF.emptyFormula fs
  where
    toSemantics :: A.Instruction arch -> IO (Maybe (SF.Formula (CBO.OnlineBackend t solver fs) arch))
    toSemantics i = matchInsn i $ \opc operands -> do
      case MapF.lookup opc sem of
        Nothing -> return Nothing
        Just pf -> do res <- (Just . snd) <$> SF.instantiateFormula sym pf operands
                      return res
