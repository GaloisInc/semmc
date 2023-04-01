{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Synthesis.Testing (
  synthesizeAndCheck,
  SynthResult(..)
  ) where

import qualified Data.Foldable as F
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Map as MapF
import qualified What4.Expr.Builder as WE
import qualified What4.Protocol.Online as WPO
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.LLVM.MemModel as LLVM

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

synthesizeAndCheck :: forall proxy arch sym solver t st fs
                    . ( A.Architecture arch
                      , A.Architecture (SS.TemplatedArch arch)
                      , A.ArchRepr arch
                      , SS.TemplatableOperand arch
                      , sym ~ WE.ExprBuilder t st fs
                      , WPO.OnlineSolver solver
                      , CB.IsSymInterface sym
                      , ?memOpts :: LLVM.MemOptions
                      )
                   => proxy arch
                   -> CBO.OnlineBackend solver t st fs
                   -> SS.SynthesisEnvironment sym arch
                   -> MapF.MapF (A.Opcode arch (A.Operand arch)) (SF.ParameterizedFormula sym arch)
                   -> (forall a . A.Instruction arch -> (forall sh . A.Opcode arch (A.Operand arch) sh -> PL.List (A.Operand arch) sh -> a) -> a)
                   -> [A.Instruction arch]
                   -> IO SynthResult
synthesizeAndCheck proxy bak env sem matchInsn p = do
  print $ "Synthesizing program from "
  putStrLn $ show p
  mfrm <- symbolicallySimulateProgram proxy bak sem matchInsn p
  case mfrm of
    Just frm -> do
      print $ "Obtained formula from program"
      putStrLn $ show frm
      mp <- SS.mcSynth bak env frm
      case mp of
        Nothing -> return FailedSynthesis
        Just p' -> do
          print $ "Obtained synthesized program "
          putStrLn $ show p'
          mfrm' <- symbolicallySimulateProgram proxy bak sem matchInsn p'
          case mfrm' of
            Nothing -> return MissingSemantics
            Just frm' -> do
              print $ "Obtained formula from new program"
              er <- SF.formulasEquivConcrete bak (SF.formStripIP frm) (SF.formStripIP frm')
              case er of
                SF.Equivalent -> return Equivalent
                SF.DifferentBehavior {} -> return BadSemantics
                SF.Timeout -> return Timeout
    Nothing -> return MissingSemantics

symbolicallySimulateProgram :: forall proxy sym solver t st fs arch
                             . ( A.Architecture arch
                               , WPO.OnlineSolver solver
                               , sym ~ WE.ExprBuilder t st fs
                               , CB.IsSymInterface sym
                               )
                            => proxy arch
                            -> CBO.OnlineBackend solver t st fs
                            -> MapF.MapF (A.Opcode arch (A.Operand arch)) (SF.ParameterizedFormula sym arch)
                            -> (forall a . A.Instruction arch -> (forall sh . A.Opcode arch (A.Operand arch) sh -> PL.List (A.Operand arch) sh -> a) -> a)
                            -> [A.Instruction arch]
                            -> IO (Maybe (SF.Formula sym arch))
symbolicallySimulateProgram _ bak sem matchInsn p = do
  let sym = CB.backendGetSym bak
  mfs <- mapM toSemantics p
  case sequence mfs of
    Nothing -> return Nothing
    Just fs -> Just <$> F.foldrM (SF.sequenceFormulas sym) SF.emptyFormula fs
  where
    toSemantics :: A.Instruction arch -> IO (Maybe (SF.Formula sym arch))
    toSemantics i = matchInsn i $ \opc operands -> do
      case MapF.lookup opc sem of
        Nothing -> return Nothing
        Just pf -> do res <- (Just . snd) <$> SF.instantiateFormula bak pf operands
                      return res
