{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
module SemMC.Synthesis.Core
  ( synthesizeFormula
  , SynthesisEnvironment(..)
  , SynthesisParams(..)
  ) where

import           Control.Arrow ( (&&&) )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Parameterized.Some ( Some(..), viewSome )
import           Data.Parameterized.Classes (ShowF(..) )
import qualified Data.Sequence as Seq
import           Data.Foldable
import qualified Data.Set as Set
import           Data.Typeable

import qualified What4.Protocol.Online as WPO
import qualified What4.Interface as S
import qualified What4.Expr.Builder as WE
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.LLVM.MemModel as LLVM

import           SemMC.Architecture
import           SemMC.Formula
import           SemMC.Synthesis.Template
import           SemMC.Synthesis.Cegis
import           SemMC.Util
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen ( (<+>) )


-- NOTE: This initial implementation is not meant to be at all fast; it is just
-- a proof of concept.


-- Algorithm Synthesize, from [Srinivasan and Reps 2015]:

data SynthesisEnvironment sym arch =
  SynthesisEnvironment { synthSym :: sym
                       , synthBaseSet :: BaseSet sym arch
                       , synthInsns :: [Some (TemplatedInstruction sym arch)]
                       }

data SynthesisParams sym arch =
  SynthesisParams { synthEnv :: SynthesisEnvironment sym arch
                  , synthMaxLength :: Int
                  }

data SynthesisState sym arch =
  SynthesisState { synthTests :: [ConcreteTest sym arch]
                 , synthPrefixes :: Seq.Seq [Some (TemplatedInstruction sym arch)]
                 }

type Synth sym arch = ReaderT (SynthesisParams sym arch) (StateT (SynthesisState sym arch) IO)

askInsns :: Synth sym arch [Some (TemplatedInstruction sym arch)]
askInsns = reader (synthInsns . synthEnv)

askMaxLength :: Synth sym arch Int
askMaxLength = reader synthMaxLength

calcFootprint :: (Architecture arch)
              => [Some (TemplatedInstruction sym arch)]
              -> (Set.Set (Some (Location arch)), Set.Set (Some (Location arch)))
calcFootprint = foldl' addPrint (Set.empty, Set.empty)
  where addPrint (curInputs, curOutputs) insn =
          let (newInputs, newOutputs) = viewSome (templatedInputs &&& templatedOutputs) insn
              upInputs  = curInputs `Set.union` (newInputs Set.\\ curOutputs)
              upOutputs = curOutputs `Set.union` newOutputs
          -- footprint should not include IP
          in ( Set.filter (\(Some l) -> not (isIP l)) upInputs
             , Set.filter (\(Some l) -> not (isIP l)) upOutputs )

footprintFilter :: (Architecture arch, ShowF (S.BoundVar sym), ShowF (S.SymExpr sym))
                => Formula sym arch
                -> [Some (TemplatedInstruction sym arch)]
                -> Bool
footprintFilter target candidate =
  let (candInputs, candOutputs) = calcFootprint candidate
      targetInputs  = formInputs target
      targetOutputs = formOutputs target
  in candInputs `Set.isSubsetOf` targetInputs &&
     candOutputs `Set.isSubsetOf` targetOutputs



instantiate :: ( TemplateConstraints arch, ArchRepr arch
               , sym ~ WE.ExprBuilder t st fs
               , WPO.OnlineSolver solver
               , CB.IsSymInterface sym
               , ?memOpts :: LLVM.MemOptions
               )
            => CBO.OnlineBackend solver t st fs
            -> CegisParams sym arch
            -> Formula sym arch
            -> [Some (TemplatedInstruction sym arch)]
            -> Synth sym arch (Maybe [Instruction arch])
instantiate bak params target trial
  | footprintFilter target trial = do
      -- Instantiate the templated formulas for the templated instructions we're
      -- trying. Ideally we'd like to cache these somewhere, but this is hard
      -- due to the implementation of 'TemplatedInstruction'. (It stores a list
      -- of 'TemplatedOperand', which has a function inside it, and it's
      -- non-trivial to either make it not use a function or come up with a
      -- surrogate key.)
      liftIO $ putStrLn $ "\nTrial: " ++ show (prettyTrialInsns trial)
      tifs <- liftIO $ traverse (viewSome (genTemplatedFormula bak)) trial
      st <- get
      cegisResult <- liftIO $ cegis bak params (synthTests st) tifs
      case cegisResult of
        -- If we find an equivalent instantiation, we're done!
        CegisEquivalent insns -> do
                         liftIO $ putStrLn "CegisEquivalent"
                         return (Just insns)
        -- Otherwise, add this template as a possible prefix, and adopt the new
        -- test set.
        CegisUnmatchable newTests -> do
          -- liftIO $ putStrLn "CegisUnmatchable"
          let oldPrefixes = synthPrefixes st
          put (st { synthTests = newTests
                  , synthPrefixes = oldPrefixes Seq.|> trial
                  })
          return Nothing
  | otherwise = do -- liftIO . print $ PP.text "Could not instantiate trial" <+> prettyTrialInsns trial
                   return Nothing

prettyTrialInsns :: forall sym arch. Architecture arch => [Some (TemplatedInstruction sym arch)] -> PP.Doc
prettyTrialInsns ls = foldl (PP.<+>) PP.empty $ fmap go ls
  where
    go :: Some (TemplatedInstruction sym arch) -> PP.Doc
    go (Some (TemplatedInstruction opcode _ ops)) = PP.text (showF opcode) <+> PP.parens (PP.text (show ops))

synthesizeFormula' :: (Architecture arch,
                       TemplatableOperand arch,
                       ArchRepr arch,
                       Architecture (TemplatedArch arch),
                       WPO.OnlineSolver solver,
                       sym ~ WE.ExprBuilder t st fs,
                       CB.IsSymInterface sym,
                       ?memOpts :: LLVM.MemOptions
                       )
                   => CBO.OnlineBackend solver t st fs
                   -> CegisParams sym arch
                   -> Formula sym arch
                   -> Synth sym arch (Maybe [Instruction arch])
synthesizeFormula' bak params target = do
  liftIO . putStrLn $ "Calling synthesizeFormula' on target " ++ show target
  st <- get
  case Seq.viewl (synthPrefixes st) of
    prefix Seq.:< prefixesTail -> do
      maxLen <- askMaxLength
      if 1 + length prefix > maxLen then return Nothing else do
        put $ st { synthPrefixes = prefixesTail }
        possibleInsns <- askInsns
        -- N.B.: 'instantiate' here will add back to the prefixes if it's worth
        -- saving. I'm not a big fan of MonadState here, but it was the nicest
        -- solution I could come up with.
        result <- sequenceMaybes $
          map (\insn -> instantiate bak params target (prefix ++ [insn])) possibleInsns
        case result of
          Just insns -> return (Just insns)
          Nothing -> synthesizeFormula' bak params target
    -- If there are no more possible prefixes, we can't synthesize this formula.
    Seq.EmptyL -> return Nothing

synthesizeFormula :: forall solver t st fs sym arch .
                     (Architecture arch,
                      TemplatableOperand arch,
                      ArchRepr arch,
                      Architecture (TemplatedArch arch),
                      Typeable arch,
                      WPO.OnlineSolver solver,
                      sym ~ WE.ExprBuilder t st fs,
                      CB.IsSymInterface sym,
                      ?memOpts :: LLVM.MemOptions
                     )
                  => CBO.OnlineBackend solver t st fs
                  -> SynthesisParams sym arch
                  -> Formula sym arch
                  -> IO (Maybe [Instruction arch])
synthesizeFormula bak params target = do
  putStrLn $ "Calling synthesizeFormula on target " ++ show target
  let baseSet = synthBaseSet $ synthEnv params
  cegisParams <- liftIO $ mkCegisParams bak baseSet target
  evalStateT (runReaderT (synthesizeFormula' bak cegisParams target) params) $
    SynthesisState { synthTests = []
                   , synthPrefixes = Seq.singleton []
                   }



