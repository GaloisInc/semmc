{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main ( main, memtest, nonmemtest ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import           GHC.Word (Word8)
import           GHC.Int (Int16)

import           Data.Parameterized.Classes ( OrdF )
import           Data.Parameterized.List ( List(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )

import qualified Data.Int.Indexed as I
import qualified Dismantle.PPC as D
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.LLVM.MemModel as LLVM
import qualified What4.Expr.Builder as WEB
import qualified What4.ProblemFeatures as WPF
import qualified What4.Protocol.Online as WPO
import qualified What4.InterpretedFloatingPoint as WIF


import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as SF
import qualified SemMC.Log as SL
import qualified SemMC.Synthesis.Testing as SST
import qualified SemMC.Synthesis as SS

import qualified SemMC.Architecture.PPC64 as PPC64
import qualified SemMC.Architecture.PPC64.Opcodes as PPC64


main :: IO ()
main = executeTests $ memProgs ++ nonMemProgs -- ++ [longProg]

-- just test for programs that deal with memory
memtest :: IO ()
memtest = executeTests memProgs

-- just test for programs that do not deal with memory
nonmemtest :: IO ()
nonmemtest = executeTests nonMemProgs

-- synthesize a "long" sequence of memory instructions
_longtest :: IO ()
_longtest = executeTests [longProg]

data SemMCPPCData t = SemMCPPCData

executeTests :: [(String, [D.Instruction])] -> IO ()
executeTests progsToTest = do
  PN.withIONonceGenerator $ \ng -> do
    sym <- WEB.newExprBuilder WEB.FloatRealRepr SemMCPPCData ng
    CBO.withZ3OnlineBackend sym CBO.NoUnsatFeatures WPF.noFeatures $ \bak -> do
      let sems = [ (sop, bs) | (sop, bs) <- PPC64.allSemantics, S.member sop insns ]
      (baseSet, synthEnv) <- loadBaseSet PPC64.allDefinedFunctions sems bak
      let ?memOpts = LLVM.defaultMemOptions
      T.defaultMain (allTests bak baseSet synthEnv progsToTest)

allTests :: ( sym ~ WEB.ExprBuilder scope st fs
            , WPO.OnlineSolver solver
            , WIF.IsInterpretedFloatExprBuilder sym
            , ?memOpts :: LLVM.MemOptions
            )
         => CBO.OnlineBackend solver scope st fs
         -> MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula sym PPC64.PPC)
         -> SS.SynthesisEnvironment sym PPC64.PPC
         -> [(String, [D.Instruction])]
         -> T.TestTree
allTests bak baseSet synthEnv progsToTest =
  T.adjustOption @T.Timeout (\_ -> T.mkTimeout 1000000000) $
    T.testGroup "synthesis" (map (toSynthesisTest bak baseSet synthEnv) progsToTest)

insns :: S.Set (Some (D.Opcode o))
insns = S.fromList
        [ Some D.ADD4
        , Some D.LHA
        , Some D.LI
        , Some D.NEG
        , Some D.NOR
        , Some D.STD
        , Some D.STDU
        , Some D.LWZ
        ]

nonMemProgs :: [(String, [D.Instruction])]
nonMemProgs =
      [("add", [ D.Instruction D.ADD4 (mkGPR 11 :< mkGPR 5 :< mkGPR 3 :< Nil) ])]
      ++
      [("negated", [D.Instruction D.NEG (mkGPR 5 :< mkGPR 2 :< Nil)])]
      ++
      [("independent", [ D.Instruction D.ADD4 (mkGPR 11 :< mkGPR 5 :< mkGPR 3 :< Nil)
                       , D.Instruction D.NEG (mkGPR 5 :< mkGPR 2 :< Nil)
                       ])]
      ++
      [("addNegated", [ D.Instruction D.NEG (mkGPR 5 :< mkGPR 2 :< Nil)
                      , D.Instruction D.ADD4 (mkGPR 11 :< mkGPR 5 :< mkGPR 3 :< Nil)
                      ])]
      ++
      [("LI", [ D.Instruction D.LI $ mkGPR 0 :< D.S16imm 1 :< Nil ])]
      ++
      [("redundency", [ D.Instruction D.LI $ mkGPR 2 :< D.S16imm 1 :< Nil
                      , D.Instruction D.LI $ mkGPR 2 :< D.S16imm 2 :< Nil ])]

longProg :: (String, [D.Instruction])
longProg = ("LongProg", [ D.Instruction D.STD  $ mkMemRIX 1 (-1)   :< mkGPR 31 :< Nil
                        , D.Instruction D.LI   $ mkGPR 31 :< D.S16imm 3 :< Nil
                        , D.Instruction D.LHA  $ mkGPR 31 :< mkMemRI 1 2 :< Nil
                        ])



memProgs  :: [(String,[D.Instruction])]
memProgs =
    [("STD2",    [ D.Instruction D.STD  $ mkMemRIX 1 (2)   :< mkGPR 31     :< Nil ])]
    ++
    [("STD-4",   [ D.Instruction D.STD  $ mkMemRIX 1 (-4)   :< mkGPR 31     :< Nil])]
    ++
    [("STD-1",   [ D.Instruction D.STD  $ mkMemRIX 1 (-1)   :< mkGPR 31     :< Nil])]
    ++
    [("STDU16",  [ D.Instruction D.STDU $ mkMemRIX 1 (16) :< mkGPR 2      :< Nil ])]
    ++
    [("STDU-3",  [ D.Instruction D.STDU $ mkMemRIX 1 (-3)  :< mkGPR 2      :< Nil ])]
    ++
    [("LHA4",    [ D.Instruction D.LHA  $ mkGPR 31         :< mkMemRI 1 4  :< Nil ])]
    ++
    [("LHA0",    [ D.Instruction D.LHA  $ mkGPR 31         :< mkMemRI 1 0  :< Nil ])]
    ++
    [("LHA-1",   [ D.Instruction D.LHA  $ mkGPR 31         :< mkMemRI 1 (-1)  :< Nil ])]
    ++
    [("LHA-16",  [ D.Instruction D.LHA  $ mkGPR 31         :< mkMemRI 1 (-16) :< Nil ])]
    ++
    [("LWZ363",  [ D.Instruction D.LWZ  $ mkGPR 31         :< mkMemRI 1 (363)  :< Nil ])]
    ++
    [("LWZ-2",   [ D.Instruction D.LWZ  $ mkGPR 31         :< mkMemRI 1 (-2)  :< Nil ])]
    ++
    [("LWZ-82",  [ D.Instruction D.LWZ  $ mkGPR 31         :< mkMemRI 1 (-82)  :< Nil ])]


mkGPR :: Word8 -> D.Operand "Gprc"
mkGPR n = D.Gprc (D.GPR n)

mkMemRIX :: Word8 -> I.I 14 -> D.Operand "Memrix"
mkMemRIX n i = D.Memrix (D.MemRIX (Just (D.GPR n)) (i :: I.I 14))

mkMemRI :: Word8 -> Int16 -> D.Operand "Memri"
mkMemRI n i = D.Memri (D.MemRI (Just (D.GPR n)) i)

toSynthesisTest :: ( WPO.OnlineSolver solver
                   , CB.IsSymInterface sym
                   , sym ~ WEB.ExprBuilder t st fs
                   , ?memOpts :: LLVM.MemOptions
                   )
                => CBO.OnlineBackend solver t st fs
                -> MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula sym PPC64.PPC)
                -> SS.SynthesisEnvironment sym PPC64.PPC
                -> (String, [D.Instruction])
                -> T.TestTree
toSynthesisTest bak baseSet synthEnv (name, p) = T.testCase name $ do
  res <- SST.synthesizeAndCheck (Proxy @PPC64.PPC) bak synthEnv baseSet matchInsn p
  T.assertEqual "Expected equivalence" SST.Equivalent res

matchInsn :: D.Instruction
          -> (forall sh . D.Opcode D.Operand sh -> List D.Operand sh -> a)
          -> a
matchInsn (D.Instruction opc operands) k = k opc operands

loadBaseSet :: forall sym solver t st fs.
               (WPO.OnlineSolver solver, sym ~ WEB.ExprBuilder t st fs, CB.IsSymInterface sym)
            => [(String, BS8.ByteString)]
            -> [(Some (D.Opcode D.Operand), BS8.ByteString)]
            -> CBO.OnlineBackend solver t st fs
            -> IO (MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula sym PPC64.PPC),
                   SS.SynthesisEnvironment sym PPC64.PPC)
loadBaseSet funcs ops bak = do
  let sym = (CB.backendGetSym bak)
  lcfg <- SL.mkNonLogCfg
  let ?logCfg = lcfg
  env <- SF.formulaEnv (Proxy @PPC64.PPC) sym
  lib <- SF.loadLibrary (Proxy @PPC64.PPC) sym env funcs
  baseSet <- SF.loadFormulas sym (templateEnv env) lib ops
  let synthEnv = SS.setupEnvironment bak baseSet
  return (removeTemplates baseSet, synthEnv)
  where
    templateEnv :: SF.FormulaEnv sym arch -> SF.FormulaEnv sym (SS.TemplatedArch arch)
    templateEnv (SF.FormulaEnv fs b) = SF.FormulaEnv fs b

removeTemplates :: forall arch sym
                 . (OrdF (SA.Opcode arch (SA.Operand arch)),
                    OrdF (SA.Location arch))
                => SS.BaseSet sym arch
                -> MapF.MapF (SA.Opcode arch (SA.Operand arch)) (SF.ParameterizedFormula sym arch)
removeTemplates = MapF.foldrWithKey f MapF.empty
  where f :: forall sh
           . SA.Opcode arch (SA.Operand arch) sh
          -> SF.ParameterizedFormula sym (SS.TemplatedArch arch) sh
          -> MapF.MapF (SA.Opcode arch (SA.Operand arch)) (SF.ParameterizedFormula sym arch)
          -> MapF.MapF (SA.Opcode arch (SA.Operand arch)) (SF.ParameterizedFormula sym arch)
        f op pf = MapF.insert op (SS.unTemplate pf)
