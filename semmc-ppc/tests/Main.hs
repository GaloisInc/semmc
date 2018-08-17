{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import           Data.Parameterized.Classes ( OrdF )
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Dismantle.PPC as D
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified What4.Protocol.Online as WPO

import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as SF
import qualified SemMC.Log as SL
import qualified SemMC.Synthesis.Testing as SST
import qualified SemMC.Synthesis as SS

import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC32.Opcodes as PPC32

main :: IO ()
main = do
  PN.withIONonceGenerator $ \ng ->
    CBO.withYicesOnlineBackend @_ @(CBO.Flags CBO.FloatReal) ng $ \sym -> do
      let sems = [ (sop, bs) | (sop, bs) <- PPC32.allSemantics, S.member sop insns ]
      (baseSet, synthEnv) <- loadBaseSet PPC32.allDefinedFunctions sems sym
      T.defaultMain (allTests baseSet synthEnv)

allTests :: (WPO.OnlineSolver t solver)
         => MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula (CBO.OnlineBackend t solver fs) PPC32.PPC)
         -> SS.SynthesisEnvironment (CBO.OnlineBackend t solver fs) PPC32.PPC
         -> T.TestTree
allTests baseSet synthEnv =
  T.testGroup "synthesis" (map (toSynthesisTest baseSet synthEnv) progs)

insns :: S.Set (Some (D.Opcode o))
insns = S.fromList
        [ Some D.ADD4
        , Some D.ADDC
        , Some D.ADDI
        , Some D.ADDIS
        , Some D.AND
        , Some D.ANDC
        , Some D.EQV
        , Some D.LI
        , Some D.MULLW
        , Some D.MULLD
        , Some D.NAND
        , Some D.NEG
        , Some D.NOR
        , Some D.OR
        , Some D.ORI
        , Some D.ORC
        , Some D.ORIS
        , Some D.SLD
        , Some D.SLW
        , Some D.SRAW
        , Some D.SRAD
        , Some D.SRD
        , Some D.SRW
        , Some D.SUBF
        ]

progs :: [(String, [D.Instruction])]
progs = [ ("addNegated", [ D.Instruction D.NEG (reg 5 PL.:< reg 2 PL.:< PL.Nil)
                         , D.Instruction D.ADD4 (reg 11 PL.:< reg 5 PL.:< reg 3 PL.:< PL.Nil)
                         ])
        ]
  where
    reg n = D.Gprc (D.GPR n)

toSynthesisTest :: (WPO.OnlineSolver t solver)
                => MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula (CBO.OnlineBackend t solver fs) PPC32.PPC)
                -> SS.SynthesisEnvironment (CBO.OnlineBackend t solver fs) PPC32.PPC
                -> (String, [D.Instruction])
                -> T.TestTree
toSynthesisTest baseSet synthEnv (name, p) = T.testCase name $ do
  res <- SST.synthesizeAndCheck (Proxy @PPC32.PPC) synthEnv baseSet matchInsn p
  T.assertEqual "Expected equivalence" SST.Equivalent res

matchInsn :: forall a . D.Instruction -> (forall sh . D.Opcode D.Operand sh -> PL.List D.Operand sh -> a) -> a
matchInsn i k =
  case i of
    D.Instruction opc operands -> k opc operands

loadBaseSet :: (WPO.OnlineSolver t solver, sym ~ CBO.OnlineBackend t solver fs, CB.IsSymInterface sym)
            => [(String, BS8.ByteString)]
            -> [(Some (D.Opcode D.Operand), BS8.ByteString)]
            -> sym
            -> IO (MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula sym PPC32.PPC),
                   SS.SynthesisEnvironment sym PPC32.PPC)
loadBaseSet funcs ops sym = do
  lcfg <- SL.mkNonLogCfg
  let ?logCfg = lcfg
  lib <- SF.loadLibrary (Proxy @PPC32.PPC) sym funcs
  baseSet <- SF.loadFormulas sym lib ops
  let synthEnv = SS.setupEnvironment sym baseSet
  return (removeTemplates baseSet, synthEnv)

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
