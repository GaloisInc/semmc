{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main ( main ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import           Data.Parameterized.Classes ( OrdF )
import           Data.Parameterized.List ( List(..) )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )

import qualified Data.Int.Indexed as I
import qualified Dismantle.PPC as D
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified What4.Protocol.Online as WPO
import           What4.Config
import           What4.Interface
import           What4.Solver
import           Control.Monad
import qualified Data.Text as Text


import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as SF
import qualified SemMC.Log as SL
import qualified SemMC.Synthesis.Testing as SST
import qualified SemMC.Synthesis as SS

import qualified SemMC.Architecture.PPC64 as PPC64
import qualified SemMC.Architecture.PPC64.Opcodes as PPC64

main :: IO ()
main = do
  PN.withIONonceGenerator $ \ng ->
    CBO.withYicesOnlineBackend @_ @(CBO.Flags CBO.FloatReal) ng $ \sym -> do


      -- set the verbosity level
      void $ join (setOpt <$> getOptionSetting verbosity (getConfiguration sym)
                          <*> pure (toInteger 3))

      -- set the path to yices
      void $ join (setOpt <$> getOptionSetting yicesPath (getConfiguration sym)
                          <*> pure (Text.pack "/home/jpaykin/scripts/yices"))


      let sems = [ (sop, bs) | (sop, bs) <- PPC64.allSemantics, S.member sop insns ]
      (baseSet, synthEnv) <- loadBaseSet PPC64.allDefinedFunctions sems sym

      T.defaultMain (allTests baseSet synthEnv)

allTests :: (WPO.OnlineSolver t solver)
         => MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula (CBO.OnlineBackend t solver fs) PPC64.PPC)
         -> SS.SynthesisEnvironment (CBO.OnlineBackend t solver fs) PPC64.PPC
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
        , Some D.LHA
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
        , Some D.STD
        , Some D.STDU
        , Some D.SC
        ]

progs :: [(String, [D.Instruction])]
progs = [-- ("addNegated", [ D.Instruction D.NEG (reg 5 :< reg 2 :< Nil)
         --                , D.Instruction D.ADD4 (reg 11 :< reg 5 :< reg 3 :< Nil)
         --                ])
          ("STD",  [ D.Instruction D.STD  $ memrix 1 (2)   :< reg 31     :< Nil ])
--        , ("STD",  [ D.Instruction D.STD  $ memrix 1 (-2)  :< reg 31     :< Nil ])
--      ,   ("LHA",  [ D.Instruction D.LHA  $ reg 31         :< memri 1 (-2) :< Nil ])
--        , ("LI",   [ D.Instruction D.LI   $ reg 0          :< D.S16imm 1 :< Nil ])
--        , ("STDU", [ D.Instruction D.STDU $ memrix 1 (-16) :< reg 1      :< Nil ])
        ]
  where
    reg n = D.Gprc (D.GPR n)
    memrix n i = D.Memrix (D.MemRIX (Just (D.GPR n)) (i :: I.I 14))
    memri n i = D.Memri (D.MemRI (Just (D.GPR n)) i)

toSynthesisTest :: (WPO.OnlineSolver t solver)
                => MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula (CBO.OnlineBackend t solver fs) PPC64.PPC)
                -> SS.SynthesisEnvironment (CBO.OnlineBackend t solver fs) PPC64.PPC
                -> (String, [D.Instruction])
                -> T.TestTree
toSynthesisTest baseSet synthEnv (name, p) = T.testCase name $ do
  res <- SST.synthesizeAndCheck (Proxy @PPC64.PPC) synthEnv baseSet matchInsn p
  T.assertEqual "Expected equivalence" SST.Equivalent res

matchInsn :: forall a . D.Instruction -> (forall sh . D.Opcode D.Operand sh -> List D.Operand sh -> a) -> a
matchInsn i k =
  case i of
    D.Instruction opc operands -> k opc operands

loadBaseSet :: (WPO.OnlineSolver t solver, sym ~ CBO.OnlineBackend t solver fs, CB.IsSymInterface sym)
            => [(String, BS8.ByteString)]
            -> [(Some (D.Opcode D.Operand), BS8.ByteString)]
            -> sym
            -> IO (MapF.MapF (D.Opcode D.Operand) (SF.ParameterizedFormula sym PPC64.PPC),
                   SS.SynthesisEnvironment sym PPC64.PPC)
loadBaseSet funcs ops sym = do
  lcfg <- SL.mkNonLogCfg
  let ?logCfg = lcfg
  lib <- SF.loadLibrary (Proxy @PPC64.PPC) sym funcs
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
