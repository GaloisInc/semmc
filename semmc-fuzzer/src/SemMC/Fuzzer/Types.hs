{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module SemMC.Fuzzer.Types
  ( OpcodeMatch(..)
  , TestStrategy(..)

  , Batch(..)
  , BatchEntry(..)
  , TestSuccess(..)
  , TestSignalError(..)
  , TestFailure(..)
  , TestFailureState(..)

  , FuzzerConfig(..)
  , FuzzerTestHost(..)

  , ArchImpl(..)
  , archImplName
  )
where

import qualified Data.Aeson as AE
import qualified Data.ByteString.UTF8 as BS8
import           Data.Int (Int32)
import           Data.EnumF (EnumF)
import qualified Data.Text as T
import           Text.PrettyPrint.HughesPJ (Doc)

import           Data.Parameterized.Some (Some(..))
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.HasRepr (HasRepr)
import qualified Data.Parameterized.List as L

import           Dismantle.Instruction (GenericInstruction)
import qualified Dismantle.Instruction.Random as D

import qualified SemMC.Log as L
import qualified SemMC.Concrete.Execution as CE
import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.Concrete as C
import qualified SemMC.Architecture.View as V
import           SemMC.Synthesis.Template ( TemplatableOperand
                                          , TemplatedOperand
                                          )

data OpcodeMatch =
    AllOpcodes
    -- ^ Test all opcodes in the ISA.
    | SpecificOpcodes [String]
    -- ^ Test only the specified opcodes.
    deriving (Show, Eq, Read)

data TestStrategy =
    RoundRobin
    -- ^ Test the opcodes in a round-robin fashion.
    | Randomized
    -- ^ Test the opcodes by choosing randomly from the set of available
    -- opcodes to test.
    deriving (Show, Eq, Read)

data FuzzerConfig =
    FuzzerConfig { fuzzerArchName :: String
                 , fuzzerArchTestingHosts :: [FuzzerTestHost]
                 , fuzzerTestOpcodes :: OpcodeMatch
                 , fuzzerTestStrategy :: TestStrategy
                 , fuzzerMaximumLogLevel :: L.LogLevel
                 , fuzzerReportURL :: Maybe String
                 }
                 deriving (Show)

data FuzzerTestHost =
    FuzzerTestHost { fuzzerTestHostname :: String
                   , fuzzerTestChunkSize :: Int
                   , fuzzerRunnerPath :: FilePath
                   , fuzzerTestThreads :: Int
                   }
                   deriving (Show)

data ArchImpl where
    ArchImpl :: forall proxy arch .
                ( TemplatableOperand arch
                , A.Architecture arch
                , C.ConcreteArchitecture arch
                , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
                , MapF.OrdF (A.Opcode arch (TemplatedOperand arch))
                , MapF.ShowF (A.Opcode arch (TemplatedOperand arch))
                , Show (GenericInstruction (A.Opcode arch) (A.Operand arch))
                , EnumF (A.Opcode arch (TemplatedOperand arch))
                , HasRepr (A.Opcode arch (A.Operand arch)) (L.List (A.OperandTypeRepr arch))
                )
             => String
             -> proxy arch
             -> [Some ((A.Opcode arch) (A.Operand arch))]
             -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
             -> CE.TestSerializer (V.ConcreteState arch) (A.Instruction arch)
             -> (GenericInstruction (A.Opcode arch) (A.Operand arch) -> Doc)
             -> (Some ((A.Opcode arch) (A.Operand arch)) -> Bool)
             -> ArchImpl

archImplName :: ArchImpl -> String
archImplName (ArchImpl n _ _ _ _ _ _) = n

-- Note: the JSON encodings of the types below must match the decoder
-- implementation in the 'fuzzermon' web service.

data Batch =
    Batch { batchFuzzerHost :: String
          , batchFuzzerUser :: String
          , batchTestingHost :: String
          , batchArch :: String
          , batchEntries :: [BatchEntry]
          }

instance AE.ToJSON Batch where
    toJSON b =
        AE.object [ "fuzzer-host" AE..= batchFuzzerHost b
                  , "fuzzer-user" AE..= batchFuzzerUser b
                  , "testing-host" AE..= batchTestingHost b
                  , "arch" AE..= batchArch b
                  , "entries" AE..= batchEntries b
                  ]

data BatchEntry = Success TestSuccess
                | Failure TestFailure
                | UnexpectedSignal TestSignalError

instance AE.ToJSON BatchEntry where
    toJSON (Success s) = AE.toJSON s
    toJSON (Failure f) = AE.toJSON f
    toJSON (UnexpectedSignal s) = AE.toJSON s

data TestSuccess =
    TestSuccess { testSuccessOpcode :: String
                , testSuccessCount :: Int
                }

instance AE.ToJSON TestSuccess where
    toJSON s =
        AE.object [ "type" AE..= ("success"::T.Text)
                  , "opcode" AE..= testSuccessOpcode s
                  , "count" AE..= testSuccessCount s
                  ]

data TestSignalError =
    TestSignalError { testSignalOpcode :: String
                    , testSignalPretty :: String
                    , testSignalNum :: Int32
                    }

instance AE.ToJSON TestSignalError where
    toJSON s =
        AE.object [ "type" AE..= ("unexpectedSignal"::T.Text)
                  , "opcode" AE..= testSignalOpcode s
                  , "pretty" AE..= testSignalPretty s
                  , "signal" AE..= testSignalNum s
                  ]

data TestFailure =
    TestFailure { testFailureOpcode :: String
                , testFailureRawOperands :: String
                , testFailurePretty :: String
                , testFailureStates :: [TestFailureState]
                }

instance AE.ToJSON TestFailure where
    toJSON s =
        AE.object [ "type" AE..= ("failure"::T.Text)
                  , "opcode" AE..= testFailureOpcode s
                  , "raw-operands" AE..= testFailureRawOperands s
                  , "pretty" AE..= testFailurePretty s
                  , "state" AE..= testFailureStates s
                  ]

data TestFailureState =
    TestFailureState { testFailureLocation :: String
                     , testFailureExpected :: String
                     , testFailureActual :: String
                     }

instance AE.ToJSON TestFailureState where
    toJSON s =
        AE.object [ "location" AE..= testFailureLocation s
                  , "expected" AE..= testFailureExpected s
                  , "actual" AE..= testFailureActual s
                  ]
