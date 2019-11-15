{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module SemMC.Fuzzer.Types
  ( OpcodeMatch(..)
  , TestStrategy(..)

  , Batch(..)
  , TestInfo(..)
  , TestState(..)
  , TestOutcome(..)
  , TestInput(..)

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
import           Data.Proxy ( Proxy(..) )
import           Text.PrettyPrint.HughesPJ (Doc)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSC8

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
import           SemMC.Fuzzer.Filters(FilterParser(..))

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
                 , fuzzerFilters :: [String]
                 }
                 deriving (Show)

data FuzzerTestHost =
    FuzzerTestHost { fuzzerTestHostname :: String
                   , fuzzerTestUser :: Maybe String
                   , fuzzerTestChunkSize :: Int
                   , fuzzerRunnerPath :: FilePath
                   , fuzzerTestThreads :: Int
                   }
                   deriving (Show)

data ArchImpl where
    ArchImpl :: forall arch .
                ( TemplatableOperand arch
                , A.Architecture arch
                , C.ConcreteArchitecture arch
                , D.ArbitraryOperands (A.Opcode arch) (A.Operand arch)
                , MapF.OrdF (A.Opcode arch (TemplatedOperand arch))
                , MapF.ShowF (A.Opcode arch (TemplatedOperand arch))
                , Show (GenericInstruction (A.Opcode arch) (A.Operand arch))
                , EnumF (A.Opcode arch (TemplatedOperand arch))
                , HasRepr (A.Opcode arch (A.Operand arch)) (L.List (A.OperandTypeRepr arch))
                , FilterParser arch
                )
             => String
             -> Proxy arch
             -> [Some ((A.Opcode arch) (A.Operand arch))]
             -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
             -> [(String, BS.ByteString)]
             -> CE.TestSerializer (V.ConcreteState arch) (A.Instruction arch)
             -> (GenericInstruction (A.Opcode arch) (A.Operand arch) -> Doc)
             -> (GenericInstruction (A.Opcode arch) (A.Operand arch) -> BSC8.ByteString)
             -> (Some ((A.Opcode arch) (A.Operand arch)) -> Bool)
             -> ArchImpl

archImplName :: ArchImpl -> String
archImplName (ArchImpl n _ _ _ _ _ _ _ _) = n

-- Note: the JSON encodings of the types below must match the decoder
-- implementation in the 'fuzzermon' web service.

data Batch =
    Batch { batchFuzzerHost :: String
          -- ^ The host running the semmc-fuzzer program.
          , batchFuzzerUser :: String
          -- ^ The user running the semmc-fuzzer program.
          , batchTestingHost :: String
          -- ^ The architecture-specific hardware used to run the test case.
          , batchArch :: String
          , batchEntries :: [TestInfo]
          }

instance AE.ToJSON Batch where
    toJSON b =
        AE.object [ "fuzzer-host" AE..= batchFuzzerHost b
                  , "fuzzer-user" AE..= batchFuzzerUser b
                  , "testing-host" AE..= batchTestingHost b
                  , "arch" AE..= batchArch b
                  , "entries" AE..= batchEntries b
                  ]

data TestOutcome = Success
                 | Failure
                 | UnexpectedSignal Int32
  deriving (Eq, Show)

data TestInfo =
    TestInfo { testInfoOpcode :: String
             , testInfoPretty :: String
             , testInfoInstructionBytes :: String
             , testInfoInputs :: [TestInput]
             , testInfoRawOperands :: String
             , testInfoStates :: [TestState]
             , testOutcome :: TestOutcome
             }
    deriving Show

instance AE.ToJSON TestInfo where
    toJSON i =
        let (ty, extra) = case testOutcome i of
              Success -> ("success", [])
              Failure -> ("failure", [])
              UnexpectedSignal num -> ("unexpectedSignal", ["signal" AE..= num])
        in AE.object $ [ "type" AE..= (ty :: T.Text)
                       , "opcode" AE..= testInfoOpcode i
                       , "pretty" AE..= testInfoPretty i
                       , "inputs" AE..= testInfoInputs i
                       , "bytes" AE..= testInfoInstructionBytes i
                       , "state" AE..= testInfoStates i
                       , "raw-operands" AE..= testInfoRawOperands i
                       ] ++ extra

data TestInput =
    TestInput { testInputLocation :: String
              , testInputValue :: String
              }
    deriving Show

instance AE.ToJSON TestInput where
    toJSON i =
        AE.object [ "location" AE..= testInputLocation i
                  , "value" AE..= testInputValue i
                  ]

data TestState =
    TestState { testLocation :: String
              , testExpected :: String
              , testActual :: String
              }
  deriving Show

instance AE.ToJSON TestState where
    toJSON s =
        AE.object [ "location" AE..= testLocation s
                  , "expected" AE..= testExpected s
                  , "actual" AE..= testActual s
                  ]
