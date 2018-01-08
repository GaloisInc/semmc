module Main where

-- TODO:
--
-- Command line options type
--
-- How do tests get spread over remote hosts? Need remote host list for
-- each ISA. Probably want to have one worker thread per host, with a
-- manager thread per ISA giving out work to do.
--
-- Testing loop:
--   Inputs: ISA, N, list of opcodes (or all) OS
--   For each opcode O in OS:
--     Generate a chunk of N pairs of (random initial state, random instruction for O)
--     Submit chunk to remote host for execution
--     Evaluate chunk final states and evaluate instructions against semantics and initial states, compare results
--     Emit chunk result summary to event log
--       (coalesce successes)

-- Generation of random instructions:
--   Dismantle.Instruction.Random.randomInstruction
--     randomInstruction :: (ArbitraryOperands c o, OrdF (c o))
--                       => A.Gen
--                       -> NES.Set (Some (c o))
--                       -> IO (I.GenericInstruction c o)
--   example: SemMC.Stochastic.Instantiate
--
-- Generation of random tests:
--   SemMC.Architecture.Concrete, randomState method of Architecture class
--     randomState :: proxy arch -> DA.Gen -> IO (ConcreteState arch)
--   see also serialize/deserialize class methods
--     serialize :: proxy arch -> ConcreteState arch -> B.ByteString
--     deserialize :: proxy arch -> B.ByteString -> Maybe (ConcreteState arch)
--
-- Running on remote hosts:
--   SemMC.Concrete.Execution, runRemote
--     Make a remote runner for a given host:
--     runRemote :: (U.HasLogCfg)
--               => Maybe FilePath
--               -- ^ Optionally, a different name for the remote runner executable
--               -> String
--               -- ^ The hostname to run test cases on
--               -> TestSerializer c i
--               -- ^ Functions for converting to and from machine states
--               -> TestRunner c i
--
--    Returns a TestRunner, so we have to then bind a
--      Chan (Maybe [TestCase c i])
--    to send it test cases (Nothing means stop) and then bind a
--      Chan (ResultOrError c)
--    to get results from it.
--    The 'c' is a machine state.
--    The 'i' is an instruction.
--
--    Seems to raise an exception if for some reason the connection to
--    the remote host (or invocation of the runner) fails.
--
-- Remote runner: semmc/tools/remote-runner.c
--   Needs to be built on each remote host and needs to be in the PATH
--   of the account used for testing
--   Note: defaults to $USER on remote host and defaults to using a
--   local SSH key + remote authorized_keys to authenticate.
--
-- Helpers:
--   SemMC.Stochastic.Monad.runConcreteTests (interacts with remote-runner)
--   semmc/semmc-ppc/tools/Test.hs
--
-- Instantiating semantics:
--   semmc-ppc/tools/SynthDemo.hs

data FuzzerConfiguration =
    FuzzerConfiguration { fuzzerArchitectures :: [FuzzerArchConfig]
                        , fuzzerOutputDirectory :: FilePath
                        }
                        deriving (Show)

data FuzzerArchConfig =
    FuzzerArchConfig { fuzzerArchName :: String
                     , fuzzerArchTestingHosts :: [FuzzerTestHost]
                     }
                     deriving (Show)

data FuzzerTestHost =
    FuzzerTestHost { fuzzerTestHostname :: String
                   , fuzzerTestRunners :: Int
                   , fuzzerTestChunkSize :: Int
                   }
                   deriving (Show)

-- Need a list of known architectures. What do we need for each?
--  * Something from dismantle
--  * Something from semmc
-- knownArchitectures :: []
-- knownArchitectures =
--     [
--     ]

main :: IO ()
main = putStrLn "Hello, Haskell!"
