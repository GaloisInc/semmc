module Main where

-- TODO:
--
-- Command line options type
-- Configuration type
-- List of architectures
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
--   example: SemMC.Stochastic.Instantiate
--
-- Generation of random tests:
--   SemMC.Architecture.Concrete, randomState
--   see also serialize/deserialize class methods
--
-- Running on remote hosts:
--   SemMC.Concrete.Execution, runRemote
--
-- Remote runner: semmc/tools/remote-runner.c
--
-- Helpers:
--   SemMC.Stochastic.Monad.runConcreteTests (interacts with remote-runner)
--   semmc/semmc-ppc/tools/Test.hs
--
-- Instantiating semantics:
--   semmc-ppc/tools/SynthDemo.hs

main :: IO ()
main = putStrLn "Hello, Haskell!"
