module SemMC.Strata ( strata ) where

-- Have this process the worklist in a single-threaded way, but for each work
-- item spawn off an Async thread to process it.  Wait once there are enough
-- outstanding threads.

strata :: [(instr, semantics)] -> IO [(instr, semantics)]
strata = undefined
