module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs
    )
    where

import qualified Data.Map as M

import SemMC.Architecture.ARM.BaseSemantics.Arithmetic
import SemMC.Architecture.ARM.BaseSemantics.Branch
import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.Architecture.ARM.BaseSemantics.Misc
import SemMC.DSL


-- TODO: Change this value type from a pair to something nicer
semdefs :: [ (String, ([(String, Definition)], M.Map String LibraryFunction)) ]
semdefs = [ ("memory", memory)
          , ("arithmetic", arithmetic)
          , ("bitwise", bitwise)
          , ("branches", branches)
          , ("miscellaneous", misc)
          ]

memory :: ([(String, Definition)], M.Map String LibraryFunction)
memory = runSem $ do
            manualMemory
            return ()

arithmetic :: ([(String, Definition)], M.Map String LibraryFunction)
arithmetic = runSem $ do
             manualArithmetic
             return ()

bitwise :: ([(String, Definition)], M.Map String LibraryFunction)
bitwise = runSem $ do
            manualBitwise
            return ()

branches :: ([(String, Definition)], M.Map String LibraryFunction)
branches = runSem $ do
             manualBranches
             return ()

misc :: ([(String, Definition)], M.Map String LibraryFunction)
misc = runSem miscSemantics
