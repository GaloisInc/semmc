module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs
    )
    where

import qualified Data.Map as M

import SemMC.Architecture.ARM.BaseSemantics.Arithmetic
import SemMC.Architecture.ARM.BaseSemantics.Base
import SemMC.Architecture.ARM.BaseSemantics.Branch
import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.Architecture.ARM.BaseSemantics.Misc
import SemMC.DSL


-- TODO: Change this value type from a pair to something nicer
semdefs :: [ (String, ([(String, Definition)], M.Map String (LibraryFunction SemM_ARMData))) ]
semdefs = [ ("memory", memory)
          , ("arithmetic", arithmetic)
          , ("bitwise", bitwise)
          , ("branches", branches)
          , ("miscellaneous", misc)
          ]

memory :: ([(String, Definition)], M.Map String (LibraryFunction SemM_ARMData))
memory = runSem $ do
            manualMemory
            return ()

arithmetic :: ([(String, Definition)], M.Map String (LibraryFunction SemM_ARMData))
arithmetic = runSem $ do
             manualArithmetic
             return ()

bitwise :: ([(String, Definition)], M.Map String (LibraryFunction SemM_ARMData))
bitwise = runSem $ do
            manualBitwise
            return ()

branches :: ([(String, Definition)], M.Map String (LibraryFunction SemM_ARMData))
branches = runSem $ do
             manualBranches
             return ()

misc :: ([(String, Definition)], M.Map String (LibraryFunction SemM_ARMData))
misc = runSem miscSemantics
