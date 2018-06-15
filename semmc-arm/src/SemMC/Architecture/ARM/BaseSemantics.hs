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
semdefs :: [ (String, ([(String, Definition)], [(String, FunctionDefinition)])) ]
semdefs = [ ("memory", memory)
          , ("arithmetic", arithmetic)
          , ("bitwise", bitwise)
          , ("branches", branches)
          , ("miscellaneous", misc)
          ]

memory, arithmetic, bitwise, branches, misc
  :: ([(String, Definition)], [(String, FunctionDefinition)])
memory = runSem $ do
            manualMemory
            return ()

arithmetic = runSem $ do
             manualArithmetic
             return ()

bitwise = runSem $ do
            manualBitwise
            return ()

branches = runSem $ do
             manualBranches
             return ()

misc = runSem miscSemantics
